;;; -*- Mode: Scheme; scheme48-package: parser-combinators -*-

;;;; Parsing Tools
;;;; Parser Combinators

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-record-type <parse-state>
    (make-parse-state stream position advancer)
    parse-state?
  (stream parse-state/stream)
  (position parse-state/position)
  (advancer parse-state/advancer))

(define (parse parser stream initial-position advancer lose)
  (run 'PARSE parser (make-parse-state stream initial-position advancer)
       (lambda (consumed? pstate value perror)
         consumed? pstate perror        ;ignore
         value)
       (lambda (consumed? pstate perror)
         consumed? pstate               ;ignore
         (lose perror))))

(define (run marker parser pstate win lose)
  (if *parse-trace?*
      (begin
        (write `(RUN ,marker))
        (newline)))
  (parser pstate win lose))

(define (success* marker win consumed? pstate value)
  (success marker win consumed? pstate value
           (make-parse-error:unknown (parse-state/position pstate))))

(define (success marker win consumed? pstate value perror)
  (if *parse-trace?*
      (begin
        (write `(SUCCESS ,marker -> ,value
                         ,(parse-error/position perror)
                         ,@(parse-error/messages perror)))
        (newline)))
  (win consumed? pstate value perror))

(define (failure marker lose consumed? pstate perror)
  (if *parse-trace?*
      (begin
        (write `(FAILURE ,marker
                         ,(parse-error/position perror)
                         ,@(parse-error/messages perror)))
        (newline)))
  (lose consumed? pstate perror))

(define *parse-trace?* #f)

(define (enable-parse-trace) (set! *parse-trace?* #t))
(define (disable-parse-trace) (set! *parse-trace?* #f))

(define (with-parse-trace thunk)
  (if *parse-trace?*
      (thunk)
      (dynamic-wind enable-parse-trace
                    thunk
                    disable-parse-trace)))

(define (parser:epsilon thunk)
  (lambda (pstate win lose)
    lose                                ;ignore
    (success* 'PARSER:EPSILON win #f pstate (thunk))))

(define (parser:error . messages)
  (lambda (pstate win lose)
    win                                 ;ignore
    (failure 'PARSER:ERROR lose #f pstate
             (make-parse-error (parse-state/position pstate)
                               messages))))

(define (parser:on-failure lose parser)
  (lambda (pstate win *lose)
    (run 'PARSER:ON-FAILURE parser pstate win
         (lambda (consumed? pstate perror)
           (lose consumed? pstate perror *lose)))))

(define (parser:extend parser extender)
  (lambda (pstate win lose)
    (run 'PARSER:EXTEND parser pstate
         (lambda (consumed? pstate value perror)
           (run '(PARSER:EXTEND EXTENSION) (extender value) pstate
                (lambda (consumed?* pstate* value* perror*)
                  (success '(PARSER:EXTEND EXTENSION) win
                           (or consumed? consumed?*)
                           pstate*
                           value*
                           (if consumed?*
                               perror*
                               (merge-parse-errors perror perror*))))
                (lambda (consumed?* pstate* perror*)
                  (failure '(PARSER:EXTEND EXTENSION) lose
                           (or consumed? consumed?*)
                           pstate*
                           perror*))))
         lose)))

(define (parser:binary-choice left-parser right-parser)
  (lambda (pstate win lose)
    (run '(PARSER:BINARY-CHOICE LEFT) left-parser pstate
         win
         (lambda (consumed? pstate perror)
           (if consumed?
               (failure '(PARSER:BINARY-CHOICE CONSUMED) lose #t pstate perror)
               (let ((merge (lambda (consumed?* perror*)
                              (if consumed?*
                                  perror*
                                  (merge-parse-errors perror perror*)))))
                 (run '(PARSER:BINARY-CHOICE RIGHT) right-parser pstate
                      (lambda (consumed?* pstate* value* perror*)
                        (success '(PARSER:BINARY-CHOICE RIGHT)
                                 win consumed?* pstate* value*
                                 (merge consumed?* perror*)))
                      (lambda (consumed?* pstate* perror*)
                        (failure '(PARSER:BINARY-CHOICE RIGHT)
                                 lose consumed?* pstate*
                                 (merge consumed?* perror*))))))))))

(define (parser:backtrackable parser)
  (lambda (pstate win lose)
    (run 'PARSER:BACKTRACKABLE parser pstate
         win
         (lambda (consumed? pstate* perror)
           (failure 'PARSER:BACKTRACKABLE lose #f pstate
                    (if consumed?
                        (parse-error-with-position
                         perror
                         (parse-state/position pstate))
                        perror))))))

(define (parser:token* processor)
  (lambda (pstate win lose)
    (let ((stream (parse-state/stream pstate))
          (position (parse-state/position pstate))
          (advancer (parse-state/advancer pstate)))
      (if (stream-pair? stream)
          (let ((token (stream-car stream)))
            (processor
             token
             (lambda (value)
               (success* 'PARSER:TOKEN* win #t
                         (make-parse-state (stream-cdr stream)
                                           (advancer position token)
                                           advancer)
                         value))
             (lambda ()
               (failure 'PARSER:TOKEN* lose #f pstate
                        (make-parse-error:unexpected-token token position)))))
          (failure 'PARSER:TOKEN* lose #f pstate
                   (make-parse-error:unexpected-end-of-input position))))))

(define (parser:peek parser)
  (lambda (pstate win lose)
    (run 'PARSER:PEEK parser pstate
         (lambda (consumed? pstate* value perror)
           (success 'PARSER:PEEK win #f pstate value perror))
         (lambda (consumed? pstate* perror)
           (failure 'PARSER:PEEK lose #f pstate perror)))))

(define (parser:end)
  (lambda (pstate win lose)
    (if (stream-null? (parse-state/stream pstate))
        (success* 'PARSER:END win #f pstate '())
        (failure 'PARSER:END lose #f pstate
                 (make-parse-error:trailing-garbage
                  (parse-state/position pstate))))))

(define (parser:complete parser)
  (*parser ((value parser)
            ((parser:end)))
    (parser:return value)))

(define (parser:delayed promise)
  (lambda (pstate win lose)
    (run 'PARSER:DELAYED (force promise) pstate win lose)))

(define (parser:label name parser)
  (lambda (pstate win lose)
    (run name parser pstate
         (lambda (consumed? pstate value perror)
           (success name win consumed? pstate value perror))
         (lambda (consumed? pstate perror)
           (failure name lose consumed? pstate perror)))))

;;;; Syntactic Sugar

;;; In order not to surprise users who write parsers as top-level
;;; variables or mutually recursive procedures, we delay the top-level
;;; of *PARSER forms.  Consider, for example,
;;;
;;;   (define mumble-parser:foo
;;;     (*parser ((frob mumble-parser:frob)
;;;               (mumble-parser:zarquon))
;;;       (parser:return frob)))
;;;
;;;   (define mumble-parser:frob ...),
;;;
;;; which would otherwise have a forward reference at the top level and
;;; cause an error; or
;;;
;;;   (define (mumble-parser:foo bar)
;;;     (*parser ((frob (mumble-parser:frob bar))
;;;               ((mumble-parser:zarquon)))
;;;       (parser:return frob)))
;;;
;;;   (define (mumble-parser:frob bar)
;;;     (parser:choice (mumble-parser:foo bar)
;;;                    ...)),
;;;
;;; which would (and did, for me!) lead to a very confusing infinite
;;; loop that would blow the stack.

(define-syntax *parser
  (syntax-rules ()
    ((*PARSER clauses tail-parser loser)
     (PARSER:ON-FAILURE loser (*PARSER clauses tail-parser)))

    ((*PARSER clauses tail-parser)
     (PARSER:DELAYED (DELAY (**PARSER clauses tail-parser))))))

(define-syntax **parser
  (syntax-rules ()
    ((**PARSER () tail-parser)
     tail-parser)

    ((**PARSER ((parser) more ...) tail-parser)
     (**PARSER ((IGNORE parser) more ...) tail-parser))

    ((**PARSER ((variable parser) more ...) tail-parser)
     (PARSER:EXTEND parser (LAMBDA (variable)
                             (**PARSER (more ...) tail-parser))))))

(define-syntax define-parser
  (syntax-rules ()
    ((DEFINE-PARSER (name . bvl) parser)
     (DEFINE (name . bvl)
       (PARSER:LABEL 'name parser)))

    ((DEFINE-PARSER name parser)
     (DEFINE name
       (PARSER:LABEL 'name parser)))))

;;;; Higher-Level Utilities

(define (parser:return value)
  (parser:epsilon (lambda () value)))

(define (parser:token . mapper-option)
  (parser:token* (if (pair? mapper-option)
                     (let ((mapper (car mapper-option)))
                       (lambda (token win lose)
                         lose           ;ignore
                         (win (mapper token))))
                     (lambda (token win lose)
                       lose             ;ignore
                       (win token)))))

(define (parser:token-if predicate . mapper-option)
  (parser:token* (if (pair? mapper-option)
                     (let ((mapper (car mapper-option)))
                       (lambda (token win lose)
                         (if (predicate token)
                             (win (mapper token))
                             (lose))))
                     (lambda (token win lose)
                       (if (predicate token)
                           (win token)
                           (lose))))))

(define (parser:eqv-token token)
  (parser:token-if (lambda (token*) (eqv? token* token))))

(define (parser:binary-sequence first-parser second-parser)
  (*parser ((ignored first-parser))
    second-parser))

(define (parser:sequence sequent . sequents)
  (reduce-right parser:binary-sequence
                #f                      ; dummy, will be ignored
                (cons sequent sequents)))

(define (parser:choice . alternatives)
  (reduce-right parser:binary-choice (parser:error) alternatives))

(define (parser:deep-choice . alternatives)
  (reduce-map-right parser:binary-choice
                    parser:backtrackable
                    (parser:error)
                    alternatives))

(define (parser:map parser mapper)
  (parser:extend parser
                 (lambda (value)
                   (parser:epsilon (lambda ()
                                     (mapper value))))))

(define (parser:optional default-value parser)
  (parser:choice parser (parser:return default-value)))

(define (parser:optional-noise parser)
  (parser:choice (parser:sequence parser (parser:return #t))
                 (parser:return #f)))

(define (parser:refuse parser error)
  (parser:choice (*parser ((datum parser))
                   (error datum))
                 (parser:return '())))

;;;;; Repetition

(define (parser:repeated parser)
  (let loop ((items '()))
    (parser:choice (*parser ((item parser))
                     (loop (cons item items)))
                   (parser:epsilon (lambda () (reverse items))))))

(define (parser:repeated-until terminal-parser repeated-parser)
  (let loop ((items '()))
    (parser:choice (*parser ((terminal-parser))
                     (parser:return (reverse items)))
                   (*parser ((item repeated-parser))
                     (loop (cons item items))))))

(define (parser:at-most n parser)
  (let loop ((n n) (items '()))
    (let ((final (parser:epsilon (lambda () (reverse items)))))
      (if (zero? n)
          final
          (parser:choice (*parser ((item parser))
                           (loop (- n 1) (cons item items)))
                         final)))))

(define (parser:at-most-until n terminal-parser repeated-parser)
  (let loop ((n n) (items '()))
    (let* ((final (parser:epsilon (lambda () (reverse items))))
           (terminal (parser:sequence terminal-parser final)))
      (if (zero? n)
          terminal
          (parser:choice terminal
                         (*parser ((item repeated-parser))
                           (loop (- n 1) (cons item items))))))))

(define (parser:exactly n parser)
  (let loop ((n n) (items '()))
    (if (zero? n)
        (parser:epsilon (lambda () (reverse items)))
        (*parser ((item parser))
          (loop (- n 1) (cons item items))))))

(define (parser:at-least n parser)
  (*parser ((a (parser:exactly n parser))
            (b (parser:repeated parser)))
    (parser:return (append a b))))

(define (parser:at-least-until n terminal-parser repeated-parser)
  (*parser ((a (parser:exactly n repeated-parser))
            (b (parser:repeated-until terminal-parser repeated-parser)))
    (parser:return (append a b))))

(define (parser:between n m parser)
  (*parser ((a (parser:exactly n parser))
            (b (parser:at-most (- m n) parser)))
    (parser:return (append a b))))

(define (parser:between-until n m terminal-parser repeated-parser)
  (*parser ((a (parser:exactly n repeated-parser))
            (b (parser:at-most-until (- m n)
                                     terminal-parser
                                     repeated-parser)))
    (parser:return (append a b))))

;;;;; Brackets

(define (parser:bracketed left-bracket right-bracket body-parser)
  (*parser ((left-bracket)
            (body body-parser)
            (right-bracket))
    (parser:return body)))

(define (parser:bracketed* left-bracket right-bracket repeated-parser)
  (parser:sequence left-bracket
                   (parser:repeated-until right-bracket repeated-parser)))

;;;; Matching Parsers

;;; This is a bit kludgey, because the matcher combinators are
;;; primitive: they don't track source position for us.

(define (parser:match matcher processor)
  (lambda (pstate win lose)
    (let ((stream (parse-state/stream pstate))
          (position (parse-state/position pstate))
          (advancer (parse-state/advancer pstate)))
      (cond ((match matcher (parse-state/stream pstate))
             => (lambda (stream*)
                  (success* 'PARSER:MATCH
                            win
                            (not (eq? stream* stream))
                            (make-parse-state
                             stream*
                             (advance-tokens stream stream* position advancer)
                             advancer)
                            (processor stream stream*))))
            (else
             (failure 'PARSER:MATCH lose #f pstate
                      (make-parse-error (parse-state/position pstate)
                                        '("Match failed"))))))))

(define (advance-tokens from-stream to-stream position advancer)
  (let loop ((stream from-stream) (position position))
    (if (eq? stream to-stream)
        position
        (loop (stream-cdr stream)
              (advancer position
                        (stream-car stream))))))

(define (parser:match->ignore matcher)
  (parser:match matcher
                (lambda (from-stream to-stream)
                  from-stream to-stream ;ignore
                  '())))

(define (parser:match->list matcher)
  (parser:match matcher
                (lambda (from-stream to-stream)
                  (let recur ((stream from-stream))
                    (if (eq? stream to-stream)
                        '()
                        (cons (stream-car stream)
                              (recur (stream-cdr stream))))))))

;;;; Random Utilities

(define (reduce-right combiner right-identity list)
  (reduce-map-right combiner
                    (lambda (element) element)
                    right-identity
                    list))

(define (reduce-map-right combiner mapper right-identity list)
  (if (pair? list)
      (let recur ((list list))
        (let ((item (mapper (car list)))
              (tail (cdr list)))
          (if (pair? tail)
              (combiner item (recur tail))
              item)))
      right-identity))
