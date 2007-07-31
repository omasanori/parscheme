;;; -*- Mode: Scheme; scheme48-package: parser-combinators -*-

;;;; Parsing Tools
;;;; Parser Combinators

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-record-type <parse-state>
    (make-parse-state stream position advancer stack)
    parse-state?
  (stream parse-state/stream)
  (position parse-state/position)
  (advancer parse-state/advancer)
  (stack parse-state/stack))

(define (parse-stream parser stream position advancer win lose)
  (run 'PARSE parser (initial-parse-state stream position advancer lose)
       (lambda (pstate value perror)
         perror                         ;ignore
         (win value (parse-state/stream pstate)))))

(define (initial-parse-state stream position advancer lose)
  (make-parse-state stream position advancer
                    (lambda (pstate perror)
                      (lose perror (parse-state/stream pstate)))))

(define (parse-state-with-stack pstate stack)
  (make-parse-state (parse-state/stream pstate)
                    (parse-state/position pstate)
                    (parse-state/advancer pstate)
                    stack))

(define (parse-state/end? pstate)
  (stream-null? (parse-state/stream pstate)))

(define (parse-state/next-token pstate)
  (stream-car (parse-state/stream pstate)))

(define (parse-state/advance pstate)
  (let ((stream (parse-state/stream pstate))
        (position (parse-state/position pstate))
        (advancer (parse-state/advancer pstate))
        (stack (parse-state/stack pstate)))
    (make-parse-state (stream-cdr stream)
                      (advancer position (stream-car stream))
                      advancer
                      (flush-parse-stack stack))))

;;;; Parse Stack Management

;;; The parse stack is a list terminated by a final losing continuation
;;; or a parse state to which to backtrack.  The elements of the stack
;;; are losing continuations.
;;;
;;; This page was substantially more complex until I decided to use a
;;; flat continuation-per-frame representation of the stack, rather
;;; than a list of lists of continuations representing choices.  I
;;; originally tried the more complex format in the hope that it would
;;; benefit PARSER:CHOICE to to as little preprocessing as possible,
;;; but then I realized (1) it has to do preprocessing anyway, and (2)
;;; this thing is not going to be fast unless it runs in a fast Scheme
;;; that properly integrates procedures.

(define (parse-state/push pstate continuation)
  (parse-state-with-stack pstate
                          (cons continuation (parse-state/stack pstate))))

(define (parse-state/pop pstate)
  (let ((stack (parse-state/stack pstate)))
    (cond ((pair? stack)                ;+++ Common case first.
           (parse-state/*pop pstate stack))
          ((parse-state? stack)         ;Preserved parse state.
           (parse-state/*pop stack (parse-state/stack stack)))
          (else                         ;Final continuation.
           (values stack pstate)))))

(define (parse-state/*pop pstate stack)
  (values (car stack)
          (parse-state-with-stack pstate (cdr stack))))

(define (parse-state/preserve pstate)
  (if (pair? (parse-state/stack pstate))
      (parse-state-with-stack pstate pstate)
      pstate))

(define (parse-state/flush pstate)
  (parse-state-with-stack pstate (flush-parse-stack pstate)))

(define (flush-parse-stack stack)
  (let loop ((stack stack))
    (if (pair? stack)
        (loop (cdr stack))
        stack)))

;;;; Parser Drivers

;;; This little abstraction for driving the parser exists for the
;;; purpose of carefully controlling tracing behaviour.  If the
;;; conditionals on *PARSE-TRACE?* are eliminated, all of these
;;; procedures will integrate nicely to straight tail calls with no
;;; indirection through a top-level reference.  (Scheme48 is too stupid
;;; to eliminate the conditionals even when it can *prove* that
;;; *PARSE-TRACE?* will be false, if ENABLE-PARSE-TRACE and
;;; DISABLE-PARSE-TRACe are commented out so that there is no
;;; assignment to it.  Blah.)
;;;
;;; The tracing is not especially well-engineered at the moment; it is
;;; mostly useful for me to scan to get an idea of whether the parser
;;; is being remotely sane.  However, the way I have written the code
;;; below makes it easy for a better-engineered mechanism than what is
;;; on this page to be substituted trivially.

(define (run marker parser pstate win)
  (if *parse-trace?*
      (begin
        (write `(RUN ,marker))
        (newline)))
  (parser pstate win))

(define (success* marker win pstate value)
  (success marker win pstate value
           (make-parse-error:unknown (parse-state/position pstate))))

(define (success marker win pstate value perror)
  (if *parse-trace?*
      (begin
        (write `(SUCCESS ,marker -> ,value
                         ,(parse-error/position perror)
                         ,@(parse-error/messages perror)))
        (newline)))
  (win pstate value perror))

(define (failure marker pstate perror)
  (if *parse-trace?*
      (begin
        (write `(FAILURE ,marker
                         ,(parse-error/position perror)
                         ,@(parse-error/messages perror)))
        (newline)))
  (receive (lose pstate) (parse-state/pop pstate)
    (lose pstate perror)))

(define *parse-trace?* #f)

(define (enable-parse-trace) (set! *parse-trace?* #t))
(define (disable-parse-trace) (set! *parse-trace?* #f))

(define (with-parse-trace thunk)
  (if *parse-trace?*
      (thunk)
      (dynamic-wind enable-parse-trace
                    thunk
                    disable-parse-trace)))

;;;; Primitive Parser Combinators

(define (parser:epsilon thunk)
  (lambda (pstate win)
    (success* 'PARSER:EPSILON win pstate (thunk))))

(define (parser:error . messages)
  (lambda (pstate win)
    win                                 ;ignore
    (failure 'PARSER:ERROR
             pstate
             (make-parse-error (parse-state/position pstate)
                               messages))))

(define (parser:on-failure lose parser)
  (lambda (pstate win)
    (run 'PARSER:ON-FAILURE parser (parse-state/push pstate lose) win)))

(define (parser:extend parser extender)
  (lambda (pstate win)
    (run 'PARSER:EXTEND parser pstate
         (lambda (pstate value perror)
           (run '(PARSER:EXTEND EXTENSION) (extender value)
                (parse-state/push pstate
                  (lambda (pstate* perror*)
                    (failure '(PARSER:EXTEND EXTENSION)
                             pstate*
                             (merge-parse-errors perror perror*))))
                win)))))

(define (parser:binary-choice left-parser right-parser)
  (lambda (pstate win)
    (run '(PARSER:BINARY-CHOICE LEFT) left-parser
         (parse-state/push pstate
           (lambda (pstate perror)
             (run '(PARSER:BINARY-CHOICE RIGHT) right-parser
                  (parse-state/push pstate
                    (lambda (pstate* perror*)
                      (failure 'PARSER:CHOICE
                               pstate
                               (merge-parse-errors perror perror*))))
                  win)))
         win)))

(define (parser:backtrackable parser)
  (lambda (pstate win)
    (run 'PARSER:BACKTRACKABLE parser
         (parse-state/preserve
          (parse-state/push pstate
            (lambda (pstate perror)
              (failure 'PARSER:BACKTRACKABLE
                       pstate
                       (parse-error-with-position
                        perror
                        (parse-state/position pstate))))))
         (lambda (pstate* value perror)
           (success 'PARSER:BACKTRACKABLE
                    win
                    (parse-state-with-stack pstate* (parse-state/stack pstate))
                    value
                    perror)))))

;;;;; Odds and Ends

;;; This totally loses because most Scheme systems won't integrate
;;; PARSER:TOKEN*.  Phooey.

(define (parser:token* processor)
  (lambda (pstate win)
    (if (parse-state/end? pstate)
        (failure 'PARSER:TOKEN*
                 pstate
                 (make-parse-error:unexpected-end-of-input
                  (parse-state/position pstate)))
        (let ((token (parse-state/next-token pstate)))
          (processor token
                     (lambda (value)
                       (success* 'PARSER:TOKEN*
                                 win
                                 (parse-state/advance pstate)
                                 value))
                     (lambda ()
                       (failure 'PARSER:TOKEN*
                                pstate
                                (make-parse-error:unexpected-token
                                 token
                                 (parse-state/position pstate)))))))))

(define (parser:peek parser)
  (lambda (pstate win)
    (run 'PARSER:PEEK parser
         (parse-state/push pstate
           (lambda (pstate* perror*)
             pstate*                    ;ignore
             (failure 'PARSER:PEEK
                      pstate
                      (parse-error-with-position
                       perror*
                       (parse-state/position pstate)))))
         (lambda (pstate* value perror)
           pstate*                      ;ignore
           (success 'PARSER:PEEK win pstate value perror)))))

(define (parser:end)
  (lambda (pstate win)
    (if (parse-state/end? pstate)
        (success* 'PARSER:END win pstate '())
        (failure 'PARSER:END
                 pstate
                 (make-parse-error:trailing-garbage
                  (parse-state/position pstate))))))

(define (parser:complete parser)
  (*parser ((value parser)
            ((parser:end)))
    (parser:return value)))

(define (parser:delayed promise)
  (lambda (pstate win)
    (run 'PARSER:DELAYED (force promise) pstate win)))

;;; This will label only the entrance to the parse state.  To label
;;; exit as well, use PARSER:ON-FAILURE or PARSER:EXTEND.

(define (parser:label name parser)
  (lambda (pstate win)
    (run name parser pstate win)))

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
;;; loop that would blow the stack.  Unfortunately, this leads to a
;;; dreadful inefficiency which I am powerless to avoid.  (I'm not sure
;;; precisely what impact this has on performance, though.)
;;;
;;; I am not sure whether the code on the following pages ought to use
;;; DEFINE-PARSER.  (Primarily, it would clutter up traces, but that is
;;; sometimes what I want.  Ah, how nice it would be to have a high-
;;; level mechanism for describing precisely and accurately the traces
;;; that I want to see at exactly the levels I want to see.)

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
  (syntax-rules (*PARSER)
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

;;; Did I mean to populate this page with anything else?  It's awfully
;;; bare.

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
;;; primitive: they don't track source position for us.  Also, this
;;; procedure is three lines too long.

(define (parser:match matcher processor)
  (lambda (pstate win)
    (let ((stream (parse-state/stream pstate))
          (position (parse-state/position pstate))
          (advancer (parse-state/advancer pstate))
          (stack (parse-state/stack pstate)))
      (cond ((match matcher (parse-state/stream pstate))
             => (lambda (stream*)
                  (success* 'PARSER:MATCH
                            win
                            (make-parse-state
                             stream*
                             (advance-tokens stream stream* position advancer)
                             advancer
                             (if (not (eq? stream stream*))
                                 (flush-parse-stack stack)
                                 stack))
                            (processor stream stream*))))
            (else
             (failure 'PARSER:MATCH
                      pstate
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
