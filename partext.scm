;;; -*- Mode: Scheme; scheme48-package: text-parser-combinators -*-

;;;; Parsing Tools
;;;; Combinators for Parsing Text

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (parse-file parser pathname lose)
  (call-with-input-file pathname
    (lambda (input-port)
      (parse-stream parser
                    (let recur ()
                      (lazy (let ((char (read-char input-port)))
                              (if (eof-object? char)
                                  stream-nil
                                  (stream-cons char (recur))))))
                    (cons 1 1)
                    (lambda (position char)
                      (let ((line (car position))
                            (column (cdr position)))
                        (if (char=? char #\newline)
                            (cons (+ line 1) 1)
                            (cons line (+ column 1)))))
                    (lambda (value stream)
                      stream            ;ignore
                      value)
                    lose))))

(define (parse-string parser string lose)
  (parse-stream parser
                (string->stream string)
                0
                (lambda (position token) token (+ position 1))
                (lambda (value stream)
                  stream                ;ignore
                  value)
                lose))

(define (parser:char)
  (parser:token-if char?))

(define (parser:char-test procedure argument)
  (parser:token-if (lambda (token)
                     (and (char? token)
                          (procedure argument token)))))

(define (char/=? a b) (not (char=? a b)))
(define (char-ci/=? a b) (not (char-ci=? a b)))

(define (parser:char= char) (parser:char-test char=? char))
(define (parser:char/= char) (parser:char-test char/=? char))
(define (parser:char-ci= char) (parser:char-test char-ci=? char))
(define (parser:char-ci/= char) (parser:char-test char-ci/=? char))

(define (parser:char-in-set char-set)
  (parser:char-test char-set-contains? char-set))

(define (parser:char-not-in-set char-set)
  (parser:char-test (lambda (char-set char)
                      (not (char-set-contains? char-set char)))
                    char-set))

(define (parser:list->string parser)
  (parser:map parser list->string))

(define (parser:reverse-list->string parser)
  (parser:map parser
              (lambda (list)
                (list->string (reverse list)))))

(define (parser:match->string matcher)
  ;++ This could be improved dramatically if we improved matchers.
  (parser:list->string (parser:match->list matcher)))

(define (parser:string-compare parser:char-compare string)
  (let recur ((index 0))
    (if (= index (string-length string))
        (parser:return string)
        (*parser (((parser:char-compare (string-ref string index))))
          (recur (+ index 1))))))

(define (parser:string= string)
  (parser:string-compare parser:char= string))

(define (parser:string-ci= string)
  (parser:string-compare parser:char-ci= string))
