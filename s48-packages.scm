;;; -*- Mode: Scheme; scheme48-package: (config) -*-

;;;; Parsing Tools
;;;; Package Definitions

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-structure matcher-combinators matcher-combinators-interface
  (open scheme
        srfi-1                          ;List Library
        lazy-streams
        )
  (optimize auto-integrate)
  (files matcomb))

(define-structure text-matcher-combinators text-matcher-combinators-interface
  (open scheme
        srfi-14                         ;Character-Set Library
        lazy-streams
        matcher-combinators
        )
  (optimize auto-integrate)
  (files mattext))

(define-structure parser-combinators parser-combinators-interface
  (open scheme
        srfi-1                          ;list-lib
        srfi-9                          ;define-record-type
        lazy-streams
        parse-errors
        matcher-combinators
        )
  (optimize auto-integrate)
  (files parcomb))

(define-structure text-parser-combinators text-parser-combinators-interface
  (open scheme
        srfi-14                         ;Character-Set Library
        laziness
        lazy-streams
        parser-combinators
        )
  (optimize auto-integrate)
  (files partext))

(define-structure parse-errors parse-errors-interface
  (open scheme
        srfi-9                          ;define-record-type
        )
  (optimize auto-integrate)

  (open (subset extended-ports (call-with-string-output-port))
        (subset display-conditions (limited-write)))
  (begin
    (define (limited-write-to-string object)
      (call-with-string-output-port
        (lambda (output-port)
          (limited-write object output-port 5 3))))
    )

  (files perror)

  (open (subset define-record-types (define-record-discloser)))
  (begin
    (define-record-discloser <parse-error>
      (lambda (perror)
        `(PARSE-ERROR (AT ,(parse-error/position perror))
                      ,@(parse-error/messages perror))))
    ))

(define-structure lazy-streams lazy-streams-interface
  (open (modify scheme (hide delay force))
        laziness
        )
  (optimize auto-integrate)
  (files stream))

(define-structure laziness laziness-interface
  (open (modify scheme (hide delay force))
        srfi-9                          ;define-record-type
        )
  (optimize auto-integrate)
  (files lazy))
