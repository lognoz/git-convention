#lang racket/base

#|
  Copyright (C) 2021 Marc-Antoine Loignon

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 3, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program see the file LICENSE. If not see
  <http://www.gnu.org/licenses/>.
|#

(provide raise-init-error
         raise-commit-error
         raise-context-error)

(require racket/string
         racket/list)


;; --- Implementation

(define (raise-init-error message-str)
  (raise-user-error 'gitcommit
    (error-template message-str)))

(define (raise-commit-error sym suggestions)
  (raise-user-error 'gitcommit
    (error-template (string-append "You need to define a " (symbol->string sym) " (" suggestions ")."))))

(define (raise-context-error message-str)
  (let* ([message (string-split message-str "\n")]
         [context (context-match #rx"^set-(.+)!" (first message))]
         [who null]
         [expected null]
         [given null])
    (if (not (void? context))
        (begin
          (set! who (string->symbol context))
          (set! expected (string-trim (context-match #rx"expected:? a?(.+)" (second message)) "?"))
          (set! given (context-match #rx"given: (.+)" (third message)))
          (raise-user-error who
            (error-template (format "Expected to be defined with a ~a, ~a is given." expected given))))
      (raise-user-error message-str))))

(define (error-template message-str)
  (string-append message-str "\n\n"
                 "For more information please see the link below:\n"
                 "https://github.com/lognoz/gitcommit"))

(define (context-match regex message)
  (let ([matched (regexp-match regex message)])
    (when matched
      (second matched))))
