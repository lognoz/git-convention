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

(provide load-context
         matched-component)


;; --- Requirements

(require racket/port
         racket/string
         racket/system
         gitcommit/data
         gitcommit/exceptions)


;; --- Implementation

(define (load-context)
  (with-handlers
    ([exn:fail:contract?
      (λ (exn)
        (raise-context-error
         (exn-message exn)))])
    (parameterize ([current-namespace context-namespace])
      (load context-file)
      (refresh-context)
      (void))))

(define (matched-component)
  (let* ([staged-files (string-split
                         (with-output-to-string
                           (λ ()
                             (system* git-executable "diff" "--name-only" "--cached")))
                         "\n")]
         [staged-files-length (length staged-files)]
         [matched-component null])
    (for ([component (in-list (components))])
      (let* ([title (car component)]
             [regex (cdr component)]
             [matched-length 0])
        (for ([staged-file (in-list staged-files)])
          (when (regexp-match regex staged-file)
            (set! matched-length (+ matched-length 1)))
          (when (= matched-length staged-files-length)
            (set! matched-component title)))))
    (or matched-component (default-component))))
