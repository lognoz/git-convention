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

(require file/glob
         racket/port
         racket/system
         racket/string
         gitcommit/data)


;; --- Implementation

(define (matched-component)
  (let* ([staged-files-length (length staged-files)]
         [matched-component #f])
    (for/list ([component (context-ref 'components)]
               #:break matched-component)
      (let* ([title (car component)]
             [regex (cdr component)]
             [matched-length 0])
        (for/list ([staged-file staged-files])
          (when (glob-match? regex staged-file)
            (set! matched-length (+ matched-length 1)))
          (when (= matched-length staged-files-length)
            (set! matched-component title)))))
    (or matched-component (context-ref 'default-component))))


;; --- Hook

(let ([refresh-commit? #f])
  ;; Add default commit message if it's empty.
  (when (and (void? header-commit-message)
            (context? 'default-commit-message))
    (set-header-commit-message (context-ref 'default-commit-message))
    (set! refresh-commit? #t))
  ;; Add matched component if it's not present.
  (when (and (context? 'components)
            (context? 'default-component)
            (not (regexp-match-commit "^[A-Z1-9].+:")))
    (set-header-commit-message (string-append (matched-component) ": " header-commit-message))
    (set! refresh-commit? #t))
  ;; Edit commit.
  (when refresh-commit?
    (refresh-commit)))
