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

(provide components/files-recursively)


;; --- Requirements

(require file/glob
         racket/path
         racket/list
         racket/string
         racket/contract)


;; --- Implementation

(define/contract (components/files-recursively pattern)
  (-> glob/c list?)
  (let ([files (glob pattern)]
        [components null])
    (for/list ([path files])
      (let* ([filename (filename path)]
             [filename-body (filename-body filename)]
             [regex (string-replace (path->string path)
                                    (path->string (current-directory))
                                    "")])
        (set! components (append components
                                 (list (cons (string-titlecase filename-body)
                                             (regexp regex)))))))
    components))

(define (filename path)
  (path->string (file-name-from-path path)))

(define (filename-body filename)
  (path->string (path-replace-extension filename "")))
