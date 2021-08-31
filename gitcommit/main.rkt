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

(provide default-markers
         generate-components)


;; --- Requirements

(require gitcommit/utils
         file/glob
         racket/list
         racket/path
         racket/string
         racket/contract)


;; --- Implementation

(define default-markers
  '("Add"
    "Bump"
    "Change"
    "Fix"
    "Move"
    "Refactor"
    "Remove"
    "Rephrase"))

(define/contract (generate-components pattern [procedure null])
  (case-> (-> glob/c list?)
          (-> glob/c procedure? list?))
    (let ([files (glob pattern)]
          [components null])
      (for/list ([path files])
        (let ([filename (file-name path)]
              [filename-body (file-name-without-extension path)]
              [regex (regex-from-path path)]
              [component null])
          (unless (null? procedure)
            (set! component (procedure filename-body)))
          (when (null? component)
            (set! component (string-titlecase filename-body)))
          (set! components
                (append components
                        (list (cons component regex))))))
      components))
