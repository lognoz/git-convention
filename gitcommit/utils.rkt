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

(provide file-name
         file-name-without-extension
         directory-from-path
         regex-from-path
         convert-to-string)


;; --- Requirements

(require racket/path
         racket/string)


;; --- Implementation (path)

(define (file-name path)
  (path->string (file-name-from-path path)))

(define (file-name-without-extension path)
  (path->string (path-replace-extension (file-name path) "")))

(define (regex-from-path path)
  (let ([path (path->string path)]
        [directory (path->string (current-directory))])
    (regexp (string-append "^" (string-replace path directory "") "$"))))

(define (directory-from-path path)
  (let ([directory (string-trim (string-replace path (file-name path) "") "/")])
    (if (string=? directory "")
        "root"
      directory)))


;; --- Implementation (list)

(define (convert-to-string lst)
  (string-join lst ", " #:before-last " and "))
