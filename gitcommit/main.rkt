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

(provide prepare-commit-msg)


;; --- Requirements

(require racket/file
         racket/string
         gitcommit/data
         gitcommit/base
         gitcommit/exceptions)


;; --- Implementation

(define (prepare-commit-msg)
  (load-context)
  (if (and (components?)
           (default-component?)
           (> (length argvs) 0)
           (< (length argvs) 3))
      (let* ([path (car argvs)]
             [file-content
               (if (file-exists? path)
                   (file->string path)
                 (exit 0))]
             [matched-component (matched-component)])
        (call-with-output-file path #:exists 'replace
          (λ (out)
            (write-string (string-append matched-component ": " file-content) out)))
        (void))
    (exit 0)))
