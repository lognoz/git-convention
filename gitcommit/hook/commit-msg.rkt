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

(require racket/string
         racket/list
         gitcommit/data
         gitcommit/utils
         gitcommit/exceptions
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax))


;; --- Implementation

(define initial-commit-message (string-trim commit-file-content))

(define commit-message initial-commit-message)


;; --- Implementation (replacement)

(define replacements (make-hash))

(define replacement-files null)

(define replacement-directories null)

(define (set-replacements)
  (let ([files null]
        [directories null])
    (for/list ([path staged-files])
      (set! files (append files (list (file-name-without-extension path))))
      (set! directories (append directories (list (directory-from-path path)))))
    (set!-values (replacement-files replacement-directories)
                 (values (remove-duplicates files)
                         (remove-duplicates directories)))))

(define (plural-replacement lst singular [plural #f])
  (if (> (length lst) 1)
      (if (not plural)
          (string-append singular "s")
        plural)
    singular))

(define (replace-in-commit reference func)
  (when (string-contains? commit-message reference)
    (when (null? replacement-files)
      (set-replacements))
    (set! commit-message (string-replace commit-message reference (func)))))

(define (apply-replacements)
  (for/list ([parameters (hash->list replacements)])
    (replace-in-commit (car parameters) (cdr parameters)))
  (void))

(define-syntax (define-replacement syntax-object)
  (syntax-parse syntax-object
    [(_ name reference getter)
     #'(hash-set! replacements 'reference
                  (λ ()
                    getter))]))

(define-replacement files "(f)"
  (convert-to-string replacement-files))

(define-replacement directories "(d)"
  (convert-to-string replacement-directories))

(define-replacement directories-and-context "(d-)"
  (let ([text (convert-to-string replacement-directories)]
        [context (plural-replacement replacement-directories "directory" "directories")])
    (string-append text " " context)))

(define-replacement files-and-context "(f-)"
  (let ([text (convert-to-string replacement-files)]
        [context (plural-replacement replacement-files "file")])
    (string-append text " " context)))

(apply-replacements)

(unless (string=? commit-message initial-commit-message)
  (edit-commit
    (λ (content)
      commit-message)))


;; --- Implementation (validation)

(define matched-component null)

(define header-commit-message
  (car (string-split commit-message "\n")))

(define-syntax (regexp-match-commit syntax-object)
  (syntax-case syntax-object ()
    ((_ a ...)
     #'(regexp-match (regexp (string-append a ...))
                     header-commit-message))))

(define (validate-component)
  (let* ([default-component (cons (context-ref 'default-component) ".+")]
         [components (append (context-ref 'components) (list default-component))])
    (set! matched-component
          (for/first ([parameters components]
                      #:when (regexp-match-commit "^" (car parameters) ": .+"))
            (car parameters)))
    (unless matched-component
      (raise-commit-error 'component
                          (convert-to-string
                           (map (λ (x)
                                  (format "~s" (car x)))
                                components))))))

(define (validate-marker)
  (let ([markers (context-ref 'markers)]
        [component (if matched-component (string-append matched-component ": ") "")])
    (unless (for/first ([marker markers]
                        #:when (regexp-match-commit "^" component marker " .+"))
              marker)
      (raise-commit-error 'marker
                          (convert-to-string
                           (map (λ (x)
                                  (format "~s" x))
                                markers))))))

(when (and (context? 'components) (context? 'default-component))
  (validate-component))

(when (context? 'markers)
  (validate-marker))
