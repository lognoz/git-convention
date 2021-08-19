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

(provide (all-defined-out))


;; --- Requirements

(require gitcommit/exceptions
         racket/contract
         racket/system
         racket/string
         racket/port
         racket/dict
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax))


;; --- Implementation

(define argvs
  (vector->list (current-command-line-arguments)))

(define git-executable
  (or (find-executable-path "git")
      (raise-init-error (format "Required program ~s not found in your path" "git"))))

(define root-directory
  (let* ([proc (process* git-executable "rev-parse" "--show-toplevel")]
         [stdout (port->string (car proc))]
         [directory (string-trim stdout "\n")])
    (if (directory-exists? directory)
        directory
      (raise-init-error "Not a git repository (or any parent up to mount point /usr)"))))

(define context-file
  (let ([path (string-append root-directory "/.gitcommit")])
    (if (file-exists? path)
        path
      (exit 0))))


;; --- Implementation (context)

(define context (make-hash))

(define context-namespace (make-base-namespace))

(define-syntax (define-context stx)
  (syntax-parse stx
    [(_ name contract)
     #:with getter (format-id stx "~a" #'name)
     #:with predicate (format-id stx "~a?" #'name)
     #:with setter (format-id stx "set-~a!" #'name)
     #'(begin
         ;; Variable to `null' to prevent unbound errors.
         (hash-set! context 'name null)
         (parameterize ([current-namespace context-namespace])
           (namespace-set-variable-value! 'name null))
         ;; Function to checks if value exists.
         (define (predicate)
           (not (null? (hash-ref context 'name))))
         ;; Function to get the context value.
         (define (getter)
           (hash-ref context 'name))
         ;; Contract definition to change the variable value into
         ;; namespace.
         (define/contract (setter value)
           contract
           (hash-set! context 'name value)))]))

(define (refresh-context)
  (for/list ([parameters (hash->list context)])
    (let* ([reference (car parameters)]
           [setter (string->symbol (format "set-~a!" reference))]
           [context-value (parameterize ([current-namespace context-namespace])
                            (namespace-variable-value reference))])
      (unless (null? context-value)
        ((eval setter (variable-reference->namespace (#%variable-reference))) context-value))))
    context)

(define-context default-component
  (-> string? any))

(define-context components
  (-> (*list/c (cons/c string? regexp?)) any))

(define-context markers
  (-> (*list/c string?) any))
