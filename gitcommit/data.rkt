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


  --- Documentation

  This file is the first to be loaded. If the argvs, commit file or
  git executable is not found, the program will be exited. The
  intention is to speed up the execution time if the right
  configuration is not ensured.
|#

(provide commit-file-content
         edit-commit
         staged-files
         context-ref
         context?)

(require gitcommit/exceptions
         racket/contract
         racket/system
         racket/string
         racket/port
         racket/dict
         racket/file
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax))


;; --- Internal variables

(define argvs
  (let ([arguments (vector->list (current-command-line-arguments))])
    (if (and (> (length arguments) 0)
             (< (length arguments) 3))
        arguments
      (exit 0))))

(define commit-file
  (let ([path (car argvs)])
    (if (file-exists? path)
        path
      (exit 0))))

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


;; --- External implementation

(define staged-files
  (string-split
    (with-output-to-string
      (λ ()
        (system* git-executable "diff" "--name-only" "--cached")))
    "\n"))

(define commit-file-content
  (file->string commit-file))

(define/contract (edit-commit procedure)
  (-> procedure? void?)
  (call-with-output-file commit-file #:exists 'replace
    (λ (out)
      (write-string (procedure commit-file-content) out)))
  (void))

(define/contract (context-ref reference)
  (-> symbol? any)
  (hash-ref context reference))

(define/contract (context? reference)
  (-> symbol? any)
  (and (not (null? (hash-ref context reference)))
       (hash-ref context reference)))


;; --- Internal implementation

(define context (make-hash))

(define context-namespace (make-base-namespace))

(define-syntax (define-context stx)
  (syntax-parse stx
    [(_ name contract)
     #:with setter (format-id stx "set-~a!" #'name)
     #'(begin
         ;; Set hash to `null' and namespace variables to prevent
         ;; unbound errors.
         (hash-set! context 'name null)
         (parameterize ([current-namespace context-namespace])
           (namespace-set-variable-value! 'name null))
         ;; Define contract to change the variable value into
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

(define-context use-substitutions
  (-> boolean? any))

(define-context default-component
  (-> string? any))

(define-context components
  (-> (*list/c (cons/c string? path-string?)) any))

(define-context markers
  (-> (*list/c string?) any))

(with-handlers
  ([exn:fail:contract?
    (λ (exn)
      (raise-context-error
       (exn-message exn)))])
  (parameterize ([current-namespace context-namespace])
    (load context-file)
    (refresh-context)
    (void)))
