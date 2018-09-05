#lang racket

(provide (rename-out [process p])
         d
         rd
         l)

(require (for-syntax syntax/parse))

(define FAILURE-CODE 1)

(struct dir (name args to pref) #:transparent)
(struct read-dir (name to pref) #:transparent)
(struct lnk (name to) #:transparent)

(define (d/f name args #:to [to #f] #:prefix [pref ""])
  (dir name args to pref))

(define (rd/f name #:to [to #f] #:prefix [pref ""])
  (read-dir name to pref))

(define (l/f name #:to [to #f])
  (lnk name to))

(define-syntax (rd stx)
  (syntax-parse stx
    [(_ name (~and (~seq kw-list ...)
                   (~seq (~seq k:keyword e:expr) ...)))
     #'(rd/f name kw-list ...)]))

(define-syntax (d stx)
  (syntax-parse stx
    [(_ name (~and (~seq kw-list ...)
                   (~seq (~seq k:keyword e:expr) ...)) children ...)
     #'(d/f name (list children ...) kw-list ...)]))

(define-syntax (l stx)
  (syntax-parse stx
    [(_ name (~and (~seq kw-list ...)
                   (~seq (~seq k:keyword e:expr) ...)))
     #'(l/f name kw-list ...)]))

(define (abs? path)
  (ormap (lambda (p) (string-prefix? (path->string path) p))
         '("~" "/")))

(define (copy src dst)
  (let* ([src (simple-form-path (fakepath->path src))]
         [dst (fakepath->path dst)]
         [dst (expand-user-path
               (cond
                 [(abs? dst) dst]
                 [else (build-path "~" dst)]))])
    (make-parent-directory* dst)
    (cond
      [(and (link-exists? dst) (= (file-or-directory-identity src)
                                  (file-or-directory-identity dst)))
       (printf "Skipping existing link ~a\n" dst)]
      [(file-exists? dst) (eprintf "ERROR: ~a exists when linking ~a\n"
                                   dst src)
                          (exit FAILURE-CODE)]
      [else (printf "Creating a link ~a\n" dst)
            (make-file-or-directory-link src dst)])))

(define (fakepath->path path)
  (apply build-path (reverse path)))

(define (dir-filesystem path)
  (for/list ([path (directory-list (simple-form-path
                                    (fakepath->path path)))])
    (l (path->string path))))

(define (process cfg)
  (let loop ([cfg cfg] [src-path '()] [dst-path '()] [pref ""])
    (define (get-src name) (cons name src-path))
    (define (get-dst name to) (cons (string-append pref (or to name)) dst-path))
    (match cfg
      [(lnk name to) (copy (get-src name) (get-dst name to))]
      [(dir name args to next-pref)
       (for ([cfg args])
         (loop cfg
               (get-src name)
               (get-dst name to)
               next-pref))]
      [(read-dir name to next-pref)
       (for ([cfg (dir-filesystem (get-src name))])
         (loop cfg
               (get-src name)
               (get-dst name to)
               next-pref))])))

