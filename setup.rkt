#lang racket

(require "lib.rkt")

; (p (rd "config" #:to ".config"))

#|
(p (d "local" #:to ".local"
      (d "share"
         (rd "applications"))))
|#

; (p (l "bin"))

(p (rd "dotfiles" #:to "~" #:prefix "."))

; (p (rd "xournal" #:to ".xournal"))

(p (rd "spacemacs-private" #:to ".emacs.d/private"))
