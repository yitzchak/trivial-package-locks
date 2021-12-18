(defpackage #:trivial-package-locks
  (:use #:common-lisp)
  (:documentation "A standard interface to the various package lock implementations.")
  (:export #:package-locked-p
           #:without-package-locks
           #:with-unlocked-packages))

