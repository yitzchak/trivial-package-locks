(defpackage #:trivial-package-locks
  (:use #:common-lisp)
  (:documentation "A standard interface to the various package lock implementations.")
  (:nicknames #:package-locks)
  (:export #:package-implements-package-p
           #:package-implementation-packages
           #:package-locked-p
           #:without-package-locks
           #:with-locked-packages
           #:with-unlocked-packages))

