(in-package #:trivial-package-locks/test)

(define-test package-locked-p.1
  (let* ((pkg (make-package "FU1"))
         (bar (intern "BAR" pkg)))
    (false (trivial-package-locks:package-locked-p pkg))
    (setf (fdefinition bar) (lambda ()))
    (true (setf (trivial-package-locks:package-locked-p pkg) t))
    #+package-locks
      (true (trivial-package-locks:package-locked-p pkg))
    #-package-locks
      (false (trivial-package-locks:package-locked-p pkg))
    #+package-locks
      (fail (setf (fdefinition bar) (lambda () t)))
    (false (setf (trivial-package-locks:package-locked-p pkg) nil))
    (true (setf (fdefinition bar) (lambda () t)))
    (true (funcall (fdefinition bar)))
    (true (delete-package pkg))))

(define-test without-package-locks.1
  (let* ((pkg (make-package "FU2"))
         (bar (intern "BAR" pkg)))
    (setf (fdefinition bar) (lambda ()))
    (true (setf (trivial-package-locks:package-locked-p pkg) t))
    #+package-locks
      (true (trivial-package-locks:package-locked-p pkg))
    (trivial-package-locks:without-package-locks
      #-global-package-locks (false (trivial-package-locks:package-locked-p pkg))
      (true (setf (fdefinition bar) (lambda () t)))
      (true (funcall (fdefinition bar)))
      (true (delete-package pkg)))))

(define-test with-unlocked-packages.1
  (let* ((pkg (make-package "FU3"))
         (bar (intern "BAR" pkg)))
    (setf (fdefinition bar) (lambda ()))
    (true (setf (trivial-package-locks:package-locked-p pkg) t))
    #+package-locks
      (true (trivial-package-locks:package-locked-p pkg))
    (trivial-package-locks:with-unlocked-packages ("FU3")
      (false (trivial-package-locks:package-locked-p pkg))
      (true (setf (fdefinition bar) (lambda () t)))
      (true (funcall (fdefinition bar)))
      (true (delete-package pkg)))))

