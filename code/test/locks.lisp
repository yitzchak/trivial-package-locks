(in-package #:trivial-package-locks/test)

(defmacro with-test-packages ((&rest vars) &body body)
  `(let* ,(loop for (package-binding exports-binding) on vars by #'cddr
                for package-var = (if (listp package-binding)
                                      (first package-binding)
                                      package-binding)
                for package-name = (if (listp package-binding)
                                       (second package-binding)
                                       '(gensym))
                for exports-var = (if (listp exports-binding)
                                      (first exports-binding)
                                      exports-binding)
                for exports-name = (if (listp exports-binding)
                                       (second exports-binding)
                                       (symbol-name (gensym)))
                collect (list package-var `(make-package ,package-name))
                when exports-var
                  collect (list exports-var `(list (intern ,exports-name ,package-var))))
     ,@(loop for (package-binding exports-binding) on vars by #'cddr
             collect `(export ,(if (listp exports-binding)
                                   (first exports-binding)
                                   exports-binding)
                               ,(if (listp package-binding)
                                    (first package-binding)
                                    package-binding)))
     ,@body))

(define-test package-locked-p
  (with-test-packages (pkg exp)
    (false (trivial-package-locks:package-locked-p pkg))
    (true (setf (trivial-package-locks:package-locked-p pkg) t))
    #+package-locks
      (true (trivial-package-locks:package-locked-p pkg))
    #-package-locks
      (false (trivial-package-locks:package-locked-p pkg))
    #+package-locks
      (fail (unexport exp pkg))
    (false (setf (trivial-package-locks:package-locked-p pkg) nil))
    (true (unexport exp pkg))
    (true (delete-package pkg))))

(define-test without-package-locks
  (with-test-packages (pkg exp)
    (false (trivial-package-locks:package-locked-p pkg))
    (true (setf (trivial-package-locks:package-locked-p pkg) t))
    #+package-locks
      (true (trivial-package-locks:package-locked-p pkg))
    #-package-locks
      (false (trivial-package-locks:package-locked-p pkg))
    #+package-locks
      (fail (unexport exp pkg))
    (trivial-package-locks:without-package-locks
      #-global-package-locks (false (trivial-package-locks:package-locked-p pkg))
      (true (unexport exp pkg)))
    #+package-locks
      (true (trivial-package-locks:package-locked-p pkg))
    (false (setf (trivial-package-locks:package-locked-p pkg) nil))
    (true (delete-package pkg))))

(define-test with-unlocked-packages
  (with-test-packages ((pkg "FU") exp)
    (false (trivial-package-locks:package-locked-p pkg))
    (true (setf (trivial-package-locks:package-locked-p pkg) t))
    #+package-locks
      (true (trivial-package-locks:package-locked-p pkg))
    #-package-locks
      (false (trivial-package-locks:package-locked-p pkg))
    #+package-locks
      (fail (unexport exp pkg))
    (trivial-package-locks:with-unlocked-packages ("FU")
      (false (trivial-package-locks:package-locked-p pkg))
      (true (unexport exp pkg)))
    #+package-locks
      (true (trivial-package-locks:package-locked-p pkg))
    (false (setf (trivial-package-locks:package-locked-p pkg) nil))
    (true (delete-package pkg))))

(define-test package-implements-package-p
  (skip-on ((not implementation-packages)) "Implementation package not supported")
  (with-test-packages (pkg nil *package* nil)
    (false (setf (trivial-package-locks:package-locked-p pkg) nil))
    (false (trivial-package-locks:package-locked-p pkg))
    (true (setf (trivial-package-locks:package-locked-p pkg) t))
    (true (trivial-package-locks:package-locked-p pkg))
    (false (trivial-package-locks:package-implements-package-p *package* pkg))
    (fail (intern "FU" pkg))
    (true (setf (trivial-package-locks:package-implements-package-p *package* pkg) t))
    (true (trivial-package-locks:package-implements-package-p *package* pkg))
    (true (intern "FU" pkg))
    (false (setf (trivial-package-locks:package-locked-p pkg) nil))
    (true (delete-package pkg))
    (true (delete-package *package*))))


