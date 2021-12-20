(in-package #:trivial-package-locks)

#+(or acl clisp cmucl sb-package-locks)
  (pushnew :package-locks *features*)

(defun package-locked-p (package &aux (pkg (find-package package)))
  #+acl
    (or (excl:package-lock pkg)
        (excl:package-definition-lock pkg))
  #+clisp
    (ext:package-lock pkg)
  #+cmucl
    (or (ext:package-lock pkg)
        (ext:package-definition-lock pkg))
  #+ecl
    (ext:package-locked-p pkg)
  #+sb-package-locks
    (sb-ext:package-locked-p pkg)
  #-(or acl clisp cmucl ecl sb-package-locks)
    nil)

(defun (setf package-locked-p) (new-value package &aux (pkg (find-package package)))
  #+acl
    (setf (excl:package-lock pkg) new-value
          (excl:package-definition-lock pkg) new-value)
  #+clisp
    (setf (ext:package-lock pkg) new-value)
  #+cmucl
    (setf (ext:package-lock pkg) new-value
          (ext:package-definition-lock pkg) new-value)
  #+ecl
    (if new-value
        (ext:lock-package pkg)
        (ext:unlock-package pkg))
  #+sb-package-locks
    (if new-value
        (sb-ext:lock-package pkg)
        (sb-ext:unlock-package pkg))
  new-value)

(defun with-unlocked-packages/fallback (packages body-func)
  (let ((locked-pkgs (loop for pkg in packages
                           when (package-locked-p pkg)
                             do (setf (package-locked-p pkg) nil)
                             and collect pkg)))
    (unwind-protect
        (funcall body-func)
      (loop for pkg in locked-pkgs
            do (setf (package-locked-p pkg) t)))))

(defmacro without-package-locks (&body body)
  #+acl
    `(excl:without-package-locks ,@body)
  #+clisp
    `(with-unlocked-packages/fallback (list-all-packages)
                                      (lambda () ,@body))
  #+cmucl
    `(ext:without-package-locks ,@body)
  #+ecl
    `(ext:without-package-locks ,@body)
  #+sb-package-lock
    `(sb-ext:without-package-locks ,@body)
  #-(or acl clisp cmucl ecl sb-package-locks)
    `(progn ,@body))

(defmacro with-unlocked-packages ((&rest packages) &body body)
  #+(or acl cmucl)
    `(with-unlocked-packages/fallback (quote ,packages)
                                      (lambda () ,@body))
  #+clisp
    `(ext:without-package-lock ,packages ,@body)
  #+ecl
    `(ext:with-unlocked-packages ,packages ,@body)
  #+sb-package-lock
    `(sb-ext:with-unlocked-packages ,packages ,@body)
  #-(or acl clisp cmucl ecl sb-package-locks)
    `(progn ,@body))

