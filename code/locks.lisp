(in-package #:trivial-package-locks)

#+(or acl clisp cmucl sb-package-locks)
  (pushnew :package-locks *features*)

(defun package-locked-p (package &aux (pkg (find-package package)))
  #+acl
    (or (excl:package-lock pkg)
        (excl:package-lock-definition pkg))
  #+clisp
    (ext:package-lock pkg)
  #+cmucl
    (or (ext:package-lock pkg)
        (ext:package-lock-definition pkg))
  #+ecl
    (ext:package-locked-p pkg)
  #+sb-package-locks
    (sb-ext:package-locked-p pkg)
  #-(or acl clisp cmucl ecl sb-package-locks)
    nil)

(defun (setf package-locked-p) (new-value package &aux (pkg (find-package package)))
  #+acl
    (setf (excl:package-lock pkg) new-value
          (excl:package-lock-definition pkg) new-value)
  #+clisp
    (setf (ext:package-lock pkg) new-value)
  #+cmucl
    (setf (ext:package-lock pkg) new-value
          (ext:package-lock-definition pkg) new-value)
  #+ecl
    (cond (new-value
           (ext:lock-package pkg))
          ((ext:unlock-package pkg)
           nil))
  #+sb-package-locks
    (cond (new-value
           (sb-ext:lock-package pkg))
          ((sb-ext:unlock-package pkg)
           nil))
  #-(or acl clisp cmucl ecl sb-package-locks)
    new-value)

(defun with-unlocked-packages/fallback (packages body-func)
  (let ((locked-pkgs (loop for pkg in packages
                           when (package-locked-p pkg)
                             do (setf (package-locked-p pkg) nil)
                             and collect pkg)))
    (unwind-protect
        (funcall body-func)
      (dolist (pkg locked-pkgs)
        (setf (package-locked-p pkg) t)))))

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

(defmacro with-unlocked-packages (packages &body body)
  #+(or acl cmucl)
    `(with-unlocked-packages/fallback ,packages
                                      (lambda () ,@body))
  #+clisp
    `(ext:without-package-lock ,packages ,@body)
  #+ecl
    `(ext:with-unlocked-packages ,packages ,@body)
  #+sb-package-lock
    `(sb-ext:with-unlocked-packages ,packages ,@body)
  #-(or acl clisp cmucl ecl sb-package-locks)
    `(progn ,@body))

