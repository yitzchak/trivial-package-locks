(in-package #:trivial-package-locks)

#+(or acl clisp cmucl ecl sb-package-locks)
  (pushnew :package-locks *features*)

#+(or acl cmucl ecl sb-package-locks)
  (pushnew :global-package-locks *features*)

#+(or acl sb-package-locks)
  (pushnew :implementation-packages *features*)

(defun package-locked-p (&optional (package *package*)
                         &aux (pkg (find-package package)))
  (declare (ignorable package pkg))
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
    (sb-ext:package-locked-p pkg))

(defun (setf package-locked-p) (new-value
                                &optional (package *package*)
                                &aux (pkg (find-package package)))
  (declare (ignorable package pkg))
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
  (let ((locked-pkgs (loop for pkg in (mapcar #'find-package packages)
                           when (package-locked-p pkg)
                             do (setf (package-locked-p pkg) nil)
                             and collect pkg)))
    (unwind-protect
        (funcall body-func)
      (loop for pkg in locked-pkgs
            when (package-name pkg)
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
  #+sb-package-locks
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
  #+sb-package-locks
    `(sb-ext:with-unlocked-packages ,packages ,@body)
  #-(or acl clisp cmucl ecl sb-package-locks)
    `(progn ,@body))

(defun package-implementation-packages (&optional (package *package*))
  (declare (ignorable package))
  #+acl
    (mapcar #'find-package (excl:package-implementation-packages (find-package package)))
  #+sb-package-locks
    (sb-ext:package-implemented-by-list (find-package package)))

(defun (setf package-implementation-packages) (implementation-packages
                                               &optional (package *package*))
  (declare (ignorable package))
  #+acl
    (setf (excl:package-implementation-packages (find-package package))
          (mapcar #'package-name package))
  #+sb-package-locks
    (let* ((pkg (find-package package))
           (current (sb-ext:package-implemented-by-list pkg))
           (new (mapcar #'find-package implementation-packages)))
      (loop for p in current
            unless (member p new)
              do (sb-ext:remove-implementation-package p pkg))
      (loop for p in new
            unless (member p current)
              do (sb-ext:add-implementation-package p pkg)))
  implementation-packages)

(defun package-implements-package-p (implementation-package
                                     &optional (package *package*))
  (declare (ignorable implementation-package package))
  #+(or acl sb-package-locks)
    (and (member (find-package implementation-package)
                 (package-implementation-packages package))
         t))

(defun (setf package-implements-package-p) (new-value implementation-package
                                            &optional (package *package*))
  (declare (ignorable implementation-package package))
  #+(or acl sb-package-locks)
    (let ((impl-pkg (find-package implementation-package))
          (pkg (find-package package)))
      #+acl
        (setf (excl:package-implementation-packages pkg)
              (if new-value
                  (pushnew (package-name implementation-package)
                           (excl:package-implementation-packages pkg)
                           :test #'equal)
                  (remove (package-name implementation-package)
                          (excl:package-implementation-packages pkg)
                          :test #'equal)))
      #+sbcl
        (if new-value
            (sb-ext:add-implementation-package impl-pkg pkg)
            (sb-ext:remove-implementation-package impl-pkg pkg)))
  new-value)
