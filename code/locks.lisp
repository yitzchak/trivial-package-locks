(in-package #:trivial-package-locks)

#+ecl (require :package-locks)

#+(or allegro clisp cmucl ecl sb-package-locks)
(pushnew :package-locks *features*)

#+(or allegro cmucl ecl sb-package-locks)
(pushnew :global-package-locks *features*)

#+(or allegro sb-package-locks)
(pushnew :implementation-packages *features*)

#+clasp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-symbol (symbol-name '#:package-locked-p) '#:ext)
    (pushnew :package-locks *features*)
    (pushnew :implementation-packages *features*)))

(defun package-locked-p (&optional (package *package*)
                         &aux (pkg (find-package package)))
  (declare (ignorable package pkg))
  #+allegro
  (or (excl:package-lock pkg)
      (excl:package-definition-lock pkg))
  #+(and clasp package-locks)
  (ext:package-locked-p pkg)
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
  #+allegro
  (setf (excl:package-lock pkg) new-value
        (excl:package-definition-lock pkg) new-value)
  #+(and clasp package-locks)
  (if new-value
      (ext:lock-package pkg)
      (ext:unlock-package pkg))
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

#+(or allegro clisp cmucl)
(defun with-unlocked-packages/fallback (packages body-func)
  (let ((pkgs (loop for designator in packages
                    for pkg = (find-package designator)
                    when (package-locked-p pkg)
                      do (setf (package-locked-p pkg) nil)
                      and collect pkg)))
    (unwind-protect
         (funcall body-func)
      (loop for pkg in pkgs
            when (package-name pkg)
              do (setf (package-locked-p pkg) t)))))

#+(or allegro (and clasp package-locks) clisp cmucl ecl sb-package-locks)
(defun with-locked-packages/fallback (packages body-func)
  (let ((pkgs (loop for designator in packages
                    for pkg = (find-package designator)
                    unless (package-locked-p pkg)
                      do (setf (package-locked-p pkg) t)
                      and collect pkg)))
    (unwind-protect
         (funcall body-func)
      (loop for pkg in pkgs
            when (package-name pkg)
              do (setf (package-locked-p pkg) nil)))))

(defmacro without-package-locks (&body body)
  #+allegro
  `(excl:without-package-locks ,@body)
  #+ccl
  `(let ((ccl:*warn-if-redefine-kernel* nil))
     ,@body)
  #+clisp
  `(with-unlocked-packages/fallback (list-all-packages)
     (lambda () ,@body))
  #+cmucl
  `(ext:without-package-locks ,@body)
  #+ecl
  `(ext:without-package-locks ,@body)
  #+sb-package-locks
  `(sb-ext:without-package-locks ,@body)
  #-(or allegro ccl clisp cmucl ecl sb-package-locks)
  `(progn ,@body))

(defmacro with-unlocked-system-packages (&body body)
  #+allegro
  `(excl:without-package-locks ,@body)
  #+ccl
  `(let ((ccl:*warn-if-redefine-kernel* nil))
     ,@body)
  #+clisp
  `(ext:without-package-lock () ,@body)
  #+cmucl
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (ext:without-package-locks ,@body))
  #+ecl
  `(ext:without-package-locks ,@body)
  #+sb-package-locks
  `(sb-ext:without-package-locks ,@body)
  #-(or allegro ccl clisp cmucl ecl sb-package-locks)
  `(progn ,@body))

(defmacro with-unlocked-packages ((&rest packages) &body body)
  (declare (ignorable packages))
  #+(or allegro cmucl)
  `(with-unlocked-packages/fallback (quote ,packages)
     (lambda () ,@body))
  #+ccl
  `(let ((ccl:*warn-if-redefine-kernel* nil))
     ,@body)
  #+(and clasp package-locks)
  `(ext:with-unlocked-packages ,packages ,@body)
  #+clisp
  `(ext:without-package-lock ,packages ,@body)
  #+ecl
  `(ext:with-unlocked-packages ,packages ,@body)
  #+sb-package-locks
  `(sb-ext:with-unlocked-packages ,packages ,@body)
  #-(or allegro ccl clisp cmucl ecl sb-package-locks (and clasp package-locks))
  `(progn ,@body))

(defmacro with-locked-packages ((&rest packages) &body body)
  (declare (ignorable packages))
  #+(or allegro clisp cmucl ecl sb-package-locks (and clasp package-locks))
  `(with-locked-packages/fallback (quote ,packages)
     (lambda () ,@body))
  #+ccl
  `(let ((ccl:*warn-if-redefine-kernel* t))
     ,@body)
  #-(or allegro ccl clisp cmucl ecl sb-package-locks (and clasp package-locks))
  `(progn ,@body))

(defun package-implementation-packages (&optional (package *package*))
  (declare (ignorable package))
  #+allegro
  (mapcar #'find-package (excl:package-implementation-packages (find-package package)))
  #+(and clasp package-locks)
  (ext:package-implemented-by-list (find-package package))
  #+sb-package-locks
  (sb-ext:package-implemented-by-list (find-package package)))

(defun (setf package-implementation-packages) (implementation-packages
                                               &optional (package *package*))
  (declare (ignorable package))
  #+allegro
  (setf (excl:package-implementation-packages (find-package package))
        (mapcar #'package-name package))
  #+(and clasp package-locks)
  (let* ((pkg (find-package package))
         (current (ext:package-implemented-by-list pkg))
         (new (mapcar #'find-package implementation-packages)))
    (loop for p in current
          unless (member p new)
            do (ext:remove-implementation-package p pkg))
    (loop for p in new
          unless (member p current)
            do (ext:add-implementation-package p pkg)))
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
  #+(or allegro sb-package-locks (and clasp package-locks))
  (and (member (find-package implementation-package)
               (package-implementation-packages package))
       t))

(defun (setf package-implements-package-p) (new-value implementation-package
                                            &optional (package *package*))
  (declare (ignorable implementation-package package))
  #+(or allegro sb-package-locks (and clasp package-locks))
  (let ((impl-pkg (find-package implementation-package))
        (pkg (find-package package)))
    #+allegro
    (setf (excl:package-implementation-packages pkg)
          (if new-value
              (pushnew (package-name implementation-package)
                       (excl:package-implementation-packages pkg)
                       :test #'equal)
              (remove (package-name implementation-package)
                      (excl:package-implementation-packages pkg)
                      :test #'equal)))
    #+clasp
    (if new-value
        (ext:add-implementation-package impl-pkg pkg)
        (ext:remove-implementation-package impl-pkg pkg))
    #+sbcl
    (if new-value
        (sb-ext:add-implementation-package impl-pkg pkg)
        (sb-ext:remove-implementation-package impl-pkg pkg)))
  new-value)
