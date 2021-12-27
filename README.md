# trivial-package-locks

A standard interface to the various package lock implementations. Currently
supported implementations are ACL, CLISP, CMUCL, ECL and SBCL. On all other
implementations the following functions are exported but will evaluate to a
NOOP where appropriate.

## Package Locks Interface

Implementations that support this interface will have the keyword 
`:package-locks` present in `*features*`.

```common-lisp
(package-locked-p &optional (package *package*)) => lock-state
(setf (package-locked-p &optional (package *package*)) new-lock-state)
```

Accesses the lock state of a package. `t` indicates that the package is
locked and `nil` indicates that it is not locked.

```common-lisp
(without-package-locks &body body) => results
```

Disables the checking of package locks during the evaluation of `body`.
For implementations that do not have the ability to disable all package
locks via a dynamic variable `(list-all-packages)` will be used to unlock
each package before the evaluation of `body` and then relock each package
that was locked after the evaluation of `body`.

```common-lisp
(with-unlocked-packages (&rest packages) &body body) => results
```

Unlocks and relocks each of the named `packages` around the evaluation of
`body`.

```common-lisp
(with-locked-packages (&rest packages) &body body) => results
```

Locks and and then unlocks each of the named `packages` around the evaluation of
`body`.

## Implementation Packages Interface

Implementations that support this interface will have the keyword 
`:implementation-packages` present in `*features*`. Currently ACL and SBCL
are the only implementations that support this interface.

```common-lisp
(package-implementation-packages (&optional (package *package*)) => packages
(setf (package-implementation-packages &optional (package *package*)) packages)
```

Accesses the list of packages that are considered implementation packages of
`package`.

```common-lisp
(package-implements-package-p (implementation-package 
                               &optional (package *package*)) => state
(setf (package-implements-package-p implementation-package
                                    &optional (package *package*))
      new-state)
```

Access the implementation state of an individual package.
