(asdf:defsystem #:trivial-package-locks
  :description "A standard interface to the various package lock implementations."
  :author "Tarn W. Burton"
  :license "MIT"
  :version "0.1"
  :homepage "https://github.com/yitzchak/trivial-package-locks"
  :bug-tracker "https://github.com/yitzchak/trivial-package-locks/issues"
  :depends-on ((:feature :ecl #:package-locks))
  :in-order-to ((asdf:test-op (asdf:test-op #:trivial-package-locks/test)))
  :components
    ((:module code
      :serial t
      :components
        ((:file "packages")
         (:file "locks")))))

(asdf:defsystem #:trivial-package-locks/test
  :description "Test suite for trivial-package-locks"
  :author "Tarn W. Burton"
  :license "MIT"
  :version "0.1"
  :homepage "https://github.com/yitzchak/trivial-package-locks"
  :bug-tracker "https://github.com/yitzchak/trivial-package-locks/issues"
  :depends-on (#:parachute #:trivial-package-locks)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :trivial-package-locks/test))
  :components
    ((:module code
      :components
      ((:module test
        :serial t
        :components
          ((:file "packages")
           (:file "locks")))))))
