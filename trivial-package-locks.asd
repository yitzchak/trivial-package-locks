(asdf:defsystem #:trivial-package-locks
  :description "A standard interface to the various package lock implementations."
  :author "Tarn W. Burton"
  :license "MIT"
  :version "0.1"
  :homepage "https://yitzchak.github.io/trivial-stream-column/"
  :bug-tracker "https://github.com/yitzchak/trivial-stream-column/issues"
  :depends-on ((:feature :ecl #:package-locks))
  :components
    ((:module code
      :serial t
      :components
        ((:file "packages")
         (:file "locks")))))
