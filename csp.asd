(asdf:defsystem :csp
  :class :package-inferred-system
  :version "0.0.1"
  :description "Communicating Sequential Processes"
  :license "BSD"
  :depends-on (:bordeaux-threads
               :stmx
               :alexandria)
  :components ((:file "csp"))
  :serial t)
