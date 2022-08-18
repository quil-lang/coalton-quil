(defsystem "coalton-quil"
  :version "0.0.1"
  :pathname "src/"
  :depends-on ((:version "uiop" "3.3.5") ; `uiop:define-package' `:local-nicknames' support added in asdf/uiop 3.3.5
               "cl-quil"
               "alexandria"
               "coalton")
  :serial t
  :components ((:file "interface")
               (:file "parsed-language")))
