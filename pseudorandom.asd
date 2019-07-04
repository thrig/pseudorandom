(asdf:defsystem #:pseudorandom
  :description "pseudo random distribution functions"
  :author "Jeremy Mates <jmates@cpan.org>"
  :license "BSD"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "pseudorandom")))
