;;; env-test.el --- Tests for package env  -*- lexical-binding: t;

;;; Commentary:

;; Tests for package env.

;;; Code:

(require 'dash)
(require 'env)
(require 'f)

;;; Helper functions

(defun env-project-file (rel-path)
  "Return the absolute path to REL-PATH.
REL-PATH is a path relative to this project's root."
  (expand-file-name rel-path env-project-dir))

;;; Tests

(ert-deftest env-set-file ()
  "Test running env-set-file."
  (let ((process-environment '())
        (test-file (env-project-file "test/examples/simple")))

    (env-set-file test-file)

    (should (equal "foo"
                   (getenv "A")))
    (should (equal "bar"
                   (getenv "B")))
    (should (equal "R$%!$KP$"
                   (getenv "C")))
    (should (equal "foo-bar"
                   (getenv "D")))
    (should (equal (expand-file-name "~/cats")
                   (getenv "E")))))

(ert-deftest env-unset-file ()
  "Test running `env-unset-file'."
  (let ((process-environment '("A=a" "B=b" "C=C" "Z=z"))
        (test-file (env-project-file "test/examples/simple")))

    (env-unset-file test-file)

    (should (equal '("Z=z")
                   process-environment))))

(ert-deftest env-set-str ()
  "Test running `env-set-str'."
  (let ((process-environment '())
        (test-str "A=a\nB=b"))

    (env-set-str test-str)

    (should (equal "a" (getenv "A")))
    (should (equal "b" (getenv "B")))))

(ert-deftest env-unset-str ()
  "Test running `env-unset-str'."
  (let ((process-environment '("FOO=foo" "BAR=bar" "CATS=cats"))
        (test-str "FOO=foo\nBAR=bar"))

    (env-unset-str test-str)

    (should (-same-items? '("CATS=cats")
                          process-environment))))

(ert-deftest env-set-pairs ()
  "Test running `env-set-pairs' to set env vars."
  (let ((process-environment '()))

    (env-set-pairs '(("A" "a")
                     ("B" "'R$%!$KP$'")))

    (should (-same-items? '("B=R$%!$KP$" "A=a")
                          process-environment))))

(ert-deftest env-unset-pairs ()
  "Test running `env-unset-pairs' to unset env vars."
  (let ((process-environment '("FOO=foo" "BAR=bar" "BAZ=baz")))

    (env-unset-pairs '(("FOO" "foo")
                       ("BAR" "barrr")))

    (should (-same-items? '("BAZ=baz")
                          process-environment))))

(ert-deftest env-unset-names ()
  "Test running `env-unset-names'."
  (let ((process-environment '("FOO=foo" "BAR=bar" "BAZ=baz")))

    (env-unset-names '("FOO" "BAR"))

    (should (equal nil (getenv "FOO")))
    (should (equal nil (getenv "BAR")))
    (should (equal "baz" (getenv "BAZ")))
    (should (equal '("BAZ=baz") process-environment))

    ;; Unset names that do not exist
    (env-unset-names '("CATS" "HATS"))

    (should (equal '("BAZ=baz") process-environment))))

(ert-deftest env--set-pair ()
  "Test running `env--export-pair' to set a single environment variable."
  (let ((process-environment '()))
    (env--set-pair '("FOO" "foo"))
    (should (equal "foo" (getenv "FOO")))

    (env--set-pair '("FOO" "1"))
    (should (equal "1" (getenv "FOO")))))

(ert-deftest env--unset-name ()
  (let ((process-environment '("\360\237\220\225=\360\237\220\210")))
    (should (equal "🐈" (getenv "🐕")))
    (env--unset-name "🐕")
    (should (equal nil (getenv "🐈")))))

(ert-deftest env--eval-pairs ()
  "Test running `env--eval-pairs'."

  ;; Should be able to set simple values
  (let* ((process-environment '())
         (evald-pairs (env--eval-pairs '(("FOO" "foo")
                                        ("BAR" "bar")))))
    (should (equal '(("FOO" "foo")
                    ("BAR" "bar"))
                   evald-pairs)))

  ;; Should be able to interpolate values
  (let* ((process-environment '())
         (evald-pairs (env--eval-pairs '(("FOO" "foo")
                                        ("BAR" "$FOO-bar")))))
    (should (equal '(("FOO" "foo")
                    ("BAR" "foo-bar"))
                   evald-pairs)))

  ;; Should be able to surround a value in single quotes to make it a literal
  ;; value (i.e. prevent interpolation)
  (let* ((process-environment '())
         (evald-pairs (env--eval-pairs '(("FOO" "'f$oo'")
                                         ("B" "'R$%!$KP$'")))))
    (should (-same-items? '(("FOO" "f$oo")
                            ("B" "R$%!$KP$"))
                          evald-pairs))))

;;; env-test.el ends here
