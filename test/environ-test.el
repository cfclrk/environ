;;; environ-test.el --- Tests for package env  -*- lexical-binding: t;

;;; Commentary:

;; Tests for package env.

;;; Code:

(require 'dash)
(require 'environ)
(require 'f)

;;; Helper functions

(defun environ-project-file (rel-path)
  "Return the absolute path to REL-PATH.
REL-PATH is a path relative to this project's root."
  (expand-file-name rel-path environ-project-dir))

;;; Tests

(ert-deftest environ-set-file ()
  "Test running environ-set-file."
  (let ((process-environment '())
        (test-file (environ-project-file "test/examples/simple")))
    (environ-set-file test-file)
    (should (equal "foo" (getenv "A")))
    (should (equal "bar" (getenv "B")))
    (should (equal "R$%!$KP$" (getenv "C")))
    (should (equal "foo-bar" (getenv "D")))
    (should (equal (expand-file-name "~/cats") (getenv "E")))))

(ert-deftest environ-unset-file ()
  "Test running `environ-unset-file'."
  (let ((process-environment '("A=a" "B=b" "C=C" "Z=z"))
        (test-file (environ-project-file "test/examples/simple")))
    (environ-unset-file test-file)
    (should (equal '("Z=z") process-environment))))

(ert-deftest environ-set-str ()
  "Test running `environ-set-str'."
  (let ((process-environment '())
        (test-str "A=a\nB=b"))
    (environ-set-str test-str)
    (should (equal "a" (getenv "A")))
    (should (equal "b" (getenv "B")))))

(ert-deftest environ-unset-str ()
  "Test running `environ-unset-str'."
  (let ((process-environment '("FOO=foo" "BAR=bar" "CATS=cats"))
        (test-str "FOO=foo\nBAR=bar"))
    (environ-unset-str test-str)
    (should (-same-items? '("CATS=cats") process-environment))))

(ert-deftest environ-set-pairs ()
  "Test running `environ-set-pairs' to set env vars."
  (let ((process-environment '()))
    (environ-set-pairs '(("A" "a") ("B" "'R$%!$KP$'")))
    (should (-same-items? '("A=a" "B=R$%!$KP$")
                          process-environment))
    ;; Test tilde expansion
    (let ((c-val (expand-file-name "~/foo")))
      (environ-set-pairs '(("C" "~/foo")))
      (should (equal c-val (getenv "C"))))))

(ert-deftest environ-unset-pairs ()
  "Test running `environ-unset-pairs' to unset env vars."
  (let ((process-environment '("FOO=foo" "BAR=bar" "BAZ=baz")))
    (environ-unset-pairs '(("FOO" "foo") ("BAR" "barrr")))
    (should (-same-items? '("BAZ=baz") process-environment))))

(ert-deftest environ-unset-names ()
  "Test running `environ-unset-names'."
  (let ((process-environment '("FOO=foo" "BAR=bar" "BAZ=baz")))
    (environ-unset-names '("FOO" "BAR" "CATS"))
    (should (equal nil (getenv "FOO")))
    (should (equal nil (getenv "BAR")))
    (should (equal "baz" (getenv "BAZ")))
    (should (equal '("BAZ=baz") process-environment))))

(ert-deftest environ--set-pair ()
  "Test running `environ--export-pair' to set a single environment variable."
  (let ((process-environment '()))
    (environ--set-pair '("FOO" "foo"))
    (should (equal "foo" (getenv "FOO")))
    (environ--set-pair '("FOO" "1"))
    (should (equal "1" (getenv "FOO")))))

(ert-deftest environ-unset-name ()
  (let ((process-environment '("\360\237\220\225=\360\237\220\210")))
    (should (equal "🐈" (getenv "🐕")))
    (environ-unset-name "🐕")
    (should (equal nil (getenv "🐈")))))

(ert-deftest environ--eval-pairs-simple ()
  "We should be able to set simple values."
  (let* ((process-environment '())
         (evald-pairs (environ--eval-pairs '(("FOO" "foo")
                                             ("BAR" "bar")))))
    (should (-same-items? '(("FOO" "foo") ("BAR" "bar"))
                          evald-pairs))))

(ert-deftest environ--eval-pairs-interpolate ()
  "We should be able to interpolate values."
  (let* ((process-environment '())
         (evald-pairs (environ--eval-pairs '(("FOO" "foo")
                                             ("BAR" "$FOO-bar")))))
    (should (-same-items? '(("FOO" "foo") ("BAR" "foo-bar"))
                          evald-pairs))))

(ert-deftest environ--eval-pairs-quotes ()
  "Surrounding value with single quotes should prevent intepolation."
  (let* ((process-environment '())
         (evald-pairs (environ--eval-pairs '(("FOO" "'f$oo'")
                                             ("B" "'R$%!$KP$'")))))
    (should (-same-items? '(("FOO" "f$oo") ("B" "R$%!$KP$"))
                          evald-pairs))))


;;; environ-test.el ends here
