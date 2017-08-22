(in-package :cl-user)
(defpackage :one/tests/dsl
  (:use :cl :rove))
(in-package :one/tests/dsl)


(deftest internal-util-test
    (testing "checks if supplied symbol is one's connective?"
      (testing "one's connectives are fives bellow"
        (ok (one::connective-p '<))  ; scan
        (ok (one::connective-p '>))  ; gather
        (ok (one::connective-p '+>)) ; fold
        (ok (one::connective-p '$))  ; compose
        (ok (one::connective-p '?))) ; call-if

      (testing "there's no connectives"
        (ng (one::connective-p '->))
        (ng (one::connective-p '|\||))))

    (testing "replace connectives in toplevel code"
      (ok (null (one::replace-connective nil)))

      (ok (equal (one::replace-connective '(< > +> $ ?))
                 '(< > one::+> one::$ one::?)))

      (testing "replacement occurs only toplevel elements"
        (ok (equal (one::replace-connective '((< > +> $ ?)))
                   '((< > +> $ ?))))))

    (testing "replacement placeholders"
      (testing "'place holder' is a symbol named `_`."
        (ok (one::place-holder-p 'one::_))
        (ok (one::place-holder-p 'one/core::_)))

      (testing "replace all `_` in given S-expression"
         (ok (equal (one::replace-place-holder '-replaced-
                                               '(lambda (in) (cons in (cons _))))
                    '(lambda (in) (cons in (cons -replaced-)))))
         (ok (equal (one::replace-place-holder '-replaced-
                                               '(lambda () (+ _ _)))
                    '(lambda () (+ -replaced- -replaced-)))))))

