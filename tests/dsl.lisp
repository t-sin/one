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


(deftest simplified-lambda-test
    (testing "for symbol, it just wrap with `(function)`"
      (ok (equal (one::simplified-lambda 'string=)
                 '#'string=)))

    (testing "for list"
      (testing "if first element is `lambda`, it is lambda expression, pass through `code`"
        (ok (equal (one::simplified-lambda '(lambda (x) (* x 2)))
                   '(lambda (x) (* x 2)))))

      (testing "if first element is `function`, it's a function, pass through `code`"
        (ok (equal (one::simplified-lambda '(function identity))
                   '(function identity)))
        (testing "tricky case ;p"
          (ok (equal (one::simplified-lambda (read-from-string "#'identity"))
                     '(function identity)))))

      (testing "otherwise, that is simplified lambda, wrap it with lambda"
        (ok (expands (one::simplified-lambda '(format nil "  ~a" _))
                     '#'(lambda (#:slmd) (format nil "  ~a" #:slmd)))))))

(deftest one-parse-test
  (testing "empty body, returns stree"
    (ok (eq (one::parse '() nil) nil))
    (ok (eq (one::parse '() 'stree) 'stree)))

  (testing "basic syntax is: [conn op-fn]*"
    (ok (equal (one::parse '(one::$ fn1))
               '(one::$ nil fn1)))
    (ok (equal (one::parse '(one::$ fn1 one::> fn2))
               '(one::> (one::$ nil fn1) fn2)))
    (ok (equal (one::parse '(one::$ fn1 one::> fn2 one::? fn3))
               '(one::? (one::> (one::$ nil fn1) fn2) fn3))))

  (testing "special treatment for folding behavior `+>`"
    (ok (equal (one::parse '(one::$ fn1 one::+> fn2 one::? fn3))
               '(one::? (one::+> (one::$ nil fn1) fn2) fn3))
        "1. folding behavior with one parameter: folding operation")
    (ok (equal (one::parse '(one::$ fn1 one::+> fn2 :init one::? fn3))
               '(one::? (one::+> (one::$ nil fn1) fn2 :init) fn3))
        "2. folding behavior with two parameters: folding operation and initial value"))

  (testing "syntax errors"
    (ok (signals (one::parse '(-> fn1)) 'one/error:one-syntax-error)
        "error occurs: first element is not a connective")
    (ok (signals (one::parse '(fn1 fn2)) 'one/error:one-syntax-error)
        "error occurs: first element is not a connective"))

  (testing "some abnormal case"
    (ok (equal (one::parse '(one::$)) '(one::$ nil nil))
        "parse OK, empty syntax tree is returned. this maybe raise error on `build`")))

(deftest one-build-test
  (testing "basic construction"
    (testing "scan: `<`"
      (ok (rove/core/assertion::equal*
           (one::build '(one::< nil read-line))
           '(lambda (#:in) (funcall (one/core:$scan #:in #'read-line) #'identity)))))
    (testing "gather: `>`"
      (ok (rove/core/assertion::equal*
           (one::build '(one::> nil (lambda (x) (sort x #'<))))
           '(multiple-value-bind (#:slurp #:barf)
                (one/core:$gather (lambda (x) (sort x #'<)))
              (lambda (#:in)
                (funcall #:slurp #:in)
                (funcall #:barf #'identity))))))
    (testing "fold: `+>`"
      (ok (rove/core/assertion::equal*
           (one::build '(one::+> nil +))
           '(multiple-value-bind (#:slurp #:barf)
                (one/core:$fold #'+ nil)
              (lambda (#:in)
                (funcall #:slurp #:in)
                (funcall #:barf #'identity)))))
      (ok (rove/core/assertion::equal*
           (one::build '(one::+> nil + 1))
           '(multiple-value-bind (#:slurp #:barf)
                (one/core:$fold #'+ 1)
              (lambda (#:in)
                (funcall #:slurp #:in)
                (funcall #:barf #'identity))))))
    (testing "compose: `$`"
      (ok (rove/core/assertion::equal*
           (one::build '(one::$ nil print))
           '(lambda (#:in) (funcall #'identity (funcall #'print #:in))))))
    (testing "call-if: `?`"
      (ok (rove/core/assertion::equal*
           (one::build '(one::? nil oddp))
           '(lambda (#:in) (funcall (one/core:$call-if #'oddp #'identity) #:in))))))

  (testing "complex construction"
    (diag "to be written...")))

(deftest one-for-test
  (testing "normal case"
    (ok (expands '(one:for #P"one.asd" < one:read-line* $ print)
                 '(funcall (lambda (#:in2)
                             (funcall (one/core:$scan #:in2 #'one:read-line*)
                                      (lambda (#:in)
                                        (funcall #'identity (funcall #'print #:in)))))
                           #P"one.asd"))))
  (testing "shorthand for standard input"
    (ok (expands '(one:for - < one:read-line* $ print)
                 '(funcall (lambda (#:in2)
                             (funcall (one/core:$scan #:in2 #'one:read-line*)
                                      (lambda (#:in)
                                        (funcall #'identity (funcall #'print #:in)))))
                           *standard-input*)))))

(deftest one-for*-test
  (testing "shorthand for stdout"
    (ok (expands '(one:for* - < one:read-line*)
                 '(one:for - < one/io:read-line* one::$ one/io:print*)))))
