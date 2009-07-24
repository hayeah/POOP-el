(require 'elk-test)
;; http://nschum.de/src/emacs/elk-test/elk-test.el

(deftest "prototype"
    (obj (f (Object)
          b (Object))
      (b :set 'prototype f)
      (assert-eql *elp-undefined* (f :get 'foo))
      (assert-eql *elp-undefined* (b :get 'foo))
      (f :set 'foo 10)
      (assert-eql 10 (f :get 'foo))
      ;; b should get the value of the prototype
      (assert-eql (f :get 'foo) (b :get 'foo))
      ;; overwrite prototype property
      (b :set 'foo 11)
      (assert-eql (b :get 'foo) 11)
      (assert-eql (f :get 'foo) 10)))

(deftest "Array"
    (obj (a (Array [1 2 3]))
      (assert-eql 3 (a :size))
      (assert-eql (a :size) (a :length))
      (assert-eql 2 (a :at 1))
      (assert-equal '(2 3 4) (a :map (lambda (e) (1+ e))))))