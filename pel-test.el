(require 'elk-test)
;; http://nschum.de/src/emacs/elk-test/elk-test.el

(deftest "Object"
    (obj (o1 (Object)
          o2 (Object))
      (o1 :set 'v1 1)
      (o2 :extend o1)
      (o1 :set 'v2 1)
      (assert-eql 1 (o2 :v1))
      (assert-eql *pel-undefined* (o2 :v2))
      ))

(deftest "prototype"
    (obj (f (Object)
          b (Object))
      (b :set 'prototype f)
      (assert-eql *pel-undefined* (f :get 'foo))
      (assert-eql *pel-undefined* (b :get 'foo))
      (f :set 'foo 10)
      (assert-eql 10 (f :get 'foo))
      ;; b should get the value of the prototype
      (assert-eql (f :get 'foo) (b :get 'foo))
      ;; overwrite prototype property
      (b :set 'foo 11)
      (assert-eql (b :get 'foo) 11)
      (assert-eql (f :get 'foo) 10)))

(deftest "Seq"
    (obj (a (Array [1 2 3]))
      (assert-eql 3 (a :size))
      (assert-eql (a :size) (a :length))
      (assert-eql 2 (a :at 1))
      (assert-equal '(2 3 4) (a :map (lambda (e) (1+ e))
                               :el))
      (assert-eql 0 (progn (a :at! 1 0)
                           (a :at 1)))

      (assert-equal
       [150 104 106]
       (obj (a (Array [1 2 3 4 5 6]))
         (a :at! 1 50
            :map (fn (e) (+ 100 e))
            :select (fn (e) (= (mod e 2) 0))
            :to_a
            :el)))

      (assert-equal
       '(t nil nil t)
       (obj (a (Array [1 2 3]))
         (list (a :all? (fn (e) (integerp e)))
               (a :all? (fn (e) (symbolp e)))
               (a :any? (fn (e) (symbolp e)))
               (a :any? (fn (e) (integerp e)))))
       )

      (assert-equal
       '((Array [97 98 99])
         (String "abc")
         (List (97 98 99)))
       (obj (v (List "abc"))
         (list (obj (o (v :to_a))
                 (list (o :get 'constructor)
                       (o :el)))
               (obj (o (v :to_s))
                 (list (o :get 'constructor)
                       (o :el)))
               (obj (o (v :to_l))
                 (list (o :get 'constructor)
                       (o :el)))))
       )

      (assert-equal
       "abc"
       (obj (v (List "abc"))
         (v :to_a :to_s :to_l :to_s :el)))

      (assert-equal
       'abc
       (obj (v (List "abc"))
         (v :to_sym)))))

(deftest "String"
    (assert-equal
     "aabb3x3x3x3x"
     (obj (s (String "aabbcc"))
      (s :gsub "c" "3131"
         :gsub "1" "x"
         :el)
      ))
  (assert-equal
   "Aa Bb Cc"
   (obj (s (String "aa bb cc"))
     (s :capitalize :el)))
  (assert-equal
   '("00aaaebbb11" "aaa" "bbb" nil)
   (obj (s (String "00aaaebbb11"))
     (s :match "\\(a*\\)e\\(b*\\)"
        (fn ()
            (list
             (concat $b $m $a)
             ($ 1) ($ 2) ($ 3)))))))


