(ns fogus.lexical-chocolate.core-test
  (:use clojure.test
        fogus.lexical.chocolate))

(deftest test-context
  (are [L R] (= L R)
       
       {}     (context)
       
       '{a 1} (let [a 1]
                (context))
       
       '{a 1
         b 2} (let [a 1
                    b 2]
                (context))

        '{a 1
          b 2
          c 3} (let [a 1
                     b 2
                     c 3]
                 (context))
          
        '{a 1
          b 2} (let [a 1]
                 (let [b 2]
                   (context)))

         '{a 1
           b 2
           c 3} (let [a 1]
                  (let [b 2]
                    (let [c 3]
                      (context))))))

(deftest test-circular-locals
  (def foo 33)
  (is (= '{a 42 c 33 foo 33}
         (let [a 42
               c 33
               foo 33]
           (context)))))

(deftest test-destro
  (are [L R] (= L R)
       
    '#{1 [3 4 5 6 7 8] [1 2 [3 4 5 6 7 8]] (5 6 7 8) 3 33 2 4}
    (set (vals
          (destro [a b [c d & e] :as Z]
                  [1 2 [3 4 5 6 7 8]])))))
