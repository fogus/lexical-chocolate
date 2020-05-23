(ns lexical-chocolate.core-test
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
