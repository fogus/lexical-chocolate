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

(deftest lloop-test
  (testing "tail-position call, single binding — sanity check"
    (is (= 0
           (lloop go [n 5]
             (if (zero? n) n (go (dec n)))))))

  (testing "non-tail: label inside cons — impossible with loop/recur"
    (is (= '(1 2 3)
           (lloop build [xs [1 2 3]]
             (if (empty? xs)
               '()
               (cons (first xs) (build (rest xs))))))))

  (testing "multiple bindings with accumulator"
    (is (= 10
           (lloop sum [n 4 acc 0]
             (if (zero? n)
               acc
               (sum (dec n) (+ acc n)))))))

  (testing "two non-tail calls in the same expression: tree recursion"
    (is (= 8
           (lloop fib [n 6]
             (if (< n 2)
               n
               (+ (fib (- n 1)) (fib (- n 2))))))))

  (testing "label invoked from multiple branches of a conditional"
    (is (= [0 2 4 6 8]
           (lloop evens [n 0 acc []]
             (cond
               (> n 9)   acc
               (even? n) (evens (inc n) (conj acc n))
               :else     (evens (inc n) acc)))))))
