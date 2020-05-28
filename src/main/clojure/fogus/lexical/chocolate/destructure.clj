(ns fogus.lexical.chocolate.destructure)

(declare process-bind-pair)

(def reduce1 #'clojure.core/reduce1)

(defn pvec [bvec b val]
  (let [gvec (gensym "vec__")
        gseq (gensym "seq__")
        gfirst (gensym "first__")
        has-rest (some #{'&} b)]
    (loop [ret (let [ret (conj bvec gvec val)]
                 (if has-rest
                   (conj ret gseq (list `seq gvec))
                   ret))
           n 0
           bs b
           seen-rest? false]
      (if (seq bs)
        (let [firstb (first bs)]
          (cond
            (= firstb '&) (recur (process-bind-pair ret (second bs) gseq)
                                 n
                                 (nnext bs)
                                 true)
            (= firstb :as) (process-bind-pair ret (second bs) gvec)
            :else (if seen-rest?
                    (throw (new Exception "Unsupported binding form, only :as can follow & parameter"))
                    (recur (process-bind-pair (if has-rest
                                 (conj ret
                                       gfirst `(first ~gseq)
                                       gseq `(next ~gseq))
                                 ret)
                               firstb
                               (if has-rest
                                 gfirst
                                 (list `nth gvec n nil)))
                           (inc n)
                           (next bs)
                           seen-rest?))))
        ret))))

(defn process-bind-pair [bvec b v]
  (cond
    (symbol? b) (-> bvec (conj b) (conj v))
    (vector? b) (pvec bvec b v)
    (map? b) (throw (new Exception. "Map bindings not currently supported."))
    :else (throw (new Exception (str "Unsupported binding form: " b)))))

(defn process-entry [bvec b] (process-bind-pair bvec (first b) (second b)))

(defn destructure [bindings]
  (let [bents (partition 2 bindings)]
    (if (every? symbol? (map first bents))
      bindings
      (reduce1 process-entry [] bents))))

(comment

  (process-bind-pair [] 'a 42)

)
