(ns fogus.lexical.chocolate.destructure)

(declare pb)

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
            (= firstb '&) (recur (pb ret (second bs) gseq)
                                 n
                                 (nnext bs)
                                 true)
            (= firstb :as) (pb ret (second bs) gvec)
            :else (if seen-rest?
                    (throw (new Exception "Unsupported binding form, only :as can follow & parameter"))
                    (recur (pb (if has-rest
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

(defn pmap [bvec b v]
  (let [gmap (gensym "map__")
        gmapseq (with-meta gmap {:tag 'clojure.lang.ISeq})
        defaults (:or b)]
    (loop [ret (-> bvec (conj gmap) (conj v)
                   (conj gmap) (conj `(if (seq? ~gmap) (clojure.lang.PersistentHashMap/create (seq ~gmapseq)) ~gmap))
                   ((fn [ret]
                      (if (:as b)
                        (conj ret (:as b) gmap)
                        ret))))
           bes (let [transforms
                     (reduce1
                      (fn [transforms mk]
                        (if (keyword? mk)
                          (let [mkns (namespace mk)
                                mkn (name mk)]
                            (cond (= mkn "keys") (assoc transforms mk #(keyword (or mkns (namespace %)) (name %)))
                                  (= mkn "syms") (assoc transforms mk #(list `quote (symbol (or mkns (namespace %)) (name %))))
                                  (= mkn "strs") (assoc transforms mk str)
                                  :else transforms))
                          transforms))
                      {}
                      (keys b))]
                 (reduce1
                  (fn [bes entry]
                    (reduce1 #(assoc %1 %2 ((val entry) %2))
                             (dissoc bes (key entry))
                             ((key entry) bes)))
                  (dissoc b :as :or)
                  transforms))]
      (if (seq bes)
        (let [bb (key (first bes))
              bk (val (first bes))
              local (if (instance? clojure.lang.Named bb) (with-meta (symbol nil (name bb)) (meta bb)) bb)
              bv (if (contains? defaults local)
                   (list `get gmap bk (defaults local))
                   (list `get gmap bk))]
          (recur (if (ident? bb)
                   (-> ret (conj local bv))
                   (pb ret bb bv))
                 (next bes)))
        ret))))

(defn pb [bvec b v]
  (cond
    (symbol? b) (-> bvec (conj b) (conj v))
    (vector? b) (pvec bvec b v)
    (map? b) (pmap bvec b v)
    :else (throw (new Exception (str "Unsupported binding form: " b)))))

(defn process-entry [bvec b] (pb bvec (first b) (second b)))

(defn destructure [bindings]
(let [bents (partition 2 bindings)]
  (if (every? symbol? (map first bents))
    bindings
    (reduce1 process-entry [] bents))))

()
