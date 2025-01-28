(ns fogus.lexical.chocolate)

(set! *warn-on-reflection* true)

(defmacro context
  "Builds a map corresponding to the lexical enviroment in which this macros occurs.
  Takes a namespace instance as its argument (default is the value of *ns*)
  and will build the return map according to the occurance of bindings in the ns
  with local bindings shadowing top-level bindings."
  [& nses]
  (let [nses (or (seq nses) [*ns*])
        globals (remove (comp :macro meta val) (mapcat ns-publics nses))
        syms (mapcat keys [globals &env])
        entries (for [sym syms]
                  [`(quote ~sym) sym])]
    `(into {}
           (for [[sym# value#] [~@entries]
                 :when (not (fn? value#))]
             [sym# value#]))))

(defmacro destro
  "Provides a simple way to obtain a map of the lexical context based on the
   result of a destructuring operation.  That is, the typical call to the
   `destructure` function will operate along the lines of:

    (destructure '[[_ _ x _ _] [1 2 3 4 5]])

    ;=> [V [1 2 3 4 5]
         _ (nth V 0 nil)
         _ (nth V 1 nil)
         x (nth V 2 nil)
         _ (nth V 3 nil)
         _ (nth V 4 nil)]

   whereby the form returned contains the operations needed to pull apart (i.e. destructure)
   the data structure under examination.  However, `destro` will instead resolve the values
   of the destructuring operation, including any intermediate bindings, as below:

    (destro [a b [c d & e] :as Z]
            [1 2 [3 4 5 6 7 8]])

    ;=> {vec__2330 [1 2 [3 4 5 6 7 8]],
         a 1,
         b 2,
         vec__2331 [3 4 5 6 7 8],
         c 3,
         d 4,
         e (5 6 7 8),
         Z [1 2 [3 4 5 6 7 8]]}

   This will also operate as expected within a lexical context:

    (let [c [1 2]]
      (destro [a b] c))

    ;=> {c [1 2],
         vec__2336 [1 2],
         a 1,
         b 2}"
  [binds form]
  `(let [~binds ~form]
     (context)))
