(ns fogus.lexical.chocolate)

(defmacro context []
  (let [globals (remove (comp :macro meta val) (ns-publics *ns*))
        syms (mapcat keys [globals, &env])
        entries (for [sym syms]
                  [`(quote ~sym) sym])]
    `(into {}
           (for [[sym# value#] [~@entries]
                 :when (not (fn? value#))]
             [sym# value#]))))

