(ns main)

(defn addOneToEnd
  ([element, lst, acc]
   (if (> (count lst) 0)
     (if (= (str (first (first lst))) element)
       (recur element (rest lst) acc)
       (recur element (rest lst) (concat acc (list (str element (first lst))))))
     acc))
  ([element, lst] (addOneToEnd element lst `()))
  )

(defn addAllToEnd
  ([alp, lst, acc]
   (if (> (count alp) 0)
     (recur (rest alp) lst (concat acc (addOneToEnd (first alp) lst)))
     acc))
  ([alp, lst] (addAllToEnd alp, lst, (list)))
  )

(defn combine
  ([alp, curLength, maxLength, acc]
   (if (not= curLength 1)                                   ;check if we have reached target length
     (recur alp (dec curLength) maxLength (addAllToEnd alp acc))
     acc
     ))
  ([alp, curLength, maxLength] (combine alp curLength maxLength alp))
  )

(defn findCombinations
  [alp, length]
  (if (<= length 0)
    (list)
    (combine alp length length)
    )
  )

(defn -main
  []
  (println (addOneToEnd "a" `("1" "2")))
  (println (addAllToEnd `("a" "b") `("1" "2")))
  (println (findCombinations `("a", "b", "c"), 2))
  )


;(ns main)
;
;(defn addOneToEnd
;  [element, list]
;  (if (> (count list) 0)
;    (if (= (str (last (first list))) element)
;      (addOneToEnd element (rest list))
;      (cons (str (first list) element)
;            (addOneToEnd element (rest list))))
;    `())
;  )
;
;(defn addAllToEnd
;  [alp, list]
;  (if (> (count alp) 0)
;    (apply conj (cons (addOneToEnd (first alp) list)
;                      (addAllToEnd (rest alp) list)))
;    `())
;  )
;
;(defn combine
;  [alp, result, curLength, maxLength]
;  (if (not= curLength 1)                                    ;check if we have reached target length
;    (addAllToEnd alp (combine alp result (dec curLength) maxLength))
;    alp
;    )
;  )
;
;(defn findCombinations
;  [alp, length]
;  (combine alp alp length length)
;  )
;
;(defn -main
;  []
;  (println (findCombinations `("a", "b", "c"), 2))
;  )

