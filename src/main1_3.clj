(ns main1_3)

;1.3
(defn make-map-fun
  [f]
  (fn [fir sec]
    (if (seq? fir)
      (concat fir (list (f sec)))                           ;объединяем имеющийся список с результатом применения к очередному элементу
      (concat (list (f fir)) (list (f sec)))                ;превращаем результат применения к первому в список, добавляем результат применения ко второму
      )
    ))

(defn my-map
  [f coll]
  (if (= 1 (count coll))
    (list (f (first coll)))
    (reduce (make-map-fun f) coll)
    )
  )

(defn make-filter-fun
  [p?]
  (fn [fir sec]
    (if (seq? fir)
      (if (p? sec) (concat fir (list sec)) fir)
      (if (p? fir)
        (if (p? sec)
          (concat (list fir) (list sec))
          (list fir)
          )
        (if (p? sec)
          (list sec)
          (list))
        )
      ))
  )

(defn my-filter
  [p? coll]
  (if (= 1 (count coll))
    (if (p? (first coll))
      coll
      `()
      )
    (reduce (make-filter-fun p?) coll)
    )
  )

;1.4
(defn addOneToFirst
  [e, coll]
  (my-map (fn [x] (str e x)) coll))

(defn my-flatten
  [f s]
  (if (seq? f)
    (if (seq? s)
      (concat f s)
      (concat f (list s))
      )
    (if (seq? s)
      (concat (list f) s)
      (concat (list f) (list s))
      )
    )
  )

(defn addAllToFirst
  [addColl coll]
  (reduce my-flatten (my-map (fn [a] (addOneToFirst a (my-filter (fn [e] (not= a (str (first e)))) coll))) addColl))
  )

(defn findCombinations
  [coll n]
  (reduce (fn [f s] (addAllToFirst s f)) (repeat n coll))
  )

(defn -main
  []
  ;1.3
  ;map
  (println (my-map (fn [a] (if (= (rem a 2) 0) `("lol") `("not lol"))) `(1 2 3 4 5)))
  ;filter
  (println (my-filter (fn [a] (if (not= a 1) true false)) `(2 1 3 1 5)))
  (println (my-filter (fn [a] (if (not= a 1) true false)) `(1 1 3 1 5)))
  (println (my-filter (fn [a] (if (not= a 1) true false)) `(1 2 3 1 5)))
  (println (my-filter (fn [a] (if (not= a 1) true false)) `(2 1 3 1 1)))
  (println (my-filter (fn [a] (if (not= a 1) true false)) `(3)))

  ;1.4
  ;(println (addAllToFirst `("a" "b" "c") `("a" "b" "c")))
  ;(println (my-filter (fn [e] (not= e (str (first "abc")))) `("a" "b" "c")))
  ;(println (findCombinations `("a" "b") 2))
  ;(println (findCombinations `("a" "b") 4))
  )