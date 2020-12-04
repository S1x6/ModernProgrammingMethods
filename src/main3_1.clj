(ns main3_1)

(defn my-partition
  [n coll]
  (lazy-seq (if (>= (count coll) n)
              (cons (take n coll) (my-partition n (drop n coll)))
              (if (> (count coll) 0)
                (cons coll (my-partition n `()))
                ()
                )
              )
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

(defn my-old-filter
  [p? coll]
  (if (= 1 (count coll))
    (if (p? (first coll))
      coll
      `()
      )
    (reduce (make-filter-fun p?) coll)
    )
  )

(defn my-filter
  [p? coll]
  (flatten (->>
             (my-partition 20 coll)
             (map #(future (my-old-filter p? %)))
             (doall)
             (map deref)
             ))
  )

(defn -main
  []
  (println (my-partition 2 `(1 2 3 4 5 6 7)))
  ;(println (my-filter #(= (rem % 2) 0) (range 100)))
  ;(time (my-filter #(= (rem % 2) 0) (range 1000000)))
  ;(time (my-filter #(= (rem % 2) 0) (range 1000000)))
  ;(time (my-filter #(= (rem % 2) 0) (range 1000000)))
  (println (my-filter #(= (rem % 2) 0) (range 10)))
  ;(time (doall (filter #(do (Thread/sleep 10) (= (rem % 2) 0)) (range 1000))))
  ;(time (my-filter #(do (Thread/sleep 10) (= (rem % 2) 0)) (range 1000)))
  ;(time (doall (filter #(do (Thread/sleep 10) (= (rem % 2) 0)) (range 1000))))
  ;(time (my-filter #(= (rem % 2) 0) (range 1000000)))
  ;(time (doall (filter #(= (rem % 2) 0) (range 1000000))))
  )