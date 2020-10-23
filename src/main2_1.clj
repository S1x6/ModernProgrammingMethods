(ns main2_1)

(def step 1/10)

(defn area
  [a b]
  (* step (/ (+ a b) 2))
  )

(def area-mem (memoize area))



(def part-sum-tail                                          ;correct
  (memoize
    (fn [f x sum]
      (if (< (- x step) 0)
        (+ sum (area (f 0) (f x)))
        (do (print (str x " ")) (recur f (- x step) (+ sum (area (f x) (f (- x step))))))
        )))
  )


(def part-sum                                               ;correct
  (memoize
    (fn [f x]
      (if (< (- x step) 0)
        (area (f 0) (f x))
        (do (print (str x " ")) (+ (area (f x) (f (- x step))) (part-sum f (- x step))))
        )))
  )

(defn part-sum-seq
  [f n]
  (lazy-seq
    (if (= n 1)
      (area (f 0) (f step))
      (+ (part-sum-seq f (dec n)) (area (f (* step n)) (f (* step (dec n)))))
      ))
  )


(defn sq [x] (* x x))

(defn part-wrapper
  [f x]
  (if (or (= 0N (rem x step)) (= 0.0 (rem x step)))
    (part-sum f x)
    (let [decimal (Math/round (Math/floor (/ x step)))] (+ (part-sum f (* step decimal)) (area decimal x)))
    )
  )

;  2.2
;   |
;   |
;   V

; создание последовательности частичных сумм
(defn part-sum-iterate
  [f]
  (map (fn [a] (nth a 2)) (iterate
                            (fn [[a b sum]] [b (+ b step) (+ sum (area (f a) (f b)))]) [0 step 0]))
  )

; для каждой функции мемоизируем последовательность, иначе она будет создаваться с нуля заново
(def part-sum-iterate-mem
  (memoize part-sum-iterate)
  )

; проверяем, попали ли в сетку. Если да, то берем сумму из последовательности, если нет, то
; берем ближайшую сумму из последовательности меньше X и считаем маленькую трапецию
(defn part-wrapper-seq
  [f x]
  (let [decimal (Math/round (Math/floor (/ x step)))]
    (if (or (= 0N (rem x step)) (= 0.0 (rem x step)))
      (nth (part-sum-iterate-mem f) decimal)
      (+ (nth (part-sum-iterate-mem f) decimal) (area (* step decimal) x))
      ))
  )

; оборачиваем в возврат функции
(defn integral-seq [f]
  (fn [x]
    (part-wrapper-seq f x)
    )
  )

(defn -main
  []
  ; 2.1 done
  ;(let [f sq]
  ;  (println ((integral f) 3))
  ;  (time ((integral f) 5))
  ;  (time ((integral f) 11.2))
  ;  )

  ; 2.2 done
  (let [f sq]
    (println ((integral-seq f) 3))
    (time ((integral-seq f) 100))                           ; занимает много времени (1000 вычислений)
    (time ((integral-seq f) 101))                           ; занимает мало времени (10 вычислений)
    (time ((integral-seq f) 1))                             ; занимает мало времени (0 вычислений)
    )
  )