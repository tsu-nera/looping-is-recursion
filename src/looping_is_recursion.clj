(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(power 2 2)  ;=> 4
(power 5 3)  ;=> 125
(power 7 0)  ;=> 1
(power 0 10) ;=> 0
    
(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    :else
    (and (= (first seq1) (first seq2))
         (recur (rest seq1) (rest seq2)))))

(seq= [1 2 4] '(1 2 4))  ;=> true
(seq= [1 2 3] [1 2 3 4]) ;=> false
(seq= [1 3 5] [])        ;=> false

(defn find-first-index [pred a-seq]
  (loop [a-seq a-seq
         index 0]
    (cond
      (empty? a-seq)
        nil
      (pred (first a-seq))
        index 
      :else
        (recur (rest a-seq) (inc index)))))

(defn avg [a-seq]
  (loop [a-seq a-seq
         sum 0
         count 0]
    (if (empty? a-seq)
      (/ sum count)
      (recur (rest a-seq) (+ (first a-seq) sum) (inc count)))))

(avg [1 2 3])   ;=> 2
(avg [0 0 0 4]) ;=> 1
(avg [1 0 0 1]) ;=> 1/2 ;; or 0.5

(defn parity [a-seq]
  ":(")

(parity [:a :b :c])           ;=> #{:a :b :c}
(parity [:a :b :c :a])        ;=> #{:b :c}
(parity [1 1 2 1 2 3 1 2 3 4]) ;=> #{2 4}


(defn fast-fibo [n]
  (loop [n n fib-2 0 fib-1 1]
    (if (< n 1)
      fib-2
      (recur (dec n) fib-1 (+ fib-2 fib-1)))))
(fast-fibo 0) ;=> 0
(fast-fibo 1) ;=> 1
(fast-fibo 2) ;=> 1
(fast-fibo 3) ;=> 2
(fast-fibo 4) ;=> 3
(fast-fibo 5) ;=> 5
(fast-fibo 6) ;=> 8

(defn cut-at-repetition [a-seq]
  [":("])

