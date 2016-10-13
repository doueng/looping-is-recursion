(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [base exp result]
                 (if (zero? exp)
                   result
                   (recur base (dec exp) (* base result))))]
    (helper base exp 1)))

(defn last-element [a-seq]
  (let [helper (fn [a-seq]
                 (if (>= 1 (count a-seq))
                   (first a-seq)
                   (recur (rest a-seq))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2]
                 (cond
                   (and (empty? seq1) (empty? seq2)) true
                   (or (empty? seq1) (empty? seq2)) false
                   (not= (first seq1) (first seq2)) false
                   :else (recur (rest seq1) (rest seq2))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [pred pred
         a-seq a-seq
         i 0]
    (cond
      (empty? a-seq) nil
      (pred (first a-seq)) i
      :else (recur pred (rest a-seq) (inc i)))))

(defn avg [a-seq]
  (loop [a-seq a-seq
         sum 0
         length 0]
    (if (first a-seq)
      (recur (rest a-seq) (+ sum (first a-seq)) (inc length))
      (/ sum length))))


(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))


(defn parity [a-seq]
  (loop [a-seq a-seq
         result #{}]
        (if (empty? a-seq)
          result
          (recur (rest a-seq) (toggle result (first a-seq))))))

(defn fast-fibo [n]
  (loop [n n
         Fn 0
         Fn-1 1]
    (if (>= 0 n)
      Fn
      (recur (dec n) (+ Fn Fn-1) Fn))))

(defn cut-at-repetition [a-seq]
  (loop [a-seq a-seq
         result []]
    (cond
      (empty? a-seq) result
      (some #{(first a-seq)} result) result
      :else (recur (rest a-seq) (conj result (first a-seq))))))

