(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                (if (zero? n)
                  acc
                (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [elem seq]
                (if (empty? seq)
                  elem
                (recur (first seq) (rest seq))))]
    (helper (first a-seq) (rest a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [value fst snd]
                (cond
                  (and (empty? fst) (empty? snd))
                    value
                  (= value false)
                    value
                  (not (== (count fst) (count snd)))
                    false
                  :else
                    (recur (== (first fst) (first snd)) (rest fst) (rest snd))))]
    (helper true seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [elem (first a-seq)
        index 0
        seq (rest a-seq)]
    (cond
      (empty? a-seq)
        nil
      (pred elem)
        index
      (empty? seq)
        nil
      :else
        (recur (first seq) (inc index) (rest seq)))))

(defn avg [a-seq]
  (loop [sum 0
         pos 0
         seq a-seq]
    (if (empty? seq)
        (/ sum pos)
        (recur (+ sum (first seq)) (inc pos) (rest seq)))))

(defn toggle 
  "If element exists in a set, removes it. Otherwise adds it." 
  [a-set elem]
  (if (contains? a-set elem) 
    (disj a-set elem) 
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [result (set ())
        elem (first a-seq)
        seq (rest a-seq)]
    (if (and (empty? seq) (= elem nil))
      result
      (recur (toggle result elem) (first seq) (rest seq)))))

(defn fast-fibo [n]
  (loop [idx 1
        cur 1
        prev 0]
    (cond
      (== 0 n)
        0
      (== n idx)
        cur
      :else
        (recur (inc idx) (+ cur prev) cur))))

(defn cut-at-repetition [a-seq]
  (loop [elem (first a-seq)
        seq (rest a-seq)
        copy (set ())
        result []]
    (cond 
      (and (empty? seq) (= nil elem))
        result
      (contains? copy elem)
        result
      :else
        (recur (first seq) (rest seq) (conj copy elem) (conj result elem)))))

