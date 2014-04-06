(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (<= exp 0)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (cond
   (empty? a-seq)
     nil
   (empty? (rest a-seq))
     (first a-seq)
   :else
     (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2))
     true
   (not= (first seq1) (first seq2))
     false
   (or (empty? seq1) (empty? seq2))
     false
   :else
     (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [index 0
         pred pred
         a-seq a-seq]
    (cond
     (empty? a-seq)
       nil
     (pred (first a-seq))
       index
     :else
      (recur (inc index) pred (rest a-seq)))))

(defn avg [a-seq]
  (loop [sum 0
         cnt 0
         seqw a-seq]
    (if (empty? seqw)
      (/ sum cnt)
      (recur (+ sum (first seqw)) (inc cnt) (rest seqw)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [res (set [])
         seqw a-seq]
    (if (empty? seqw)
      res
      (recur (toggle res (first seqw)) (rest seqw)))))

(defn fast-fibo [n]
  (loop [f 0
         s 1
         cnt n]
    (if (zero? cnt)
      f
      (recur s (+ f s) (dec cnt)))))

(defn cut-at-repetition [a-seq]
  (loop [res []
         seqw a-seq]
    (let [tmp (set res)]
      (cond
       (empty? seqw)
         res
       (not= (count (conj res (first seqw))) (count (conj tmp (first seqw))))
         res
       :else
         (recur (conj res (first seqw)) (rest seqw))))))

