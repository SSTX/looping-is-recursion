(ns looping-is-recursion)

(defn power [base exp]
  (let [iter (fn [base exp acc] (if (zero? exp)
                                  acc
                                  (recur base (dec exp) (* base acc))))]
    (iter base exp 1)))

(defn last-element [a-seq]
  (let [iter (fn [b-seq elem] (if (empty? b-seq)
                                elem
                                (recur (rest b-seq) (first b-seq))))]
    (iter a-seq nil)))

(defn seq= [seq1 seq2]
  (let [iter (fn [xs ys] (cond
                          (and (empty? xs) (empty? ys)) true
                          (not (= (first xs) (first ys))) false
                          :else (recur (rest xs) (rest ys))))]
    (if (== (count seq1) (count seq2))
      (iter seq1 seq2)
      false)))

(defn find-first-index [pred a-seq]
  (loop [xs a-seq
         elem (first a-seq)
         index 0]
    (cond
      (empty? xs) nil
      (pred elem) index
      :else (recur (rest xs)
                   (first (rest xs))
                   (inc index)))))

(defn avg [a-seq]
  (let [iter (fn [sum cnt xs] (if (empty? xs)
                                (/ sum cnt)
                                (recur (+ sum (first xs))
                                       (inc cnt)
                                       (rest xs))))]
    (if (empty? a-seq)
      nil
      (iter 0 0 a-seq))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [a-set #{}
         xs a-seq]
    (if (empty? xs)
      a-set
      (recur (toggle a-set (first xs))
             (rest xs)))))

(defn fast-fibo [n]
  (loop [cur 1
         prev 0
         c 0]
    (if (= c n)
      prev
      (recur (+ cur prev)
             cur
             (inc c)))))

(defn cut-at-repetition [a-seq]
  (loop [a-set #{}
         init []
         xs a-seq]
    (cond 
      (empty? xs) init
      (contains? a-set (first xs)) init
      :else (recur (conj a-set (first xs))
             (conj init (first xs))
             (rest xs)))))

