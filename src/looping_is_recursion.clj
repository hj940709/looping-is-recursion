(ns looping-is-recursion)

(defn power [base exp]
  (let [power-helper (fn [pro n k]
                 (if (zero? k)
                   pro
                   (recur (* pro n) n (dec k))))]
    (power-helper 1 base exp)))

(defn last-element [a-seq]
  (let [last-element-helper (fn [seq]
                 (if (empty? seq)
                   nil
				   (if (and (not (empty? seq)) (empty? (rest seq)))
                     (first seq)
                     (recur (rest seq)))))]
    (last-element-helper a-seq)))

(defn seq= [seq1 seq2]
   (let [seq=-helper (fn [a-seq b-seq]
                 (cond
				  (and (empty? a-seq) (empty? b-seq)) true
				  (not (= (empty? a-seq) (empty? b-seq))) false
				  (== (first a-seq) (first b-seq)) 
				    (recur (rest a-seq) (rest b-seq))
				  :else false))]
    (seq=-helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0]
    (cond
	  (empty? a-seq) nil
	  (== index (count a-seq)) nil
	  (pred (get a-seq index)) index
      :else (recur (inc index)))))

(defn avg [a-seq]
  (loop [i 1
        a (get a-seq 0)]
    (cond
	  (empty? a-seq) 0
	  (< i (count a-seq)) (recur (inc i) (/ (+ (* a i) (get a-seq i)) (inc i)))
	  (>= i (count a-seq)) a)))

(defn toggle [a-set elem]
  (if(contains? a-set elem)
    (disj a-set elem)
	(conj a-set elem)))	  

(defn parity [a-seq]
  (loop [seq a-seq
        set #{}]
	(if-not (empty? seq)
	  (if(contains? set (first seq))
        (recur (rest seq) (disj set (first seq)))
	    (recur (rest seq) (conj set (first seq))))
	  set)))

(defn fast-fibo [n]
  (loop [p 0
        q 1
		i 0]
	(cond
	  (zero? n) 0
	  (== 1 n) 1
	  (== n i) p
	  :else (recur q (+ p q) (inc i)))))

(defn cut-at-repetition [a-seq]
  (loop [seq a-seq vec [] set #{}]
    (if (or (contains? set (first seq)) (empty? seq))
	  vec
	  (recur (rest seq) (conj vec (first seq)) (conj set (first seq))))))

