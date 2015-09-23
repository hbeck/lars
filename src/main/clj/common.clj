(ns common
  (:require [clojure.set :as s]))

(defn conj-pred
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & ps] (apply conj-pred (conj-pred p1 p2) ps)))

(defn disj-pred
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & ps] (apply disj-pred (disj-pred p1 p2) ps)))

(defn in? [x seq]
  "true if seq contains x"
  (some #(= % x) seq))

(defn remove-all [coll1 coll2]
  "remove from coll1 elements of coll2"
    (filter (fn [x] (not-any? #(= % x) coll2)) coll1))

(defn land
  "logical and"
  ([] false)
  ([x] (if x true false))
  ([x y] (and x y))  
  ([x y & z] (if x (apply land y z) false)))

(defn lor
  "logical or"
  ([] false)
  ([x] (if x true false))
  ([x y] (or x y))
  ([x y & z] (if x true (apply lor y z))))

(defn forall? [xs f]
  (or
   (empty? xs)
   (apply land (for [x xs] (f x)))))

(defn exists? [xs f]
  (loop [xs xs]
    (cond (empty? xs) false
          (f (first xs)) true
          :else (recur (rest xs)))))

;; TODO better usage like for:
;; (forall? [x xs] pred)
;; (exists? [x xs] pred)

(defn default-val [v d]
  "returns v if not nil, else d"
  (if (nil? v) d v))

(defn positions [pred coll]
  (keep-indexed (fn [idx x] (when (pred x) idx))
                coll))

(defn disjoint? [S1 S2]
  (empty? (s/intersection (S1 S2))))

(def sum (partial reduce +))

(def product (partial reduce *))
