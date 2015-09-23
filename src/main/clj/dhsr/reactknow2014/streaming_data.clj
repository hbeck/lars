(ns dhsr.reactknow2014.streaming-data
  (:use [common])
  (:require [clojure.set :as s]))

;;
;;  2.1 Streaming Data
;;

;;  * atom *
;;  [p t1 ... t2] for predicates, ti terms

;;  * interval *
;;  represented as 2-elem vector [min-elem max-elem]

(defn lower-bound [in] (in 0))

(defn upper-bound [in] (in 1))

(defn interval? [in]
  (and (vector? in)
       (= 2 (count in))
       (<= (lower-bound in) (upper-bound in))))

(defn in-interval? [k in]
  (and (>= k (lower-bound in))
       (<= k (upper-bound in))))

(defn subinterval-of? [in' in]
  (and (>= (lower-bound in') (lower-bound in))
       (<= (upper-bound in') (upper-bound in))))

(defn timepoints [in]
  "creates list of timepoints based on interval in, e.g.,
   [3,6] --> (3 4 5 6)"
  (range (lower-bound in) (inc (upper-bound in))))

;;  * Stream (Def 1) *
;;
;;  {:T [tmin tmax]
;;   :v {tmin #{...}, ..., tmax #{...}}}
;;
;;   (store only non-empty mappings in :v)

(defn stream? [S]
  (let [T (:T S), v (:v S)]
    (and (interval? T)
         (apply land (map #(in-interval? % T) (keys v))))))

(defn subinterpretation-of? [v' v T]
  "true if v' is a subinterpretation of v based on reference timeline T, i.e.,
   for all t in T, v'(t) subseteq v(t)"
   (apply land (for [t T]
                 (let [ats' (v' t)
                       ats  (v  t)]
                   (cond (empty? ats') true
                         (empty? ats)  false
                         :else (s/subset? ats' ats))))))

(defn substream-of? [S' S]
  (and (subinterval-of? (:T S') (:T S))
       (subinterpretation-of? (:v S') (:v S) (:T S'))))

(def window-of? substream-of?)

(defn cardinality [S]
  (let [v (:v S), T (keys v)]
    (reduce + (map #(count (v %)) T))))

(defn restrict-interpretation [v T]
  (apply dissoc v (filter #((complement in-interval?) % T) (keys v))))

(defn restriction [S T]
  {:T T, :v (restrict-interpretation (:v S) T)})

