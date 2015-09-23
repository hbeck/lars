(ns dhsr.aaai2015.streaming-data
  (:use [common])
  (:require [clojure.math.combinatorics :as c])
  (:require [clojure.set :as s]))

;;
;;  Streaming Data
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

(defn time-points [in]
  "creates list of time-points based on interval in, e.g.,
   [3,6] --> (3 4 5 6)"
  (range (lower-bound in) (inc (upper-bound in))))

(defn time-points-with-data [S]
  (let [T (:T S), v (:v S)]
    (vec (filter (fn [t] (not (empty? (v t))))
                 (time-points T)))))

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

(defn substream? [S' S]
  (and (subinterval-of? (:T S') (:T S))
       (subinterpretation-of? (:v S') (:v S) (:T S'))))

(def window-of? substream?)

(defn cardinality [S]
  (let [v (:v S), T (keys v)]
    (reduce + (map #(count (v %)) T))))

(defn restrict-interpretation [v T]
  (apply dissoc v (filter #((complement in-interval?) % T) (keys v))))

(defn restriction [S T]
  {:T T, :v (restrict-interpretation (:v S) T)})

;;

(defn atoms-in-stream [S]
  ;;(println S)
  (set (apply concat (for [t (:T S)] ((:v S) t)))))

(defn predicate-symbol [at]
  (if (vector? at) (at 0) at))

(defn stream-join [S S' T-join v-join]
  (let [T (T-join (:T S) (:T S'))]
  {:T T
   :v (v-join T (:v S) (:v S'))}))

(defn mkfn-set-op-based-v-left-join [op]
  "use time points of v1 and apply set op there"
  (fn [T v1 v2]
    (merge
     {} ;; avoid nil
     (apply merge
            (set (for [t (time-points T)]
                   (let [ats (op (v1 t) (v2 t))]
                     (if-not (empty? ats) ;; do not store t -> #{}
                       {t ats}))))))))

(defn stream-minus [S S']
  "take timeline of left stream, set difference on interpretations"
  (let [diff (mkfn-set-op-based-v-left-join s/difference)]
    (stream-join S S' (fn [l r] l) diff)))

(defn merge-intervals [T1 T2]
  "return smallest interval including T1 T2"
  (let [tmin (min (lower-bound T1) (lower-bound T2))
        tmax (max (upper-bound T1) (upper-bound T2))]
    [tmin tmax]))  

(defn stream-union [S S']
  (let [u (mkfn-set-op-based-v-left-join s/union)]
    (stream-join S S' merge-intervals u)))

(defn enumerate-subintervals [T]
  (let [[tmin tmax] T]
    (for [tlow (time-points [tmin tmax]),
          tup  (time-points [tlow tmax])]
      [tlow tup])))

(defn atom-subsets-for-timepoint [S t]
  (let [ats ((:v S) t)]
    (map set (c/subsets ats))))

;;  naive enumeration of substreams:
;;
;;  outer loop: sub-intervals S' (timeline)
;;  inner loop: sub-interpretations for each timepoint in timepline of S'
;;
;;  ex input stream S:
;;  1 -> #{a b}, 2 -> #{c}
;;
;;  substreams:
;;  1 -> #{a}, 2 -> #{c}
;;  1 -> #{b}, 2 -> #{c}
;;  1 -> #{},  2 -> #{c}
;;  1 -> #{a}, 2 -> #{}
;;  1 -> #{b}, 2 -> #{}
;;  1 -> #{},  2 -> #{}
;;
;;  ==> cross-product of all 2^n per timepoint
;;  (c/cartesian-product
;;    (map set (c/subsets #{a b}))
;;    (map set (c/subsets #{c})))
;;
;;    ((#{} #{}) (#{} #{c}) (#{a} #{}) (#{a} #{c}) (#{b} #{})
;;     (#{b} #{c}) (#{a b} #{}) (#{a b} #{c}))
;;
;;  i.e.:
;;  (c/cartesian-product
;;    (atom-subsets-for-timepoint S' 1)
;;    (atom-subsets-for-timepoint S' 2))
;;
;;  each one of the resulting list corresponds to a mapping of according
;;  time points
;;
;;  thus:
;;  1. create subsets for every time point t, i.e., t -> 2^n subsets [ss4t]
;;  2. cross product over all time points t in T
;;
(defn enumerate-substreams-fixed-T [S T]
  (let [S'   (restriction S T) ;; fix substream S' based on T
        tp   (time-points (:T S'))
        ss4t (for [t tp] (atom-subsets-for-timepoint S' t))
        cp   (apply c/cartesian-product ss4t)]
    (for [mp cp] ;; iterate mappings
      (let [v' (zipmap tp mp) ;; joint timepoints with their subsets
            v  (into {} (filter #(not-empty (second %)) v'))]
        {:T T, :v v}))))
  
(defn enumerate-substreams [S]
  (apply concat ;; unnest collected streams per fixed timeline
    (for [T' (enumerate-subintervals (:T S))] ;; for all sub-intervals T' of T
      (enumerate-substreams-fixed-T S T'))))

(defn enumerate-proper-substreams-fixed-T [S T]
  (let [Ss (set (enumerate-substreams-fixed-T S T))]
    (apply list (s/difference Ss #{S}))))

;; (defn enumerate-all-substreams [S]
;;   (apply concat ;; unnest collected streams per fixed timeline
;;     (for [T' (enumerate-subintervals (:T S))] ;; for all sub-intervals T' of T
;;       (let [S'   (restriction S T') ;; fix substream S' based on T'
;;             tp   (time-points (:T S'))
;;             ss4t (for [t tp]
;;                    (atom-subsets-for-timepoint S' t))
;;             cp   (apply c/cartesian-product ss4t)]
;;         (for [mp cp] ;; iterate mappings
;;           (let [v (zipmap tp mp)] ;; joint timepoints with their subsets
;;             {:T T', :v v}))))))

(defn print-es [S T]
  (doseq [x (enumerate-substreams-fixed-T S T)]
    (println x)))

(defn print-proper-es [S T]
  (doseq [x (enumerate-substreams-fixed-T S T)]
    (println x)))
    

