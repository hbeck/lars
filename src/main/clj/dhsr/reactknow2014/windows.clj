(ns dhsr.reactknow2014.windows
  (:use [dhsr.reactknow2014.streaming-data])
  (:require [clojure.math.numeric-tower :as math])
  (:require [clojure.set :as s]))

;;
;;  2.2 Windows
;;

(defn default-val [m k d]
  "returns the value of key k in map m, or default value d, if nil"
  (let [v (m k)]
    (if (nil? v) d v)))

(defn time-based-window-prms [prms]
  "checks arguments and returns [l u d] with according defaults"
  (let [l (default-val prms :l 0)
        u (default-val prms :u 0)
        d (default-val prms :d 1)]
    (do
      (assert (<= d (+ l u)))
    [l u d])))

;;  * time-based window (Def 3) *
;;
;;  paper uses fixed parameters (see below).
;;  with parameters as arguments:
;;
;; (defn time-based-window [S t prms]
;;   (let [T (:T S)
;;         v (:v S)
;;         [l u d] (time-based-window-prms prms)
;;         [tmin tmax] T
;;         t' (* (math/floor (/ t d)) d)
;;         tl (max tmin (- t' l))
;;         tu (min (+ t' u) tmax)
;;         T' [tl tu]
;;         v' (restrict-interpretation v T')]
;;     {:T T', :v v'}))
  
(defn mk-time-based-window-fn [prms]
  "create time-based window function with fixed parameters"
  (let [[l u d] (time-based-window-prms prms)]
    (fn [S t]
      {:pre [(in-interval? t (:T S))]}
      (let [T (:T S)
            v (:v S)
            [tmin tmax] T
            t' (* (math/floor (/ t d)) d)
            tl (max tmin (- t' l))
            tu (min (+ t' u) tmax)
            T' [tl tu]
            v' (restrict-interpretation v T')]
        {:T T', :v v'}))))


;;  helper for tuple-based window

(defn find-tl [S t l]
  (let [[tmin tmax] (:T S)
        p  (fn [t'] (>= (cardinality (restriction S [t' t])) l))
        ts (filter p (timepoints [tmin t]))]
    (apply max (conj (set ts) tmin))))

(defn find-tu [S t u]
  (let [[tmin tmax] (:T S)
        p  (fn [t'] (>= (cardinality (restriction S [(+ t 1) t'])) u))
        ts (filter p (timepoints [t tmax]))] ; paper has bug there, says [(+ t 1) tmax] instead of [t tmax]
    (apply min (conj (set ts) tmax))))

(defn ensure-exact-nr-of-tuples [S t l u]
  "in given stream throws away tuples at
   tl=tmin and tu=tmax of (:T S) to ensure exactly
   l tuples to occur before t and u tuples after t"
  (let [[tl tu] (:T S)
        v (:v S)
        cl (cardinality (restriction S [tl t]))
        cu (cardinality (restriction S [(+ t 1) tu]))
        diffl (- cl l)
        diffu (- cu u)
        setl (set (drop diffl (v tl)))
        setu (set (drop diffu (v tu)))
        v (if (empty? setl) v (merge v {tl setl}))
        v (if (empty? setu) v (merge v {tu setu}))]
    {:T (:T S) :v v}))

(defn tuple-based-window-prms [prms]
  "checks arguments and returns [l u d] with according defaults"
  (let [l (default-val prms :l 0)
        u (default-val prms :u 0)]
    [l u]))

;;  * tuple-based window (Def 4) *

(defn mk-tuple-based-window-fn [prms]
  "create tuple-based window function with fixed parameters"
  (let [[l u] (tuple-based-window-prms prms)]
    (fn [S t]
      {:pre [(in-interval? t (:T S))]}
      (let [tl (find-tl S t l)
            tu (find-tu S t u)            
            T' [tl tu]
            v' (restrict-interpretation (:v S) T')]
        (ensure-exact-nr-of-tuples {:T T', :v v'} t l u)))))

;; helper for partition-based window

(defn idx-subinterpretation [v idx i]
  (let [kv (for [[k val] v]
             [k (set (filter #(= i (idx %)) val))])
        val-not-empty (fn [[k val]] (not-empty val))]
    (into {} (filter val-not-empty kv))))
  
(defn idx-substream [S idx i]
  {:T (:T S), :v (idx-subinterpretation (:v S) idx i)})

;; tuple-counts n(i)=(l_i,u_i) as function returning vector [l_i u_i]    

(defn partition-based-window-prms [prms]
  "creates defaults, returns [I idx n]"
  (let [n (into {} (for [[k v] (:n prms)]
                     (let [[l u] (tuple-based-window-prms v)]
                       [k {:l l, :u u}])))]
    [(:I prms) (:idx prms) n]))

;;  * partition-based window (Def 5) *

(defn mk-partition-based-window-fn [prms]
  "create tuple-based window function with fixed parameters"
  (let [[I idx n] (partition-based-window-prms prms)]
    (fn [S t]
      {:pre [(in-interval? t (:T S))]}
      (let [fsubstr (fn [i] (let [S' (idx-substream S idx i)
                                  w  (mk-tuple-based-window-fn (n i))]
                              (w S' t)))
            substreams (map fsubstr I)
            Ts  (map :T substreams)
            vs  (map :v substreams)
            tl  (apply min (for [[tmin tmax] Ts] tmin))
            tu  (apply max (for [[tmin tmax] Ts] tmax))
            T'  [tl tu]
            v'  (apply s/union vs)]
        {:T T', :v v'}))))
                              
                          

