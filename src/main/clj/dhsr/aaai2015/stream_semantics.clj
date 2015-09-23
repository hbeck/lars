(ns dhsr.aaai2015.stream-semantics
  (:use [common])
  (:use [dhsr.aaai2015.streaming-data])
  (:use [dhsr.aaai2015.windows])  
  (:require [clojure.core.match :as m]))

;;
;;   Stream Semantics
;;

(defn ch1 [S1 S2] S1)
(defn ch2 [S1 S2] S2)  

;;  * Structure (Def 6) *
;;
;;  straighforward would be:
;;
;;  {:T [tmin tmax]
;;   :v {tmin #{...}, ..., tmax #{...}}
;;   :W {type1 w1, ..., typen wn}
;;   :B #{...}}
;;
;;  use instead:
;;  {:stream {:T [tmin tmax],
;;            :v {tmin #{...}, ..., tmax #{...}}}
;;   :data}
;;  and omit W; give window functions directly (not via type)

;;  * Entailment (Def 7) *
;;  for windows, use a map that includes
;;  - the choice function ch (optionally, default ch2)
;;  - the type specific parameters x
;;  use window function w directly instead of reference via type \iota
;; e.g.: {:ch ch2 :l 3 :u 1}, i.e., x = (dissoc chw :ch)

(defn entails? [M S t fm]
  ;;(print "\n" (:T S) "\t" t fm)
  (let [SM (:stream M)]
    (and
     (substream? S SM)
     (in? t (:T S))
     (m/match fm      
      [a :and b]         (and (entails? M S t a) (entails? M S t b))
      [a :or b]          (or (entails? M S t a) (entails? M S t b))
      [a :implies b]     (or (not (entails? M S t a)) (entails? M S t b))
      [:not a]           (not (entails? M S t a))
      [:box a]           (forall? (time-points (:T S)) (fn [u] (entails? M S u a))) ;; TODO better: (forall? [t (time-points (:T S))] ...)
      [:diamond a]       (exists? (time-points (:T S)) (fn [u] (entails? M S u a)))
      [:at t' a]         (and (in-interval? t' (:T S)) (entails? M S t' a))
      [:window w chx a]  (let [ch  (default-val (:ch chx) ch2)
                               x   (dissoc chx :ch) ;; not essential
                               S'  (w (ch SM S) t x)]
                               (entails? M S' t a))
      a                  (or (in? a ((:v S) t)) (in? a (:data M)))))))

(defn satisfies-formula? [M t fm]
  (entails? M (:stream M) t fm))

(defn satisfies-formula-set? [M t s]
  (forall? s (fn [el] (satisfies-formula? M t el))))
