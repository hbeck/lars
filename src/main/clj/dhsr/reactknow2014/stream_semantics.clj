(ns dhsr.reactknow2014.stream-semantics
  (:use [common])
  (:use [dhsr.reactknow2014.streaming-data])
  (:use [dhsr.reactknow2014.windows])  
  (:require [clojure.core.match :as m]))

;;
;;  3.1 Stream Semantics
;;

(defn mk-extended-window-fn [w ch]
  "create extended window function based on choice function"
  (fn [S1 S2 t] (w (ch S1 S2) t)))

(defn ch1 [S1 S2] S1)
(defn ch2 [S1 S2] S2)  

;;  * Structure (Def 7) *
;;
;;  {:T [tmin tmax]
;;   :v {tmin #{...}, ..., tmax #{...}}}
;;   :W {1 w1, ..., n wn}

;;  * Entailment (Def 8) *

(defn ||- [M S t fm]
  ;;(print "\n" (:T S) "\t" t fm)
   (m/match fm      
    [a :and b]     (and (||- M S t a) (||- M S t b))
    [a :or b]      (or (||- M S t a) (||- M S t b))
    [a :implies b] (or (not (||- M S t a)) (||- M S t b))
    [:not a]       (not (||- M S t a))
    [:box a]       (forall? (timepoints (:T S)) (fn [u] (||- M S u a)))
    [:diamond a]   (exists? (timepoints (:T S)) (fn [u] (||- M S u a)))
    [:at t' a]     (and (in-interval? t' (:T S)) (||- M S t' a))
    [:window i a]  (let [ew ((:W M) i)
                         SM {:T (:T M), :v (:v M)}
                         S' (ew SM S t)]
                     (||- M S' t a))
    a              (in? a ((:v S) t))))
