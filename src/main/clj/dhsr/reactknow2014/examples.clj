(ns dhsr.reactknow2014.examples
  (:use [common])
  (:use [dhsr.reactknow2014.streaming-data])
  (:use [dhsr.reactknow2014.windows])
  (:use [dhsr.reactknow2014.stream-semantics])
  (:use [dhsr.reactknow2014.queries]))

;;
;;  2.1 Streaming Data
;;

;;  Example 2

(def tr 'tr)
(def bus 'bus)
(def a 'a)
(def c 'c)
(def d 'd)
(def e 'e)
(def p1 'p1)
(def p2 'p2)

(def exS
  {:T [0 13]
   :v {2  #{ [tr a p1], [bus c p1] }
       8  #{ [tr d p2] }
       11 #{ [bus e p2] }}})

;;
;;  2.2 Windows
;;

;;  Example 3

(def w5 (mk-time-based-window-fn {:l 5}))

(let [S (w5 exS 5)]
  (assert (= (:T S) [0 5]))
  (assert (= ((:v S) 2) #{[tr a p1] [bus c p1]}))
  (assert (= (count (:v S)) 1)))

(let [S (w5 exS 11)]
  (assert (= (:T S) [6 11]))
  (assert (= ((:v S) 8) #{[tr d p2]}))
  (assert (= ((:v S) 11) #{[bus e p2]}))
  (assert (= (count (:v S)) 2)))

(def w213 (mk-time-based-window-fn {:l 2 :u 1 :d 3}))

(let [S (w213 exS 5)]
  (assert (= (:T S) [1 4]))
  (assert (= ((:v S) 2) #{[tr a p1] [bus c p1]}))
  (assert (= (count (:v S)) 1)))

(let [S (w213 exS 11)]
  (assert (= (:T S) [7 10]))
  (assert (= ((:v S) 8) #{[tr d p2]}))
  (assert (= (count (:v S)) 1)))  

;;  Example 4

(def wt30 (mk-tuple-based-window-fn {:l 3, :u 0}))

(let [S (wt30 exS 11)]
  (assert (= (:T S) [2 11]))
  (assert (= (count (:v S)) 3))
  (assert (= ((:v S) 8) #{[tr d p2]}))
  (assert (= ((:v S) 11) #{[bus e p2]}))
  (assert (or (= ((:v S) 2) #{[bus c p1]})
              (= ((:v S) 2) #{[tr a p1]}))))

;;  Example 5

(defn idx-tr-1 [a] (if (= tr (first a)) 1 2))

(def wp2tr (mk-partition-based-window-fn {:I #{1 2}
                                          :idx idx-tr-1
                                          :n {1 {:l 2, :u 0}
                                              2 {:l 0, :u 0}}}))

(let [S1  (idx-substream exS idx-tr-1 1)
      S2  (idx-substream exS idx-tr-1 2)
      wt2 (mk-tuple-based-window-fn {:l 2, :u 0})
      wt0 (mk-tuple-based-window-fn {:l 0, :u 0})]
  (assert (= S1 {:T [0 13], :v {2 #{[tr a p1]}, 8 #{[tr d p2]}}}))    ; 1)
  (assert (= S2 {:T [0 13], :v {2 #{[bus c p1]}, 11 #{[bus e p2]}}})) ; 1)
  (assert (= (wt2 S1 13) {:T [2 13], :v {2 #{[tr a p1]}, 8 #{[tr d p2]}}}))
  (assert (= (wt0 S2 13) {:T [13 13], :v {}})))

(let [S (wp2tr exS 13)]
  (assert (= (:T S) [2 13]))
  (assert (= (count (:v S)) 2))
  (assert (= ((:v S) 2) #{[tr a p1]}))
  (assert (= ((:v S) 8) #{[tr d p2]})))

;;
;;  3.1 Stream Semantics
;;

;;  Example 6

(def w5e (mk-extended-window-fn w5 ch2))

(def M6
  {:T (:T exS), :v (:v exS), :W {1 w5e}})

(def alpha6
  [:window 1 [[:diamond [tr d p2]]
              :and
              [:diamond [bus e p2]]]])

(def SM6 exS)

(let [S' (w5e SM6 SM6 11)]
  (assert (= S' {:T [6 11], :v {8 #{[tr d p2]}, 11 #{[bus e p2]}}}))
  (assert (||- M6 S' 8 [tr d p2]))    ; 2)
  (assert (||- M6 S' 11 [bus e p2]))) ; 2)

(assert (||- M6 SM6 11 alpha6))

;;
;;  3.2 Queries
;;

;;  Example 7

(def ?X '?X)
(def ?Y '?Y)
(def ?P '?P)
(def ?U '?U)

(def q1
  {:formula [:window 1 [[:diamond [tr ?X ?P]]
                        :and
                        [:diamond [bus ?Y ?P]]]]
   :time ?U})

(def q2
  {:formula [:window 1 [:diamond [[tr ?X ?P]
                                  :and
                                  [bus ?Y ?P]]]]
   :time ?U})

(def gq-fm [:window 1 [:diamond [[tr a p1] :and [bus c p1]]]])

(assert (ground-query? {:formula gq-fm :time 0}))

(defn assert-all [bs]
  (assert (apply land bs)))

(defn verify-q [fm ts]
  "verify correctness of formula fm queried over timepoints ts"
  (->> ts
       (map #(answer M6 (merge {:formula fm} {:time %})))
       (assert-all)))

(verify-q gq-fm (timepoints [2 7]))
(verify-q [:not gq-fm] (concat (timepoints [0 1]) (timepoints [8 13])))

(defn verify [fm ts]
  "verify that formula fm is entailed at timepoints ts"
  (->> ts
       (map #(||- M6 SM6 % fm))
       (assert-all)))

(let [fm [:window 1 [[:diamond [tr a p1]]
                     :and
                     [:diamond [bus c p1]]]]]
  (verify fm (timepoints [2 7]))
  (verify [:not fm] (concat (timepoints [0 1]) (timepoints [8 13]))))

(let [fm [:window 1 [[:diamond [tr d p2]]
                     :and
                     [:diamond [bus e p2]]]]]
  (verify fm (timepoints [11 13]))
  (verify [:not fm] (timepoints [0 10])))

(defn contains-binding [ans b]
  (->> ans
       (map #(= b %))
       (apply lor)))

(let [ans (answer M6 q1)]
  (->> (for [t (timepoints [2 7])]
         (contains-binding ans [{?P p1, ?Y c, ?X a} {?U t}]))
       (assert-all))
  (->> (for [t (timepoints [11 13])]
         (contains-binding ans [{?P p2, ?Y e, ?X d} {?U t}]))
       (assert-all)))

;;  Example 8

(def q8a
  {:formula [:at ?U [[tr ?X ?P] :and [bus ?Y ?P]]]
   :time 13})

(def q8b
  {:formula [[tr ?X ?P] :and [bus ?Y ?P]]
   :time ?U})

(for [q [q8a q8b]]
  (let [ans (answer M6 q)]
    (assert (= (count ans) 1))
    (assert (contains-binding ans [{?P p1, ?Y c, ?X a} {?U 2}]))))

(def q8c
  {:formula [[tr ?X ?P] :and [bus ?Y ?P]]
   :time 13})

(let [ans (answer M6 q8c)]
  (assert (empty? ans)))

;;  Example 9

(def w+3 (mk-time-based-window-fn {:u 3}))

(def M9
  {:T (:T exS), :v (:v exS), :W {1 (mk-extended-window-fn wp2tr ch2)
                                 2 (mk-extended-window-fn w+3 ch1)}})

(def q3 ; 3)
  {:formula [:window 1 [:box [[tr ?X ?P]
                              :implies
                              [:window 2 [:diamond [bus ?Y ?P]]]]]]
   :time 13})

;;  Errors found
;;  1) Papers states intervals [2 13] instead of [0 13]
;;  2) Paper says $S_1$ instead of $S'$
;;  3) The paper does not talk about all variable bindings, where tr(X,P) is false,
;;     and where, as a consequence, the implication holds trivially.

(def q3-g1
  {:formula [:window 1 [:box [[tr a p1]
                              :implies
                              [:window 2 [:diamond [bus c p1]]]]]]
   :time 13})

(def q3-g2
  {:formula [:window 1 [:box [[tr d p2]
                              :implies
                              [:window 2 [:diamond [bus e p2]]]]]]
   :time 13})

(def M605
  (let [w60 (mk-time-based-window-fn {:l 60})
        w+5 (mk-time-based-window-fn {:u 5})]
    {:T (:T exS), :v (:v exS), :W {1 (mk-extended-window-fn w60 ch1)
                                   2 (mk-extended-window-fn w+5 ch1)}}))

(def q605-1 {:time ?U
             :formula [:window 1 [:box [:window 2 [:diamond [bus ?X p1]]]]]})

(def q2b
  {:formula [:window 1 [:diamond [[tr ?X ?P]
                                  :and
                                  [bus ?Y ?P]]]]
   :time ?U})

(def exS-not
  {:T [0 13]
   :v {2  #{ [tr a p1] } ; removed: , [bus c p1]
       8  #{ [tr d p2] }
       11 #{ [bus e p2] }}})

(def M-not
  {:T (:T exS-not), :v (:v exS-not), :W {1 w5e}})

(def q-not
  {:time 13
   :formula [:diamond [[tr ?X ?P] :and [:not [:diamond [:window 1 [bus ?Y ?P]]]]]]})
