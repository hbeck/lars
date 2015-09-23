(ns dhsr.aaai2015.queries
  (:use [common])
  (:use [dhsr.aaai2015.streaming-data])
  (:use [dhsr.aaai2015.windows])
  (:use [dhsr.aaai2015.stream-semantics])
  (:require [clojure.core.match :as m])
  (:require [clojure.math.combinatorics :as c])
  (:require [clojure.set :as s]))

;;
;;  3.2 Queries
;;

;;  * Query (Def 9) *
;;  {:formula Formula, :time query-time}

(defn variable? [x]
  "'?X ==> true, 'X ==> false"
  (= \? (get (str x) 0)))

(defn variables-of-atom [a]
  "determines variables based on ? sign.
   eg: [tr '?X '?Y] ==> #{?X ?Y}.
   allows predicate to be a variable."
  (set (filter variable? a)))

(defn variables [fm] 
  (m/match fm      
   [a :and b]     (s/union (variables a) (variables b))
   [a :or b]      (s/union (variables a) (variables b))
   [a :implies b] (s/union (variables a) (variables b))
   [:not a]       (variables a)
   [:box a]       (variables a)
   [:diamond a]   (variables a)
   [:at t' a]     (variables a)
   [:window i a]  (variables a)
   a              (variables-of-atom a)))

(defn time-variables-fm [fm]
  (m/match fm      
   [a :and b]     (s/union (time-variables-fm a) (time-variables-fm b))
   [a :or b]      (s/union (time-variables-fm a) (time-variables-fm b))
   [a :implies b] (s/union (time-variables-fm a) (time-variables-fm b))
   [:not a]       (time-variables-fm a)
   [:box a]       (time-variables-fm a)
   [:diamond a]   (time-variables-fm a)
   [:at t' a]     (s/union (if (variable? t') #{t'} #{})
                           (time-variables-fm a))
   [:window i a]  (time-variables-fm a)
   a              #{}))

(defn time-variables-query [q]
  (s/union (time-variables-fm (:formula q))
           (let [t (:time q)]
             (if (variable? t) #{t} #{}))))                              

(defn ground-atom? [a]
  (empty? (variables-of-atom a)))

(defn ground? [fm] 
  (m/match fm      
   [a :and b]     (and (ground? a) (ground? b))
   [a :or b]      (and (ground? a) (ground? b))
   [a :implies b] (and (ground? a) (ground? b))
   [:not a]       (ground? a)
   [:box a]       (ground? a)
   [:diamond a]   (ground? a)
   [:at t' a]     (ground? a)
   [:window i a]  (ground? a)
   a              (ground-atom? a)))

(defn ground-query? [q]
  (and (ground? (:formula q))
       (not (variable? (:time q)))))

;; variable assignment (var-as):          map from variables to constants
;; time variable assignment (timevar-as): map from time variables to time points
;; query assignment (query-as):           {:var-as variable-assignment,
;;                                         :timevar-as timevariable-assignment}

(defn assign-if-mapping [x as]
  (cond (variable? x) (if-let [y (as x)] y x)
        :else x))

(defn substitution-of-atom [a var-as]
  "atom a, variable assignment var-as"
  (vec (for [x a] (assign-if-mapping x var-as))))

(defn substitution-of-fm [fm q-as]
  (m/match fm      
   [a :and b]      [(substitution-of-fm a q-as) :and (substitution-of-fm b q-as)]
   [a :or b]       [(substitution-of-fm a q-as) :or (substitution-of-fm b q-as)]
   [a :implies b]  [(substitution-of-fm a q-as) :implies (substitution-of-fm b q-as)]
   [:not a]        [:not (substitution-of-fm a q-as)]
   [:box a]        [:box (substitution-of-fm a q-as)]
   [:diamond a]    [:diamond (substitution-of-fm a q-as)]
   [:at t' a]      [:at (assign-if-mapping t' (:timevar-as q-as)) (substitution-of-fm a q-as)]
   [:window i a]   [:window i (substitution-of-fm a q-as)]
   a               (substitution-of-atom a (:var-as q-as))))

(defn substitution [q query-as] 
  {:formula (substitution-of-fm (:formula q) query-as)
   :time    (assign-if-mapping (:time q) (:timevar-as query-as))})

(defn grounds? [query-as q]
  (ground-query? (substitution q query-as)))

(defn compatible? [query-as q S]
  (and (grounds? query-as q)
       (let [tvars (time-variables-query q)
             t-as  (:timevar-as query-as)
             inT   (fn [tvar] (in-interval? (assign-if-mapping tvar t-as) (:T S)))]
        (forall? tvars inT))))

;; naive enumeration

(defn ground-atoms-in-stream [S]
  (apply s/union (vals (S :v))))

(defn constants-in-atom [a]
  (set (filter (fn [x] (not (variable? x))) a)))

(defn constants-in-stream [S]
  "currently including predicate symbols; TODO"    
  (apply s/union (map constants-in-atom (ground-atoms-in-stream S))))

(defn idx [v]
  "index vector of vector, e.g. [:a :b] ==> [0 1]"
  (vec (range (count v))))

(defn enumerate-var-assignments [q S]
  "out: [{?X bus, ..., ?P bus} ... {?X d, ..., ?P d}]"
  (let [v (vec (variables (:formula q)))
        c (vec (constants-in-stream S))
        s (c/selections c (count v))] ; target assignment values
    (map (partial zipmap v) s)))

(defn enumerate-timevar-assignments [q S]
  (let [v (vec (time-variables-query q))
        c (time-points (:T S))
        s (c/selections c (count v))]
    (map (partial zipmap v) s)))

(defn enumerate-query-assignments [q S]
  (map
   (partial zipmap [:var-as :timevar-as])
   (c/cartesian-product (enumerate-var-assignments q S)
                        (enumerate-timevar-assignments q S))))

;;
;;  * Answer (Def 10) *
;;

(defn answer [M q]
  (let [T (:T M)
        v (:v M)
        SM {:T T, :v v}]
  (if (ground-query? q) ;; then yes/no; else list yes-substitutions
    (entails? M SM (:time q) (:formula q)) 
    (for [q-as (enumerate-query-assignments q SM)
          :when (and (compatible? q-as q SM)
                     (answer M (substitution q q-as)))]
      [(:var-as q-as) (:timevar-as q-as)]))))


(defn print-table [ans]
  (if-not (empty? ans)
    (let [e (first ans)
          header (concat (keys (first e)) (keys (second e)))]
      (do
        (print "\n")
        (doseq [h header] (print h "\t"))
        (print "\n")
        ;;
        (doseq [e ans]
          (let [m (merge (e 0) (e 1))]
            (print "\n")
            (doseq [h header] (print (m h) "\t"))))))))




