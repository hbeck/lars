(ns dhsr.aaai2015.programs
  (:use [common])
  (:use [dhsr.aaai2015.stream-semantics])
  (:use [dhsr.aaai2015.streaming-data])
  (:use [dhsr.aaai2015.windows])
  (:require [clojure.set :as s])
  (:require [clojure.core.match :as m]))

;;
;;   Programs
;;

;; - rule form: [head :- body]  
;; - no explicit distinction between intensional and
;;   extensional predicates  
;; - instead, the set of extensional predicates is implicitly defined
;;   by the data stream

(defn coll-as-conjunction [x]
  "[x1 x2 x3] ==> [fm1 :and [fm2 :and fm3]]
   used for rule bodies and programs, i.e., sets of rules"
  (when (not (empty? x))
    (if (= 1 (count x))
      (first x)
      [(first x) :and (coll-as-conjunction (rest x))])))

(defn parse-rule [r]
  "a :- b c ==> {:head a, :body [b c]}"
  (let [idx (first (positions #{:-} r))]
    (case idx
      nil (if (< 1 (count r)) (assert false)
             {:head (first r), :body nil})
      0   (do (print "constraints not supported") (assert false))
      1   {:head (first r), :body (subvec r 2)})))
   
(defn rule-as-formula [r]
  "[a :- b [:not c]] ==> [[b :and [:not c]] :implies a]"
  (let [r' (parse-rule r)
        h  (:head r')
        b  (coll-as-conjunction (:body r'))]
    (if (empty? b) h [b :implies h])))

;; program: #{[..rule1..] ... [..ruleN..]}

(defn satisfies-program? [M t P]
  (let [implications (map rule-as-formula P)]
    (satisfies-formula? M t (coll-as-conjunction implications))))

(defn interpretation-stream? [I D]
  (and
   (substream? D I)
   (let [ats-ext (atoms-in-stream D)
         ats-new (atoms-in-stream (stream-minus I D))
         preds-ext (set (map predicate-symbol ats-ext))
         preds-new (set (map predicate-symbol ats-new))]
     (empty? (s/intersection preds-ext preds-new)))))

(defn model? [M P D t]
  (let [I (:stream M)]
    (and
     (interpretation-stream? I D)
     (satisfies-program? M t P))))

(defn minimal-model? [M P D t]
  (let [SM  (:stream M)
        B   (:data M)
        Int (stream-minus SM D)] ;; intensional part (=guess)
    (and
     (model? M P D t)
     ;; minimal:
     (not (exists?
           (enumerate-proper-substreams-fixed-T Int (:T D))
           (fn [S]
             (let [S' (stream-union D S)
                   M' {:stream S', :data B}]
               (model? M' P D t))))))))

(defn reduct [P M t]
  (let [satisfies-rule-body?
        (fn [r] (let [b  (:body (parse-rule r))
                      b' (coll-as-conjunction b)]
                  (satisfies-formula? M t b')))]
  (set (filter satisfies-rule-body? P))))

(defn answer-stream? [I args]
  (let [P (:program args)
        D (:data-stream args)
        t (:t args)
        B (default-val (:data args) #{})
        M {:stream I, :data B}
        PMt (reduct P M t)]
    (minimal-model? M PMt D t)))

;; TODO: answer stream generation
