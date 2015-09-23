(ns dhsr.reactknow2014.add-examples
  (:use [common])
  (:use [dhsr.reactknow2014.queries]))

(def q0
  "ground. ==> yes"
  {:formula ['bus 'e 'p2]
   :time 11})

(def q1
  "==> X=e P=p2"
  {:formula ['bus '?X '?P]
   :time 11})

(def q2
  "==> U=11"
  {:formula ['bus 'e 'p2]
   :time '?U})

(def q3
  "==> X=e P=p2 U=11, X=c, P=p1, U=2"
  {:formula ['bus '?X '?P]
   :time '?U})

(def q4
  "==> X=tr Y=d U=8, X=bus Y=e U=11. (higher order!)"
  {:formula [:window 1 ['?X '?Y 'p2]]
   :time '?U})

(def q5
  "==> X=tr Y=a, X=bus Y=c"
  {:formula [:window 1 [:diamond ['?X '?Y 'p1]]]
   :time 3})

(def q6
  "==> ?T=2, ?U=[2,7]"
  {:formula [:window 1 [:at '?T ['tr 'a 'p1]]]
   :time '?U})

(def q7
  "==> 1335 substitutions"
  {:formula [:at '?T [:window 1 [:diamond [['tr '?X '?P] :or ['bus '?Y '?Q]]]]]
   :time 13})

(def query1_7_partially_ground
  {:formula [:window 1 [[:diamond ['tr '?X 'p2]]
                        :and
                        [:diamond ['bus '?Y 'p2]]]]
   :time '?U})
