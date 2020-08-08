#lang inductive

(ind (List [a Type])
     [nil (List a)]
     [:: (-> a (List a) (List a))])

nil
(:: z nil)     
