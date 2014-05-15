(ns reddit.c160
  (:refer-clojure :exclude [== record?])
  (:require [clojure.walk :refer [keywordize-keys]]
   [clojure.contrib.generic.math-functions :as mathfn]
   [clojure.contrib.math :as math]
   [clojure.set :as set])
  (:use clojure.core.logic))

(def input-file "resources/cs160_input.txt")

;;; http://www.reddit.com/r/dailyprogrammer/comments/2451r5/4282014_challenge_160_easy_trigonometric_triangle/


;; Angles Sum to 180
;; A + B + C = 180

;; Law of Cosines c^2 = a^2 + b^2 - 2*a*b*cos(C)

;; Law of Sines
;; a/sin(A) = b / sin(B) = c (/ sin (C))

(def info
  (atom (keywordize-keys
    (into {}
      (map (comp vec rest)
        (re-seq #"([a-zA-Z])=(\d+)" (slurp input-file)))))))

(doseq [k (keys @info)]
  (swap! info assoc k (read-string (k @info))))

(defn solve-by-sides []
  (let [a (:a @info)
        b (:b @info)
        c (:c @info)]
    (when (nil? a)
      (swap! info assoc :a (math/sqrt (- (mathfn/sqr c) (mathfn/sqr b)))))
    (when (nil? b)
      (swap! info assoc :b (math/sqrt (- (mathfn/sqr c) (mathfn/sqr a)))))
    (when (nil? c)
      (swap! info assoc :c (math/sqrt (+ (mathfn/sqr a) (mathfn/sqr b)))))
    (swap! info assoc :A (mathfn/acos (/ (:a @info) (:c @info))))
    (swap! info assoc :B (mathfn/acos (/ (:b @info) (:c @info))))))

(defn solve-by-sines []
  (let [A (:A @info)
        B (:B @info)
        C (:C @info)
        a (:a @info)
        b (:b @info)
        c (:b @info)]
    (when (nil? A)
      (swap! info assoc :A 180 - B -  C))
    (when (nil? B)
      (swap! info assoc :B 180 - A - C))
    (when (nil? C)
      (swap! info assoc :C 180 - A - B))
    (if a
      (swap! info assoc :c (/ a (mathfn/sin A))))))
(println info "\n")
info


info
(solve-by-sides)



