(ns reddit.compression
  (:require [clojure.string :as s :refer [split]]))

(def input-file "resources/story.txt")

(def dictionary (atom []))
(def lookup (atom {}))
(def compressions (atom []))

(defn get-data
  []
  (split (s/replace (slurp input-file) #"\n" " \n ") #"[^\S+\n]"))
