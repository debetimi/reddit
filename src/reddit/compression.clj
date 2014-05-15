(ns reddit.compression
  (:require [clojure.string :as s :refer [split]])
  (:import (java.lang IllegalArgumentException)))

;http://www.reddit.com/r/dailyprogrammer/comments/25hlo9/5142014_challenge_162_intermediate_novel/

(def input-file "resources/story.txt")
(def output-file "resources/recompressed.txt")
(def dictionary (atom []))
(def lookup (atom {}))
(def compressions (atom []))
(def uppercased  #"([A-Z]+)([\.\!\,\?\;\:]?)")
(def capitalized  #"([A-Z][a-z]+)([\.\!\,\?\;\:]?)")
(def lowercased #"([a-z]+)([\.\!\,\?\;\:]?)")

(defn get-data
  []
  (split (s/replace (slurp input-file) #"\n" " \n ") #"[^\S+\n]"))

(defn add-compression
  [cmp]
  (swap! compressions conj str cmp))

(defn extract
  [phrase regex]
  (let [matches (re-matches regex phrase)
        word (keyword (s/lower-case (second matches)))
        punc (nth matches 2)]
    (when-not (word @lookup)
      (swap! lookup assoc word (count @dictionary))
      (swap! dictionary conj word))
    (let [idx (word @lookup)]
      (condp = regex 
        uppercased (add-compression (str idx "!"))
        capitalized (add-compression (str idx "^"))
        lowercased (add-compression (str idx)))
      (if-not (empty? punc)
        (add-compression punc)))))

(defn compress
  []
  (let [data (get-data)]
    (doseq [phrase data]
            (condp re-matches phrase 
              uppercased (extract phrase uppercased)
              capitalized (extract phrase capitalized) 
              lowercased (extract phrase lowercased)
              #"\n" (add-compression "R") 
              (throw (IllegalArgumentException. (str "Invalid Option " phrase)))))
    (add-compression "E")
    (let [dict-str (s/join "\n" (map name @dictionary))
          compression-str (s/join " " @compressions)]
      (spit output-file (s/join "\n" [(count @dictionary) dict-str compression-str])))))
