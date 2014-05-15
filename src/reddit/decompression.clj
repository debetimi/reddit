(ns reddit.decompression
  (:require [clojure.string :as s :refer [upper-case capitalize
                                          lower-case split join]]))

;; Question
;; http://www.reddit.com/r/dailyprogrammer/comments/25clki/5122014_challenge_162_easy_novel_compression_pt_1/

(def input-file "resources/recompressed.txt")
(def out-file "resources/decompressed.txt")

(def out (atom []))

(defn add
  [info]
  (swap! out conj info))

(defn get-data 
  "Read in data to a string, then return
   vector of string split on spaces"
  []
  (split (s/replace (slurp input-file) #"\n" " ") #"\s+"))

(defn parse
  "Return a subvector of data from the specificed
   indicies"
  [data start & more]
  (let [end (first more)
        sub (if end #(subvec % start end) #(subvec % start))]
    (-> data rest vec sub)))


(defn cmd->fn 
  "Match the command to a function"
  [cmd]
  (condp re-matches cmd 
    #"\d+!" (comp add upper-case)
    #"\d+\^" (comp add capitalize)
    #"\d+" (comp add lower-case)))

(defn get-index 
  [cmd]
  "Extract the index from command and convert to integer"
  (read-string (last (re-matches #"(\d+).*" cmd))))

(defn update
  "Set the boolean for adding a space on the next iteration
   and call your add function on provided word"
  [addfn word add-space bool]
  (reset! add-space bool)
  (addfn word))

(defn decompress 
  []
  (let [data (get-data) 
        num-words (-> data first read-string)
        words (parse data 0 num-words)
        commands (parse data num-words)
        i (atom 0)
        space? (atom false)
        continue? (atom true)]
    (while @continue? 
      (let [cmd (get commands @i)]
        (condp re-matches cmd
          #"[eE]" (swap! continue? not)
          #"-" (update add "-" space? false) 
          #"[\.\!\,\?\;\:]" (add cmd)
          #"[rR]" (update add "\n" space? false)
          (let [f (cmd->fn cmd)
                n (get-index cmd)
                w (get words n)]
            (if @space? (add " "))
            (update f w space? true))))
      (swap! i inc))
    (spit out-file (join @out))))


(comment 
  I do not like them in a house.
  I do not like them with a mouse.
  I do not like them here or there.
  I do not like them anywhere.
  I do not like green eggs and ham.
  I do not like them, Sam-I-am.
  nil)

