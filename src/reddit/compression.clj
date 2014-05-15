(ns reddit.compression
  (:require [clojure.string :as string]))

;; Question
;; http://www.reddit.com/r/dailyprogrammer/comments/25clki/5122014_challenge_162_easy_novel_compression_pt_1/

(def input-file "resources/reader.txt")

(defn get-data 
  "Read in data to a string, then return
   vector of string split on spaces"
  []
  (string/split (string/replace (slurp input-file) #"\n" " ") #"\s+"))

(defn get-fn 
  "Match the command to a function"
  [cmd]
  (condp re-matches cmd 
    #"\d+!" (comp print string/upper-case)
    #"\d+\^" (comp print string/capitalize)
    #"\d+" (comp print string/lower-case)))

(defn get-index 
  [cmd]
  "Extract the index from command and convert to integer"
  (read-string (last (re-matches #"(\d+).*" cmd))))

(defn set-and-print
  "Set the boolean for printing a space on the next iteration
   and call your print function on provided word"
  [printfn word add-space bool]
  (reset! add-space bool)
  (printfn word))

(defn run []
  (let [data (get-data) 
        num-words (-> data first read-string)
        words (-> data rest vec (subvec 0 num-words))
        commands (-> data rest vec (subvec num-words))
        i (atom 0)
        add-space? (atom false)
        continue? (atom true)]
    (while @continue? 
      (let [cmd (get commands @i)]
        (condp re-matches cmd
          #"[eE]" (swap! continue? not)
          #"-" (set-and-print print "-" add-space? false) 
          #"[\.\!\,\?\;\:]" (print cmd)
          #"[rR]" (set-and-print println "" add-space? false)
          (let [printfn (get-fn cmd)
                idx (get-index cmd)
                word (get words idx)]
            (if (and @add-space? (not= 0 i)) (print " "))
            (set-and-print printfn word add-space? true))))
      (swap! i inc))))


(comment 
  I do not like them in a house.
  I do not like them with a mouse.
  I do not like them here or there.
  I do not like them anywhere.
  I do not like green eggs and ham.
  I do not like them, Sam-I-am.
  nil)
