(ns reddit.zip
  (:require [clojure.string :as s :refer [upper-case capitalize
                                          lower-case split join]]
            [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

(def dictionary (atom []))
(def out (atom []))
(def lookup (atom {}))
(def compressions (atom []))
(def uppercased  #"([A-Z]+)([\.\!\,\?\;\:]?)")
(def capitalized  #"([A-Z][a-z]+)([\.\!\,\?\;\:]?)")
(def lowercased #"([a-z]+)([\.\!\,\?\;\:]?)")

(defn add
  [info]
  (swap! out conj info))

(defn read-compressed 
  "Read in data to a string, then return
   vector of string split on spaces"
  [input]
  (split (s/replace (slurp input) #"\n" " ") #"\s+"))

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

(defn read-raw 
  [filename]
  (-> (slurp filename) (s/replace #"\n" " \n ")
      (s/replace #"\-" " - ") (split #"[^\S+\n]")))

(defn add-compression
  [cmp]
  (swap! compressions conj cmp))

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

(defn decompress 
  [input output]
  (let [data (read-compressed input) 
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
    (spit output (join @out))))

(defn compress
  [input output]
  (let [data (read-raw input)]
    (doseq [phrase (filter #(not= % "") data)]
      (condp re-matches phrase 
        uppercased (extract phrase uppercased)
        capitalized (extract phrase capitalized) 
        lowercased (extract phrase lowercased)
        #"\n" (add-compression "R")
        #"\-" (add-compression "-")
        (throw (IllegalArgumentException. (str "Invalid Option " phrase)))))
    (add-compression "E")
    (let [dict-str (s/join "\n" (map name @dictionary))
          compression-str (s/join " " @compressions)]
      (spit output (s/join "\n" [(count @dictionary) dict-str compression-str])))))

(def cli-options
  [["-c" nil "Compress File" :id :c :default false]
   ["-d" nil "Decompress File" :id :d :default false]])

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (let [cmd (or (:c options) (:d options))
          input (first arguments)
          output (second arguments)] 
      (when (and cmd input output)
        (case (:c options)
          true (compress input output)
          false (decompress input output))))))
