(ns ys.v0.platform
  (:require [clojure.string :as str]))

(defn args []
  (if (System/getProperty "babashka.version")
    (vec (or *command-line-args* []))
    (vec (rest os.Args))))

(defn arch []
  runtime.GOARCH)

(defn cwd []
  (let [[path err] (os.Getwd)]
    (if (nil? err) path ".")))

(defn env []
  (reduce
   (fn [result entry]
     (let [index (str/index-of entry "=")]
       (if index
         (assoc result
                (subs entry 0 index)
                (subs entry (inc index)))
         result)))
   {}
   (os.Environ)))

(defn executable []
  (let [[path err] (os.Executable)]
    (when (nil? err) path)))

(defn hostname []
  (let [[name err] (os.Hostname)]
    (when (nil? err) name)))

(defn os []
  runtime.GOOS)

(defn pid []
  (os.Getpid))
