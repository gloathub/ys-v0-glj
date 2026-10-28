;; Copyright 2023-2025 Ingy dot Net
;; This code is licensed under MIT license (See License for details)

;; Glojure implementation of the subset of babashka.process used by ys.v0.

(ns babashka.process
  (:require [clojure.string :as str]))

;;------------------------------------------------------------------------------
;; Helper functions
;;------------------------------------------------------------------------------

(defn- bytes-to-str [b]
  "Convert Go []byte to string"
  (fmt.Sprintf "%s" b))

;;------------------------------------------------------------------------------
;; Process execution functions
;;------------------------------------------------------------------------------

(defn- split-opts [args]
  (if (map? (first args))
    [(first args) (rest args)]
    [{} args]))

(defn sh [& all-args]
  "Execute command directly (first arg is command, rest are arguments)"
  ;; TODO: Honor :dir, :env, :in, :out, :err and other process options.
  (let [[_opts args] (split-opts all-args)]
    (if (empty? args)
      {:exit 1 :out "" :err "No command specified"}
      (let [[cmd-name & cmd-args] args
            command (str/split (str cmd-name) #"\s+")
            cmd-name (first command)
            cmd-args (concat (rest command) cmd-args)
            cmd (apply os:exec.Command cmd-name (vec cmd-args))
            [stdout-pipe stdout-err] (.StdoutPipe cmd)
            [stderr-pipe stderr-err] (.StderrPipe cmd)]
        (if (or (not (nil? stdout-err)) (not (nil? stderr-err)))
          {:exit 1 :out "" :err (str "Failed to create pipes")}
          (let [start-err (.Start cmd)]
            (if (not (nil? start-err))
              {:exit 1 :out "" :err (str "Failed to start: " start-err)}
              (let [[stdout-bytes stdout-read-err] (io.ReadAll stdout-pipe)
                    [stderr-bytes stderr-read-err] (io.ReadAll stderr-pipe)
                    wait-err (.Wait cmd)
                    exit-code (if (nil? wait-err)
                                0
                                (.ExitCode (.ProcessState cmd)))]
                {:exit exit-code
                 :out (if (nil? stdout-read-err)
                        (bytes-to-str stdout-bytes)
                        "")
                 :err (if (nil? stderr-read-err)
                        (bytes-to-str stderr-bytes)
                        "")}))))))))

(defn shell [& all-args]
  "Execute command via /bin/sh -c (joins args into single command string)"
  (let [[opts args] (split-opts all-args)
        cmd-str (str/join " " args)]
    (sh opts "/bin/sh" "-c" cmd-str)))

;; TODO: Implement process handles and exec semantics beyond these aliases.
(defn process [& args] (apply shell args))
(defn exec [& args] (apply sh args))
