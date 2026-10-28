;; Copyright 2023-2025 Ingy dot Net
;; This code is licensed under MIT license (See License for details)

;; Glojure implementation of the subset of babashka.http-client used by ys.v0.

(ns babashka.http-client
  (:require [clojure.string :as str])
  (:refer-clojure :exclude [get]))

;;------------------------------------------------------------------------------
;; Helper functions
;;------------------------------------------------------------------------------

(defn- bytes-to-str [b]
  "Convert Go []byte to string"
  (fmt.Sprintf "%s" b))

;;------------------------------------------------------------------------------
;; HTTP functions
;;------------------------------------------------------------------------------

(defn- request [method url opts]
  ;; TODO: Support request options beyond :body and :headers.
  (let [body-str (or (:body opts) "")
        reader (strings.NewReader body-str)
        [req req-err] (net:http.NewRequest method url reader)]
    (if (not (nil? req-err))
      {:status 500 :body (str "Failed to create request: " (.Error req-err))}
      (do
        ;; Set headers (convert keyword keys to strings)
        (doseq [[k v] (:headers opts)]
          (.Set (.Header req) (name k) (str v)))
        ;; Execute request
        (let [client net:http.DefaultClient
              [resp resp-err] (.Do client req)]
          (if (not (nil? resp-err))
            {:status 500 :body (str "Request failed: " (.Error resp-err))}
            (let [[body-bytes body-err] (io.ReadAll (.Body resp))]
              (.Close (.Body resp))
              ;; TODO: Include response headers and other response metadata.
              {:status (.StatusCode resp)
               :body (if (nil? body-err)
                       (bytes-to-str body-bytes)
                       (str "Failed to read response: " (.Error body-err)))})))))))

(defn delete ([url] (delete url {})) ([url opts] (request "DELETE" url opts)))
(defn head ([url] (head url {})) ([url opts] (request "HEAD" url opts)))
(defn options ([url] (options url {})) ([url opts] (request "OPTIONS" url opts)))
(defn patch ([url] (patch url {})) ([url opts] (request "PATCH" url opts)))
(defn post ([url] (post url {})) ([url opts] (request "POST" url opts)))
(defn put ([url] (put url {})) ([url opts] (request "PUT" url opts)))

(defn get
  "HTTP GET request
   url: string
   opts (optional): {:headers {\"Key\" \"Value\"}}
   returns: {:status N :body \"...\"}"
  ([url] (get url {}))
  ([url opts] (request "GET" url opts)))

(comment
  ;; Test examples
  (get "https://api.github.com/zen")
  (post "https://httpbin.org/post"
        {:headers {"Content-Type" "application/json"}
         :body "{\"test\": true}"}))
