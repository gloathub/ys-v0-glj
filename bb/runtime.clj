
;; The ys.v0.global namespace owns the mutable runtime state used by the YS
;; standard library. It is portable Clojure with no compiler or SCI
;; dependencies, so it works under the ys runtime, babashka, and JVM Clojure.
;; The ys runtime redirects the hook atoms below at its SCI counterparts.

(ns ys.v0.global)

(def stream-anchors_ (atom {}))
(def doc-anchors_ (atom {}))
(def stream-values (atom []))
(def opts (atom {}))

;; Runtime variables. Under the ys runtime these are shadowed by SCI dynamic
;; vars of the same names; under plain Clojure runtimes ys.v0/init binds them.
(def ^:dynamic _ nil)
(def ^:dynamic ARGS [])
(def ^:dynamic ARGV [])
(def ^:dynamic CWD nil)
(def ^:dynamic DIR nil)
(def ^:dynamic ENV nil)
(def ^:dynamic FILE nil)
(def ^:dynamic INC [])
(def ^:dynamic PUN nil)
(def ^:dynamic RUN {})
(def ^:dynamic VERSION nil)

(def env {})

(def input-hook
  (atom (fn [] *in*)))
(def output-hook
  (atom (fn [] *out*)))
(def error-hook
  (atom (fn [] *err*)))

(defn current-input []
  (@input-hook))

(defn current-output []
  (@output-hook))

(defn current-error []
  (@error-hook))

(defn update-env
  "Update env in the current context."
  [m]
  (let [m (reduce-kv
            (fn [m k v] (if v (assoc m k v) (dissoc m k)))
            env m)]
    (alter-var-root #'env (constantly m))))

(defn reset-env
  "Reset env to its initial state."
  [m]
  (let [m (or m (into {} (System/getenv)))]
    (alter-var-root #'env (constantly m))))

(defn- default-set-underscore [v]
  (alter-var-root #'_ (constantly v)))

(defn- environ-updater [m]
  (fn [env]
    (if (empty? m)
      (reduce dissoc env (keys env))
      (reduce-kv
        (fn [env k v]
          (if v
            (assoc env k v)
            (dissoc env k)))
        env m))))

(defn- default-update-environ [m]
  (alter-var-root #'ENV (environ-updater m)))

;; The ys runtime resets these hooks to SCI-aware implementations.
(def underscore-hook (atom default-set-underscore))
(def environ-hook (atom default-update-environ))

(defn set-underscore
  "Set underscore in the current context."
  [v]
  (@underscore-hook v))

(defn update-environ
  "Update environ in the current context."
  [m]
  (@environ-hook m))

(defn make-environ-updater
  "Build the ENV update fn for a given map. Used by runtime hooks."
  [m]
  (environ-updater m))

(comment
  )


(ns ys.v0.util)

(defn die
  "Throw a YAMLScript exception with a normalized trailing newline.
  Thrown as ex-info because every Clojure runtime YS targets supports
  it (there is no Exception class outside the JVM family)."
  ([] (die "Died"))
  ([msg] (throw (ex-info (str msg "\n") {})))
  ([x & xs] (die (apply str x xs))))

(defmacro catching
  "Evaluate expr, returning fallback if anything is thrown. The caught
  type is chosen when THIS file loads (via reader conditional), so
  consumers stay portable across runtimes with different catch types
  (glojure catches go/any; the JVM family catches Exception)."
  [expr fallback]
  `(try
     ~expr
     (catch #?(:glj go/any :default Exception) e# ~fallback)))

(defn backend
  "Resolve a backend library var at call time, so that Clojure runtimes
  without the library (like jolt) can still load the ys.v0 namespaces.
  Dies with a clear message when the library is not available.
  Requires then resolves (rather than requiring-resolve) because some
  runtimes only resolve reliably in already loaded namespaces."
  [sym]
  (or
    (catching
      (do
        (require (symbol (namespace sym)))
        (resolve sym))
      nil)
    (die (str "The '" (namespace sym) "' library is not available"
           " in this Clojure runtime"))))

(defn- resolve-assignment-key
  "Resolve one dotted-assignment key against its current container."
  [container [kind value]]
  (case kind
    :bare
    (if (map? container)
      (cond
        (contains? container value) value
        (contains? container (str value)) (str value)
        (contains? container (keyword value)) (keyword value)
        :else (str value))
      (str value))

    :call (value container)
    :value value))

(defn- resolve-assignment-path
  "Resolve dotted-assignment step descriptors into an exact key path."
  [root steps]
  (loop [container root
         steps (seq steps)
         path []]
    (if-let [step (first steps)]
      (let [key (resolve-assignment-key container step)]
        (recur (get container key) (next steps) (conj path key)))
      path)))

(defn assoc-assignment-path
  "Associate a value at a dotted-assignment path."
  [root steps value]
  (assoc-in root (resolve-assignment-path root steps) value))

(defn update-assignment-path
  "Apply an update function at a dotted-assignment path."
  [root steps f & args]
  (apply update-in root (resolve-assignment-path root steps) f args))

(defn pprint*
  "Pretty print x via clojure.pprint when available, else plain prn."
  [x]
  (if-let [f (catching (backend 'clojure.pprint/pprint) nil)]
    (f x)
    (prn x)))

(defmacro condf
  "Like condp, but each clause predicate is called with the same value."
  [x & clauses]
  `(condp (fn [f# x#] (f# x#)) ~x ~@clauses))

(defmacro cond-lets
  "Try groups of let-style bindings and run the first successful body."
  {:style/indent [0]}
  [& clauses]
  (when clauses
    `(if-lets ~(first clauses)
       ~(if (next clauses)
          (second clauses)
          (die "Odd number of forms"))
       (cond-lets ~@(nnext clauses)))))

(defn eprint
  "Print values to stderr without adding a newline."
  [& xs]
  (binding [*out* *err*]
    (apply print xs)))

(defn eprintln
  "Print values to stderr with a newline."
  [& xs]
  (binding [*out* *err*]
    (apply println xs)))

(defmacro if-lets
  "Like if-let, but require every binding/test pair to succeed."
  ([bindings then]
   `(if-lets ~bindings ~then nil))
  ([bindings then else]
   (if (seq bindings)
     `(if-let [~(first bindings) ~(second bindings)]
        (if-lets ~(drop 2 bindings) ~then ~else)
        ~else)
     then)))

(defn macro?
  "Return true when a symbol resolves to a macro var."
  [x]
  (and
    (symbol? x)
    (when-let [x (resolve x)]
      (:macro (meta x)))))

(defn type-name
  "Return a readable type name for diagnostics."
  [x]
  (condf x
    map? "Map"
    set? "Set"
    vector? "Vector"
    list? "List"
    seq? "Seq"
    (type x)))

(defmacro when-lets
  "Like when-let, but require every binding/test pair to succeed."
  ([bindings & body]
   (if (seq bindings)
     `(when-let [~(first bindings) ~(second bindings)]
        (when-lets ~(drop 2 bindings) ~@body))
     `(do ~@body))))

(intern 'clojure.core 'die die)
(intern 'clojure.core 'eprint eprint)
(intern 'clojure.core 'eprintln eprintln)

(intern 'clojure.core
        (with-meta 'condf {:macro true})
        @#'condf)


(intern 'clojure.core
        (with-meta 'cond-lets {:macro true})
        @#'cond-lets)

(intern 'clojure.core
        (with-meta 'if-lets {:macro true})
        @#'if-lets)

(intern 'clojure.core
        (with-meta 'when-lets {:macro true})
        @#'when-lets)

(comment
  )


;; The ys.v0.re namespace defines the regex tokens shared between the YS
;; standard library and the YS compiler. The compiler-only tokens live in
;; yamlscript.re, which composes over these.

(ns ys.v0.re
  (:require
   [clojure.string :as str]))

(defn re
  "Expand regex template variables. A greedy scan makes each match the
  full variable name, so no lookahead is needed (Go RE2 engines have
  none)."
  [rgx]
  (loop [rgx (str rgx)]
    (if (re-find #"\$[a-zA-Z]" rgx)
      (recur
        (str/replace rgx #"\$[a-zA-Z]+"
          (fn [match]
            (str (var-get
                   (resolve
                     (symbol (str "ys.v0.re/" (subs match 1)))))))))
      (re-pattern rgx))))

;; Numeric literal tokens

;; Integer literal token
(def inum #"(?:[-+]?(?:0|[1-9][0-9]*))")
;; Big integer literal token
(def ibig (re #"(?:(?:$inum)N)"))
;; Hexadecimal literal token
(def hnum #"(?:[-+]?0x[0-9a-fA-F]+)")
;; Octal literal token
(def onum #"(?:[-+]?0o[0-7]+)")
;; Rational literal token
(def rnum #"(?:[-+]?[0-9]+/[0-9]+)")
;; Radix integer literal token
(def bnum #"(?:[-+]?(?:[2-9]|[12][0-9]|3[0-6])r[0-9a-zA-Z]+)")
;; Floating point literal token
(def fnum (re #"(?:$inum\.[0-9]+(?:[eE]$inum)?)"))
;; Big floating point literal token
(def fbig (re #"(?:(?:$fnum|$inum\.?)M)"))
;; Special number literal token
(def snum #"(?:\\\\(?:Inf|-Inf|NaN))")
;; Numeric literal token
(def xnum (re #"(?:$fbig|$fnum|$hnum|$onum|$rnum|$bnum|$ibig|$inum|$snum)"))

;; Symbol tokens

(def alph #"(?:[a-zA-Z])")                 ; Alpha
(def anum #"(?:[a-zA-Z0-9])")              ; Alphanumeric
(def symw (re #"(?:$alph$anum*(?:-$anum+)*)"))  ; Symbol word
(def keyw (re #"(?:\:$symw)"))             ; Keyword token

(comment
  )


;; The ys.v0.debug library defines a set of Clojure debugging functions.

(ns ys.v0.debug
  (:require
   [clojure.string :as str]
   [ys.v0.global :as global]
   [ys.v0.util :as util])
  (:refer-clojure :exclude [YSC DBG PPP WWW XXX YYY ZZZ]))

(def width 50)

(defn YSC0
  "Compile YAMLScript source for interactive debugging."
  [ys-str]
  (let [compile (some-> (resolve 'yamlscript.compiler/compile) var-get)
        pretty-format (some-> (resolve 'yamlscript.compiler/pretty-format)
                        var-get)
        _ (when-not (and compile pretty-format)
            (util/die "YSC requires the ys runtime (no YS compiler loaded)"))]
    (binding [*ns* (find-ns 'yamlscript.compiler)]
      (let [code (eval
                   (->
                     ys-str
                     compile
                     pretty-format
                     (str/replace #"(?m)^\(\+\+\+ +(.*)\)$" "$1")
                     (str/replace #"(?s)^\(\+\+\+[ \n]+(.*)\)$" "$1")
                     (str/trim-newline)))]
        code))))

(defn YSC
  "Compile YAMLScript source for interactive debugging."
  [ys-str]
  (let [code (if (re-find #"^\n*!(?:yamlscript|YS)" ys-str)
               ys-str
               (str "!ys-0\n" ys-str))]
    (YSC0 code)))

(defn fmt
  "Format a debug value with readable width-limited output."
  [value]
  (cond
    (and (string? value)
      (re-find #"\n." value) (> (count (pr-str value)) width))
    (str "\"\"\"\\\n" value "\"\"\"")
    :else (if (> (count (pr-str value)) 80)
            (str/trim-newline
              (with-out-str
                (util/pprint* (condf value
                                map? (into (sorted-map) value)
                                set? (apply sorted-set value)
                                value))))
            (pr-str value))))

(defn- dump
  "Print a group of debug values and return the last one."
  [values]
  (let [parts (map fmt values)
        text (str/join "\n" parts)]
    (if (re-find #"\n" text)
      (str ">>>\n" text "\n<<<\n")
      (str ">>>" text "<<<\n"))))

(defn PPP
  "Print debugging values and return the final value."
  [& values]
  (print (dump values))
  (flush)
  (last values))

(defn DBG
  "Print debugging values and return the final value."
  [& values]
  (binding [*out* *err*]
    (print (dump values))
    (flush))
  (last values))

(defn WWW
  "Print debugging values and return the final value."
  [& values]
  (apply DBG values))

(defn XXX
  "Print debugging values and return the final value."
  [& values]
  (apply DBG values)
  (util/die ""))

(defn YYY
  "Print debugging values and return the final value."
  [& values]
  (print (dump values))
  (flush)
  (last values))

; TODO Turn on stack trace printing
(defn ZZZ
  "Print debugging values and return the final value."
  [& values]
  (apply DBG values)
  (swap! global/opts assoc :stack-trace true)
  (util/die ""))

(def ttt-ctr (atom 0))

(defn remove-ttt
  "Remove nested TTT tracing forms from printed source."
  [s]
  (let [tokens (re-seq #"(?:\".*?\"|\(TTT |[()]|[^()\"]+)" s)
        list
        (loop [tokens tokens list [] level 0 levels '()]
          (let [[token & tokens] tokens]
            (condp = token
              nil list
              "(TTT "
              (recur tokens list (inc level) (cons (inc level) levels))
              "("
              (recur tokens (conj list token) (inc level) levels)
              ")"
              (recur
                tokens (if (= (first levels) level)
                         list (conj list token))
                (dec level) (if (= (first levels) level)
                              (rest levels) levels))
              (recur tokens (conj list token) level levels))))]
    (str/join "" (remove nil? list))))

(defn ttt-fmt
  "Format a traced form and its result for TTT output."
  ([xs width]
   (str/join ", "
     (for [x xs]
       (cond
         (util/macro? x) (str "'" x)
         (fn? x) (str x)
         ,
         (string? x)
         (let [s (pr-str x)
               s (str/replace s "\n" "\\n")]
           (if (> (count x) width)
             (str (subs s 0 width) "...(" (count x) " chars)")
             s))
         ,
         (or (vector? x) (map? x) (set? x))
         (let [s (pr-str x)]
           (if (> (count s) width)
             (str (subs s 0 width) "...(" (util/type-name x) " "
               (count (take 999 x)) " items)")
             s))
         ,
         (coll? x)
         (let [s (str (pr-str (take 30 x)))]
           (if (> (count s) width)
             (str (subs s 0 width) "...(" (util/type-name x) " "
               (count (take 999 x)) " items)")
             s))
         ,
         (nil? x) "nil"
         (char? x) (pr-str x)
         (symbol? x) (str "'" x)
         :else (str x)))))
  ([s width _]
   (let [width 100
         t (ttt-fmt s (* 2 width))
         t (remove-ttt t)]
     (if (> (count t) width)
       (str (subs t 0 width) "...(" (count s) " chars)")
       t))))

(def ys-macros
  '(+++
     &&&
     |||
     and?
     call
     +def
     each
     or?
     q ql qm qo qv qw
     source
     use
     value
     when+
     F?
     T?
     TTT
     clojure.core/DBG))

(def clj-specials
  '(def do if let quote recur try))

(def skip-trace (concat ys-macros clj-specials))

(defmacro TTT
  "Trace a form at runtime while preserving its value."
  [form]
  (let [[fun & args] form
        name (when (and
                     (not (util/macro? fun))
                     (not (some #{fun} skip-trace)))
               (-> fun
                 str
                 (str/replace #"@.*" "")))
        fname (when-lets [fname (str fun)
                          _ (not= fname "TTT")]
                fname)
        fargs (ttt-fmt args width true)]
    (if name
      `((fn [& xs#]
          (swap! ttt-ctr inc)
          (util/eprintln (str "+" @ttt-ctr " >>> " ~name
                      "(" (ttt-fmt xs# width) ")"))
          (apply ~fun xs#))
        ~@args)
      `(do
         (swap! ttt-ctr inc)
         (when ~fname
           (util/eprintln (str "+" @ttt-ctr " >>> " ~fname
                       "(" ~fargs ")")))
         (~@form)))))

(comment
  (macroexpand '(TTT (q USER)))
  (ns-resolve 'clojure.core 'let)
  )


;; The ys.v0.common namespace holds small helpers shared by the compiler,
;; runtime, and module loader. The :glj reader conditional branches keep
;; it loadable on glojure, which has no java.io or ProcessHandle interop.

(ns ys.v0.common
  (:require
   #?@(:glj [] :default [[clojure.java.io :as io]])
   [clojure.string :as str]
   [ys.v0.debug]
   [ys.v0.util :as util]))

(defn cwd
  "Return the current working directory."
  []
  (System/getProperty "user.dir"))

(defn abspath
  "Return an absolute path, resolving relative paths from base."
  ([path] (abspath path (cwd)))
  ([path base]
   #?(:glj
      (let [path (if (path:filepath.IsAbs path)
                   path
                   (path:filepath.Join base path))
            [path error] (path:filepath.Abs path)]
        (if error (throw error) path))
      :default
      (if (-> path io/file .isAbsolute)
        path
        (.getAbsolutePath (io/file (abspath base) path))))))

(def ^:private atom-type (type (atom nil)))

(defn atom?
  "Return true when x is a Clojure atom."
  [x]
  (= (type x) atom-type))

(defn chop
  "Drop N trailing items from a string or sequence."
  ([S] (chop 1 S))
  ([N S]
   (let [lst (drop-last N S)]
     (if (string? S)
       (str/join "" lst)
       lst))))

(defn dirname
  "Return the parent directory of a path, or . for no parent."
  [path]
  #?(:glj
     (path:filepath.Dir path)
     :default
     (->
       path
       io/file
       .getParent
       (or "."))))

#?(:glj
   (do
     (defn get-process-handle [] nil)
     (defn get-process-info [] nil)
     (defn get-cmd-path [] nil)
     (defn get-cmd-bin [] nil)
     (defn get-cmd-args [] [])
     (defn get-cmd-pid [] nil))
   :default
   (do
     (defn get-process-handle
       "Return process handle for the current context."
       []
       (java.lang.ProcessHandle/current))

     (defn get-process-info
       "Return process info for the current context."
       []
       (-> ^java.lang.ProcessHandle (get-process-handle) .info))

     (defn get-cmd-path
       "Return cmd path for the current context."
       []
       (-> ^java.lang.ProcessHandle$Info (get-process-info) .command .get))

     (defn get-cmd-bin
       "Return cmd bin for the current context."
       []
       (-> ^String (get-cmd-path) io/file .getParent))

     (defn get-cmd-args
       "Return cmd args for the current context."
       []
       (-> ^java.lang.ProcessHandle$Info
           (get-process-info)
           .arguments
           (.orElse (into-array String []))))

     (defn get-cmd-pid
       "Return cmd pid for the current context."
       []
       (-> ^java.lang.ProcessHandle (get-process-handle) .pid))))

(defn- split-yspath [yspath separator]
  (str/split yspath (if (= separator ";") #";" #":")))

(defn get-yspath
  "Return yspath for the current context."
  [base]
  (let [separator #?(:glj (if (= runtime.GOOS "windows") ";" ":")
                     :default java.io.File/pathSeparator)
        yspath (or
                 (get (System/getenv) "YSPATH")
                 (when (re-matches #".*[\\/]NO-NAME$" base) (cwd))
                 (->
                   base
                   dirname
                   abspath))
        _ (when-not yspath
            (util/die "YSPATH environment variable not set"))]
    (split-yspath yspath separator)))

(defn re-find+
  "Reprocess find+ for YAMLScript parsing."
  [R S]
  (re-find R (str S)))

(def ^:private regex-type (type #""))

(defn regex?
  "Return true when x is a regular expression pattern."
  [x]
  (= (type x) regex-type))

(intern 'clojure.core (with-meta 'TTT {:macro true}) @#'ys.v0.debug/TTT)
(intern 'clojure.core 'YSC ys.v0.debug/YSC)
(intern 'clojure.core 'YSC0 ys.v0.debug/YSC0)
(intern 'clojure.core 'DBG ys.v0.debug/DBG)
(intern 'clojure.core 'PPP ys.v0.debug/PPP)
(intern 'clojure.core 'WWW ys.v0.debug/WWW)
(intern 'clojure.core 'XXX ys.v0.debug/XXX)
(intern 'clojure.core 'YYY ys.v0.debug/YYY)
(intern 'clojure.core 'ZZZ ys.v0.debug/ZZZ)

(comment
  )


;; The clojure.data.csv backend resolves at call time so that Clojure
;; runtimes without it can still load this namespace. The writer interop
;; lives in a private helper so its analysis is also deferred on
;; runtimes that analyze function bodies lazily.

(ns ys.v0.csv
  (:require
   [clojure.string :as str]
   [ys.v0.util :as util])
  (:refer-clojure :exclude [read]))

(defn read-csv [s]
  ((util/backend 'clojure.data.csv/read-csv)
   (str/trim-newline s) :separator \,))

(defn read-tsv [s]
  ((util/backend 'clojure.data.csv/read-csv)
   (str/trim-newline s) :separator \tab))

(defn- write-str [write-fn data sep]
  (with-open [s (java.io.StringWriter.)]
    (write-fn s data :separator sep)
    (str s)))

(defn write-csv [data]
  (write-str (util/backend 'clojure.data.csv/write-csv) data \,))

(defn write-tsv [data]
  (write-str (util/backend 'clojure.data.csv/write-csv) data \tab))

(defn read [s]
  (read-csv s))

(defn write [data]
  (write-csv data))

(comment
  )


(ns ys.v0.dwim
  (:require
   [clojure.string :refer [escape replace]]
   [ys.v0.common :as common :refer [chop re-find+ regex?]])
  (:refer-clojure :exclude [replace]))


;;------------------------------------------------------------------------------
(defn- regex-to-fn [a b f]
  (let [a (if (regex? a) #(re-find+ a %1) a)]
    (f a b)))


;;------------------------------------------------------------------------------
(defn ++filter [a b] (regex-to-fn a b filter))

(defn ++filterv [a b] (regex-to-fn a b filterv))

(defn ++keep [a b] (regex-to-fn a b keep))

(defn ++map [a b]
  (if (and (ifn? b) (or (sequential? a) (string? a)))
    (map b a)
    (map a b)))

(defn ++mapv [a b]
  (if (and (ifn? b) (or (sequential? a) (string? a)))
    (mapv b a)
    (mapv a b)))

(defn ++remove [a b] (regex-to-fn a b remove))

(defn ++replace
  ([a b] (clojure.string/replace a b ""))
  ([a b c] (clojure.string/replace a b c)))

(defn ++take-while [a b] (regex-to-fn a b take-while))


;;------------------------------------------------------------------------------
(defmacro ^:private dwim [type idfn name]
  (let [dname (symbol (str "+" name))
        pname (symbol (str "++" name))
        name (if (resolve pname) pname name)]
    (condp = type
      2.1 `(defn ~dname [x# y#]
             (if (~idfn y#)
               (~name y# x#)
               (~name x# y#)))
      2.2 `(defn ~dname [x# y#]
             (if (~idfn x#)
               (~name y# x#)
               (~name x# y#)))
      23.2 `(defn ~dname
              ([x# y#]
               (if (~idfn x#)
                 (~name y# x#)
                 (~name x# y#)))
              ([x# y# z#]
               (if (~idfn x#)
                 (~name y# x# z#)
                 (~name x# y# z#))))
      9.1 `(defn ~dname [x# y# & xs#]
             (if (~idfn y#)
               (apply ~name y# x# xs#)
               (apply ~name x# y# xs#)))
      9.9 `(defn ~dname [x# & xs#]
             (if (~idfn x#)
               (apply ~name (concat xs# [x#]))
               (apply ~name x# xs#)))
      (die "Bad dwim  type: " type))))


;;------------------------------------------------------------------------------
(dwim 2.2 seqable? apply)
(dwim 2.2 seqable? chop)
(dwim 2.2 seqable? cons)
(dwim 2.1 seqable? contains?)
(dwim 2.2 seqable? drop)
(dwim 2.2 seqable? drop-last)
(dwim 2.2 seqable? drop-while)
(dwim 2.2 map?     escape)
(dwim 2.2 seqable? every?)
(dwim 2.2 seqable? filter)
(dwim 2.2 seqable? filterv)
(dwim 9.1 string?  format)
(dwim 2.2 seqable? interpose)
(dwim 2.2 seqable? keep)
(dwim 2.2 seqable? map)
(dwim 2.2 seqable? mapcat)
(dwim 2.2 seqable? mapv)
(dwim 2.2 seqable? not-any?)
(dwim 2.1 seqable? nth)
(dwim 9.9 seqable? partition)
(dwim 2.2 seqable? random-sample)
(dwim 2.1 regex?   re-find)
(dwim 2.1 regex?   re-matches)
(dwim 2.1 regex?   re-seq)
(dwim 9.9 seqable? reduce)
(dwim 2.2 seqable? remove)
(dwim 2.2 seqable? repeat)
(dwim 23.2 regex?  replace)
(dwim 2.2 seqable? some)
(dwim 2.2 seqable? sort)
(dwim 2.2 seqable? sort-by)
(dwim 2.2 seqable? split-at)
(dwim 2.2 seqable? split-with)
(dwim 2.2 seqable? take)
(dwim 2.2 seqable? take-last)
(dwim 2.2 seqable? take-while)

(comment
  )


;; The clj-yaml (SnakeYAML) backend resolves at call time so that Clojure
;; runtimes without it (like jolt) can still load this namespace.

(ns ys.v0.yaml
  (:require
   [clojure.string :as str]
   [ys.v0.util :as util])
  (:refer-clojure :exclude [load]))

;; Alternate clj-yaml.core implementations (like jolt's libyaml based
;; one) may not support the option arguments; retry with just the data
;; (a genuine data error throws the same way on the retry).
(defn- parse-string [str & opts]
  (let [f (util/backend 'clj-yaml.core/parse-string)]
    (util/catching
      (apply f str opts)
      (f str))))

(defn- generate-string [data & opts]
  (let [f (util/backend 'clj-yaml.core/generate-string)]
    (util/catching
      (apply f data opts)
      (f data))))

;; Skip leading comment and blank lines before a --- document marker.
;; Spelled without (?x) so it also compiles on Go RE2 engines.
(def prefix-re
  #"^(?:(?:[ \t]*#.*\n)|(?:\s*\n))*---\s+")

(defn load [str]
  (parse-string str
    :code-point-limit (* 10 1024 1024)
    :keywords false))

(defn load-all [str]
  (let [str (str/replace str prefix-re "")
        documents (str/split str #"(?m)^---\s+")]
    (reduce
      (fn [data doc]
        (conj data (load doc)))
      []
      documents)))

(defn dump [data]
  (generate-string
    data
    :dumper-options
    {:flow-style :block}))

(defn dump-all [data]
  (str/join "\n"
    (reduce
      (fn [strings node]
        (conj strings
          (let [yaml (str/trimr
                       (generate-string node
                         :dumper-options
                         {:flow-style :block}))]
            (if (> (count data) 1)
              (str "---\n" yaml)
              yaml))))
      [] data)))

(comment
  )


(ns ys.v0.ext
  (:require
   [clojure.string :as str]
   [ys.v0.util :as util]
   [ys.v0.yaml :as yaml]))

(defn yq [data cmd]
  (let [yaml (yaml/dump data)
        res ((util/backend 'babashka.process/sh) {:in yaml}
             "yq" "-e" cmd)
        {:keys [exit out err]} res]
    (when (and
            (not= 0 exit)
            (not= err "Error: no matches found\n"))
      (util/die "yq error: " (:err res)))

    (let [data (if (str/blank? out)
                 nil
                 (yaml/load-all out))]
      (if (= 1 (count data))
        (first data)
        data))))

(defn github-raw-url
  "Convert a raw url shorthand into a raw URL."
  [url]
  (let [[path ref] (str/split url #"\@")
        ref (or ref "HEAD")
        [user repo path] (str/split path #"/" 3)
        _ (when-not (and user repo path)
            (util/die (str "Invalid github url: " url)))]
    (str/join "/" ["https://raw.githubusercontent.com"
                   user repo ref path])))

(defn github-gist-url
  "Convert a gist url shorthand into a raw URL."
  [url]
  (let [url (str/replace url #"/raw/?" "/")
        [path ref] (str/split url #"\@")
        [user gist-id path] (str/split path #"/" 3)
        _ (when-not (and user gist-id)
            (util/die (str "Invalid github gist url: " url)))]
    (str/join "/" (remove nil? ["https://gist.githubusercontent.com"
                                user gist-id "raw" ref path]))))

(defn convert-url
  "Convert url into its canonical form."
  [url]
  (cond
    (re-find #"^https?://" url) url
    (str/starts-with? url "gist:") (github-gist-url (subs url 5))
    (str/starts-with? url "github:") (github-raw-url (subs url 7))
    (str/starts-with? url "https:") url
    (str/starts-with? url "http:") url
    (not (str/includes? url ":")) (str "https://" url)
    :else (util/die (str "Invalid url for ':url': " url))))

(comment
  )


;; The babashka.http-client backend resolves at call time so that Clojure
;; runtimes without it (like jolt) can still load this namespace.

(ns ys.v0.http
  (:require
   [ys.v0.ext :as ext]
   [ys.v0.util :as util])
  (:refer-clojure :exclude [get]))

(defn- backend [name]
  (util/backend (symbol "babashka.http-client" name)))

(defn delete [& args] (apply (backend "delete") args))
(defn get [& args] (apply (backend "get") args))
(defn head [& args] (apply (backend "head") args))
(defn patch [& args] (apply (backend "patch") args))
(defn post [& args] (apply (backend "post") args))
(defn put [& args] (apply (backend "put") args))

(defn curl [url]
  (let [url (ext/convert-url url)
        resp (get url)]
    (if-let [body (:body resp)]
      (str body)
      (util/die resp))))

(comment
  )


;; The clojure.data.json backend resolves at call time so that Clojure
;; runtimes that can't load it (like jolt, currently) can still load
;; this namespace.

(ns ys.v0.json
  (:require
   [ys.v0.util :as util])
  (:refer-clojure :exclude [load]))

(defn load [str]
  ((util/backend 'clojure.data.json/read-str) str))

(defn dump [data]
  ((util/backend 'clojure.data.json/write-str) data))

(defn pretty [data]
  (with-out-str
    ((util/backend 'clojure.data.json/pprint) data)))

(comment
  )


;; The ys.v0.manifest namespace is the single source of truth for what the
;; YS runtime injects into user code. It is consumed by ys.v0/init (for
;; babashka and JVM Clojure) and by yamlscript.runtime (for the SCI context
;; inside the ys binary). It is pure data; no code.

(ns ys.v0.manifest)

;; Namespaces whose vars are referred into the user's namespace.
;; :all means every public var. Order matters: later wins on conflict.
(def refers
  '[[ys.v0.std :all]
    [ys.v0.dwim :all]
    [ys.v0.util [condf]]
    [ys.v0.debug [DBG PPP TTT WWW XXX YYY ZZZ]]])

;; Runtime variables. ys.v0/init refers these from ys.v0.global and binds
;; them; the ys runtime provides SCI dynamic vars of the same names.
(def runtime-vars
  '[_ ARGS ARGV CWD DIR ENV FILE INC PUN RUN VERSION])

;; clojure.core functions that SCI does not provide but babashka and JVM
;; Clojure do. The ys runtime adds these to its clojure.core; ys.v0/init
;; leaves them alone.
(def sci-core-extras
  '[abs infinite? parse-double parse-long parse-uuid random-uuid NaN?])

;; Host clojure.core functions that are not part of the YS core surface.
(def hidden-core
  '[compile file-seq flush line-seq load-file load-reader newline
    pr prn printf println read-line slurp spit])

;; clojure.core functions overridden by the ys runtime with YS behavior.
(def runtime-overrides
  '[load use])

;; Namespaces that some Clojure runtimes lack (glojure bundles neither
;; the clojure.* ones nor the babashka.fs backend that ys.v0.fs and
;; ys.v0.taptest pull in). ys.v0 requires them via a guarded loop and
;; only refers exports from the ones that loaded.
(def optional-nses
  '[clojure.java.io
    clojure.math
    clojure.pprint
    clojure.set
    clojure.tools.cli
    ys.v0.fs
    ys.v0.taptest])

;; Public modules provided by the YS runtime. They are loaded only by an
;; explicit use. The values are their internal host namespaces.
(def modules
  '{ys.std     ys.v0.std
    ys.clj     ys.v0.clj
    ys.ys      ys.v0.ys
    ys.cli     clojure.tools.cli
    ys.csv     ys.v0.csv
    ys.ext     ys.v0.ext
    ys.fs      ys.v0.fs
    ys.http    ys.v0.http
    ys.io      ys.v0.io
    ys.ipc     ys.v0.ipc
    ys.json    ys.v0.json
    ys.math    clojure.math
    ys.pods    ys.v0.pods
    ys.pprint  ys.v0.pprint
    ys.set     clojure.set
    ys.str     clojure.string
    ys.walk    clojure.walk
    ys.yaml    ys.v0.yaml
    ys.taptest ys.v0.taptest})

(defn exported-syms
  "Return the set of symbols that init will refer into the user
  namespace. Namespaces this runtime could not load contribute
  nothing."
  []
  (into #{}
    (mapcat (fn [[ns-sym syms]]
              (when (find-ns ns-sym)
                (if (= :all syms)
                  (keys (ns-publics ns-sym))
                  syms))))
    refers))

(comment
  )


(ns ys.v0.imports
  (:require [clojure.string :as str]
            [ys.v0.util :as util]))

(defn short-module? [module]
  "Return true for an unqualified, undotted module symbol."
  (and (symbol? module)
    (nil? (namespace module))
    (not (str/includes? (str module) "."))))

(defn with-short-from-alias [module options]
  "Use a short :from module name as its default alias."
  (if (and (short-module? module)
        (= :from (first (:source options)))
        (nil? (:as options)))
    (assoc options :as module)
    options))

(defn v0-imports
  "Alias available, permitted standard modules without replacing aliases."
  [available aliases allowed]
  (for [module (sort available)
          :let [alias (symbol (subs (str module) 3))]
          :when (and (contains? available module)
                  (or (nil? allowed) (contains? allowed module))
                  (not (contains? aliases alias)))]
      (with-meta (list module :as alias) {:umbrella true})))

(defn expression-imports?
  "Let leading namespace and import declarations establish their aliases."
  [form]
  (if (and (seq? form) (contains? '#{+++ TTT} (first form)))
    (expression-imports? (second form))
    (not (and (seq? form) (contains? '#{ns use} (first form))))))

(defn normalize-use-forms
  ([forms]
   (normalize-use-forms
     forms @(util/backend 'ys.v0.manifest/modules)))
  ([forms public-modules]
   (let [forms (if (every? symbol? forms)
                 (map list forms)
                 (if (symbol? (first forms)) (list forms) forms))]
     (map
       (fn [form]
         (let [[module & args] form]
           (cond
             (contains? '#{v0 ys.v0} module)
             (do
               (when (seq args)
                 (util/die
                   "The v0 umbrella import does not accept modifiers"))
               (list 'ys.v0))
             (short-module? module)
             (let [public-module (symbol (str "ys." module))]
               (if (contains? public-modules public-module)
                 (if (seq args)
                   (cons public-module args)
                   (list public-module :as module))
                 form))
             :else form)))
       forms))))


;; The ys.v0.ys namespace is the standard library's bridge to the YS
;; compiler and runtime. Under the ys runtime the real implementations are
;; installed into the hooks below. Under plain Clojure runtimes (babashka,
;; JVM Clojure) the compiler-dependent functions die with a clear message,
;; while the portable ones use plain Clojure implementations.

(ns ys.v0.ys
  (:require
   [clojure.string :as str]
   [ys.v0.common :refer [get-yspath]]
   [ys.v0.global :as global]
   [ys.v0.manifest :as manifest]
   [ys.v0.imports :as imports]
   [ys.v0.re :as re]
   [ys.v0.util :as util])
  (:refer-clojure
   :exclude [compile
             eval
             load-file
             use]))

(def hooks
  "Implementation hooks installed by the ys runtime."
  (atom {}))

(defn configured-modules []
  (when-let [value (System/getenv "YS_MODULES")]
    (into #{}
      (map #(if (str/starts-with? %1 "ys.")
              (symbol %1)
              (symbol (str "ys." %1))))
      (remove str/blank? (str/split value #"[,\s]+")))))

(defn check-module-access! [module]
  (when-let [allowed (configured-modules)]
    (when-not (allowed module)
      (util/die (str module " is disabled by YS_MODULES")))))

(defn- hook [key & args]
  (if-let [f (get @hooks key)]
    (apply f args)
    (util/die
      (str "ys/" (name key) " requires the ys runtime "
        "(not available in this Clojure runtime)"))))

(defn compile [code]
  (hook :compile code))

(defn eval
  ([ys-code] (hook :eval ys-code))
  ([ys-code file stream-mode] (hook :eval ys-code file stream-mode)))

(defn eval-stream [ys-code]
  (hook :eval-stream ys-code))

(defn load-file [ys-file]
  (check-module-access! 'ys.fs)
  (hook :load-file ys-file))

(defn load-url [url]
  (check-module-access! 'ys.http)
  (hook :load-url url))

(defn- default-load-pod [args]
  (let [load-pod (requiring-resolve 'babashka.pods/load-pod)]
    (apply load-pod args)))

(defn load-pod [& args]
  (check-module-access! 'ys.pods)
  (if-let [f (get @hooks :load-pod)]
    (f args)
    (default-load-pod args)))

(defn unload-pods []
  (check-module-access! 'ys.pods)
  (when-let [f (get @hooks :unload-pods)]
    (f)))

(defn- default-def-vars [ns m force]
  (let [ns (condf ns
             #(instance? clojure.lang.Namespace %1) ns
             string? (create-ns (symbol ns))
             symbol? (create-ns ns)
             (util/die (str "Invalid namespace for set-vars: '" ns "'")))]
    (reduce-kv
      (fn [_ k v]
        (let [key (condf k
                    string? k
                    keyword? (name k)
                    symbol? (name k)
                    (util/die (str "Invalid key for set-vars: '" k "'")))
              key (str/replace key #"_" "-")
              _ (when-not (re-matches re/symw key)
                  (util/die (str "Invalid key for set-vars: '" key "'")))
              key (symbol key)]
          (when (and (not force)
                  (ns-resolve ns key))
            (util/die
              (str "Variable '" key
                "' already defined in namespace '" (ns-name ns) "'")))
          (intern ns key v)))
      nil m)
    nil))

(defn +def-vars
  ([ns m]
   (+def-vars ns m false))
  ([ns m force]
   (let [m (dissoc m "_" '_)]
     (if-let [f (get @hooks :+def-vars)]
       (f ns m force)
       (default-def-vars ns m force)))))

(defmacro def-vars-from-map [ns m]
  `(let [[m# ns#] (if (~m "+")
                    [(dissoc ~m "+") (symbol (~m "+"))]
                    [~m ~ns])]
     (+def-vars ns# m#)))

(def source-options #{:path :file :url :from})
(def selection-options #{:as :get :all :none :not})
(def use-options (into source-options selection-options))

(defn- use-option-value [option args pred description]
  (let [value (second args)]
    (when-not (and value (pred value))
      (util/die (str "Invalid 'use' option '" option
                  "': expected " description)))
    value))

(defn- use-option-symbols [option args]
  (let [[symbols more] (split-with (complement keyword?) (rest args))]
    (when-not (and (seq symbols) (every? symbol? symbols))
      (util/die (str "Invalid 'use' option '" option
                  "': expected at least one symbol")))
    [symbols more]))

(defn- validate-use-options [options]
  (when (and (:none options) (some options [:get :all :not]))
    (util/die "Invalid 'use' options: ':none' cannot be combined with "
      "':get', ':all', or ':not'"))
  (when (and (:get options) (:all options))
    (util/die "Invalid 'use' options: ':get' cannot be combined with ':all'"))
  (when (and (:get options) (:not options))
    (util/die "Invalid 'use' options: ':get' cannot be combined with ':not'"))
  options)

(defn- parse-use-args [args]
  (loop [args (seq args) options {}]
    (if-not args
      (validate-use-options options)
      (let [option (first args)]
        (when-not (and (keyword? option) (use-options option))
          (util/die (str "Invalid 'use' option '" option "'")))
        (when (contains? options option)
          (util/die (str "Duplicate 'use' option '" option "'")))
        (cond
          (source-options option)
          (let [value (use-option-value option args string? "one string")]
            (when-let [[source] (:source options)]
              (util/die (str "Invalid 'use' option '" option
                          "': source option '" source "' is already set")))
            (recur (nnext args) (assoc options :source [option value])))

          (= option :as)
          (let [alias (use-option-value
                        option args
                        #(and (symbol? %1) (nil? (namespace %1)))
                        "one symbol")]
            (recur (nnext args) (assoc options option alias)))

          (some #{option} [:get :not])
          (let [[symbols more] (use-option-symbols option args)]
            (when (and (= option :not) (some namespace symbols))
              (util/die
                "Invalid 'use' option ':not': expected plain symbols"))
            (recur (seq more) (assoc options option (vec symbols))))

          :else
          (recur (next args) (assoc options option true)))))))

(defn- resolve-bb-add-classpath []
  (try
    (clojure.core/require 'babashka.classpath)
    (resolve 'babashka.classpath/add-classpath)
    (catch Throwable _ nil)))

(defn- add-jvm-load-path [path]
  (try
    (clojure.core/eval
      (read-string
        (str
          "(let [thread (Thread/currentThread) "
          "loader (clojure.lang.DynamicClassLoader. "
          "(.getContextClassLoader thread)) "
          "file (java.io.File. " (pr-str path) ")] "
          "(.addURL loader (.toURL (.toURI file))) "
          "(.setContextClassLoader thread loader))")))
    true
    (catch Throwable _ false)))

(defn- add-load-paths [paths]
  (let [add-load-path (resolve 'add-load-path)
        add-classpath (when-not add-load-path
                        (resolve-bb-add-classpath))]
    (doseq [path paths]
      (cond
        add-load-path (add-load-path path)
        add-classpath (add-classpath path)
        (add-jvm-load-path path) nil
        :else
        (util/die "This Clojure runtime cannot load modules from paths")))))

(defn- resolve-require-deps []
  (try
    (clojure.core/require 'clojurestar.deps)
    (resolve 'clojurestar.deps/require-deps*)
    (catch Throwable error
      (if (some resolve
            '[*glojure-version* *jolt-version* *gobb-version*])
        (throw error)
        nil))))

(defn- load-required-namespace [require-deps options coordinate]
  (let [loader-name (gensym "ys-use-loader-")
        loader-ns (create-ns loader-name)
        loader-alias (gensym "module-")]
    (try
      (binding [*ns* loader-ns]
        (require-deps options [coordinate :as loader-alias]))
      (if-let [loaded-ns (get (ns-aliases loader-ns) loader-alias)]
        (ns-name loaded-ns)
        (util/die
          (str "Unable to determine dependency namespace for '"
            coordinate "'")))
      (finally
        (remove-ns loader-name)))))

(defn- coordinate-namespace [module coordinate]
  (if-let [[_ namespace-name]
           (re-matches #"mvn:[^/@]+/[^/@]+@[^/]+/([^/]+)" coordinate)]
    (symbol namespace-name)
    module))

(defn- load-classpath-dependency [module coordinate]
  (let [module (coordinate-namespace module coordinate)]
    (try
      (clojure.core/require module)
      module
      (catch Throwable _
        (util/die
          (str "Portable 'use :from' cannot acquire dependencies in this "
            "runtime; put namespace '" module "' on the classpath"))))))

(defn- load-portable-module [target module options]
  (check-module-access! module)
  (let [[kind spec] (or (:source options) [:yspath (get-yspath *file*)])]
    (if-let [host-namespace
             (and (= kind :yspath) (manifest/modules module))]
      (do
        (binding [*ns* target]
          (clojure.core/require host-namespace)
          (when-not (and (:preserve-aliases options)
                      (contains? (ns-aliases target) module))
            (clojure.core/alias module host-namespace)))
        host-namespace)
      (let [loaded-module
            (case kind
              :yspath
              (do
                (add-load-paths spec)
                (clojure.core/require module)
                module)

              :path
              (do
                (add-load-paths [spec])
                (clojure.core/require module)
                module)

              :file
              (do
                (when (str/ends-with? spec ".ys")
                  (util/die
                    "Portable 'use :file' does not support .ys files"))
                (when-not (or (str/ends-with? spec ".clj")
                            (str/ends-with? spec ".cljc"))
                  (util/die
                    (str "Invalid 'use' option ':file': expected a .clj or "
                      ".cljc file")))
                (clojure.core/load-file spec)
                module)

              :url
              (do
                (when-not (str/starts-with? spec "https://")
                  (util/die
                    "Invalid 'use' option ':url': expected an HTTPS URL"))
                (load-string (slurp spec))
                module)

              :from
              (let [dependency-options
                    (cond-> {}
                      (get global/ENV "YS_MAVEN_REPOSITORY")
                      (assoc :mvn/local-repo
                        (get global/ENV "YS_MAVEN_REPOSITORY"))

                      (get global/ENV "YS_GITLIBS_DIR")
                      (assoc :gitlibs/dir
                        (get global/ENV "YS_GITLIBS_DIR")))]
                (if-let [require-deps (resolve-require-deps)]
                  (load-required-namespace
                    require-deps dependency-options spec)
                  (load-classpath-dependency module spec))))
            module
            (cond
              (not= kind :from) module
              (imports/short-module? module) loaded-module
              (= loaded-module module) module
              :else
              (util/die
                (str "Dependency namespace '" loaded-module
                  "' does not match use module '" module "'")))]
        (when-not (find-ns module)
          (util/die (str "Namespace not found: " module)))
        module))))

(defn- select-portable-vars [module options]
  (when-let [alias (:as options)]
    (clojure.core/alias alias module))
  (when-let [symbols (:get options)]
    (let [only (mapv #(if (namespace %1)
                        (symbol (namespace %1))
                        %1)
                 symbols)
          rename (into {}
                   (keep #(when-let [old (namespace %1)]
                            [(symbol old) (symbol (name %1))]))
                   symbols)]
      (refer module :only only :rename rename)))
  (when (or (:all options) (:not options))
    (refer module :exclude (vec (:not options)))))

(defn- selects-vars? [options]
  (some options [:get :all :none :not]))

(defn- clear-portable-std [target]
  (doseq [[sym var] (ns-refers target)
          :when (= 'ys.v0.std (some-> var meta :ns ns-name))]
    (ns-unmap target sym)))

(defn normalize-use-forms [forms]
  (imports/normalize-use-forms forms manifest/modules))

(defn- portable-use [ns forms]
  (when-not (seq forms)
    (util/die "use requires at least one form"))
  (doseq [form forms]
    (if (= 'ys.v0 (first form))
      (when-let [imports (seq (imports/v0-imports
          (into #{} (filter #(find-ns (manifest/modules %))
                      (keys manifest/modules)))
          (ns-aliases ns) (configured-modules)))]
        (portable-use ns imports))
      (let [module (first form)
            options (assoc
                      (imports/with-short-from-alias
                        module (parse-use-args (rest form)))
                      :preserve-aliases (:umbrella (meta form)))
            loaded-module (load-portable-module ns module options)]
        (binding [*ns* ns]
          (when (and (= module 'ys.std) (selects-vars? options))
            (clear-portable-std ns))
          (select-portable-vars loaded-module options)))))
  nil)

(defn +use [ns forms]
  (let [forms (normalize-use-forms forms)]
    (if-let [f (get @hooks :+use)]
      (f ns forms)
      (portable-use ns forms))))

(defmacro use [& forms]
  `(+use *ns* '~forms))

(comment
  )


(ns ys.v0.fs
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:refer-clojure :exclude [abs empty? file-seq find read]))

(def this 'ys.v0.fs)
(def TRUE (constantly true))

(defn- multi [func]
  (fn [& paths]
    (let [sequential (and (= 1 (count paths))
                       (sequential? (first paths)))
          associative (and (= 1 (count paths))
                        (associative? (first paths)))
          path-list (cond
                      sequential (first paths)
                      associative (keys (first paths))
                      :else paths)
          return (for [path path-list] (func path))]
      (cond
        sequential return
        associative (zipmap (keys paths) return)
        (= 1 (count return)) (first return)
        :else return))))

(defn- when-str [x]
  (if x (str x) nil))

(declare
  dir?
  file?
  filename
  ls
  size?
  )

(defn read [path]
  (slurp path))

(defn write [path content]
  (spit path content))

(intern this 'file-seq clojure.core/file-seq)

;; FS boolean functions
(intern this 'abs? (multi fs/absolute?))
(intern this 'dir? (multi fs/directory?))
(intern this 'empty? (multi
                       (fn [path]
                         (condf path
                           file? (-> (not (size? path)))
                           dir? (-> path ls seq not)
                           TRUE
                           (die "empty? called with non-file, non-dir path: '"
                             path "'")))))
(intern this 'exec? fs/executable?)
(intern this 'exists? fs/exists?)
(intern this 'file? fs/regular-file?)
(intern this 'link? fs/sym-link?)
(intern this 'read? fs/readable?)
(intern this 'rel? fs/relative?)
(intern this 'size? (multi (fn [path] (not= 0 (fs/size path)))))
(intern this 'write? fs/writable?)

;; FS boolean short functions
(intern this 'd dir?)
(intern this 'e exists?)
(intern this 'f file?)
(intern this 'l link?)
(intern this 'r read?)
(intern this 's size?)
(intern this 'w write?)
(intern this 'x exec?)
(intern this 'z empty?)

;; FS getters
(intern this 'abs (multi #(-> %1 fs/absolutize str)))
(intern this 'basename (multi #(-> %1 fs/canonicalize fs/file-name str)))
(intern this 'ctime (multi #(fs/file-time->millis (fs/last-modified-time %1))))
(intern this 'cwd #(str (fs/cwd)))
(intern this 'dirname (multi #(-> %1 fs/canonicalize fs/parent str)))
(intern this 'filename
  (fn
    ([path] (fs/file-name path))
    ([path ext]
     (if (= ext "*")
       (str/replace (filename path) #"(\w)\.\w{1,16}$" "$1")
       (fs/strip-ext (filename path) {:ext (str/replace ext #"^\." "")})))))
(intern this 'find
  (fn [path]
    (filter #(re-find #"/" %1)
      (map str
        (file-seq (io/file path))))))
(intern this 'glob (multi #(map str (fs/glob "." %1))))
(intern this 'ls #(map str (fs/list-dir %1)))
(intern this 'mtime (multi #(fs/file-time->millis (fs/last-modified-time %1))))
(intern this 'path (multi #(-> %1 fs/canonicalize)))
(intern this 'readlink (multi #(-> %1 fs/read-link)))
(intern this 'rel fs/relativize)
(intern this 'rel (multi #(str (fs/relativize (fs/cwd) (fs/canonicalize %1)))))
(intern this 'which (multi #(-> %1 fs/which when-str)))

;; FS mutators
(intern this 'cp #(str (fs/copy %1 %2)))
(intern this 'cp-r #(str (fs/copy-tree %1 %2)))
(intern this 'mkdir (multi #(-> %1 fs/canonicalize fs/create-dir str)))
(intern this 'mkdir-p (multi #(-> %1 fs/canonicalize fs/create-dirs str)))
(intern this 'mv #(str (fs/move %1 %2)))
(intern this 'rm (multi #(-> %1 fs/canonicalize fs/delete boolean not)))
(intern this 'rm-f (multi #(-> %1 fs/canonicalize fs/delete-if-exists boolean)))
(intern this 'rm-r (multi #(-> %1 fs/canonicalize fs/delete-tree boolean)))
(intern this 'rmdir (multi #(-> %1 fs/canonicalize fs/delete not)))
(intern this 'touch (multi #(str
                              (if (exists? %1)
                                (fs/set-last-modified-time
                                  %1 (java.time.Instant/now))
                                (fs/create-file %1)))))

(comment
  )


;; Stream I/O functions owned by the ys::io public module.

(ns ys.v0.io
  (:require
   [ys.v0.global :as global])
  (:refer-clojure :exclude [print]))

(defn err [& xs]
  (binding [*out* (global/current-error)]
    (apply clojure.core/print xs)
    (flush)))

(defn out [& xs]
  (binding [*out* (global/current-output)]
    (apply clojure.core/print xs)
    (flush)))

(defn print [& xs]
  (binding [*out* (global/current-output)]
    (apply clojure.core/print xs)
    (flush)))

(defn say [& xs]
  (binding [*out* (global/current-output)]
    (apply println xs)
    (flush)))

(defn warn [& xs]
  (binding [*out* (global/current-error)]
    (apply println xs)
    (flush)))

(defn readline [reader]
  (binding [*in* reader]
    (clojure.core/read-line)))

(comment
  )


;; Process functions owned by the ys::ipc public module.

(ns ys.v0.ipc
  (:require
   [clojure.string :as str]
   [ys.v0.global :as global]
   [ys.v0.util :as util]))

(defn- backend [name]
  (util/backend (symbol "babashka.process" name)))

(defn- process-opts [[opts & xs]]
  (let [opts (if (map? opts)
               [(assoc opts :env (or (:env opts) global/env))]
               [{:env global/env} opts])]
    (vec (concat opts xs))))

(defn exec [& xs]
  (apply (backend "exec") (process-opts xs)))

(defn process [& xs]
  (apply (backend "process") (process-opts xs)))

(defn sh [& xs]
  (apply (backend "sh") (process-opts xs)))

(defn shell [& xs]
  (apply (backend "shell") (process-opts xs)))

(defn sh-out [& xs]
  (let [ret (apply sh xs)]
    (when (not= 0 (:exit ret))
      (util/die (:err ret)))
    (str/trim-newline (:out ret))))

(defn bash [& xs]
  (let [cmd (str/join " " xs)]
    (sh "bash -c" cmd)))

(defn bash-out [& xs]
  (let [cmd (str/join " " xs)]
    (sh-out "bash -c" cmd)))

(comment
  )


(ns ys.v0.pods
  (:require [ys.v0.ys :as ys]))

(defn load-pod [& args]
  (apply ys/load-pod args))

(defn unload-pods []
  (ys/unload-pods))


;; Portable pretty printing for the ys::pprint public module.

(ns ys.v0.pprint
  (:require
   [clojure.string :as str]
   [ys.v0.global :as global]))

(def ^:dynamic *right-margin* 72)
(def ^:dynamic *miser-width* 40)

(def ^:private option-names
  #{:length :level :miser-width :pretty :right-margin :stream
    :suppress-namespaces})

(defn- spaces [n]
  (apply str (repeat n " ")))

(defn- scalar-str [value suppress-namespaces]
  (if (and suppress-namespaces (symbol? value) (namespace value))
    (name value)
    (pr-str value)))

(declare render)

(defn- limited-items [value length]
  (let [items (if length (take (inc length) value) value)]
    (if (and length (> (count items) length))
      [(take length items) true]
      [items false])))

(defn- flat-str [value options depth]
  (let [level (:level options)]
    (if (and level (>= depth level))
      "#"
      (cond
        (map? value)
        (let [[items more?] (limited-items value (:length options))
              parts (map (fn [[key item]]
                           (str (flat-str key options (inc depth)) " "
                             (flat-str item options (inc depth))))
                      items)]
          (str "{" (str/join ", "
                     (cond-> (vec parts) more? (conj "..."))) "}"))

        (vector? value)
        (let [[items more?] (limited-items value (:length options))]
          (str "[" (str/join " "
                     (cond-> (mapv #(flat-str %1 options (inc depth))
                               items)
                       more? (conj "..."))) "]"))

        (set? value)
        (let [[items more?] (limited-items value (:length options))]
          (str "#{" (str/join " "
                      (cond-> (mapv #(flat-str %1 options (inc depth))
                                items)
                        more? (conj "..."))) "}"))

        (seq? value)
        (let [[items more?] (limited-items value (:length options))]
          (str "(" (str/join " "
                     (cond-> (mapv #(flat-str %1 options (inc depth))
                               items)
                       more? (conj "..."))) ")"))

        :else
        (scalar-str value (:suppress-namespaces options))))))

(defn- render-seq [value column depth options open close]
  (let [[items more?] (limited-items value (:length options))
        items (cond-> (vec items) more? (conj '...))
        indent (inc column)]
    (if (empty? items)
      (str open close)
      (str open
        (render (first items) indent (inc depth) options)
        (apply str
          (map #(str "\n" (spaces indent)
                  (render %1 indent (inc depth) options))
            (rest items)))
        close))))

(defn- render-map [value column depth options]
  (let [[items more?] (limited-items value (:length options))
        entries
        (mapv
          (fn [[key item]]
            (let [key-text (render key (inc column) (inc depth) options)
                  value-column (+ column 2 (count key-text))]
              (str key-text " "
                (render item value-column (inc depth) options))))
          items)
        entries (cond-> entries more? (conj "..."))
        indent (inc column)]
    (if (empty? entries)
      "{}"
      (str "{" (first entries)
        (apply str
          (map #(str ",\n" (spaces indent) %1) (rest entries)))
        "}"))))

(defn- render [value column depth options]
  (let [flat (flat-str value options depth)
        margin (:right-margin options)]
    (if (or (not (:pretty options))
          (<= (+ column (count flat)) margin)
          (not (coll? value)))
      flat
      (cond
        (map? value) (render-map value column depth options)
        (vector? value) (render-seq value column depth options "[" "]")
        (set? value) (render-seq value (inc column) depth options "#{" "}")
        (seq? value) (render-seq value column depth options "(" ")")
        :else flat))))

(defn- parse-options [args]
  (when (odd? (count args))
    (throw (ex-info "ys::pprint/write requires key/value options" {})))
  (let [options (apply hash-map args)
        invalid (seq (remove option-names (keys options)))]
    (when invalid
      (throw
        (ex-info
          (str "Invalid ys::pprint/write option: " (first invalid)) {})))
    (merge {:length nil
            :level nil
            :miser-width *miser-width*
            :pretty true
            :right-margin *right-margin*
            :stream true
            :suppress-namespaces false}
      options)))

(defn write [value & args]
  (let [options (parse-options args)
        text (render value 0 0 options)
        stream (:stream options)]
    (if (nil? stream)
      text
      (binding [*out* (if (= true stream) *out* stream)]
        (print text)
        nil))))

(defn pp
  ([value]
   (binding [*out* (global/current-output)]
     (print (write value :stream nil))
     (println)))
  ([value writer]
   (binding [*out* writer]
     (print (write value :stream nil))
     (println))))

(comment
  )


;; This is the YS standard library.

;; The :glj reader conditionals keep this namespace loadable on
;; glojure, which lacks the clojure.math/pprint/data namespaces, Java
;; class imports and the babashka.fs backed ys.v0.fs namespace.
(ns ys.v0.std
  (:require
   #?@(:glj []
       :default [[clojure.data :as data]
                 [clojure.math :as math]
                 [ys.v0.fs :as fs]])
   [clojure.set :as cset]
   [clojure.string :as str]
   [ys.v0.common :as common :refer
    [atom? re-find+ regex?]]
   [ys.v0.ext :as ext]
   [ys.v0.global :as global]
   [ys.v0.io :as io]
   [ys.v0.pprint :as pprint]
   [ys.v0.re :as re]
   [ys.v0.util :as util]
   [ys.v0.ys :as ys])
  #?@(:glj []
      :default [(:import java.security.MessageDigest
                         java.util.Base64)])
  (:refer-clojure :exclude [atom
                            die
                            eval
                            print
                            read
                            replace
                            reverse
                            set]))


;; Guard against billion laughs style attacks
(def _max-alias-size (* 1024 1024))


;;------------------------------------------------------------------------------
;; String functions
;;------------------------------------------------------------------------------
(defn base64-decode [S]
  (String. (.decode (Base64/getDecoder) ^String S)))
(defn base64-encode [S]
  (.encodeToString (Base64/getEncoder) (.getBytes ^String S)))
(defn base64-points [S]
  (.encode (Base64/getEncoder) (.getBytes ^String S)))
(defn base64 [S] (base64-encode S))

(intern 'ys.v0.std 'blank? clojure.string/blank?)
(intern 'ys.v0.std 'chomp clojure.string/trim-newline)
(intern 'ys.v0.std 'chop common/chop)
(intern 'ys.v0.std 'ends? clojure.string/ends-with?)
(intern 'ys.v0.std 'escape clojure.string/escape)
(intern 'ys.v0.std 'index clojure.string/index-of)

(defn index [C x]
  (condf C
    string? (clojure.string/index-of C x)
    sequential? (let [i (.indexOf ^java.util.List C x)]
                  (if (>= i 0) i nil))
    (util/die "Can't index a " (type C))))

(defn join
  ([Ss] (join "" Ss))
  ([S Ss]
   (if (string? S)
     (str/join S Ss)
     (str/join Ss S)))
  ([S x & xs]
   (str/join S (cons x xs))))

(defn joins [Ss] (join " " Ss))

(intern 'ys.v0.std 'lc clojure.string/lower-case)

(defn lines [S]
  (if (empty? S)
    []
    (let [S (if (= (last S) \newline)
              (subs S 0 (dec (count S)))
              S)]
      (str/split S #"\n" -1))))

(defn pretty [x]
  (pprint/write x :stream nil))

(defn replace
  ([x] (clojure.core/replace x))
  ([x y] (if (or (regex? y) (string? y))
           (clojure.string/replace x y "")
           (clojure.core/replace x y)))
  ([x y z] (clojure.string/replace x y z)))

(intern 'ys.v0.std 'replace1 clojure.string/replace-first)
(intern 'ys.v0.std 'rindex clojure.string/last-index-of)

(defn split
  ([S]
   (let [S (str S)]
     (if (empty? S)
       []
       (clojure.string/split S #""))))
  ([S R]
   (let [[S R] (if (regex? S) [R S] [S R])
         S (str S)
         R (if (string? R) (re-pattern R) R)]
     (clojure.string/split S R))))

(intern 'ys.v0.std 'starts? clojure.string/starts-with?)

(defn substr
  ([str off] (substr str off (- (count str) off)))
  ([str off len]
   (let [slen (count str)
         off (if (neg? off) (+ slen off) off)
         [len slen] (if (neg? len)
                      (let [len (max 0 (+ slen len))]
                        [len len])
                      [len slen])]
     (condp apply [off slen]
       < (when (>= (+ off len) 0)
           (let [[off len] (if (neg? off)
                             [0 (+ off len)]
                             [off len])
                 len (if (> (+ off len) slen) (- slen off) len)]
             (subs str off (+ off len))))
       = ""
       > nil))))

(defn text [Ss]
  (if (empty? Ss)
    ""
    (str/join "\n"
      (concat Ss (list "")))))

(intern 'ys.v0.std 'trim clojure.string/trim)
(intern 'ys.v0.std 'triml clojure.string/triml)
(intern 'ys.v0.std 'trimr clojure.string/trimr)

(intern 'ys.v0.std 'uc clojure.string/upper-case)
(intern 'ys.v0.std 'uc1 clojure.string/capitalize)

(defn words [S]
  (clojure.string/split S #"\s+"))


;;------------------------------------------------------------------------------
;; Collection functions
;;------------------------------------------------------------------------------
(defn put
  ([map key value]
   (assoc map key value))
  ([map key value & key-values]
   (apply assoc map key value key-values)))

(defn del
  ([map]
   map)
  ([map key]
   (dissoc map key))
  ([map key & keys]
   (apply dissoc map key keys)))

(defn get+ [C K]
  (condf C
    map? (if (symbol? K)
           (cond
             (contains? C K) (get C K)
             (contains? C (str K)) (get C (str K))
             :else (get C (keyword K)))
           (if (contains? C K)
             (get C K)
             (get C (str K))))
    set? (if (contains? C K)
           K
           (if (symbol? K)
             (let [s (str K)]
               (if (contains? C s) s nil))
             nil))
    nil? nil
    seqable? (condf K
               number? (nth C K nil)
               #(= '$ %1) (last C)
               nil? nil
               nil)
    nil))

(defn diff [a b]
  ((util/backend 'clojure.data/diff) a b))

(defn flat [C]
  (mapcat
    (fn [x] (if (seqable? x) x [x]))
    C))

(defn grep [P C]
  (let [[P C] (if (seqable? C) [P C] [C P])
        _ (when-not (seqable? C)
            (util/die "No seqable arg passed to grep"))]
    (condf P
      regex? (filter #(re-find P %1) C)
      fn? (filter P C)
      (filter #(= P %1) C))))

(defn has?
  ([C] #(has? C %1))
  ([C x]
   (boolean
     (cond
       (and (string? C) (string? x)) (str/includes? C x)
       (map? C) (get+ C (symbol x))
       :else (some #(= %1 x) C)))))

#_{:clj-kondo/ignore [:syntax]}
(defn in?
  ([C] #(in? C %1))
  ([x C] (has? C x)))

(defn +concat [& C]
  (vec
    (apply concat (map #(cond
                          (sequential? %1) %1
                          (or
                            (string? %1)
                            (number? %1)
                            (boolean? %1)
                            (map? %1)
                            (set? %1)) [%1]
                          :else nil) C))))

(defn +merge [M]
  (if (:-<< M)
    (let [m (dissoc M :-<<)
          q (get M :-<<)
          v (if (map? q)
              (vector q)
              (if (seqable? q)
                (vec q)
                (util/die "Can't merge " q)))
          M (apply merge-with (fn [x _] x) m v)]
      (+merge M))
    M))

;; The flatland.ordered backend resolves at first call so that Clojure
;; runtimes that can't run it (like jolt, currently) can still load
;; this namespace. Those runtimes fall back to array-map, which keeps
;; construction order at any size; order is only lost when assoc grows
;; a map past eight entries and promotes it to a hash map. The probe
;; constructs a map because loading can succeed where construction
;; does not.
(def ^:private ordered-map-fn
  (delay
    (util/catching
      (let [f (do (require 'flatland.ordered.map)
                  (resolve 'flatland.ordered.map/ordered-map))]
        (f :a 1 :b 2)
        f)
      nil)))

(defn omap [& xs]
  (if-let [ordered-map @ordered-map-fn]
    (apply ordered-map xs)
    (apply array-map xs)))

(defn merge-omap
  "Merge maps while preserving the first insertion position of each key."
  [& maps]
  (apply omap
    (mapcat #(mapcat identity %1) maps)))

(intern 'ys.v0.std '% omap)

(defn reverse [x]
  (condf x
    string? (clojure.string/reverse x)
    vector? (vec (clojure.core/reverse x))
    seqable? (clojure.core/reverse x)
    (util/die "Can't reverse " x)))

(defn rng [x y]
  (let [[a b] (for [n [x y]] (if (char? n) (long n) n))]
    (cond
      (and (number? x) (number? y))
      (if (> b a)
        (range a (inc b))
        (range a (dec b) -1))
      (and (char? x) (char? y))
      (if (> b a)
        (map char (range a (inc b)))
        (map char (range a (dec b) -1)))
      :else
      (util/die "Can't rng(" (pr-str x) ", " (pr-str y) ")"))))

(defn slice [C & ks]
  (let [ks (flatten ks)]
    (vec (map (fn [k] (get+ C k)) ks))))


;;------------------------------------------------------------------------------
;; Math functions
;;------------------------------------------------------------------------------
(declare to-list to-num to-map to-set to-vec)

(defn add
  ([] 0)
  ([x] #(+ %1 x))
  ([x y] (+ x y))
  ([x y & xs] (apply + x y xs)))

(defn sub
  ([x] #(- %1 x))
  ([x y] (- x y))
  ([x y & xs] (apply - x y xs)))

(defn mul
  ([] 1)
  ([x] #(* %1 x))
  ([x y] (* x y))
  ([x y & xs] (apply * x y xs)))

(defn div
  ([x] #(div %1 x))
  ([x y]
   (let [a (/ x y)]
     (if (ratio? a)
       (double a)
       a)))
  ([x y & xs]
   (reduce div (div x y) xs)))

(defn digits [n]
  (let [n (str n)]
    (when (re-matches #"[0-9]+" n)
      (for [d n]
        (- (byte d) 48)))))

;; On glojure the math functions come from Go's math package (there is
;; no clojure.math namespace there yet):
(def max-long 9223372036854775807)

(defn- pow* [x y]
  #?(:glj (math.Pow (double x) (double y))
     :default (math/pow x y)))

(intern 'ys.v0.std 'floor
  #?(:glj (fn [x] (math.Floor (double x)))
     :default math/floor))

(defn pow
  ([x] #(pow %1 x))
  ([x y]
   (if (and (integer? x) (integer? y) (>= y 0))
     (let [a (pow* x y)]
       (if (<= a max-long)
         (long a)
         a))
     (pow* x y)))

  ([x y & xs]
   (let [[& xs] (clojure.core/reverse (conj xs y x))]
     (reduce #(pow %2 %1) 1 xs))))

(intern 'ys.v0.std 'round
  #?(:glj (fn [x] (long (math.Round (double x))))
     :default math/round))

(defn sum [xs]
  (reduce + 0 (filter identity xs)))

(defn sqr  [N] (pow N 2))
(defn cube [N] (pow N 3))
(intern 'ys.v0.std 'sqrt
  #?(:glj (fn [x] (math.Sqrt (double x)))
     :default math/sqrt))

(defn- op-error
  ([op x]
   (util/die "Can't " op "(" (pr-str x) ")"))
  ([op x y]
   (util/die "Can't " op "(" (pr-str x) " " (pr-str y) ")")))

(defn inc+ [x]
  (condf x
    number? (inc x)
    char? (char (inc (long x)))
    (let [n (to-num x 0)]
      (condf n
        number? (inc n)
        (op-error "inc+" x)))))

(defn dec+ [x]
  (condf x
    number? (dec x)
    char? (char (dec (long x)))
    (let [n (to-num x 0)]
      (condf n
        number? (dec n)
        (op-error "dec+" x)))))

(defn- set-pair? [x y]
  (and (or (set? x) (set? y))
    (or (set? x) (sequential? x))
    (or (set? y) (sequential? y))))

(defn- filter-members [x y keep?]
  (let [members (to-set y)
        values (filter #(= keep? (contains? members %1)) x)]
    (if (vector? x) (vec values) values)))

(defn add+
  ([x y]
   (condf x
     number? (+ (to-num x 0) (to-num y 0))
     string? (str x y)
     map? (merge (to-map x) (to-map y))
     set? (cset/union x (to-set y))
     vector? (vec (concat (to-vec x)
                    (if (or (sequential? y) (set? y))
                      (seq y)
                      (list y))))
     char? (if (number? y)
             (char (+ (int x) y))
             (str x y))
     fn? (partial x y)
     nil? (util/die "Can't add+ to a nil value")
     seqable? (concat (to-list x)
                (if (or (sequential? y) (set? y))
                  (seq y)
                  (list y)))
     (+ (to-num x 0) (to-num y 0))))

  ([x y & xs]
   (reduce add+ (add+ x y) xs)))

(defn div+ [& xs] (apply div (map to-num xs)))

(defn mul+
  ([x y]
   (cond
     (set-pair? x y) (if (set? x)
                       (cset/intersection x (to-set y))
                       (filter-members x y true))
     (and (string? x) (number? y)) (apply str (repeat y x))
     (and (number? x) (string? y)) (apply str (repeat x y))
     (and (vector? x) (number? y)) (vec (apply concat (repeat y x)))
     (and (number? x) (vector? y)) (vec (apply concat (repeat x y)))
     (and (sequential? x) (number? y)) (apply concat (repeat y x))
     (and (number? x) (sequential? y)) (apply concat (repeat x y))
     (fn? x) (comp y x)
     (nil? x) (util/die "Can't mul+ to a nil value")
     :else  (* (to-num x 1) (to-num y 1))))

  ([x y & xs]
   (reduce mul+ (mul+ x y) xs)))

(defn sub+
  ([x y]
   (if (set-pair? x y)
     (if (set? x)
       (cset/difference x (to-set y))
       (filter-members x y false))
     (condf x
       string? (str/replace x (str y) "")
       map? (dissoc x y)
       set? (disj x y)
       vector? (vec (remove #(= y %1) x))
       number? (- x (to-num y 0))
       char? (condf y
               number? (char (- (long x) y))
               char? (- (long x) (long y))
               (op-error "sub" x y))
       nil? (util/die "Can't sub+ to a nil value")
       seqable? (remove #(= y %1) x)
       (+ (to-num x 0) (to-num y 0)))))

  ([x y & xs]
   (reduce sub+ (sub+ x y) xs)))


;;------------------------------------------------------------------------------
;; Control functions
;;------------------------------------------------------------------------------

(defmacro value [x]
  `(let [var# (condp #(%1 %2) ~x
                string? (ns-resolve *ns* (symbol ~x))
                symbol? (ns-resolve *ns* ~x)
                var? ~x
                nil)]
     (when var# (var-get var#))))

(defmacro call [x & xs]
  `(let [f# (or (value ~x) ~x)]
     (when-not (ifn? f#) (util/die "Can't call(" (pr-str f#) ")"))
     (f# ~@xs)))

(intern 'ys.v0.std 'die ys.v0.util/die)

(defmacro each [bindings & body]
  `(doall (for ~bindings (do ~@body))))

(defn eval [S]
  (ys/eval (str "!ys-0\n" S)))

(defn exit
  ([] (exit 0))
  ([I] (System/exit I)))

;; `if` is a special form in Clojure, but we can make resolve with this for use
;; in dot chaining.
(defn if [cond then else] (if cond then else))

(defn sleep [I]
  (Thread/sleep (int (* 1000 I))))

(defn throw [e] (throw e))

(defmacro when+ [test & body]
  (list 'when-let ['_ test] (cons 'do body)))


;;------------------------------------------------------------------------------
;; Function functions
;;------------------------------------------------------------------------------

(defn flip [f]
  (fn
    ([] (f))
    ([a] (f a))
    ([a b] (f b a))
    ([a b c] (f c b a))
    ([a b c d] (f d c b a))
    ([a b c d & rest]
     (->> rest
       (concat [a b c d])
       clojure.core/reverse
       (apply f)))))


;;------------------------------------------------------------------------------
;; Regex functions
;;------------------------------------------------------------------------------

;; See: `qr` function above

(defn =-- [S R]
  (re-find+ R S))

(defn !-- [S R]
  (not (=-- S R)))

(defn =--- [S R]
  (re-matches R (str S)))

(defn !--- [S R]
  (not (=--- S R)))


;;------------------------------------------------------------------------------
;; I/O functions
;;------------------------------------------------------------------------------
#?(:glj
   (defn read [& _]
     (util/die "The fs functions are not available"
       " in this Clojure runtime"))
   :default
   (defn read [path]
     (ys/check-module-access! 'ys.fs)
     (fs/read path)))

#?(:glj
   (defn write [& _]
     (util/die "The fs functions are not available"
       " in this Clojure runtime"))
   :default
   (defn write [path content]
     (ys/check-module-access! 'ys.fs)
     (fs/write path content)))

(defn err [& xs]
  (ys/check-module-access! 'ys.io)
  (apply io/err xs))

(defn out [& xs]
  (ys/check-module-access! 'ys.io)
  (apply io/out xs))

(defn pp [x]
  (ys/check-module-access! 'ys.pprint)
  (pprint/pp x))

(defn print [& xs]
  (ys/check-module-access! 'ys.io)
  (apply io/print xs))

(defn readline
  ([]
   (ys/check-module-access! 'ys.io)
   (io/readline (global/current-input)))
  ([reader]
   (ys/check-module-access! 'ys.io)
   (io/readline reader)))

(defn say [& xs]
  (ys/check-module-access! 'ys.io)
  (apply io/say xs))

(defn warn [& xs]
  (ys/check-module-access! 'ys.io)
  (apply io/warn xs))


;;------------------------------------------------------------------------------
;; Shorter named alias functions
;;------------------------------------------------------------------------------

(intern 'ys.v0.std 'a clojure.core/identity)

(intern 'ys.v0.std 'len clojure.core/count)


;;------------------------------------------------------------------------------
;; Quoting functions
;;------------------------------------------------------------------------------

(defmacro q
  ([x] `(quote ~x))
  ([x & xs] `(quote (~x ~@xs))))

(defmacro ql [& xs] `(list ~@xs))
(defmacro qm [& xs] `(hash-map ~@xs))
(defmacro qo [& xs] `(omap ~@xs))
(defmacro qs [& xs]
  `(clojure.string/join " " '(~@xs)))
(defmacro qv [& xs] `(vector ~@xs))

(defn qr [S] (re-pattern S))

(defmacro qw [& xs]
  (let [xs# (map #(cond
                    (nil? %1) "nil"
                    (= {} %1) "{}"
                    :else (str %1))
              xs)]
    `[~@xs#]))


;;------------------------------------------------------------------------------
;; Named function aliases for infix operators
;;------------------------------------------------------------------------------
(defn eq
  ([x] #(= %1 x))
  ([x y] (= x y))
  ([x y & xs] (apply = x y xs)))

(defn ne
  ([x] #(not= %1 x))
  ([x y] (not= x y))
  ([x y & xs] (apply not= x y xs)))

(defn- compare-chain [compare x y xs]
  (loop [x x y y xs xs]
    (if (compare x y)
      (if (seq xs)
        (recur y (first xs) (next xs))
        true)
      false)))

(defn gt
  ([x] #(gt %1 x))
  ([x y] (if (and (set? x) (set? y))
           (and (not= x y) (cset/superset? x y))
           (> x y)))
  ([x y & xs] (compare-chain gt x y xs)))

(defn ge
  ([x] #(ge %1 x))
  ([x y] (if (and (set? x) (set? y))
           (cset/superset? x y)
           (>= x y)))
  ([x y & xs] (compare-chain ge x y xs)))

(defn lt
  ([x] #(lt %1 x))
  ([x y] (if (and (set? x) (set? y))
           (and (not= x y) (cset/subset? x y))
           (< x y)))
  ([x y & xs] (compare-chain lt x y xs)))

(defn le
  ([x] #(le %1 x))
  ([x y] (if (and (set? x) (set? y))
           (cset/subset? x y)
           (<= x y)))
  ([x y & xs] (compare-chain le x y xs)))


;;------------------------------------------------------------------------------
;; Common type casting functions
;;------------------------------------------------------------------------------
(declare to-num to-type)

(defn to-bool [x] (boolean x))

(defn to-char [x]
  (condf x
    char? x
    string? (if (= 1 (count x))
              (first x)
              (util/die "Can't convert multi-char string to char"))
    number? (char x)
    (util/die "Can't convert " (to-type x) " to char")))

(defn to-float [x] (double (to-num x)))

(defn to-int [x] (long (to-num x)))

(defn to-keyw [x] (keyword x))

(defn to-list [x]
  (condf x
    map? (reduce-kv (fn [acc k v] (conj acc v k)) '() x)
    sequential? (if (empty? x) '() (seq x))
    string? (if (empty? x) '() (seq x))
    nil? '()
    (util/die "Can't convert " (to-type x) " to list")))

(defn to-map [x]
  (condf x
    map? x
    set? (zipmap (seq x) (repeat nil))
    sequential? (apply hash-map (seq x))
    string? (apply hash-map (seq x))
    nil? {}
    (util/die "Can't convert " (to-type x) " to map")))

(defn to-num
  ([x] (to-num x nil))
  ([x default]
   (condf x
     number? x
     string? (when (re-matches re/xnum x)
               (read-string x))
     nil? (util/die "Can't convert a nil value to a number")
     seqable? (count x)
     char? (int x)
     boolean? (if x 1 0)
     (util/die (str "Can't convert a value of type '"
                 (to-type x) "' to a number")))))

(defn to-omap [x]
  (condf x
    sequential? (apply omap x)
    map? (apply omap (into [] cat x))
    nil? (omap)
    (util/die "Can't convert " (to-type x) " to omap")))

(defn to-set [x]
  (condf x
    map? (clojure.core/set (keys x))
    seqable? (clojure.core/set (seq x))
    nil? (clojure.core/set nil)
    (util/die "Can't convert " (to-type x) " to set")))

(defn set
  ([] #{})
  ([x] (to-set x)))

(defn to-str [x]
  (condf x
    string? x
    nil? "nil"
    (str x)))

(defn to-type [x]
  (condf x
    atom? "atom"
    boolean? "bool"
    char? "char"
    class? "class"
    float? "float"
    fn? "fun"
    int? "int"
    keyword? "keyw"
    list? "list"
    map? "map"
    nil? "nil"
    number? "num"
    regex? "rgx"
    seq? "seq"
    set? "set"
    string? "str"
    symbol? "sym"
    var? "var"
    vector? "vec"
    (util/die "Can't determine type of '" (type x) "' value")))

(defmacro to-vars
  ([m] `(ys/+def-vars *ns* ~m))
  ([m force]
   (when (and force (not= force :force))
     (util/die "to-vars() force argument must be :force"))
  `(ys/+def-vars *ns* ~m ~force)))

(defn to-vec [x]
  (condf x
    map? (reduce-kv (fn [acc k v] (conj acc k v)) [] x)
    sequential? (vec x)
    string? (vec x)
    nil? []
    (util/die "Can't convert " (or (type x) "nil") " to vector")))

(intern 'ys.v0.std 'B to-bool)
(intern 'ys.v0.std 'C to-char)
(intern 'ys.v0.std 'F to-float)
(intern 'ys.v0.std 'I to-int)
(intern 'ys.v0.std 'K to-keyw)
(intern 'ys.v0.std 'L to-list)
(intern 'ys.v0.std 'M to-map)
(intern 'ys.v0.std 'N to-num)
(intern 'ys.v0.std 'O to-omap)
(intern 'ys.v0.std 'S to-str)
(intern 'ys.v0.std 'T to-type)
(intern 'ys.v0.std 'V to-vec)

(intern 'ys.v0.std 'L+ list)
(intern 'ys.v0.std 'M+ hash-map)
(intern 'ys.v0.std 'O+ omap)
(intern 'ys.v0.std 'V+ vector)


;;------------------------------------------------------------------------------
;; Alternate truth functions
;;------------------------------------------------------------------------------

(defn falsey? [x]
  (condf x
    number? (zero? x)
    seqable? (empty? x)
    identity false
    true))

(defmacro F? [x] `(falsey? ~x))

(defn truey? [x]
  (if (falsey? x) nil x))

(defmacro T? [x] `(truey? ~x))

(defmacro or?
  ([] nil)
  ([x]
   `(truey? ~x))
  ([x & xs]
   `(or (truey? ~x) (or? ~@xs))))

(defmacro ||| [x & xs] `(or? ~x ~@xs))

(defmacro and?
  ([] true)
  ([x]
   `(truey? ~x))
  ([x & xs]
   `(and (truey? ~x) (and? ~@xs))))

(defmacro &&& [x & xs] `(and? ~x ~@xs))


;;------------------------------------------------------------------------------
;; Date/Time functions
;;------------------------------------------------------------------------------
(defn now
  ([] (java.time.Instant/now))
  ([f] (condp eq f
         :local (java.time.LocalDateTime/now)
         :zoned (java.time.ZonedDateTime/now)
         :utc (java.time.Instant/now)
         (util/die "Unknown time format: '" f "'"))))

(defn instant [x]
  (condf x
    number? (java.time.Instant/ofEpochMilli x)
    string? (java.time.Instant/parse x)
    (util/die "Can't instant(" (pr-str x) ")")))


;;------------------------------------------------------------------------------
;; YAML anchor and alias functions
;;------------------------------------------------------------------------------
(defn _& [sym val]
  (when (> (count (str val)) _max-alias-size)
    (util/die "Anchored node &" sym " exceeds max size of " _max-alias-size))
  (swap! global/stream-anchors_ assoc sym val)
  (swap! global/doc-anchors_ assoc sym val)
  val)

(defn _* [sym]
  (or
    (+merge (get @global/doc-anchors_ sym))
    (util/die "1 Anchor not found: &" sym)))

(defn _** [sym]
  (or
    (+merge (get @global/stream-anchors_ sym))
    (util/die "2 Anchor not found: &" sym)))


;;------------------------------------------------------------------------------
;; Java interop functions
;;------------------------------------------------------------------------------
;; clojure.lang.Reflector only exists on JVM Clojure, so resolve it lazily
;; and die at call time on other runtimes (like babashka).
(def ^:private invoke-constructor
  (delay
    (util/catching
      (clojure.core/eval
        '(fn [class args]
           (clojure.lang.Reflector/invokeConstructor
             class (into-array Object args))))
      nil)))

(defn new [class & xs]
  (if-let [f @invoke-constructor]
    (f class xs)
    (util/die "new() requires a JVM Clojure runtime")))


;;------------------------------------------------------------------------------
;; Security functions
;;------------------------------------------------------------------------------
(defn md5 [^String string]
  (let [digest (.digest (MessageDigest/getInstance "MD5")
                 (.getBytes string "UTF-8"))]
    (apply str (map (partial format "%02x") digest))))

(defn sha1 [^String string]
  (let [digest (.digest (MessageDigest/getInstance "SHA-1")
                 (.getBytes string "UTF-8"))]
    (apply str (map (partial format "%02x") digest))))

(defn sha256 [^String string]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                 (.getBytes string "UTF-8"))]
    (apply str (map (partial format "%02x") digest))))


;;------------------------------------------------------------------------------
;; External library functions
;;------------------------------------------------------------------------------
(defn use-pod [pod-name version]
  (ys/load-pod pod-name version))


;;------------------------------------------------------------------------------
;; HTTP functions
;;------------------------------------------------------------------------------

(defn get-url [url]
  (ext/convert-url url))

(defn load-url [url]
  (ys/load-url url))

;;------------------------------------------------------------------------------
;; YS document result stashing functions
;;------------------------------------------------------------------------------
(defn +++* [value]
  (reset! global/doc-anchors_ {})
  (when ((some-fn map? seqable? number? string?) value)
    (global/set-underscore value)
    (swap! global/stream-values conj value))
  value)

(defmacro +++ [& xs]
  `(do
     (intern '~'main '~'+value (+++* (do ~@xs)))
     (~'in-ns '~'main)
     ~'main/+value))

(defn stream
  ([] @global/stream-values)
  ([values] (reset! global/stream-values values)
            nil))


;;------------------------------------------------------------------------------
;; Atom functions
;;------------------------------------------------------------------------------

(defn atom
  ([] (atom nil))
  ([x] (clojure.core/atom x)))

(defn reset
  ([x y] (clojure.core/reset! x y)))

(defn swap
  ([f & xs] (apply clojure.core/swap! f xs)))


;;------------------------------------------------------------------------------
;; Special functions
;;------------------------------------------------------------------------------

;; Used to run a YS file as a Bash script:
(defmacro source [& xs])

;; def destructuring
(declare +def-defn)

(defn- destructure-vector [V idx]
  (map-indexed
    (fn [i name]
      (+def-defn name `(nth ~idx ~i nil)))
    V))

(defn- destructure-map [M idx]
  (map
    (fn [[k v]]
      (+def-defn k `(get ~idx ~v)))
    M))

(defn- destructure-idx [x idx]
  (let [root (gensym)]
    `(let [~root ~idx]
       ~@(condf x
           map? (destructure-map x root)
           seqable? (destructure-vector x root)
           []))))

(defn- +def-defn [x y]
  (if (symbol? x)
    `(def ~x ~y)
    (destructure-idx x y)))

(defmacro +def [x y]
  (+def-defn x y))

(defn env-update
  ([m]
   (let
    [m (reduce-kv
         (fn [env k v]
           (when-not (string? k)
             (util/die "env-update() keys must be strings"))
           (let [v (condf v
                     string? v
                     number? (str v)
                     boolean? (str v)
                     nil? nil
                     (util/die "env-update() values must be scalars"))]
             (assoc env k v))) {} m)]
     (global/update-env m)
     (global/update-environ m)))
  ([k v & xs] (env-update (apply hash-map k v xs))))


;;------------------------------------------------------------------------------
(comment
  )


;; This library contains the clojure.core functions that are replaced by the
;; ys::std library.
;; They can be accessed with clj/foo instead of foo.
;;
;; Functions that the host Clojure runtime does not provide (babashka has no
;; compile, load or load-file) are skipped.

(ns ys.v0.clj
  (:refer-clojure :only [doseq intern ns-resolve var-get when-let]))

(doseq [sym '[atom compile eval flush line-seq load load-file
              load-reader newline print printf println pr prn read read-line
              replace reverse set use]]
  (when-let [v (ns-resolve 'clojure.core sym)]
    (intern 'ys.v0.clj sym (var-get v))))


(ns yamlscript.regex
  (:refer-clojure :exclude [compile]))

(defn compile [pattern]
  (re-pattern pattern))

(ns ys.v0.taptest
  (:require
   [clojure.string :as str]
   [ys.v0.util :as util]
   [ys.v0.ys :as ys]
   [ys.v0.yaml :as yaml]
   [ys.v0.global :as global]
   [yamlscript.regex :as regex]
   [ys.v0.common])
  (:refer-clojure
   :exclude [test]))

(def counter (atom 0))

(def error-map-hook
  (atom (fn [error] (Throwable->map error))))

(def command-hook
  (atom
    (fn [opts command]
      ((util/backend 'babashka.process/sh) opts command))))

(defn- error-map [error]
  (@error-map-hook error))

(defn- get-test-name [test]
  (let [name (or
               (get test "name")
               (get test "cmnd")
               (get test "code")
               "")
        name (if (and
                   (= "error" (get test "what"))
                   (get test "want"))
               (get test "want")
               name)
        name (str/replace
               (str/trim name)
               #"\n"
               (str/re-quote-replacement "\\n"))]
    name))

(defn- passed [test]
  (let
   [name (get-test-name test)
    count (deref counter)]
    (if name
      (println (str "ok " count " - " name))
      (println (str "ok " count)))))

;; TODO - Handle unexpected LazySeq values
(defn- format-value [value]
  (let  [value (with-out-str (pr value))]
    (if (re-find #"^[^\"].*\n" value)
      (str "'" value "'")
      value)))

(defn- failed [test got]
  (let
   [name (get-test-name test)
    count (deref counter)
    keys (set (keys test))
    want (if (contains? keys "want")  ;; want can be false or nil
           (get test "want")
           (or
             (get test "like")
             (get test "have")))
    want (format-value want)
    got (format-value got)
    out (str " Failed test '" name "'\n"
          "        got: "   got "\n   expected: " want)
    out (str/replace out #"(?m)^" "#  ")]
    (if name
      (println (str "not ok " count " - " name))
      (println (str "not ok " count)))
    (println out)))

(defn- check-string [value key count]
  (when-not (string? value)
    (die (str "taptest: Test " count " '" key "' key must be a string"))))

(defn- run-code [test]
  (swap! counter inc)
  (let [count (deref counter)
        code (get test "code")
        what (get test "what")
        _ (check-string code "code" count)
        mode (get test "mode")
        code (cond
               (= "data" mode) (str "!ys-0:\n" code "\n")
               (= "bare" mode) code
               :else (str "!ys-0\n" code "\n"))]
    (try (if (= "out" what)
           (with-out-str (ys/eval code))
           (ys/eval code))
         (catch Exception e
           (cond
             (= "error" what) (str/trim-newline (.getMessage e))
             (get test "form") (error-map e)
             :else (throw e))))))

(defn- load-data [test]
  (swap! counter inc)
  (let [count (deref counter)
        data (get test "data")
        what (get test "what")
        _ (check-string data "data" count)
        data (str "!ys-0:\n" data "\n")]
    (try (if (= "out" what)
           (with-out-str (ys/eval data))
           (yaml/dump (ys/eval data)))
         (catch Exception e
           (cond
             (= "error" what) (str/trim-newline (.getMessage e))
             (get test "form") (error-map e)
             :else (throw e))))))

(defn- run-cmnd [test]
  (swap! counter inc)
  (let [count (deref counter)
        cmnd (get test "cmnd")
        what (get test "what")
        _ (check-string cmnd "cmnd" count)
        opts {}
        stdi (get test "stdi")
        opts (if stdi
               (assoc opts :in stdi)
               opts)
        ret (@command-hook opts cmnd)]
    (if (get test "form")
      ret
      (case what
        "all" {"exit" (:exit ret)
               "out" (:out ret)
               "err" (:err ret)}
        "exit" (:exit ret)
        "out" (:out ret)
        "err" (:err ret)
        ""))))

(defn- get-tests [tests]
  (let [tests2 (filter #(get %1 "ONLY") tests)]
    (if (empty? tests2)
      (filter #(not (get %1 "SKIP")) tests)
      tests2)))

(defn- init-test [test]
  (swap! global/opts assoc :unordered true)
  (let [keys (set (keys test))
        what (get test "what")
        form (get test "form")]
    (when (and what form (not= what "out"))
      (die "taptest: 'what' and 'form' are mutually exclusive"))
    (cond
      (contains? keys "code")
      (assoc test "what" (or what "value"))
      ,
      (contains? keys "data")
      (assoc test "what" (or what "value"))
      ,
      (contains? keys "cmnd")
      (assoc test "what" (or what "out"))
      ,
      (some #{"diag" "note"} keys)
      test
      ,
      :else
      (die "taptest: Test requires one of: 'code', 'data', 'cmnd'"))))

(defn- normalize [got test]
  (let [what (get test "what")
        form (get test "form")
        mode (get test "mode")]
    (cond
      (= "data" mode) (pr-str got)
      (= "bare" mode) (pr-str got)
      form (if (fn? form)
             (form got test)
             (die "taptest: 'form' must be a function"))
      (or (= what "out") (= what "err"))
      (let [got (str/trimr got)]
        (if (empty? got)
          ""
          (str got "\n")))
      :else got)))


;;------------------------------------------------------------------------------
;; Public API functions
;;------------------------------------------------------------------------------
(defn test [tests]
  (let [tests (get-tests tests)]
    (doall
      (for [test tests]
        (let [test (init-test test)
              keys (set (keys test))
              _ (do
                  (when-let [note (get test "note")]
                    (println (str "# " note)))
                  (when-let [diag (get test "diag")]
                    (binding [*out* *err*]
                      (println (str "# " diag)))))
              got (condp #(contains? %2 %1) keys
                    "code" [(run-code test)]
                    "data" [(load-data test)]
                    "cmnd" [(run-cmnd test)]
                    nil)]
          (when got
            (let [got (first got)]
              (condp #(contains? %2 %1) keys
                "want"
                (let [got (normalize got test)
                      want (normalize (get test "want")
                             {"what" (get test "what")})]
                  (if (= got want)
                    (passed test)
                    (failed test got)))
                ,
                "like"
                (let [rgx (regex/compile (get test "like"))]
                  (if (re-find rgx got)
                    (passed test)
                    (failed test got)))
                ,
                "have"
                (if (str/includes? got (get test "have"))
                  (passed test)
                  (failed test got))
                ,
                "code"
                (let [test (assoc test "want" true)]
                  (if (= got true)
                    (passed test)
                    (failed test got)))
                "note" nil
                "diag" nil
                ,
                (die (str "taptest: Test " @counter
                       " requires one of: 'want', 'like', 'have'"))))))))))

(defn plan [n]
  (println (str "1.." n)))

(defn done
  ([n] (let [n (or n (deref counter))]
         (println (str "1.." n))))
  ([] (done nil)))

(comment
  )


;; The ys.v0 namespace makes code compiled by `ys -T clj` (and the other
;; code targets) runnable by plain Clojure runtimes like babashka, JVM
;; Clojure, jolt and glojure. Compiled programs start with:
;;
;;   (ns main (:require ys.v0))
;;   (ys.v0/init)
;;
;; init refers the full YS standard library into the calling namespace
;; (silently replacing any clojure.core mappings, so no replace warnings),
;; installs the YS runtime functions and binds the YS runtime variables.
;;
;; Under the SCI ys runtime this namespace is shadowed by a no-op stub.
;; Under the Glojure ys runtime init detects the native runtime and is a no-op,
;; so the same compiled code runs identically in both.
;;
;; Namespaces some runtimes lack (see manifest/optional-nses) load via
;; guarded requires.

(ns ys.v0
  (:require
   [clojure.string :as str]
   [clojure.walk]
   [ys.v0.clj]
   [ys.v0.common :as common]
   [ys.v0.csv]
   [ys.v0.debug]
   [ys.v0.dwim]
   [ys.v0.ext]
   [ys.v0.global :as global]
   [ys.v0.http]
   [ys.v0.io]
   [ys.v0.ipc]
   [ys.v0.json]
   [ys.v0.manifest :as manifest]
   [ys.v0.pods]
   [ys.v0.pprint]
   [ys.v0.re :as re]
   [ys.v0.std]
   [ys.v0.util :as util]
   [ys.v0.yaml]
   [ys.v0.ys]))

(def VERSION "0.3.5")

;; Load what this runtime can provide of the optional namespaces:
(doseq [ns-sym manifest/optional-nses]
  (util/catching (require ns-sym) nil))

(defn- refer-exports
  "Refer every manifest export into the target namespace. Symbols are
  ns-unmapped first so clojure.core shadows replace silently. (Needs
  glojure newer than 0.6.8, where ns-unmap was broken.)"
  [target]
  (doseq [sym (manifest/exported-syms)]
    (ns-unmap target sym))
  (doseq [sym manifest/hidden-core]
    (ns-unmap target sym))
  (doseq [[ns-sym syms] manifest/refers]
    (when (find-ns ns-sym)
      (if (= :all syms)
        (refer ns-sym)
        (refer ns-sym :only (vec syms)))))
  ;; The YS runtime overrides clojure.core load and use with YS versions.
  (doseq [sym manifest/runtime-overrides]
    (ns-unmap target sym))
  (refer 'ys.v0.ys :only '[load-file use] :rename '{load-file load}))

(defn- refer-runtime-vars [target]
  (doseq [sym manifest/runtime-vars]
    (ns-unmap target sym))
  (refer 'ys.v0.global :only (vec manifest/runtime-vars)))

(defn- coerce-args
  "Coerce command line arguments the way the ys runtime does."
  [args]
  (vec
    (map #(cond
            (re-matches re/xnum %1)
            (read-string (str/replace %1 #"^([-+]?)0o"
                           (str "$1" "0")))
            ,
            (re-matches re/keyw %1)
            (keyword (subs %1 1))
            :else %1)
      args)))

(defn- normalize-os [os-name]
  (let [os-name (str/lower-case (or os-name ""))]
    (cond
      (str/includes? os-name "linux") "linux"
      (str/includes? os-name "mac") "macos"
      (str/includes? os-name "windows") "windows"
      :else "unknown")))

(defn- normalize-arch [arch]
  (let [arch (str/lower-case (or arch ""))]
    (case arch
      ("amd64" "x86_64") "x86_64"
      ("aarch64" "arm64") "aarch64"
      (if (seq arch) arch "unknown"))))

(defn- get-hostname []
  (or
    #?(:glj nil
       :default
       (util/catching
         (not-empty (.getHostName (java.net.InetAddress/getLocalHost)))
         nil))
    (not-empty (System/getenv "HOSTNAME"))
    (not-empty (System/getenv "COMPUTERNAME"))))

(defn- get-runtime-info []
  {:args (util/catching (common/get-cmd-args) [])
   :arch (normalize-arch (System/getProperty "os.arch"))
   :bin (util/catching (common/get-cmd-bin) nil)
   :hostname (get-hostname)
   :os (normalize-os (System/getProperty "os.name"))
   :pid (util/catching (common/get-cmd-pid) nil)
   :versions {:clojure #?(:glj (str "glojure " *glojure-version*)
                          :default (clojure-version))
              :yamlscript VERSION}
   :yspath (util/catching (common/get-cmd-path) nil)})

(defn- set-var! [v value]
  (alter-var-root v (constantly value)))

(defn- bind-runtime-vars []
  (let [file (if (and *file* (not= *file* "NO_SOURCE_PATH"))
               (common/abspath *file*)
               (common/abspath "NO-NAME"))
        argv (vec (or *command-line-args* []))]
    (set-var! #'global/ARGS (coerce-args argv))
    (set-var! #'global/ARGV argv)
    (set-var! #'global/CWD (common/cwd))
    (set-var! #'global/DIR (common/dirname file))
    (set-var! #'global/ENV (into {} (System/getenv)))
    (set-var! #'global/FILE file)
    (set-var! #'global/INC (common/get-yspath file))
    (set-var! #'global/RUN (get-runtime-info))
    (set-var! #'global/VERSION VERSION)
    (global/reset-env nil)))

(defn init
  "Set up the calling namespace to run YS compiled code."
  ([] (init nil))
  ([opts]
   (when-not (find-ns 'yamlscript.glojure-runtime)
     (let [target *ns*]
       (refer-exports target)
       (refer-runtime-vars target)
       (bind-runtime-vars)
       (when-let [v (:v opts)]
         (when (not= v VERSION)
           (binding [*out* *err*]
             (println (str "WARNING: code compiled by ys " v
                        " running with ys.v0 " VERSION)))))))
   nil))

(comment
  )

