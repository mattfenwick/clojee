(ns clojee.core)

; syntax
;   (let* [a v1 b v2 c v3] form)
;   (do form1 form2 form3)
;   (fn* [a b c] form)
;   (def q form)
;   (set! x form)
;   (loop* [x v1 y v2] form_including_recur)
;       - no recur: warning, suggest let*
;       - nested loops: warning, recur may not work as expected
;       - duplicate var names: error
;   (if pred then else)
;     (if true  a b) -> a
;     (if false a b) -> b

(defn make-let
  [bindings body]
  {:type "let*"
   :bindings bindings
   :body body})

(defn make-do
  [forms]
  {:type "do"
   :forms forms})

(defn make-fn
  [symbols body]
  {:type "fn*"
   :params symbols
   :body body})

(defn make-def
  [symbol value]
  {:type "def"
   :symbol symbol
   :value value})

(defn make-set!
  [symbol value]
  {:type "set!"
   :symbol symbol
   :value value})

(defn make-if
  [pred then else]
  {:type "if"
   :pred pred
   :then then
   :else else})

(defn make-loop
  [bindings form]
  {:type "loop"
   :bindings bindings
   :form form})

(defn make-app
  [f args]
  {:type "application"
   :function f
   :arguments args})


(def eg1
  [(make-def 'id (make-fn '(x) 'x))
   (make-app 'f '[x y])
   (make-let '[[f g] [x z]] (make-app 'f '[x y]))
   (make-def '. (make-fn '(f g x) '(f (g x))))
   (make-def 'd (make-fn '(x) '(x x)))
   (make-def 't (make-fn '(f x) '(. f f x)))])


(defn my-resolve
  [sym env]
  (cond 
    (nil? env) nil
    (contains? (env :bindings) sym) (env :depth)
    :else (recur sym (env :parent))))

(defn new-env
  [bindings old-env]
  (doseq [x bindings]
    (or (symbol? x)
        (throw (new Exception "new-env: requires symbols for bindings"))))
  {:bindings bindings
   :parent old-env
   :depth (+ 1 (:depth old-env))})


(defn f-symbol
  [node log env]
  (cons {:symbol node, 
         :type "symbol", 
         :resolution (my-resolve node env)}
        log))

(defn f-def
  [node log env]
  ; todo: ... everything! ...
  log)

(defn m-seq
  [nodes log env]
  (loop [log-n log node-n nodes]
    (if (empty? node-n)
        log-n
        (recur (f-node (first node-n) log-n env) (rest node-n)))))

(defn f-let
  "recurs on: value of each binding, form"
  ; todo: unique symbols?
  [node log env]
  (let [log-2 (m-seq (map second (node :bindings)) log env)]
    (f-node (node :body)
            log-2
            (new-env (apply hash-set (map first (node :bindings))) env))))

(defn f-app
  [node log env]
  (let [log2 (f-node (node :function) log env)]
    (m-seq (node :arguments) log2 env)))

(def actions
 {"symbol"      f-symbol
  "def"         f-def
  "application" f-app
  "let*"        f-let})

(defn my-type
  [node]
  (cond
    (symbol? node) "symbol"
    (map? node) (node :type)
    :else (throw (new Exception (str "unrecognized node -- " (if (nil? node) "nil" node))))))

(defn f-node
  [node log env]
  (do (prn (str "checking ..." node " in " env)))
  (let [type (my-type node)]
    (if (contains? actions type)
        ((actions type) node log env)
        (throw (new Exception (str "unrecognized node type -- " node))))))


(def root-env (new-env #{'z} {:depth 0}))

(defn run-eg
  ([] (run-eg (second eg1)))
  ([node] (run-eg node '() root-env))
  ([node log env] 
   (doseq [x (f-node node log env)]
     (prn x))))

