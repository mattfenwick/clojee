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

(defn symbol-check
  [sym]
  (and (not (symbol? sym))
       (throw (new Exception (str "expected symbol, got: " sym)))))

(defn make-bindings
  [bindings]
  (if (not (= 0 (mod (count bindings) 2))) ; just for the side effects
      (throw (new Exception "even number of bindings required")))
  (let [bs (partition 2 bindings)]
    (doseq [[sym _] bs]
           (symbol-check sym))
    bs))
  

(defn make-let
  [bindings body]
  (list 'let*
        (make-bindings bindings)
        body))

(defn make-do
  [forms]
  forms)

(defn make-fn
  [symbols body]
  (doseq [x symbols] (symbol-check x))
  (list 'fn* symbols body))

(defn make-def
  [symbol value]
  (symbol-check symbol)
  (list 'def symbol value))

(defn make-set!
  [symbol value]
  (symbol-check symbol)
  (list 'set! symbol value))

(defn make-if
  [pred then else]
  (list 'if pred then else))

(defn make-loop
  [bindings form]
  (list 'loop* 
        (make-bindings bindings)
        form))


(def eg1
  [(make-def 'id (make-fn '(x) 'x))
   (make-def '. (make-fn '(f g x) '(f (g x))))
   (make-def 'd (make-fn '(x) '(x x)))
   (make-def 't (make-fn '(f x) '(. f f x)))])


(defn my-resolve
  [sym env]
  ((env :specials) sym))

(defn f-symbol
  [node log env]
  (cons {:symbol node, 
         :type "symbol", 
         :resolution (my-resolve node env)}
        log))

(defn f-list
  [node log env]
  (loop [log-n (cons {:type "list" :length (count node)} log); :env env} log)
         node-n node]
        (if (empty? node-n)
            log-n
            (recur (f-node (first node-n) log-n env)
                   (rest node-n)))))

(defn f-node
  [node log env]
  (if (symbol? node)
      (f-symbol node log env)
      (f-list node log env)))


(def specials
  {'if "yes!"
   'def "yes!"
   'fn* "yes!"
   'let* "yes!"
   'loop* "yes!"
   'do "yes!" 
   'set! "yes!"})
   
(def root-env {:specials specials})

(defn run-eg
  ([] (run-eg '() root-env))
  ([log env] 
   (doseq [x (f-node (second eg1) log env)]
     (prn x))))

