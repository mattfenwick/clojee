(ns clojee.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))


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

