(ns beatbot.core
  (:require ;[overtone.core :as o]
            ;[shadertone.tone :as t]
            [clojure.core.cache :as cache])
  (:gen-class))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

#_(t/start "glsl/spectrograph.glsl"
         :title "Spectrograph"
         :width 1024 :height 512
         :textures [:overtone-audio :previous-frame])

#_(def tambourine (o/sample "sounds/tambourine3.wav"))
#_(def frame-drum-hi (o/sample "sounds/frame-drum2.wav"))
#_(def frame-drum-mid (o/sample "sounds/hand-frame-drum.wav"))
#_(def frame-drum-lo (o/sample "sounds/bodhran2.wav"))
#_(def wood-block (o/sample "sounds/woodblock3.wav"))
#_(def scraper (o/sample "sounds/scraper.wav"))

(defn randn []
  (- (rand 2) 1))

(defn concatv [v1 v2]
  (vec (concat v1 v2)))

(def time1 identity)

(def num-excitatory-neurons 800)
(def num-inhibitory-neurons 200)
(def num-total-neurons (+ num-excitatory-neurons num-inhibitory-neurons))
      

(defmacro loop-neurons
  [sym & body]
  `(loop [~sym 0]
     (when (clojure.lang.Numbers/lt ~sym (long num-total-neurons))
       ~@body
       (recur (inc ~sym)))))
         
(defmacro add*
  ([x] x)
  ([x y] `(unchecked-add ~x ~y))
  ([x y & more]
   (if (seq more)
     `(add* ~x (add* ~y ~@more))
     `(add* ~x ~y))))

(defmacro mul*
  ([x] x)
  ([x y] `(unchecked-multiply ~x ~y))
  ([x y & more]
   (if (seq more)
     `(mul* ~x (mul* ~y ~@more))
     `(mul* ~x ~y))))

(defn -main [& args]
  ;; Adapted from Izhikevich
  (let [rand-excitatory (map (fn [_] (rand)) (range num-excitatory-neurons))
        rand-inhibitory (map (fn [_] (rand)) (range num-excitatory-neurons))
        as ^doubles               (double-array
                           (concat (map #(* 0.02 %) (repeat num-excitatory-neurons 1))
                                   (map #(+ 0.02 (* 0.08 %)) (repeat num-inhibitory-neurons 1))))
        bs ^doubles              (double-array
                           (concat (map #(* 0.2 %) (repeat num-excitatory-neurons 1))
                                   (map #(- 0.25 (* 0.05 %)) (repeat num-inhibitory-neurons 1))))
        cs ^doubles              (double-array
                           (concat (map #(+ -65 (* 15 % %)) rand-excitatory)
                                   (map #(* -65 %) (repeat num-inhibitory-neurons 1))))
        ds ^doubles              (double-array
                           (concat (map #(- 8 (* 6 % %)) rand-excitatory)
                                   (map #(* 2 %) (repeat num-inhibitory-neurons 1))))
        ;; [1000x1000] random weights
        ;;    800           200
        ;; |e e e e e e e e e e|
        ;; |e e e e e e e e e e|
        ;; |e e e e e e e e e e|
        ;; |e e e e e e e e e e|
        ;; |e e e e e e e e e e|
        ;; |e e e e e e e e e e|
        ;; |e e e e e e e e e e|
        ;; |e e e e e e e e e e|
        ;; |e e e e e e e e e e|
        ;; |i i i i i i i i i i|
        ;; |i i i i i i i i i i|
        ;; row number = source neuron index
        ;; column number = output index
        ;; row slice = output weights
        Se (to-array
             (mapv (fn [_]
                   (double-array
                     (map (fn [_] (rand 0.5))
                           (range num-total-neurons))))
                  (range num-excitatory-neurons)))
        Si (to-array
              (mapv (fn [_]
                   (double-array
                     (map (fn [_] (- (rand)))
                           (range num-total-neurons))))
                 (range num-inhibitory-neurons)))
         get-row (fn ^doubles [^long idx]
                   (if (> idx num-excitatory-neurons)
                     (aget Si (- idx num-excitatory-neurons))
                     (aget Se idx)))
         vs (double-array (repeat num-total-neurons -65))
         us (double-array (map * vs bs))
         Is  (double-array (make-array Double/TYPE num-total-neurons))
         fired (boolean-array (repeat num-total-neurons false))
         t-input (double-array (repeatedly (* 2 num-total-neurons) randn))
         firings (atom (cache/ttl-cache-factory {} :ttl 1000))]
  (time
    (loop [t       0] ;; nx2
           ;; 1000x1
      (if (> t 10000)
        (println @firings)
        (let [t (inc t)
              ;; thalmic input
              ;_ (print "I thalmic ")
              t-mod ^long (mod t num-total-neurons)
              _ (time1 (loop-neurons idx
                  (aset-double Is idx 
                               (if (clojure.lang.Numbers/lt idx ^long num-excitatory-neurons)
                                 (unchecked-multiply 5.0 (aget t-input (unchecked-add idx t-mod)))
                                 (unchecked-multiply 2.0 (aget t-input (unchecked-add idx t-mod)))))))
                    
              ;_ (print "fired ")
              _ (time1 (loop-neurons idx
                  (let [idx (int idx)]
                    (if (clojure.lang.Numbers/gte (aget vs idx) 30.0)
                      (do
                        (swap! firings assoc idx t)
                        (aset-boolean fired idx true))
                      (aset-boolean fired idx false)))))
              ;_ (println "n-fired" (count fired))
              ;_ (print "firings ")
              ;firings (time1 (concatv firings (map (fn [i] [t i]) fired)))
              #_#__ (println "v[0,4]" (take 4 v))
              #_#__ (println "v[0,4]" (take 4 v))
              ;_ (print "vus0 ")
              _ (time1 (loop-neurons idx
                  (let [idx (int idx)]
                  (when (aget fired idx)
                    (let [u ^double (aget us idx)
                          row ^doubles (get-row idx)
                          sum ^double (areduce row row-idx ret 0.0 (+ ret (aget row row-idx)))]
                      (aset-double vs idx (aget cs idx))
                      (aset-double us idx (+ u (aget ds idx)))
                      (aset-double Is idx (+ (aget Is idx)
                                             sum))))
                  (let [I (aget Is idx)
                        a (aget as idx)
                        b (aget bs idx)
                        v (aget vs idx)
                        u (aget us idx)
                        v (add* v (unchecked-multiply 0.5 (add* (mul* 0.04 v v) (unchecked-multiply 5 v) 140.0 (- u) I)))
                        v (add* v (unchecked-multiply 0.5 (add* (mul* 0.04 v v) (unchecked-multiply 5 v) 140.0 (- u) I)))
                        u (add* u (unchecked-multiply a (unchecked-subtract (unchecked-multiply b v) u)))]
                    (aset-double vs idx v)
                    (aset-double us idx u)))))]
                  
          #_(println "I[0,4]" (take 4 Is))
          #_(println "v[0,4]" (take 4 vs))
          ;(println "t =" t)
          (recur t)))))))
       
