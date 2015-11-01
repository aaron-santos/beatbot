(ns beatbot.core
  #_(:require [overtone.live :as o]
            [shadertone.tone :as t])
  (:gen-class))

(set! *warn-on-reflection* true)

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
     (when (< ~sym num-total-neurons)
       ~@body
       (recur (inc ~sym)))))
         

(defn -main [& args]
  ;; Adapted from Izhikevich
  (let [rand-excitatory (map (fn [_] (rand)) (range num-excitatory-neurons))
      rand-inhibitory (map (fn [_] (rand)) (range num-excitatory-neurons))
      a               (concatv (map #(* 0.02 %) (repeat num-excitatory-neurons 1))
                               (map #(+ 0.02 (* 0.08 %)) (repeat num-inhibitory-neurons 1)))
      b               (concatv (map #(* 0.2 %) (repeat num-excitatory-neurons 1))
                               (map #(- 0.25 (* 0.05 %)) (repeat num-inhibitory-neurons 1)))
      c               (concatv (map #(+ -65 (* 15 % %)) rand-excitatory)
                               (map #(* -65 %) (repeat num-inhibitory-neurons 1)))
      d               (concatv (map #(- 8 (* 6 % %)) rand-excitatory)
                               (map #(* 2 %) (repeat num-inhibitory-neurons 1)))
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
      Se (mapv (fn [_]
                 (double-array
                   (map (fn [_] (rand 0.5))
                         (range num-total-neurons))))
                (range num-excitatory-neurons))
      Si (mapv (fn [_]
                 (double-array
                   (map (fn [_] (- (rand)))
                         (range num-total-neurons))))
               (range num-inhibitory-neurons))
       get-row (fn [^long idx]
                 (if (> idx num-excitatory-neurons)
                   (nth Si (- idx num-excitatory-neurons))
                   (nth Se idx)))
       vs (double-array (repeat num-total-neurons -65))
       us (double-array (map * vs b))
       I  (double-array (make-array Double/TYPE num-total-neurons))
       ni (range num-total-neurons)]
  (time
    (loop [t       0
           ;; nx2
           ;; TODO: when performing reinforcement learning, use firing for eligibility trace calculations
           firings []] ;; nx2
           ;; 1000x1
      (if (> t 1000)
        nil
        (let [t (inc t)
              ;; thalmic input
              ;_ (print "I thalmic ")
              _ (time1 (loop-neurons idx
                  (aset-double I idx 
                               (if (< idx num-excitatory-neurons)
                                 (* 5.0 (randn))
                                 (* 2.0 (randn))))))
                    
              ;_ (print "fired ")
              fired (time1
                      (persistent!
                        (areduce vs idx ret (transient #{})
                               (if (>= (aget vs idx) 30)
                                 (do
                                   #_(println "firing" idx "@" t)
                                   (conj! ret idx))
                                 ret))))
              ;_ (println "n-fired" (count fired))
              ;_ (print "firings ")
              ;firings (time1 (concatv firings (map (fn [i] [t i]) fired)))
              #_#__ (println "v[0,4]" (take 4 v))
              #_#__ (println "v[0,4]" (take 4 v))
              ;_ (print "vus0 ")
              _ (time1 (loop-neurons idx
                  (when (contains? fired idx)
                    (let [u (aget us idx)
                          row ^doubles (get-row idx)
                          sum ^double (areduce row si ret 0.0 (+ ret (aget row si)))]
                      (aset-double vs idx (nth c idx))
                      (aset-double us idx (+ u (nth d idx)))
                      (aset-double I idx (+ (aget I idx)
                                              sum))))
                  (let [I (aget I idx)
                        a (nth a idx)
                        b (nth b idx)
                        v (aget vs idx)
                        u (aget us idx)
                        v (+ v (* 0.5 (+ (* 0.04 v v) (* 5 v) 140 (- u) I)))
                        v (+ v (* 0.5 (+ (* 0.04 v v) (* 5 v) 140 (- u) I)))
                        u (+ u (* a (- (* b v) u)))]
                    (aset-double vs idx v)
                    (aset-double us idx u))))]
                  
          #_(println "I[0,4]" (take 4 I))
          #_(println "v[0,4]" (take 4 v))
          ;(println "t =" t)
          (recur t firings)))))))
       
