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

(defn -main [& args]
  ;; Adapted from Izhikevich
  (time
    (let [num-excitatory-neurons 800
        num-inhibitory-neurons 200
        num-total-neurons (+ num-excitatory-neurons num-inhibitory-neurons)
        rand-excitatory (map (fn [_] (rand)) (range num-excitatory-neurons))
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
        S         (vec (concat (map (fn [_]
                                      (mapv (fn [_] (rand 0.5))
                                            (range num-total-neurons)))
                                    (range num-excitatory-neurons))
                               (map (fn [_]
                                      (mapv (fn [_] (- (rand)))
                                            (range num-total-neurons)))
                                    (range num-inhibitory-neurons))))]
    (loop [t       0
           ;; nx2
           ;; TODO: when performing reinforcement learning, use firing for eligibility trace calculations
           firings [] ;; nx2
           ;; 1000x1
           v (vec (repeat num-total-neurons -65))
           ;; 1000x1
           u (mapv * v b)]
      (if (> t 1000)
        nil
        (let [t (inc t)
              ;; thalmic input
              ;_ (print "I thalmic")
              I (time1 (concatv (map (fn I_ex [_] (* 5 (randn))) (range num-excitatory-neurons))
                         (map (fn I_in [_] (* 2 (randn))) (range num-inhibitory-neurons))))
              ;_ (print "fired ")
              fired (time1 (set (reduce (fn acc-fired [acc [^long i ^double v]]
                              (if (>= v 30)
                                (do
                                  #_(println "firing" i)
                                  (conj acc i))
                                acc))
                            #{}
                            (map-indexed vector v))))
              ;_ (print "firings ")
              firings (time1 (concatv firings (map (fn [i] [t i]) fired)))
              #_#__ (println "v[0,4]" (take 4 v))
              ;_ (print "v0 ")
              v (time1 (vec (map-indexed (fn reset-v [^long i ^double v]
                               (if (contains? fired i)
                                 (nth c i)
                                 v))
                             v)))
              #_#__ (println "v[0,4]" (take 4 v))
              ;_ (print "u0 ")
              u (time1 (vec (map-indexed (fn reset-u [^long i ^double u]
                               (if (contains? fired i)
                                 (+ u (nth d i))
                                 u))
                             u)))
              ;_ (print "I ")
              I (time1 (vec (map-indexed (fn reset-I [^long i ^double I]
                               (+ I (if (contains? fired i)
                                      (apply + (nth S i))
                                      0)))
                               I)))
              ;_ (print "v1 ")
              v (time1 (mapv (fn step-v-1 [^double v ^double u ^double I]
                        (+ v (* 0.5 (+ (* 0.04 v v) (* 5 v) 140 (- u) I))))
                      v u I))
              ;_ (print "v2 ")
              v (time1 (mapv (fn step-v-2 [^double v ^double u ^double I]
                        (+ v (* 0.5 (+ (* 0.04 v v) (* 5 v) 140 (- u) I))))
                      v u I))
              ;_ (print "u1 ")
              u (time1 (mapv (fn step-u [^double u ^double v ^double a ^double b]
                        (+ u (* a (- (* b v) u))))
                      u v a b))]
          #_(println "I[0,4]" (take 4 I))
          #_(println "v[0,4]" (take 4 v))
          ;(println "t =" t)
          (recur t firings v u)))))))
       
