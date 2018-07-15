(ns life.core
    (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

(def shifts
  [[-1 -1]
   [-1 0]
   [-1 1]
   [0 -1]
   [0 1]
   [1 -1]
   [1 0]
   [1 1]])
 
(defn ->point
  [{:keys [cols rows]} [i j] [inci incj]]
  (let [new-i (+ i inci)
        new-j (+ j incj)
        i*  (if (< new-i 0) (+ rows new-i) new-i)
        j*  (if (< new-j 0) (+ cols new-j) new-j)]
    (int (+ (* (mod i* rows) cols) (mod j* cols)))))
 
; (defn neighbors
;   [{:keys [cols rows] :as state} cell]
;   (let [i (quot cell cols)
;         j (- cell (* i cols))]
;    (map #(->point state [i j] %) shifts)))

(def neighbors*
  (atom {}))

(defn neighbors
  [{:keys [cols rows] :as state} cell]
  (if-let [value (get @neighbors* cell)]
    value
    (let [i (quot cell cols)
          j (- cell (* i cols))
          r (map #(->point state [i j] %) shifts)]
      (swap! neighbors* assoc cell r)
      r)))
 
(defn random-cell []
  (> (rand) 0.6))
 
(defn ->state
  [rows cols]
  (let [cells (->> (take (* rows cols) (repeatedly random-cell))
                   (into []))]
    {:rows rows :cols cols :cells cells}))

(defn ->stable 
  [rows cols]
  (let [cells [true true false false false false false false false false
               true true false false false false false false false false
               false false false false false false false false false false
               false false false false false false false false false false
               false false false false false false false false false false
               false false false false false false false false false false
               false false false false false false false false false false
               false false false false false false false false false false
               false false false false false false false false false false
               false false false false false false false false false false]]
    {:rows rows :cols cols :cells cells}))

(defn ->blink
  [rows cols]
  (let [cells [false true false false false false false false false false
                false true false false false false false false false false
                false true false false false false false false false false
                false false false false false false false false false false
                false false false false false false false false false false
                false false false false false false false false false false
                false false false false false false false false false false
                false false false false false false false false false false
                false false false false false false false false false false
                false false false false false false false false false false]]
    {:rows rows :cols cols :cells cells}))

(defn ->glider
  [rows cols]
  (let [cells [false true false false false false false false false false
                false false true false false false false false false false
                true true true false false false false false false false
                false false false false false false false false false false
                false false false false false false false false false false
                false false false false false false false false false false
                false false false false false false false false false false
                false false false false false false false false false false
                false false false false false false false false false false
                false false false false false false false false false false]]
    {:rows rows :cols cols :cells cells}))

(defn alive-around
  [{:keys [cells] :as state} idx]
  (->> (neighbors state idx)
       (map #(nth cells %))
       (filter true?)
       count))

(defn alive?
  [state idx alive]
  (if alive
    (let [alive-around* (alive-around state idx)]
      (or (= alive-around* 2) (= alive-around* 3)))
    false))

(defn risen?
  [state idx alive]
  (and (not alive)
    (= 3 (alive-around state idx))))

(defn update-state
  [{:keys [cells rows cols] :as state}]  
  (let [new-cells (map-indexed #(or (alive? state %1 %2) (risen? state %1 %2)) cells)]
    (assoc state :cells new-cells)))

(defonce app-state 
  (atom (-> (->state 20 20)
            (assoc :step 0)
            (assoc :timer nil))))

(defn grid [width height]
  [:div.container
    [:h1.title "Conway's Game of Life"]
    (doall 
      (for [i (range height)]
        [:div.row {:key (str i)}
          (doall 
            (for [j (range width)]
              (let [cell (nth (:cells @app-state) (+ (* i width) j))  
                    class (if cell "alive" "dead")]
                [:div.cell {:key (str i j) 
                            :class class}])))]))])

(defn start-timer []
  (let [timer (js/setInterval
                (fn [] 
                  (let [new-state (update-state @app-state)
                        new-step (inc (:step @app-state))]
                    (reset! app-state new-state)
                    (swap! app-state assoc :step new-step)))
                100)]
    (swap! app-state assoc :timer timer)))

(defn stop-timer []
  (js/clearInterval (:timer @app-state))
  (swap! app-state assoc :timer nil))

(defn hello-world []
  (let [width  (:cols @app-state)
        height (:rows @app-state)
        step (:step @app-state)]
    [:div.container {:style {:margin-top 10}}
      [grid width height]    
      [:div.container {:style {:margin-top 10}}
        [:h2 "Step:" (str step)]
        [:input.slider {:type "range" :step "1" :min "0" :max "100" :value step}]
        [:div.container
          [:input {:type "button" :value "Start" :on-click #(start-timer)}]
          [:input {:type "button" :value "Stop" :on-click #(stop-timer)}]]]]))
                        
(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))

(defn on-js-reload [])
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)