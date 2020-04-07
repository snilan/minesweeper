(ns minesweeper.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [clojure.set :as cljset]
            [clojure.string :as str]))



;; STATE DECLARATIONS

(defonce board (r/atom [[]]))
(defonce running (r/atom false))
(defonce level-selected (r/atom :medium))
(defonce timer (r/atom 0))
(defonce game-ready (r/atom true))

(defonce default-levels {
   :easy {
      :height 8
      :width 8
      :bomb-count 10
    }
   :medium {
      :height 16
      :width 16
	    :bomb-count 40
    }
   :hard {
      :height 16
	    :width 30
      :bomb-count 99
	}
})


;; HELPER FUNCTIONS


(defn num-bombs [b]
  (->> b flatten (filter :bomb) count))

(defn num-flags [b]
  (->> b flatten (filter :flag) count))

(defn bombs-remaining [b]
  (- (num-bombs b) (num-flags b)))

(def flag-image
  [:img {:src "flag.svg"}])

(def bomb-image
  [:img {:src "flag.svg"}])

(defn make-cell [y x clicked flag bomb bomb-neighbors]
  {
    :y y
    :x x
    :flag flag
    :bomb bomb
    :bomb-neighbors bomb-neighbors})

(defn valid? [b [y x]]
  (let [bh (count b) bw (count (first b))]
    (and (< -1 y bh) (< -1 x bw))))


(defn neighbors [b y x]
  (filter
    (partial valid?	b)
    (map #(map + % [y x])
         [[-1 0] [-1 1] [0 1] [1 1] [1 0] [1 -1] [0 -1] [-1 -1]])))


(defn place-bomb [b y x]
  "Place a bomb at a certain position on the board. Cannot be removed
  Also updates neighbors"
  (let [ nbrs 	(neighbors b y x)
		     brd		(assoc-in b [y x :bomb] true)]
    (reduce
      (fn [bx [i j]]
        (update-in bx [i j :bomb-neighbors] inc))
      brd nbrs)))


(defn place-bombs [b n]
  (let [h (count b) w (count (first b))
        locs (take n (shuffle (for [i (range h) j (range w)] (vector i j))))]
    (reduce #(apply place-bomb % %2) b locs)))


;; factory function
(defn new-board [h w]
  (vec
    (for [y (range h)]
      (vec
        (for [x (range w)]
          (make-cell y x false false false 0))))))


(defn new-game [level]
  (let [{:keys [height width bomb-count]} (default-levels level)]
    (place-bombs
      (new-board height width)
      bomb-count)))


(defn bomb-neighbors [b y x]
  (:bomb-neighbors (get-in b [y x])))

(defn flag-neighbors [b y x]
  "Returns an integer representing the number of borderingi flags"
  (count
    (filter
      #(:flag (get-in b %))
      (neighbors b y x))))


(defn click [b y x]
  "Mark a position as clicked. Cannot be unclicked"
  (assoc-in b [y x :clicked] true))


(defn clear-path [b y-start x-start]
  (let [
        to-click
        (loop [[[y x] & more :as all] [[y-start x-start]] seen #{[y-start x-start]}]
          (if (seq all)
            (let [dont-click #(or (seen %)
                                  (:clicked (get-in b %))
                                  (:flag (get-in b %)))
                  n (remove dont-click (neighbors b y x))
                  flagn (flag-neighbors b y x)
                  bombn (get-in b [y x :bomb-neighbors])]
              (if (and (zero? flagn) (zero? bombn))
                (recur (concat more n) (cljset/union seen (set n)))
                (recur more seen)))
          seen))]
    (println "y = " y-start "x = " x-start "to-click = " to-click)
    (reduce #(apply click % %2) b to-click)))



(defn reveal-all-squares [b]
  (vec (for [row b]
    (vec (map #(assoc % :clicked true) row)))))

(defn game-won? [b]
  "Success if every bomb is flagged and every square with flag has a bomb"
  (let [squares (flatten b)]
    (=
      (set (filter :bomb squares))
      (set (filter :flag squares)))))


(defn check-for-win [b]
  (if (game-won? b)
    (do
      (js/alert "CONGRATS YOU WIN")
      (swap! board reveal-all-squares)
      (reset! running false))))


(defn game-over []
  (println "Oops! You clicked a bomb! Game over")
  (assert (= @running true))
  (reset! running false)
  (js/alert "GAME OVER"))


(defn reset-game []
  (do
    (reset! running false)
    (reset! timer 0)
    (reset! board (new-game @level-selected))
    (reset! game-ready true)))




;; EVENT CALLBACK FUNCTIONS


(defn on-level-change [e]
  (reset! level-selected (-> e .-target .-value keyword))
  (if (and @game-ready (not @running))
    (reset! board (new-game @level-selected))))

(defn start-timer []
  (if @running
    (js/setTimeout
      #(if @running (do
                      (swap! timer inc)
                      (start-timer)))
      1000)))


(defn on-cell-click [y x e]
  (let [cell (get-in @board [y x])]
    (if (and (not @running) @game-ready)
      (do
        (reset! running true)
        (reset! game-ready false)
        (start-timer)))
    (cond
      (not @running) nil
      (:clicked cell) nil
      (:flag cell) nil
      (:bomb cell) (game-over)
      :else (swap! board clear-path y x))))

(defn on-toggle-flag [y x e]
  (let [cell (get-in @board [y x])]
    (if (and (or @running @game-ready)
        (not (:clicked cell)))
    (do
      (swap! board update-in [y x :flag] not)
      (check-for-win @board)))
    (.preventDefault e)))


;; REAGENT COMPONENTS


(defn game-levels [levels selected]
  [:div#game-level-container
    (for [level (keys levels)]
      ^{:key (str "level" level)}
        [:label (name level)
        [:input
          { :type :radio
            :class :level-radio-button
            :name :level
            :value level
            :checked (= selected level)
            :on-change on-level-change}]])])



(defn game-cell [{:keys [y x bomb flag clicked bomb-neighbors]}]
  [:td
     {:class [:game-cell (if clicked :clicked) (if flag :flag)]
      :on-click (partial on-cell-click y x)
      :on-context-menu (partial on-toggle-flag y x)}
   (cond
       (and (.log js/console (str y "," x " was updated")) false) nil
       (and clicked bomb) "uhoh"
       (and clicked (zero? bomb-neighbors)) " "
       clicked bomb-neighbors
       flag flag-image
       :else "")])


(defn game-board [b]
  [:table.game-board-container
     (for [row @board]
       ^{:key (str "row" (:y (first row)))}
       [:tr {:class :game-row}
          (for [cell row]
              ^{:key (str "cell" (:y cell) "," (:x cell))}
              [game-cell cell])
          [:br]])])

(defn game-info []
  [:div.game-info-container
    [:div "Bomb count: " [:span (bombs-remaining @board)]]
    [:button {:on-click reset-game} "Reset"]
    [:label "Timer :" @timer]
    [game-levels default-levels @level-selected]])


(defonce init
    (do
      (reset! board (new-game @level-selected))))

(defn minesweeper-app []
  [:div
    [game-info]
    [game-board @board]])


(defn ^:export run []
  (rdom/render [minesweeper-app] (js/document.getElementById "app")))

