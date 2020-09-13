(ns blog-sketch.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def palete
  {:yellow [255 246 220]
   :gold [228 183 71]
   :dark-red [122 98 99]
   :salmon [255 204 178]
   :sapphire [6 90 130]})


(defn pt-dist [pt1 pt2]
  (Math/sqrt (+ (Math/pow (- (:x pt1) (:x pt2)) 2)
                (Math/pow (- (:y pt1) (:y pt2)) 2))))

(defn make-star [r c max-r max-c]
  (let [ox (* c 100)
        oy (* r 100)
        make-key (fn [id] (str r "-" c "-" id))
        nodes (into {} (map (fn [[id {:keys [x y static]}]]
                              [(make-key id) {:x (+ x ox) :y (+ y oy) :vx 0 :vy 0 :static static}])
                            {0 {:x 8 :y 0 :static (= r 0)}
                             1 {:x 50 :y 0 :static (= r 0)}
                             2 {:x 92 :y 0 :static (= r 0)}

                             3 {:x 0 :y 10 :static (= c 0)}
                             4 {:x 100 :y 10 :static (= c (dec max-c))}

                             5 {:x 50 :y 29}

                             6 {:x 35 :y 35}
                             7 {:x 66 :y 35}

                             8 {:x 0 :y 50 :static (= c 0)}
                             9 {:x 30 :y 50}
                             10 {:x 70 :y 50}
                             11 {:x 100 :y 50 :static (= c (dec max-c))}

                             12 {:x 35 :y 65}
                             13 {:x 65 :y 65}

                             14 {:x 50 :y 71}

                             15 {:x 0 :y 90 :static (= c 0)}
                             16 {:x 100 :y 90 :static (= c (dec max-c))}

                             17 {:x 8 :y 100 :static (= r (dec max-r))}
                             18 {:x 50 :y 100 :static (= r (dec max-r))}
                             19 {:x 92 :y 100 :static (= r (dec max-r))}}))]
    {:nodes nodes
     :edges (map (fn [e]
                   (-> e
                       (update :a make-key)
                       (update :b make-key)
                       (assoc :len (pt-dist (get nodes (make-key (:a e)))
                                            (get nodes (make-key (:b e)))))))
             [{:a 0 :b 9 :len (pt-dist (get nodes 0) (get nodes 9))}
              {:a 1 :b 6 :len (pt-dist (get nodes 1) (get nodes 6))}
              {:a 1 :b 7 :len (pt-dist (get nodes 1) (get nodes 7))}
              {:a 2 :b 10 :len (pt-dist (get nodes 2) (get nodes 10))}

              {:a 3 :b 5 :len (pt-dist (get nodes 3) (get nodes 5))}
              {:a 4 :b 5 :len (pt-dist (get nodes 4) (get nodes 5))}

              {:a 6 :b 8 :len (pt-dist (get nodes 6) (get nodes 8))}
              {:a 7 :b 11 :len (pt-dist (get nodes 7) (get nodes 11))}

              {:a 8 :b 12 :len (pt-dist (get nodes 8) (get nodes 12))}
              {:a 9 :b 17 :len (pt-dist (get nodes 9) (get nodes 17))}
              {:a 10 :b 19 :len (pt-dist (get nodes 10) (get nodes 19))}
              {:a 11 :b 13 :len (pt-dist (get nodes 11) (get nodes 13))}

              {:a 12 :b 18 :len (pt-dist (get nodes 12) (get nodes 18))}
              {:a 13 :b 18 :len (pt-dist (get nodes 13) (get nodes 18))}

              {:a 14 :b 15 :len (pt-dist (get nodes 14) (get nodes 15))}
              {:a 14 :b 16 :len (pt-dist (get nodes 14) (get nodes 16))}])}))

(defn replace-node [{:keys [edges nodes]} from-id to-id]
  (let [nodes (-> nodes
                  (dissoc from-id))
        edges (map (fn [{:keys [a b len]}]
                     (cond
                       (= a from-id) {:a to-id :b b :len len}
                       (= b from-id) {:a a :b to-id :len len}
                       :else {:a a, :b b, :len len})) edges)]
    {:nodes nodes
     :edges edges}))

(defn setup []
  (q/frame-rate 30)
  (let [rows 4
        cols 8
        make-key (fn [a b id] (str a "-" b "-" id))
        stars (for [a (range rows)
                    b (range cols)]
                (make-star a b rows cols))
        conn-stars (reduce (fn [stars [a b]]
                             (-> stars
                                 (replace-node (make-key (+ a 1) b 0) (make-key a b 17))
                                 (replace-node (make-key (+ a 1) b 1) (make-key a b 18))
                                 (replace-node (make-key (+ a 1) b 2) (make-key a b 19))
                                 (replace-node (make-key a (+ b 1) 3) (make-key a b 4))
                                 (replace-node (make-key a (+ b 1) 8) (make-key a b 11))
                                 (replace-node (make-key a (+ b 1) 15) (make-key a b 16))))
                           {:edges (apply concat (map :edges stars))
                            :nodes (apply merge (map :nodes stars))}
                           (for [a (range rows), b (range cols)] [a b]))]
    conn-stars))

(defn partner-points [id edges nodes]
  (filter identity
          (for [{:keys [a b len]} edges]
            (cond (= a id) (assoc (get nodes b) :len len)
                  (= b id) (assoc (get nodes a) :len len)
                  :else nil))))

(defn make-unit [[x y]]
  (let [w (pt-dist {:x x :y y} {:x 0 :y 0})]
    [(/ x w) (/ y w)]))

(defn dampen [v] (* v 0.05))

(defn mouse-force [pt]
  (if (q/mouse-pressed?)
    (let [dir (if (= (q/mouse-button) :left) 1 -1)
          [mx my] [(q/mouse-x) (q/mouse-y)]
          [x y] pt
          dist (pt-dist {:x x :y y} {:x mx :y my})
          str (* (/ (- 1 (/ 800 dist)) 1000) dir)]
      [(* (- x mx) str) (* (- y my) str)])
    (let [[mx my] [(q/mouse-x) (q/mouse-y)]
          [x y] pt
          dist (pt-dist {:x x :y y} {:x mx :y my})
          str (/ (- 1 (/ 50 dist)) -30)]
      (if (< dist 50)
        [(* (- x mx) str) (* (- y my) str)]
        [0 0]))))

(defn graph-move [edges nodes [id node]]
  (if (:static node)
    [id node]
    (let [partners (partner-points id edges nodes)
          [dx dy] (reduce (fn [[dx dy] partner]
                            (let [dist (pt-dist partner node)
                                  m (/ (- dist (:len partner)) 100)
                                  m (if (< 0 m) (max 0.1 m) (* 2 m))
                                  [parx pary] (make-unit [(- (:x partner) (:x node))
                                                          (- (:y partner) (:y node))])
                                  dmpx (dampen (:vx node))
                                  dmpy (dampen (:vy node))]

                              [(+ dx (* m parx) (* -1 dmpx))
                               (+ dy (* m pary) (* -1 dmpy))]))
                          [0 0]
                          partners)
          [mfx mfy] (mouse-force [(:x node) (:y node)])]
      [id {:vx (+ (:vx node) dx mfx)
           :vy (+ (:vy node) dy mfy)
           :x (+ (:x node) (:vx node))
           :y (+ (:y node) (:vy node))}])))

(defn slide-move [[id node]]
  [id {:x (+ (:x node) 1)
       :y (:y node)}])


(defn update-state [{:keys [edges nodes] :as state}]
  ;; Update sketch state by changing circle color and position.
  (let [new-nodes (map (partial graph-move edges nodes) nodes)]
    {:nodes (into {} new-nodes)
     :edges edges}))

(defn draw-state [state]
  (q/background 255)
  (q/fill 0 0 0)
  (apply q/fill (:sapphire palete))
  (apply q/stroke (:sapphire palete))
  (doseq [{:keys [a b len]} (:edges state)]
    (let [pt1 (get (:nodes state) a)
          pt2 (get (:nodes state) b)]
      (when (and pt1 pt2)
        (let [stretch (Math/abs (- len (pt-dist pt1 pt2)))
              [r g b] (:sapphire palete)]
          (q/stroke (+ r (* stretch 10)) (- g (* stretch 2)) (- b (* stretch 10)))
          (q/line (:x pt1) (:y pt1) (:x pt2) (:y pt2))))))
  (q/no-stroke)
  (doseq [[id {:keys [x y]}] (:nodes state)]
    (q/ellipse x y 4 4))
  (apply q/stroke (:sapphire palete)))

; this function is called in index.html
(defn ^:export run-sketch []
  (q/defsketch blog-sketch
    :host "blog-sketch"
    :size [800 400]
    ; setup function called only once, during sketch initialization.
    :setup setup
    ; update-state is called on each iteration before draw-state.
    :update update-state
    :draw draw-state
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode]))

; uncomment this line to reset the sketch:
; (run-sketch)
