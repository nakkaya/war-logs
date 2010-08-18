(ns war-logs.core
  (:use [incanter core charts excel])
  (:import (org.jfree.chart.axis CategoryLabelPositions)
	   (javax.swing JFrame JLabel)
	   (java.io File)
	   (javax.imageio ImageIO)))

(def total-casualties (read-xls "resources/war-log.xls" :sheet 0))
(def ied-month (read-xls "resources/war-log.xls" :sheet 1))
(def ied-region (read-xls "resources/war-log.xls" :sheet 2))
(def lat-long-deaths (read-xls "resources/war-log.xls" :sheet 3))
(def lat-long-woundings (read-xls "resources/war-log.xls" :sheet 4))

;;(view total-casualties)
;;(view ied-month)
;;(view ied-region)
;;(view lat-long-deaths)

(def taliban-casualties 
     (doto (line-chart (sel total-casualties :cols 1)
		       (sel total-casualties :cols 2)
		       :group-by (sel total-casualties :cols 0)
		       :title "Casualties"
		       :legend true
		       :y-label "Taliban"
		       :x-label "Year")
       (-> .getPlot .getDomainAxis (.setCategoryLabelPositions 
				    CategoryLabelPositions/UP_90))))

;;(view taliban-casualties :size [650 400])

(def all-casualties  
     (let [sum-not-nil #(apply + (filter (fn[v] (not (nil? v))) %))
	   taliban (sum-not-nil (sel total-casualties :cols 2))
	   civilian (sum-not-nil (sel total-casualties :cols 3))
	   afghan (sum-not-nil (sel total-casualties :cols 4))
	   nato (+ (sum-not-nil (sel total-casualties :cols 5))
		   (sum-not-nil (sel total-casualties :cols 6)))]
       (pie-chart ["Taliban" "Civilians" "Afghan Forces" "Nato Forces"]
		  [taliban civilian afghan nato]
		  :title "All Casualties"
		  :legend true)))

;;(view all-casualties :size [650 400])

(def ied-region (let [dat (to-map ied-region)] 
		  (pie-chart (filter #(not(nil? %)) (:Region dat)) 
			     (filter #(not(nil? %)) (:Total dat))
			     :title "IED/Region"
			     :legend true)))

;;(view ied-region :size [650 400])


(defn map-number [x in-min in-max out-min out-max]
  (+ (/ (* (- x in-min) (- out-max out-min)) (- in-max in-min)) out-min))

(defn to-pixel [long lat width height]
  [(map-number long 60.0 75.3 0 width) (map-number lat 38.8 29.0 0 height)])

(defn prepare-data [data key]
  (reduce (fn[h v] 
	    (let [long (v "Longitude") lat  (v "Latitude") num (v key)]
	      (if (and (not (nil? long)) (not (nil? lat)) (> num 0))
		(conj h [long lat num]) h))) [] (:rows data)))

(defn all-deaths []
  (let [data (prepare-data lat-long-deaths "Deaths")
	max (apply max (map #(nth % 2) data))] 
    [data max]))

(defn all-woundings []
  (let [data (prepare-data lat-long-woundings "Woundings")
	max (apply max (map #(nth % 2) data))] 
    [data max]))

(defn circle [g [x y] rad]
  (let [offset (int (/ rad 2))
	x (- x offset)
	y (- y offset)]
    (.fill g (java.awt.geom.Ellipse2D$Double. x y rad rad))))

(defn draw [width height graphics data]
  (let [[data max] data
	points (map #(let [[long lat num] %]
		       [(to-pixel long lat width height) num]) data)]
    (.setColor graphics (java.awt.Color. 0 0 255 100))
    (doseq [[cord num] points]
      (circle graphics cord (map-number num 1 max 3 10)))))

(defn frame [data]
  (let [image  (ImageIO/read (File. "resources/afghan.png"))
	canvas (proxy [JLabel] [] 
		 (paintComponent [g] (.drawImage g image 0 0 this))
		 (getPreferredSize[] (java.awt.Dimension. 
				      (.getWidth image) 
				      (.getHeight image))))]
    (draw (.getWidth image) (.getHeight image) (.createGraphics image) data)
    (doto (JFrame.)
      (.add canvas)
      (.pack)
      (.show))))

;;(frame (all-deaths))
;;(frame (all-woundings))
