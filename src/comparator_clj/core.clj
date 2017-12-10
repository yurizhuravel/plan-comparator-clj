(ns comparator-clj.core

  (:require
  [clojure.data.json :as json]
  [clojure.string :as string]
  [clojure.java.io :as io]
  [clojure.walk :as walk]
  [taoensso.timbre :as log]))

(def plans-list "resources/plans.json")
(def inputs "resources/inputs")
(def output "resources/output")
(def plan-number-atom (atom 0))

(defn parse-plans [json]
  (let [plans (json/read-str (slurp json))]
    (walk/keywordize-keys plans)))

(def parsed-plans (parse-plans plans-list))

(defn parse-inputs [inputs]
  ;(apply hash-map )
  (string/split-lines (slurp inputs)))

(defn output-to-file [line-to-add]
  (spit output line-to-add :append true))

(defn calculate-price [plan-number usage]
  (if (= @plan-number-atom (count parsed-plans))
    (reset! plan-number-atom 0))
  (do (swap! plan-number-atom inc)
      (let [evaluated-plan (nth parsed-plans (- @plan-number-atom 1))
            rates (get evaluated-plan :rates)
            calculation (atom 0)]
        (do
          (log/debug (get (first rates) :price :threshold))
          (if (= (count rates) 1)
            "one rate")
          ;  (reset! calculation (+ @calculation (* (get rates :price) usage))))
          (if (contains? evaluated-plan :standing_charge)
            (reset! calculation (+ @calculation (* (get evaluated-plan :standing_charge) 365)))
            @calculation)))))

(defn output-price [parsed-plans usage]
  (if (seq parsed-plans)
    (let [evaluated-plan (first parsed-plans)
          supplier (evaluated-plan :supplier)
          plan (evaluated-plan :plan)
          rates (evaluated-plan :rates)]
      (do
        (output-to-file (str supplier ","))
        (output-to-file (str plan ","))
        (output-to-file (str (calculate-price @plan-number-atom usage) "\n"))
        (output-price (rest parsed-plans) usage)))))

(defn show-results []
  (println (slurp output)))

(defn exit []
  (show-results)
  (io/delete-file output)
  (log/info "Exiting now, thank you for using the comparator!")
  (System/exit 0))

(defn handle-inputs [parsed-inputs]
  (let [evaluated-string (first parsed-inputs)
        command (first (string/split evaluated-string #"\s+"))]
        ;usage-if-rates ]
    (if (seq parsed-inputs)
      (do (case command
            "exit" (exit)
            "price" (let [usage (Integer/parseInt (last (string/split evaluated-string #"\s+")))]
                      (output-price parsed-plans usage))
            "usage" (output-to-file "evaluating usage\n")
            (output-to-file "no valid commands supplied"))
          (handle-inputs (rest parsed-inputs))))))

(defn -main [& args]
  ;(parse-plans plans-list)
  ;(log/debug (parse-inputs inputs))
  (handle-inputs (parse-inputs inputs)))

;(defn parse-inputs [inputs]
;  (with-open [rdr (clojure.java.io/reader inputs)]
;    (line-seq rdr)))