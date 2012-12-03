(ns learning.BitcoinCheck
  (:require [clj-http.client :as client])
  (:use [clojure.data.json :only (read-json)])
  (:import [org.apache.commons.mail SimpleEmail])
  (:gen-class))

(def addresses ["1AKCUiAPdGkj1fi38csdeSqYo5kkPrsACa"
                "179M77z5W1Fd2H1wFSW17v1zKMLWPJ14gj"
                "1G7wrY4fHgLMwfme5HKpMCVWRxfDsY8J8z"])

(def email-address "opzk2dh0983@gmail.com")

(def email-password "0Dsm3U'0~K(Raq9[Mzgz% mzI-3Sn]r)eM46ZC[")

(defn- blockchain-url [address]
  (str "https://blockchain.info/address/" address))

(defn- blockchain-json-url [address]
  (str (blockchain-url address) "?format=json"))

(defn- send-email! [subject message]
  (doto (SimpleEmail.)
    (.setHostName "smtp.gmail.com")
    (.setSslSmtpPort "465")
    (.setSSL true)
    (.addTo "avi.rei@gmail.com")
    (.setFrom email-address)
    (.setSubject (str "TZBITBIT-" subject))
    (.setMsg message)
    (.setAuthentication email-address email-password)
    (.send)))

(defn- notify-me! [address]
  (let [message (str "check out address " address ", at: "
                     (blockchain-url address))]
    (send-email! "ERROR" message)))

(defn- report-check-carried-out! []
  (send-email! "OK" "wasn't that easy?"))

(defn- report-exception! [error-message]
  (send-email! "ERROR" error-message))

(defn -main [& args]
  (doseq [address addresses]
    (do (println "looking at" address)
        (let [url (blockchain-json-url address)
              string-json (:body (client/get url))
              data (try (read-json string-json)
                        (catch Exception e
                          (report-exception! (str (.getMessage e)))))]
          (if (and (map? data)
                   (= (:address data) address)
                   (>= (:final_balance data) 2667000100))
            (println "final balance is" (/ (:final_balance data) 1E8))
            (do (notify-me! address)
                (keyword (str "failed-" address)))))))
  (report-check-carried-out!))
