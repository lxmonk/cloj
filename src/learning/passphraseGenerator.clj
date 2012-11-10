(ns learning.passphraseGenerator
  (:require [crypto.random :as crypto :only [hex]]
            [clojure.string :as string
             ;; :only (join lower-case split split-lines)
             ]
            [clj-http.client :as client]
            )
  (:use [clojure.main :only (repl-read)])
  (:gen-class))
 ; gen-class needed since this is the 'Main' class for the program.

(def total-passwords-printed 20 ;10000 FIXME
  )

(def random-org-url
  (str "https://www.random.org/integers/"
       "?num=4000&min=0&max=94&col=4000&base=10&format=plain&rnd=new"))

(declare diceware-passphrases short-passphrases web-passphrases get-web-random!)

(defn- random-dice-idx
  "return a (crypto) random number between 11111 and 66666 inclusive."
  []
  (let [random-number (read-string
                       (str "0x" (crypto.random/hex 4)))
        ;; convert to a base-6 string
        base-6 (Long/toString random-number 6)]
    ;; take the first 5 digits, and convert each from [0,5] to [1,6],
    ;; since the file is keyed that way (to be used with a dice).
    (str (+ (Long/parseLong (subs base-6 0 5)) 11111))))

(defn- web-passphrases
  ([random-quality _]
     (diceware-passphrases random-quality word-map 1))
  ([random-quality _ n]
     (for [_ (range n)]
       (string/join " " (for [_ (range 10)]
                          (word-map (diceware-idx random-quality))))))))
;; (defn- get-web-random!
;;   "this function has state! it gets random bits from random.org (via https),
;;    and keeps them and returns wanted random numbers on demand."
;;   ;; (letfn (random-nums! []
;;    ;;         (let [response (client/get random-org-url
;;    ;;                                    {:headers
;;    ;;                                     {"user-agent" "avi.rei@gmail.com"}})
;;    ;;               (body (string/split (:body res) #"\t"))
;;    ;;               nums (map Long/parseLong
;;    ;;                         (conj (butlast body)
;;    ;;                               (string/join (butlast (last body)))))]
;;    ;;           nums))    (
;;    ([] (let [rands (random-nums!)]
;;           (get-web-random! rands)))
;;     ([rands]
;;        (if (empty? rands)
;;          (get-web-random!)
;;          ;; we're out of random numbers.. get some.
;;          (cons (first rands) (lazy-seq (next rands)))))))

(defn- diceware-idx [random-quality]
  "generate a random index between 11111 and
   66666 with the digits 1-6"
  (cond (= random-quality :weak-random) (string/join
                                         "" (for [_ (range 5)]
                                              (str (inc (rand-int 6)))))
        (= random-quality :crypto-random) (random-dice-idx)
        :else :bad-input-to-diceware-idx))

(def word-map (atom nil))

(defn- diceware-passphrases
  ([random-quality]
     (diceware-passphrases random-quality 1))
  ([random-quality n]
     (if (nil? @word-map)
       (compare-and-set! word-map nil (map-words)))
     (for [_ (range n)]
       (string/join " " (for [_ (range 10)]
                          (word-map (diceware-idx random-quality)))))))

(defn- map-words []
  (let [s (slurp "files/beale.wordlist.asc")
        lines (drop 2 (drop-last 10 (string/split-lines s)))
        pairs-list (flatten
                    (map (fn [line] (string/split line #"\t")) lines))
        word-map (apply assoc {} pairs-list)]
    word-map))

(defn- rand-decoys []
  (mod (read-string (str "0x" (crypto/hex 2)))
       total-passwords-printed))

(defn- generate-diceware-passphrases []
  (let [word-map (map-words)
        n-decoys-before (rand-decoys)]
    (doseq [_ (range n-decoys-before)
            pass (diceware-passphrases :weak-random word-map)]
      (println pass "\n"))
    (doseq [_(range 9)
            pass (diceware-passphrases :crypto-random word-map)]
      (println pass "\n"))
    (apply println (diceware-passphrases :crypto-random word-map))
    (repl-read "" "")                   ;pause
    (doseq [_ (range (- total-passwords-printed n-decoys-before))
            pass (diceware-passphrases :weak-random word-map)]
      (println pass "\n"))))

(defn- print-passphrases [passphrase-type]
  (let [n-decoys-before (rand-decoys)
        passphrases-fn (passphrase-type {:diceware diceware-passphrases
                                         :web web-passphrases
                                         :short short-passphrases})]
    (doseq [_ (range n-decoys-before)
            pass (passphrases-fn :weak-random)]
      (println pass "\n"))
    (doseq [_ (range 9)                 ; first 9 'real' passwords
            pass (passphrases-fn :crypto-random)]
      (println pass "\n"))
    ;; last one, to prevent trailing newline
    (apply println (passphrases-fn :crypto-random))
    (repl-read "" "")                   ; pause
    (doseq [_ (range (- total-passwords-printed n-decoys-before))
            pass (passphrases-fn :weak-random)]
      (println pass "\n"))))

(defn generate-random-org-passwords []
  (let [word-map (map-words)
        n-decoys-before (rand-decoys)]
    ))

(defn -main [& args]
  (if (seq args)
    (let [arg (string/lower-case (first args))]
      (cond (= arg "short") (print-passphrases :short)
            (= arg "web") (print-passphrases :web)
            :else (do (println "unknown arguments:" args)
                      (print-passphrases :diceware))))
    (print-passphrases :diceware)))
