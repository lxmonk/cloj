(ns learning.passphraseGenerator
  (:require [crypto.random :as crypto :only [hex]]
            [clojure.string :as string
             ;; :only (join lower-case split split-lines)
             ]
            [clj-http.client :as client]
            )
  (:use [clojure.main :only (repl-read)])
  (:gen-class))                         ;gen-class needed since this
                                        ;is the 'Main' class for the program.

(def total-passwords-printed 20 ;10000 FIXME
  )

(def random-org-url
  "https://www.random.org/integers/?num=400&min=0&max=94&col=400&base=10&format=plain&rnd=new")

(declare diceware-passphrases short-passphrases web-passphrases)

(defn- generate-random
  "return a (crypto) random number between 11111 and 66666 inclusive."
  []
  (let [random-number (read-string
                       (str "0x" (crypto.random/hex 4)))
        ;; convert to a base-6 string
        base-6 (Long/toString random-number 6)]
    ;; take the first 5 digits, and convert each from [0,5] to [1,6],
    ;; since the file is keyed that way (to be used with dice).
    (str (+ (Long/parseLong (subs base-6 0 5)) 11111))))

(defn- get-web-random!
  "this function has state! it gets random bits from random.org (via https),
   and keeps them and returns wanted random numbers on demand."
  (letfn (random-nums! []
           (let [response (client/get random-org-url
                                      {:headers
                                       {"user-agent" "avi.rei@gmail.com"}})
                 (body (string/split (:body res) #"\t"))
                 nums (map Long/parseLong
                           (conj (butlast body)
                                 (string/join (butlast (last body)))))]
             nums))
    ([] (let [rands (random-nums!)]
          (get-web-random! rands)))
    ([rands]
       (if (empty? rands)
         (get-web-random!)
         ;; we're out of random numbers.. get some.
         (cons (first rands) (lazy-seq (next rands)))))))

(defn- generate-idx [random-quality]
  "generate a random index between 11111 and
   66666 with the digits 1-6"
  (cond (= random-quality :weak-random) (string/join
                                         "" (for [_ (range 5)]
                                              (str (inc (rand-int 6)))))
        (= random-quality :crypto-random) (generate-random)
        (= random-quality :random-org) (get-web-random!)
        :else :bad-input-to-generate-idx))

(defmulti decoy
  "print decoy passwords/passphrases to screen,
  formatting is the same as the real passwords'"
  :password-style)

(defn- diceware-passphrases
  ([random-quality word-map]
     (diceware-passphrases random-quality word-map 1))
  ([random-quality word-map n]
     (for [_ (range n)]
       (string/join " " (for [_ (range 10)]
                          (word-map (generate-idx random-quality)))))))

(defmethod decoy :diceware [{word-map :word-map n :n}]
      (diceware-passphrases :weak-random word-map n))

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
    ;; (apply (fn [pass] (println pass "\n"))
    ;;        (decoy {:password-style :diceware :word-map word-map
    ;;                :n n-decoys-before}))
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
      (println pass "\n")
    ;; (apply println (decoy {:password-style :diceware :word-map word-map
    ;;                   :n (- 10000 n-decoys-before)}))
      )))

(defn- generate-passphrases [passphrase-type]
  (let [word-map (map-words)
        n-decoys-before (rand-decoys)
        passphrases-fn (passphrase-type {:diceware diceware-passphrases
                                         :web web-passphrases
                                         :short short-passphrases})]
    ;; (apply (fn [pass] (println pass "\n"))
    ;;        (decoy {:password-style :diceware :word-map word-map
    ;;                :n n-decoys-before}))
    (doseq [_ (range n-decoys-before)
            pass (passphrases-fn :weak-random word-map)]
      (println pass "\n"))
    (doseq [_(range 9)
            pass (passphrases-fn :crypto-random word-map)]
      (println pass "\n"))
    (apply println (passphrases-fn :crypto-random word-map))
    (repl-read "" "")                   ;pause
    (doseq [_ (range (- total-passwords-printed n-decoys-before))
            pass (passphrases-fn :weak-random word-map)]
      (println pass "\n")
    ;; (apply println (decoy {:password-style :diceware :word-map word-map
    ;;                   :n (- 10000 n-decoys-before)}))
      )))

(defn generate-random-org-passwords []
  (let [word-map (map-words)
        n-decoys-before (rand-decoys)]
    ))

(defn -main [& args]
  (if (seq args)
    (let [arg (string/lower-case (first args))]
      (cond (= arg "short") (generate-passphrases :short)
            (= arg "web") (generate-passphrases :web)
            :else (generate-passphrases :diceware)))
    (generate-passphrases :diceware))
  ;; (prn "it's working.")
  )
