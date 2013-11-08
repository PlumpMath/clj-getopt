(ns com.lemonodor.getopt
  "Clojure version of GNU getopt.  This module provides one function: getopt.")

(set! *warn-on-reflection* true)


(defn- substring? [^String a ^String b]
  (>= (.indexOf b a) 0))


(defn- long-has-args [opt longopts]
  (let [possibilities (filter (fn [^String o]
                                (.startsWith o opt))
                              longopts)]
    (if (not (seq possibilities))
      (throw (Exception. (str "option --" opt " not recognized")))
      (if (some #{opt} possibilities)
        ;; Exact match
        [false opt]
        (if (some #{(str opt "=")} possibilities)
          [true opt]
          ;; No exact match, so better be unique.
          (if (> (count possibilities) 1)
            (throw (Exception. (str "option --" opt " is not a unique prefix")))
            (let [^String unique-match (first possibilities)]
              (if (.endsWith unique-match "=")
                [true (subs unique-match 1)]
                [false unique-match]))))))))


(defn- do-longs [opts opt longopts args]
  (let [[_ opt optarg] (re-matches #"([^=]+)(?:=(.*))?" opt)
        [has-arg opt] (long-has-args opt longopts)]
    (when (and (not has-arg) optarg)
      (throw (Exception. (str "option --" opt " must not have an argument"))))
    (when (and has-arg (not optarg) (not (seq args)))
      (throw (Exception. (str "option --" opt " requires an argument"))))
    (let [[optarg args]
          (if (and has-arg (not optarg))
            [(first args) (rest args)]
            [optarg args])]
      [(conj opts [(str "--" opt) (or optarg "")])
       args])))


(defn- short-has-arg [opt shortopts]
  (if (not (substring? opt shortopts))
    (throw (Exception. (str "option -" opt " not recognized")))
    (substring? (str opt ":") shortopts)))


(defn- do-shorts [opts optstring shortopts args]
  (loop [opts opts
         optstring optstring
         args args]
    (if (= optstring "")
      [opts args]
      (let [opt (subs optstring 0 1)
            optstring (subs optstring 1)]
        (if (not (short-has-arg opt shortopts))
          (recur (conj opts [(str "-" opt) ""]) optstring args)
          (if (and (= optstring "") (not (seq args)))
            (throw (Exception. (str "option -" opt " requires argument")))
            (if (= optstring "")
              (recur (conj opts [(str "-" opt) (first args)])
                     ""
                     (rest args))
              (recur (conj opts [(str "-" opt) optstring])
                     ""
                     args))))))))


(defn getopt
  "(getopt <args> <options> [<long options>]) -> [opts, args]

   This function works like getopt(), except that GNU style scanning
   mode is used by default. This means that option and non-option
   arguments may be intermixed. The getopt() function stops processing
   options as soon as a non-option argument is encountered.

   If the first character of the option string is `+', or if the
   environment variable POSIXLY_CORRECT is set, then option processing
   stops as soon as a non-option argument is encountered."
  ([args shortopts]
     (getopt args shortopts []))
  ([args ^String shortopts longopts]
     (let [[shortopts all-options-first]
           (cond (.startsWith shortopts "+") [(subs shortopts 1) true]
                 (System/getenv "POSIXLY_CORRECT") [shortopts true]
                 :else [shortopts false])]
       (loop [opts []
              args (seq args)
              prog-args []]
         (if (not (seq args))
           [opts prog-args]
           (let [^String arg (first args)]
             (cond
              (= arg "--")
              (recur opts [] (concat prog-args (rest args)))
              (.startsWith arg "--")
              (let [[opts args] (do-longs opts (subs arg 2) longopts (rest args))]
                (recur opts args prog-args))
              (and (= (subs arg 0 1) "-") (not (= (subs arg 1 2) "-")))
              (let [[opts args] (do-shorts opts (subs arg 1) shortopts (rest args))]
                (recur opts args prog-args))
              :else (if all-options-first
                      (recur opts [] (concat prog-args args))
                      (recur opts (rest args) (conj prog-args arg))))))))))
