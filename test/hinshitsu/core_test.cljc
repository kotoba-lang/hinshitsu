(ns hinshitsu.core-test
  (:require [hinshitsu.core :as h]
            #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing]])))

(deftest evidence-shape-test
  (let [ev (h/evidence :passed ["a" "b"] "ok")]
    (is (= "hinshitsu.evidence.v0" (:hinshitsu/schema ev)))
    (is (h/passed? ev))
    (is (not (h/failed? ev)))
    (is (= ["a" "b"] (:hinshitsu/checks ev)))))

(deftest status-predicates-test
  (is (h/passed? (h/evidence :passed [] "")))
  (is (h/failed? (h/evidence :failed [] "")))
  (is (h/skipped? (h/evidence :skipped [] ""))))

(deftest gate-all-passed-test
  (let [g (h/gate [(h/evidence :passed ["x"] "")
                    (h/evidence :passed ["y"] "")])]
    (is (h/passed? g))
    (is (= #{"x" "y"} (set (:hinshitsu/checks g))))))

(deftest gate-one-failed-fails-test
  (let [g (h/gate [(h/evidence :passed ["x"] "")
                    (h/evidence :failed ["y"] "boom")])]
    (is (h/failed? g))
    (is (= 1 (count (get-in g [:data :failed]))))))

(deftest gate-skipped-does-not-fail-test
  (let [g (h/gate [(h/evidence :passed ["x"] "")
                    (h/evidence :skipped ["y"] "not run")])]
    (is (h/passed? g))))

(deftest gate-required-checks-test
  (testing "missing required check fails the gate even with no explicit failures"
    (let [g (h/gate [(h/evidence :passed ["x"] "")]
                     {:required-checks #{"x" "y"}})]
      (is (h/failed? g))
      (is (= ["y"] (get-in g [:data :missing-required])))))
  (testing "present required checks pass"
    (let [g (h/gate [(h/evidence :passed ["x"] "") (h/evidence :passed ["y"] "")]
                     {:required-checks #{"x" "y"}})]
      (is (h/passed? g)))))

(deftest coverage-test
  (let [c (h/coverage {:baseline "tauri"
                        :implemented ["a" "b"]
                        :partial ["c"]
                        :missing ["d"]})]
    (is (= "tauri" (:hinshitsu/baseline c)))
    (is (= 63 (:hinshitsu/functional-coverage-percent c)))))
