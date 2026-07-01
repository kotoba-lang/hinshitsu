(ns hinshitsu.core
  "hinshitsu (品質) — portable evidence/gate primitives for software quality
  checks. Generalizes the ad-hoc `SdkCheckStatus` / `*CheckReport` /
  `EvidenceCheckReport` shapes kotoba-shell's Rust implementation hand-rolls
  per check into one reusable, host-neutral EDN schema, so any kotoba tool
  (kotoba-shell, kotoba-cli, hinshitsu.mokushi, or a future check) can produce
  and consume the same evidence shape instead of inventing its own.

  Pure data + pure functions. No filesystem, no process, no network — those
  live in the caller (or in a sibling namespace like `hinshitsu.mokushi`)."
  (:require [clojure.string :as str]))

(def schema "hinshitsu.evidence.v0")

;; ── Evidence ──────────────────────────────────────────────────────────────

(defn evidence
  "Build a single evidence map.

    (evidence :passed [\"built\" \"launched\"] \"looks good\")
    (evidence :failed [\"built\"] \"xcodebuild exited 65\" {:command [...] :stdout \"...\"})

  `status` is one of :passed :skipped :failed. `checks` is a seq of short
  check-name strings that were attempted. `detail` is a human-readable
  one-line summary. `opts` may carry :command / :stdout / :stderr / :data
  (an arbitrary EDN payload specific to the check, e.g. a similarity score)."
  ([status checks detail] (evidence status checks detail {}))
  ([status checks detail opts]
   {:pre [(#{:passed :skipped :failed} status)]}
   (merge {:hinshitsu/schema schema
           :hinshitsu/status status
           :hinshitsu/checks (vec checks)
           :hinshitsu/detail detail}
          (select-keys opts [:command :stdout :stderr :data]))))

(defn passed? [ev] (= :passed (:hinshitsu/status ev)))
(defn failed? [ev] (= :failed (:hinshitsu/status ev)))
(defn skipped? [ev] (= :skipped (:hinshitsu/status ev)))

;; ── Gate ──────────────────────────────────────────────────────────────────

(defn gate
  "Aggregate a seq of evidence maps into one pass/fail gate decision.

  Default policy: every entry must be :passed (a single :failed fails the
  gate; :skipped entries are recorded but don't fail it — mirrors
  kotoba-shell's evidence-check treating Skipped as non-blocking dev-time
  state). Pass `:required-checks #{...}` to additionally require specific
  check names to be present and passed somewhere in the evidence seq."
  ([evidences] (gate evidences {}))
  ([evidences {:keys [required-checks]}]
   (let [failed (filterv failed? evidences)
         passed-checks (into #{} (mapcat :hinshitsu/checks) (filter passed? evidences))
         missing-required (when required-checks
                             (into [] (remove passed-checks) required-checks))
         ok? (and (empty? failed) (empty? missing-required))]
     (evidence
      (if ok? :passed :failed)
      (into [] (mapcat :hinshitsu/checks) evidences)
      (cond
        (seq failed) (str (count failed) " check(s) failed")
        (seq missing-required) (str "missing required evidence: " (str/join ", " missing-required))
        :else "all checks passed")
      {:data {:entries evidences
              :failed failed
              :missing-required (or missing-required [])}}))))

(defn coverage
  "A coverage/maturity assessment: what fraction of a check surface is
  implemented vs. partial vs. missing. Mirrors kotoba-shell's
  CoverageAssessment shape but as portable data, not a Rust struct.

    (coverage {:baseline \"tauri\" :implemented [\"ios-build\" \"macos-dev\"]
               :partial [\"android-build\"] :missing [\"ios-device-install\"]})"
  [{:keys [baseline implemented partial missing next-steps]
    :or {implemented [] partial [] missing [] next-steps []}}]
  (let [total (+ (count implemented) (count partial) (count missing))
        pct (fn [n] (if (zero? total) 0 (int (+ 0.5 (* 100.0 (/ n total))))))]
    {:hinshitsu/schema "hinshitsu.coverage.v0"
     :hinshitsu/baseline baseline
     :hinshitsu/functional-coverage-percent (pct (+ (count implemented) (* 0.5 (count partial))))
     :hinshitsu/implemented (vec implemented)
     :hinshitsu/partial (vec partial)
     :hinshitsu/missing (vec missing)
     :hinshitsu/next-steps (vec next-steps)}))
