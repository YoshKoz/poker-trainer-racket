#lang racket

;; Tests for data-backed action outcome decisions.
(require rackunit
         racket/runtime-path
         "../poker_trainer/stats.rkt")

(define-runtime-path seed-data "../data/action-outcomes.csv")

(module+ test
  ;; Keys are normalized so CSV rows can match human drill titles.
  (check-equal? (spot-key "Top pair top kicker against a calling station")
                "top-pair-top-kicker-against-a-calling-station")

  ;; A row stores aggregate outcome data for one action in one type of spot.
  (define row (parse-outcome-row
               "top-pair,raise,1800,1161,639,124.5"))
  (check-equal? (action-outcome-key row) "top-pair")
  (check-equal? (action-outcome-action row) 'raise)
  (check-= (outcome-win-rate row) 0.645 0.001)

  ;; The best action uses confidence-adjusted results, not just raw max wins.
  ;; Here a tiny 2/2 raise sample should not beat a large profitable call sample.
  (define cautious-stats
    (list (action-outcome "draw" 'raise 2 2 0 1.2)
          (action-outcome "draw" 'call 400 232 168 42.0)
          (action-outcome "draw" 'fold 400 0 400 -400.0)))
  (define cautious-decision (best-action-for-key cautious-stats "draw" 'fold))
  (check-equal? (data-decision-action cautious-decision) 'call)
  (check-equal? (data-decision-source cautious-decision) 'data)

  ;; Missing data falls back to the built-in training rule.
  (define fallback (best-action-for-key cautious-stats "missing" 'raise))
  (check-equal? (data-decision-action fallback) 'raise)
  (check-equal? (data-decision-source fallback) 'fallback)

  ;; Loading CSV skips the header and keeps all action rows.
  (define loaded (load-action-outcomes seed-data))
  (check-true (> (length loaded) 0))
  (check-equal? (data-decision-source
                 (best-action-for-key loaded
                                      "top-pair-top-kicker-against-a-calling-station"
                                      'fold))
                'data))
