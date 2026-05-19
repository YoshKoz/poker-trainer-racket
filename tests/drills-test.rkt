#lang racket

;; Tests for generated training drills and answer scoring.
(require rackunit
         "../poker_trainer/drills.rkt")

(module+ test
  ;; The built-in drills should be deterministic enough for repeat practice.
  (define spot (first default-drills))

  (check-true (drill-spot? spot))
  (check-not-false (member (drill-spot-correct-action spot) '(fold call raise)))

  ;; Answer checking returns both correctness and an explanation string.
  (define right (check-answer spot (drill-spot-correct-action spot)))
  (define wrong (check-answer spot 'fold))

  (check-true (answer-result? right))
  (check-true (answer-result-correct? right))
  (check-false (answer-result-correct? wrong))

  ;; Matching aggregate data is allowed to drive the best answer.
  (check-match (answer-result-explanation right)
    (regexp #rx"Data-backed choice")))
