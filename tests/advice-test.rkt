#lang racket

;; Tests for the training advice layer.
(require rackunit
         "../poker_trainer/advice.rkt")

(module+ test
  ;; Calling 5 into a final 25 pot needs exactly 20% equity.
  (check-= (pot-odds 20 5) 0.2 0.0001)

  ;; Strongly profitable spots should continue, weak spots should fold.
  (define profitable (recommend-action 0.42 20 5))
  (define losing (recommend-action 0.12 20 5))

  (check-equal? (recommendation-action profitable) 'raise)
  (check-match (recommendation-reason profitable)
    (regexp #rx"above"))

  (check-equal? (recommendation-action losing) 'fold)
  (check-match (recommendation-reason losing)
    (regexp #rx"below")))
