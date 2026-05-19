#lang racket

;; Tests for Monte Carlo equity simulation.
(require rackunit
         "../poker_trainer/cards.rkt"
         "../poker_trainer/equity.rkt")

(module+ test
  ;; Equity is probabilistic, so tests check invariants instead of exact values.
  (define result (estimate-equity (parse-cards "Ah Kh")
                                  (parse-cards "Qh Jh 2c")
                                  2
                                  #:simulations 100
                                  #:seed 1234))

  (check-true (equity-result? result))
  (check-true (<= 0.0 (equity-result-equity result) 1.0))
  (check-equal? (+ (equity-result-wins result)
                   (equity-result-ties result)
                   (equity-result-losses result))
                100))
