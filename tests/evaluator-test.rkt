#lang racket

;; Tests for five-card and seven-card hand evaluation.
(require rackunit
         "../poker_trainer/cards.rkt"
         "../poker_trainer/evaluator.rkt")

(module+ test
  ;; Named categories make the app explanations easier to understand.
  (check-equal? (hand-category-name 8) "straight flush")
  (check-equal? (hand-category-name 0) "high card")

  ;; Seven-card evaluation chooses the best five-card hand from the available cards.
  (define royalish-hearts (parse-cards "Ah Kh Qh Jh Th 2c 3d"))
  (define quads (parse-cards "As Ah Ac Ad 9s 2d 3c"))
  (define full-house (parse-cards "Ks Kh Kc 9d 9h 2s 3c"))
  (define flush (parse-cards "Ah Jh 9h 4h 2h Kd Qc"))

  (check-equal? (hand-category (evaluate-seven royalish-hearts)) 8)
  (check-equal? (hand-category (evaluate-seven quads)) 7)
  (check-equal? (hand-category (evaluate-seven full-house)) 6)
  (check-equal? (hand-category (evaluate-seven flush)) 5)

  ;; Comparison returns a positive value when the first hand wins.
  (check-true (> (compare-hands (evaluate-seven quads)
                                (evaluate-seven full-house))
                 0)))
