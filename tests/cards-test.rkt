#lang racket

;; Tests for card parsing and deck helpers.
(require rackunit
         "../poker_trainer/cards.rkt")

(module+ test
  ;; A compact card like "Ah" should become a transparent card struct.
  (check-equal? (parse-card "Ah") (card 14 'hearts))
  (check-equal? (parse-card "Td") (card 10 'diamonds))

  ;; Space-separated input is the friendly format used by the GUI and examples.
  (check-equal? (parse-cards "Ah Kd 2c")
                (list (card 14 'hearts) (card 13 'diamonds) (card 2 'clubs)))

  ;; Duplicate cards are impossible in a real hand, so the parser rejects them.
  (check-exn exn:fail? (lambda () (parse-cards "Ah Ah")))

  ;; A full deck has 52 unique cards and can remove known cards.
  (check-equal? (length full-deck) 52)
  (check-equal? (length (remove-known full-deck (parse-cards "Ah Kd"))) 50))
