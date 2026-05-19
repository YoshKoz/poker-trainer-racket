#lang racket

;; Tests for importing existing PHH hand-history files into aggregate stats.
(require rackunit
         "../poker_trainer/cards.rkt"
         "../poker_trainer/phh_import.rkt")

(module+ test
  (define sample-phh
    (string-join
     (list
      "variant = 'NT'"
      "min_bet = 100"
      "starting_stacks = [1000, 1000]"
      "finishing_stacks = [700, 1300]"
      "actions = ['d dh p1 KcJd', 'd dh p2 AsAh', 'p1 cc', 'p2 cbr 300', 'p1 f']")
     "\n"))

  (define postflop-phh
    (string-join
     (list
      "variant = 'NT'"
      "min_bet = 100"
      "starting_stacks = [1000, 1000]"
      "finishing_stacks = [1300, 700]"
      "actions = ['d dh p1 AhKd', 'd dh p2 QsJh', 'p1 cbr 200', 'p2 cc', 'd db Ks7c2d', 'p1 cbr 600', 'p2 f']")
     "\n"))

  ;; The parser extracts real PHH actions and stack outcomes.
  (define hand (parse-phh-text sample-phh))
  (check-equal? (parsed-hand-variant hand) "NT")
  (check-equal? (length (parsed-hand-actions hand)) 5)

  ;; Hole-card classes become reusable training spot keys.
  (check-equal? (hole-cards->history-key "KcJd")
                "dominated-offsuit-broadway-out-of-position")
  (check-equal? (hole-cards->history-key "AsAh")
                "preflop-premium-pair")

  ;; Each player action is converted into one action-outcome sample.
  (define rows (phh-text->outcomes sample-phh))
  (check-equal? (length rows) 3)
  (check-true (ormap (lambda (row)
                       (and (string=? (history-action-outcome-key row)
                                      "dominated-offsuit-broadway-out-of-position")
                            (eq? (history-action-outcome-action row) 'fold)
                            (= (history-action-outcome-losses row) 1)))
                     rows))
  (check-true (ormap (lambda (row)
                       (and (string=? (history-action-outcome-key row)
                                      "preflop-premium-pair")
                            (eq? (history-action-outcome-action row) 'raise)
                            (= (history-action-outcome-wins row) 1)))
                     rows))

  ;; Aggregation merges duplicate key/action rows.
  (define aggregated (aggregate-history-outcomes (append rows rows)))
  (check-true (ormap (lambda (row)
                       (and (string=? (history-action-outcome-key row)
                                      "preflop-premium-pair")
                            (eq? (history-action-outcome-action row) 'raise)
                            (= (history-action-outcome-trials row) 2)
                            (= (history-action-outcome-wins row) 2)))
                     aggregated))

  ;; Rich postflop keys include street, board texture, hand category, position,
  ;; and pressure instead of only hole-card class.
  (check-equal? (board-texture-key (map parse-card '("Ks" "7c" "2d")))
                "dry")
  (check-equal? (pressure-key 'raise 600 300 100) "large")
  (define postflop-rows (phh-text->outcomes postflop-phh))
  (check-true (ormap (lambda (row)
                       (and (regexp-match? #rx"street-flop" (history-action-outcome-key row))
                            (regexp-match? #rx"texture-dry" (history-action-outcome-key row))
                            (regexp-match? #rx"hand-one-pair" (history-action-outcome-key row))
                            (regexp-match? #rx"position-early" (history-action-outcome-key row))
                            (regexp-match? #rx"pressure-large" (history-action-outcome-key row))
                            (eq? (history-action-outcome-action row) 'raise)))
                     postflop-rows)))
