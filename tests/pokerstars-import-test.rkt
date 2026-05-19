#lang racket

(require rackunit
         "../poker_trainer/pokerstars_import.rkt"
         "../poker_trainer/phh_import.rkt")

(module+ test
  (define sample-ps
    (string-join
     (list
      "PokerStars Hand #245605842579:  Hold'em No Limit ($0.05/$0.10 USD) - 2026/05/17 12:25:05 GMT [2026/05/17 08:25:05 ET]"
      "Table 'Procyon' 6-max Seat #3 is the button"
      "Seat 1: Player_A ($10.00 in chips)"
      "Seat 2: Player_B ($12.45 in chips)"
      "Seat 3: Hero ($10.50 in chips)"
      "Seat 4: Player_D ($9.80 in chips)"
      "Seat 5: Player_E ($25.10 in chips)"
      "Seat 6: Player_F ($10.00 in chips)"
      "Player_D: posts small blind $0.05"
      "Player_E: posts big blind $0.10"
      "*** HOLE CARDS ***"
      "Dealt to Hero [As Ks]"
      "Player_F: folds"
      "Player_A: folds"
      "Player_B: raises $0.20 to $0.30"
      "Hero: raises $0.60 to $0.90"
      "Player_D: folds"
      "Player_E: folds"
      "Player_B: calls $0.60"
      "*** FLOP *** [Ad 7s 2c]"
      "Player_B: checks"
      "Hero: bets $0.65"
      "Player_B: calls $0.65"
      "*** TURN *** [Ad 7s 2c] [Js]"
      "Player_B: checks"
      "Hero: bets $2.10"
      "Player_B: raises $3.50 to $5.60"
      "Hero: raises $3.35 to $8.95 and is all-in"
      "Player_B: calls $3.35"
      "*** RIVER *** [Ad 7s 2c Js] [Qs]"
      "*** SHOW DOWN ***"
      "Player_B: shows [Jh Jc] (three of a kind, Jacks)"
      "Hero: shows [As Ks] (a flush, Ace high)"
      "Hero collected $20.15 from pot"
      "*** SUMMARY ***"
      "Total pot $21.15 | Rake $1.00"
      "Board [Ad 7s 2c Js Qs]"
      "Seat 1: Player_A folded before Flop (did not bet)"
      "Seat 2: Player_B showed [Jh Jc] and lost with three of a kind, Jacks"
      "Seat 3: Hero (button) showed [As Ks] and won ($20.15) with a flush, Ace high"
      "Seat 4: Player_D (small blind) folded before Flop"
      "Seat 5: Player_E (big blind) folded before Flop"
      "Seat 6: Player_F folded before Flop (did not bet)")
     "\n"))

  (define hand (parse-pokerstars-hand sample-ps))
  
  (check-equal? (parsed-hand-variant hand) "NT")
  (check-equal? (parsed-hand-min-bet hand) 0.10)
  
  ;; Check starting stacks
  (check-equal? (list-ref (parsed-hand-starting-stacks hand) 0) 10.00)
  (check-equal? (list-ref (parsed-hand-starting-stacks hand) 1) 12.45)
  (check-equal? (list-ref (parsed-hand-starting-stacks hand) 2) 10.50)
  
  ;; Check hole cards
  (check-equal? (hash-ref (parsed-hand-hole-cards hand) 2) "AsKs")
  (check-equal? (hash-ref (parsed-hand-hole-cards hand) 1) "JhJc")
  
  ;; Check actions count (rough check)
  (check-true (> (length (parsed-hand-actions hand)) 10))
  
  ;; Check specific action conversion
  (check-true (member "p3 cbr 0.9" (parsed-hand-actions hand)))
  (check-true (member "d db Ad7s2c" (parsed-hand-actions hand)))
  (check-true (member "d db Js" (parsed-hand-actions hand)))
  (check-true (member "d db Qs" (parsed-hand-actions hand)))
  
  ;; Check finishing stacks
  ;; Hero (idx 2): 10.50 - 10.50 + 20.15 = 20.15
  (check-equal? (list-ref (parsed-hand-finishing-stacks hand) 2) 20.15)
  ;; Player_B (idx 1): 12.45 - 10.50 + 0 = 1.95
  (check-equal? (list-ref (parsed-hand-finishing-stacks hand) 1) 1.95)
  ;; Player_D (idx 3): 9.80 - 0.05 = 9.75
  (check-equal? (list-ref (parsed-hand-finishing-stacks hand) 3) 9.75)
)
