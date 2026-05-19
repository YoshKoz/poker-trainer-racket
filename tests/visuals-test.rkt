#lang racket

;; Tests for the visual state helpers used by the GUI.
(require rackunit
         "../poker_trainer/visuals.rkt")

(module+ test
  ;; Equity far above the required price is green.
  (check-equal? (equity-zone 0.42 0.20) 'good)

  ;; Barely profitable calls are yellow so the user knows it is close.
  (check-equal? (equity-zone 0.22 0.20) 'close)

  ;; Equity below the required price is red.
  (check-equal? (equity-zone 0.12 0.20) 'bad)

  ;; Before answering, buttons stay neutral so the trainer does not spoil drills.
  (check-equal? (action-tile-zone 'fold 'raise #f #f) 'neutral)

  ;; After answering, the correct action is green and the wrong click is red.
  (check-equal? (action-tile-zone 'raise 'raise 'fold #t) 'best)
  (check-equal? (action-tile-zone 'fold 'raise 'fold #t) 'avoid)
  (check-equal? (action-tile-zone 'call 'raise 'fold #t) 'neutral)

  ;; UI labels explain that "raise" also means betting when nobody bet first.
  (check-equal? (action-label 'raise) "BET/RAISE")
  (check-equal? (action-label 'call) "CALL/CHECK")
  (check-equal? (action-label 'fold) "FOLD")

  ;; Card suits get recognizable miniature-card symbols and colors.
  (check-equal? (suit-symbol 'hearts) "♥")
  (check-equal? (suit-ink 'diamonds) "firebrick")
  (check-equal? (suit-ink 'clubs) "black")

  ;; The app uses a distinct casino-trainer palette, not any poker-room assets.
  (check-equal? (surface-fill 'felt) "felt green")
  (check-equal? (surface-fill 'rail) "rail green")
  (check-equal? (surface-fill 'card-face) "warm card")
  (check-equal? (accent-fill 'gold) "casino gold"))
