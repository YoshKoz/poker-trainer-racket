#lang racket

;; -----------------------------------------------------------------------------
;; Monte Carlo equity simulation
;; -----------------------------------------------------------------------------
;; This estimates how often the hero wins by randomly completing the board and
;; dealing unknown opponent hands. It is a trainer-grade estimator, not a solver.
(provide (struct-out equity-result)
         estimate-equity)

(require (except-in racket/list range)
         "cards.rkt"
         "evaluator.rkt"
         "ranges.rkt")

(struct equity-result (equity wins ties losses simulations) #:transparent)

;; -----------------------------------------------------------------------------
;; Random dealing helpers
;; -----------------------------------------------------------------------------
;; A simple Fisher-Yates style shuffle is enough for repeated training drills.
(define (shuffled xs)
  (shuffle xs))

(define (deal-opponents deck opponent-count)
  (for/list ([i (in-range opponent-count)])
    (take (drop deck (* i 2)) 2)))

;; -----------------------------------------------------------------------------
;; Single trial
;; -----------------------------------------------------------------------------
;; Return 'win, 'tie, 'loss, or 'fail for one random completion.
(define (simulate-once hero-cards board-cards opponent-ranges)
  (define board-needed (- 5 (length board-cards)))
  (when (< board-needed 0)
    (error 'estimate-equity "board cannot contain more than five cards"))

  (let loop ([ranges opponent-ranges] [used (append hero-cards board-cards)] [opponents '()])
    (cond
      [(empty? ranges)
       (define deck (shuffled (remove-known full-deck used)))
       (define completed-board (append board-cards (take deck board-needed)))
       (define hero-score (evaluate-seven (append hero-cards completed-board)))
       (define opponent-scores
         (for/list ([opponent opponents])
           (evaluate-seven (append opponent completed-board))))
       (define comparisons
         (map (lambda (opponent-score) (compare-hands hero-score opponent-score))
              opponent-scores))
       (cond
         [(ormap negative? comparisons) 'loss]
         [(ormap zero? comparisons) 'tie]
         [else 'win])]
      [else
       (define r (first ranges))
       (define all-hands (range-hands r))
       (define available-hands
         (filter (lambda (h)
                   (not (or (member (first h) used)
                            (member (second h) used))))
                 all-hands))
       (if (empty? available-hands)
           'fail
           (let* ([picked-hand (list-ref available-hands (random (length available-hands)))]
                  [new-used (append picked-hand used)])
             (loop (rest ranges) new-used (cons picked-hand opponents))))])))

;; -----------------------------------------------------------------------------
;; Public equity estimator
;; -----------------------------------------------------------------------------
;; Equity treats ties as half a win. That keeps the explanation easy for humans.
(define (estimate-equity hero-cards board-cards player-count-or-ranges
                         #:simulations [simulations 5000]
                         #:seed [seed #f])
  (unless (= (length hero-cards) 2)
    (error 'estimate-equity "hero needs exactly two hole cards"))

  (define-values (player-count opponent-ranges)
    (if (list? player-count-or-ranges)
        (values (add1 (length player-count-or-ranges)) player-count-or-ranges)
        (values player-count-or-ranges (make-list (sub1 player-count-or-ranges) "any"))))

  ;; Pre-parse ranges for efficiency
  (define processed-ranges
    (for/list ([r opponent-ranges])
      (cond
        [(range? r) r]
        [(equal? r "any")
         (range (for*/list ([c1 full-deck] [c2 full-deck]
                            #:when (string<? (card->string c1) (card->string c2)))
                  (list c1 c2)))]
        [else (parse-range r)])))

  (when seed
    (random-seed seed))

  (let loop ([count 0] [wins 0] [ties 0] [losses 0] [actual-sims 0])
    (cond
      [(>= actual-sims simulations)
       (let ([equity (/ (+ wins (* 0.5 ties)) actual-sims)])
         (equity-result (exact->inexact equity) wins ties losses actual-sims))]
      [(> count (* simulations 10)) ; Safety break
       (if (> actual-sims 0)
           (let ([equity (/ (+ wins (* 0.5 ties)) actual-sims)])
             (equity-result (exact->inexact equity) wins ties losses actual-sims))
           (error 'estimate-equity "Could not complete any simulations (ranges might be impossible)"))]
      [else
       (let ([result (simulate-once hero-cards board-cards processed-ranges)])
         (match result
           ['win  (loop (add1 count) (add1 wins) ties losses (add1 actual-sims))]
           ['tie  (loop (add1 count) wins (add1 ties) losses (add1 actual-sims))]
           ['loss (loop (add1 count) wins ties (add1 losses) (add1 actual-sims))]
           ['fail (loop (add1 count) wins ties losses actual-sims)]))]))
)