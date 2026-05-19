#lang racket

;; -----------------------------------------------------------------------------
;; Poker hand evaluator
;; -----------------------------------------------------------------------------
;; This file evaluates Texas Hold'em hands. It intentionally stays dependency-free:
;; it checks every five-card combination from the available seven cards.
(provide (struct-out hand-score)
         evaluate-five
         evaluate-seven
         compare-hands
         hand-category
         hand-category-name)

(require racket/list
         "cards.rkt")

(struct hand-score (category kickers cards) #:transparent)

;; -----------------------------------------------------------------------------
;; Category names
;; -----------------------------------------------------------------------------
;; Higher category numbers are stronger. Kickers break ties inside a category.
(define category-names
  (hash 8 "straight flush"
        7 "four of a kind"
        6 "full house"
        5 "flush"
        4 "straight"
        3 "three of a kind"
        2 "two pair"
        1 "one pair"
        0 "high card"))

(define (hand-category score)
  (hand-score-category score))

(define (hand-category-name category)
  (hash-ref category-names category "unknown"))

;; -----------------------------------------------------------------------------
;; List helpers
;; -----------------------------------------------------------------------------
;; Racket makes recursion pleasant, so combinations are a short recursive helper.
(define (combinations xs k)
  (cond
    [(zero? k) (list '())]
    [(empty? xs) '()]
    [else
     (append
      (map (lambda (tail) (cons (first xs) tail))
           (combinations (rest xs) (sub1 k)))
      (combinations (rest xs) k))]))

(define (descending xs)
  (sort xs >))

(define (counts-by-rank cards)
  (for/fold ([counts (hash)]) ([c cards])
    (hash-update counts (card-rank c) add1 0)))

(define (ranks-by-count counts n)
  (descending
   (for/list ([(rank count) (in-hash counts)]
              #:when (= count n))
     rank)))

;; -----------------------------------------------------------------------------
;; Straight and flush detection
;; -----------------------------------------------------------------------------
;; Ace-low straights are represented with a high card of 5.
(define (straight-high ranks)
  (define unique (remove-duplicates (descending ranks)))
  (cond
    [(and (= (length unique) 5)
          (= (- (first unique) (last unique)) 4))
     (first unique)]
    [(equal? unique '(14 5 4 3 2)) 5]
    [else #f]))

(define (flush? cards)
  (define suits (map card-suit cards))
  (andmap (lambda (suit) (equal? suit (first suits))) (rest suits)))

;; -----------------------------------------------------------------------------
;; Five-card scoring
;; -----------------------------------------------------------------------------
;; Each result stores a category and ordered tie-breakers. That makes comparison
;; a simple lexicographic walk through numbers.
(define (evaluate-five cards)
  (unless (= (length cards) 5)
    (error 'evaluate-five "expected exactly five cards"))
  (define ranks (map card-rank cards))
  (define counts (counts-by-rank cards))
  (define straight (straight-high ranks))
  (define suited? (flush? cards))
  (define fours (ranks-by-count counts 4))
  (define threes (ranks-by-count counts 3))
  (define pairs (ranks-by-count counts 2))
  (define singles (ranks-by-count counts 1))
  (cond
    [(and suited? straight)
     (hand-score 8 (list straight) cards)]
    [(not (empty? fours))
     (hand-score 7 (append fours singles) cards)]
    [(and (not (empty? threes)) (not (empty? pairs)))
     (hand-score 6 (append threes pairs) cards)]
    [suited?
     (hand-score 5 (descending ranks) cards)]
    [straight
     (hand-score 4 (list straight) cards)]
    [(not (empty? threes))
     (hand-score 3 (append threes singles) cards)]
    [(>= (length pairs) 2)
     (hand-score 2 (append pairs singles) cards)]
    [(= (length pairs) 1)
     (hand-score 1 (append pairs singles) cards)]
    [else
     (hand-score 0 (descending ranks) cards)]))

;; -----------------------------------------------------------------------------
;; Seven-card scoring
;; -----------------------------------------------------------------------------
;; Texas Hold'em chooses the best five cards from the seven visible to a player.
(define (evaluate-seven cards)
  (unless (>= (length cards) 5)
    (error 'evaluate-seven "need at least five cards"))
  (define scores (map evaluate-five (combinations cards 5)))
  (foldl (lambda (candidate best)
           (if (> (compare-hands candidate best) 0) candidate best))
         (first scores)
         (rest scores)))

;; -----------------------------------------------------------------------------
;; Score comparison
;; -----------------------------------------------------------------------------
;; Positive means the first hand wins, negative means the second hand wins.
(define (score->numbers score)
  (cons (hand-score-category score) (hand-score-kickers score)))

(define (compare-number-lists left right)
  (cond
    [(and (empty? left) (empty? right)) 0]
    [(> (first left) (first right)) 1]
    [(< (first left) (first right)) -1]
    [else (compare-number-lists (rest left) (rest right))]))

(define (compare-hands left right)
  (compare-number-lists (score->numbers left) (score->numbers right)))
