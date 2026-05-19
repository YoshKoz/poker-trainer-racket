#lang racket

;; -----------------------------------------------------------------------------
;; Card model
;; -----------------------------------------------------------------------------
;; A card is just a rank and a suit. Ranks use poker numeric values:
;; 14 = ace, 13 = king, ..., 2 = deuce.
(provide (struct-out card)
         full-deck
         parse-card
         parse-cards
         remove-known
         card->string
         rank->string
         suit->char
         rank-by-token
         token-by-rank
         hand->string
         string->hand)

(struct card (rank suit) #:transparent)

;; -----------------------------------------------------------------------------
;; Rank and suit conversion tables
;; -----------------------------------------------------------------------------
;; These hash tables keep the parser small and make the accepted notation obvious.
(define rank-by-token
  (hash "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9
        "T" 10 "10" 10 "J" 11 "Q" 12 "K" 13 "A" 14))

(define token-by-rank
  (hash 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9"
        10 "T" 11 "J" 12 "Q" 13 "K" 14 "A"))

(define suit-by-token
  (hash #\h 'hearts
        #\d 'diamonds
        #\c 'clubs
        #\s 'spades))

(define token-by-suit
  (hash 'hearts "h"
        'diamonds "d"
        'clubs "c"
        'spades "s"))

;; -----------------------------------------------------------------------------
;; Deck construction
;; -----------------------------------------------------------------------------
;; The full deck is deterministic. Shuffling happens in the equity simulator.
(define full-deck
  (for*/list ([suit '(hearts diamonds clubs spades)]
              [rank (in-range 2 15)])
    (card rank suit)))

;; -----------------------------------------------------------------------------
;; Public parsing helpers
;; -----------------------------------------------------------------------------
;; Cards are written in poker shorthand: Ah, Kd, Qs, Jc, Th, or 10h.
(define (parse-card text)
  (define cleaned (string-upcase (string-trim text)))
  (unless (member (string-length cleaned) '(2 3))
    (error 'parse-card "expected a card like Ah, Td, or 10h; got ~a" text))
  (define rank-token (substring cleaned 0 (sub1 (string-length cleaned))))
  (define suit-token (string-ref (string-downcase cleaned) (sub1 (string-length cleaned))))
  (define rank (hash-ref rank-by-token rank-token #f))
  (define suit (hash-ref suit-by-token suit-token #f))
  (unless rank
    (error 'parse-card "unknown rank in card ~a" text))
  (unless suit
    (error 'parse-card "unknown suit in card ~a" text))
  (card rank suit))

;; Parse a space-separated card list and reject physical impossibilities.
(define (parse-cards text)
  (define tokens (filter non-empty-string? (string-split (string-trim text))))
  (define cards (map parse-card tokens))
  (unless (= (length cards) (length (remove-duplicates cards)))
    (error 'parse-cards "duplicate card in input: ~a" text))
  cards)

;; Remove visible cards from a deck before dealing unknown cards.
(define (remove-known deck known-cards)
  (filter (lambda (candidate) (not (member candidate known-cards))) deck))

;; -----------------------------------------------------------------------------
;; Display helpers
;; -----------------------------------------------------------------------------
;; The GUI uses these to show generated drills in familiar poker notation.
(define (rank->string rank)
  (hash-ref token-by-rank rank (lambda () (number->string rank))))

(define (suit->char suit)
  (hash-ref token-by-suit suit "?"))

(define (card->string c)
  (string-append (rank->string (card-rank c))
                 (suit->char (card-suit c))))

(define (hand->string h)
  (apply string-append (map card->string h)))

(define (string->hand s)
  (parse-cards s))
