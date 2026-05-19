#lang racket

;; -----------------------------------------------------------------------------
;; Visual state helpers
;; -----------------------------------------------------------------------------
;; The GUI paints colors, but the decisions about *which* color belongs to a
;; situation live here so they can be tested without opening a desktop window.
(provide equity-zone
         action-tile-zone
         action-label
         suit-symbol
         suit-ink
         surface-fill
         accent-fill
         zone-fill
         zone-border
         zone-ink
         zone-title)

;; -----------------------------------------------------------------------------
;; Equity color zones
;; -----------------------------------------------------------------------------
;; Green means clearly profitable, yellow means close, red means losing.
(define (equity-zone equity required)
  (cond
    [(>= equity (+ required 0.08)) 'good]
    [(>= equity required) 'close]
    [else 'bad]))

;; -----------------------------------------------------------------------------
;; Drill action tile zones
;; -----------------------------------------------------------------------------
;; Before the user answers, every tile is neutral. After answering, the best
;; action turns green and a wrong selected action turns red.
(define (action-tile-zone action correct-action chosen-action revealed?)
  (cond
    [(not revealed?) 'neutral]
    [(eq? action correct-action) 'best]
    [(and chosen-action (eq? action chosen-action)) 'avoid]
    [else 'neutral]))

;; -----------------------------------------------------------------------------
;; Human-facing action labels
;; -----------------------------------------------------------------------------
;; The underlying trainer action is 'raise, but in spots where nobody has bet yet
;; the same aggressive choice is really a value bet. The UI says both.
(define (action-label action)
  (case action
    [(raise) "INZETTEN/RAISEN"]
    [(call) "CALLEN/CHECKEN"]
    [(fold) "FOLDEN"]
    [else (string-upcase (symbol->string action))]))

;; -----------------------------------------------------------------------------
;; Color palette
;; -----------------------------------------------------------------------------
;; Colors are intentionally high-contrast and simple: green = click/good,
;; red = avoid/bad, yellow = close decision, gray = unrevealed.
(define (zone-fill zone)
  (case zone
    [(good best) "action green"]
    [(close) "casino gold"]
    [(bad avoid) "warning red"]
    [else "slate gray"]))

(define (zone-border zone)
  (case zone
    [(good best) "deep green"]
    [(close) "deep gold"]
    [(bad avoid) "deep red"]
    [else "charcoal"]))

(define (zone-ink zone)
  (case zone
    [(close) "black"]
    [else "white"]))

(define (zone-title zone)
  (case zone
    [(good) "GOED"]
    [(best) "JUISTE KEUZE ✓"]
    [(close) "BIJNA"]
    [(bad) "FOUT"]
    [(avoid) "VERKEERDE KEUZE ✗"]
    [else "KIES"]))

;; -----------------------------------------------------------------------------
;; Card miniatures
;; -----------------------------------------------------------------------------
;; These are our own plain playing-card symbols, not PokerStars assets.
(define (suit-symbol suit)
  (case suit
    [(hearts) "♥"]
    [(diamonds) "♦"]
    [(clubs) "♣"]
    [(spades) "♠"]
    [else "?"]))

(define (suit-ink suit)
  (case suit
    [(hearts diamonds) "firebrick"]
    [(clubs spades) "black"]
    [else "dim gray"]))

;; -----------------------------------------------------------------------------
;; Distinct poker-trainer surfaces
;; -----------------------------------------------------------------------------
;; This gives the app a casino-table feel without copying any poker-room brand.
(define (surface-fill surface)
  (case surface
    [(felt) "felt green"]
    [(felt-dark) "felt dark"]
    [(rail) "rail green"]
    [(panel) "panel cream"]
    [(card-face) "warm card"]
    [(card-shadow) "card shadow"]
    [else "white smoke"]))

(define (accent-fill accent)
  (case accent
    [(gold) "casino gold"]
    [(gold-dark) "deep gold"]
    [(ink) "charcoal"]
    [(muted) "muted ink"]
    [else "dim gray"]))
