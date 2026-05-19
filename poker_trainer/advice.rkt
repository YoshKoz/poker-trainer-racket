#lang racket

;; -----------------------------------------------------------------------------
;; Decision advice
;; -----------------------------------------------------------------------------
;; This module is deliberately simple: it teaches pot odds and basic expected
;; value logic rather than pretending to be a live GTO solver.
(provide (struct-out recommendation)
         pot-odds
         recommend-action)

(struct recommendation (action equity required-equity reason) #:transparent)

;; Calling C into a pot P creates a final pot of P + C, so required equity is
;; C / (P + C). A free check needs no equity.
(define (pot-odds pot to-call)
  (cond
    [(< pot 0) (error 'pot-odds "pot cannot be negative")]
    [(< to-call 0) (error 'pot-odds "amount to call cannot be negative")]
    [(zero? to-call) 0.0]
    [else (/ to-call (+ pot to-call))]))

;; Turn equity and pot odds into an easy training answer.
(define (recommend-action equity pot to-call)
  (define required (pot-odds pot to-call))
  (cond
    [(>= equity (+ required 0.15))
     (recommendation
      'raise
      equity
      required
      "Jouw winkans is ruim hoger dan de prijs. Goed moment om in te zetten of te raisen.")]
    [(>= equity required)
     (recommendation
      'call
      equity
      required
      "Jouw winkans is hoger dan de prijs. Doorgaan is winstgevend.")]
    [else
     (recommendation
      'fold
      equity
      required
      "Jouw winkans is lager dan de prijs. Folden voorkomt een verliezende call.")]))
