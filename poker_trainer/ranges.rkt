#lang racket

(require "cards.rkt"
         racket/match
         racket/string)

(provide (struct-out range)
         parse-range
         expand-range)

(struct range (hands) #:transparent)

(define (expand-range r)
  (range-hands r))

(define (rank-token->num tok)
  (hash-ref rank-by-token tok))

(define (all-pairs r)
  (for*/list ([s1 '(hearts diamonds clubs spades)]
              [s2 '(hearts diamonds clubs spades)]
              #:when (string<? (symbol->string s1) (symbol->string s2)))
    (list (card r s1) (card r s2))))

(define (all-suited r1 r2)
  (for/list ([s '(hearts diamonds clubs spades)])
    (list (card r1 s) (card r2 s))))

(define (all-offsuit r1 r2)
  (for*/list ([s1 '(hearts diamonds clubs spades)]
              [s2 '(hearts diamonds clubs spades)]
              #:when (not (eq? s1 s2)))
    (list (card r1 s1) (card r2 s2))))

(define (parse-single-range-token tok)
  (let ([tok (string-trim tok)])
    (cond
      ;; Pairs: JJ+, 88-QQ
      [(regexp-match #px"^([2-9TJQKA])\\1\\+$" tok)
       => (lambda (m)
            (let ([r (rank-token->num (second m))])
              (for*/list ([rank (in-range r 15)]
                          [hand (all-pairs rank)])
                hand)))]
      [(regexp-match #px"^([2-9TJQKA])\\1-([2-9TJQKA])\\2$" tok)
       => (lambda (m)
            (let ([r1 (rank-token->num (second m))]
                  [r2 (rank-token->num (third m))])
              (let ([low (min r1 r2)]
                    [high (max r1 r2)])
                (for*/list ([rank (in-range low (add1 high))]
                            [hand (all-pairs rank)])
                  hand))))]
      [(regexp-match #px"^([2-9TJQKA])\\1$" tok)
       => (lambda (m)
            (all-pairs (rank-token->num (second m))))]

      ;; Suited: AQs+, KJs-KQs
      [(regexp-match #px"^([2-9TJQKA])([2-9TJQKA])s\\+$" tok)
       => (lambda (m)
            (let ([r1 (rank-token->num (second m))]
                  [r2 (rank-token->num (third m))])
              (let ([high (max r1 r2)]
                    [low (min r1 r2)])
                (for*/list ([r (in-range low high)]
                            [hand (all-suited high r)])
                  hand))))]
      [(regexp-match #px"^([2-9TJQKA])([2-9TJQKA])s-([2-9TJQKA])([2-9TJQKA])s$" tok)
       => (lambda (m)
            (let ([r1 (rank-token->num (second m))]
                  [r2 (rank-token->num (third m))]
                  [r3 (rank-token->num (fourth m))]
                  [r4 (rank-token->num (fifth m))])
              (let ([h-min (min r1 r3)] [h-max (max r1 r3)]
                    [l-min (min r2 r4)] [l-max (max r2 r4)])
                (for*/list ([h (in-range h-min (add1 h-max))]
                            [l (in-range l-min (add1 l-max))]
                            #:when (> h l)
                            [hand (all-suited h l)])
                  hand))))]
      [(regexp-match #px"^([2-9TJQKA])([2-9TJQKA])s$" tok)
       => (lambda (m)
            (all-suited (rank-token->num (second m)) (rank-token->num (third m))))]

      ;; Offsuit: AQo+, JTo-QJo
      [(regexp-match #px"^([2-9TJQKA])([2-9TJQKA])o\\+$" tok)
       => (lambda (m)
            (let ([r1 (rank-token->num (second m))]
                  [r2 (rank-token->num (third m))])
              (let ([high (max r1 r2)]
                    [low (min r1 r2)])
                (for*/list ([r (in-range low high)]
                            [hand (all-offsuit high r)])
                  hand))))]
      [(regexp-match #px"^([2-9TJQKA])([2-9TJQKA])o-([2-9TJQKA])([2-9TJQKA])o$" tok)
       => (lambda (m)
            (let ([r1 (rank-token->num (second m))]
                  [r2 (rank-token->num (third m))]
                  [r3 (rank-token->num (fourth m))]
                  [r4 (rank-token->num (fifth m))])
              (unless (= r1 r3) (error 'parse-range "Inconsistent high cards in range: ~a" tok))
              (let ([high r1]
                    [low1 (min r2 r4)]
                    [low2 (max r2 r4)])
                (for*/list ([r (in-range low1 (add1 low2))]
                            [hand (all-offsuit high r)])
                  hand))))]
      [(regexp-match #px"^([2-9TJQKA])([2-9TJQKA])o$" tok)
       => (lambda (m)
            (all-offsuit (rank-token->num (second m)) (rank-token->num (third m))))]

      ;; Generic (both suited and offsuit): AK, AK+, A8-AQ
      [(regexp-match #px"^([2-9TJQKA])([2-9TJQKA])\\+$" tok)
       => (lambda (m)
            (let ([r1 (rank-token->num (second m))]
                  [r2 (rank-token->num (third m))])
              (let ([high (max r1 r2)]
                    [low (min r1 r2)])
                (for*/list ([r (in-range low high)]
                            [hand (append (all-suited high r) (all-offsuit high r))])
                  hand))))]
      [(regexp-match #px"^([2-9TJQKA])([2-9TJQKA])-([2-9TJQKA])([2-9TJQKA])$" tok)
       => (lambda (m)
            (let ([r1 (rank-token->num (second m))]
                  [r2 (rank-token->num (third m))]
                  [r3 (rank-token->num (fourth m))]
                  [r4 (rank-token->num (fifth m))])
              (unless (= r1 r3) (error 'parse-range "Inconsistent high cards in range: ~a" tok))
              (let ([high r1]
                    [low1 (min r2 r4)]
                    [low2 (max r2 r4)])
                (for*/list ([r (in-range low1 (add1 low2))]
                            [hand (append (all-suited high r) (all-offsuit high r))])
                  hand))))]
      [(regexp-match #px"^([2-9TJQKA])([2-9TJQKA])$" tok)
       => (lambda (m)
            (let ([r1 (rank-token->num (second m))]
                  [r2 (rank-token->num (third m))])
              (append (all-suited r1 r2) (all-offsuit r1 r2))))]

      [else (error 'parse-range "Unknown range token: ~a" tok)])))

(define (parse-range str)
  (let* ([tokens (string-split str ",")]
         [hands (remove-duplicates (for*/list ([tok tokens]
                                               [hand (parse-single-range-token tok)])
                                     hand))])
    (range hands)))
