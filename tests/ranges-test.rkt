#lang racket

(require rackunit
         "../poker_trainer/cards.rkt"
         "../poker_trainer/ranges.rkt"
         "../poker_trainer/equity.rkt")

(test-case "parse-range pairs"
  (let ([r (parse-range "AA")])
    (check-equal? (length (range-hands r)) 6))
  (let ([r (parse-range "KK+")])
    (check-equal? (length (range-hands r)) 12)) ; AA, KK
  (let ([r (parse-range "QQ-AA")])
    (check-equal? (length (range-hands r)) 18))) ; QQ, KK, AA

(test-case "parse-range suited"
  (let ([r (parse-range "AKs")])
    (check-equal? (length (range-hands r)) 4))
  (let ([r (parse-range "AQs+")])
    (check-equal? (length (range-hands r)) 8)) ; AQs, AKs
  (let ([r (parse-range "AJs-AKs")])
    (check-equal? (length (range-hands r)) 12))) ; AJs, AQs, AKs

(test-case "parse-range offsuit"
  (let ([r (parse-range "AKo")])
    (check-equal? (length (range-hands r)) 12))
  (let ([r (parse-range "AQo+")])
    (check-equal? (length (range-hands r)) 24)) ; AQo, AKo
  (let ([r (parse-range "AJo-AKo")])
    (check-equal? (length (range-hands r)) 36))) ; AJo, AQo, AKo

(test-case "parse-range generic"
  (let ([r (parse-range "AK")])
    (check-equal? (length (range-hands r)) 16)) ; AKs(4) + AKo(12)
  (let ([r (parse-range "AQ+")])
    (check-equal? (length (range-hands r)) 32)) ; AQ(16) + AK(16)
  (let ([r (parse-range "AJ-AK")])
    (check-equal? (length (range-hands r)) 48))) ; AJ(16) + AQ(16) + AK(16)

(test-case "parse-range combinations"
  (let ([r (parse-range "AA, AKs, AKo")])
    (check-equal? (length (range-hands r)) 22))) ; 6 + 4 + 12 = 22

(test-case "equity with ranges"
  (let* ([hero (parse-cards "Ah As")]
         [board '()]
         [result (estimate-equity hero board '("KK") #:simulations 1000)])
    (printf "AA vs KK Equity: ~a\n" (equity-result-equity result))
    (check-true (> (equity-result-equity result) 0.75))
    (check-true (< (equity-result-equity result) 0.90))))

(test-case "equity with ranges and board"
  (let* ([hero (parse-cards "Ah Kh")]
         [board (parse-cards "Qh Jh 2d")]
         [result (estimate-equity hero board '("AA") #:simulations 100)])
    ;; Hero has royal flush draw vs AA.
    (check-true (> (equity-result-equity result) 0.3))))
