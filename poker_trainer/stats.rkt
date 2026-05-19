#lang racket

;; -----------------------------------------------------------------------------
;; Data-backed action outcome stats
;; -----------------------------------------------------------------------------
;; This module turns aggregate hand-history outcomes into a training signal.
;; It expects already-aggregated rows rather than raw live-table data:
;;
;; spot-key,action,trials,wins,losses,ev-bb
;;
;; The algorithm is deliberately conservative. It ranks actions by a Wilson-style
;; lower confidence bound so tiny lucky samples do not dominate larger samples.
(provide (struct-out action-outcome)
         (struct-out data-decision)
         spot-key
         parse-outcome-row
         load-action-outcomes
         outcome-win-rate
         confidence-score
         rows-for-key
         best-action-for-key
         decision-explanation)

(require racket/list
         racket/string)

(struct action-outcome (key action trials wins losses ev-bb) #:transparent)
(struct data-decision (action source explanation outcomes) #:transparent)

;; -----------------------------------------------------------------------------
;; Key normalization
;; -----------------------------------------------------------------------------
;; Human drill titles are converted into stable CSV keys.
(define (spot-key text)
  (define lowered (string-downcase text))
  (define cleaned
    (regexp-replace* #rx"[^a-z0-9]+" lowered "-"))
  (string-trim cleaned "-"))

;; -----------------------------------------------------------------------------
;; CSV parsing
;; -----------------------------------------------------------------------------
;; The seed file uses simple comma-separated values without quoted commas. This is
;; enough for aggregate numeric data and keeps the Racket code easy to read.
(define (->action text)
  (define sym (string->symbol (string-downcase (string-trim text))))
  (unless (member sym '(fold call raise))
    (error 'parse-outcome-row "unknown action: ~a" text))
  sym)

(define (->number text field-name)
  (define value (string->number (string-trim text)))
  (unless value
    (error 'parse-outcome-row "expected number for ~a: ~a" field-name text))
  value)

(define (parse-outcome-row line)
  (define parts (map string-trim (string-split line ",")))
  (unless (= (length parts) 6)
    (error 'parse-outcome-row "expected 6 columns, got: ~a" line))
  (match parts
    [(list key action trials wins losses ev-bb)
     (action-outcome key
                     (->action action)
                     (inexact->exact (->number trials "trials"))
                     (inexact->exact (->number wins "wins"))
                     (inexact->exact (->number losses "losses"))
                     (exact->inexact (->number ev-bb "ev-bb")))]))

(define (data-line? line)
  (define trimmed (string-trim line))
  (and (not (string=? trimmed ""))
       (not (string-prefix? trimmed "#"))
       (not (string-prefix? (string-downcase trimmed) "spot-key,"))))

(define (load-action-outcomes path)
  (if (file-exists? path)
      (map parse-outcome-row
           (filter data-line? (file->lines path)))
      '()))

;; -----------------------------------------------------------------------------
;; Outcome math
;; -----------------------------------------------------------------------------
;; Win rate is useful, but raw win rate is too optimistic for small samples.
(define (outcome-win-rate outcome)
  (if (zero? (action-outcome-trials outcome))
      0.0
      (exact->inexact
       (/ (action-outcome-wins outcome)
          (action-outcome-trials outcome)))))

;; Wilson lower bound for a binomial proportion. z=1.28 is roughly an 80%
;; one-sided confidence level, chosen to avoid overfitting while still letting
;; useful sample data influence training decisions.
(define (confidence-score outcome)
  (define n (action-outcome-trials outcome))
  (if (zero? n)
      -inf.0
      (let* ([p (outcome-win-rate outcome)]
             [z 1.28]
             [z2 (* z z)]
             [denominator (+ 1 (/ z2 n))]
             [center (+ p (/ z2 (* 2 n)))]
             [spread (* z (sqrt (/ (+ (* p (- 1 p)) (/ z2 (* 4 n))) n)))])
        (/ (- center spread) denominator))))

(define (rows-for-key outcomes key)
  (filter (lambda (row) (string=? (action-outcome-key row) key)) outcomes))

(define (outcome-score outcome)
  ;; EV is a small tie-breaker after confidence-adjusted win rate.
  (+ (confidence-score outcome)
     (* 0.0001 (action-outcome-ev-bb outcome))))

(define (best-outcome rows)
  (argmax outcome-score rows))

;; -----------------------------------------------------------------------------
;; Decision generation
;; -----------------------------------------------------------------------------
;; If data exists for the spot, use it. Otherwise keep the built-in training rule.
(define (best-action-for-key outcomes key fallback-action)
  (define rows (rows-for-key outcomes key))
  (if (empty? rows)
      (data-decision
       fallback-action
       'fallback
       "Geen data gevonden voor deze situatie. Antwoord gebaseerd op ingebouwde trainingsregel."
       '())
      (let ([best (best-outcome rows)])
        (data-decision
         (action-outcome-action best)
         'data
         (decision-explanation best rows)
         rows))))

(define (decision-explanation best rows)
  (define total (apply + (map action-outcome-trials rows)))
  (format
   "Op basis van data: ~a heeft het beste resultaat in deze situatie (~a handen totaal, ~a voor deze actie, ~a winkans, EV ~a bb)."
   (string-upcase (symbol->string (action-outcome-action best)))
   total
   (action-outcome-trials best)
   (format "~a%" (real->decimal-string (* 100 (outcome-win-rate best)) 1))
   (real->decimal-string (action-outcome-ev-bb best) 1)))
