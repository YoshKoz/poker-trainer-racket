#lang racket

;; -----------------------------------------------------------------------------
;; Training drills
;; -----------------------------------------------------------------------------
;; These are intentionally opinionated microstakes spots. They train the baseline
;; habits that beat loose/passive pools: value bet clearly, fold dominated trash,
;; and continue when the price is good.
(provide (struct-out drill-spot)
         (struct-out answer-result)
         default-drills
         default-action-outcomes
         best-decision-for-spot
         best-action-for-spot
         check-answer
         random-drill)

(require racket/runtime-path
         "cards.rkt"
         "stats.rkt"
         "visuals.rkt")

(define-runtime-path default-outcomes-path "../data/action-outcomes.csv")
(define-runtime-path generated-outcomes-path "../data/generated/phh-action-outcomes.csv")

(struct drill-spot
  (title hand board pot to-call players correct-action explanation leak-category)
  #:transparent)

(struct answer-result (correct? explanation) #:transparent)

;; -----------------------------------------------------------------------------
;; Aggregate data source
;; -----------------------------------------------------------------------------
;; This is the local seed dataset. It can be regenerated from public or personal
;; hand histories later; the trainer only needs the aggregate CSV shape.
(define default-action-outcomes
  (append (load-action-outcomes default-outcomes-path)
          (load-action-outcomes generated-outcomes-path)))

;; -----------------------------------------------------------------------------
;; Built-in drill bank
;; -----------------------------------------------------------------------------
;; Keeping these in code makes the first version easy to inspect and edit.
(define default-drills
  (list
   (drill-spot
    "Jij hebt het beste paar — tegenstander speelt losjes"
    (parse-cards "Ah Kd")
    (parse-cards "Ks 7c 2d")
    18
    0
    2
    'raise
    "Inzetten! Je hebt een koning op tafel met de beste bijkaart (aas). Losse spelers callen dit te vaak met een slechtere hand. Pak het geld."
    "gemiste waarde")
   (drill-spot
    "Jij moet geld betalen om mee te doen — maar je hand is zwak"
    (parse-cards "Kc Jd")
    '()
    3
    3
    2
    'fold
    "Folden. Koning-boer is een hand die er goed uitziet maar vaak verliest als de tegenstander raises. Je bent als eerste aan de beurt (slechte positie) en moet al geld betalen. Gooi weg."
    "te losse preflop call")
   (drill-spot
    "Bijna een flush — meespelen kost maar weinig"
    (parse-cards "Ah Jh")
    (parse-cards "Kh 7h 2c")
    20
    5
    2
    'call
    "Callen. Je hebt vier harten en hebt nog één nodig voor een flush. De pot is 20, je betaalt maar 5 — dat is goedkoop genoeg. Plus: als de flush niet komt, heb je nog de aas als sterke kaart."
    "pot odds")
   (drill-spot
    "Jij hebt een klein paar — tegenstander zet veel in"
    (parse-cards "8c 8d")
    (parse-cards "As Qh 9s 2d")
    24
    20
    2
    'fold
    "Folden. Je hebt een paar achten, maar op tafel liggen een aas, koningin en negen — allemaal hoger. Als de tegenstander zoveel inzet (bijna de hele pot), klopt het zelden met alleen een klein paar. Wegleggen."
    "te veel callen")
   (drill-spot
    "Jij hebt het sterkste paar — tafel is veilig"
    (parse-cards "Qh Qc")
    (parse-cards "7s 4d 2c")
    16
    0
    2
    'raise
    "Inzetten! Je hebt twee koninginnen en op tafel liggen alleen lage kaarten (7, 4, 2). Niemand maakt snel iets beters. Zet in om de pot te laten groeien — tegenstanders met een slechter paar of een losse hoge kaart betalen te snel mee."
    "gemiste waarde")))

;; -----------------------------------------------------------------------------
;; Answer checking
;; -----------------------------------------------------------------------------
;; The GUI stores progress by comparing the clicked action with the data-backed
;; decision when aggregate rows exist, or the built-in rule otherwise.
(define (best-decision-for-spot spot)
  (best-action-for-key default-action-outcomes
                       (spot-key (drill-spot-title spot))
                       (drill-spot-correct-action spot)))

(define (best-action-for-spot spot)
  (data-decision-action (best-decision-for-spot spot)))

(define (check-answer spot action)
  (define decision (best-decision-for-spot spot))
  (define best-action (data-decision-action decision))
  (define data-note (data-decision-explanation decision))
  (define explanation
    (if (eq? (data-decision-source decision) 'data)
        (format "~a Trainingstip: ~a" data-note (drill-spot-explanation spot))
        (drill-spot-explanation spot)))
  (if (eq? action best-action)
      (answer-result #t explanation)
      (answer-result
       #f
       (format "Niet goed. Beste keuze: ~a. ~a"
               (action-label best-action)
               explanation))))

;; Pick a drill from the bank. The GUI can call this after every answer.
(define (random-drill)
  (list-ref default-drills (random (length default-drills))))
