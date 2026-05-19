#lang racket

;; -----------------------------------------------------------------------------
;; Racket Poker Trainer GUI
;; -----------------------------------------------------------------------------
;; This is a study tool: it generates drills, scores your answers, and lets you
;; manually analyze saved/off-table poker spots. It does not read live poker
;; tables, overlay PokerStars, or automate play.
(require racket/class
         racket/draw
         racket/format
         racket/gui/base
         "poker_trainer/advice.rkt"
         "poker_trainer/cards.rkt"
         "poker_trainer/drills.rkt"
         "poker_trainer/equity.rkt"
         "poker_trainer/stats.rkt"
         "poker_trainer/visuals.rkt")

;; -----------------------------------------------------------------------------
;; Formatting helpers
;; -----------------------------------------------------------------------------
;; GUI labels should be friendly, so numbers and cards get small display helpers.
(define (percent value)
  (format "~a%" (~r (* value 100.0) #:precision 1)))

(define (cards->text cards)
  (if (empty? cards)
      "(none)"
      (string-join (map card->string cards) " ")))

;; Racket knows many named colors, but not every CSS color name. Keeping a small
;; RGB map here makes the visual palette predictable on this Linux install.
(define color-rgb
  (hash "felt green" '(11 92 55)
        "felt dark" '(6 49 34)
        "rail green" '(4 71 46)
        "panel cream" '(238 233 218)
        "warm card" '(255 252 241)
        "card shadow" '(33 32 28)
        "casino gold" '(218 171 64)
        "deep gold" '(138 94 24)
        "action green" '(25 145 75)
        "warning red" '(190 45 48)
        "deep green" '(5 83 46)
        "deep red" '(122 17 27)
        "charcoal" '(28 31 32)
        "muted ink" '(93 99 96)
        "slate gray" '(88 96 99)
        "white smoke" '(245 245 245)
        "light gray" '(211 211 211)
        "dim gray" '(105 105 105)
        "gray" '(128 128 128)
        "forest green" '(34 139 34)
        "dark green" '(0 100 0)
        "goldenrod" '(218 165 32)
        "dark goldenrod" '(184 134 11)
        "firebrick" '(178 34 34)
        "dark red" '(139 0 0)
        "white" '(255 255 255)
        "black" '(0 0 0)))

(define (->color name)
  (match (hash-ref color-rgb name #f)
    [(list r g b) (make-object color% r g b)]
    [#f (make-object color% name)]))

(define (solid-brush name)
  (make-object brush% (->color name) 'solid))

(define (solid-pen name [width 1])
  (make-object pen% (->color name) width 'solid))

;; -----------------------------------------------------------------------------
;; Drawing helpers
;; -----------------------------------------------------------------------------
;; These custom drawings are deliberately plain and readable. They are not copied
;; from PokerStars or any other poker room.
(define (draw-header dc)
  (send dc set-brush (solid-brush (surface-fill 'felt-dark)))
  (send dc set-pen (solid-pen (surface-fill 'felt-dark) 1))
  (send dc draw-rectangle 0 0 860 88)
  (send dc set-brush (solid-brush (surface-fill 'felt)))
  (send dc set-pen (solid-pen (accent-fill 'gold-dark) 3))
  (send dc draw-rounded-rectangle 12 12 836 62 18)
  (send dc set-text-foreground (->color (accent-fill 'gold)))
  (send dc set-font (make-object font% 24 'default 'normal 'bold))
  (send dc draw-text "POKER TRAINER" 34 24)
  (send dc set-text-foreground (->color "white"))
  (send dc set-font (make-object font% 11 'default 'normal 'bold))
  (send dc draw-text "oefeningen • pot odds • winkans • handanalyse" 286 33))

(define (draw-card dc x y c)
  (define w 56)
  (define h 74)
  (define rank (rank->string (card-rank c)))
  (define suit (suit-symbol (card-suit c)))
  (define ink (suit-ink (card-suit c)))
  (send dc set-brush (solid-brush (surface-fill 'card-shadow)))
  (send dc set-pen (solid-pen (surface-fill 'card-shadow) 1))
  (send dc draw-rounded-rectangle (+ x 3) (+ y 4) w h 8)
  (send dc set-brush (solid-brush (surface-fill 'card-face)))
  (send dc set-pen (solid-pen (accent-fill 'gold-dark) 1))
  (send dc draw-rounded-rectangle x y w h 8)
  (send dc set-pen (solid-pen "light gray" 1))
  (send dc draw-rounded-rectangle (+ x 3) (+ y 3) (- w 6) (- h 6) 6)
  (send dc set-text-foreground (->color ink))
  (send dc set-font (make-object font% 16 'default 'normal 'bold))
  (send dc draw-text rank (+ x 7) (+ y 5))
  (send dc set-font (make-object font% 28 'default 'normal 'bold))
  (send dc draw-text suit (+ x 16) (+ y 27))
  (send dc set-font (make-object font% 11 'default 'normal 'bold))
  (send dc draw-text rank (+ x 37) (+ y 55)))

(define (draw-card-strip dc cards)
  (send dc set-brush (solid-brush (surface-fill 'rail)))
  (send dc set-pen (solid-pen (accent-fill 'gold-dark) 2))
  (send dc draw-rounded-rectangle 0 0 820 94 14)
  (send dc set-brush (solid-brush (surface-fill 'felt)))
  (send dc set-pen (solid-pen (surface-fill 'felt-dark) 1))
  (send dc draw-rounded-rectangle 8 8 804 78 12)
  (if (empty? cards)
      (begin
        (send dc set-text-foreground (->color "white"))
        (send dc set-font (make-object font% 13 'default 'normal 'bold))
        (send dc draw-text "Nog geen tafelkaarten" 22 36))
      (for ([c cards] [i (in-naturals)])
        (draw-card dc (+ 18 (* i 68)) 10 c))))

(define (make-card-strip parent initial-cards)
  (define cards-box (box initial-cards))
  (define canvas
    (new canvas%
         [parent parent]
         [min-height 96]
         [stretchable-height #f]
         [paint-callback
          (lambda (_canvas dc)
            (draw-card-strip dc (unbox cards-box)))]))
  (values canvas
          (lambda (new-cards)
            (set-box! cards-box new-cards)
            (send canvas refresh))))

(define (draw-equity-meter dc equity required)
  (define w 820)
  (define h 58)
  (send dc set-brush (solid-brush (surface-fill 'rail)))
  (send dc set-pen (solid-pen (accent-fill 'gold-dark) 2))
  (send dc draw-rounded-rectangle 0 0 w h 12)
  (send dc set-brush (solid-brush (surface-fill 'felt-dark)))
  (send dc set-pen (solid-pen (surface-fill 'felt-dark) 1))
  (send dc draw-rounded-rectangle 10 10 (- w 20) (- h 20) 8)
  (if (and equity required)
      (let* ([zone (equity-zone equity required)]
             [inner-w (- w 20)]
             [inner-h (- h 20)]
             [equity-width (inexact->exact (round (* inner-w (min 1.0 (max 0.0 equity)))))]
             [required-x (+ 10 (inexact->exact (round (* inner-w (min 1.0 (max 0.0 required))))))]
             [label (format "Winkans ~a  |  Nodig ~a  |  ~a"
                            (percent equity)
                            (percent required)
                            (zone-title zone))])
        (send dc set-brush (solid-brush (zone-fill zone)))
        (send dc set-pen (solid-pen (zone-border zone) 1))
        (send dc draw-rounded-rectangle 10 10 equity-width inner-h 8)
        (send dc set-pen (solid-pen "black" 3))
        (send dc draw-line required-x 8 required-x (- h 8))
        (send dc set-text-foreground (->color (zone-ink zone)))
        (send dc set-font (make-object font% 13 'default 'normal 'bold))
        (send dc draw-text label 22 20))
      (begin
        (send dc set-text-foreground (->color "white"))
        (send dc set-font (make-object font% 12 'default 'normal 'bold))
        (send dc draw-text "Gebruik de analysator om de winkans te zien." 22 20))))

(define (wrap-text text max-chars)
  (define words (string-split text))
  (define-values (lines current)
    (for/fold ([lines '()] [current ""]) ([word words])
      (define candidate
        (if (string=? current "") word (string-append current " " word)))
      (if (> (string-length candidate) max-chars)
          (values (append lines (list current)) word)
          (values lines candidate))))
  (filter non-empty-string? (append lines (list current))))

(define (draw-feedback-card dc title best-label body zone)
  (define w 820)
  (define h 122)
  (send dc set-brush (solid-brush (surface-fill 'panel)))
  (send dc set-pen (solid-pen (zone-border zone) 3))
  (send dc draw-rounded-rectangle 0 0 w h 14)
  (send dc set-brush (solid-brush (zone-fill zone)))
  (send dc set-pen (solid-pen (zone-border zone) 1))
  (send dc draw-rounded-rectangle 10 10 220 38 10)
  (send dc set-text-foreground (->color (zone-ink zone)))
  (send dc set-font (make-object font% 13 'default 'normal 'bold))
  (send dc draw-text title 22 20)
  (send dc set-text-foreground (->color (accent-fill 'ink)))
  (send dc set-font (make-object font% 15 'default 'normal 'bold))
  (send dc draw-text (format "Beste keuze: ~a" best-label) 250 18)
  (send dc set-font (make-object font% 12 'default))
  (for ([line (wrap-text body 108)] [i (in-naturals)])
    (send dc draw-text line 18 (+ 60 (* i 18)))))

;; -----------------------------------------------------------------------------
;; Mutable GUI state
;; -----------------------------------------------------------------------------
;; The GUI keeps only the current drill and a simple score. The poker logic stays
;; in the modules under poker_trainer/.
(define current-spot (box (random-drill)))
(define chosen-action (box #f))
(define answer-revealed? (box #f))
(define feedback-title (box "Kies een actie"))
(define feedback-best-label (box ""))
(define feedback-body (box "Kies wat jij zou doen. Dan laat de trainer het juiste antwoord zien."))
(define feedback-zone (box 'neutral))
(define correct-count 0)
(define total-count 0)

;; -----------------------------------------------------------------------------
;; Top-level window
;; -----------------------------------------------------------------------------
;; Racket's GUI toolkit is cross-platform; on Linux it creates a normal desktop
;; window that you can keep beside notes, videos, or hand histories.
(define frame
  (new frame%
       [label "Poker Trainer"]
       [width 900]
       [height 860]))

(define root
  (new vertical-panel%
       [parent frame]
       [alignment '(left top)]
       [spacing 10]
       [border 12]))

(define header-canvas
  (new canvas%
       [parent root]
       [min-height 90]
       [stretchable-height #f]
       [paint-callback (lambda (_canvas dc) (draw-header dc))]))

(new message%
     [parent root]
     [label "Poker leerhulp voor oefeningen en handanalyse. Niet voor gebruik aan de speeltafel."]
     [auto-resize #t])

;; -----------------------------------------------------------------------------
;; Drill panel
;; -----------------------------------------------------------------------------
;; This section asks one poker decision at a time and records whether your answer
;; matched the built-in microstakes baseline.
(define drill-panel
  (new group-box-panel%
       [parent root]
       [label "Oefening"]
       [alignment '(left top)]
       [spacing 6]))

(define drill-title
  (new message% [parent drill-panel] [label ""] [auto-resize #t]))

(new message% [parent drill-panel] [label "Jouw hand"] [auto-resize #t])

(define-values (_drill-hand-canvas set-drill-hand-cards!)
  (make-card-strip drill-panel '()))

(new message% [parent drill-panel] [label "Tafelkaarten"] [auto-resize #t])

(define-values (_drill-board-canvas set-drill-board-cards!)
  (make-card-strip drill-panel '()))

(define drill-situation
  (new message% [parent drill-panel] [label ""] [auto-resize #t]))

(define drill-equity-label
  (new message% [parent drill-panel] [label ""] [auto-resize #t]))

(define drill-score
  (new message% [parent drill-panel] [label "Score: 0 / 0"] [auto-resize #t]))

(define drill-feedback
  (new canvas%
       [parent drill-panel]
       [min-height 126]
       [stretchable-height #f]
       [paint-callback
        (lambda (_canvas dc)
          (draw-feedback-card dc
                              (unbox feedback-title)
                              (unbox feedback-best-label)
                              (unbox feedback-body)
                              (unbox feedback-zone)))]))

(define drill-buttons
  (new horizontal-panel%
       [parent drill-panel]
       [spacing 8]
       [stretchable-height #f]))

(define action-tiles '())

;; Paint one clickable action tile. Green means the best answer after reveal,
;; red means the wrong clicked answer, and gray means unrevealed/neutral.
(define (paint-action-tile dc action)
  (define spot (unbox current-spot))
  (define zone
    (action-tile-zone action
                      (best-action-for-spot spot)
                      (unbox chosen-action)
                      (unbox answer-revealed?)))
  (define revealed? (unbox answer-revealed?))
  (define winrate (and revealed? (action-csv-winrate spot action)))
  (send dc set-brush (solid-brush (zone-fill zone)))
  (send dc set-pen (solid-pen (zone-border zone) 2))
  (send dc draw-rounded-rectangle 0 0 142 72 8)
  (send dc set-text-foreground (->color (zone-ink zone)))
  (send dc set-font (make-object font% 10 'default 'normal 'bold))
  (send dc draw-text (zone-title zone) 12 9)
  (send dc set-font (make-object font% 16 'default 'normal 'bold))
  (send dc draw-text (action-label action) 12 30)
  (when winrate
    (send dc set-font (make-object font% 9 'default))
    (send dc draw-text (format "winst: ~a" (percent winrate)) 12 50)))

;; Native buttons are hard to color consistently on Linux themes, so these
;; canvases behave like buttons while letting the trainer paint clear colors.
(define (make-action-tile parent action)
  (new
   (class canvas%
     (inherit refresh)
     (super-new [parent parent]
                [min-width 146]
                [min-height 76]
                [stretchable-width #f]
                [stretchable-height #f])
     (define/override (on-event event)
       (when (and (send event button-up? 'left)
                  (not (unbox answer-revealed?)))
         (answer-drill! action)))
     (define/override (on-paint)
       (define dc (send this get-dc))
       (paint-action-tile dc action)))))

;; Update every drill label from the current boxed drill.
(define (render-drill!)
  (define spot (unbox current-spot))
  (define pot (drill-spot-pot spot))
  (define to-call (drill-spot-to-call spot))
  (define required (pot-odds pot to-call))
  (define odds-str
    (if (zero? to-call)
        "Gratis (check/bet)"
        (format "Te betalen: ~a | Benodigde kans: ~a" to-call (percent required))))
  (send drill-title set-label (format "Situatie: ~a" (drill-spot-title spot)))
  (set-drill-hand-cards! (drill-spot-hand spot))
  (set-drill-board-cards! (drill-spot-board spot))
  (send drill-situation set-label
        (format "Pot: ~a | ~a | Spelers: ~a | Veelgemaakte fout: ~a"
                pot odds-str
                (drill-spot-players spot)
                (drill-spot-leak-category spot)))
  (send drill-equity-label set-label
        (if (unbox answer-revealed?) "" "Winkans: wordt berekend na jouw keuze..."))
  (send drill-score set-label (format "Score: ~a / ~a" correct-count total-count))
  (send drill-feedback refresh)
  (for ([tile action-tiles])
    (send tile refresh)))

(define (action-csv-winrate spot action)
  (define key (spot-key (drill-spot-title spot)))
  (define rows (rows-for-key default-action-outcomes key))
  (define matching (filter (lambda (r) (eq? (action-outcome-action r) action)) rows))
  (if (empty? matching) #f (outcome-win-rate (first matching))))

;; Score an answer, show the explanation, and reveal red/green action colors.
;; Equity simulation runs in a background thread so the GUI stays responsive.
(define (answer-drill! action)
  (define spot (unbox current-spot))
  (define result (check-answer spot action))
  (set-box! chosen-action action)
  (set-box! answer-revealed? #t)
  (set! total-count (add1 total-count))
  (when (answer-result-correct? result)
    (set! correct-count (add1 correct-count)))
  (set-box! feedback-title (if (answer-result-correct? result) "GOED" "FOUT"))
  (set-box! feedback-best-label (action-label (best-action-for-spot spot)))
  (set-box! feedback-body (answer-result-explanation result))
  (set-box! feedback-zone (if (answer-result-correct? result) 'best 'avoid))
  (send drill-equity-label set-label "Winkans berekenen...")
  (render-drill!)
  (thread
   (lambda ()
     (define hero (drill-spot-hand spot))
     (define board (drill-spot-board spot))
     (define players (drill-spot-players spot))
     (define pot (drill-spot-pot spot))
     (define to-call (drill-spot-to-call spot))
     (define eq-result
       (with-handlers ([exn:fail? (lambda (_) #f)])
         (estimate-equity hero board players #:simulations 2000)))
     (define label-text
       (if eq-result
           (let* ([eq-pct (percent (equity-result-equity eq-result))]
                  [req-pct (percent (pot-odds pot to-call))]
                  [verdict (if (>= (equity-result-equity eq-result) (pot-odds pot to-call))
                               "→ doorgaan is winstgevend"
                               "→ folden bespaart geld")])
             (if (zero? to-call)
                 (format "Jouw winkans: ~a (gratis hand — inzetten voor waarde)" eq-pct)
                 (format "Jouw winkans: ~a | Benodigde kans: ~a ~a" eq-pct req-pct verdict)))
           "Winkans: kon niet berekenen"))
     (send drill-equity-label set-label label-text))))

(for ([action '(fold call raise)])
  (set! action-tiles (append action-tiles (list (make-action-tile drill-buttons action)))))

(new button%
     [parent drill-buttons]
     [label "VOLGENDE"]
     [callback (lambda (_button _event)
                 (set-box! current-spot (random-drill))
                 (set-box! chosen-action #f)
                 (set-box! answer-revealed? #f)
                 (set-box! feedback-title "Kies een actie")
                 (set-box! feedback-best-label "")
                 (set-box! feedback-body
                           "Kies wat jij zou doen. Dan laat de trainer het juiste antwoord zien.")
                 (set-box! feedback-zone 'neutral)
                 (render-drill!))])

;; -----------------------------------------------------------------------------
;; Analyzer panel
;; -----------------------------------------------------------------------------
;; This section lets you manually type a saved or off-table hand and estimate
;; equity. It is intentionally manual so it remains a study tool.
(define analyzer-panel
  (new group-box-panel%
       [parent root]
       [label "Kansberekening"]
       [alignment '(left top)]
       [spacing 6]))

(define hand-field
  (new text-field%
       [parent analyzer-panel]
       [label "Jouw hand"]
       [init-value "Ah Kh"]))

(define board-field
  (new text-field%
       [parent analyzer-panel]
       [label "Tafelkaarten"]
       [init-value "Qh Jh 2c"]))

(define pot-field
  (new text-field%
       [parent analyzer-panel]
       [label "Pot"]
       [init-value "20"]))

(define call-field
  (new text-field%
       [parent analyzer-panel]
       [label "Te betalen"]
       [init-value "5"]))

(define players-field
  (new text-field%
       [parent analyzer-panel]
       [label "Spelers (inclusief jij)"]
       [init-value "2"]))

(define simulations-field
  (new text-field%
       [parent analyzer-panel]
       [label "Simulaties"]
       [init-value "3000"]))

(define analyzer-result
  (new message%
       [parent analyzer-panel]
       [label "Voer een hand in en druk op Analyseer."]
       [auto-resize #t]))

(new message% [parent analyzer-panel] [label "Kaartweergave"] [auto-resize #t])

(define-values (_analyzer-card-canvas set-analyzer-cards!)
  (make-card-strip analyzer-panel '()))

(define latest-equity (box #f))
(define latest-required (box #f))

(define analyzer-bar
  (new canvas%
       [parent analyzer-panel]
       [min-height 52]
       [stretchable-height #f]
       [paint-callback
        (lambda (_canvas dc)
          (draw-equity-meter dc (unbox latest-equity) (unbox latest-required)))]))

;; Convert a numeric text field or raise a useful GUI-facing error.
(define (field-number field name)
  (define value (string->number (send field get-value)))
  (unless value
    (error 'analyzer "expected a number for ~a" name))
  value)

;; Run the Monte Carlo estimate and show both equity and pot-odds advice.
(define (run-analysis!)
  (with-handlers ([exn:fail?
                   (lambda (err)
                     (set-box! latest-equity #f)
                     (set-box! latest-required #f)
                     (send analyzer-bar refresh)
                     (send analyzer-result set-label
                           (format "Fout: ~a" (exn-message err))))])
    (send analyzer-result set-label "Berekening bezig...")
    (define hero (parse-cards (send hand-field get-value)))
    (define board (parse-cards (send board-field get-value)))
    (set-analyzer-cards! (append hero board))
    (define pot (field-number pot-field "pot"))
    (define to-call (field-number call-field "to call"))
    (define players (inexact->exact (round (field-number players-field "players"))))
    (define simulations
      (max 100 (inexact->exact (round (field-number simulations-field "simulations")))))
    (define equity (estimate-equity hero board players #:simulations simulations))
    (define rec (recommend-action (equity-result-equity equity) pot to-call))
    (set-box! latest-equity (equity-result-equity equity))
    (set-box! latest-required (recommendation-required-equity rec))
    (send analyzer-bar refresh)
    (send analyzer-result set-label
          (format "Winst/Gelijk/Verlies: ~a/~a/~a | Advies: ~a. ~a"
                  (equity-result-wins equity)
                  (equity-result-ties equity)
                  (equity-result-losses equity)
                  (action-label (recommendation-action rec))
                  (recommendation-reason rec)))))

(new button%
     [parent analyzer-panel]
     [label "ANALYSEER"]
     [callback (lambda (_button _event) (run-analysis!))])

;; -----------------------------------------------------------------------------
;; Startup
;; -----------------------------------------------------------------------------
;; Rendering before showing the frame gives the first drill immediately.
(render-drill!)

(module+ main
  (send frame show #t))
