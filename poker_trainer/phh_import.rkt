#lang racket

;; -----------------------------------------------------------------------------
;; PHH hand-history importer
;; -----------------------------------------------------------------------------
;; PHH is a public, machine-friendly poker hand-history format. This importer
;; reads existing PHH files and converts no-limit Texas Hold'em preflop actions
;; and postflop actions into the aggregate CSV shape used by stats.rkt.
;;
;; The first pass is intentionally conservative:
;; - variant must be "NT" (no-limit Texas Hold'em)
;; - finishing_stacks must exist, so win/loss/EV comes from the real result
;; - imported keys include broad preflop buckets plus richer postflop features
(provide (struct-out parsed-hand)
         (struct-out history-action-outcome)
         parse-phh-text
         phh-text->outcomes
         phh-file->outcomes
         phh-directory->outcomes
         aggregate-history-outcomes
         write-history-outcomes-csv
         hole-cards->history-key
         board-texture-key
         pressure-key)

(require racket/list
         racket/string
         "cards.rkt"
         "evaluator.rkt")

(struct parsed-hand
  (variant min-bet starting-stacks finishing-stacks actions hole-cards)
  #:transparent)

(struct history-action-outcome
  (key action trials wins losses ev-bb)
  #:transparent)

;; -----------------------------------------------------------------------------
;; Text extraction helpers
;; -----------------------------------------------------------------------------
;; PHH is TOML-like. For this importer we only need a few scalar/list fields and
;; quoted action strings, so a small parser is easier to audit than a dependency.
(define (quoted-values line)
  (for/list ([match (regexp-match* #px"'[^']*'|\"[^\"]*\"" line)])
    (substring match 1 (sub1 (string-length match)))))

(define (extract-string-field lines field)
  (define rx (pregexp (format "^\\s*~a\\s*=\\s*['\"]([^'\"]+)['\"]" (regexp-quote field))))
  (for/or ([line lines])
    (define m (regexp-match rx line))
    (and m (second m))))

(define (extract-number-field lines field [default 0])
  (define rx (pregexp (format "^\\s*~a\\s*=\\s*([-0-9.]+)" (regexp-quote field))))
  (or
   (for/or ([line lines])
     (define m (regexp-match rx line))
     (and m (string->number (second m))))
   default))

(define (extract-number-list-field lines field)
  (define rx (pregexp (format "^\\s*~a\\s*=\\s*\\[([^\\]]*)\\]" (regexp-quote field))))
  (or
   (for/or ([line lines])
     (define m (regexp-match rx line))
     (and m
          (for/list ([part (string-split (second m) ",")]
                     #:when (not (string=? (string-trim part) "")))
            (string->number (string-trim part)))))
   '()))

(define (extract-actions lines)
  (define-values (actions _inside?)
    (for/fold ([actions '()] [inside? #f]) ([line lines])
      (define starts? (regexp-match? #px"^\\s*actions\\s*=" line))
      (define now-inside? (or inside? starts?))
      (define new-actions
        (if now-inside?
            (append actions (quoted-values line))
            actions))
      (define ends? (and now-inside? (regexp-match? #px"\\]" line)))
      (values new-actions (and now-inside? (not ends?)))))
  actions)

;; -----------------------------------------------------------------------------
;; Action parsing
;; -----------------------------------------------------------------------------
;; PHH action examples:
;;   d dh p1 Ac2d    ; deal hole cards
;;   p3 f            ; fold
;;   p2 cc           ; call/check
;;   p1 cbr 35000    ; complete/bet/raise
(define (hole-deal-action? action)
  (regexp-match #px"^d\\s+dh\\s+p([0-9]+)\\s+([2-9TJQKA][cdhs][2-9TJQKA][cdhs])" action))

(define (player-decision-action action)
  (define m (regexp-match #px"^p([0-9]+)\\s+(f|cc|cbr)(?:\\s|$)" action))
  (and m
       (let ([player-index (sub1 (string->number (second m)))]
             [token (third m)]
             [amount-match (regexp-match #px"\\s([0-9.]+)\\s*$" action)])
         (list player-index
               (case (string->symbol token)
                 [(f) 'fold]
                 [(cc) 'call]
                 [(cbr) 'raise])
               (and amount-match (string->number (second amount-match)))))))

(define (board-deal-action action)
  (define m (regexp-match #px"^d\\s+db\\s+((?:[2-9TJQKA][cdhs])+)" action))
  (and m (second m)))

(define (compact-board->cards compact)
  (for/list ([i (in-range 0 (string-length compact) 2)])
    (parse-card (substring compact i (+ i 2)))))

(define (extract-hole-cards actions)
  (for/hash ([action actions]
             #:when (hole-deal-action? action))
    (define m (hole-deal-action? action))
    (values (sub1 (string->number (second m))) (third m))))

;; -----------------------------------------------------------------------------
;; Hole-card buckets
;; -----------------------------------------------------------------------------
;; These keys intentionally match or complement built-in drill keys.
(define broadway-ranks '(10 11 12 13 14))

(define (two-card-list compact)
  (unless (= (string-length compact) 4)
    (error 'hole-cards->history-key "expected two-card compact hand like KcJd"))
  (list (parse-card (substring compact 0 2))
        (parse-card (substring compact 2 4))))

(define (suited? cards)
  (eq? (card-suit (first cards)) (card-suit (second cards))))

(define (paired? cards)
  (= (card-rank (first cards)) (card-rank (second cards))))

(define (connected? cards)
  (= 1 (abs (- (card-rank (first cards)) (card-rank (second cards))))))

(define (both-broadway? cards)
  (andmap (lambda (rank) (member rank broadway-ranks)) (map card-rank cards)))

(define (hole-cards->history-key compact)
  (define cards (two-card-list compact))
  (define ranks (sort (map card-rank cards) >))
  (cond
    [(and (paired? cards) (>= (first ranks) 11))
     "preflop-premium-pair"]
    [(and (member 14 ranks) (member 13 ranks))
     "preflop-ace-king"]
    [(and (both-broadway? cards) (not (suited? cards)))
     "dominated-offsuit-broadway-out-of-position"]
    [(and (suited? cards) (connected? cards))
     "preflop-suited-connector"]
    [(member 14 ranks)
     "preflop-ace-x"]
    [else
     "preflop-other"]))

;; -----------------------------------------------------------------------------
;; Rich postflop feature keys
;; -----------------------------------------------------------------------------
(define (street-key board)
  (case (length board)
    [(0) "preflop"]
    [(3) "flop"]
    [(4) "turn"]
    [(5) "river"]
    [else "unknown"]))

(define (rank-gaps ranks)
  (define sorted (sort (remove-duplicates ranks) >))
  (for/list ([a sorted] [b (rest sorted)])
    (- a b)))

(define (board-texture-key board)
  (if (< (length board) 3)
      "none"
      (let* ([suit-counts
              (for/fold ([counts (hash)]) ([c board])
                (hash-update counts (card-suit c) add1 0))]
             [max-suit (apply max (hash-values suit-counts))]
             [ranks (map card-rank board)]
             [gaps (rank-gaps ranks)]
             [paired-board? (< (length (remove-duplicates ranks)) (length ranks))]
             [connected-board? (ormap (lambda (gap) (<= gap 2)) gaps)])
        (cond
          [(or (>= max-suit 3) connected-board?) "wet"]
          [paired-board? "paired"]
          [else "dry"]))))

(define (position-key player-index player-count)
  (cond
    [(zero? player-index) "early"]
    [(= player-index (sub1 player-count)) "late"]
    [else "middle"]))

(define (pressure-key action amount pot min-bet)
  (cond
    [(eq? action 'fold) "fold"]
    [(not amount) "none"]
    [(<= pot 0) "open"]
    [else
     (define ratio (/ amount (max min-bet pot)))
     (cond
       [(>= ratio 1.0) "large"]
       [(>= ratio 0.5) "medium"]
       [else "small"])]))

(define (hand-category-key hero-cards board)
  (if (< (length board) 3)
      (hole-cards->history-key (string-append (card->string (first hero-cards))
                                              (card->string (second hero-cards))))
      (let ([name (hand-category-name
                   (hand-category (evaluate-seven (append hero-cards board))))])
        (string-append "hand-" (regexp-replace* #px"\\s+" name "-")))))

(define (history-spot-key compact board player-index player-count action amount pot min-bet)
  (define hero-cards (two-card-list compact))
  (define street (street-key board))
  (if (string=? street "preflop")
      (hole-cards->history-key compact)
      (string-join
       (list (format "street-~a" street)
             (format "texture-~a" (board-texture-key board))
             (hand-category-key hero-cards board)
             (format "position-~a" (position-key player-index player-count))
             (format "pressure-~a" (pressure-key action amount pot min-bet)))
       "-")))

;; -----------------------------------------------------------------------------
;; PHH conversion
;; -----------------------------------------------------------------------------
;; A player wins a sample if their finishing stack is higher than their starting
;; stack. EV is normalized by min_bet so different blind levels can be combined.
(define (parse-phh-text text)
  (define lines (string-split text "\n"))
  (define actions (extract-actions lines))
  (parsed-hand
   (or (extract-string-field lines "variant") "")
   (extract-number-field lines "min_bet" 1)
   (extract-number-list-field lines "starting_stacks")
   (extract-number-list-field lines "finishing_stacks")
   actions
   (extract-hole-cards actions)))

(define (player-delta hand player-index)
  (- (list-ref (parsed-hand-finishing-stacks hand) player-index)
     (list-ref (parsed-hand-starting-stacks hand) player-index)))

(define (pot-after-action pot action amount)
  (cond
    [(and amount (member action '(call raise))) (+ pot amount)]
    [else pot]))

(define (append-board board compact)
  (append board (compact-board->cards compact)))

(define (phh-text->outcomes text)
  (define hand (parse-phh-text text))
  (if (or (not (string=? (parsed-hand-variant hand) "NT"))
          (empty? (parsed-hand-finishing-stacks hand)))
      '()
      (let ([player-count (length (parsed-hand-starting-stacks hand))]
            [min-bet (max 1 (parsed-hand-min-bet hand))])
        (define-values (rows _board _pot)
          (for/fold ([rows '()] [board '()] [pot 0])
                    ([raw-action (parsed-hand-actions hand)])
            (define dealt-board (board-deal-action raw-action))
            (define decision (player-decision-action raw-action))
            (cond
              [dealt-board
               (values rows (append-board board dealt-board) pot)]
              [decision
               (define player-index (first decision))
               (define action-kind (second decision))
               (define amount (third decision))
               (define compact (hash-ref (parsed-hand-hole-cards hand) player-index #f))
               (if compact
                   (let* ([delta (player-delta hand player-index)]
                          [key (history-spot-key compact
                                                 board
                                                 player-index
                                                 player-count
                                                 action-kind
                                                 amount
                                                 pot
                                                 min-bet)]
                          [row (history-action-outcome
                                key
                                action-kind
                                1
                                (if (> delta 0) 1 0)
                                (if (< delta 0) 1 0)
                                (exact->inexact (/ delta min-bet)))])
                     (values (cons row rows)
                             board
                             (pot-after-action pot action-kind amount)))
                   (values rows board (pot-after-action pot action-kind amount)))]
              [else
               (values rows board pot)])))
        (reverse rows))))

(define (phh-file->outcomes path)
  (phh-text->outcomes (file->string path)))

(define (phh-directory->outcomes directory)
  (define phh-files
    (filter (lambda (path) (regexp-match? #px"\\.phh$" (path->string path)))
            (directory-list directory #:build? #t)))
  (append-map phh-file->outcomes phh-files))

;; -----------------------------------------------------------------------------
;; Aggregation and export
;; -----------------------------------------------------------------------------
(define (aggregate-key row)
  (list (history-action-outcome-key row)
        (history-action-outcome-action row)))

(define (merge-row row by-key)
  (define key (aggregate-key row))
  (define existing (hash-ref by-key key #f))
  (hash-set
   by-key
   key
   (if existing
       (history-action-outcome
        (history-action-outcome-key row)
        (history-action-outcome-action row)
        (+ (history-action-outcome-trials existing)
           (history-action-outcome-trials row))
        (+ (history-action-outcome-wins existing)
           (history-action-outcome-wins row))
        (+ (history-action-outcome-losses existing)
           (history-action-outcome-losses row))
        (+ (history-action-outcome-ev-bb existing)
           (history-action-outcome-ev-bb row)))
       row)))

(define (aggregate-history-outcomes rows)
  (sort
   (hash-values (foldl merge-row (hash) rows))
   (lambda (a b)
     (string<? (format "~a-~a"
                       (history-action-outcome-key a)
                       (history-action-outcome-action a))
               (format "~a-~a"
                       (history-action-outcome-key b)
                       (history-action-outcome-action b))))))

(define (csv-line row)
  (format "~a,~a,~a,~a,~a,~a"
          (history-action-outcome-key row)
          (history-action-outcome-action row)
          (history-action-outcome-trials row)
          (history-action-outcome-wins row)
          (history-action-outcome-losses row)
          (real->decimal-string (history-action-outcome-ev-bb row) 3)))

(define (write-history-outcomes-csv rows output-path)
  (call-with-output-file output-path
    (lambda (out)
      (displayln "spot-key,action,trials,wins,losses,ev-bb" out)
      (for ([row rows])
        (displayln (csv-line row) out)))
    #:exists 'replace))
