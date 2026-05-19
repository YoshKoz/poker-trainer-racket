#lang racket

(require "phh_import.rkt"
         "cards.rkt"
         racket/list
         racket/string)

(provide parse-pokerstars-hand)

;; Regexes for PokerStars format
(define header-rx #px"PokerStars Hand #(\\d+): +Hold'em No Limit \\(\\$([0-9.]+)/\\$([0-9.]+) USD\\)")
(define seat-rx #px"^Seat (\\d+): (.*?) \\(\\$([0-9.]+) in chips\\)")
(define dealt-rx #px"^Dealt to (.*?) \\[(.. ..)\\]")
(define action-rx #px"^(.*?): (folds|checks|calls|bets|raises|posts small blind|posts big blind)(?: \\$([0-9.]+))?(?: to \\$([0-9.]+))?")
(define board-rx #px"^\\*\\*\\* (FLOP|TURN|RIVER) \\*\\*\\* \\[(.*?)\\](?: \\[(.*?)\\])?")
(define summary-rx #px"^\\*\\*\\* SUMMARY \\*\\*\\*")
(define summary-seat-rx #px"^Seat (\\d+): (.*?) (folded|showed|collected|won)")

(define (parse-pokerstars-hand text)
  (define lines (map string-trim (string-split text "\n")))
  
  (define header (for/or ([line lines]) (regexp-match header-rx line)))
  (unless header (error "Could not parse hand header"))
  
  (define min-bet (string->number (third header)))
  
  ;; Collect all player seats
  (define seat-matches (for/list ([line lines] #:when (regexp-match? seat-rx line))
                         (regexp-match seat-rx line)))
  
  (if (empty? seat-matches)
      (error "No players found in seats")
      (void))
  
  (define max-seat (apply max (map (lambda (m) (string->number (second m))) seat-matches)))
  
  ;; PHH uses a list of stacks. We'll use 0-indexed positions 0 to max-seat-1.
  ;; If a seat is empty, its starting stack is 0.
  (define starting-stacks (make-list max-seat 0.0))
  (define player-names (make-vector max-seat #f))
  
  (for ([m seat-matches])
    (let ([seat (sub1 (string->number (second m)))]
          [name (third m)]
          [stack (string->number (fourth m))])
      (set! starting-stacks (list-set starting-stacks seat stack))
      (vector-set! player-names seat name)))
  
  (define (name->index name)
    (for/or ([i (in-range max-seat)])
      (and (equal? (vector-ref player-names i) name) i)))

  (define hole-cards (make-hash))
  (define actions '())
  (define current-bets (make-vector max-seat 0.0))
  (define total-bets (make-vector max-seat 0.0))
  (define winnings (make-vector max-seat 0.0))
  
  (define (add-action! s) (set! actions (append actions (list s))))

  (define in-summary? #f)

  (for ([line lines])
    (cond
      [(regexp-match? summary-rx line) (set! in-summary? #t)]
      [in-summary?
       (define m (regexp-match summary-seat-rx line))
       (when m
         (let* ([seat (sub1 (string->number (second m)))])
           ;; Extract winnings
           (define won-match (regexp-match #px"won \\(\\$([0-9.]+)\\)" line))
           (define coll-match (regexp-match #px"collected \\$([0-9.]+)" line))
           (when won-match
             (vector-set! winnings seat (string->number (second won-match))))
           (when coll-match
             (vector-set! winnings seat (string->number (second coll-match))))
           ;; Extract hole cards if shown
           (define show-match (regexp-match #px"showed \\[(.. ..)\\]" line))
           (when show-match
             (hash-set! hole-cards seat (string-replace (second show-match) " " "")))))]
      [else
       (define m-dealt (regexp-match dealt-rx line))
       (define m-action (regexp-match action-rx line))
       (define m-board (regexp-match board-rx line))
       
       (cond
         [m-dealt
          (let* ([name (second m-dealt)]
                 [idx (name->index name)]
                 [cards (string-replace (third m-dealt) " " "")])
            (when idx
              (hash-set! hole-cards idx cards)))]
         
         [m-board
          (vector-fill! current-bets 0.0)
          (let* ([cards-str (third m-board)]
                 [new-cards-str (fourth m-board)]
                 [compact (if (and new-cards-str (not (string=? new-cards-str "")))
                              (string-replace new-cards-str " " "")
                              (string-replace cards-str " " ""))])
            (add-action! (format "d db ~a" compact)))]
            
         [m-action
          (let* ([name (second m-action)]
                 [idx (name->index name)]
                 [type (third m-action)]
                 [amt1 (and (fourth m-action) (string->number (fourth m-action)))]
                 [amt2 (and (fifth m-action) (string->number (fifth m-action)))])
            (when idx
              (cond
                [(string=? type "folds")
                 (add-action! (format "p~a f" (add1 idx)))]
                [(string=? type "checks")
                 (add-action! (format "p~a cc" (add1 idx)))]
                [(string=? type "calls")
                 (vector-set! current-bets idx (+ (vector-ref current-bets idx) amt1))
                 (vector-set! total-bets idx (+ (vector-ref total-bets idx) amt1))
                 (add-action! (format "p~a cc ~a" (add1 idx) amt1))]
                [(string=? type "bets")
                 (vector-set! current-bets idx amt1)
                 (vector-set! total-bets idx (+ (vector-ref total-bets idx) amt1))
                 (add-action! (format "p~a cbr ~a" (add1 idx) amt1))]
                [(string=? type "raises")
                 ;; PokerStars "raises $X to $Y": amt1 is X, amt2 is Y.
                 ;; The amount added to the pot is (Y - previous_bet_this_street).
                 (let ([inc (- amt2 (vector-ref current-bets idx))])
                   (vector-set! total-bets idx (+ (vector-ref total-bets idx) inc))
                   (vector-set! current-bets idx amt2)
                   (add-action! (format "p~a cbr ~a" (add1 idx) inc)))]
                [(or (string=? type "posts small blind")
                     (string=? type "posts big blind"))
                 (vector-set! current-bets idx amt1)
                 (vector-set! total-bets idx (+ (vector-ref total-bets idx) amt1))
                 (add-action! (format "p~a cbr ~a" (add1 idx) amt1))])))]))])

  (define finishing-stacks
    (for/list ([i (in-range max-seat)])
      (let ([start (list-ref starting-stacks i)]
            [bet (vector-ref total-bets i)]
            [won (vector-ref winnings i)])
        (+ (- start bet) won))))

  (parsed-hand "NT" min-bet starting-stacks finishing-stacks actions hole-cards))
