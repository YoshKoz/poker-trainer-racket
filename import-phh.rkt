#lang racket

;; -----------------------------------------------------------------------------
;; PHH import command
;; -----------------------------------------------------------------------------
;; Usage:
;;   env TMPDIR=/tmp racket import-phh.rkt data/phh data/generated/phh-action-outcomes.csv
;;
;; Reads existing `.phh` hand histories from a directory and writes aggregate
;; fold/call/bet outcome data in the CSV format consumed by stats.rkt.
(require "poker_trainer/phh_import.rkt")

(define args (current-command-line-arguments))

(unless (= (vector-length args) 2)
  (error 'import-phh "expected input directory and output CSV path"))

(define input-directory (vector-ref args 0))
(define output-path (vector-ref args 1))

(define rows (phh-directory->outcomes input-directory))
(define aggregated (aggregate-history-outcomes rows))

(write-history-outcomes-csv aggregated output-path)

(printf "Imported ~a action samples into ~a aggregate rows: ~a\n"
        (length rows)
        (length aggregated)
        output-path)
