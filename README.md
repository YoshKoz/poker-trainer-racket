# Racket Poker Trainer

A GUI poker study app, written in Racket — deliberately, not by default. The
`racket/gui` + `racket/class` combo gives a real desktop app (drills, an
equity meter, click-to-answer tiles) without the ceremony of an Electron or
web stack for something that only needs to run on one machine.

This is a training and saved-hand analysis tool. It does not read live poker
tables, overlay PokerStars, or automate play.

## Tech stack

- Racket (`racket/gui`, `racket/class`) for the GUI and drill engine
- `rackunit` for tests
- Plain CSV for aggregate outcome data — no database

## Run

```bash
racket main.rkt
```

## Test

```bash
raco test tests
```

Ten test modules under `tests/`, one per core unit (cards, evaluator, equity,
advice, drills, stats, visuals, and the PHH/PokerStars importers).

## What It Trains

- preflop discipline
- value betting against weak callers
- folding dominated hands
- pot odds
- equity estimates for saved or manually entered spots
- data-backed review of fold/call/bet outcomes when aggregate hand-history data exists

## Data-Backed Answers

The built-in drills now consult both `data/action-outcomes.csv` and generated
imports like `data/generated/phh-action-outcomes.csv` before falling back to the
hand-written training rule.

CSV format:

```text
spot-key,action,trials,wins,losses,ev-bb
top-pair-top-kicker-against-a-calling-station,raise,1800,1161,639,124.5
```

The trainer chooses the action with the strongest confidence-adjusted result,
using EV as a small tie-breaker. The seed CSV is intentionally small and exists
to prove the workflow; it can be replaced with aggregates parsed from real hand
histories such as public PHH files, Kaggle hand histories, or your own saved
session exports.

## Import Existing PHH Hand Histories

Public PHH files can be placed in `data/phh/`, then aggregated with:

```bash
racket import-phh.rkt data/phh data/generated/phh-action-outcomes.csv
```

The importer supports no-limit Texas Hold'em PHH files with `finishing_stacks`,
so outcomes come from the actual final stack deltas. Preflop actions are grouped
into buckets such as premium pairs, offsuit broadway, suited connectors, ace-x,
and other hands. A separate parser (`poker_trainer/pokerstars_import.rkt`)
handles PokerStars-format hand histories the same way.

Postflop actions get richer keys:

```text
street-flop-texture-dry-hand-one-pair-position-early-pressure-large
```

Those keys include:

- street: flop, turn, river
- board texture: dry, wet, paired
- made hand: high card, one pair, two pair, straight, etc.
- position bucket: early, middle, late
- pressure: fold, none, small, medium, large

## Visual Cues

- Drill actions are clickable color tiles.
- The aggressive action is labeled `BET/RAISE` so bet-first spots are clearer.
- After an answer, the best action turns green.
- A wrong clicked action turns red.
- Explanations appear in a colored best-answer panel instead of plain text.
- Analyzer equity is shown as a red/yellow/green bar against the required pot odds.
- The window uses a distinct green-felt trainer style with gold accents.
- Cards are drawn as custom mini playing cards with red/black suits. No PokerStars
  assets or lookalike brand art are included.

## Project Layout

```text
main.rkt                            GUI entrypoint
poker_trainer/cards.rkt             card model, parser, deck helpers
poker_trainer/evaluator.rkt         hand evaluation
poker_trainer/equity.rkt            equity estimates
poker_trainer/advice.rkt            hand-written training rules (fallback)
poker_trainer/drills.rkt            drill selection and flow
poker_trainer/ranges.rkt            starting-hand range buckets
poker_trainer/visuals.rkt           color and display-state helpers
poker_trainer/stats.rkt             aggregate action-outcome decisions
poker_trainer/phh_import.rkt        PHH hand-history importer
poker_trainer/pokerstars_import.rkt PokerStars hand-history importer
data/action-outcomes.csv            seed aggregate action data
data/phh/                           sample public PHH hand histories
data/generated/                     generated aggregate imports
tests/                              rackunit tests, one module per unit above
```
