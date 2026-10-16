# Halatro — A Poker-Scoring & Prediction Engine in Haskell

> *What's the best possible hand hiding inside a pile of cards — and what's the smartest way to play it?*

Halatro is a pure-functional implementation of the core scoring ruleset from **Balatro**, built from first principles in Haskell. It is simultaneously three things:

1. A **combinatorial poker-hand engine** that classifies, scores, and optimally selects hands from an arbitrary pool of cards.
2. A **decision-making AI** that plays an entire round — choosing when to play, when to discard, and what to hold — in pursuit of the highest possible score.
3. A **terminal UI** (built on `brick`/`vty`) that renders the engine live, with real-time score previews as you select cards.

The project is deliberately split into a pure, side-effect-free core and a thin imperative shell — a textbook demonstration of functional architecture applied to a genuinely nontrivial combinatorial problem.

---

## Why this is interesting

Most "poker hand evaluator" toy projects stop at *classification* — is this a flush? a straight? Halatro goes further and solves three progressively harder problems entirely with immutable data and recursive, declarative algorithms:

| Problem | What it actually requires |
|---|---|
| **Classify** a hand of 5+ cards | Pattern-matching across 10 hand types, several of which (straight, straight flush, royal flush) require reasoning about wraparound-free rank sequences |
| **Extract** the exact scoring subset of cards from a larger hand | Not just "is there a flush" but "*which 5 cards* constitute it," since Balatro scores only the cards that participate in the best hand type |
| **Search** for the highest-scoring 5-card hand out of a larger pool | A genuine combinatorial optimization — every 5-card subsequence of the hand is scored, and the maximum is selected |
| **Strategize** across a full round of 3 plays and 3 discards | A multi-step decision policy that reasons about near-misses ("I'm 2 away from a flush, is it worth a discard?") rather than just playing greedily |

---

## Key Features

- **Full Balatro-style hand hierarchy** — High Card through Royal Flush, each mapped to its own `(Chips, Mult)` scoring tuple, matching the real game's progression curve (`(5,1)` for High Card up to `(100,8)` for Straight/Royal Flush).
- **Exact scoring-subset extraction** — rather than scoring an entire 5-card hand indiscriminately, the engine identifies precisely which cards contributed to the detected hand type (e.g. only the paired cards in a Two Pair, not every card dealt).
- **Brute-force-optimal hand search** — `highestScoringHand` evaluates every valid 5-card combination of a larger hand via `subsequences` and selects the true maximum, so the "best hand" is provably best, not heuristically guessed.
- **A layered autonomous AI (`myAI`)** that doesn't just play the best hand it's holding — it looks ahead:
  - Locks in a play once a hand clears a score threshold judged "good enough."
  - Otherwise probes for live draws: an almost-flush (3+ of one suit) or an almost-straight (4 consecutive ranks already in hand) triggers a targeted discard of the *other* cards, preserving the draw.
  - Falls back to discarding singleton (non-paired) cards when sitting on Two Pair / Three of a Kind, angling for a Full House.
  - Respects the game's hard constraints — forced to play once discards are exhausted, forced to commit on the final hand of the round.
- **Two additional reference AIs** (`simpleAI`, `sensibleAI`) that let you benchmark naive strategies ("just play your 5 highest cards") against the optimal-search and lookahead strategies.
- **A live terminal UI** — cards render as bordered unicode playing cards, selection highlights the cards you've picked, and the interface previews in real time which of those cards will actually score and for how much, before you commit to playing them.
- **Property-based + unit test suite** (`tasty`, `tasty-quickcheck`, `tasty-hunit`) covering hand classification, card extraction, scoring, and optimal-hand search independently of each other.

---

## Design Decisions

**1. A hard boundary between pure logic and effectful shell.**
All game *rules* — hand classification, scoring, card selection, AI strategy — live in `src/CourseworkOne.hs` and `src/Halatro/*` as pure functions with no `IO`. Randomness (shuffling), terminal rendering, and the game loop live only in `app/`. This means the entire poker engine can be fuzzed, unit-tested, and reasoned about with equational logic, with no mocking of IO ever required.

**2. Types that make illegal states hard to represent.**
`Rank` and `Suit` are closed `Enum`/`Bounded` ADTs rather than raw integers or strings, so a "card" can never have an invalid rank and the full deck can be generated mechanically (`Card <$> [minBound..maxBound] <*> [minBound..maxBound]`). `HandType` is an `Ord`-ranked enum, so "is X a better hand than Y" is literally `x > y` — the ordering *is* the game rule, not a side lookup table. `Move` is modeled as `Move PlayOrDiscard [Card]`, making every AI decision a single, inspectable, equality-comparable value.

**3. Declarative, comprehension-driven algorithms over imperative loops.**
Nearly every predicate in the engine (`findFlush`, `findStraight`, `findFullHouse`, `findNCards`, …) is expressed as a list comprehension or filter over the hand, rather than manual index-chasing. This keeps each hand-type check close to its mathematical definition (e.g. a flush is "5+ cards sharing a suit," expressed directly as a `sum`/`filter` over each suit) and makes the logic easy to verify against the rules it's modeling.

**4. Favor exact search over heuristics where it's actually tractable.**
A hand of 8 cards has only `C(8,5) = 56` five-card subsequences — small enough that computing the true best-scoring hand by brute-force enumeration (`findNCombinations` + `scoreHand` + `maximum`) is both simpler *and* more trustworthy than a greedy heuristic. The AI reserves heuristics for the genuinely unbounded problem — multi-turn strategy across discards — where exhaustive search isn't the right tool.

**5. The AI as a pure, replayable function of history.**
`type AI = [Move] -> [Card] -> Move` — an AI strategy has no hidden state. It is handed its own move history and current hand and returns a decision. This makes any AI trivially testable in isolation, swappable at the call site (`runSomeAI simpleAI` vs `runSomeAI myAI`), and guarantees that two runs with identical history produce identical decisions — determinism without needing a seeded `IO` action anywhere in the strategy layer.

**6. Strict compiler hygiene.**
The project builds under `GHC2021` with `-Wall -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wmissing-home-modules -Wpartial-fields -Wredundant-constraints` — a warning set specifically chosen to catch non-exhaustive pattern matches and accidental partiality, which matters enormously in a project built almost entirely out of pattern-matched ADTs.

---

## Architecture

```
Poker-Prediction-Engine/
├── src/                          -- pure library (no IO)
│   ├── Halatro/
│   │   ├── Types.hs              -- Card, Rank, Suit, HandType, Move, Hand
│   │   ├── Constants.hs          -- scoring tables, hand size
│   │   └── BasicGame.hs          -- round/game-state machinery, AI harness
│   └── CourseworkOne.hs          -- ★ the engine: classification, scoring,
│                                     optimal search, and the AI strategies
├── app/                          -- effectful shell
│   ├── Main.hs
│   └── Halatro/Frontend.hs,      -- the `brick`/`vty` terminal UI
│       Frontend/{Types,Utils,Widgets}.hs
└── test/
    └── Spec.hs                   -- tasty unit + QuickCheck property tests
```

The dependency arrow only ever points one way: `app` depends on `src`, never the reverse. The TUI is a *consumer* of the engine, not a participant in its logic.

---

## How It Works, In Brief

A round deals an 8-card hand from a shuffled 52-card deck. Across the round you get **3 plays** and **3 discards**. Each play scores using the classic Balatro formula:

```
score = (Σ rank-values of the scoring cards + base chips for the hand type) × multiplier for the hand type
```

e.g. a pair of Aces scores `(11 + 11 + 10) × 2 = 64`, where `10` is Pair's base chip value and `2` is its multiplier.

`myAI` plays the full round autonomously: it evaluates the best hand currently sitting in your cards, decides whether that score already clears its internal bar, and — if not — looks for a live draw to protect before discarding everything that isn't part of it.

---

## Getting Started

### Prerequisites

- [GHC](https://www.haskell.org/ghc/) (via [GHCup](https://www.haskell.org/ghcup/) is easiest)
- [Stack](https://docs.haskellstack.org/) — the project is pinned via `stack.yaml` / `stack.yaml.lock`

### Build

```bash
stack build
```

### Play it yourself (terminal UI)

```bash
stack run
```

Arrow keys / `j`/`k` to move, space/enter to select cards, and dedicated keys to play or discard your selection — the panel on the right shows your live score preview as you select cards, before you commit.

### Run the AI headlessly and see what score it gets

From a REPL:

```bash
stack ghci
ghci> runAIDirectly          -- runs myAI over one shuffled round, returns its final score
ghci> runSomeAI simpleAI     -- compare against the naive "play your 5 highest cards" baseline
ghci> runSomeAI sensibleAI   -- compare against the "always play the optimal hand you're holding" baseline
```

Because every AI is a pure function of `([Move], [Card])`, you can run thousands of simulated rounds to get a distribution of scores for any strategy:

```haskell
ghci> scores <- replicateM 1000 (runSomeAI myAI)
ghci> fromIntegral (sum scores) / 1000  -- average score across 1000 simulated rounds
```

### Run the test suite

```bash
stack test
```

The suite is organized into five parts mirroring the engine's layers: hand-type detection, best-hand-type identification, scoring, optimal-hand search, and AI behavior — combining targeted unit cases with QuickCheck properties for the combinatorial pieces.

---

## Tech Stack

| Layer | Tooling |
|---|---|
| Language | Haskell, `GHC2021` |
| Build | Stack / Cabal (via `hpack`) |
| Terminal UI | [`brick`](https://hackage.haskell.org/package/brick) + [`vty`](https://hackage.haskell.org/package/vty) |
| Testing | [`tasty`](https://hackage.haskell.org/package/tasty), `tasty-hunit`, `tasty-quickcheck` |
| Core deps | `containers`, `text`, `time`, `random` |

---

## License

BSD-3-Clause
