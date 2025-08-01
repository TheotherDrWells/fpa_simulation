# Fast Projection Alignment (FPA) Benchmark

This repository contains all code and simulation results for evaluating **Fast Projection Alignment (FPA)** — a deterministic polarity optimization method for outcome-aligned scoring.

FPA is compared to traditional greedy flip search and a stacked FPA+Greedy procedure across increasing item counts and classification complexity.

---

## What Is FPA?

**FPA** (Fast Projection Alignment) assigns a polarity vector (flip or keep) to each item by projecting its correlation with an outcome variable `Y`. Unlike greedy methods, FPA requires no search or tuning — it is:

- Deterministic
- Structure-aware
- Runtime-stable (constant-time per item count)
- Scalable up to 100+ items

It is suitable for:
- Outcome-aligned scoring
- Polarity correction audits
- Classification pipelines
- Measurement diagnostics

---


---

## What This Simulation Tests

- **Classification Utility**: Does polarity alignment improve prediction of a multi-class outcome?
- **Runtime Scaling**: Can polarity methods handle high-dimensional response matrices?
- **Stability Under Noise**: How do methods perform with 15% polarity reversals?

### Methods Compared:
- `FPA`: Projection-based one-pass polarity assignment  
- `Greedy`: Item-wise flip optimization  
- `FPA+Greedy`: Hybrid of both (best performance)

---

## Key Results (1000 replications per item count)

- **FPA+Greedy** yields the highest classification accuracy across all item counts.
- **FPA alone** is fast (<1.5 ms at 100 items) and improves with item dimensionality.
- **Greedy** degrades in accuracy as item count increases and scales linearly in runtime.

See `tableFPA1000c.csv` and the included plots for full benchmarks.

---

## Citation

Wells, K. E. (2025). Fast Projection Alignment (FPA): A Deterministic Polarity Optimization Method for Outcome-Aligned Scoring. [Manuscript in preparation].


