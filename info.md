This simulation will:

* Generate response data `X`
* Generate an outcome variable `Y` based on a known scoring rule
* Randomly reverse a subset of items (simulate polarity misalignment)
* Compare:

  * Sum scores
  * FPA-aligned scores
  * (Optional) FPA+Greedy or other methods
* Evaluate alignment utility (e.g., Spearman correlation with Y)

---

##  FPA Simulation Outline

###  **Simulation Parameters**

* `n`: Number of people (e.g., 1000)
* `p`: Number of items (e.g., 20, 40, 80)
* `scale`: Likert response range (e.g., 1 to 5)
* `prop_reversed`: Proportion of items to randomly reverse (e.g., 0.15 = 15%)
* `noise_sd`: Standard deviation of noise added to outcome (e.g., 1)

---

### 🛠 **Step-by-Step Simulation Plan**

#### 1. **Generate Response Matrix X**

```r
X <- matrix(sample(1:5, n * p, replace = TRUE), nrow = n, ncol = p)
```

#### 2. **Define True Polarity (π\_true)**

```r
pi_true <- rep(1, p)  # All items start as forward
n_flip <- round(p * prop_reversed)
flip_indices <- sample(1:p, n_flip)
pi_true[flip_indices] <- 0
```

#### 3. **Generate Aligned Responses X\_aligned**

```r
reverse_fn <- function(x, L = 5) L + 1 - x
X_aligned <- X
for (j in flip_indices) {
  X_aligned[, j] <- reverse_fn(X[, j])
}
```

#### 4. **Create Outcome Y (true structure)**

```r
Y <- rowMeans(X_aligned) + rnorm(n, mean = 0, sd = noise_sd)
```

#### 5. **Apply Scoring Methods**

* **Sum Score (unaligned)**: `rowMeans(X)`
* **FPA-Aligned Score**:

```r
result_fpa <- fpa_code(X, Y)
fpa_score <- result_fpa$aligned_score
```

#### 6. **Evaluate Utility (Alignment Accuracy)**

```r
sum_score <- rowMeans(X)
data.frame(
  Method = c("Sum", "FPA"),
  Utility = c(cor(sum_score, Y, method = "spearman"),
              cor(fpa_score, Y, method = "spearman"))
)
```

---

##  Optional Extensions

| Extension                  | What It Adds                                       |
| -------------------------- | -------------------------------------------------- |
| Vary p (items)             | Show how FPA scales vs sum scores                  |
| Vary % of reversals        | Test performance under polarity noise              |
| Add Greedy or FPA+Greedy   | Benchmark utility improvements                     |
| Track runtime              | Validate scalability                               |
| Repeat across reps         | Average across simulations (e.g., 1000 reps)       |
| Track misclassification    | If you dichotomize Y (e.g., high-risk vs low-risk) |
| Plot structure restoration | Add SFI or ∆PII if simulating latent recovery      |

---


