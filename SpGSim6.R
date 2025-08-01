library(dplyr)

# --- Utility Function ---
utility_classification <- function(score, y_class) {
  pred <- cut(score, breaks = quantile(score, probs = c(0, 1/3, 2/3, 1)),
              labels = c(0, 1, 2), include.lowest = TRUE)
  mean(pred == y_class)
}

# --- FPA ---
fpa_flip <- function(X, y_class, L) {
  polarity <- sign(cor(X, as.numeric(y_class)))
  Xc <- X
  for (j in seq_along(polarity)) {
    if (polarity[j] == -1) {
      Xc[, j] <- L + 1 - Xc[, j]
    }
  }
  list(score = rowMeans(Xc), polarity = polarity)
}

# --- Greedy ---
greedy_flip <- function(X, y_class, L) {
  Xc <- matrix(as.numeric(X), nrow = nrow(X))
  flip_vec <- rep(1, ncol(Xc))
  for (j in 1:ncol(Xc)) {
    orig <- Xc[, j]
    flip <- L + 1 - orig
    score_orig <- rowMeans(Xc)
    Xc[, j] <- flip
    score_flip <- rowMeans(Xc)
    u_orig <- utility_classification(score_orig, y_class)
    u_flip <- utility_classification(score_flip, y_class)
    if (u_flip > u_orig) {
      flip_vec[j] <- -1
    } else {
      Xc[, j] <- orig
    }
  }
  list(score = rowMeans(Xc), polarity = flip_vec)
}

# --- FPA + Greedy ---
fpa_then_greedy <- function(X, y_class, L) {
  polarity <- sign(cor(X, as.numeric(y_class)))
  Xc <- X
  for (j in seq_along(polarity)) {
    if (polarity[j] == -1) {
      Xc[, j] <- L + 1 - Xc[, j]
    }
  }
  for (j in 1:ncol(Xc)) {
    orig <- Xc[, j]
    flip <- L + 1 - orig
    score_orig <- rowMeans(Xc)
    Xc[, j] <- flip
    score_flip <- rowMeans(Xc)
    u_orig <- utility_classification(score_orig, y_class)
    u_flip <- utility_classification(score_flip, y_class)
    if (u_flip <= u_orig) {
      Xc[, j] <- orig
    } else {
      polarity[j] <- -1 * polarity[j]
    }
  }
  list(score = rowMeans(Xc), polarity = polarity)
}

# --- Main Simulation Loop ---
set.seed(4444)
n <- 1000
L <- 5
reps <- 1000
p_vals <- seq(10, 100, by = 5)

results <- data.frame()

for (p in p_vals) {
  for (r in 1:reps) {
    # Simulate raw ordinal data
    X_raw <- matrix(sample(1:L, n * p, replace = TRUE), nrow = n)
    
    # Create structural Y (interaction + linear + group)
    X_true <- X_raw
    if (p >= 6) {
      num_to_flip <- max(1, round(0.15 * p))
      flip_indices <- sample(1:p, num_to_flip, replace = FALSE)
      X_true[, flip_indices] <- L + 1 - X_true[, flip_indices]
      
    }
    interaction <- if (p >= 6) X_true[, 4] * X_true[, 6] else rowSums(X_true[, 1:2])
    score_linear <- X_true %*% seq(0.5, 1.0, length.out = p)
    group_labels <- ifelse(interaction > quantile(interaction, 2/3), 2,
                           ifelse(interaction > quantile(interaction, 1/3), 1, 0))
    Y <- 1.5 * score_linear + 6.0 * interaction + 3.0 * group_labels + rnorm(n, 0, 2)
    risk_class <- cut(Y, breaks = quantile(Y, probs = c(0, 1/3, 2/3, 1)),
                      labels = c(0, 1, 2), include.lowest = TRUE)
    
    # --- FPA ---
    t1 <- system.time(fpa <- fpa_flip(X_raw, risk_class, L))[3]
    u1 <- utility_classification(fpa$score, risk_class)
    
    # --- Greedy ---
    t2 <- system.time(greedy <- greedy_flip(X_raw, risk_class, L))[3]
    u2 <- utility_classification(greedy$score, risk_class)
    
    # --- FPA + Greedy ---
    t3 <- system.time(hybrid <- fpa_then_greedy(X_raw, risk_class, L))[3]
    u3 <- utility_classification(hybrid$score, risk_class)
    
    # Store
    results <- rbind(results,
                     data.frame(Method = "FPA", Items = p, Utility = u1, Time_ms = t1 * 1000),
                     data.frame(Method = "Greedy", Items = p, Utility = u2, Time_ms = t2 * 1000),
                     data.frame(Method = "FPA+Greedy", Items = p, Utility = u3, Time_ms = t3 * 1000)
    )
  }
}

# --- Summary by item count and method ---
summary_table <- results %>%
  group_by(Method, Items) %>%
  summarise(
    Mean_Utility = mean(Utility),
    SD_Utility = sd(Utility),
    Mean_Time_ms = mean(Time_ms),
    SD_Time_ms = sd(Time_ms),
    .groups = "drop"
  )

print(as.data.frame(summary_table))

library(ggplot2)

# Plot 1: Time scaling by number of items
ggplot(summary_table, aes(x = Items, y = Mean_Time_ms, color = Method)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Mean_Time_ms - SD_Time_ms,
                    ymax = Mean_Time_ms + SD_Time_ms),
                width = 1, alpha = 0.4) +
  labs(
    title = "Runtime Scaling of Polarity Methods",
    x = "Number of Items (p)",
    y = "Mean Time (ms)"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_brewer(palette = "Set1")

# Plot 2: Utility by number of items
ggplot(summary_table, aes(x = Items, y = Mean_Utility, color = Method)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Mean_Utility - SD_Utility,
                    ymax = Mean_Utility + SD_Utility),
                width = 1, alpha = 0.4) +
  labs(
    title = "Utility (Accuracy) of Polarity Methods",
    x = "Number of Items (p)",
    y = "Mean Utility (Accuracy)"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_brewer(palette = "Set1")

library(ggplot2)

# --- Time-Only Scaling Plot ---
ggplot(summary_table, aes(x = Items, y = Mean_Time_ms, color = Method)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Runtime Scaling of Polarity Methods",
    x = "Number of Items (p)",
    y = "Mean Runtime (ms)"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_brewer(palette = "Set1")

aa<-as.data.frame(summary_table)
write.csv(aa,"E:/PAD/tableFPA1000c.csv")
aa