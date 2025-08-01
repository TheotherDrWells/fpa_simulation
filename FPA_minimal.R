fpa_code <- function(X, Y, reverse_fn = function(x, L = 5) L + 1 - x) {
  # X: matrix of item responses (rows = people, cols = items)
  # Y: outcome vector (length = nrow(X))
  # reverse_fn: function to reverse code items (default assumes Likert 1 to 5)
  
  if (!is.matrix(X)) X <- as.matrix(X)
  if (length(Y) != nrow(X)) stop("Y must match number of rows in X")
  
  n <- nrow(X)
  p <- ncol(X)
  
  # Compute item-level correlations with Y (Spearman by default)
  cor_vec <- apply(X, 2, function(col) suppressWarnings(cor(col, Y, method = "spearman")))
  
  # Determine flip direction: flip if correlation with Y is negative
  global_pi <- ifelse(cor_vec >= 0, 1, 0)
  
  # Apply π to each person
  X_aligned <- X
  for (j in 1:p) {
    if (global_pi[j] == 0) {
      X_aligned[, j] <- reverse_fn(X[, j])
    }
  }
  
  # Compute aligned score (mean of aligned items)
  aligned_score <- rowMeans(X_aligned)
  
  # Return π, aligned data, and aligned score
  return(list(
    pi = global_pi,
    X_aligned = X_aligned,
    aligned_score = aligned_score,
    utility = cor(aligned_score, Y, method = "spearman")
  ))
}


#######################
# Example Usage

# Simulate data
set.seed(1)
X <- matrix(sample(1:5, 100 * 10, replace = TRUE), nrow = 100, ncol = 10)
Y <- rowSums(X) + rnorm(100)  # Outcome correlated with raw scores

# Run FPA
result <- fpa_code(X, Y)

# View outputs
result$pi             # The polarity vector (1 = keep, 0 = flip)
result$X_aligned[1:5, ]  # First few aligned rows
result$aligned_score[1:5] # First few aligned scores
result$utility        # Spearman correlation between aligned scores and Y
