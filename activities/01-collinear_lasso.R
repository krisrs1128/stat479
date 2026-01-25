# Lasso Instability with Correlated Predictors
library(glmnet)
library(UpSetR)
set.seed(202601)

n <- 100  # samples
p <- 25   # genes

# Create data: Gene1 and Gene2 are highly correlated, both truly important
X <- matrix(rnorm(n * p), n, p, dimnames = list(NULL, paste0("Gene", 1:p)))
X[, 2] <- 0.9 * X[, 1] + 0.1 * rnorm(n)  # Make Gene2 ~90% correlated with Gene1
true_beta <- c(2, 2, 0, 0, 1.5, rep(0, p - 5))  # Gene1, Gene2, Gene5 matter
y <- X %*% true_beta + rnorm(n, sd = 2)

cat("Correlation between Gene1 and Gene2:", round(cor(X[,1], X[,2]), 2), "\n\n")

# Bootstrap experiment: fit lasso 1000 times
n_boot <- 1000
selected <- matrix(0, n_boot, p, dimnames = list(NULL, colnames(X)))

for (i in 1:n_boot) {
  boot_idx <- sample(n, replace = TRUE)
  fit <- cv.glmnet(X[boot_idx, ], y[boot_idx], alpha = 1)
  selected[i, ] <- as.numeric(coef(fit, s = "lambda.1se")[-1] != 0)
}

# Results
cat("Selection frequencies:\n")
cat(sprintf("  Gene1: %d%%\n", round(100 * mean(selected[,1]))))
cat(sprintf("  Gene2: %d%%\n", round(100 * mean(selected[,2]))))
cat(sprintf("  Gene5: %d%%\n\n", round(100 * mean(selected[,5]))))

upset(as.data.frame(selected),
      nintersects = 10,
      order.by = "freq",
      mainbar.y.label = "Combination Frequency",
      sets.x.label = "Selection Count")