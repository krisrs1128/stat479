# Visualizing Regularization Geometry
library(ggplot2)
library(patchwork)

# Function to visualize regularization
plot_regularization <- function(lambda = 5, penalty_type = "l2", save_plot = FALSE, filename = NULL) {

  # Create a grid of parameter values
  beta1 <- seq(-3, 3, length.out = 100)
  beta2 <- seq(-3, 3, length.out = 100)
  grid <- expand.grid(beta1 = beta1, beta2 = beta2)

  # Define a quadratic loss function with elliptical contours
  # L(beta) = 5*(beta1 - 1)^2 + 0.5*(beta2 - 1.5)^2
  grid$loss <- 5*(grid$beta1 - 1.5)^2 + 0.5*(grid$beta2 - 1)^2

  # Define penalty based on type
  if (penalty_type == "l2") {
    grid$penalty <- lambda * (grid$beta1^2 + grid$beta2^2)
    penalty_title <- "L2 Penalty"
  } else if (penalty_type == "l1") {
    grid$penalty <- lambda * (abs(grid$beta1) + abs(grid$beta2))
    penalty_title <- "L1 Penalty"
  } else {
    stop("penalty_type must be either 'l1' or 'l2'")
  }

  # Combined objective: Loss + Penalty
  grid$combined <- grid$loss + grid$penalty

  # Plot 1: Original Loss Function
  p1 <- ggplot(grid, aes(x = beta1, y = beta2, z = loss)) +
    geom_vline(xintercept = 0, col = "#949292") +
    geom_hline(yintercept = 0, col = "#949292") +
    geom_contour(aes(color = after_stat(level)), bins = 15) +
    geom_point(x = 1.5, y = 1, size = 3) +
    scale_color_viridis_c() +
    annotate("text", x = 1.5, y = 1.4, label = "Min: (1.5, 1)") +
    labs(title = "Original Loss Function",
         x = expression(beta[1]),
         y = expression(beta[2])) +
    theme_minimal() +
    coord_fixed()

  # Plot 2: Penalty
  p2 <- ggplot(grid, aes(x = beta1, y = beta2, z = penalty)) +
    geom_vline(xintercept = 0, col = "#949292") +
    geom_hline(yintercept = 0, col = "#949292") +
    scale_color_viridis_c() +
    geom_contour(aes(color = after_stat(level)), bins = 15) +
    geom_point(x = 0, y = 0, size = 3) +
    annotate("text", x = 0, y = 0.3, label = "Min: (0, 0)") +
    labs(title = paste0(penalty_title, " (λ = ", lambda, ")"),
         x = expression(beta[1]),
         y = expression(beta[2])) +
    theme_minimal() +
    coord_fixed()

  # Plot 3: Combined Objective
  # Find minimum of combined function
  min_idx <- which.min(grid$combined)
  min_beta1 <- grid$beta1[min_idx]
  min_beta2 <- grid$beta2[min_idx]

  p3 <- ggplot(grid, aes(x = beta1, y = beta2, z = combined)) +
    geom_vline(xintercept = 0, col = "#949292") +
    geom_hline(yintercept = 0, col = "#949292") +
    scale_color_viridis_c() +
    geom_contour(aes(color = after_stat(level)), bins = 15) +
    geom_point(x = min_beta1, y = min_beta2, size = 3) +
    annotate("text", x = min_beta1, y = min_beta2 + 0.3,
             label = sprintf("Min: (%.2f, %.2f)", min_beta1, min_beta2)) +
    labs(title = paste0("Loss + ", penalty_title),
         x = expression(beta[1]),
         y = expression(beta[2])) +
    theme_minimal() +
    coord_fixed()

  # Arrange plots
  combined_plot <- p1 + p2 + p3

  # Save if requested
  if (save_plot) {
    if (is.null(filename)) {
      filename <- paste0("notes/figures/", penalty_type, "_losses_lam", lambda, ".png")
    }
    ggsave(filename, plot = combined_plot, dpi = 300, width = 12, height = 4)
    cat("Plot saved to:", filename, "\n")
  }

  return(combined_plot)
}

# Example usage:
# L2 with lambda = 5
plot_regularization(lambda = 0.5, penalty_type = "l1", save_plot = TRUE)
plot_regularization(lambda = 1, penalty_type = "l1", save_plot = TRUE)
plot_regularization(lambda = 0.5, penalty_type = "l2", save_plot = TRUE)
plot_regularization(lambda = 1, penalty_type = "l2", save_plot = TRUE)