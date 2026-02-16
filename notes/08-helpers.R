library(MASS)
library(patchwork)
library(rpart)
library(xgboost)
library(tidyverse)

#' Generate data and grid predictions for a diagonal classification boundary
#' approximated by a decision tree's axis-aligned splits.
diagonal_boundary_plot <- function(n = 160) {
  df <- tibble(
    x1 = runif(n), x2 = runif(n),
    class = factor(ifelse(x2 > x1 + 0.05, "A", "B"))
  )
  tree <- rpart(class ~ x1 + x2, data = df,
                control = rpart.control(cp = 0.005, minsplit = 2))
  grid <- expand.grid(x1 = seq(0, 1, length = 200),
                      x2 = seq(0, 1, length = 200))
  grid$pred <- predict(tree, grid, type = "class")
  df$leaf <- factor(tree$where)
  leaf_counts <- count(df, leaf)
  df <- left_join(df, leaf_counts, by = "leaf")

  ggplot() +
    geom_tile(data = grid, aes(x1, x2, fill = pred), alpha = 0.25) +
    geom_point(data = df, aes(x1, x2, color = class, size = 1 / n)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_manual(values = c(A = "#469da5", B = "#b0549e"), guide = "none") +
    scale_color_manual(values = c(A = "#469da5", B = "#b0549e"), guide = "none") +
    scale_size_continuous(range = c(0.5, 2.5)) +
    labs(x = expression(x[1]), y = expression(x[2]), size = "1/#{Leaf samples}") +
    coord_fixed()
}

#' Generate data and show a decision tree step-function approximation of sin(x).
#' Point size is inversely proportional to the number of samples in the leaf.
smooth_curve_plot <- function(n = 160) {
  df <- tibble(x = seq(0, 2 * pi, length = n),
               y = sin(x) + rnorm(n, sd = 0.2))
  tree <- rpart(y ~ x, data = df,
                control = rpart.control(cp = 0.005, minsplit = 2))
  df$leaf <- factor(tree$where)
  leaf_counts <- count(df, leaf)
  df <- left_join(df, leaf_counts, by = "leaf")

  grid <- tibble(x = seq(0, 2 * pi, length = 500))
  grid$yhat <- predict(tree, grid)

  ggplot() +
    geom_line(data = grid, aes(x, yhat), color = "#797777", linewidth = 0.8) +
    stat_function(fun = sin, linetype = "dashed", linewidth = 0.5) +
    geom_point(data = df, aes(x, y, size = 1 / n), color = "#000000") +
    scale_size_continuous(range = c(0.5, 2.5)) +
    labs(x = expression(x), y = expression(y), size = "1/#{Leaf samples}")
}

#' Simulate correlated predictors and fit CART + Boosting on a shared grid.
#' Returns a data.frame with columns: x1, x2, pred, method, replicate.
fit_replicates <- function(B = 20, n = 500, rho = 0.8,
                           grid_range = c(-3, 3), grid_n = 80) {
    Sigma <- matrix(c(1, rho, rho, 1), 2)
    grid <- expand.grid(
        x1 = seq(grid_range[1], grid_range[2], length.out = grid_n),
        x2 = seq(grid_range[1], grid_range[2], length.out = grid_n)
    )
    X_grid <- as.matrix(grid)
    f_true <- \(x1, x2) sin(x1) + x2^2 / 4

    map_dfr(seq_len(B), \(b) {
        X <- mvrnorm(n, mu = c(0, 0), Sigma = Sigma)
        df <- tibble(
            x1 = X[, 1], x2 = X[, 2],
            y = f_true(x1, x2) + rnorm(n)
        )

        cart_pred <- predict(rpart(y ~ x1 + x2, data = df), newdata = grid)

        dtrain <- xgb.DMatrix(X, label = df$y)
        boost_fit <- xgb.train(
            list(max_depth = 2, eta = 0.01, objective = "reg:squarederror"),
            dtrain,
            nrounds = 200, verbose = 0
        )
        boost_pred <- predict(boost_fit, newdata = X_grid)
        bind_rows(
            grid |> mutate(pred = cart_pred, method = "CART", replicate = b),
            grid |> mutate(pred = boost_pred, method = "Boosting", replicate = b)
        )
    })
}
