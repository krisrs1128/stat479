tidy_xy <- function(label) {
    cur_vars <- annotation |>
        dplyr::filter(label == !!label) |>
        pull(variable)

    xy |>
        select(viability, all_of(cur_vars)) |>
        pivot_longer(-viability)
}

plot_viability <- function(label, page = 1, nrow = 6, ncol = 5, keep_names = NULL) {
    if (is.null(keep_names)) {
        keep_names <- colnames(xy)
    }

    xy_long <- tidy_xy(label) |>
        filter(name %in% keep_names)
    ggplot(xy_long) +
        geom_point(aes(value, viability)) +
        facet_wrap_paginate(~name, nrow = nrow, ncol = ncol, page = page)
}

plot_coef <- function(fit, ...) {
    beta_hat <- tibble(beta_hat = coef(fit, ...)[-1, 1]) |>
        bind_cols(annotation)

    beta_hat |>
        filter(beta_hat != 0) |>
        ggplot() +
        geom_col(aes(beta_hat, variable)) +
        facet_wrap(~label, scales = "free")
}

plot_predictions <- function(fit, X, y) {
    predictions <- tibble(
        y = y,
        y_hat = predict(fit, X)
    )

    ggplot(predictions) +
        geom_abline(slope = 1, col = "#7cbae4ff") +
        geom_point(aes(y, y_hat))
}

annotated_beta <- function(coefs, annotation, min_b = 0.001) {
    # join and filter coefficients
    beta_mat <- coef(fit$glmnet.fit)[-1, ] |>
        as.matrix()
    keep_ix <- apply(beta_mat, 1, \(b) max(b) > min_b)
    beta_hat_lambda <- beta_mat[keep_ix, ] |>
        as.data.frame() |>
        rownames_to_column("variable") |>
        pivot_longer(-variable, names_to = "lambda") |>
        left_join(annotation) |>
        mutate(lambda = factor(lambda, levels = colnames(beta_mat)))

    # sort by coefficient average value
    coef_order <- beta_hat_lambda |>
        group_by(variable) |>
        summarise(mvalue = mean(value)) |>
        arrange(-mvalue) |>
        pull(variable)

    beta_hat_lambda |>
        mutate(variable = factor(variable, levels = coef_order))
}

tile_plot <- function(beta_hat_lambda) {
    ggplot(beta_hat_lambda) +
        geom_tile(aes(lambda, variable, fill = log(value), col = log(value))) +
        scale_fill_scico(na.value = "#f7f7f7", palette = "glasgow") +
        scale_color_scico(na.value = "#f7f7f7", palette = "glasgow") +
        facet_grid(label ~ ., scales = "free_y", space = "free") +
        theme(axis.text.x = element_blank())
}
