# order SHAP attributions using a hierarchical clustering
shap_cluster_order <- function(explainer, important_vars) {
    hclust(dist(explainer[, important_vars]))$order
}

# reshape the SHAP explanatiosn from wide to long format
# also sets NA values for non-important_vars, which helps plotting
shap_to_long <- function(explainer, y_hat, baseline, important_vars) {
    cluster_order <- shap_cluster_order(explainer, important_vars)
    explainer |>
        data.frame() |>
        mutate(
            id = factor(row_number(), levels = cluster_order),
            pred = y_hat - baseline
        ) |>
        pivot_longer(cols = -c(id, pred)) |>
        mutate(name = ifelse(name %in% important_vars, name, NA))
}

# offsets the attributions according to the prediction
# i.e., makes sure the attributions above pred are the positive attributions,
# and those below pred are the negative attributions
add_waterfall_offsets <- function(shap_long) {
    shap_long |>
        mutate(pos = value >= 0) |>
        group_by(id, pos) |>
        mutate(
            cumsum_before = lag(cumsum(value), default = 0),
            ymin = pred + cumsum_before + pmin(value, 0),
            ymax = pred + cumsum_before + pmax(value, 0),
            xpos = as.integer(id)
        ) |>
        ungroup()
}
