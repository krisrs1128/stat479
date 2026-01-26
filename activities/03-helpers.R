# Helper to calculate p-value from a contingency table
leaf_pvalue <- function(df) {
  tab <- table(df$neighbor, df$y)
  if(nrow(tab) < 2 || ncol(tab) < 2) return(NA) # Handle edge cases
  chisq.test(tab)$p.value
}

# Helper to get rounded proportions
leaf_props <- function(df) {
  table(df$neighbor, df$y) |>
    prop.table(margin = 1) |>
    round(2)
}