# Memory-safe High-Value Heterogeneity Analysis
# Event fixed effects using within-event demeaning

library(data.table)
data_dir <- path.expand("~/Desktop/H&M")

analysis_main <- fread(
  file.path(data_dir, "final_customer_event_analysis_dataset_all_valid_events.csv"),
  select = c(
    "event_id",
    "exposed",
    "high_value",
    "inactive_4w",
    "inactive_12w",
    "same_style_repurchase",
    "close_substitution",
    "short_term_revenue",
    "long_term_revenue",
    "pre_event_spending",
    "pre_event_purchase_frequency",
    "pre_event_active_weeks",
    "pre_event_unique_styles"
  )
)

analysis_main[, event_id := as.integer(event_id)]
analysis_main[, exposed := as.numeric(exposed)]
analysis_main[, high_value := as.numeric(high_value)]

# 2. Construct clean heterogeneity sample
# Keep all exposed observations.
# Randomly sample non-exposed observations within each event.
# This keeps the within-event comparison but reduces memory burden.

setkey(analysis_main, NULL)
analysis_main[, row_id := .I]

set.seed(814)

NONEXPOSED_RATIO <- 10L
MIN_NONEXPOSED_PER_EVENT <- 200L

keep_ids <- analysis_main[, {
  
  exposed_ids <- row_id[exposed == 1]
  nonexposed_ids <- row_id[exposed == 0]
  
  n_exp <- length(exposed_ids)
  n_non <- length(nonexposed_ids)
  
  n_non_keep <- min(
    n_non,
    max(MIN_NONEXPOSED_PER_EVENT, NONEXPOSED_RATIO * n_exp)
  )
  
  sampled_nonexposed_ids <- if (n_non_keep > 0) {
    sample(nonexposed_ids, size = n_non_keep, replace = FALSE)
  } else {
    integer(0)
  }
  
  .(row_id = c(exposed_ids, sampled_nonexposed_ids))
  
}, by = event_id]

keep_ids <- unique(keep_ids$row_id)

analysis_sample <- analysis_main[row_id %in% keep_ids]

analysis_sample[, row_id := NULL]

cat("Heterogeneity sample observations:", nrow(analysis_sample), "\n")
cat("Heterogeneity sample events:", uniqueN(analysis_sample$event_id), "\n")
cat("Heterogeneity sample exposed share:", mean(analysis_sample$exposed), "\n")

stopifnot(nrow(analysis_sample) <= nrow(analysis_main))
stopifnot(uniqueN(analysis_sample$event_id) == uniqueN(analysis_main$event_id))

rm(analysis_main, keep_ids)
gc()

# Interaction term
analysis_sample[, exposed_high_value := exposed * high_value]

# 3. Memory-safe event fixed effects regression function

run_within_fe <- function(dt, y, xvars, fe = "event_id", cluster = "event_id") {
  
  vars_needed <- c(y, xvars, fe)
  d <- dt[, ..vars_needed]
  d <- d[complete.cases(d)]
  
  reg_vars <- c(y, xvars)
  dm_vars <- paste0(reg_vars, "_dm")
  
  # Demean each variable within event
  d[, (dm_vars) := lapply(.SD, function(z) z - mean(z, na.rm = TRUE)),
    by = fe,
    .SDcols = reg_vars]
  
  y_dm <- d[[paste0(y, "_dm")]]
  X_dm <- as.matrix(d[, paste0(xvars, "_dm"), with = FALSE])
  
  fit <- lm.fit(x = X_dm, y = y_dm)
  
  beta <- fit$coefficients
  u <- fit$residuals
  
  # Cluster-robust SE by event
  XtX <- crossprod(X_dm)
  XtX_inv <- tryCatch(
    chol2inv(chol(XtX)),
    error = function(e) qr.solve(XtX)
  )
  
  Xu <- X_dm * as.numeric(u)
  S <- rowsum(Xu, group = d[[cluster]], reorder = FALSE)
  meat <- crossprod(S)
  
  G <- nrow(S)
  N <- length(y_dm)
  K <- ncol(X_dm)
  
  vcov_cluster <- (G / (G - 1)) * ((N - 1) / (N - K)) *
    XtX_inv %*% meat %*% XtX_inv
  
  se <- sqrt(diag(vcov_cluster))
  tval <- beta / se
  pval <- 2 * pt(abs(tval), df = G - 1, lower.tail = FALSE)
  
  result <- data.table(
    outcome = y,
    term = xvars,
    estimate = as.numeric(beta),
    se = as.numeric(se),
    t = as.numeric(tval),
    p = as.numeric(pval),
    N = N,
    events = G
  )
  
  rm(d, X_dm, y_dm, fit, beta, u, Xu, S, meat)
  gc()
  
  return(result)
}


# 4. Run high-value heterogeneity regressions

outcomes <- c(
  "inactive_4w",
  "inactive_12w",
  "same_style_repurchase",
  "close_substitution",
  "short_term_revenue",
  "long_term_revenue"
)

xvars <- c(
  "exposed",
  "high_value",
  "exposed_high_value",
  "pre_event_spending",
  "pre_event_purchase_frequency",
  "pre_event_active_weeks",
  "pre_event_unique_styles"
)

heterogeneity_results <- rbindlist(
  lapply(outcomes, function(y) {
    cat("Running heterogeneity regression for:", y, "\n")
    run_within_fe(analysis_sample, y, xvars)
  }),
  fill = TRUE
)

fwrite(
  heterogeneity_results,
  file.path(data_dir, "high_value_heterogeneity_results_full.csv")
)

key_terms <- c("exposed", "high_value", "exposed_high_value")

heterogeneity_key <- heterogeneity_results[term %in% key_terms]

heterogeneity_key[, stars := fifelse(
  p < 0.01, "***",
  fifelse(p < 0.05, "**",
          fifelse(p < 0.10, "*", ""))
)]

heterogeneity_key[, estimate_print := paste0(
  sprintf("%.4f", estimate), stars
)]

heterogeneity_key[, se_print := paste0(
  "(", sprintf("%.4f", se), ")"
)]

print(heterogeneity_key)

fwrite(
  heterogeneity_key,
  file.path(data_dir, "high_value_heterogeneity_key_results.csv")
)

#Create LaTeX table


outcome_order <- c(
  "inactive_4w",
  "inactive_12w",
  "same_style_repurchase",
  "close_substitution",
  "short_term_revenue",
  "long_term_revenue"
)

term_order <- c(
  "exposed",
  "high_value",
  "exposed_high_value"
)

term_labels <- c(
  exposed = "Exposed customer",
  high_value = "High-value customer",
  exposed_high_value = "Exposed $\\times$ High-value"
)

make_row <- function(term_name) {
  
  temp <- heterogeneity_key[term == term_name]
  temp <- temp[match(outcome_order, outcome)]
  
  coef_row <- temp$estimate_print
  se_row <- temp$se_print
  
  paste0(
    term_labels[[term_name]], " & ",
    paste(coef_row, collapse = " & "), " \\\\\n",
    " & ",
    paste(se_row, collapse = " & "), " \\\\\n"
  )
}

n_row <- heterogeneity_results[
  term == "exposed" & outcome %in% outcome_order
][match(outcome_order, outcome), N]

event_row <- heterogeneity_results[
  term == "exposed" & outcome %in% outcome_order
][match(outcome_order, outcome), events]

latex_heterogeneity <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{High-Value Heterogeneity Regressions}\n",
  "\\label{tab:high_value_heterogeneity}\n",
  "\\scriptsize\n",
  "\\begin{tabular}{lrrrrrr}\n",
  "\\toprule\n",
  " & Inactive 4w & Inactive 12w & Same-style & Close sub. & Short-term & Long-term \\\\\n",
  " &  &  & repurchase &  & revenue & revenue \\\\\n",
  "\\midrule\n",
  make_row("exposed"),
  make_row("high_value"),
  make_row("exposed_high_value"),
  "\\midrule\n",
  "Pre-event controls & Yes & Yes & Yes & Yes & Yes & Yes \\\\\n",
  "Event fixed effects & Yes & Yes & Yes & Yes & Yes & Yes \\\\\n",
  "Clustered SE by event & Yes & Yes & Yes & Yes & Yes & Yes \\\\\n",
  "Observations & ", paste(format(n_row, big.mark = ","), collapse = " & "), " \\\\\n",
  "Events & ", paste(event_row, collapse = " & "), " \\\\\n",
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{flushleft}\n",
  "\\footnotesize\n",
  "\\textit{Notes:} The table reports customer-event level regressions estimated on the heterogeneity comparison sample. All exposed observations are retained, while non-exposed observations are randomly sampled within each event. All specifications include event fixed effects and pre-event controls. Standard errors clustered by style decline event are reported in parentheses. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.\n",
  "\\end{flushleft}\n",
  "\\end{table}\n"
)

writeLines(
  latex_heterogeneity,
  file.path(data_dir, "high_value_heterogeneity_regression_table.tex")
)

cat("LaTeX heterogeneity table saved to:",
    file.path(data_dir, "high_value_heterogeneity_regression_table.tex"), "\n")

