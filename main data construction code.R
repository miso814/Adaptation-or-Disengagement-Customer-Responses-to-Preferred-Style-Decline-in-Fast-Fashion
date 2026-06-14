
# H&M Data Construction

library(data.table)
library(knitr)
data_dir <- path.expand("~/Desktop/H&M")

# 1. Read only necessary columns

transactions <- fread(
  file.path(data_dir, "transactions_train.csv"),
  select = c("t_dat", "customer_id", "article_id", "price")
)

articles <- fread(
  file.path(data_dir, "articles.csv"),
  select = c("article_id", "garment_group_name", "colour_group_name")
)


# 2. Basic cleaning and merge

transactions[, t_dat := as.IDate(t_dat)]
transactions[, week := t_dat - (as.integer(format(t_dat, "%u")) - 1L)]

articles[, style_id := paste(garment_group_name, colour_group_name, sep = " x ")]
articles[, broad_category := garment_group_name]

article_keep <- articles[, .(
  article_id,
  style_id,
  broad_category
)]

tx <- merge(
  transactions,
  article_keep,
  by = "article_id",
  all.x = TRUE
)

# Remove objects no longer needed to save memory
rm(transactions, articles, article_keep)
gc()

# Keep tx as small as possible
tx <- tx[, .(
  customer_id,
  article_id,
  price,
  t_dat,
  week,
  style_id,
  broad_category
)]

cat("Transaction data merged successfully.\n")
cat("Rows in tx:", nrow(tx), "\n")

# 3. Raw data overview

raw_data_summary <- tx[, .(
  start_date = min(t_dat, na.rm = TRUE),
  end_date = max(t_dat, na.rm = TRUE),
  total_transactions = .N,
  unique_customers = uniqueN(customer_id),
  unique_articles = uniqueN(article_id),
  unique_styles = uniqueN(style_id),
  unique_broad_categories = uniqueN(broad_category)
)]

print(raw_data_summary)

# 4. Construct active article presence

style_week_counts <- tx[, .(
  active_articles = uniqueN(article_id)
), by = .(style_id, broad_category, week)]

style_map <- unique(tx[, .(style_id, broad_category)])

all_weeks <- seq(
  from = min(tx$week, na.rm = TRUE),
  to = max(tx$week, na.rm = TRUE),
  by = "week"
)

style_week <- CJ(
  style_id = unique(style_map$style_id),
  week = all_weeks
)

style_week <- merge(
  style_week,
  style_map,
  by = "style_id",
  all.x = TRUE
)

style_week <- merge(
  style_week,
  style_week_counts,
  by = c("style_id", "broad_category", "week"),
  all.x = TRUE
)

style_week[is.na(active_articles), active_articles := 0L]

rm(style_week_counts, style_map)


# 5. Identify style decline events

PRE_WEEKS <- 12
POST_SHORT_WEEKS <- 4
POST_LONG_WEEKS <- 12

DECLINE_THRESHOLD <- 0.50
MIN_PRE_ACTIVE_ARTICLES <- 3
COOLDOWN_WEEKS <- 8

setorder(style_week, style_id, week)

style_week[, pre_active_mean := frollmean(
  shift(active_articles, 1L),
  n = PRE_WEEKS,
  align = "right",
  fill = NA
), by = style_id]

style_week[, decline_ratio := fifelse(
  pre_active_mean > 0,
  (pre_active_mean - active_articles) / pre_active_mean,
  NA_real_
)]

style_week[, event_candidate := (
  pre_active_mean >= MIN_PRE_ACTIVE_ARTICLES &
    decline_ratio >= DECLINE_THRESHOLD
)]

events0 <- style_week[event_candidate == TRUE]

setorder(events0, style_id, week)

events0[, weeks_since_previous_candidate :=
          as.numeric(week - shift(week)) / 7,
        by = style_id]

events <- events0[
  is.na(weeks_since_previous_candidate) |
    weeks_since_previous_candidate > COOLDOWN_WEEKS
]

min_week <- min(tx$week, na.rm = TRUE)
max_week <- max(tx$week, na.rm = TRUE)

events <- events[
  week - PRE_WEEKS * 7 >= min_week &
    week + POST_LONG_WEEKS * 7 <= max_week
]

events[, event_id := .I]

events <- events[, .(
  event_id,
  event_week = week,
  declining_style_id = style_id,
  event_category = broad_category,
  pre_active_mean,
  active_articles,
  decline_ratio
)]

events[, `:=`(
  pre_start = event_week - PRE_WEEKS * 7,
  pre_end = event_week - 7,
  post_start = event_week + 7,
  post_short_end = event_week + POST_SHORT_WEEKS * 7,
  post_long_end = event_week + POST_LONG_WEEKS * 7
)]

cat("Number of style decline events identified:", nrow(events), "\n")
print(head(events, 10))

rm(events0)
gc()


# 6. Select valid events from all identified style decline events

# Add indices to speed filtering
setindex(tx, broad_category, week)
setindex(tx, customer_id, week)

# This function counts exposed customers for one event.
# This is not a separate robustness check; it is part of sample construction.
count_exposed_one_event <- function(e) {
  
  event_id_e <- e$event_id
  declining_style_e <- e$declining_style_id
  event_category_e <- e$event_category
  pre_start_e <- e$pre_start
  pre_end_e <- e$pre_end
  
  pre_tx_e <- tx[
    broad_category == event_category_e &
      week >= pre_start_e &
      week <= pre_end_e
  ]
  
  if (nrow(pre_tx_e) == 0) {
    return(data.table(
      event_id = event_id_e,
      n_customers = 0,
      n_exposed = 0,
      exposed_share = NA_real_
    ))
  }
  
  pre_style_e <- pre_tx_e[, .(
    pre_style_purchases = .N,
    pre_style_spending = sum(price, na.rm = TRUE)
  ), by = .(customer_id, style_id)]
  
  setorder(
    pre_style_e,
    customer_id,
    -pre_style_purchases,
    -pre_style_spending,
    style_id
  )
  
  preferred_e <- pre_style_e[, .SD[1], by = customer_id]
  preferred_e[, exposed := as.integer(style_id == declining_style_e)]
  
  data.table(
    event_id = event_id_e,
    n_customers = nrow(preferred_e),
    n_exposed = sum(preferred_e$exposed, na.rm = TRUE),
    exposed_share = mean(preferred_e$exposed, na.rm = TRUE)
  )
}

# If exposure diagnostic already exists, load it.
# Otherwise, compute it from all identified events.
exposure_file <- file.path(data_dir, "exposure_by_event_all_events.csv")

if (file.exists(exposure_file)) {
  
  exposure_by_event_all <- fread(exposure_file)
  cat("Loaded existing exposure diagnostic file.\n")
  
} else {
  
  exposure_list_all <- vector("list", nrow(events))
  
  start_time_exposure <- Sys.time()
  
  for (j in seq_len(nrow(events))) {
    
    exposure_list_all[[j]] <- count_exposed_one_event(events[j])
    
    if (j %% 50 == 0) {
      cat(
        "Checked event", j, "out of", nrow(events),
        "| Time used:",
        round(as.numeric(Sys.time() - start_time_exposure, units = "mins"), 2),
        "minutes\n"
      )
    }
    
    gc()
  }
  
  exposure_by_event_all <- rbindlist(exposure_list_all, fill = TRUE)
  
  exposure_by_event_all <- merge(
    exposure_by_event_all,
    events[, .(
      event_id,
      event_week,
      declining_style_id,
      event_category,
      decline_ratio
    )],
    by = "event_id",
    all.x = TRUE
  )
  
  fwrite(exposure_by_event_all, exposure_file)
  
  rm(exposure_list_all)
  gc()
}

# Main event selection rule:
# Keep events with at least 50 exposed customers.
MIN_EXPOSED_CUSTOMERS <- 50

valid_event_ids_all <- exposure_by_event_all[
  n_exposed >= MIN_EXPOSED_CUSTOMERS,
  event_id
]

events_final <- events[event_id %in% valid_event_ids_all]

cat("Total identified style decline events:", nrow(events), "\n")
cat("Events used for final analysis:", nrow(events_final), "\n")
cat("Minimum exposed customers per retained event:", MIN_EXPOSED_CUSTOMERS, "\n")

# Save event selection table
fwrite(
  events_final,
  file.path(data_dir, "final_valid_style_decline_events.csv")
)

# 7. Define function to build one customer-event dataset

build_one_event <- function(e) {
  
  event_id_e <- e$event_id
  event_week_e <- e$event_week
  declining_style_e <- e$declining_style_id
  event_category_e <- e$event_category
  
  pre_start_e <- e$pre_start
  pre_end_e <- e$pre_end
  post_start_e <- e$post_start
  post_short_end_e <- e$post_short_end
  post_long_end_e <- e$post_long_end
  
# Pre-event purchases
  
  pre_tx_e <- tx[
    broad_category == event_category_e &
      week >= pre_start_e &
      week <= pre_end_e
  ]
  
  if (nrow(pre_tx_e) == 0) {
    return(NULL)
  }
  
  # Customer-style purchase intensity before the event
  pre_style_e <- pre_tx_e[, .(
    pre_style_purchases = .N,
    pre_style_spending = sum(price, na.rm = TRUE)
  ), by = .(customer_id, style_id)]
  
  # Preferred style = most frequently purchased style before event
  # Tie-breaker = higher pre-style spending
  setorder(
    pre_style_e,
    customer_id,
    -pre_style_purchases,
    -pre_style_spending,
    style_id
  )
  
  preferred_e <- pre_style_e[, .SD[1], by = customer_id]
  setnames(preferred_e, "style_id", "preferred_style_id")
  
  # Pre-event customer controls
  pre_controls_e <- pre_tx_e[, .(
    pre_event_spending = sum(price, na.rm = TRUE),
    pre_event_purchase_frequency = .N,
    pre_event_active_weeks = uniqueN(week),
    pre_event_unique_styles = uniqueN(style_id)
  ), by = customer_id]
  
  analysis_e <- merge(
    preferred_e,
    pre_controls_e,
    by = "customer_id",
    all.x = TRUE
  )
  
  # Add event information
  analysis_e[, `:=`(
    event_id = event_id_e,
    event_week = event_week_e,
    declining_style_id = declining_style_e,
    event_category = event_category_e,
    post_start = post_start_e,
    post_short_end = post_short_end_e,
    post_long_end = post_long_end_e
  )]
  
  # Exposure indicator
  analysis_e[, exposed := as.integer(preferred_style_id == declining_style_id)]
  
# Post-event purchases
  event_customers <- unique(analysis_e$customer_id)
  
  post_tx_e <- tx[
    week >= post_start_e &
      week <= post_long_end_e &
      customer_id %chin% event_customers
  ]
  
  if (nrow(post_tx_e) > 0) {
    
    post_outcomes_e <- post_tx_e[, .(
      short_term_revenue = sum(price[week <= post_short_end_e], na.rm = TRUE),
      long_term_revenue = sum(price, na.rm = TRUE),
      same_style_repurchase = as.integer(any(style_id == declining_style_e)),
      close_substitution = as.integer(any(
        broad_category == event_category_e &
          style_id != declining_style_e
      ))
    ), by = customer_id]
    
    analysis_e <- merge(
      analysis_e,
      post_outcomes_e,
      by = "customer_id",
      all.x = TRUE
    )
    
  } else {
    
    analysis_e[, `:=`(
      short_term_revenue = 0,
      long_term_revenue = 0,
      same_style_repurchase = 0,
      close_substitution = 0
    )]
  }
  
  # Fill missing post-event outcomes with zero
  analysis_e[is.na(short_term_revenue), short_term_revenue := 0]
  analysis_e[is.na(long_term_revenue), long_term_revenue := 0]
  analysis_e[is.na(same_style_repurchase), same_style_repurchase := 0]
  analysis_e[is.na(close_substitution), close_substitution := 0]
  
  # Inactivity outcomes
  analysis_e[, inactive_4w := as.integer(short_term_revenue == 0)]
  analysis_e[, inactive_12w := as.integer(long_term_revenue == 0)]
  
  # Mutually exclusive response type
  analysis_e[, response_type := fifelse(
    inactive_12w == 1,
    "Inactive",
    fifelse(
      same_style_repurchase == 1,
      "Same Style",
      fifelse(
        close_substitution == 1,
        "Close Substitute",
        "Other Purchase"
      )
    )
  )]
  
  return(analysis_e)
}

# 8. Process all valid events in batches

BATCH_SIZE <- 50

n_batches <- ceiling(nrow(events_final) / BATCH_SIZE)

cat("Number of batches:", n_batches, "\n")
cat("Batch size:", BATCH_SIZE, "events\n")

# Final output files
final_dataset_file <- file.path(
  data_dir,
  "final_customer_event_analysis_dataset_all_valid_events.csv"
)

summary_file <- file.path(
  data_dir,
  "summary_statistics_customer_event_sample.csv"
)

latex_summary_file <- file.path(
  data_dir,
  "summary_statistics_table.tex"
)

# Remove existing final dataset if rerunning
if (file.exists(final_dataset_file)) {
  file.remove(final_dataset_file)
}

# Variables for summary statistics
summary_vars <- c(
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

summary_labels <- c(
  exposed = "Exposed customer",
  high_value = "High-value customer",
  inactive_4w = "Inactive within 4 weeks",
  inactive_12w = "Inactive within 12 weeks",
  same_style_repurchase = "Same-style repurchase",
  close_substitution = "Close substitution",
  short_term_revenue = "Short-term revenue",
  long_term_revenue = "Long-term revenue",
  pre_event_spending = "Pre-event spending",
  pre_event_purchase_frequency = "Pre-event purchase frequency",
  pre_event_active_weeks = "Pre-event active weeks",
  pre_event_unique_styles = "Pre-event unique styles"
)

# This object stores exact mean, SD, min, max summaries without loading all batches together.
summary_accumulator <- data.table(
  variable = summary_vars,
  n = 0,
  sum_x = 0,
  sum_x2 = 0,
  min_x = Inf,
  max_x = -Inf
)

event_count_total <- 0
obs_count_total <- 0

start_time_all <- Sys.time()

for (b in seq_len(n_batches)) {
  
  batch_start <- (b - 1) * BATCH_SIZE + 1
  batch_end <- min(b * BATCH_SIZE, nrow(events_final))
  
  events_batch <- events_final[batch_start:batch_end]
  
  cat(
    "\nProcessing batch", b, "out of", n_batches,
    "| events", batch_start, "to", batch_end, "\n"
  )
  
  analysis_list_batch <- vector("list", nrow(events_batch))
  
  for (j in seq_len(nrow(events_batch))) {
    analysis_list_batch[[j]] <- build_one_event(events_batch[j])
  }
  
  analysis_batch <- rbindlist(
    analysis_list_batch,
    fill = TRUE
  )
  
  if (nrow(analysis_batch) == 0) {
    cat("Batch", b, "has no observations. Skipping.\n")
    next
  }
  

  
  # Define high-value customers within each event
  analysis_batch[, high_value := as.integer(
    pre_event_spending >= median(pre_event_spending, na.rm = TRUE)
  ), by = event_id]
  
  # Final dataset
  fwrite(
    analysis_batch,
    final_dataset_file,
    append = file.exists(final_dataset_file),
    col.names = !file.exists(final_dataset_file)
  )
  
  # Update summary accumulator
  available_summary_vars <- intersect(summary_vars, names(analysis_batch))
  
  for (v in available_summary_vars) {
    
    x <- analysis_batch[[v]]
    x <- x[!is.na(x)]
    
    if (length(x) > 0) {
      summary_accumulator[variable == v, `:=`(
        n = n + length(x),
        sum_x = sum_x + sum(x),
        sum_x2 = sum_x2 + sum(x^2),
        min_x = min(min_x, min(x)),
        max_x = max(max_x, max(x))
      )]
    }
  }
  
 
  obs_count_total <- obs_count_total + nrow(analysis_batch)
  
  cat(
    "Saved batch", b,
    "| observations:", nrow(analysis_batch),
    "| cumulative observations:", obs_count_total,
    "| time used:",
    round(as.numeric(Sys.time() - start_time_all, units = "mins"), 2),
    "minutes\n"
  )
  
  rm(analysis_list_batch, analysis_batch)
  gc()
}

cat("\nFinished all batches.\n")
cat("Total observations saved:", obs_count_total, "\n")
cat("Total events processed:", nrow(events_final), "\n")
cat(
  "Total time used:",
  round(as.numeric(Sys.time() - start_time_all, units = "mins"), 2),
  "minutes\n"
)

# 9. Create final summary statistics table

summary_stats_print <- copy(summary_accumulator)

summary_stats_print[, Mean := sum_x / n]

summary_stats_print[, SD := sqrt(
  (sum_x2 - (sum_x^2 / n)) / pmax(n - 1, 1)
)]

summary_stats_print[, Variable := summary_labels[variable]]

summary_stats_print <- summary_stats_print[, .(
  Variable,
  N = n,
  Mean,
  SD,
  Min = min_x,
  Max = max_x
)]

# Replace Inf values if any variable was missing
summary_stats_print[is.infinite(Min), Min := NA_real_]
summary_stats_print[is.infinite(Max), Max := NA_real_]

# Round for presentation
num_cols <- c("Mean", "SD", "Min", "Max")

summary_stats_print[, (num_cols) := lapply(
  .SD,
  function(x) round(x, 3)
), .SDcols = num_cols]

print(summary_stats_print)

fwrite(
  summary_stats_print,
  summary_file
)

latex_summary_table <- kable(
  summary_stats_print,
  format = "latex",
  booktabs = TRUE,
  caption = "Summary Statistics of the Customer-Event Analysis Sample",
  label = "tab:summary_stats",
  align = c("l", "r", "r", "r", "r", "r")
)

writeLines(
  latex_summary_table,
  latex_summary_file
)

cat("Final dataset saved to:", final_dataset_file, "\n")
cat("Summary statistics CSV saved to:", summary_file, "\n")
cat("LaTeX summary table saved to:", latex_summary_file, "\n")

