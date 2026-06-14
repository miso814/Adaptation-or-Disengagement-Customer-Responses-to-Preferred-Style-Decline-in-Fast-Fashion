# Baseline Event Fixed Effects Regressions
# Table 4 in the paper

rm(list = ls(all.names = TRUE))
gc()


library(data.table)
library(fixest)
data_dir <- path.expand("~/Desktop/H&M")

analysis_main <- fread(
  file.path(data_dir, "final_customer_event_analysis_dataset_all_valid_events.csv"),
  select = c(
    "event_id",
    "exposed",
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

analysis_main[, event_id := as.factor(event_id)]

m_inactive_4w <- feols(
  inactive_4w ~ exposed +
    pre_event_spending +
    pre_event_purchase_frequency +
    pre_event_active_weeks +
    pre_event_unique_styles | event_id,
  data = analysis_main,
  cluster = ~event_id,
  lean = TRUE,
  mem.clean = TRUE
)

# summary(m_inactive_4w)

m_inactive_12w <- feols(
  inactive_12w ~ exposed +
    pre_event_spending +
    pre_event_purchase_frequency +
    pre_event_active_weeks +
    pre_event_unique_styles | event_id,
  data = analysis_main,
  cluster = ~event_id,
  lean = TRUE,
  mem.clean = TRUE
)

m_same_style <- feols(
  same_style_repurchase ~ exposed +
    pre_event_spending +
    pre_event_purchase_frequency +
    pre_event_active_weeks +
    pre_event_unique_styles | event_id,
  data = analysis_main,
  cluster = ~event_id,
  lean = TRUE,
  mem.clean = TRUE
)

m_close_sub <- feols(
  close_substitution ~ exposed +
    pre_event_spending +
    pre_event_purchase_frequency +
    pre_event_active_weeks +
    pre_event_unique_styles | event_id,
  data = analysis_main,
  cluster = ~event_id,
  lean = TRUE,
  mem.clean = TRUE
)

m_short_rev <- feols(
  short_term_revenue ~ exposed +
    pre_event_spending +
    pre_event_purchase_frequency +
    pre_event_active_weeks +
    pre_event_unique_styles | event_id,
  data = analysis_main,
  cluster = ~event_id,
  lean = TRUE,
  mem.clean = TRUE
)

m_long_rev <- feols(
  long_term_revenue ~ exposed +
    pre_event_spending +
    pre_event_purchase_frequency +
    pre_event_active_weeks +
    pre_event_unique_styles | event_id,
  data = analysis_main,
  cluster = ~event_id,
  lean = TRUE,
  mem.clean = TRUE
)

etable(
  m_inactive_4w,
  m_inactive_12w,
  m_same_style,
  m_close_sub,
  m_short_rev,
  m_long_rev
)

