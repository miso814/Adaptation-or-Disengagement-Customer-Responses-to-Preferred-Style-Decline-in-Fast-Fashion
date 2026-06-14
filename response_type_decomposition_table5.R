# Response-Type Decomposition
# Table 5 in the paper

rm(list = ls(all.names = TRUE))
gc()

library(data.table)
library(knitr)

data_dir <- path.expand("~/Desktop/H&M")

# 1. Read only variables needed for response-type decomposition

analysis_response <- fread(
  file.path(data_dir, "final_customer_event_analysis_dataset_all_valid_events.csv"),
  select = c(
    "exposed",
    "inactive_12w",
    "same_style_repurchase",
    "close_substitution"
  )
)

analysis_response[, exposed := as.integer(exposed)]

# 2. Construct mutually exclusive response type
# Hierarchical priority:
# 1. Inactive
# 2. Same-style repurchase
# 3. Close substitution
# 4. Other purchase

analysis_response[, response_type := fifelse(
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

analysis_response[, response_type := factor(
  response_type,
  levels = c(
    "Inactive",
    "Same Style",
    "Close Substitute",
    "Other Purchase"
  )
)]

# 3. Response composition by exposure status

response_by_exposure <- analysis_response[, .(
  N = .N
), by = .(exposed, response_type)]

response_by_exposure[, Share := N / sum(N), by = exposed]

response_wide <- dcast(
  response_by_exposure,
  response_type ~ exposed,
  value.var = "Share"
)

setnames(
  response_wide,
  c("response_type", "0", "1"),
  c("Response type", "Non-exposed share", "Exposed share")
)

response_wide[, Difference := `Exposed share` - `Non-exposed share`]

response_wide[, `:=`(
  `Non-exposed share` = round(100 * `Non-exposed share`, 3),
  `Exposed share` = round(100 * `Exposed share`, 3),
  Difference = round(100 * Difference, 3)
)]

print(response_wide)

fwrite(
  response_wide,
  file.path(data_dir, "response_type_decomposition_by_exposure.csv")
)

latex_response_table <- kable(
  response_wide,
  format = "latex",
  booktabs = TRUE,
  caption = "Response-Type Decomposition by Exposure Status",
  label = "tab:response_type_decomposition",
  col.names = c(
    "Response type",
    "Non-exposed share",
    "Exposed share",
    "Difference"
  ),
  align = c("l", "r", "r", "r")
)

writeLines(
  latex_response_table,
  file.path(data_dir, "response_type_decomposition_table5.tex")
)

