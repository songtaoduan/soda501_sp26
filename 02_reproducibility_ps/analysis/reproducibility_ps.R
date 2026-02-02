
###############################################################################
# SODA 501 PS2: Reproducibility 
# Songtao Duan
# Three regressions + plot (Income as DV) + logging + renv + session info
###############################################################################

### Part I. Conceptual Questions #### 
#  1. \texttt{\textbf renv} solves the reproducibility problem caused by changing or inconsistent R package environments: the same code can produce different results when run with different package versions. The \texttt{renv.lock} file records the exact package dependencies used in a project, including package names, versions, sources (CRAN, GitHub, etc.), and repository information, effectively snapshotting the computating environment. \texttt{renv::restore()} reads this lockfile and reinstalls those exact versions so another user can recreate the same environment. Sharing code without dependency versions often breaks replication because package updates can change defaults, algorithms, or even function behavior, meaning an analysis can be logically correct yet yield different results or errors under a different package setup.


### Part 2. Applied Exercises ####

# install.packages(c("renv", "logger", "tidyverse", "broom"))

library(renv)
library(logger)
library(tidyverse)
library(broom)

# save folders 
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# logging
log_threshold(INFO)
log_appender(appender_file("analysis_log.txt"))

log_info("=== START ANALYSIS ===")
log_info("Timestamp: {format(Sys.time(), '%Y-%m-%d %H:%M:%S %Z')}")
log_info("Working directory: {getwd()}")

# load data
input_path <- "data/raw/education_income.csv"
stopifnot(file.exists(input_path))

df_raw <- readr::read_csv(input_path, show_col_types = FALSE)
log_info("Rows loaded: {nrow(df_raw)}")
log_info("Columns: {paste(names(df_raw), collapse = ', ')}")

# clean
df <- df_raw |>
  mutate(
    education = as.numeric(education),
    income    = as.numeric(income)
  ) |>
  filter(!is.na(education), !is.na(income))

log_info("Rows after cleaning: {nrow(df)}")

readr::write_csv(df, "data/processed/cleaned_education_income.csv")
log_info("Wrote processed data: data/processed/cleaned_education_income.csv")

# regressions
m1 <- lm(income ~ education, data = df)
m2 <- lm(income ~ education + I(education^2), data = df)

df_log <- df |>
  mutate(log_income = log(income)) |>
  filter(is.finite(log_income))

m3 <- lm(log_income ~ education, data = df_log)

# save model summaries
writeLines(capture.output(summary(m1)), "outputs/tables/model_1_summary.txt")
writeLines(capture.output(summary(m2)), "outputs/tables/model_2_summary.txt")
writeLines(capture.output(summary(m3)), "outputs/tables/model_3_summary.txt")

# save coefficients table
coef_table <- bind_rows(
  tidy(m1) |> mutate(model = "Model 1"),
  tidy(m2) |> mutate(model = "Model 2"),
  tidy(m3) |> mutate(model = "Model 3")
)

readr::write_csv(coef_table, "outputs/tables/regression_coefficients.csv")

# plot
p <- ggplot(df, aes(x = education, y = income)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Income vs Education",
    subtitle = "Linear fit (Model 1)",
    x = "Education",
    y = "Income"
  )

ggsave(
  "outputs/figures/education_income_scatter.png",
  plot = p,
  width = 7,
  height = 5,
  dpi = 300
)

# session info
writeLines(capture.output(sessionInfo()), "outputs/session_info.txt")

# renv lockfile
renv::snapshot()

log_info("=== END ANALYSIS (SUCCESS) ===")

