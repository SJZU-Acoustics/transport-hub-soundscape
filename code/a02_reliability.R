# A02 — Reliability of the eight ISO/TS 12913-2 attributes.
# Two orientations are computed: cases = listeners with the 36 recordings as
# items (Cronbach's alpha and ICC(2,1) of a single rating), and its transpose,
# targets = 36 recordings rated by 57 listwise-complete listeners, which is the
# inter-rater reliability of a recording's mean. Feeds Supplementary Table S6.

source(file.path("code", "helpers.R"))
suppressPackageStartupMessages(library(psych))
OUT <- outdir("a02_reliability")

wide_for <- function(dim) {
  observations %>%
    select(SubjID, StimID, value = all_of(dim)) %>%
    pivot_wider(names_from = StimID, values_from = value) %>%
    arrange(SubjID) %>%
    select(-SubjID) %>%
    as.data.frame()
}

res <- map_dfr(DIM8, function(d) {
  w <- wide_for(d)
  w_lw <- na.omit(w)                      # SPSS default: listwise
  a <- psych::alpha(w_lw, warnings = FALSE)
  icc <- psych::ICC(w_lw, lmer = FALSE)$results
  get <- function(type, col) icc[[col]][icc$type == type]
  tibble(
    Dimension = d,
    n_listwise = nrow(w_lw),
    n_items = ncol(w_lw),
    Cronbach_alpha = round(a$total$raw_alpha, 3),
    ICC1_single = round(get("ICC1", "ICC"), 3),
    ICC2_single = round(get("ICC2", "ICC"), 3),
    ICC3_single = round(get("ICC3", "ICC"), 3),
    ICC1k_average = round(get("ICC1k", "ICC"), 3),
    ICC2k_average = round(get("ICC2k", "ICC"), 3),
    ICC3k_average = round(get("ICC3k", "ICC"), 3)
  )
})
write_outcome(res, file.path(OUT, "table05_reliability_full.csv"))

# ---- Orientation check -------------------------------------------------------
# The run above has cases = subjects and items = stimuli, so its ICC treats the
# 36 stimuli as "raters" and the 57 subjects as "targets". Genuine inter-rater
# reliability is the transpose: targets = 36 stimuli, raters = 57 subjects.
res_t <- map_dfr(DIM8, function(d) {
  w <- na.omit(wide_for(d))
  wt <- as.data.frame(t(as.matrix(w)))    # 36 stimuli x 57 subjects
  a <- psych::alpha(wt, warnings = FALSE)
  icc <- psych::ICC(wt, lmer = FALSE)$results
  get <- function(type) icc$ICC[icc$type == type]
  tibble(Dimension = d, n_targets = nrow(wt), n_raters = ncol(wt),
         alpha_T = round(a$total$raw_alpha, 3),
         ICC2_single_T = round(get("ICC2"), 3),
         ICC2k_average_T = round(get("ICC2k"), 3),
         ICC3k_average_T = round(get("ICC3k"), 3))
})
write_outcome(res_t, file.path(OUT, "table05_transposed_true_interrater.csv"))

cat("\n== Reliability, all ICC variants ==\n"); print(as.data.frame(res))
cat("\n== Transposed orientation (targets = 36 stimuli, raters = 57 subjects) ==\n")
print(as.data.frame(res_t))
