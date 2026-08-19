# =============================================================================
# helpers.R — shared data entry, layer definitions and model helpers.
# Every analysis module source()s this file and never re-filters or re-derives
# design columns locally.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lme4)
  library(lmerTest)
})

source(file.path("code", "load_data.R"))

# Output root: each module writes into output/<analysis id>/.
OUT_ROOT <- "output"
outdir <- function(analysis) {
  d <- file.path(OUT_ROOT, analysis)
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

# ---- Single data entry -------------------------------------------------------

stimuli <- read_frozen_csv("stimuli.csv")
observations <- read_frozen_csv("observations.csv")
participants <- read_frozen_csv("participants.csv")
source_shares <- read_frozen_csv("source_shares.csv")
source_counts <- read_frozen_csv("source_counts.csv")

# Design-factor levels, fixed in the order the tables use.
FUNC_LEVELS <- c("TTF", "VEF", "ITF", "TAF", "IWF", "LAF")
FUNC_FULL <- c(
  TTF = "Traffic transfer function",
  VEF = "Vehicle function",
  ITF = "Internal traffic function",
  TAF = "Traffic auxiliary function",
  IWF = "Internal waiting function",
  LAF = "Living auxiliary function"
)
SPL_LEVELS <- c("HSPL", "MSPL", "LSPL")

stimuli <- stimuli %>%
  mutate(
    FuncCode = names(FUNC_FULL)[match(Func_Type, FUNC_FULL)],
    FuncCode = factor(FuncCode, levels = FUNC_LEVELS),
    SPL_Group = factor(SPL_Group, levels = SPL_LEVELS)
  )

# Recording-level table carrying the four acoustic layers + the 3-class source
# shares (the unit of the descriptive / ANOVA / correlation analyses, N = 36).
stim36 <- stimuli %>%
  left_join(source_shares %>% select(StimID, Type1_AFS, Type2_SNS, Type3_DCS),
            by = c("Stimuli" = "StimID")) %>%
  rename(StimID = Stimuli)

# Observation-level modelling table (N = 2,116): each rating joined to its
# recording's acoustic indicators and source shares.
obs <- observations %>%
  left_join(stim36 %>% select(-ISOP, -ISOE), by = "StimID") %>%
  mutate(SubjID = factor(SubjID), StimID = factor(StimID))

# ---- Variable groupings (the four theoretical layers) ------------------------

LAYER_ENERGY <- c("LAeq", "LCeq", "LCLA", "LA50", "LC50")
LAYER_PSYCHO <- c("N50", "S50", "R50", "F50", "T50", "AI50")
LAYER_TEMPORAL <- c("LA10LA90", "LC10LC90", "N10N90", "S10S90",
                    "R10R90", "F10F90", "T10T90", "AI10AI90")
LAYER_SOURCE <- c("Type1_AFS", "Type2_SNS", "Type3_DCS")
ALL_ACOUSTIC <- c(LAYER_ENERGY, LAYER_PSYCHO, LAYER_TEMPORAL, LAYER_SOURCE)

DIM8 <- c("Pleasant", "Vibrant", "Eventful", "Chaotic",
          "Annoying", "Monotonous", "Uneventful", "Calm")

VAR_LABEL <- c(
  LAeq = "LAeq (dB(A))", LCeq = "LCeq (dB(C))", LCLA = "LC-LA (dB)",
  LA50 = "LA50 (dB(A))", LC50 = "LC50 (dB(C))",
  N50 = "N50 (sone)", S50 = "S50 (acum)", R50 = "R50 (asper)",
  F50 = "F50 (vacil)", T50 = "T50 (tu)", AI50 = "AI50 (%)",
  LA10LA90 = "LA10-LA90 (dB(A))", LC10LC90 = "LC10-LC90 (dB(C))",
  N10N90 = "N10-N90 (sone)", S10S90 = "S10-S90 (acum)",
  R10R90 = "R10-R90 (asper)", F10F90 = "F10-F90 (vacil)",
  T10T90 = "T10-T90 (tu)", AI10AI90 = "AI10-AI90 (%)",
  Type1_AFS = "Type 1 artificial functional sounds (%)",
  Type2_SNS = "Type 2 soothing natural sounds (%)",
  Type3_DCS = "Type 3 daily communication sounds (%)"
)

# ---- Small helpers -----------------------------------------------------------

# mean ± sd, formatted to the tables' precision.
ms <- function(x, digits = 2) {
  sprintf(paste0("%.", digits, "f±%.", digits, "f"),
          mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
}

# Grouping scheme used by the descriptive tables:
# Total, then the six functional types, then the three level ranks.
grouped_rows <- function(df, vars, digits = 2) {
  blocks <- list(
    tibble(Group = "Total", df),
    df %>% mutate(Group = as.character(FuncCode)) %>% arrange(FuncCode),
    df %>% mutate(Group = as.character(SPL_Group)) %>% arrange(SPL_Group)
  )
  bind_rows(blocks) %>%
    mutate(Group = factor(Group, levels = c("Total", FUNC_LEVELS, SPL_LEVELS))) %>%
    group_by(Group) %>%
    summarise(across(all_of(vars), ~ ms(.x, digits)), .groups = "drop") %>%
    arrange(Group)
}

# p-value formatting used across the result tables.
fmt_p <- function(p, digits = 3) {
  ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = digits))
}

stars <- function(p) {
  ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
}

# Crossed random-intercept model on the observation table.
# `fixed` is a character vector of predictor names (empty for the null model).
fit_crossed <- function(outcome, fixed = character(0), data = obs, REML = FALSE,
                        extra_random = NULL) {
  rhs <- if (length(fixed) == 0) "1" else paste(c("1", fixed), collapse = " + ")
  ran <- c("(1|SubjID)", "(1|StimID)", extra_random)
  form <- as.formula(paste(outcome, "~", rhs, "+", paste(ran, collapse = " + ")))
  lmerTest::lmer(form, data = data, REML = REML,
                 control = lmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5)))
}

# Variance components of a fitted crossed model, in the tables' row order.
var_components <- function(m) {
  vc <- as.data.frame(VarCorr(m))
  subj <- vc$vcov[vc$grp == "SubjID"]
  stim <- vc$vcov[vc$grp == "StimID"]
  resid <- vc$vcov[vc$grp == "Residual"]
  tibble(component = c("Subject", "Stimulus", "Residual", "Total"),
         variance = c(subj, stim, resid, subj + stim + resid))
}

# Nakagawa marginal / conditional R-squared.
r2_nak <- function(m) {
  r <- performance::r2_nakagawa(m)
  c(marginal = as.numeric(r$R2_marginal), conditional = as.numeric(r$R2_conditional))
}

# Write a result table into the calling module's output folder.
write_outcome <- function(x, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(x, path)
  invisible(x)
}

message(sprintf("[helpers] stimuli %d x %d | observations %d x %d | participants %d",
                nrow(stim36), ncol(stim36), nrow(obs), ncol(obs), nrow(participants)))
