# =============================================================================
# A14 — Pre-specified moderation and nonlinearity tests on the adopted models
# The four-layer framework is strictly additive-linear. Two theory-led
# departures are tested for each outcome, declared in advance as ONE family of
# four tests (BH-FDR across 4):
#   1. ISOP: LAeq x Type2_SNS  — do natural and musical sounds buffer the
#      level penalty on pleasantness? (semantic moderation)
#   2. ISOP: LAeq^2            — is the level penalty nonlinear?
#   3. ISOE: LA10LA90 x Type1_AFS — does level fluctuation drive eventfulness
#      more when functional sounds dominate?
#   4. ISOE: (LA10LA90)^2      — does eventfulness saturate in fluctuation?
# Each term is added to the adopted model; ML LRT (1 df) is the test, REML
# coefficient reported. Continuous moderators grand-mean centred on the
# 36-stimulus means. Any FDR survivor is re-tested with functional type
# (FuncCode) as fixed covariate BEFORE being recorded as positive (the
# stimulus-level confound control), and the effective N for all these terms
# is 36 recordings, not 2,116 observations. Feeds Supplementary Table S5.
# =============================================================================

source(file.path("code", "helpers.R"))

OUT <- outdir("a14_moderation_nonlinearity")

# Centre stimulus-level continuous variables on the 36-stimulus mean.
ctr <- function(v) v - mean(stim36[[cur_var]])  # not used; explicit below
for (v in c("LAeq", "LCLA", "T50", "AI50", "LA10LA90", "Type1_AFS", "Type2_SNS")) {
  obs[[paste0(v, "_c")]] <- obs[[v]] - mean(stim36[[v]])
}

ADOPTED <- list(
  ISOP = c("LAeq_c", "T50", "Type2_SNS_c"),
  ISOE = c("LCLA", "T50", "AI50", "LA10LA90_c", "Type1_AFS_c")
)

TESTS <- tribble(
  ~id, ~outcome, ~extra,
  "ISOP_LAeqxType2", "ISOP", "LAeq_c:Type2_SNS_c",
  "ISOP_LAeq2",      "ISOP", "I(LAeq_c^2)",
  "ISOE_FlucxType1", "ISOE", "LA10LA90_c:Type1_AFS_c",
  "ISOE_Fluc2",      "ISOE", "I(LA10LA90_c^2)"
)

run_test <- function(id, outcome, extra, covariate = NULL) {
  base_fx <- c(ADOPTED[[outcome]], covariate)
  m0 <- fit_crossed(outcome, base_fx, data = obs, REML = FALSE)
  m1 <- fit_crossed(outcome, c(base_fx, extra), data = obs, REML = FALSE)
  lrt <- anova(m0, m1)
  m1r <- fit_crossed(outcome, c(base_fx, extra), data = obs, REML = TRUE)
  co <- as.data.frame(summary(m1r)$coefficients) %>% rownames_to_column("term")
  term_row <- co[nrow(co), ]  # the added term enters last
  tibble(id = id, outcome = outcome, term = extra,
         control = ifelse(is.null(covariate), "none", covariate),
         AIC_base = AIC(m0), AIC_ext = AIC(m1), dAIC = AIC(m1) - AIC(m0),
         chisq = lrt$Chisq[2], p_lrt = lrt$`Pr(>Chisq)`[2],
         est = term_row$Estimate, se = term_row$`Std. Error`,
         df = term_row$df, t = term_row$`t value`, p_reml = term_row$`Pr(>|t|)`)
}

primary <- purrr::pmap_dfr(TESTS, run_test) %>%
  mutate(q = p.adjust(p_lrt, method = "BH"), family_n = 4, sig_fdr = q < 0.05)
write_outcome(primary, file.path(OUT, "moderation_tests.csv"))
print(as.data.frame(primary))

# FuncCode control for FDR survivors (run for nominal p<.05 too, labelled).
need_ctl <- primary %>% filter(p_lrt < 0.05)
if (nrow(need_ctl) > 0) {
  ctl <- purrr::pmap_dfr(TESTS %>% filter(id %in% need_ctl$id),
                         function(id, outcome, extra)
                           run_test(id, outcome, extra, covariate = "FuncCode"))
  write_outcome(ctl, file.path(OUT, "funccode_control.csv"))
  print(as.data.frame(ctl))
} else {
  writeLines("no test at p<.05; control not needed", file.path(OUT, "funccode_control.txt"))
}

# Simple-slope description for any interaction at nominal p<.05 (context only).
if (any(primary$p_lrt < 0.05 & grepl(":", primary$term))) {
  sl <- primary %>% filter(p_lrt < 0.05, grepl(":", term))
  desc <- purrr::pmap_dfr(sl, function(id, outcome, term, ...) {
    mod <- if (outcome == "ISOP") "Type2_SNS" else "Type1_AFS"
    foc <- if (outcome == "ISOP") "LAeq_c" else "LA10LA90_c"
    qs <- quantile(stim36[[mod]], c(.25, .75))
    m1r <- fit_crossed(outcome, c(ADOPTED[[outcome]], term), data = obs, REML = TRUE)
    fx <- fixef(m1r)
    b_foc <- fx[foc]; b_int <- fx[grep(":", names(fx))]
    tibble(id = id,
           slope_at_modQ1 = b_foc + b_int * (qs[1] - mean(stim36[[mod]])),
           slope_at_modQ3 = b_foc + b_int * (qs[2] - mean(stim36[[mod]])))
  })
  write_outcome(desc, file.path(OUT, "simple_slopes.csv"))
  print(as.data.frame(desc))
}
