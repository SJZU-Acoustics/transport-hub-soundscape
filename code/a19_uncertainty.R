# =============================================================================
# A19 — Interval estimates for the adopted models (revision 1)
#   - 95% t intervals (Satterthwaite df) for every fixed effect of the adopted
#     REML models, conditional on the selected specification  -> Table 3;
#   - the same intervals for the four pre-specified interaction / curvature
#     terms of A14                                             -> Table S5;
#   - percentile intervals of the null-model variance shares from 1,000
#     parametric bootstrap samples per outcome (bootMer, serial, seeded)
#                                                              -> Table S7;
#   - two quantities used in the response to reviewers only: the algebra of a
#     common gain shift (a constant added to LAeq moves the intercept, not the
#     slope) and the ISO-P / ISO-E correlation at rating and recording level.
# =============================================================================

source(file.path("code", "helpers.R"))

OUT <- outdir("a19_uncertainty")

SPECS <- list(
  ISOP = c("LAeq", "T50", "Type2_SNS"),
  ISOE = c("LCLA", "T50", "AI50", "LA10LA90", "Type1_AFS")
)

# ---- Fixed-effect intervals -----------------------------------------------------
ci <- list(); baseline <- list()
for (y in names(SPECS)) {
  m <- fit_crossed(y, SPECS[[y]], REML = TRUE); s <- coef(summary(m))
  ci[[y]] <- tibble(outcome = y, term = rownames(s), B = s[, 1], SE = s[, 2], df = s[, 3], p = s[, 5],
                    lo = s[, 1] - qt(.975, s[, 3]) * s[, 2], hi = s[, 1] + qt(.975, s[, 3]) * s[, 2])
  ml <- fit_crossed(y, SPECS[[y]], REML = FALSE)
  baseline[[y]] <- tibble(outcome = y, AIC = AIC(ml), singular = isSingular(m),
                          diagnostic = paste(unlist(m@optinfo$conv$lme4$messages), collapse = " | "))
}
write_outcome(bind_rows(ci), file.path(OUT, "fixed_effect_intervals.csv"))
write_outcome(bind_rows(baseline), file.path(OUT, "adopted_baseline.csv"))
# The adopted-model AICs of Table 2 / Table S3.
stopifnot(abs(baseline$ISOP$AIC - 1265.284560264004) < 1e-4,
          abs(baseline$ISOE$AIC - 1143.909059985518) < 1e-4)

# ---- Intervals for the A14 interaction / curvature coefficients -----------------
# Saved REML estimates and Satterthwaite df from A14; no new hypothesis test.
a14 <- readr::read_csv(file.path("output", "a14_moderation_nonlinearity", "moderation_tests.csv"),
                       show_col_types = FALSE) %>%
  mutate(CI_low = est - qt(.975, df) * se, CI_high = est + qt(.975, df) * se)
write_outcome(a14, file.path(OUT, "moderation_intervals.csv"))

# ---- Parametric bootstrap of the null-model variance shares ---------------------
# New listener, recording and residual draws at every replicate; ML refits.
# Serial and seeded so the replicates regenerate exactly on any machine.
varfun <- function(m) {
  v <- var_components(m); x <- v$variance[1:3]
  setNames(100 * x / sum(x), v$component[1:3])
}
bs <- list(); di <- list()
for (y in names(SPECS)) {
  m <- fit_crossed(y, REML = FALSE)
  set.seed(if (y == "ISOP") 340919 else 340920)
  message("    bootstrap starting: ", y)
  b <- bootMer(m, FUN = varfun, nsim = 1000, use.u = FALSE, type = "parametric", parallel = "no")
  reps <- as_tibble(b$t, .name_repair = ~names(b$t0)); reps$replicate <- seq_len(nrow(reps))
  write_outcome(reps, file.path(OUT, paste0("variance_bootstrap_", y, ".csv")))
  bs[[y]] <- tibble(outcome = y, component = names(b$t0), estimate = as.numeric(b$t0),
                    lo = apply(b$t, 2, quantile, .025, na.rm = TRUE),
                    hi = apply(b$t, 2, quantile, .975, na.rm = TRUE),
                    successful = colSums(is.finite(b$t)))
  di[[y]] <- capture.output(list(bootFail = attr(b, "bootFail"),
                                 boot_failures = attr(b, "boot.fail.msgs"),
                                 all_messages = attr(b, "boot.all.msgs")))
}
write_outcome(bind_rows(bs), file.path(OUT, "variance_share_intervals.csv"))
writeLines(unlist(di), file.path(OUT, "bootstrap_diagnostics.txt"))

# ---- Response-letter quantities -------------------------------------------------
m0 <- fit_crossed("ISOP", SPECS$ISOP, REML = TRUE); gain <- list()
for (shift in c(-3, 3)) {
  d <- obs; d$LAeq <- d$LAeq + shift
  m <- fit_crossed("ISOP", SPECS$ISOP, data = d, REML = TRUE)
  gain[[as.character(shift)]] <- tibble(shift = shift, B_LAeq = fixef(m)["LAeq"], intercept = fixef(m)[1],
                                        max_fitted_difference = max(abs(fitted(m) - fitted(m0))))
}
write_outcome(bind_rows(gain), file.path(OUT, "gain_relabelling.csv"))

correlations <- tibble(level = c("rating", "recording_mean"), N = c(nrow(obs), nrow(stim36)),
                       r = c(cor(obs$ISOP, obs$ISOE), cor(stim36$ISOP, stim36$ISOE)))
write_outcome(correlations, file.path(OUT, "outcome_correlations.csv"))
