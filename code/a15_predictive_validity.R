# =============================================================================
# A15 — Predictive validity of the adopted models (leave-one-stimulus-out CV)
# Every R2 in the model tables is in-sample. The claim a driver framework
# earns is prediction of recordings no listener in the fit has heard.
# Design: for each of the 36 recordings, refit the adopted model without that
# recording, then predict its held-out ratings with listener BLUPs only
# (re.form = ~(1|SubjID); the new recording's random effect is unknowable and
# set to 0 — exactly the deployment situation). Baseline: the null model
# (intercept + subject BLUPs) under the same scheme.
# Metrics (descriptive, no tests — declared policy):
#   - recording-level: r and R2 between predicted and observed held-out
#     recording means (the planning-relevant quantity), RMSE of means;
#   - observation-level RMSE, and its % reduction vs the null baseline.
# Feeds Fig. 5 and Supplementary Table S11.
# =============================================================================

source(file.path("code", "helpers.R"))

OUT <- outdir("a15_predictive_validity")

ADOPTED <- list(
  ISOP = c("LAeq", "T50", "Type2_SNS"),
  ISOE = c("LCLA", "T50", "AI50", "LA10LA90", "Type1_AFS")
)

loso <- function(outcome, fixed) {
  purrr::map_dfr(levels(obs$StimID), function(s) {
    train <- obs %>% filter(StimID != s)
    test  <- obs %>% filter(StimID == s)
    m  <- fit_crossed(outcome, fixed, data = train, REML = TRUE)
    m0 <- fit_crossed(outcome, character(0), data = train, REML = TRUE)
    tibble(StimID = s,
           obs_mean  = mean(test[[outcome]]),
           pred      = list(predict(m,  newdata = test, re.form = ~(1 | SubjID))),
           pred_null = list(predict(m0, newdata = test, re.form = ~(1 | SubjID))),
           y         = list(test[[outcome]]))
  })
}

metrics <- function(cv, outcome) {
  stim <- cv %>% mutate(pred_mean = map_dbl(pred, mean),
                        pred_null_mean = map_dbl(pred_null, mean))
  y_all  <- unlist(cv$y)
  p_all  <- unlist(cv$pred)
  p0_all <- unlist(cv$pred_null)
  tibble(
    outcome = outcome,
    r_stim = cor(stim$obs_mean, stim$pred_mean),
    R2_stim = cor(stim$obs_mean, stim$pred_mean)^2,
    rmse_stim = sqrt(mean((stim$obs_mean - stim$pred_mean)^2)),
    rmse_stim_null = sqrt(mean((stim$obs_mean - stim$pred_null_mean)^2)),
    sd_stim_means = sd(stim$obs_mean),
    rmse_obs = sqrt(mean((y_all - p_all)^2)),
    rmse_obs_null = sqrt(mean((y_all - p0_all)^2)),
    rmse_obs_reduction_pct = 100 * (1 - sqrt(mean((y_all - p_all)^2)) /
                                        sqrt(mean((y_all - p0_all)^2)))
  )
}

per_stim_all <- list(); summ <- list()
for (oc in names(ADOPTED)) {
  cv <- loso(oc, ADOPTED[[oc]])
  per_stim <- cv %>%
    transmute(outcome = oc, StimID,
              obs_mean, pred_mean = map_dbl(pred, mean),
              abs_err = abs(obs_mean - pred_mean))
  per_stim_all[[oc]] <- per_stim
  summ[[oc]] <- metrics(cv, oc)
}
per_stim_all <- bind_rows(per_stim_all)
summ <- bind_rows(summ)

write_outcome(per_stim_all, file.path(OUT, "loso_per_stimulus.csv"))
write_outcome(summ, file.path(OUT, "loso_summary.csv"))
print(as.data.frame(summ))

# Worst-predicted stimuli (diagnostic context for the report).
worst <- per_stim_all %>% group_by(outcome) %>% slice_max(abs_err, n = 5) %>% ungroup()
write_outcome(worst, file.path(OUT, "worst_predicted.csv"))
print(as.data.frame(worst))
