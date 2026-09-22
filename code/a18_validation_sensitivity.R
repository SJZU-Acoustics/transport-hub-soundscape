# =============================================================================
# A18 — Sensitivity of the cross-validated prediction and of the adopted
#       coefficients (revision 1)
# A15 validates the adopted specifications, which were selected on the full
# data. This module asks how much that selection step matters:
#   - two automatic versions of the layered forward screen, fixed before the
#     runs, are applied (a) once on the full data and then held fixed, and
#     (b) again inside every training fold ("fold re-selected");
#   - the adopted terms without the source share (acoustic-only ablation);
#   - leave-one-site-out for the adopted specifications and the null model;
#   - the adopted coefficients with each recording deleted in turn;
#   - a conditional range of R2_pred from resampling the 12 sites of the saved
#     predictions (no refitting: not a confidence interval for the pipeline);
#   - the LA10-LA90 coefficient in every A12 grid specification containing it.
# The automatic rules do not reproduce the judgement-based choices of the
# original screen; they describe sensitivity to selection, not a replay.
# Feeds Supplementary Tables S12 and S15.
# =============================================================================

source(file.path("code", "helpers.R"))

OUT <- outdir("a18_validation_sensitivity")

SPECS <- list(
  ISOP = c("LAeq", "T50", "Type2_SNS"),
  ISOE = c("LCLA", "T50", "AI50", "LA10LA90", "Type1_AFS")
)

fit <- function(y, xs, d, reml = TRUE) {
  w <- character()
  form <- as.formula(paste(y, "~", paste(c("1", xs, "(1|SubjID)", "(1|StimID)"),
                                          collapse = "+")))
  m <- withCallingHandlers(
    lme4::lmer(form, data = d, REML = reml,
               control = lmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5))),
    warning = function(e) { w <<- c(w, conditionMessage(e)); invokeRestart("muffleWarning") },
    message = function(e) { w <<- c(w, conditionMessage(e)); invokeRestart("muffleMessage") })
  attr(m, "revision_warnings") <-
    paste(unique(c(w, unlist(m@optinfo$conv$lme4$messages))), collapse = " | ")
  m
}

# Automatic layered forward screen. "strict": add the AIC-best remaining
# indicator of a layer while AIC falls and the 1-df LRT gives p < 0.05, layers
# in the framework's order and never revisited. "temporal_aic": the same, but
# temporal indicators are admitted on AIC alone.
select_terms <- function(y, d, variant) {
  xs <- character(); m <- fit(y, xs, d, FALSE); log <- list(); k <- 0
  for (layer in list(energy = LAYER_ENERGY, psycho = LAYER_PSYCHO,
                     temporal = LAYER_TEMPORAL, source = LAYER_SOURCE)) {
    repeat {
      candidates <- setdiff(layer, xs); if (!length(candidates)) break
      fits <- lapply(candidates, function(x)
        tryCatch(fit(y, c(xs, x), d, FALSE), error = function(e) e))
      valid <- vapply(fits, function(mm)
        inherits(mm, "merMod") && length(fixef(mm)) == length(xs) + 2, logical(1))
      aic <- vapply(seq_along(fits), function(i) if (valid[i]) AIC(fits[[i]]) else Inf, numeric(1))
      for (i in seq_along(fits)) {
        k <- k + 1
        log[[k]] <- tibble(base = paste(xs, collapse = "+"), candidate = candidates[i], AIC = aic[i],
                           diagnostic = if (inherits(fits[[i]], "merMod")) attr(fits[[i]], "revision_warnings")
                                        else conditionMessage(fits[[i]]),
                           rank_deficient = inherits(fits[[i]], "merMod") && !valid[i])
      }
      if (!any(is.finite(aic))) break
      j <- which.min(aic)
      lrt <- max(0, 2 * (as.numeric(logLik(fits[[j]])) - as.numeric(logLik(m))))
      p <- pchisq(lrt, 1, lower.tail = FALSE)
      relaxed <- variant == "temporal_aic" && identical(layer, LAYER_TEMPORAL)
      if (aic[j] < AIC(m) && (p < .05 || relaxed)) { xs <- c(xs, candidates[j]); m <- fits[[j]] } else break
    }
  }
  list(terms = xs, log = bind_rows(log))
}

# ---- Automatic rules on the full data ----------------------------------------
full <- list(); full_log <- list()
for (y in names(SPECS)) for (v in c("strict", "temporal_aic")) {
  key <- paste(y, v, sep = "_")
  full[[key]] <- select_terms(y, obs, v)
  full_log[[key]] <- full[[key]]$log %>% mutate(outcome = y, variant = v)
}
write_outcome(bind_rows(lapply(names(full), function(k)
  tibble(key = k, terms = paste(full[[k]]$terms, collapse = "+")))),
  file.path(OUT, "full_data_algorithm_models.csv"))
write_outcome(bind_rows(full_log), file.path(OUT, "full_data_candidate_log.csv"))

# ---- Leave-one-recording-out under every scheme -------------------------------
t_ci <- function(cs) tibble(term = rownames(cs), B = cs[, 1], SE = cs[, 2], df = cs[, 3], p = cs[, 5],
                            lo = cs[, 1] - qt(.975, cs[, 3]) * cs[, 2],
                            hi = cs[, 1] + qt(.975, cs[, 3]) * cs[, 2])

run_fold <- function(s) {
  train <- obs %>% filter(as.character(StimID) != s)
  test <- obs %>% filter(as.character(StimID) == s)
  predictions <- list(); coefs <- list(); sel_logs <- list(); k <- 0
  for (y in names(SPECS)) {
    variants <- list(adopted = SPECS[[y]],
                     acoustic_only = setdiff(SPECS[[y]], LAYER_SOURCE),
                     null = character())
    for (v in c("strict", "temporal_aic")) {
      variants[[paste0(v, "_full_selected_fixed")]] <- full[[paste(y, v, sep = "_")]]$terms
      selection <- select_terms(y, train, v)
      variants[[paste0(v, "_fold_reselected")]] <- selection$terms
      sel_logs[[paste(y, v)]] <- selection$log %>% mutate(holdout = s, outcome = y, variant = v)
    }
    for (v in names(variants)) {
      k <- k + 1
      m <- fit(y, variants[[v]], train, TRUE)
      pr <- predict(m, newdata = test, re.form = ~(1 | SubjID), allow.new.levels = TRUE)
      predictions[[k]] <- tibble(holdout = s, StimID = as.character(test$StimID),
                                 site = test[["站点名称"]], SubjID = as.character(test$SubjID),
                                 outcome = y, variant = v, observed = test[[y]],
                                 predicted = as.numeric(pr),
                                 terms = paste(variants[[v]], collapse = "+"),
                                 singular = isSingular(m), diagnostic = attr(m, "revision_warnings"))
      if (v == "adopted") {
        cs <- coef(summary(lmerTest::as_lmerModLmerTest(m)))
        coefs[[y]] <- t_ci(cs) %>% mutate(holdout = s, outcome = y, .before = 1)
      }
    }
  }
  list(pred = bind_rows(predictions), coeff = bind_rows(coefs), log = bind_rows(sel_logs))
}

folds <- levels(obs$StimID); res <- list()
for (start in seq(1, length(folds), by = 3)) {
  ix <- start:min(start + 2, length(folds))
  res <- c(res, parallel::mclapply(folds[ix], run_fold, mc.cores = 3))
  message("    recording folds complete: ", max(ix))
}
stopifnot(all(vapply(res, is.list, logical(1))))
pred <- bind_rows(lapply(res, `[[`, "pred"))
coef <- bind_rows(lapply(res, `[[`, "coeff"))
logs <- bind_rows(lapply(res, `[[`, "log"))
write_outcome(pred, file.path(OUT, "recording_cv_predictions.csv"))
write_outcome(coef, file.path(OUT, "adopted_deletion_coefficients.csv"))
write_outcome(logs, file.path(OUT, "fold_candidate_log.csv"))

# ---- Leave-one-site-out (source annotations still available; listeners recur)
sites <- unique(obs[["站点名称"]])
site_res <- parallel::mclapply(sites, function(site) {
  tr <- obs[obs[["站点名称"]] != site, ]; te <- obs[obs[["站点名称"]] == site, ]
  bind_rows(lapply(names(SPECS), function(y) bind_rows(lapply(c("adopted", "null"), function(v) {
    xs <- if (v == "adopted") SPECS[[y]] else character()
    m <- fit(y, xs, tr, TRUE)
    tibble(holdout = site, StimID = as.character(te$StimID), site = te[["站点名称"]],
           SubjID = as.character(te$SubjID), outcome = y, variant = paste0("site_", v),
           observed = te[[y]],
           predicted = as.numeric(predict(m, newdata = te, re.form = ~(1 | SubjID), allow.new.levels = TRUE)),
           terms = paste(xs, collapse = "+"), singular = isSingular(m),
           diagnostic = attr(m, "revision_warnings"))
  }))))
}, mc.cores = 3)
site_pred <- bind_rows(site_res)
write_outcome(site_pred, file.path(OUT, "site_cv_predictions.csv"))
pred <- bind_rows(pred, site_pred)

# ---- Summary metrics -----------------------------------------------------------
means <- pred %>%
  group_by(outcome, variant, StimID, site) %>%
  summarise(observed = mean(observed), predicted = mean(predicted), .groups = "drop")
metrics <- function(d) tibble(
  r = cor(d$observed, d$predicted),
  r_squared = cor(d$observed, d$predicted)^2,
  predictive_R2 = 1 - sum((d$observed - d$predicted)^2) / sum((d$observed - mean(d$observed))^2),
  rmse = sqrt(mean((d$observed - d$predicted)^2)))
sc <- means %>% group_by(outcome, variant) %>% group_modify(~metrics(.x)) %>% ungroup()
raw <- pred %>% group_by(outcome, variant) %>%
  summarise(rmse_individual = sqrt(mean((observed - predicted)^2)), .groups = "drop")
sc <- left_join(sc, raw, by = c("outcome", "variant"))
write_outcome(means, file.path(OUT, "cv_recording_means.csv"))
write_outcome(sc, file.path(OUT, "cv_summary.csv"))
print(sc, n = 30)

# The adopted scheme must reproduce A15 exactly.
old <- readr::read_csv(file.path("output", "a15_predictive_validity", "loso_summary.csv"),
                       show_col_types = FALSE)
verification <- sc %>% filter(variant == "adopted") %>% left_join(old, by = "outcome") %>%
  transmute(outcome, r_difference = r - r_stim, rmse_difference = rmse - rmse_stim)
write_outcome(verification, file.path(OUT, "baseline_verification.csv"))
stopifnot(max(abs(verification$r_difference)) < 1e-5, max(abs(verification$rmse_difference)) < 1e-5)

# ---- Conditional site resampling of the saved predictions ----------------------
set.seed(340918); boots <- list(); b <- 0
for (y in names(SPECS)) for (v in c("adopted", "strict_fold_reselected",
                                    "temporal_aic_fold_reselected", "site_adopted")) {
  d <- means %>% filter(outcome == y, variant == v); si <- unique(d$site)
  for (i in 1:1000) {
    b <- b + 1
    draw <- sample(si, length(si), replace = TRUE)
    dd <- bind_rows(lapply(draw, function(s) d[d$site == s, ]))
    boots[[b]] <- metrics(dd) %>% mutate(outcome = y, variant = v, replicate = i)
  }
}
br <- bind_rows(boots)
write_outcome(br, file.path(OUT, "conditional_site_bootstrap_replicates.csv"))
bs <- br %>%
  pivot_longer(c(r, r_squared, predictive_R2, rmse), names_to = "metric", values_to = "value") %>%
  group_by(outcome, variant, metric) %>%
  summarise(lo = quantile(value, .025, na.rm = TRUE), hi = quantile(value, .975, na.rm = TRUE), .groups = "drop")
write_outcome(bs, file.path(OUT, "conditional_site_bootstrap_ranges.csv"))

frequency <- pred %>%
  filter(grepl("fold_reselected", variant)) %>%
  distinct(outcome, variant, holdout, terms) %>%
  separate_longer_delim(terms, "+") %>%
  count(outcome, variant, terms, name = "folds") %>%
  mutate(percent = 100 * folds / 36)
write_outcome(frequency, file.path(OUT, "selection_frequency.csv"))

# ---- Recording-deletion summary of the adopted coefficients ---------------------
deletion <- coef %>%
  filter(term != "(Intercept)") %>%
  group_by(outcome, term) %>%
  summarise(n = n(), min_B = min(B), max_B = max(B), positive = sum(B > 0), negative = sum(B < 0),
            nominal_p_below_05 = sum(p < .05), max_p = max(p), .groups = "drop")
write_outcome(deletion, file.path(OUT, "deletion_coefficient_summary.csv"))
print(deletion, n = 20)

# ---- LA10-LA90 in every A12 grid specification that contains it ----------------
g <- readr::read_csv(file.path("output", "a12_exhaustive_selection", "grid_isoe.csv"),
                     show_col_types = FALSE) %>%
  mutate(delta_AIC = AIC - min(AIC)) %>%
  filter(grepl("LA10LA90", fixed, fixed = TRUE))
stopifnot(nrow(g) == 168)
rows <- parallel::mclapply(seq_len(nrow(g)), function(i) {
  xs <- strsplit(g$fixed[i], " + ", fixed = TRUE)[[1]]
  m <- fit_crossed("ISOE", xs, REML = TRUE); cs <- coef(summary(m))
  tibble(fixed = g$fixed[i], AIC = g$AIC[i], delta_AIC = g$delta_AIC[i],
         B = cs["LA10LA90", 1], SE = cs["LA10LA90", 2], df = cs["LA10LA90", 3],
         singular = isSingular(m),
         diagnostic = paste(unlist(m@optinfo$conv$lme4$messages), collapse = " | "))
}, mc.cores = 2)
tg <- bind_rows(rows)
write_outcome(tg, file.path(OUT, "temporal_grid_coefficients.csv"))
tgs <- bind_rows(lapply(c(Inf, 10, 4, 2), function(b) {
  d <- tg %>% filter(delta_AIC <= b)
  tibble(AIC_band = b, n = nrow(d), positive = sum(d$B > 0),
         min_B = if (nrow(d)) min(d$B) else NA_real_, max_B = if (nrow(d)) max(d$B) else NA_real_)
}))
write_outcome(tgs, file.path(OUT, "temporal_grid_summary.csv"))
print(tgs)
