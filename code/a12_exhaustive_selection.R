# =============================================================================
# A12 — Exhaustive model selection within the four-layer framework.
# The main models are built by a single-pass ordered screen, which is path
# dependent. This module removes the path dependence:
#   1. Exhaustive grid over every one-indicator-per-layer combination
#      (energy 5+none x psycho 6+none x temporal 8+none x source 3+none
#       = 1,512 models per outcome), ML AIC.
#   2. Greedy closure from the grid winner: offer every remaining indicator,
#      add the best while AIC decreases; the full path is recorded.
# No p-based claims; selection uncertainty is reported as dAIC to the adopted
# models. Feeds the two grid-optimum rows of Supplementary Table S3.
# =============================================================================

source(file.path("code", "helpers.R"))
suppressPackageStartupMessages({ library(parallel); library(performance) })

OUT <- outdir("a12_exhaustive_selection")

# ML AIC of a crossed model, safe against non-convergence.
aic_of <- function(outcome, fixed) {
  tryCatch({
    m <- fit_crossed(outcome, fixed, REML = FALSE)
    msgs <- unlist(m@optinfo$conv$lme4$messages)
    tibble(fixed = paste(fixed, collapse = " + "),
           k = length(fixed), AIC = AIC(m),
           note = if (is.null(msgs)) "" else paste(msgs, collapse = "; "))
  }, error = function(e) tibble(fixed = paste(fixed, collapse = " + "),
                                k = length(fixed), AIC = NA_real_,
                                note = paste("ERROR:", conditionMessage(e))))
}

grid <- expand.grid(
  energy   = c("none", LAYER_ENERGY),
  psycho   = c("none", LAYER_PSYCHO),
  temporal = c("none", LAYER_TEMPORAL),
  source   = c("none", LAYER_SOURCE),
  stringsAsFactors = FALSE
)
combos <- lapply(seq_len(nrow(grid)), function(i) {
  v <- unlist(grid[i, ]); unname(v[v != "none"])
})
message(sprintf("[A12] grid size per outcome: %d", length(combos)))

run_grid <- function(outcome) {
  res <- mclapply(combos, function(fx) aic_of(outcome, fx), mc.cores = 6)
  bind_rows(res) %>% arrange(AIC)
}

t0 <- Sys.time()
grid_isop <- run_grid("ISOP")
grid_isoe <- run_grid("ISOE")
message(sprintf("[A12] grid done in %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
write_outcome(grid_isop, file.path(OUT, "grid_isop.csv"))
write_outcome(grid_isoe, file.path(OUT, "grid_isoe.csv"))

# Greedy closure from each grid winner.
closure <- function(outcome, base_fixed) {
  path <- list()
  current <- base_fixed
  m <- fit_crossed(outcome, current, REML = FALSE)
  cur_aic <- AIC(m)
  step <- 0
  repeat {
    step <- step + 1
    cands <- setdiff(ALL_ACOUSTIC, current)
    if (length(cands) == 0 || length(current) >= 9) break
    trial <- bind_rows(mclapply(cands, function(v) aic_of(outcome, c(current, v)), mc.cores = 6))
    best <- trial %>% filter(!is.na(AIC)) %>% slice_min(AIC, n = 1)
    path[[step]] <- tibble(step = step, offered = nrow(trial),
                           best_addition = setdiff(strsplit(best$fixed, " \\+ ")[[1]], current),
                           AIC_before = cur_aic, AIC_after = best$AIC,
                           dAIC = best$AIC - cur_aic)
    if (best$AIC >= cur_aic) break
    current <- strsplit(best$fixed, " \\+ ")[[1]]
    cur_aic <- best$AIC
  }
  list(path = bind_rows(path), final = current, final_aic = cur_aic)
}

base_isop <- strsplit(grid_isop$fixed[1], " \\+ ")[[1]]
base_isoe <- strsplit(grid_isoe$fixed[1], " \\+ ")[[1]]
cl_isop <- closure("ISOP", base_isop)
cl_isoe <- closure("ISOE", base_isoe)
write_outcome(bind_rows(mutate(cl_isop$path, outcome = "ISOP"),
                        mutate(cl_isoe$path, outcome = "ISOE")),
              file.path(OUT, "closure_path.csv"))

# Reference points: the adopted models (A11) on the same ML scale.
ADOPTED <- list(ISOP = c("LAeq", "T50", "Type2_SNS"),
                ISOE = c("LCLA", "T50", "AI50", "LA10LA90", "Type1_AFS"))
ref <- bind_rows(
  aic_of("ISOP", ADOPTED$ISOP) %>% mutate(outcome = "ISOP", model = "adopted (A11)"),
  aic_of("ISOE", ADOPTED$ISOE) %>% mutate(outcome = "ISOE", model = "adopted (A11)"),
  tibble(fixed = paste(base_isop, collapse = " + "), k = length(base_isop),
         AIC = grid_isop$AIC[1], note = "", outcome = "ISOP", model = "grid winner (1/layer)"),
  tibble(fixed = paste(base_isoe, collapse = " + "), k = length(base_isoe),
         AIC = grid_isoe$AIC[1], note = "", outcome = "ISOE", model = "grid winner (1/layer)"),
  tibble(fixed = paste(cl_isop$final, collapse = " + "), k = length(cl_isop$final),
         AIC = cl_isop$final_aic, note = "", outcome = "ISOP", model = "closed winner"),
  tibble(fixed = paste(cl_isoe$final, collapse = " + "), k = length(cl_isoe$final),
         AIC = cl_isoe$final_aic, note = "", outcome = "ISOE", model = "closed winner")
)
write_outcome(ref, file.path(OUT, "adopted_vs_winner.csv"))

# REML detail (coefficients, VIF, Nakagawa R2) for grid winners and closed winners.
detail <- function(outcome, fixed, tag) {
  m <- fit_crossed(outcome, fixed, REML = TRUE)
  co <- as.data.frame(summary(m)$coefficients) %>%
    rownames_to_column("term") %>%
    rename(est = Estimate, se = `Std. Error`, t = `t value`, p = `Pr(>|t|)`)
  vif <- tryCatch({
    cc <- performance::check_collinearity(m)
    tibble(term = cc$Term, VIF = cc$VIF)
  }, error = function(e) tibble(term = NA_character_, VIF = NA_real_))
  r2 <- r2_nak(m)
  co %>% left_join(vif, by = "term") %>%
    mutate(outcome = outcome, model = tag,
           R2m = r2["marginal"], R2c = r2["conditional"])
}
details <- bind_rows(
  detail("ISOP", base_isop, "grid winner"),
  detail("ISOE", base_isoe, "grid winner"),
  detail("ISOP", cl_isop$final, "closed winner"),
  detail("ISOE", cl_isoe$final, "closed winner"),
  detail("ISOP", ADOPTED$ISOP, "adopted (A11)"),
  detail("ISOE", ADOPTED$ISOE, "adopted (A11)")
)
write_outcome(details, file.path(OUT, "winner_details.csv"))

# Rows for the SI specification-sensitivity table, in the same schema as A11's
# si_sensitivity_specifications.csv, so the table is built from data throughout.
si_optima <- map_dfr(list(list("ISOP exhaustive-grid optimum", "ISOP", base_isop),
                          list("ISOE exhaustive-grid optimum", "ISOE", base_isoe)),
                     function(x) {
  m_re <- fit_crossed(x[[2]], x[[3]], REML = TRUE)
  s <- summary(m_re)$coefficients
  r <- r2_nak(fit_crossed(x[[2]], x[[3]], REML = FALSE))
  tibble(model = x[[1]], outcome = x[[2]], k_fixed = length(x[[3]]),
         terms = paste(x[[3]], collapse = " + "),
         AIC_ML = round(AIC(fit_crossed(x[[2]], x[[3]], REML = FALSE)), 3),
         marginal_R2_pct = round(100 * r[["marginal"]], 1),
         max_VIF = round(max(performance::check_collinearity(m_re)$VIF), 2),
         n_terms_ns = sum(s[-1, "Pr(>|t|)"] >= 0.05))
})
write_outcome(si_optima, file.path(OUT, "si_grid_optima_rows.csv"))

# How many grid models beat the adopted spec?
n_beat_isop <- sum(grid_isop$AIC < ref$AIC[ref$model == "adopted (A11)" & ref$outcome == "ISOP"], na.rm = TRUE)
n_beat_isoe <- sum(grid_isoe$AIC < ref$AIC[ref$model == "adopted (A11)" & ref$outcome == "ISOE"], na.rm = TRUE)
summary_txt <- c(
  sprintf("Grid models (per outcome): %d; failures ISOP %d, ISOE %d",
          length(combos), sum(is.na(grid_isop$AIC)), sum(is.na(grid_isoe$AIC))),
  sprintf("ISOP: adopted AIC %.3f | grid best %.3f (%s) | closed %.3f (%s) | grid models beating adopted: %d",
          ref$AIC[ref$model == "adopted (A11)" & ref$outcome == "ISOP"],
          grid_isop$AIC[1], grid_isop$fixed[1], cl_isop$final_aic,
          paste(cl_isop$final, collapse = " + "), n_beat_isop),
  sprintf("ISOE: adopted AIC %.3f | grid best %.3f (%s) | closed %.3f (%s) | grid models beating adopted: %d",
          ref$AIC[ref$model == "adopted (A11)" & ref$outcome == "ISOE"],
          grid_isoe$AIC[1], grid_isoe$fixed[1], cl_isoe$final_aic,
          paste(cl_isoe$final, collapse = " + "), n_beat_isoe)
)
writeLines(summary_txt, file.path(OUT, "summary.txt"))
message(paste(summary_txt, collapse = "\n"))
