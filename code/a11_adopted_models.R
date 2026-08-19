# A11 — The adopted specifications and everything reported about them.
# Both models keep one indicator per layer. ISOE's psychoacoustic and temporal
# representatives are re-screened on a base that excludes LAeq, because LAeq is
# not carried in the ISOE model. Produces the hierarchical build, the REML fixed
# effects, the variance components, the Nakagawa R-squared, the standardised
# coefficients, the random-structure and serial-position checks, and the
# specification-sensitivity set. Feeds Tables 2 and 3, Fig. 4, and Supplementary
# Tables S3, S9 and S10.

source(file.path("code", "helpers.R"))
OUT <- outdir("a11_adopted_models")

ISOP_MAIN <- c("LAeq", "T50", "Type2_SNS")

screen_step <- function(outcome, base, candidates, label) {
  m_base <- fit_crossed(outcome, base, REML = FALSE)
  map_dfr(candidates, function(v) {
    m <- fit_crossed(outcome, c(base, v), REML = FALSE)
    if (!(v %in% names(fixef(m)))) return(NULL)       # rank-deficient: skip
    lr <- anova(m_base, m)
    tibble(step = label, base = ifelse(length(base) == 0, "(null)",
                                       paste(base, collapse = " + ")),
           added = v, AIC = round(AIC(m), 3),
           dAIC = round(AIC(m) - AIC(m_base), 3),
           LRT_p = round(lr$`Pr(>Chisq)`[2], 5))
  }) %>% arrange(AIC)
}

# ---- Re-screen ISOE's layer representatives on the LAeq-free base -----------
e1 <- screen_step("ISOE", character(0), setdiff(LAYER_ENERGY, "LAeq"),
                  "1 energy (LAeq excluded)")
e2a <- screen_step("ISOE", "LCLA", LAYER_PSYCHO, "2a psychoacoustic (on LC-LA)")
e2b <- screen_step("ISOE", c("LCLA", "T50"), setdiff(LAYER_PSYCHO, "T50"),
                   "2b psychoacoustic (on LC-LA+T50)")
e3 <- screen_step("ISOE", c("LCLA", "T50", "AI50"), LAYER_TEMPORAL,
                  "3 temporal (on LC-LA+T50+AI50)")
e4 <- screen_step("ISOE", c("LCLA", "T50", "AI50", "LA10LA90"), LAYER_SOURCE,
                  "4 source")
rescreen <- bind_rows(e1, e2a, e2b, e3, e4)
write_outcome(rescreen, file.path(OUT, "isoe_rescreen_without_laeq.csv"))

ISOE_MAIN <- c("LCLA", "T50", "AI50", "LA10LA90", "Type1_AFS")

# ---- Table 2 upper block: ISOE hierarchy, LAeq-free -------------------------
isoe_seq <- list(
  list("Model 0", "Null model", character(0)),
  list("Model 1", "Sound energy exposure", c("LCLA")),
  list("Model 2", "Psychoacoustic", c("LCLA", "T50", "AI50")),
  list("Model 3", "Temporal dynamic", c("LCLA", "T50", "AI50", "LA10LA90")),
  list("Model 4", "Sound source semantic", ISOE_MAIN)
)
isop_seq <- list(
  list("Model 0", "Null model", character(0)),
  list("Model 1", "Sound energy exposure", c("LAeq")),
  list("Model 2", "Psychoacoustic", c("LAeq", "T50")),
  list("Model 3", "Sound source semantic", ISOP_MAIN)
)
hierarchy <- function(outcome, seq) {
  fits <- map(seq, ~ fit_crossed(outcome, .x[[3]], REML = FALSE))
  map_dfr(seq_along(seq), function(i) {
    aic <- AIC(fits[[i]])
    base <- tibble(outcome = outcome, Model = seq[[i]][[1]], Layer = seq[[i]][[2]],
                   Fixed = ifelse(length(seq[[i]][[3]]) == 0, "intercept only",
                                  paste(seq[[i]][[3]], collapse = " + ")),
                   AIC = round(aic, 3))
    if (i == 1) return(base %>% mutate(dAIC = NA_real_, LRT_chisq = NA_real_,
                                       LRT_df = NA_integer_, LRT_p = NA_real_))
    lr <- anova(fits[[i - 1]], fits[[i]])
    base %>% mutate(dAIC = round(aic - AIC(fits[[i - 1]]), 2),
                    LRT_chisq = round(lr$Chisq[2], 4), LRT_df = lr$Df[2],
                    LRT_p = round(lr$`Pr(>Chisq)`[2], 5))
  })
}
t9 <- hierarchy("ISOP", isop_seq)
t11 <- hierarchy("ISOE", isoe_seq)
write_outcome(t9, file.path(OUT, "table09_adopted_isop_hierarchy.csv"))
write_outcome(t11, file.path(OUT, "table11_adopted_isoe_hierarchy.csv"))
write_outcome(bind_rows(t9, t11) %>% select(outcome, Model, Layer, AIC),
              file.path(OUT, "fig07_adopted_aic.csv"))

# ---- Table 3 fixed effects (REML) -------------------------------------------
coef_table <- function(outcome, fixed) {
  m <- fit_crossed(outcome, fixed, REML = TRUE)
  s <- summary(m)$coefficients
  tibble(outcome = outcome, Term = rownames(s),
         B = round(s[, "Estimate"], 4), SE = round(s[, "Std. Error"], 4),
         df = round(s[, "df"], 2), t = round(s[, "t value"], 3),
         p = round(s[, "Pr(>|t|)"], 4))
}
t10 <- coef_table("ISOP", ISOP_MAIN)
t12 <- coef_table("ISOE", ISOE_MAIN)
write_outcome(t10, file.path(OUT, "table10_adopted_isop_fixed_reml.csv"))
write_outcome(t12, file.path(OUT, "table12_adopted_isoe_fixed_reml.csv"))

vifs <- map_dfr(list(list("ISOP main", "ISOP", ISOP_MAIN),
                     list("ISOE main (adopted)", "ISOE", ISOE_MAIN)), function(x) {
  as_tibble(performance::check_collinearity(fit_crossed(x[[2]], x[[3]], REML = TRUE))) %>%
    transmute(model = x[[1]], Term, VIF = round(VIF, 2))
})
write_outcome(vifs, file.path(OUT, "collinearity_vif_adopted.csv"))

# ---- Table 3 variance components, REML throughout ---------------------------
t13 <- map_dfr(list(list("ISOP", ISOP_MAIN), list("ISOE", ISOE_MAIN)), function(x) {
  b <- var_components(fit_crossed(x[[1]], character(0), REML = TRUE))
  f <- var_components(fit_crossed(x[[1]], x[[2]], REML = TRUE))
  tibble(outcome = x[[1]], Component = b$component,
         Baseline_REML = round(b$variance, 4), Final_REML = round(f$variance, 4),
         Reduction_pct = round(100 * (b$variance - f$variance) / b$variance, 1))
})
write_outcome(t13, file.path(OUT, "table13_adopted_reml_to_reml.csv"))

# ---- Nakagawa R2 for the adopted models -------------------------------------
r2 <- map_dfr(list(list("ISOP main", "ISOP", ISOP_MAIN),
                   list("ISOE main (adopted)", "ISOE", ISOE_MAIN),
                   list("ISOE with LAeq retained", "ISOE",
                        c("LAeq", "LCLA", "T50", "AI50", "LA10LA90", "Type1_AFS"))),
              function(x) {
  a <- r2_nak(fit_crossed(x[[2]], x[[3]], REML = FALSE))
  b <- r2_nak(fit_crossed(x[[2]], x[[3]], REML = TRUE))
  tibble(model = x[[1]], outcome = x[[2]], k_fixed = length(x[[3]]),
         marginal_pct = round(100 * a[["marginal"]], 1),
         conditional_pct = round(100 * a[["conditional"]], 1),
         marginal_REML_pct = round(100 * b[["marginal"]], 1),
         conditional_REML_pct = round(100 * b[["conditional"]], 1))
})
write_outcome(r2, file.path(OUT, "nakagawa_r2_adopted.csv"))

# ---- Supplementary Table S10: random structure and serial position ----------
obs_b <- obs %>% mutate(Batch = factor(Batch))
s1 <- map_dfr(list(list("ISOP", ISOP_MAIN), list("ISOE", ISOE_MAIN)), function(x) {
  m2 <- fit_crossed(x[[1]], x[[2]], data = obs_b, REML = FALSE)
  m3 <- fit_crossed(x[[1]], x[[2]], data = obs_b, REML = FALSE,
                    extra_random = "(1|Batch)")
  vc <- as.data.frame(VarCorr(m3))
  tibble(outcome = x[[1]], AIC_subj_stim = round(AIC(m2), 3),
         AIC_plus_batch = round(AIC(m3), 3),
         var_batch = round(vc$vcov[vc$grp == "Batch"], 6),
         singular = isSingular(m3, tol = 1e-5))
})
write_outcome(s1, file.path(OUT, "tableS1_adopted_random_structure.csv"))

ORDER_VARS <- c("BlockOrder", "BlockPos", "GlobalOrder")
s2 <- map_dfr(list(list("ISOP", ISOP_MAIN), list("ISOE", ISOE_MAIN)), function(x) {
  base <- fit_crossed(x[[1]], x[[2]], REML = FALSE)
  rows <- map_dfr(ORDER_VARS, function(v) {
    m <- fit_crossed(x[[1]], c(x[[2]], v), REML = FALSE)
    s <- summary(m)$coefficients[v, ]
    tibble(outcome = x[[1]], added = v, AIC = round(AIC(m), 3),
           dAIC = round(AIC(m) - AIC(base), 3),
           beta = round(s[["Estimate"]], 5), p = round(s[["Pr(>|t|)"]], 4))
  })
  mj <- fit_crossed(x[[1]], c(x[[2]], ORDER_VARS), REML = FALSE)
  lrj <- anova(base, mj)
  bind_rows(tibble(outcome = x[[1]], added = "(none: main model)",
                   AIC = round(AIC(base), 3), dAIC = NA_real_,
                   beta = NA_real_, p = NA_real_),
            rows,
            tibble(outcome = x[[1]], added = "all three jointly",
                   AIC = round(AIC(mj), 3), dAIC = round(AIC(mj) - AIC(base), 3),
                   beta = NA_real_, p = round(lrj$`Pr(>Chisq)`[2], 4)))
})
write_outcome(s2, file.path(OUT, "tableS2_adopted_order_sensitivity.csv"))

# ---- SI sensitivity table: the screen closure, both outcomes ----------------
si_sens <- map_dfr(list(
  list("ISOP main (one per layer)", "ISOP", ISOP_MAIN),
  list("ISOP screen closed", "ISOP", c("LAeq", "T50", "Type2_SNS", "F50", "S50")),
  list("ISOP screen closed, restarted from null", "ISOP",
       c("T50", "AI50", "Type2_SNS", "F50")),
  list("ISOE main (one per layer, adopted)", "ISOE", ISOE_MAIN),
  list("ISOE screen closed", "ISOE", c(ISOE_MAIN, "R50", "S50")),
  list("ISOE with LAeq retained", "ISOE",
       c("LAeq", "LCLA", "T50", "AI50", "LA10LA90", "Type1_AFS")),
  list("ISOE with S50 as 2nd psychoacoustic representative", "ISOE",
       c("LCLA", "T50", "S50", "LA10LA90", "Type1_AFS")),
  # Post-AI-review sensitivity (2026-08-18): the adopted ISOE model refitted
  # without the temporal term, so its retention is evidenced, not narrated.
  list("ISOE without the temporal term", "ISOE",
       c("LCLA", "T50", "AI50", "Type1_AFS"))
), function(x) {
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
write_outcome(si_sens, file.path(OUT, "si_sensitivity_specifications.csv"))

# Collinearity map for the SI.
cmap <- expand_grid(v1 = c("LAeq", "LCeq", "LCLA", "LA50", "LC50"),
                    v2 = c("N50", "S50", "R50", "F50", "T50", "AI50")) %>%
  mutate(r = round(map2_dbl(v1, v2, ~ cor(stim36[[.x]], stim36[[.y]])), 3))
write_outcome(cmap, file.path(OUT, "si_energy_psychoacoustic_collinearity.csv"))

# ---- H1 consistency check ---------------------------------------------------
h1 <- tibble(
  test = c("bivariate r(LAeq, ISOP)", "bivariate r(LAeq, ISOE)",
           "ANOVA SPL group -> ISOP", "ANOVA SPL group -> ISOE",
           "LAeq in the adopted ISOP model", "LAeq offered to the adopted ISOE model"),
  statistic = c(
    sprintf("r = %.3f", cor(stim36$LAeq, stim36$ISOP)),
    sprintf("r = %.3f", cor(stim36$LAeq, stim36$ISOE)),
    sprintf("F = %.3f", anova(aov(ISOP ~ SPL_Group, stim36))$`F value`[1]),
    sprintf("F = %.3f", anova(aov(ISOE ~ SPL_Group, stim36))$`F value`[1]),
    sprintf("B = %.4f", summary(fit_crossed("ISOP", ISOP_MAIN, REML = TRUE))$coefficients["LAeq", "Estimate"]),
    sprintf("dAIC = %+.3f", AIC(fit_crossed("ISOE", c(ISOE_MAIN, "LAeq"))) -
              AIC(fit_crossed("ISOE", ISOE_MAIN)))),
  p = c(cor.test(stim36$LAeq, stim36$ISOP)$p.value,
        cor.test(stim36$LAeq, stim36$ISOE)$p.value,
        anova(aov(ISOP ~ SPL_Group, stim36))$`Pr(>F)`[1],
        anova(aov(ISOE ~ SPL_Group, stim36))$`Pr(>F)`[1],
        summary(fit_crossed("ISOP", ISOP_MAIN, REML = TRUE))$coefficients["LAeq", "Pr(>|t|)"],
        anova(fit_crossed("ISOE", ISOE_MAIN), fit_crossed("ISOE", c(ISOE_MAIN, "LAeq")))$`Pr(>Chisq)`[2])
) %>% mutate(p = round(p, 5), significant = p < 0.05)
write_outcome(h1, file.path(OUT, "h1_consistency_check.csv"))

# ---- Standardised fixed effects with Wald CIs (Fig. 4) ----------------------
obs_z <- obs %>% mutate(across(all_of(ALL_ACOUSTIC), ~ as.numeric(scale(.x))),
                        ISOP = as.numeric(scale(ISOP)), ISOE = as.numeric(scale(ISOE)))
std_ci <- function(outcome, fixed) {
  m <- fit_crossed(outcome, fixed, data = obs_z, REML = TRUE)
  s <- summary(m)$coefficients
  ci <- confint(m, method = "Wald", parm = rownames(s))
  tibble(outcome = outcome, Term = rownames(s), beta_std = round(s[, "Estimate"], 4),
         CI_low = round(ci[, 1], 4), CI_high = round(ci[, 2], 4),
         p = round(s[, "Pr(>|t|)"], 4)) %>% filter(Term != "(Intercept)")
}
f8 <- bind_rows(std_ci("ISOP", ISOP_MAIN), std_ci("ISOE", ISOE_MAIN))
write_outcome(f8, file.path(OUT, "fig08_adopted_standardised_betas.csv"))

cat("\n== ISOE re-screen without LAeq (top 3 per step) ==\n")
print(as.data.frame(rescreen %>% group_by(step) %>% slice_head(n = 3) %>% ungroup()))
cat("\n== Adopted Table 11 (ISOE hierarchy) ==\n"); print(as.data.frame(t11))
cat("\n== Adopted Table 9 (ISOP hierarchy) ==\n"); print(as.data.frame(t9))
cat("\n== Adopted Table 12 (ISOE, REML) ==\n"); print(as.data.frame(t12))
cat("\n== Adopted Table 10 (ISOP, REML) ==\n"); print(as.data.frame(t10))
cat("\n== VIF ==\n"); print(as.data.frame(vifs))
cat("\n== Adopted Table 13 ==\n"); print(as.data.frame(t13))
cat("\n== Nakagawa R2 ==\n"); print(as.data.frame(r2))
cat("\n== Table S1 ==\n"); print(as.data.frame(s1))
cat("\n== Table S2 ==\n"); print(as.data.frame(s2))
cat("\n== SI sensitivity specifications ==\n"); print(as.data.frame(si_sens))
cat("\n== H1 consistency ==\n"); print(as.data.frame(h1))
