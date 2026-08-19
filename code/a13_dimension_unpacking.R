# =============================================================================
# A13 — Dimension-level unpacking of the adopted models
# The paper models the two ISO rotation composites (ISOP/ISOE). This module
# fits each adopted driver set to the eight underlying ISO/TS 12913-2
# attributes, showing which attributes each driver moves — the composites'
# anatomy. Screen policy: BH-FDR within each specification's family
#   ISOP spec {LAeq, T50, Type2_SNS}          -> 3 x 8 = 24 tests
#   ISOE spec {LCLA, T50, AI50, LA10LA90, Type1_AFS} -> 5 x 8 = 40 tests
# Crossed random intercepts as everywhere; REML coefficients, Satterthwaite p.
# Caveat: attributes are single 1-5 Likert items (ordinal); the LMM treats them as
# interval, as elsewhere in the paper. Feeds Supplementary Table S4.
# =============================================================================

source(file.path("code", "helpers.R"))

OUT <- outdir("a13_dimension_unpacking")

SPECS <- list(
  ISOP_spec = c("LAeq", "T50", "Type2_SNS"),
  ISOE_spec = c("LCLA", "T50", "AI50", "LA10LA90", "Type1_AFS")
)

rows <- list()
for (spec_name in names(SPECS)) {
  fx <- SPECS[[spec_name]]
  for (dim in DIM8) {
    m <- fit_crossed(dim, fx, REML = TRUE)
    co <- as.data.frame(summary(m)$coefficients) %>%
      rownames_to_column("term") %>%
      filter(term != "(Intercept)")
    r2 <- r2_nak(m)
    # standardised betas: predictor SD over the 36 stimuli, outcome SD over obs
    sds <- sapply(fx, function(v) sd(stim36[[v]]))
    rows[[paste(spec_name, dim)]] <- co %>%
      transmute(spec = spec_name, dimension = dim, term,
                est = Estimate, se = `Std. Error`, df = df, t = `t value`,
                p = `Pr(>|t|)`,
                beta_std = Estimate * sds[term] / sd(obs[[dim]], na.rm = TRUE),
                R2m = r2["marginal"], R2c = r2["conditional"])
  }
}
res <- bind_rows(rows) %>%
  group_by(spec) %>%
  mutate(q = p.adjust(p, method = "BH"), family_n = n(),
         sig_fdr = q < 0.05) %>%
  ungroup() %>%
  arrange(spec, term, q)

write_outcome(res, file.path(OUT, "dimension_coefficients.csv"))

# Compact significance map for the report: term x dimension, cells = signed beta if q<.05
map_tab <- res %>%
  mutate(cell = ifelse(sig_fdr, sprintf("%+.2f", beta_std), "")) %>%
  select(spec, term, dimension, cell) %>%
  pivot_wider(names_from = dimension, values_from = cell)
write_outcome(map_tab, file.path(OUT, "significance_map.csv"))

n_sig <- res %>% group_by(spec) %>% summarise(k = sum(sig_fdr), n = n())
print(n_sig)
print(map_tab, width = Inf)
