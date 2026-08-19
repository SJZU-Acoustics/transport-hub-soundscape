# A05 — Variance decomposition of the crossed random-intercept null models for
# ISOP and ISOE (N = 2,116 ratings): listener, recording and residual shares.
# Feeds Fig. 2c and Supplementary Table S7.

source(file.path("code", "helpers.R"))
OUT <- outdir("a05_null_models")

null_table <- function(outcome) {
  m_ml <- fit_crossed(outcome, REML = FALSE)
  m_reml <- fit_crossed(outcome, REML = TRUE)
  vc <- var_components(m_ml)
  tot <- vc$variance[vc$component == "Total"]
  vc %>%
    mutate(outcome = outcome,
           variance = round(variance, 4),
           proportion_pct = round(100 * variance / tot, 2),
           AIC_ML = round(AIC(m_ml), 3),
           AIC_REML = round(AIC(m_reml), 3),
           logLik_ML = round(as.numeric(logLik(m_ml)), 4),
           minus2LL_ML = round(-2 * as.numeric(logLik(m_ml)), 3),
           n_par_ML = attr(logLik(m_ml), "df")) %>%
    select(outcome, everything())
}

t8 <- bind_rows(null_table("ISOP"), null_table("ISOE"))
write_outcome(t8, file.path(OUT, "table08_null_models.csv"))


cat("\n== Table 8 ==\n"); print(as.data.frame(t8))
