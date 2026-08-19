# A04 — Pearson correlations between the 22 acoustic and source indicators and
# ISOP / ISOE at recording level (N = 36), plus the full indicator correlation
# matrix and the within-layer collinearity summary. Feeds Fig. 3.

source(file.path("code", "helpers.R"))
OUT <- outdir("a04_correlations")

layer_of <- function(v) {
  case_when(v %in% LAYER_ENERGY ~ "Sound energy exposure layer",
            v %in% LAYER_PSYCHO ~ "Psychoacoustic layer",
            v %in% LAYER_TEMPORAL ~ "Temporal dynamic layer",
            v %in% LAYER_SOURCE ~ "Sound source semantic layer")
}

t7 <- map_dfr(ALL_ACOUSTIC, function(v) {
  cp <- cor.test(stim36[[v]], stim36$ISOP)
  ce <- cor.test(stim36[[v]], stim36$ISOE)
  tibble(Layer = layer_of(v), Variable = VAR_LABEL[[v]], var = v,
         ISOP_r = round(unname(cp$estimate), 3), ISOP_p = round(cp$p.value, 4),
         ISOP_sig = stars(cp$p.value),
         ISOE_r = round(unname(ce$estimate), 3), ISOE_p = round(ce$p.value, 4),
         ISOE_sig = stars(ce$p.value))
})
write_outcome(t7, file.path(OUT, "table07_correlations.csv"))


# ---- Full 24 x 24 indicator correlation matrix ------------------------------
mat_vars <- c(ALL_ACOUSTIC, "ISOP", "ISOE")
M <- cor(stim36[, mat_vars])
write_outcome(as_tibble(round(M, 4), rownames = "variable"),
              file.path(OUT, "fig06_correlation_matrix.csv"))

# ---- Within-layer collinearity ----------------------------------------------
coll <- map_dfr(list(Energy = LAYER_ENERGY, Psychoacoustic = LAYER_PSYCHO,
                     Temporal = LAYER_TEMPORAL, Source = LAYER_SOURCE),
                function(vs) {
                  m <- cor(stim36[, vs]); diag(m) <- NA
                  tibble(max_abs_r = round(max(abs(m), na.rm = TRUE), 3),
                         median_abs_r = round(median(abs(m), na.rm = TRUE), 3))
                }, .id = "Layer")
write_outcome(coll, file.path(OUT, "within_layer_collinearity.csv"))

cat("\n== Within-layer collinearity ==\n"); print(as.data.frame(coll))
