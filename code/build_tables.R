# =============================================================================
# LaTeX table fragments: Tables 1-3 and Supplementary Tables S1-S15.
# Each fragment is a bare tabular (booktabs); captions and table environments
# live in the manuscript source.
# Inputs: the deposited data tables + the analysis modules' result tables.
# =============================================================================

source(file.path("code", "helpers.R"))

TAB <- file.path("output", "tables")
dir.create(TAB, showWarnings = FALSE, recursive = TRUE)
OUTC <- function(a, f) readr::read_csv(
  file.path("output", a, f), show_col_types = FALSE)

wtab <- function(lines, name) {
  lines <- gsub("\\bISOP\\b", "\\\\textit{ISO-P}", lines, perl = TRUE)
  lines <- gsub("\\bISOE\\b", "\\\\textit{ISO-E}", lines, perl = TRUE)
  writeLines(lines, file.path(TAB, name))
  cat("wrote", name, "\n")
}
fp <- function(p) ifelse(p < 0.001, "$<$0.001", sprintf("%.3f", p))
fq <- fp
esc <- function(x) gsub("%", "\\\\%", x)
pm <- function(x) {
  x <- gsub("±", "$\\\\pm$", x)
  gsub("(?<=[ &])-(?=[0-9])", "$-$", x, perl = TRUE)
}
fnum <- function(fmt, x) sub("^-", "$-$", sprintf(fmt, x))
# Manuscript notation: ISO-P / ISO-E, set italic as variable names.
ISO_DASH <- function(x) gsub("\\bISOP\\b", "\\\\textit{ISO-P}",
                             gsub("\\bISOE\\b", "\\\\textit{ISO-E}", x))
wtab_raw <- NULL

TERM_TEX <- c(
  LAeq = "$L_{\\mathrm{Aeq}}$", LCeq = "$L_{\\mathrm{Ceq}}$",
  LCLA = "$L_{\\mathrm{Ceq}}-L_{\\mathrm{Aeq}}$",
  LA50 = "$L_{\\mathrm{A50}}$", LC50 = "$L_{\\mathrm{C50}}$",
  N50 = "$N_{50}$", S50 = "$S_{50}$", R50 = "$R_{50}$", F50 = "$F_{50}$",
  T50 = "$T_{50}$", AI50 = "$\\mathrm{AI}_{50}$",
  LA10LA90 = "$L_{\\mathrm{A10}}-L_{\\mathrm{A90}}$",
  LC10LC90 = "$L_{\\mathrm{C10}}-L_{\\mathrm{C90}}$",
  N10N90 = "$N_{10}-N_{90}$", S10S90 = "$S_{10}-S_{90}$",
  R10R90 = "$R_{10}-R_{90}$", F10F90 = "$F_{10}-F_{90}$",
  T10T90 = "$T_{10}-T_{90}$", AI10AI90 = "$\\mathrm{AI}_{10}-\\mathrm{AI}_{90}$",
  Type1_AFS = "Type 1 functional-sound share",
  Type2_SNS = "Type 2 natural-and-music share",
  Type3_DCS = "Type 3 communication-sound share",
  `(Intercept)` = "Intercept")
UNIT <- c(LAeq = "dB(A)", LCeq = "dB(C)", LCLA = "dB", LA50 = "dB(A)",
          LC50 = "dB(C)", N50 = "sone", S50 = "acum", R50 = "asper",
          F50 = "vacil", T50 = "tu", AI50 = "\\%",
          LA10LA90 = "dB(A)", LC10LC90 = "dB(C)", N10N90 = "sone",
          S10S90 = "acum", R10R90 = "asper", F10F90 = "vacil",
          T10T90 = "tu", AI10AI90 = "\\%",
          Type1_AFS = "\\%", Type2_SNS = "\\%", Type3_DCS = "\\%")

# =============================================================================
# Table 1 (main) — descriptives, overall + level ranks
# =============================================================================
t1e <- OUTC("a01_descriptives", "table01_energy_layer.csv")
t1p <- OUTC("a01_descriptives", "table02_psychoacoustic_layer.csv")
t1t <- OUTC("a01_descriptives", "table03_temporal_layer.csv")
t1s <- OUTC("a01_descriptives", "table04_sources_and_perception.csv")

grab <- function(df, var) {
  df %>% filter(Group %in% c("Total", "HSPL", "MSPL", "LSPL")) %>%
    select(Group, all_of(var)) %>%
    tidyr::pivot_wider(names_from = Group, values_from = all_of(var))
}
row1 <- function(df, var, key) {
  g <- grab(df, var)
  pm(sprintf("%s (%s) & %s & %s & %s & %s \\\\", TERM_TEX[key], UNIT[key],
             g$Total, g$HSPL, g$MSPL, g$LSPL))
}
r_isop <- grab(t1s, "ISOP_stimlevel"); r_isoe <- grab(t1s, "ISOE_stimlevel")
tab1 <- c(
  "\\begin{tabular}{lcccc}", "\\toprule",
  "Indicator & Overall & HSPL & MSPL & LSPL \\\\", "\\midrule",
  "\\multicolumn{5}{l}{\\emph{Sound energy exposure layer}} \\\\",
  sapply(c("LAeq","LCeq","LCLA","LA50","LC50"), function(v) row1(t1e, v, v)),
  "\\addlinespace",
  "\\multicolumn{5}{l}{\\emph{Psychoacoustic layer}} \\\\",
  sapply(c("N50","S50","R50","F50","T50","AI50"), function(v) row1(t1p, v, v)),
  "\\addlinespace",
  "\\multicolumn{5}{l}{\\emph{Temporal dynamic layer}} \\\\",
  sapply(c("LA10LA90","LC10LC90","N10N90"), function(v) row1(t1t, v, v)),
  "\\addlinespace",
  "\\multicolumn{5}{l}{\\emph{Sound-source semantic layer}} \\\\",
  sapply(c("Type1_AFS","Type2_SNS","Type3_DCS"), function(v) row1(t1s, v, v)),
  "\\addlinespace",
  "\\multicolumn{5}{l}{\\emph{Perception (recording-level means)}} \\\\",
  pm(sprintf("ISOP & %s & %s & %s & %s \\\\", r_isop$Total, r_isop$HSPL,
             r_isop$MSPL, r_isop$LSPL)),
  pm(sprintf("ISOE & %s & %s & %s & %s \\\\", r_isoe$Total, r_isoe$HSPL,
             r_isoe$MSPL, r_isoe$LSPL)),
  "\\bottomrule", "\\end{tabular}")
wtab(tab1, "tab1_descriptives.tex")

# =============================================================================
# Table 2 (main) — hierarchical model build (ML), both outcomes
# =============================================================================
b_isop <- OUTC("a11_adopted_models", "table09_adopted_isop_hierarchy.csv")
b_isoe <- OUTC("a11_adopted_models", "table11_adopted_isoe_hierarchy.csv")
fx_tex <- function(s) {
  out <- s
  out <- gsub("intercept only", "intercept", out)
  for (k in names(TERM_TEX)) {
    repl <- gsub("\\\\", "\\\\\\\\", TERM_TEX[k])
    out <- gsub(paste0("\\b", k, "\\b"), repl, out, perl = TRUE)
  }
  gsub("\\+", "$+$", out)
}
brow <- function(d) {
  apply(d, 1, function(r) {
    aic <- sprintf("%.2f", as.numeric(r["AIC"]))
    da <- ifelse(is.na(r["dAIC"]), "--", fnum("%.2f", as.numeric(r["dAIC"])))
    ch <- ifelse(is.na(r["LRT_chisq"]), "--",
                 sprintf("%.2f", as.numeric(r["LRT_chisq"])))
    df <- ifelse(is.na(r["LRT_df"]), "--", as.character(as.integer(r["LRT_df"])))
    p <- ifelse(is.na(r["LRT_p"]), "--", fp(as.numeric(r["LRT_p"])))
    sprintf("%s & %s & %s & %s & %s & %s & %s \\\\",
            sub("Model ", "", r["Model"]), r["Layer"], aic, da, ch, df, p)
  })
}
tab2 <- c(
  "\\begin{tabular}{llccccc}", "\\toprule",
  "Step & Layer added & AIC & $\\Delta$AIC & $\\chi^2$ & df & $p$ \\\\",
  "\\midrule",
  "\\multicolumn{7}{l}{\\emph{ISOP}} \\\\",
  brow(b_isop),
  "\\addlinespace",
  "\\multicolumn{7}{l}{\\emph{ISOE}} \\\\",
  brow(b_isoe),
  "\\bottomrule", "\\end{tabular}")
wtab(tab2, "tab2_build.tex")

# =============================================================================
# Table 3 (main) — final models: REML fixed effects, variance components, R2
# =============================================================================
f_isop <- OUTC("a11_adopted_models", "table10_adopted_isop_fixed_reml.csv")
f_isoe <- OUTC("a11_adopted_models", "table12_adopted_isoe_fixed_reml.csv")
vc <- OUTC("a11_adopted_models", "table13_adopted_reml_to_reml.csv")
r2a <- OUTC("a11_adopted_models", "nakagawa_r2_adopted.csv")
# Revision 1: Satterthwaite t intervals of the adopted REML coefficients (A19).
fci <- OUTC("a19_uncertainty", "fixed_effect_intervals.csv")
ci_tex <- function(lo, hi, fmt = "%.4f")
  sprintf("[%s, %s]", fnum(fmt, lo), fnum(fmt, hi))
r2row <- function(oc) {
  d <- r2a %>% filter(model == ifelse(oc == "ISOP", "ISOP main", "ISOE main (adopted)"))
  sprintf("Nakagawa $R^2$ (marginal / conditional) & \\multicolumn{6}{l}{%.1f\\%% / %.1f\\%%} \\\\",
          d$marginal_pct[1], d$conditional_pct[1])
}
frow <- function(d, oc) {
  apply(d, 1, function(r) {
    ci <- fci %>% filter(outcome == oc, term == r[["Term"]])
    # the A11 table stores B rounded to four decimals
    stopifnot(nrow(ci) == 1, abs(ci$B - as.numeric(r["B"])) < 6e-5)
    sprintf("%s & %s & %.4f & %s & %.1f & %s & %s \\\\",
            TERM_TEX[r[["Term"]]], fnum("%.4f", as.numeric(r["B"])),
            as.numeric(r["SE"]), ci_tex(ci$lo, ci$hi), as.numeric(r["df"]),
            fnum("%.2f", as.numeric(r["t"])), fp(as.numeric(r["p"])))
  })
}
vrow <- function(oc) {
  d <- vc %>% filter(outcome == oc, Component != "Total")
  apply(d, 1, function(r) {
    red <- as.numeric(r["Reduction_pct"])
    chg <- if (abs(red) < 0.15) "0\\%" else if (red > 0)
      sprintf("$-$%.1f\\%%", red) else sprintf("$+$%.1f\\%%", -red)
    sprintf("%s variance & %.4f & %.4f & \\multicolumn{2}{c}{%s} & & \\\\",
            recode(r[["Component"]], Subject = "Listener",
                   Stimulus = "Recording", Residual = "Residual"),
            as.numeric(r["Baseline_REML"]), as.numeric(r["Final_REML"]), chg)
  })
}
tab3 <- c(
  "\\begin{tabular}{lcccccc}", "\\toprule",
  "Term & $B$ & SE & 95\\% CI & df & $t$ & $p$ \\\\",
  "\\midrule",
  "\\multicolumn{7}{l}{\\emph{ISOP: $L_{\\mathrm{Aeq}}$ $+$ $T_{50}$ $+$ Type 2 natural-and-music share}} \\\\",
  frow(f_isop, "ISOP"),
  "\\addlinespace",
  "\\multicolumn{7}{l}{\\emph{ISOE: $L_{\\mathrm{Ceq}}-L_{\\mathrm{Aeq}}$ $+$ $T_{50}$ $+$ $\\mathrm{AI}_{50}$ $+$ $L_{\\mathrm{A10}}-L_{\\mathrm{A90}}$ $+$ Type 1 share}} \\\\",
  frow(f_isoe, "ISOE"),
  "\\midrule",
  " & Null & Final & \\multicolumn{2}{c}{Change} & & \\\\",
  "\\multicolumn{7}{l}{\\emph{ISOP random-effect variances (REML)}} \\\\",
  vrow("ISOP"),
  r2row("ISOP"),
  "\\addlinespace",
  "\\multicolumn{7}{l}{\\emph{ISOE random-effect variances (REML)}} \\\\",
  vrow("ISOE"),
  r2row("ISOE"),
  "\\bottomrule", "\\end{tabular}")
wtab(tab3, "tab3_models.tex")

# =============================================================================
# SI tables
# =============================================================================
# --- S: descriptives by functional type (19 indicators x 6 types) ------------
byty <- function(df, var, key) {
  g <- df %>% filter(Group %in% FUNC_LEVELS) %>%
    select(Group, all_of(var)) %>%
    tidyr::pivot_wider(names_from = Group, values_from = all_of(var))
  pm(sprintf("%s (%s) & %s \\\\", TERM_TEX[key], UNIT[key],
             paste(g[1, FUNC_LEVELS], collapse = " & ")))
}
si1 <- c(
  "\\begin{tabular}{lcccccc}", "\\toprule",
  paste("Indicator &", paste(FUNC_LEVELS, collapse = " & "), "\\\\"),
  "\\midrule",
  "\\multicolumn{7}{l}{\\emph{Sound energy exposure layer}} \\\\",
  sapply(c("LAeq","LCeq","LCLA","LA50","LC50"), function(v) byty(t1e, v, v)),
  "\\addlinespace",
  "\\multicolumn{7}{l}{\\emph{Psychoacoustic layer}} \\\\",
  sapply(c("N50","S50","R50","F50","T50","AI50"), function(v) byty(t1p, v, v)),
  "\\addlinespace",
  "\\multicolumn{7}{l}{\\emph{Temporal dynamic layer}} \\\\",
  sapply(c("LA10LA90","LC10LC90","N10N90","S10S90","R10R90","F10F90",
           "T10T90","AI10AI90"), function(v) byty(t1t, v, v)),
  "\\addlinespace",
  "\\multicolumn{7}{l}{\\emph{Sound-source semantic layer and perception}} \\\\",
  sapply(c("Type1_AFS","Type2_SNS","Type3_DCS"), function(v) byty(t1s, v, v)),
  {g <- t1s %>% filter(Group %in% FUNC_LEVELS);
   c(pm(sprintf("ISOP & %s \\\\", paste(g$ISOP_stimlevel, collapse = " & "))),
     pm(sprintf("ISOE & %s \\\\", paste(g$ISOE_stimlevel, collapse = " & "))))},
  "\\bottomrule", "\\end{tabular}")
wtab(si1, "si_desc_by_type.tex")

# --- S: nine-class source shares by functional type --------------------------
s9 <- OUTC("a01_descriptives", "fig03_source_shares_by_function.csv")
SRC9 <- c("交通工具声" = "Vehicle sounds (1)",
          "设备设施声" = "Equipment and facility sounds (2)",
          "公共广播声" = "Public-address announcements (3)",
          "广告影视声" = "Advertising and video sounds (6)",
          "背景音乐声" = "Background music (4)",
          "自然景观声" = "Natural landscape sounds (5)",
          "语音声" = "Speech (7)",
          "行为声" = "Activity sounds (8)",
          "手机声" = "Mobile-phone sounds (9)")
TYPE_OF <- c("Type 1", "Type 1", "Type 1", "Type 1", "Type 2", "Type 2",
             "Type 3", "Type 3", "Type 3")
si2 <- c(
  "\\begin{tabular}{llcccccc}", "\\toprule",
  paste("Source class & Aggregated type &",
        paste(FUNC_LEVELS, collapse = " & "), "\\\\"),
  "\\midrule",
  unlist(lapply(seq_along(SRC9), function(i) {
    zh <- names(SRC9)[i]
    sprintf("%s & %s & %s \\\\", SRC9[i], TYPE_OF[i],
            paste(sprintf("%.2f", s9[[zh]][match(FUNC_LEVELS, s9$FuncCode)]),
                  collapse = " & "))
  })),
  "\\midrule",
  sapply(c("Type1_AFS", "Type2_SNS", "Type3_DCS"), function(v) {
    sprintf("%s & & %s \\\\", TERM_TEX[v],
            paste(sprintf("%.2f", s9[[v]][match(FUNC_LEVELS, s9$FuncCode)]),
                  collapse = " & "))
  }),
  "\\bottomrule", "\\end{tabular}")
wtab(si2, "si_sources9.tex")

# --- S: reliability -----------------------------------------------------------
rel <- OUTC("a02_reliability", "table05_reliability_full.csv")
relT <- OUTC("a02_reliability", "table05_transposed_true_interrater.csv")
si3 <- c(
  "\\begin{tabular}{lcccc}", "\\toprule",
  "Attribute & Cronbach's $\\alpha$ & ICC(2,1) & ICC(2,57) single & ICC(2,57) average \\\\",
  "\\midrule",
  sapply(seq_len(nrow(rel)), function(i) {
    sprintf("%s & %.3f & %.3f & %.3f & %.3f \\\\", rel$Dimension[i],
            rel$Cronbach_alpha[i], rel$ICC2_single[i],
            relT$ICC2_single_T[i], relT$ICC2k_average_T[i])
  }),
  "\\bottomrule", "\\end{tabular}")
wtab(si3, "si_reliability.tex")

# --- S: ANOVA + post hoc ------------------------------------------------------
an <- OUTC("a03_anova", "table06_anova.csv")
ph <- OUTC("a03_anova", "posthoc_spl.csv")
anrow <- function(dep, grp) {
  d <- an %>% filter(Dependent == dep, Grouping == grp, Source == "Between groups")
  w <- an %>% filter(Dependent == dep, Grouping == grp, Source == "Within groups")
  sprintf("%s & %s & %.3f & %d, %d & %.3f & %.3f & %s \\\\",
          dep, grp, d$SS, as.integer(d$df), as.integer(w$df), d$MS, d$F, fp(d$p))
}
si4 <- c(
  "\\begin{tabular}{llccccc}", "\\toprule",
  "Outcome & Factor & SS$_{\\mathrm{between}}$ & df & MS & $F$ & $p$ \\\\",
  "\\midrule",
  anrow("ISOP", "Sound pressure level"), anrow("ISOP", "Function type"),
  anrow("ISOE", "Sound pressure level"), anrow("ISOE", "Function type"),
  "\\midrule",
  "\\multicolumn{7}{l}{\\emph{Post-hoc contrasts on the level factor, ISOP (Tukey HSD)}} \\\\",
  {d <- ph %>% filter(Dependent == "ISOP", method == "Tukey HSD");
   sapply(seq_len(nrow(d)), function(i)
     sprintf("\\multicolumn{2}{l}{%s} & \\multicolumn{3}{c}{difference $=$ %.3f} & \\multicolumn{2}{c}{$p_{\\mathrm{adj}}$ %s} \\\\",
             gsub("-", "--", d$contrast[i], fixed = TRUE), d$diff[i],
             ifelse(d$p_adj[i] < 0.001, "$<$ 0.001",
                    paste0("$=$ ", sprintf("%.3f", d$p_adj[i])))))},
  "\\bottomrule", "\\end{tabular}")
wtab(si4, "si_anova.tex")

# --- S: null models -----------------------------------------------------------
nm <- OUTC("a05_null_models", "table08_null_models.csv")
# Revision 1: parametric-bootstrap percentile intervals of the shares (A19).
vsi <- OUTC("a19_uncertainty", "variance_share_intervals.csv")
si5 <- c(
  "\\begin{tabular}{lccccc}", "\\toprule",
  "Outcome & Component & Variance & Share (\\%) & 95\\% CI of share (\\%) & AIC (ML) \\\\",
  "\\midrule",
  apply(nm, 1, function(r) {
    v <- vsi %>% filter(outcome == r[["outcome"]], component == r[["component"]])
    stopifnot(nrow(v) <= 1)
    if (nrow(v) == 1) stopifnot(abs(v$estimate - as.numeric(r["proportion_pct"])) < 0.05)
    sprintf("%s & %s & %.4f & %.1f & %s & %s \\\\",
            r[["outcome"]],
            recode(r[["component"]], Subject = "Listener",
                   Stimulus = "Recording", Residual = "Residual",
                   Total = "Total"),
            as.numeric(r["variance"]), as.numeric(r["proportion_pct"]),
            if (nrow(v) == 1) sprintf("[%.1f, %.1f]", v$lo, v$hi) else "--",
            sprintf("%.3f", as.numeric(r["AIC_ML"])))
  }),
  "\\bottomrule", "\\end{tabular}")
wtab(si5, "si_null.tex")

# --- S: sensitivity specifications (A10/A11 + A12 exhaustive optima) ----------
sens <- OUTC("a11_adopted_models", "si_sensitivity_specifications.csv")
opt <- OUTC("a12_exhaustive_selection", "si_grid_optima_rows.csv")
fx_short <- function(s) fx_tex(s)
mod_lab <- function(m) {
  m <- sub("ISOE main (one per layer, adopted)", "ISOE main (adopted)", m, fixed = TRUE)
  gsub("\\b2nd\\b", "second", fx_tex(m), perl = TRUE)
}
sensrow <- function(model, k, terms, aic, r2, vif, ns) {
  sprintf("%s & %d & %s & %.3f & %.1f\\%% & %.2f & %d \\\\",
          mod_lab(model), k, fx_short(terms), aic, r2, vif, ns)
}
si6 <- c(
  "\\begin{tabular}{p{4.1cm}cp{5.2cm}cccc}", "\\toprule",
  "Specification & $k$ & Fixed effects & AIC (ML) & Marginal $R^2$ & max VIF & n.s.\\ terms \\\\",
  "\\midrule",
  apply(sens, 1, function(r)
    sensrow(r[["model"]], as.integer(r[["k_fixed"]]), r[["terms"]],
            as.numeric(r[["AIC_ML"]]), as.numeric(r[["marginal_R2_pct"]]),
            as.numeric(r[["max_VIF"]]), as.integer(r[["n_terms_ns"]]))),
  "\\midrule",
  apply(opt, 1, function(r)
    sensrow(r[["model"]], as.integer(r[["k_fixed"]]), r[["terms"]],
            as.numeric(r[["AIC_ML"]]), as.numeric(r[["marginal_R2_pct"]]),
            as.numeric(r[["max_VIF"]]), as.integer(r[["n_terms_ns"]]))),
  "\\bottomrule", "\\end{tabular}")
wtab(si6, "si_sensitivity.tex")

# --- S: energy x psychoacoustic collinearity ---------------------------------
col <- OUTC("a11_adopted_models", "si_energy_psychoacoustic_collinearity.csv")
colw <- col %>% tidyr::pivot_wider(names_from = v2, values_from = r)
psy <- c("N50", "S50", "R50", "F50", "T50", "AI50")
si7 <- c(
  "\\begin{tabular}{lcccccc}", "\\toprule",
  paste(" &", paste(TERM_TEX[psy], collapse = " & "), "\\\\"),
  "\\midrule",
  apply(colw, 1, function(r) {
    sprintf("%s & %s \\\\", TERM_TEX[r[["v1"]]],
            paste(fnum("%.3f", as.numeric(r[psy])), collapse = " & "))
  }),
  "\\bottomrule", "\\end{tabular}")
wtab(si7, "si_collinearity.tex")

# --- S: random structure + playback order ------------------------------------
ord <- OUTC("a11_adopted_models", "tableS2_adopted_order_sensitivity.csv")
rs <- OUTC("a11_adopted_models", "tableS1_adopted_random_structure.csv")
ORD_LAB <- c(BlockOrder = "block order", BlockPos = "position within block",
             GlobalOrder = "global position")
lab_added <- function(a) unname(ifelse(a %in% names(ORD_LAB), ORD_LAB[a], a))
si8 <- c(
  "\\begin{tabular}{llcccc}", "\\toprule",
  "Outcome & Model & AIC (ML) & $\\Delta$AIC & $B$ & $p$ \\\\",
  "\\midrule",
  "\\multicolumn{6}{l}{\\emph{Batch as a third crossed random intercept}} \\\\",
  apply(rs, 1, function(r)
    sprintf("%s & $+$ Batch intercept & %.3f & %.3f & var $=$ %s%s & \\\\",
            r[["outcome"]], as.numeric(r[["AIC_plus_batch"]]),
            as.numeric(r[["AIC_plus_batch"]]) - as.numeric(r[["AIC_subj_stim"]]),
            format(as.numeric(r[["var_batch"]]), scientific = FALSE),
            ifelse(r[["singular"]] == "TRUE" | r[["singular"]] == TRUE,
                   " (singular)", ""))),
  "\\midrule",
  "\\multicolumn{6}{l}{\\emph{Serial-position terms added to the adopted models}} \\\\",
  apply(ord %>% filter(!grepl("none", added)), 1, function(r) {
    b <- suppressWarnings(as.numeric(r[["beta"]]))
    sprintf("%s & $+$ %s & %.3f & %s & %s & %s \\\\",
            r[["outcome"]], lab_added(r[["added"]]), as.numeric(r[["AIC"]]),
            fnum("%.3f", as.numeric(r[["dAIC"]])),
            ifelse(is.na(b), "--", fnum("%.4f", b)),
            fp(as.numeric(r[["p"]])))
  }),
  "\\bottomrule", "\\end{tabular}")
wtab(si8, "si_order.tex")

# --- S: dimension-level anatomy (A13) ----------------------------------------
dc <- OUTC("a13_dimension_unpacking", "dimension_coefficients.csv")
DIMS <- c("Pleasant", "Vibrant", "Eventful", "Chaotic", "Annoying",
          "Monotonous", "Uneventful", "Calm")
dc <- dc %>%
  mutate(dimension = factor(dimension, levels = DIMS)) %>%
  arrange(spec, term, dimension)
dcrow <- function(r) {
  sprintf("%s & %s & %s & %s & %s%s \\\\",
          TERM_TEX[r[["term"]]], r[["dimension"]],
          fnum("%.3f", as.numeric(r[["beta_std"]])), fp(as.numeric(r[["p"]])),
          fq(as.numeric(r[["q"]])),
          ifelse(r[["sig_fdr"]] == "TRUE" | r[["sig_fdr"]] == TRUE,
                 "$^{*}$", ""))
}
blk <- function(sp, label) {
  d <- dc %>% filter(spec == sp)
  c(sprintf("\\multicolumn{5}{l}{\\emph{%s}} \\\\", label),
    apply(d, 1, dcrow))
}
si9 <- c(
  "\\begin{tabular}{llccc}", "\\toprule",
  "Predictor & Attribute & Standardised $\\beta$ & $p$ & $q$ \\\\",
  "\\midrule",
  blk("ISOP_spec", "ISOP predictor set (3 predictors $\\times$ 8 attributes; BH-FDR family of 24)"),
  "\\addlinespace",
  blk("ISOE_spec", "ISOE predictor set (5 predictors $\\times$ 8 attributes; BH-FDR family of 40)"),
  "\\bottomrule", "\\end{tabular}")
wtab(si9, "si_dimensions.tex")

# --- S: moderation / nonlinearity (A14) --------------------------------------
mo <- OUTC("a14_moderation_nonlinearity", "moderation_tests.csv")
MOLAB <- c(ISOP_LAeqxType2 = "$L_{\\mathrm{Aeq}}$ $\\times$ Type 2 share",
           ISOP_LAeq2 = "$L_{\\mathrm{Aeq}}^2$",
           ISOE_FlucxType1 = "($L_{\\mathrm{A10}}-L_{\\mathrm{A90}}$) $\\times$ Type 1 share",
           ISOE_Fluc2 = "($L_{\\mathrm{A10}}-L_{\\mathrm{A90}}$)$^2$")
# Revision 1: REML coefficient of each added term with its Satterthwaite t
# interval (A19), printed in units of 10^-3.
moi <- OUTC("a19_uncertainty", "moderation_intervals.csv")
morow <- function(oc) apply(mo %>% filter(outcome == oc), 1, function(r) {
  m <- moi %>% filter(id == r[["id"]])
  stopifnot(nrow(m) == 1, abs(m$dAIC - as.numeric(r[["dAIC"]])) < 1e-6)
  sprintf("%s & $+$%.2f & %.2f & %s & %s & %s %s \\\\", MOLAB[r[["id"]]],
          as.numeric(r[["dAIC"]]), as.numeric(r[["chisq"]]),
          fp(as.numeric(r[["p_lrt"]])), fq(as.numeric(r[["q"]])),
          fnum("%.3f", 1000 * m$est),
          ci_tex(1000 * m$CI_low, 1000 * m$CI_high, "%.3f"))
})
si10 <- c(
  "\\begin{tabular}{lccccc}", "\\toprule",
  "Added term & $\\Delta$AIC & $\\chi^2$(1) & $p$ (LRT) & $q$ & $B$ [95\\% CI] ($\\times 10^{-3}$) \\\\",
  "\\midrule",
  "\\multicolumn{6}{l}{\\emph{ISOP adopted model}} \\\\", morow("ISOP"),
  "\\addlinespace",
  "\\multicolumn{6}{l}{\\emph{ISOE adopted model}} \\\\", morow("ISOE"),
  "\\bottomrule", "\\end{tabular}")
wtab(si10, "si_moderation.tex")

# --- S: LOSO per-recording predictions (A15) ---------------------------------
lo <- OUTC("a15_predictive_validity", "loso_per_stimulus.csv")
low <- lo %>%
  tidyr::pivot_wider(names_from = outcome,
                     values_from = c(obs_mean, pred_mean, abs_err))
si11 <- c(
  "\\begin{tabular}{lcccccc}", "\\toprule",
  "Recording & ISOP obs. & ISOP pred. & $|$error$|$ & ISOE obs. & ISOE pred. & $|$error$|$ \\\\",
  "\\midrule",
  apply(low, 1, function(r)
    sprintf("%s & %s & %s & %.3f & %s & %s & %.3f \\\\",
            r[["StimID"]],
            fnum("%.3f", as.numeric(r[["obs_mean_ISOP"]])),
            fnum("%.3f", as.numeric(r[["pred_mean_ISOP"]])),
            as.numeric(r[["abs_err_ISOP"]]),
            fnum("%.3f", as.numeric(r[["obs_mean_ISOE"]])),
            fnum("%.3f", as.numeric(r[["pred_mean_ISOE"]])),
            as.numeric(r[["abs_err_ISOE"]]))),
  "\\bottomrule", "\\end{tabular}")
wtab(si11, "si_loso.tex")

# =============================================================================
# Revision 1 — sensitivity tables from A17 and A18
# =============================================================================
# --- S: validation sensitivity (A18) ------------------------------------------
cvs <- OUTC("a18_validation_sensitivity", "cv_summary.csv")
cvr <- OUTC("a18_validation_sensitivity",
            "conditional_site_bootstrap_ranges.csv") %>%
  filter(metric == "predictive_R2")
CVLAB <- c(
  adopted = "Adopted specification, leave-one-recording-out",
  acoustic_only = "Adopted specification without the source share",
  site_adopted = "Adopted specification, leave-one-site-out",
  strict_full_selected_fixed = "Strict rule, selected on the full data and fixed",
  strict_fold_reselected = "Strict rule, re-selected within each training fold",
  temporal_aic_full_selected_fixed = "Temporal-AIC rule, selected on the full data and fixed",
  temporal_aic_fold_reselected = "Temporal-AIC rule, re-selected within each training fold",
  null = "Null model, leave-one-recording-out",
  site_null = "Null model, leave-one-site-out")
stopifnot(setequal(names(CVLAB), unique(cvs$variant)))
cvcell <- function(oc, v) {
  d <- cvs %>% filter(outcome == oc, variant == v)
  g <- cvr %>% filter(outcome == oc, variant == v)
  stopifnot(nrow(d) == 1, nrow(g) <= 1)
  sprintf("%s & %s & %s & %.3f", fnum("%.2f", d$r), fnum("%.2f", d$predictive_R2),
          if (nrow(g) == 1) ci_tex(g$lo, g$hi, "%.2f") else "--", d$rmse)
}
si12 <- c(
  "\\begin{tabular}{lcccccccc}", "\\toprule",
  " & \\multicolumn{4}{c}{ISOP} & \\multicolumn{4}{c}{ISOE} \\\\",
  "\\cmidrule(lr){2-5}\\cmidrule(lr){6-9}",
  "Validation scheme & $r$ & $R^2_{\\mathrm{pred}}$ & Range & RMSE & $r$ & $R^2_{\\mathrm{pred}}$ & Range & RMSE \\\\",
  "\\midrule",
  sapply(names(CVLAB), function(v)
    sprintf("%s & %s & %s \\\\", CVLAB[[v]], cvcell("ISOP", v), cvcell("ISOE", v))),
  "\\bottomrule", "\\end{tabular}")
wtab(si12, "si_validation.tex")

# --- S: disjoint-listener source check (A17) ----------------------------------
sps <- OUTC("a17_source_sensitivity", "split_summary.csv")
SPL <- list(c("ISOP", "Type2_SNS"), c("ISOE", "Type1_AFS"))
splrow <- function(s) {
  full <- fci %>% filter(outcome == s[1], term == s[2])
  c(sprintf("\\multicolumn{4}{l}{\\emph{%s: %s (full-sample $B$ = %s)}} \\\\", s[1],
            TERM_TEX[[s[2]]], fnum("%.4f", full$B)),
    sapply(c("same_half", "disjoint"), function(m) {
      d <- sps %>% filter(outcome == s[1], mode == m)
      stopifnot(nrow(d) == 1, d$n == 2000, d$failures == 0, d$singular == 0)
      sprintf("%s & %s & %s & %.0f \\\\",
              ifelse(m == "same_half", "Annotations from the rating panel itself",
                     "Annotations from the other panel"),
              fnum("%.4f", d$median), ci_tex(d$lo, d$hi), d$expected_sign_pct)
    }))
}
si13 <- c(
  "\\begin{tabular}{lccc}", "\\toprule",
  "Source of the share & Median $B$ & 2.5th--97.5th percentile & Adopted sign (\\%) \\\\",
  "\\midrule", splrow(SPL[[1]]), "\\addlinespace", splrow(SPL[[2]]),
  "\\bottomrule", "\\end{tabular}")
wtab(si13, "si_split.tex")

# --- S: compositional form of the source term (A17) ---------------------------
cmo <- OUTC("a17_source_sensitivity", "composition_models.csv")
cco <- OUTC("a17_source_sensitivity", "composition_coefficients.csv")
sst <- OUTC("a17_source_sensitivity", "second_share_tests.csv")
FOCAL <- list(ISOP = c(raw = "Type2_SNS", ilr = "z1",
                       rawlab = "Type 2 share (Type 3 as reference)",
                       ilrlab = "Balance of Type 2 against Types 1 and 3"),
              ISOE = c(raw = "Type1_AFS", ilr = "z2",
                       rawlab = "Type 1 share (Type 3 as reference)",
                       ilrlab = "Balance of Type 1 against Type 3"))
comprow <- function(oc) {
  f <- FOCAL[[oc]]
  raw_m <- cmo %>% filter(outcome == oc, variant == "omit_Type3_DCS")
  raw_c <- cco %>% filter(outcome == oc, variant == "raw_Type3_reference", term == f[["raw"]])
  tst <- sst %>% filter(outcome == oc)
  stopifnot(nrow(raw_m) == 1, nrow(raw_c) == 1, nrow(tst) == 1)
  c(sprintf("%s & Two raw shares & %s & %s & %s & %s & %s & %s \\\\", oc,
            fnum("%+.2f", raw_m$delta_vs_adopted), fp(tst$p), fq(tst$q),
            f[["rawlab"]], fnum("%.4f", raw_c$B), ci_tex(raw_c$lo, raw_c$hi)),
    sapply(c("0.1", "0.5", "1"), function(k) {
      v <- paste0("ilr_count_plus_", k)
      m <- cmo %>% filter(outcome == oc, variant == v)
      cf <- cco %>% filter(outcome == oc, variant == v, term == f[["ilr"]])
      stopifnot(nrow(m) == 1, nrow(cf) == 1)
      sprintf("%s & Log-ratio balances, counts $+$%s & %s & -- & -- & %s & %s & %s \\\\", oc, k,
              fnum("%+.2f", m$delta_vs_adopted), f[["ilrlab"]],
              fnum("%.4f", cf$B), ci_tex(cf$lo, cf$hi))
    }))
}
si14 <- c(
  "\\begin{tabular}{llccclcc}", "\\toprule",
  "Outcome & Form of the source term & $\\Delta$AIC & $p$ (LRT) & $q$ & Focal term & $B$ & 95\\% CI \\\\",
  "\\midrule", comprow("ISOP"), "\\addlinespace", comprow("ISOE"),
  "\\bottomrule", "\\end{tabular}")
si14 <- gsub("$+$-", "$-$", gsub("& \\+", "& $+$", si14), fixed = TRUE)
wtab(si14, "si_composition.tex")

# --- S: leave-one-recording-out stability of the adopted coefficients (A18) ---
dls <- OUTC("a18_validation_sensitivity", "deletion_coefficient_summary.csv")
DORD <- list(ISOP = c("LAeq", "T50", "Type2_SNS"),
             ISOE = c("LCLA", "T50", "AI50", "LA10LA90", "Type1_AFS"))
delrow <- function(oc) sapply(DORD[[oc]], function(tm) {
  d <- dls %>% filter(outcome == oc, term == tm)
  full <- fci %>% filter(outcome == oc, term == tm)
  stopifnot(nrow(d) == 1, nrow(full) == 1, d$n == 36)
  same <- if (full$B > 0) d$positive else d$negative
  sprintf("%s & %s & %s & %s & %d/36 & %d/36 & %s \\\\", TERM_TEX[[tm]],
          fnum("%.4f", full$B), fnum("%.4f", d$min_B), fnum("%.4f", d$max_B),
          same, d$nominal_p_below_05, fp(d$max_p))
})
si15 <- c(
  "\\begin{tabular}{lcccccc}", "\\toprule",
  "Term & Full $B$ & Min.\\ $B$ & Max.\\ $B$ & Sign kept & $p < 0.05$ & Max.\\ $p$ \\\\",
  "\\midrule",
  "\\multicolumn{7}{l}{\\emph{ISOP adopted model}} \\\\", delrow("ISOP"),
  "\\addlinespace",
  "\\multicolumn{7}{l}{\\emph{ISOE adopted model}} \\\\", delrow("ISOE"),
  "\\bottomrule", "\\end{tabular}")
wtab(si15, "si_deletion.tex")

cat("All table fragments written to", TAB, "\n")
