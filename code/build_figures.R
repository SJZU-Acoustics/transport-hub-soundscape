# =============================================================================
# Publication figures: Figs. 1-5 and Supplementary Figs. S1-S2.
# Style: Helvetica, 8-10 pt at print size, left/bottom spines only, outward
# ticks, no grid, Okabe-Ito palette, PNG at 600 dpi.
# Inputs: the deposited data tables + the analysis modules' result tables.
# =============================================================================

source(file.path("code", "helpers.R"))
suppressPackageStartupMessages({library(ggplot2); library(patchwork)})

FIG <- file.path("output", "figures")
dir.create(FIG, showWarnings = FALSE, recursive = TRUE)
OUTC <- function(a, f) file.path("output", a, f)

mm2in <- function(mm) mm / 25.4
W1 <- mm2in(85); W2 <- mm2in(178)
okabe_ito <- c("#0072B2", "#E69F00", "#009E73", "#D55E00", "#56B4E9", "#CC79A7")

theme_pub <- function(base_size = 9, axis_title_size = 10) {
  theme_classic(base_size = base_size, base_family = "Helvetica") %+replace%
    theme(axis.line = element_line(colour = "black", linewidth = 0.4),
          axis.ticks = element_line(colour = "black", linewidth = 0.35),
          axis.ticks.length = unit(0.10, "cm"),
          axis.title = element_text(size = axis_title_size),
          axis.text = element_text(size = base_size, colour = "black"),
          legend.text = element_text(size = base_size),
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.background = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_blank(),
          plot.background = element_rect(fill = "white", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA),
          plot.tag = element_text(size = 10, face = "bold", family = "Helvetica"))
}
sv <- function(p, name, w, h) ggsave(file.path(FIG, name), p, width = w,
                                     height = h, dpi = 600, bg = "white")

SPL_COL <- c(HSPL = "#D55E00", MSPL = "#E69F00", LSPL = "#0072B2")

# Plotmath labels (ISO 80000-8: italic quantity letter, roman subscript).
LAB <- c(
  LAeq = 'italic(L)[plain("Aeq")]', LCeq = 'italic(L)[plain("Ceq")]',
  LCLA = 'italic(L)[plain("Ceq")] - italic(L)[plain("Aeq")]',
  LA50 = 'italic(L)[plain("A50")]', LC50 = 'italic(L)[plain("C50")]',
  N50 = 'italic(N)[50]', S50 = 'italic(S)[50]', R50 = 'italic(R)[50]',
  F50 = 'italic(F)[50]', T50 = 'italic(T)[50]', AI50 = 'plain("AI")[50]',
  LA10LA90 = 'italic(L)[plain("A10")] - italic(L)[plain("A90")]',
  LC10LC90 = 'italic(L)[plain("C10")] - italic(L)[plain("C90")]',
  N10N90 = 'italic(N)[10] - italic(N)[90]',
  S10S90 = 'italic(S)[10] - italic(S)[90]',
  R10R90 = 'italic(R)[10] - italic(R)[90]',
  F10F90 = 'italic(F)[10] - italic(F)[90]',
  T10T90 = 'italic(T)[10] - italic(T)[90]',
  AI10AI90 = 'plain("AI")[10] - plain("AI")[90]',
  Type1_AFS = '"Type 1 functional"', Type2_SNS = '"Type 2 natural+music"',
  Type3_DCS = '"Type 3 communication"')
lab_of <- function(keys) parse(text = unname(LAB[keys]))

# =============================================================================
# Fig. 1 — study design and four-layer driver framework (conceptual)
# =============================================================================
box <- function(x0, x1, y0, y1, fill = "white", col = "black", lwd = 0.35) {
  annotate("rect", xmin = x0, xmax = x1, ymin = y0, ymax = y1,
           fill = fill, colour = col, linewidth = lwd)
}
txt <- function(x, y, lab, size = 2.5, face = "plain", hjust = 0.5, col = "black",
                parse = FALSE) {
  annotate("text", x = x, y = y, label = lab, size = size, fontface = face,
           hjust = hjust, family = "Helvetica", colour = col, lineheight = 0.95,
           parse = parse)
}
# Indicator lists for the Fig. 1 layer boxes, in the LAB notation.
lab_list <- function(keys) paste(LAB[keys], collapse = '*", "*')
arr <- function(x0, x1, y0, y1, lwd = 0.35) {
  annotate("segment", x = x0, xend = x1, y = y0, yend = y1, linewidth = lwd,
           arrow = arrow(length = unit(0.10, "cm"), type = "closed"))
}

pal_layer <- c("#E8F1F8", "#FDF0DC", "#E6F4EE", "#FBE9DE")

f1 <- ggplot() + xlim(0, 100) + ylim(0, 62) + theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = NA)) +
  # ---- panel a: pipeline ----------------------------------------------------
  txt(1.2, 60.5, "a", size = 3.5, face = "bold", hjust = 0) +
  box(2, 24, 48, 59, fill = "grey97") +
  txt(13, 55.5, "Field recording campaign", size = 2.5, face = "bold") +
  txt(13, 51.5, "34 transport hubs, 2013\u20132017\n676 long recordings", size = 2.3) +
  arr(24, 27.5, 53.5, 53.5) +
  box(27.5, 49.5, 48, 59, fill = "grey97") +
  txt(38.5, 55.5, "36 recordings (30 s)", size = 2.5, face = "bold") +
  txt(38.5, 51.5, "6 functional types ×\n3 level ranks × 2 recordings", size = 2.3) +
  arr(49.5, 53, 53.5, 53.5) +
  box(53, 75, 48, 59, fill = "grey97") +
  txt(64, 55.5, "Laboratory listening test", size = 2.5, face = "bold") +
  txt(64, 51.5, "59 listeners, diotic playback\n8 ISO/TS 12913-2 attributes", size = 2.3) +
  arr(75, 78.5, 53.5, 53.5) +
  box(78.5, 98.5, 48, 59, fill = "grey97") +
  txt(88.5, 55.5, "2,116 valid ratings", size = 2.5, face = "bold") +
  txt(88.5, 52.3, "ISO rotation:", size = 2.3) +
  txt(88.5, 50.6, 'italic("ISO-P")*" and "*italic("ISO-E")', size = 2.3,
      parse = TRUE) +
  # ---- panel b: four layers -> two dimensions -------------------------------
  txt(1.2, 44.5, "b", size = 3.5, face = "bold", hjust = 0) +
  box(2, 34, 34, 43, fill = pal_layer[1]) +
  txt(18, 40.6, "Sound energy exposure", size = 2.5, face = "bold") +
  txt(18, 37.2, lab_list(c("LAeq", "LCeq", "LCLA", "LA50", "LC50")),
      size = 2.2, parse = TRUE) +
  box(2, 34, 23.5, 32.5, fill = pal_layer[2]) +
  txt(18, 30.1, "Psychoacoustic", size = 2.5, face = "bold") +
  txt(18, 26.7, lab_list(c("N50", "S50", "R50", "F50", "T50", "AI50")),
      size = 2.2, parse = TRUE) +
  box(2, 34, 13, 22, fill = pal_layer[3]) +
  txt(18, 19.6, "Temporal dynamics", size = 2.5, face = "bold") +
  txt(18, 16.2, "10th\u201390th percentile ranges of the\nlevel and psychoacoustic families",
      size = 2.2) +
  box(2, 34, 2.5, 11.5, fill = pal_layer[4]) +
  txt(18, 9.1, "Sound-source semantics", size = 2.5, face = "bold") +
  txt(18, 5.7, "shares of functional (Type 1), natural+music\n(Type 2), communication (Type 3) sounds", size = 2.15) +
  # outcome boxes
  box(66, 98.5, 27, 42, fill = "white", lwd = 0.5) +
  txt(82.2, 39.2, 'bold("Perceived pleasantness (")*bolditalic("ISO-P")*bold(")")',
      size = 2.5, parse = TRUE) +
  txt(82.2, 34.4, 'italic(L)[plain("Aeq")]*" (-)   "*italic(T)[50]*" (+)"',
      size = 2.3, parse = TRUE) +
  txt(82.2, 32.6, "natural-and-music share (+)", size = 2.3) +
  box(66, 98.5, 3.5, 18.5, fill = "white", lwd = 0.5) +
  txt(82.2, 15.7, 'bold("Perceived eventfulness (")*bolditalic("ISO-E")*bold(")")',
      size = 2.5, parse = TRUE) +
  txt(82.2, 10.6,
      paste0('plain("AI")[50]*" (-)   "*italic(T)[50]*" (+)   "*',
             'italic(L)[plain("A10")]-italic(L)[plain("A90")]*" (+)"'),
      size = 2.3, parse = TRUE) +
  txt(82.2, 8.8, "functional-sound share (-)", size = 2.3) +
  # arrows layers -> outcomes (one per layer x dimension with a significant driver)
  arr(34, 66, 38.5, 36.5) +
  arr(34, 66, 28, 33) +
  arr(34, 66, 26, 13.5) +
  arr(34, 66, 17.5, 11.5) +
  arr(34, 66, 9, 28.5) +
  arr(34, 66, 7, 9) +
  # crossed random intercepts note
  box(38, 62, 20.5, 26.5, fill = "grey93", lwd = 0.35) +
  txt(50, 23.5, "Crossed random intercepts\nlistener + recording", size = 2.2)
sv(f1, "fig1_framework.png", W2, W2 * 0.44)

# =============================================================================
# Fig. 2 — perceptual structure: circumplex, level gradient, variance split
# =============================================================================
d2a <- stim36 %>% mutate(SPL_Group = factor(SPL_Group, levels = names(SPL_COL)))
p2a <- ggplot(d2a, aes(ISOP, ISOE, colour = SPL_Group, shape = SPL_Group)) +
  geom_hline(yintercept = 0, linetype = "22", linewidth = 0.3, colour = "grey55") +
  geom_vline(xintercept = 0, linetype = "22", linewidth = 0.3, colour = "grey55") +
  geom_point(size = 1.7, stroke = 0.6) +
  scale_colour_manual(values = SPL_COL) +
  scale_shape_manual(values = c(HSPL = 16, MSPL = 17, LSPL = 15)) +
  scale_x_continuous(limits = c(-0.65, 0.45), breaks = seq(-0.6, 0.4, 0.2),
                     labels = function(x) sprintf("%.1f", x)) +
  scale_y_continuous(limits = c(-0.48, 0.55), breaks = seq(-0.4, 0.4, 0.2),
                     labels = function(x) sprintf("%.1f", x)) +
  labs(x = expression(italic("ISO-P")), y = expression(italic("ISO-E"))) +
  theme_pub(8, 9) +
  theme(legend.position = "inside", legend.position.inside = c(0.03, 0.03),
        legend.justification = c(0, 0), legend.key.size = unit(0.32, "cm"))

d2b <- stim36 %>%
  select(StimID, SPL_Group, ISOP, ISOE) %>%
  pivot_longer(c(ISOP, ISOE), names_to = "dim", values_to = "score") %>%
  mutate(dim = recode(dim, ISOP = "ISO-P", ISOE = "ISO-E"),
         dim = factor(dim, levels = c("ISO-P", "ISO-E")),
         SPL_Group = factor(SPL_Group, levels = names(SPL_COL)))
p2b <- ggplot(d2b, aes(SPL_Group, score, colour = dim)) +
  geom_hline(yintercept = 0, linetype = "22", linewidth = 0.3, colour = "grey55") +
  geom_boxplot(position = position_dodge(0.75), width = 0.6, linewidth = 0.4,
               outlier.size = 0.9, fill = NA) +
  scale_colour_manual(values = c("ISO-P" = "#0072B2", "ISO-E" = "#D55E00"),
                      breaks = c("ISO-P", "ISO-E"),
                      labels = parse(text = c('italic("ISO-P")', 'italic("ISO-E")'))) +
  scale_y_continuous(limits = c(-0.65, 0.58), breaks = seq(-0.6, 0.4, 0.2),
                     labels = function(x) sprintf("%.1f", x)) +
  labs(x = "Level rank within functional type", y = "Score") +
  theme_pub(8, 9) +
  theme(legend.position = "inside", legend.position.inside = c(0.03, 0.04),
        legend.justification = c(0, 0), legend.key.size = unit(0.32, "cm"))

d2c <- read_csv(OUTC("a05_null_models", "table08_null_models.csv"),
                show_col_types = FALSE) %>%
  filter(component != "Total") %>%
  mutate(component = recode(component, Subject = "Listener",
                            Stimulus = "Recording", Residual = "Residual"),
         component = factor(component, levels = c("Residual", "Listener", "Recording")),
         outcome = recode(outcome, ISOP = "ISO-P", ISOE = "ISO-E"),
         outcome = factor(outcome, levels = c("ISO-P", "ISO-E")))
p2c <- ggplot(d2c, aes(outcome, proportion_pct, fill = component)) +
  geom_col(width = 0.62, colour = "black", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.1f", proportion_pct)),
            position = position_stack(vjust = 0.5), size = 2.5,
            family = "Helvetica") +
  scale_fill_manual(values = c(Recording = "#56B4E9", Listener = "#E69F00",
                               Residual = "grey88"),
                    breaks = c("Recording", "Listener", "Residual")) +
  scale_y_continuous(limits = c(0, 100.2), breaks = seq(0, 100, 25),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_x_discrete(labels = parse(text = c('italic("ISO-P")', 'italic("ISO-E")'))) +
  labs(x = NULL, y = "Share of rating variance (%)") +
  theme_pub(8, 9) +
  theme(legend.position = "right", legend.key.size = unit(0.32, "cm"),
        legend.margin = margin(0, 0, 0, 0))

f2 <- (p2a + p2b + p2c) + plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 10, face = "bold", family = "Helvetica"))
sv(f2, "fig2_perception.png", W2, W2 * 0.37)

# =============================================================================
# Fig. 3 — indicator-dimension correlation heatmap (N = 36)
# =============================================================================
cm <- read_csv(OUTC("a04_correlations", "table07_correlations.csv"),
               show_col_types = FALSE)
ord <- c("LAeq", "LCeq", "LCLA", "LA50", "LC50",
         "N50", "S50", "R50", "F50", "T50", "AI50",
         "LA10LA90", "LC10LC90", "N10N90", "S10S90", "R10R90", "F10F90",
         "T10T90", "AI10AI90", "Type1_AFS", "Type2_SNS", "Type3_DCS")
d3 <- cm %>%
  select(var, ISOP_r, ISOP_sig, ISOE_r, ISOE_sig) %>%
  pivot_longer(-var, names_to = c("dim", ".value"), names_sep = "_") %>%
  mutate(var = factor(var, levels = rev(ord)),
         dim = recode(dim, ISOP = "ISO-P", ISOE = "ISO-E"),
         dim = factor(dim, levels = c("ISO-P", "ISO-E")),
         lab = sprintf("%.2f", r))
p3 <- ggplot(d3, aes(dim, var, fill = r)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = lab), size = 2.5, family = "Helvetica") +
  scale_fill_gradient2(low = "#0072B2", mid = "white", high = "#D55E00",
                       limits = c(-0.7, 0.7),
                       name = expression(italic(r))) +
  scale_y_discrete(labels = lab_of(rev(ord))) +
  scale_x_discrete(position = "top",
                   labels = parse(text = c('italic("ISO-P")', 'italic("ISO-E")'))) +
  labs(x = NULL, y = NULL) +
  theme_pub(8, 9) +
  theme(axis.line = element_blank(), axis.ticks = element_blank(),
        legend.title = element_text(size = 8),
        legend.position = "right", legend.key.height = unit(0.9, "cm"),
        legend.key.width = unit(0.28, "cm"))
sv(p3, "fig3_correlations.png", W1, W1 * 1.55)

# =============================================================================
# Fig. 4 — standardised fixed effects of the adopted models (forest)
# =============================================================================
fb <- read_csv(OUTC("a11_adopted_models",
                    "fig08_adopted_standardised_betas.csv"),
               show_col_types = FALSE)
ord_isop <- c("LAeq", "T50", "Type2_SNS")
ord_isoe <- c("LCLA", "T50", "AI50", "LA10LA90", "Type1_AFS")
mk_forest <- function(dd, ordv, col) {
  dd %>%
    mutate(Term = factor(Term, levels = rev(ordv)),
           sig = p < 0.05) %>%
    ggplot(aes(beta_std, Term)) +
    geom_vline(xintercept = 0, linetype = "22", linewidth = 0.3, colour = "grey55") +
    geom_errorbar(aes(xmin = CI_low, xmax = CI_high), orientation = "y",
                  width = 0.18, linewidth = 0.45, colour = col) +
    geom_point(aes(shape = sig), size = 1.9, colour = col, fill = "white") +
    scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 21), guide = "none") +
    scale_x_continuous(limits = c(-0.62, 0.48), breaks = seq(-0.6, 0.4, 0.2),
                       labels = function(x) sprintf("%.1f", x)) +
    scale_y_discrete(labels = lab_of(rev(ordv))) +
    labs(x = expression("Standardised coefficient" ~ (beta)), y = NULL) +
    theme_pub(8, 9)
}
p4a <- mk_forest(filter(fb, outcome == "ISOP"), ord_isop, "#0072B2") +
  theme(axis.title.x = element_blank())
p4b <- mk_forest(filter(fb, outcome == "ISOE"), ord_isoe, "#D55E00")
f4 <- (p4a / p4b) + plot_layout(heights = c(3, 5)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 10, face = "bold", family = "Helvetica"))
sv(f4, "fig4_forest.png", W1, W1 * 0.95)

# =============================================================================
# Fig. 5 — LOSO predicted vs observed held-out stimulus means (A15)
# =============================================================================
lo <- read_csv(OUTC("a15_predictive_validity", "loso_per_stimulus.csv"),
               show_col_types = FALSE) %>%
  left_join(stim36 %>% select(StimID, SPL_Group), by = "StimID") %>%
  mutate(SPL_Group = factor(SPL_Group, levels = names(SPL_COL)))
ls <- read_csv(OUTC("a15_predictive_validity", "loso_summary.csv"),
               show_col_types = FALSE)
mk_loso <- function(oc, lims) {
  s <- filter(ls, outcome == oc)
  ocl <- sub("ISOP", "ISO-P", sub("ISOE", "ISO-E", oc))
  ann <- sprintf('italic(r) == "%.2f"~~italic(R)^2 == "%.2f"', s$r_stim, s$R2_stim)
  ggplot(filter(lo, outcome == oc), aes(obs_mean, pred_mean)) +
    geom_abline(slope = 1, intercept = 0, linetype = "22", linewidth = 0.35,
                colour = "grey55") +
    geom_point(aes(colour = SPL_Group, shape = SPL_Group), size = 1.8, stroke = 0.6) +
    scale_colour_manual(values = SPL_COL) +
    scale_shape_manual(values = c(HSPL = 16, MSPL = 17, LSPL = 15)) +
    scale_x_continuous(limits = lims) + scale_y_continuous(limits = lims) +
    annotate("text", x = lims[1] + 0.02, y = lims[2] - 0.02, label = ann,
             parse = TRUE, hjust = 0, vjust = 1, size = 2.8, family = "Helvetica") +
    labs(x = parse(text = sprintf('"Observed "*italic("%s")*" (held-out recording mean)"', ocl)),
         y = parse(text = sprintf('"Predicted "*italic("%s")', ocl))) +
    coord_fixed() +
    theme_pub(8, 9) +
    theme(legend.position = "inside", legend.position.inside = c(0.97, 0.03),
          legend.justification = c(1, 0), legend.key.size = unit(0.32, "cm"))
}
p5a <- mk_loso("ISOP", c(-0.66, 0.47))
p5b <- mk_loso("ISOE", c(-0.45, 0.45))
f5 <- (p5a + p5b) + plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 10, face = "bold", family = "Helvetica"))
sv(f5, "fig5_loso.png", W2, W2 * 0.52)

# =============================================================================
# SI Fig. S1 — AIC of the hierarchical builds (adopted models)
# =============================================================================
aic <- read_csv(OUTC("a11_adopted_models", "fig07_adopted_aic.csv"),
                show_col_types = FALSE) %>%
  mutate(step = as.integer(sub("Model ", "", Model)),
         Layer = recode(Layer, "Null model" = "Null",
                        "Sound energy exposure" = "Energy",
                        "Psychoacoustic" = "Psychoacoustic",
                        "Temporal dynamic" = "Temporal",
                        "Sound source semantic" = "Source"))
mk_aic <- function(oc, col) {
  d <- filter(aic, outcome == oc)
  ggplot(d, aes(step, AIC)) +
    geom_line(linewidth = 0.45, colour = col) +
    geom_point(size = 1.8, colour = col) +
    scale_x_continuous(breaks = d$step, labels = d$Layer) +
    labs(x = NULL, y = "AIC (ML)") +
    theme_pub(8, 9) +
    theme(axis.text.x = element_text(angle = 40, hjust = 1))
}
s1a <- mk_aic("ISOP", "#0072B2")
s1b <- mk_aic("ISOE", "#D55E00")
fs1 <- (s1a + s1b) + plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 10, face = "bold", family = "Helvetica"))
sv(fs1, "sifig2_aic.png", W2, W2 * 0.42)

# =============================================================================
# SI Fig. S2 — three-class source composition by hub functional type
# =============================================================================
src <- read_csv(OUTC("a01_descriptives", "fig03_source_shares_by_function.csv"),
                show_col_types = FALSE) %>%
  select(FuncCode, Type1_AFS, Type2_SNS, Type3_DCS) %>%
  pivot_longer(-FuncCode, names_to = "type", values_to = "share") %>%
  mutate(FuncCode = factor(FuncCode, levels = FUNC_LEVELS),
         type = recode(type, Type1_AFS = "Type 1 functional",
                       Type2_SNS = "Type 2 natural+music",
                       Type3_DCS = "Type 3 communication"),
         type = factor(type, levels = c("Type 3 communication",
                                        "Type 2 natural+music", "Type 1 functional")))
ps2 <- ggplot(src, aes(FuncCode, share, fill = type)) +
  geom_col(width = 0.66, colour = "black", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.1f", share)),
            position = position_stack(vjust = 0.5), size = 2.4,
            family = "Helvetica") +
  scale_fill_manual(values = c("Type 1 functional" = "#D55E00",
                               "Type 2 natural+music" = "#009E73",
                               "Type 3 communication" = "#56B4E9"),
                    breaks = c("Type 1 functional", "Type 2 natural+music",
                               "Type 3 communication")) +
  scale_y_continuous(limits = c(0, 101), breaks = seq(0, 100, 25),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Transport-hub functional type", y = "Share of source picks (%)") +
  theme_pub(9, 10) +
  theme(legend.position = "top", legend.key.size = unit(0.34, "cm"))
sv(ps2, "sifig1_sources.png", W2 * 0.7, W2 * 0.45)

cat("All figures written to", FIG, "\n")
