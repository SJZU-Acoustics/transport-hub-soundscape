# A03 — One-way ANOVA of ISOP / ISOE by level rank and by functional type at
# recording level (N = 36), with post-hoc contrasts on the level factor under
# both Tukey HSD and unadjusted LSD. Feeds Supplementary Table S8.

source(file.path("code", "helpers.R"))
OUT <- outdir("a03_anova")

one_way <- function(dv, factor_name) {
  f <- as.formula(paste(dv, "~", factor_name))
  a <- anova(aov(f, data = stim36))
  ss_b <- a$`Sum Sq`[1]; ss_w <- a$`Sum Sq`[2]
  df_b <- a$Df[1];       df_w <- a$Df[2]
  tibble(
    Dependent = dv,
    Grouping = ifelse(factor_name == "SPL_Group", "Sound pressure level", "Function type"),
    Source = c("Between groups", "Within groups", "Total"),
    SS = c(ss_b, ss_w, ss_b + ss_w),
    df = c(df_b, df_w, df_b + df_w),
    MS = c(ss_b / df_b, ss_w / df_w, NA_real_),
    F = c(a$`F value`[1], NA_real_, NA_real_),
    p = c(a$`Pr(>F)`[1], NA_real_, NA_real_)
  )
}

t6 <- bind_rows(
  one_way("ISOP", "SPL_Group"),
  one_way("ISOP", "FuncCode"),
  one_way("ISOE", "SPL_Group"),
  one_way("ISOE", "FuncCode")
) %>%
  mutate(across(c(SS, MS), ~ round(.x, 3)),
         F = round(F, 3), p = round(p, 4))
write_outcome(t6, file.path(OUT, "table06_anova.csv"))

# ---- Post-hoc comparisons (section 3.3.1 text) -------------------------------
posthoc <- map_dfr(c("ISOP", "ISOE"), function(dv) {
  m <- aov(as.formula(paste(dv, "~ SPL_Group")), data = stim36)
  tk <- as.data.frame(TukeyHSD(m)$SPL_Group) %>%
    rownames_to_column("contrast") %>%
    transmute(Dependent = dv, method = "Tukey HSD", contrast,
              diff = round(diff, 4), p_adj = round(`p adj`, 4))
  lsd <- as.data.frame(pairs(emmeans::emmeans(m, ~ SPL_Group), adjust = "none")) %>%
    transmute(Dependent = dv, method = "LSD (unadjusted)",
              contrast = as.character(contrast),
              diff = round(estimate, 4), p_adj = round(p.value, 4))
  bind_rows(tk, lsd)
})
write_outcome(posthoc, file.path(OUT, "posthoc_spl.csv"))

# ---- Group means by level rank and functional type --------------------------
means <- bind_rows(
  stim36 %>% group_by(Group = SPL_Group) %>%
    summarise(n = n(), ISOP = mean(ISOP), ISOE = mean(ISOE), .groups = "drop"),
  stim36 %>% group_by(Group = FuncCode) %>%
    summarise(n = n(), ISOP = mean(ISOP), ISOE = mean(ISOE), .groups = "drop")
) %>% mutate(across(c(ISOP, ISOE), ~ round(.x, 4)))
write_outcome(means, file.path(OUT, "group_means.csv"))


cat("\n== Table 6 ==\n"); print(as.data.frame(t6))
cat("\n== Post-hoc, SPL groups ==\n"); print(as.data.frame(posthoc))
cat("\n== Group means ==\n"); print(as.data.frame(means))
