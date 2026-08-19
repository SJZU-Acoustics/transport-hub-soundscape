# A01 — Descriptive statistics of the four indicator layers and the two
# perception outcomes. Unit: recording (N = 36) for every acoustic and source
# variable. Grouping scheme: overall / six functional types / three level ranks.
# Feeds Table 1, Supplementary Tables S1 and S2, and Supplementary Fig. S1.

source(file.path("code", "helpers.R"))
OUT <- outdir("a01_descriptives")

# ---- Table 1: sound energy exposure layer -----------------------------------
t1 <- grouped_rows(stim36, LAYER_ENERGY, digits = 2)
write_outcome(t1, file.path(OUT, "table01_energy_layer.csv"))

# ---- Table 2: psychoacoustic layer ------------------------------------------
t2 <- grouped_rows(stim36, LAYER_PSYCHO, digits = 2)
write_outcome(t2, file.path(OUT, "table02_psychoacoustic_layer.csv"))

# ---- Table 3: temporal dynamic layer ----------------------------------------
t3 <- grouped_rows(stim36, LAYER_TEMPORAL, digits = 2)
write_outcome(t3, file.path(OUT, "table03_temporal_layer.csv"))

# ---- Table 4: source composition + perception -------------------------------
# Source shares are stimulus-level (N = 36). ISOP/ISOE are reported two ways:
# (a) stimulus-level means (N = 36 stimuli), (b) raw observations (N = 2,116).
t4_src <- grouped_rows(stim36, LAYER_SOURCE, digits = 2)

t4_perc_stim <- grouped_rows(stim36, c("ISOP", "ISOE"), digits = 2) %>%
  rename(ISOP_stimlevel = ISOP, ISOE_stimlevel = ISOE)

obs_grouped <- bind_rows(
  obs %>% mutate(Group = "Total"),
  obs %>% mutate(Group = as.character(FuncCode)),
  obs %>% mutate(Group = as.character(SPL_Group))
) %>%
  mutate(Group = factor(Group, levels = c("Total", FUNC_LEVELS, SPL_LEVELS))) %>%
  group_by(Group) %>%
  summarise(ISOP_obslevel = ms(ISOP), ISOE_obslevel = ms(ISOE),
            n_obs = n(), .groups = "drop")

t4 <- t4_src %>% left_join(t4_perc_stim, by = "Group") %>%
  left_join(obs_grouped, by = "Group")
write_outcome(t4, file.path(OUT, "table04_sources_and_perception.csv"))

# ---- Text-level descriptives quoted in section 3.1 ---------------------------
txt <- tibble(
  quantity = c("LAeq min", "LAeq max", "LAeq mean",
               "LCeq mean", "LC-LA mean", "LA50 mean", "LC50 mean",
               "N50 mean", "N50 sd", "S50 mean", "R50 mean", "F50 mean",
               "T50 mean", "AI50 mean",
               "LA10-LA90 mean", "LC10-LC90 mean", "N10-N90 mean",
               "ISOP mean (obs)", "ISOP sd (obs)", "ISOE mean (obs)", "ISOE sd (obs)",
               "ISOP mean (stim)", "ISOP sd (stim)", "ISOE mean (stim)", "ISOE sd (stim)",
               "ISOP mean HSPL (stim)", "ISOP mean LSPL (stim)"),
  value = c(min(stim36$LAeq), max(stim36$LAeq), mean(stim36$LAeq),
            mean(stim36$LCeq), mean(stim36$LCLA), mean(stim36$LA50), mean(stim36$LC50),
            mean(stim36$N50), sd(stim36$N50), mean(stim36$S50), mean(stim36$R50),
            mean(stim36$F50), mean(stim36$T50), mean(stim36$AI50),
            mean(stim36$LA10LA90), mean(stim36$LC10LC90), mean(stim36$N10N90),
            mean(obs$ISOP), sd(obs$ISOP), mean(obs$ISOE), sd(obs$ISOE),
            mean(stim36$ISOP), sd(stim36$ISOP), mean(stim36$ISOE), sd(stim36$ISOE),
            mean(stim36$ISOP[stim36$SPL_Group == "HSPL"]),
            mean(stim36$ISOP[stim36$SPL_Group == "LSPL"]))
) %>% mutate(value = round(value, 4))
write_outcome(txt, file.path(OUT, "text_descriptives_sec31.csv"))

# ---- Nine-source shares by function type (Fig. 3 content, sec 3.1.2) --------
nine <- c("交通工具声", "设备设施声", "公共广播声", "背景音乐声", "自然景观声",
          "广告影视声", "语音声", "行为声", "手机声")
fig3 <- source_shares %>%
  left_join(stim36 %>% select(StimID, FuncCode), by = "StimID") %>%
  group_by(FuncCode) %>%
  summarise(across(all_of(c(nine, LAYER_SOURCE)), ~ round(mean(.x), 2)), .groups = "drop")
write_outcome(fig3, file.path(OUT, "fig03_source_shares_by_function.csv"))

# ---- Participant-level descriptives quoted in the Methods -------------------
part <- participants %>%
  filter(status != "excluded_whole") %>%
  count(sex_code) %>%
  mutate(sex = ifelse(sex_code == 1, "male", "female"))
edu <- participants %>% count(edu_code) %>%
  mutate(edu = ifelse(edu_code == 1, "undergraduate 21-22", "master 23-24"))
write_outcome(bind_rows(
  part %>% transmute(field = paste("valid subjects,", sex), n = n),
  edu %>% transmute(field = paste("all recruited,", edu), n = n),
  tibble(field = "valid observations", n = nrow(obs)),
  tibble(field = "valid subjects", n = n_distinct(obs$SubjID))
), file.path(OUT, "participant_counts.csv"))

cat("\n== Table 1 ==\n"); print(as.data.frame(t1))
cat("\n== Table 2 ==\n"); print(as.data.frame(t2))
cat("\n== Table 3 ==\n"); print(as.data.frame(t3))
cat("\n== Table 4 ==\n"); print(as.data.frame(t4))
cat("\n== Section 3.1 text values ==\n"); print(as.data.frame(txt))
cat("\n== Fig 3 source shares by function type ==\n"); print(as.data.frame(fig3))
cat("\n== Participant counts ==\n")
print(as.data.frame(read_csv(file.path(OUT, "participant_counts.csv"), show_col_types = FALSE)))
