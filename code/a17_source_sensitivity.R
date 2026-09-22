# =============================================================================
# A17 — Sensitivity of the source-share associations (revision 1)
# Two questions raised in review:
#   (1) the source shares are computed from the same listeners whose ratings
#       they predict. Does the association survive when the shares come from
#       a disjoint listener panel?  -> 1,000 random 29/30 splits, both ways.
#   (2) the shares are compositional. Does the association depend on the form
#       of the source term?  -> a second raw share (omitting each class in
#       turn), and two isometric log-ratio balances with three pseudo-counts.
# Feeds Supplementary Tables S13 and S14.
# =============================================================================

source(file.path("code", "helpers.R"))

OUT <- outdir("a17_source_sensitivity")
set.seed(340917)

SPECS <- list(
  ISOP = c("LAeq", "T50", "Type2_SNS"),
  ISOE = c("LCLA", "T50", "AI50", "LA10LA90", "Type1_AFS")
)

# lme4 fit that records (rather than prints) convergence warnings.
fit <- function(y, xs, d, reml = TRUE) {
  w <- character()
  form <- as.formula(paste(y, "~", paste(c("1", xs, "(1|SubjID)", "(1|StimID)"),
                                          collapse = "+")))
  m <- withCallingHandlers(
    lme4::lmer(form, data = d, REML = reml,
               control = lmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5))),
    warning = function(e) { w <<- c(w, conditionMessage(e)); invokeRestart("muffleWarning") })
  attr(m, "revision_warnings") <- paste(unique(w), collapse = " | ")
  m
}
diag_m <- function(m) paste(c(attr(m, "revision_warnings"),
                              unlist(m@optinfo$conv$lme4$messages)), collapse = " | ")

# ---- Rebuild the three-class shares from the individual picks ---------------
# Source codes: 1-3, 6 = Type 1 (vehicles, equipment, announcements, adverts);
# 4-5 = Type 2 (music, natural sounds); 7-9 = Type 3 (speech, activity, phones).
# Code 10 ("none") is excluded from the denominator.
picks <- observations %>%
  select(SubjID, StimID, Src1, Src2, Src3) %>%
  pivot_longer(starts_with("Src"), values_to = "code") %>%
  filter(code %in% 1:9) %>%
  mutate(class = case_when(code %in% c(1, 2, 3, 6) ~ "Type1_AFS",
                           code %in% c(4, 5) ~ "Type2_SNS",
                           TRUE ~ "Type3_DCS"))

shares <- function(ids) {
  picks %>%
    filter(as.character(SubjID) %in% ids) %>%
    count(StimID, class) %>%
    complete(StimID, class = LAYER_SOURCE, fill = list(n = 0)) %>%
    group_by(StimID) %>% mutate(share = 100 * n / sum(n)) %>% ungroup() %>%
    select(StimID, class, share) %>%
    pivot_wider(names_from = class, values_from = share)
}

ids <- levels(obs$SubjID)
full <- shares(ids)
check <- full %>%
  left_join(source_shares %>% select(StimID, all_of(LAYER_SOURCE)),
            by = "StimID", suffix = c("_new", "_old"))
delta <- max(sapply(LAYER_SOURCE, function(x)
  max(abs(check[[paste0(x, "_new")]] - check[[paste0(x, "_old")]]))))
stopifnot(delta < 1e-6, length(ids) == 59)
write_outcome(tibble(check = "source reconstruction maximum percentage-point discrepancy",
                     value = delta), file.path(OUT, "reconstruction.csv"))

# ---- (1) Disjoint listener panels --------------------------------------------
sets <- lapply(seq_len(1000), function(i) sample(ids, 29))
write_outcome(bind_rows(lapply(seq_along(sets), function(i)
  tibble(split = i, SubjID = ids, panel = ifelse(ids %in% sets[[i]], "A", "B")))),
  file.path(OUT, "split_assignments.csv"))

run_split <- function(i) {
  A <- sets[[i]]; B <- setdiff(ids, A)
  ss <- list(A = shares(A), B = shares(B)); pan <- list(A = A, B = B)
  ans <- list(); k <- 0
  for (target in c("A", "B")) for (mode in c("same_half", "disjoint")) {
    src <- if (mode == "same_half") target else setdiff(c("A", "B"), target)
    d <- obs %>%
      filter(as.character(SubjID) %in% pan[[target]]) %>%
      select(-all_of(LAYER_SOURCE)) %>%
      mutate(StimID = as.character(StimID)) %>%
      left_join(ss[[src]], by = "StimID")
    for (y in names(SPECS)) {
      k <- k + 1; term <- tail(SPECS[[y]], 1)
      ans[[k]] <- tryCatch({
        m <- fit(y, SPECS[[y]], d); b <- fixef(m)[term]
        tibble(split = i, target = target, mode = mode, outcome = y, term = term,
               B = unname(b), SE = sqrt(diag(vcov(m)))[term],
               singular = isSingular(m), diagnostic = diag_m(m), error = "")
      }, error = function(e)
        tibble(split = i, target = target, mode = mode, outcome = y, term = term,
               B = NA_real_, SE = NA_real_, singular = NA, diagnostic = "",
               error = conditionMessage(e)))
    }
  }
  bind_rows(ans)
}

# The splits are drawn above under the seed; the fits themselves are
# deterministic, so parallel evaluation does not touch the random stream.
res <- list()
for (start in seq(1, 1000, by = 50)) {
  ix <- start:min(start + 49, 1000)
  res <- c(res, parallel::mclapply(ix, run_split, mc.cores = 3, mc.set.seed = FALSE))
  message("    splits completed: ", max(ix))
}
r <- bind_rows(res)
write_outcome(r, file.path(OUT, "split_coefficients.csv"))

summ <- r %>%
  group_by(outcome, mode) %>%
  summarise(n = n(), failures = sum(!is.finite(B)), singular = sum(singular, na.rm = TRUE),
            median = median(B, na.rm = TRUE),
            lo = quantile(B, .025, na.rm = TRUE), hi = quantile(B, .975, na.rm = TRUE),
            expected_sign_pct = 100 * mean(if_else(outcome == "ISOP", B > 0, B < 0), na.rm = TRUE),
            .groups = "drop")
write_outcome(summ, file.path(OUT, "split_summary.csv"))
print(summ)

paired <- r %>%
  select(split, target, outcome, mode, B) %>%
  pivot_wider(names_from = mode, values_from = B) %>%
  mutate(difference = disjoint - same_half)
write_outcome(paired, file.path(OUT, "split_paired_differences.csv"))

# ---- (2) Compositional form of the source term --------------------------------
# Two independent raw shares: changing the omitted component reparameterises
# the same simplex plane, so the three variants share one AIC.
coeffs <- list(); fits <- list(); tests <- list(); j <- 0
counts <- picks %>%
  count(StimID, class) %>%
  complete(StimID, class = LAYER_SOURCE, fill = list(n = 0)) %>%
  pivot_wider(names_from = class, values_from = n)
write_outcome(counts, file.path(OUT, "composition_counts.csv"))

t_ci <- function(s) tibble(term = rownames(s), B = s[, 1], SE = s[, 2], df = s[, 3],
                           lo = s[, 1] - qt(.975, s[, 3]) * s[, 2],
                           hi = s[, 1] + qt(.975, s[, 3]) * s[, 2])

for (y in names(SPECS)) {
  acoustic <- setdiff(SPECS[[y]], LAYER_SOURCE)
  base <- fit(y, SPECS[[y]], obs, FALSE)
  for (omit in LAYER_SOURCE) {
    xs <- c(acoustic, setdiff(LAYER_SOURCE, omit))
    m <- fit(y, xs, obs, FALSE); j <- j + 1
    fits[[j]] <- tibble(outcome = y, variant = paste0("omit_", omit), AIC = AIC(m),
                        delta_vs_adopted = AIC(m) - AIC(base),
                        singular = isSingular(m), diagnostic = diag_m(m))
    if (omit == "Type3_DCS") {
      # 1-df LRT for the second share (Type 3 as the reference class)
      chisq <- 2 * (as.numeric(logLik(m)) - as.numeric(logLik(base)))
      df <- attr(logLik(m), "df") - attr(logLik(base), "df")
      tests[[y]] <- tibble(outcome = y, chisq = chisq, df = df,
                           p = pchisq(chisq, df, lower.tail = FALSE))
      s <- coef(summary(lmerTest::as_lmerModLmerTest(fit(y, xs, obs, TRUE))))
      coeffs[[paste0(y, "raw")]] <- t_ci(s) %>%
        mutate(outcome = y, variant = "raw_Type3_reference", .before = 1)
    }
  }
  # Isometric log-ratio balances; some shares are zero, so a pseudo-count is
  # added to every class count under three explicit assumptions.
  for (a in c(.1, .5, 1)) {
    z <- counts %>%
      mutate(z1 = sqrt(2 / 3) * log((Type2_SNS + a) / sqrt((Type1_AFS + a) * (Type3_DCS + a))),
             z2 = sqrt(1 / 2) * log((Type1_AFS + a) / (Type3_DCS + a))) %>%
      select(StimID, z1, z2)
    d <- obs %>% mutate(StimID = as.character(StimID)) %>% left_join(z, by = "StimID")
    m <- fit(y, c(acoustic, "z1", "z2"), d, FALSE); j <- j + 1
    fits[[j]] <- tibble(outcome = y, variant = paste0("ilr_count_plus_", a), AIC = AIC(m),
                        delta_vs_adopted = AIC(m) - AIC(base),
                        singular = isSingular(m), diagnostic = diag_m(m))
    s <- coef(summary(lmerTest::as_lmerModLmerTest(fit(y, c(acoustic, "z1", "z2"), d, TRUE))))
    coeffs[[paste0(y, a)]] <- t_ci(s) %>%
      mutate(outcome = y, variant = paste0("ilr_count_plus_", a), .before = 1)
  }
}
write_outcome(bind_rows(tests) %>% mutate(q = p.adjust(p, "BH")),
              file.path(OUT, "second_share_tests.csv"))
write_outcome(bind_rows(fits), file.path(OUT, "composition_models.csv"))
write_outcome(bind_rows(coeffs), file.path(OUT, "composition_coefficients.csv"))
