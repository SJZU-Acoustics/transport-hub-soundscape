# =============================================================================
# run_all.R — reproduce every figure and table of the transport-hub soundscape
# paper from the Mendeley Data workbook.
#
#   Rscript run_all.R
#
# Reads   data/P34_transport_hub_soundscape_data.xlsx
# Writes  output/<analysis id>/  one folder per analysis module
#         output/figures/        Figs. 1-5 and Supplementary Figs. S1-S2 (PNG)
#         output/tables/         Tables 1-3 and Supplementary Tables S1-S11
# =============================================================================

options(warn = 1)
start_time <- Sys.time()

if (!file.exists(file.path("code", "helpers.R"))) {
  stop("Run this script from the repository root: Rscript run_all.R", call. = FALSE)
}

# Analysis modules. Each is self-contained: it reads the deposited tables and
# writes its own result tables, so the order below is for readability only.
MODULES <- c(
  "a01_descriptives",
  "a02_reliability",
  "a03_anova",
  "a04_correlations",
  "a05_null_models",
  "a11_adopted_models",
  "a12_exhaustive_selection",
  "a13_dimension_unpacking",
  "a14_moderation_nonlinearity",
  "a15_predictive_validity"
)

for (module in MODULES) {
  message("\n=== ", module, " ===")
  module_start <- Sys.time()
  # Each module runs in its own environment so no object leaks between them.
  sys.source(file.path("code", paste0(module, ".R")), envir = new.env(parent = globalenv()))
  message("    done in ",
          round(as.numeric(difftime(Sys.time(), module_start, units = "secs")), 1), " s")
}

message("\n=== display items ===")
sys.source(file.path("code", "build_figures.R"), envir = new.env(parent = globalenv()))
sys.source(file.path("code", "build_tables.R"), envir = new.env(parent = globalenv()))

message("\nComplete in ",
        round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 1),
        " min. Figures and table fragments are in output/figures and output/tables.")
