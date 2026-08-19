# =============================================================================
# load_data.R — single data entry for the transport-hub-soundscape release.
#
# Data source: the Mendeley Data workbook
#   data/P34_transport_hub_soundscape_data.xlsx  (CC BY 4.0)
#
# Sheets are read with col_types = "text" at the XLSX boundary and re-typed
# through a CSV round-trip, so readr infers exactly the types the working
# pipeline's frozen CSVs carried and both "NA" and "" map to NA.
#
# code/helpers.R reads every analysis table through read_frozen_csv(), which
# names the original frozen CSV; the mapping below resolves each name to its
# workbook sheet. Nothing else in the repository touches the workbook.
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(readr)
})

XLSX_PATH <- file.path("data", "P34_transport_hub_soundscape_data.xlsx")
CACHE_DIR <- file.path("output", "_workbook_cache")

# frozen-CSV name -> workbook sheet
SHEET_FOR_FILE <- c(
  "observations.csv"        = "observations",
  "participants.csv"        = "participants",
  "stimuli.csv"             = "stimuli",
  "artemis_per_second.csv"  = "artemis_per_second",
  "source_counts.csv"       = "source_counts",
  "source_shares.csv"       = "source_shares"
)

read_workbook_sheet <- function(sheet) {
  if (!file.exists(XLSX_PATH)) {
    stop("Workbook not found at ", XLSX_PATH, "\n",
         "Download it from Mendeley Data and place it in data/ — see README.",
         call. = FALSE)
  }
  dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
  cached <- file.path(CACHE_DIR, paste0(sheet, ".csv"))
  if (!file.exists(cached)) {
    raw <- readxl::read_excel(XLSX_PATH, sheet = sheet, col_types = "text")
    readr::write_csv(raw, cached, na = "")
  }
  readr::read_csv(cached, show_col_types = FALSE, progress = FALSE)
}

#' Read a canonical frozen table by its original file name.
read_frozen_csv <- function(filename) {
  sheet <- SHEET_FOR_FILE[[filename]]
  if (is.null(sheet)) {
    stop("No workbook sheet is registered for ", filename, call. = FALSE)
  }
  read_workbook_sheet(sheet)
}
