# Data folder

## The Mendeley Data workbook (required)

Download `P34_transport_hub_soundscape_data.xlsx` from Mendeley Data —
**DOI [10.17632/jyzgj67fjb](https://doi.org/10.17632/jyzgj67fjb)** (CC BY 4.0) — and place it in **this folder**:

```
data/P34_transport_hub_soundscape_data.xlsx
```

`run_all.R` reads nothing else. The workbook is not tracked in this repository — it is
archived at Mendeley Data, which is its citable home.

Sheets used by the analysis:

| Sheet | What it holds |
|---|---|
| `observations` | one row per valid listener × recording rating (2,116): the eight ISO/TS 12913-2 attributes, the top-three source picks, the ISO-P/ISO-E coordinates, and the playback-order variables |
| `participants` | all 60 recruited listeners with batch, sex and education codes, validity status and per-listener valid-recording counts |
| `stimuli` | the 36 excerpts: design cells (functional type, level rank), 24 ArtemiS acoustic indicators, recording-level ISO-P/ISO-E means, site metadata and sampling dates |
| `source_counts` | per-recording pick counts for the nine source classes plus "none" |
| `source_shares` | per-recording source shares (%, denominator = picks of the nine classes) and the three-class aggregation |
| `artemis_per_second` | the 1-s ArtemiS series (36 × 30 s) for level, loudness, sharpness, roughness, fluctuation strength, tonality and articulation index |

The workbook also carries a README sheet, a sheet summary and two dictionary sheets
(`dict_variables`, `dict_codes`) that define every column and code.

`artemis_per_second` is the series from which the recording-level percentile and range
indicators in `stimuli` were derived. It is archived for reuse; no display item in the
paper is computed from it here.

The 36 audio excerpts themselves are not part of the deposit.
