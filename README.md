# Milan_Peterschmitt_MSc_thesis_2026

**Repository description (for GitHub):**  
Milan Peterschmitt, supervised by Sergio Rasmann (2025-2026, UniNE). Thesis on how land use, habitat structures and climate shape micromammal abundance in the Swiss Alps, using Faune Concept capture data and species-level models on 11 species.

## 1. Overview

This repository contains the files needed to archive and reproduce the analyses from the MSc thesis:

**Micromammals and mountain habitats: Effects of land use, local habitat structures and climatic gradients on species abundance**

**Author:** Milan Peterschmitt  
**Supervisor:** Sergio Rasmann  
**Institution:** University of Neuchâtel (UniNE), Master in Biodiversity Conservation  
**Academic year:** 2025-2026

The thesis investigates how dominant land-use typologies, local habitat structures, their combinations, and climatic gradients are associated with micromammal abundance in Swiss mountain landscapes. The analyses are species-specific and use negative-binomial generalized linear models, with Poisson fallback when required. Climate covariates (BIO1, BIO2, BIO5, BIO12 and PCA1) are added separately to evaluate the robustness of habitat associations.

The field data were collected by **Faune Concept** during surveys in the Swiss Alps and were made available for this thesis. The data remain the property of Faune Concept. The archive CSV is an **analysis-ready subset** of the source workbook containing only the variables required by the R script. Keep the original, untouched Excel workbook separately as the source backup.

### Files in this repository

- `Peterschmitt_Milan_micromammals_analysis_data.csv` - analysis-ready data used by the R script.
- `Peterschmitt_Milan_analysis.R` - complete R workflow used for data preparation, harmonization, statistical analyses, second hierarchical passes, tables and figures.
- `Peterschmitt_Milan_MasterThesis_UniNE_2026.pdf` - PDF version of the MSc thesis.

### Important data-permission note

The LEF instructions request a **public** repository, but the source capture data belong to Faune Concept. Before making the repository public, confirm with Sergio Rasmann / Faune Concept that public redistribution of `Peterschmitt_Milan_micromammals_analysis_data.csv` is authorized. If authorization is not granted, do not publish the CSV and use a data-availability statement explaining that the capture data remain the property of Faune Concept and cannot be publicly redistributed.

---

## 2. Script information

### Software

- R 4.4.3
- RStudio 2026.01.2+418

### Main R packages

`dplyr`, `tidyr`, `stringr`, `purrr`, `tibble`, `MASS`, `writexl`, `ggplot2`, `forcats`, `scales`, `sf`, `rnaturalearth`, `rnaturalearthdata`, `sessioninfo`.

### What the script does

1. Reads the archived CSV file.
2. Cleans text fields and constructs the station-level analysis unit from GPS coordinates.
3. Harmonizes fine habitat typologies and structures into analytical categories.
4. Treats `structure_1` and `structure_2` as interchangeable and non-exclusive.
5. Calculates species abundance per station and inserts zero abundance for stations where a species was not captured.
6. Selects the three most frequently occupied habitat categories for each species at the typology, structure, and typology x structure levels.
7. Fits negative-binomial GLMs with a log link; if the negative-binomial model cannot be fitted, a Poisson model is used.
8. Fits an unadjusted model and five separate climate-adjusted models using BIO1, BIO2, BIO5, BIO12, or PCA1.
9. Applies Benjamini-Hochberg adjustment and reports rate ratios with 95% Wald confidence intervals.
10. Performs targeted second hierarchical passes for selected broad categories.
11. Exports analysis tables, summaries, an Excel workbook, and thesis figures.
12. Prints and saves `sessioninfo::session_info()`.

### How to run

Place the `.R` file and the `.csv` file in the same folder. Open `Peterschmitt_Milan_analysis.R` in RStudio, set the working directory to the repository folder, and run the script from top to bottom.

The script creates the folder:

`sorties_analyse_micro_UNINE_script_complet/`

with the statistical outputs, tables and figures.

**Before final archiving:** run the script once in the final thesis R/RStudio environment and paste the printed `sessioninfo::session_info()` output into the commented block at the end of the R script, as requested by the LEF guideline.

---

## 3. Data frame information

### General information

The CSV contains 2,724 source records and 17 variables needed to reproduce the analyses. One malformed source row (Excel row 217; `coord_x = 73916`, `coord_y = 155522`) was excluded from the archive because it was the only record with a five-digit X coordinate and lacked method, locality, canton and climate data. After the R script applies the comparable live-trapping filter (`PVIV`), the analysis dataset contains **2,684 records and 504 stations**, matching the thesis. Records span **2014-2025**.

Missing values are encoded as `NA`. The CSV is UTF-8 encoded and uses `.` as the decimal separator.

### Variable dictionary

| Variable | Type | Description / units |
|---|---|---|
| `espece` | Factor | Species identification. Full scientific name is included in each level, with the French common name in parentheses. |
| `nombre_total` | Continuous/count | Number of individuals represented by the record (individuals). If missing for a non-missing species record, the R script replaces it with 1. |
| `coord_x` | Continuous | Swiss projected X coordinate used to define sampling stations; coordinate reference system used in the mapping code: EPSG:21781. |
| `coord_y` | Continuous | Swiss projected Y coordinate used to define sampling stations; coordinate reference system used in the mapping code: EPSG:21781. |
| `altitude` | Continuous | Sampling altitude (m above sea level). |
| `annee` | Integer | Survey year. |
| `methode` | Factor | Field detection / capture method as recorded in the source dataset. Main analyses retain live-trapping methods containing `PVIV`. |
| `typologie` | Factor | Dominant habitat typology surrounding the station. |
| `structure_1` | Factor | First local habitat structure recorded at the station. |
| `structure_2` | Factor | Second local habitat structure recorded at the station. It is treated as interchangeable and non-exclusive with `structure_1` in the analysis. |
| `localite` | Factor/text | Locality associated with the sampling station. |
| `canton` | Factor | Swiss canton code as recorded in the source dataset. |
| `bio1` | Continuous | BIO1: mean annual temperature, stored exactly in the units/scaling of the source climate extraction used for the thesis. |
| `bio12` | Continuous | BIO12: annual precipitation, stored exactly in the units/scaling of the source climate extraction used for the thesis. |
| `bio2` | Continuous | BIO2: mean diurnal temperature range, stored exactly in the units/scaling of the source climate extraction used for the thesis. |
| `bio5` | Continuous | BIO5: maximum temperature of the warmest month, stored exactly in the units/scaling of the source climate extraction used for the thesis. |
| `pca1` | Continuous | First principal component axis summarizing the multivariate climatic variables; dimensionless PCA score. |

### Factor levels present in the archived data

**Species (11 levels)**

- `Apodemus alpicola (Mulot alpestre)`
- `Apodemus flavicollis (Mulot à collier)`
- `Apodemus sylvaticus (Mulot sylvestre)`
- `Chionomys nivalis (Campagnol des neiges)`
- `Clethrionomys glareolus (Campagnol roussâtre)`
- `Microtus agrestis (Campagnol agreste)`
- `Microtus arvalis (Campagnol des champs)`
- `Microtus lavernedii (Campagnol de Lavernède)`
- `Neomys fodiens (Musaraigne aquatique)`
- `Sorex antinorii (Musaraigne du Valais)`
- `Sorex araneus (Musaraigne carrelet)`

**Method (5 levels)**

- `Lebendfalle : PVIV`
- `Piège photo : PPHO`
- `Pièges à capturer vivant : PVIV`
- `Sichtbeobachtung : CVUE`
- `déterminé génétiquement : DGENE`

**Typology (9 levels)**

- `Milieux forestiers (conifères)`
- `Milieux forestiers (feuillus)`
- `Milieux forestiers (mixtes)`
- `Milieux forestiers ouverts / perturbés (conifères)`
- `Milieux forestiers ouverts / perturbés (feuillus)`
- `Milieux humides (Bas marais)`
- `Milieux humides (Hauts marais)`
- `Milieux humides (prairies humides)`
- `Prairies`

**Structures (`structure_1` and `structure_2`, 14 unique levels combined)**

- `Boisements isolés`
- `Broussailles / formations arbustives`
- `Lisières forestières (conifères)`
- `Lisières forestières (feuillus)`
- `Lisières forestières (mixtes)`
- `Milieux anthopiques batis`
- `Milieux rocheux anthropiques`
- `Milieux rocheux naturels`
- `Ruisseau (avec végétation)`
- `Ruisseau (sans végétation)`
- `Ruisseau avec végétation`
- `Structures linéaires arborées dans les paysages agricoles`
- `Torrent (avec végétation)`
- `Torrent (sans végétation)`

**Canton (8 recorded levels after trimming whitespace)**

- `BE`
- `GR`
- `OB`
- `OW`
- `SZ`
- `TI`
- `UR`
- `VS`

---

## Data provenance and ownership

Field collection and initial identification of individuals were performed by Faune Concept. The data were not collected specifically for this MSc thesis; they were provided as part of the collaboration with Faune Concept. The data remain the property of Faune Concept.

The original source workbook should be retained unchanged as a backup, even if it is not uploaded to the public repository.
