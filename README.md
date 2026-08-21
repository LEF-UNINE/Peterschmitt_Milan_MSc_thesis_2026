Milan_Peterschmitt_MSc_thesis_2026

**Repository description (for GitHub):**  
Milan Peterschmitt, supervised by Sergio Rasmann (2025-2026, UniNE). Thesis on how land use, habitat structures and climate shape micromammal abundance in the Swiss Alps, using Faune Concept capture data and species-level models on 11 species.

1. Overview


**Micromammals and mountain habitats: Effects of land use, local habitat structures and climatic gradients on species abundance**

**Author:** Milan Peterschmitt  
**Supervisor:** Sergio Rasmann  
**Institution:** University of Neuchâtel (UniNE), Master in Biodiversity Conservation  
**Academic year:** 2025-2026

The thesis investigates how dominant land-use typologies, local habitat structures, their combinations, and climatic gradients are associated with micromammal abundance in Swiss mountain landscapes. The analyses are species-specific and use negative-binomial generalized linear models, with Poisson fallback when required. Climate covariates (BIO1, BIO2, BIO5, BIO12 and PCA1) are added separately to evaluate the robustness of habitat associations.

The field data were collected by **Faune Concept** during surveys in the Swiss Alps and were made available for this thesis. The data remain the property of Faune Concept.

2. Script information

- R 4.4.3
- RStudio 2026.01.2+418

Main R packages

`dplyr`, `tidyr`, `stringr`, `purrr`, `tibble`, `MASS`, `writexl`, `ggplot2`, `forcats`, `scales`, `sf`, `rnaturalearth`, `rnaturalearthdata`, `sessioninfo`.

What the script does

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


Variable dictionary

| `spece` | Factor | Species identification.
| `nombre_total` | Continuous/count | Number of individuals represented by the record (individuals).
| `coord_x` | X coordinate used to define sampling stations;
| `coord_y` | Y coordinate used to define sampling stations 
| `altitude` | Continuous | Sampling altitude (m above sea level).
| `annee` | Survey year. |
| `methode` | capture method as recorded in the source dataset. Main analyses retain live-trapping methods containing `PVIV`. |
| `typologie` | Dominant habitat typology surrounding the station.
| `structure_1` | First local habitat structure recorded at the station.
| `structure_2` | Second local habitat structure recorded at the station. It is treated as interchangeable and non-exclusive with `structure_1` in the analysis. |
| `bio1` | BIO1: mean annual temperature, stored exactly in the units/scaling of the source climate extraction used for the thesis.
| `bio12` | BIO12: annual precipitation, stored exactly in the units/scaling of the source climate extraction used for the thesis.
| `bio2` | BIO2: mean diurnal temperature range, stored exactly in the units/scaling of the source climate extraction used for the thesis.
| `bio5` | BIO5: maximum temperature of the warmest month, stored exactly in the units/scaling of the source climate extraction used for the thesis. 
| `pca1` | First principal component axis summarizing the multivariate climatic variables.

Species

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

Typology (9 levels)

- `Milieux forestiers (conifères)`
- `Milieux forestiers (feuillus)`
- `Milieux forestiers (mixtes)`
- `Milieux forestiers ouverts / perturbés (conifères)`
- `Milieux forestiers ouverts / perturbés (feuillus)`
- `Milieux humides (Bas marais)`
- `Milieux humides (Hauts marais)`
- `Milieux humides (prairies humides)`
- `Prairies`

Structures (`structure_1` and `structure_2`, 14 unique levels combined)

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

Data provenance and ownership

Field collection and initial identification of individuals were performed by Faune Concept. The data were not collected specifically for this MSc thesis; they were provided as part of the collaboration with Faune Concept. The data remain the property of Faune Concept.
