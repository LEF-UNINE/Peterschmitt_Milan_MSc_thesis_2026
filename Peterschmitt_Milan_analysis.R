
# ============================================================
# UNINE - Micromammiferes
#
# Software used for the final thesis analyses:
# - R 4.4.3
# - RStudio 2026.01.2+418
# Script complet et tracable pour RStudio
#
# Ce script :
# 1) lit le fichier CSV archive utilise pour reproduire les analyses
# 2) construit l'unite d'analyse = station GPS
# 3) harmonise les categories
# 4) conserve un niveau fin + un niveau analytique harmonise
# 5) traite STRUCTURE et STRUCTURE 2 comme interchangeables
# 6) calcule les TOP 3 categories les plus frequentes
# 7) teste leur association avec l'abondance par station
#    - modele brut
#    - modeles ajustes separement avec bio1, bio2, bio5, bio12, PCA1
# 8) exporte les sorties CSV + un classeur Excel complet
#
# IMPORTANT :
# - seuil de significativite = 0.05 sur les p-values corrigees BH
# - l'analyse principale se fait sur le niveau harmonise
# - le niveau fin est conserve pour la tracabilite et l'interpretation
# ============================================================

# -----------------------------
# 0) Parametres utilisateur
# -----------------------------
input_file  <- "Peterschmitt_Milan_micromammals_analysis_data.csv"
output_dir  <- "sorties_analyse_micro_UNINE_script_complet"

alpha_level <- 0.05
top_n <- 3

# Seuils minimaux pour tester une modalite
min_stations_modalite <- 5
min_stations_hors_modalite <- 5
min_stations_occupees_espece <- 5

# -----------------------------
# 1) Packages
# -----------------------------
packages <- c(
  "dplyr", "tidyr", "stringr", "purrr",
  "tibble", "MASS", "writexl",
  "ggplot2", "forcats", "scales", "sf", "rnaturalearth", "rnaturalearthdata",
  "sessioninfo"
)

to_install <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

invisible(lapply(packages, library, character.only = TRUE))

# -----------------------------
# 2) Fonctions utilitaires
# -----------------------------
clean_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, fixed("\u00A0"), " ")
  x <- stringr::str_replace_all(x, "\\s+", " ")
  x <- stringr::str_trim(x)
  x[x %in% c("", "NA", "NaN", "NULL")] <- NA_character_
  x
}

to_ascii_lower <- function(x) {
  out <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  out <- tolower(out)
  out
}

fmt_coord <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(
    is.na(x),
    NA_character_,
    sub("\\.0+$", "", format(x, scientific = FALSE, trim = TRUE))
  )
}

mode_na <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

safe_exp <- function(x) {
  ifelse(is.na(x), NA_real_, exp(x))
}

interpret_effect <- function(rate_ratio, q_value, alpha = 0.05) {
  dplyr::case_when(
    is.na(q_value) ~ "Non testable",
    q_value < alpha & rate_ratio > 1 ~ "Association positive avec l'abondance",
    q_value < alpha & rate_ratio < 1 ~ "Association negative avec l'abondance",
    TRUE ~ "Pas d'effet statistiquement detecte"
  )
}

# -----------------------------
# 3) Harmonisation des categories
# -----------------------------
harmonise_typologie_fine <- function(x) {
  x <- clean_text(x)

  dplyr::recode(
    x,
    "Milieux forestiers ouverts  / perturbés (feuillus)" = "Milieux forestiers ouverts / perturbés (feuillus)",
    "Milieux humide (Bas-marais)" = "Milieux humides (Bas marais)",
    "Milieux humide (Bas-marais" = "Milieux humides (Bas marais)",
    "Milieux humide Bas-marais)" = "Milieux humides (Bas marais)",
    "Milieux humide (Haut-marais)" = "Milieux humides (Hauts marais)",
    "Milieux humide (Prairies humides)" = "Milieux humides (prairies humides)",
    .default = x
  )
}

harmonise_structure_fine <- function(x) {
  x <- clean_text(x)

  dplyr::recode(
    x,
    "Ruisseau avec végétation" = "Ruisseau (avec végétation)",
    "Ruisseau avec vegetation" = "Ruisseau (avec végétation)",
    "Ruisseau (avec vegetation)" = "Ruisseau (avec végétation)",
    "Ruisseau (sans vegetation)" = "Ruisseau (sans végétation)",
    "Torrent (avec vegetation)" = "Torrent (avec végétation)",
    "Torrent (sans vegetation)" = "Torrent (sans végétation)",
    "Milieux anthopiques batis" = "Milieux anthropiques bâtis",
    "Milieux anthropiques batis" = "Milieux anthropiques bâtis",
    "Lisières forestières (mixtes) " = "Lisières forestières (mixtes)",
    .default = x
  )
}

typologie_analytique <- function(x) {
  x <- harmonise_typologie_fine(x)

  dplyr::case_when(
    x %in% c(
      "Milieux forestiers (conifères)",
      "Milieux forestiers (feuillus)",
      "Milieux forestiers (mixtes)",
      "Milieux forestiers ouverts / perturbés (feuillus)"
    ) ~ "Milieux forestiers",

    x %in% c(
      "Milieux humides (Bas marais)",
      "Milieux humides (Hauts marais)",
      "Milieux humides (prairies humides)"
    ) ~ "Milieux humides",

    TRUE ~ x
  )
}

structure_analytique <- function(x) {
  x <- harmonise_structure_fine(x)

  dplyr::case_when(
    x %in% c(
      "Lisières forestières (feuillus)",
      "Lisières forestières (conifères)",
      "Lisières forestières (mixtes)"
    ) ~ "Lisières forestières",

    x %in% c(
      "Ruisseau (avec végétation)",
      "Torrent (avec végétation)"
    ) ~ "Milieux aquatiques avec végétation",

    x %in% c(
      "Ruisseau (sans végétation)",
      "Torrent (sans végétation)"
    ) ~ "Milieux aquatiques sans végétation",

    x %in% c(
      "Milieux rocheux anthropiques",
      "Milieux rocheux naturels"
    ) ~ "Milieux rocheux",

    x %in% c(
      "Broussailles / formations arbustives",
      "Structures linéaires arborées dans les paysages agricoles"
    ) ~ "Structures ligneuses semi-ouvertes",

    TRUE ~ x
  )
}

structure_niveau_aquatique <- function(x) {
  x <- structure_analytique(x)

  dplyr::case_when(
    x %in% c(
      "Milieux aquatiques avec végétation",
      "Milieux aquatiques sans végétation"
    ) ~ "Milieux aquatiques",
    TRUE ~ x
  )
}

# -----------------------------
# 4) Modeles
# -----------------------------
fit_nb_or_poisson <- function(formula_obj, data_in) {
  mod_nb <- tryCatch(
    suppressWarnings(MASS::glm.nb(formula_obj, data = data_in)),
    error = function(e) NULL
  )

  if (!is.null(mod_nb)) {
    return(list(model = mod_nb, family_used = "negative_binomial"))
  }

  mod_pois <- tryCatch(
    suppressWarnings(stats::glm(formula_obj, family = stats::poisson(link = "log"), data = data_in)),
    error = function(e) NULL
  )

  if (!is.null(mod_pois)) {
    return(list(model = mod_pois, family_used = "poisson_fallback"))
  }

  list(model = NULL, family_used = "non_testable")
}

fit_indicator_model <- function(df_species, indicator_var = "indicateur", covar_var = NULL) {

  df_model <- df_species

  if (!is.null(covar_var)) {
    df_model <- df_model %>% dplyr::filter(!is.na(.data[[covar_var]]))
  }

  n_in  <- sum(df_model[[indicator_var]] == 1, na.rm = TRUE)
  n_out <- sum(df_model[[indicator_var]] == 0, na.rm = TRUE)
  n_occ <- sum(df_model$abondance > 0, na.rm = TRUE)

  mean_in  <- mean(df_model$abondance[df_model[[indicator_var]] == 1], na.rm = TRUE)
  mean_out <- mean(df_model$abondance[df_model[[indicator_var]] == 0], na.rm = TRUE)

  if (length(unique(df_model[[indicator_var]])) < 2 ||
      n_in < min_stations_modalite ||
      n_out < min_stations_hors_modalite ||
      n_occ < min_stations_occupees_espece ||
      nrow(df_model) < (min_stations_modalite + min_stations_hors_modalite)) {

    return(tibble::tibble(
      modele = "non_testable",
      n_stations_modalite = n_in,
      n_stations_hors_modalite = n_out,
      n_stations_occupees_espece = n_occ,
      abondance_moyenne_modalite = mean_in,
      abondance_moyenne_hors_modalite = mean_out,
      estimate = NA_real_,
      std.error = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      rate_ratio = NA_real_,
      rate_ratio_low = NA_real_,
      rate_ratio_high = NA_real_,
      p_value = NA_real_
    ))
  }

  if (is.null(covar_var)) {
    formula_obj <- stats::as.formula(paste0("abondance ~ ", indicator_var))
  } else {
    formula_obj <- stats::as.formula(paste0("abondance ~ ", indicator_var, " + ", covar_var))
  }

  fitted <- fit_nb_or_poisson(formula_obj, df_model)

  if (is.null(fitted$model)) {
    return(tibble::tibble(
      modele = "non_testable",
      n_stations_modalite = n_in,
      n_stations_hors_modalite = n_out,
      n_stations_occupees_espece = n_occ,
      abondance_moyenne_modalite = mean_in,
      abondance_moyenne_hors_modalite = mean_out,
      estimate = NA_real_,
      std.error = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      rate_ratio = NA_real_,
      rate_ratio_low = NA_real_,
      rate_ratio_high = NA_real_,
      p_value = NA_real_
    ))
  }

  coef_tab <- tryCatch(
    summary(fitted$model)$coefficients,
    error = function(e) NULL
  )

  if (is.null(coef_tab) || !(indicator_var %in% rownames(coef_tab))) {
    return(tibble::tibble(
      modele = "non_testable",
      n_stations_modalite = n_in,
      n_stations_hors_modalite = n_out,
      n_stations_occupees_espece = n_occ,
      abondance_moyenne_modalite = mean_in,
      abondance_moyenne_hors_modalite = mean_out,
      estimate = NA_real_,
      std.error = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      rate_ratio = NA_real_,
      rate_ratio_low = NA_real_,
      rate_ratio_high = NA_real_,
      p_value = NA_real_
    ))
  }

  est <- unname(coef_tab[indicator_var, "Estimate"])
  se  <- unname(coef_tab[indicator_var, "Std. Error"])

  p_col <- intersect(colnames(coef_tab), c("Pr(>|z|)", "Pr(>|t|)"))
  p_val <- if (length(p_col) == 1) unname(coef_tab[indicator_var, p_col]) else NA_real_

  conf_low  <- est - 1.96 * se
  conf_high <- est + 1.96 * se

  tibble::tibble(
    modele = fitted$family_used,
    n_stations_modalite = n_in,
    n_stations_hors_modalite = n_out,
    n_stations_occupees_espece = n_occ,
    abondance_moyenne_modalite = mean_in,
    abondance_moyenne_hors_modalite = mean_out,
    estimate = est,
    std.error = se,
    conf.low = conf_low,
    conf.high = conf_high,
    rate_ratio = safe_exp(est),
    rate_ratio_low = safe_exp(conf_low),
    rate_ratio_high = safe_exp(conf_high),
    p_value = p_val
  )
}

# -----------------------------
# 5) Import du fichier archive (.csv)
# -----------------------------
# Note: the archive CSV excludes one malformed source row with an invalid
# five-digit X coordinate (see README.md for full traceability).
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

raw <- utils::read.csv(
  input_file,
  na.strings = "NA",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

dat <- raw %>%
  dplyr::mutate(
    espece = clean_text(espece),
    typologie_fine = harmonise_typologie_fine(typologie),
    typologie_analysee = typologie_analytique(typologie),
    structure_1_fine = harmonise_structure_fine(structure_1),
    structure_2_fine = harmonise_structure_fine(structure_2),
    structure_1_analysee = structure_analytique(structure_1),
    structure_2_analysee = structure_analytique(structure_2),
    structure_1_niveau_aquatique = structure_niveau_aquatique(structure_1),
    structure_2_niveau_aquatique = structure_niveau_aquatique(structure_2),
    methode = clean_text(methode),
    localite = clean_text(localite),
    canton = clean_text(canton),
    coord_x_chr = fmt_coord(coord_x),
    coord_y_chr = fmt_coord(coord_y),
    altitude = suppressWarnings(as.numeric(altitude)),
    annee = suppressWarnings(as.integer(annee)),
    bio1 = suppressWarnings(as.numeric(bio1)),
    bio2 = suppressWarnings(as.numeric(bio2)),
    bio5 = suppressWarnings(as.numeric(bio5)),
    bio12 = suppressWarnings(as.numeric(bio12)),
    pca1 = suppressWarnings(as.numeric(pca1)),
    nombre_total = suppressWarnings(as.numeric(nombre_total)),
    nombre_total = dplyr::if_else(!is.na(espece) & is.na(nombre_total), 1, nombre_total),
    methode_ascii = to_ascii_lower(methode),
    capture_comparable = is.na(methode) |
      stringr::str_detect(methode_ascii, "pviv|lebendfalle|pieges a capturer vivant"),
    station_id = paste(coord_x_chr, coord_y_chr, sep = "_")
  ) %>%
  dplyr::filter(
    !is.na(espece),
    !is.na(coord_x_chr),
    !is.na(coord_y_chr),
    capture_comparable
  )

# -----------------------------
# 6) Table station
# -----------------------------
stations <- dat %>%
  dplyr::group_by(station_id) %>%
  dplyr::summarise(
    coord_x = dplyr::first(coord_x_chr),
    coord_y = dplyr::first(coord_y_chr),
    altitude = suppressWarnings(mean(altitude, na.rm = TRUE)),
    localite = mode_na(localite),
    canton = mode_na(canton),
    typologie_fine = mode_na(typologie_fine),
    typologie_analysee = mode_na(typologie_analysee),
    structure_1_fine = mode_na(structure_1_fine),
    structure_2_fine = mode_na(structure_2_fine),
    structure_1_analysee = mode_na(structure_1_analysee),
    structure_2_analysee = mode_na(structure_2_analysee),
    structure_1_niveau_aquatique = mode_na(structure_1_niveau_aquatique),
    structure_2_niveau_aquatique = mode_na(structure_2_niveau_aquatique),
    bio1 = suppressWarnings(mean(bio1, na.rm = TRUE)),
    bio2 = suppressWarnings(mean(bio2, na.rm = TRUE)),
    bio5 = suppressWarnings(mean(bio5, na.rm = TRUE)),
    bio12 = suppressWarnings(mean(bio12, na.rm = TRUE)),
    pca1 = suppressWarnings(mean(pca1, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    altitude = ifelse(is.nan(altitude), NA_real_, altitude),
    bio1 = ifelse(is.nan(bio1), NA_real_, bio1),
    bio2 = ifelse(is.nan(bio2), NA_real_, bio2),
    bio5 = ifelse(is.nan(bio5), NA_real_, bio5),
    bio12 = ifelse(is.nan(bio12), NA_real_, bio12),
    pca1 = ifelse(is.nan(pca1), NA_real_, pca1)
  )

# Structures non exclusives
stations_structures_fines <- stations %>%
  dplyr::select(station_id, typologie_fine, structure_1_fine, structure_2_fine) %>%
  tidyr::pivot_longer(
    cols = c(structure_1_fine, structure_2_fine),
    names_to = "rang_structure",
    values_to = "structure_fine"
  ) %>%
  dplyr::filter(!is.na(structure_fine), structure_fine != "") %>%
  dplyr::distinct(station_id, typologie_fine, structure_fine)

stations_structures_analytiques <- stations %>%
  dplyr::select(station_id, typologie_analysee, structure_1_analysee, structure_2_analysee) %>%
  tidyr::pivot_longer(
    cols = c(structure_1_analysee, structure_2_analysee),
    names_to = "rang_structure",
    values_to = "structure_analysee"
  ) %>%
  dplyr::filter(!is.na(structure_analysee), structure_analysee != "") %>%
  dplyr::distinct(station_id, typologie_analysee, structure_analysee)

stations_structures_niveau_aquatique <- stations %>%
  dplyr::select(station_id, typologie_analysee, structure_1_niveau_aquatique, structure_2_niveau_aquatique) %>%
  tidyr::pivot_longer(
    cols = c(structure_1_niveau_aquatique, structure_2_niveau_aquatique),
    names_to = "rang_structure",
    values_to = "structure_niveau_aquatique"
  ) %>%
  dplyr::filter(!is.na(structure_niveau_aquatique), structure_niveau_aquatique != "") %>%
  dplyr::distinct(station_id, typologie_analysee, structure_niveau_aquatique)

# Combinaisons
stations_combos_fins <- stations_structures_fines %>%
  dplyr::mutate(combinaison_fine = paste(typologie_fine, structure_fine, sep = " x "))

stations_combos_analytiques <- stations_structures_analytiques %>%
  dplyr::mutate(combinaison_analysee = paste(typologie_analysee, structure_analysee, sep = " x "))

stations_combos_niveau_aquatique <- stations_structures_niveau_aquatique %>%
  dplyr::mutate(combinaison_niveau_aquatique = paste(typologie_analysee, structure_niveau_aquatique, sep = " x "))

# -----------------------------
# 7) Abondance espece x station
# -----------------------------
abondance_station <- dat %>%
  dplyr::group_by(espece, station_id) %>%
  dplyr::summarise(
    abondance = sum(nombre_total, na.rm = TRUE),
    .groups = "drop"
  )

grille_complete <- tidyr::crossing(
  espece = sort(unique(dat$espece)),
  station_id = sort(unique(stations$station_id))
) %>%
  dplyr::left_join(stations, by = "station_id") %>%
  dplyr::left_join(abondance_station, by = c("espece", "station_id")) %>%
  dplyr::mutate(abondance = tidyr::replace_na(abondance, 0))

# -----------------------------
# 8) Tables descriptives TOP 3
# -----------------------------
top_typologies_fines <- grille_complete %>%
  dplyr::filter(abondance > 0, !is.na(typologie_fine)) %>%
  dplyr::group_by(espece, typologie_fine) %>%
  dplyr::summarise(
    n_stations_occupees = dplyr::n_distinct(station_id),
    abondance_totale_dans_modalite = sum(abondance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(espece, dplyr::desc(n_stations_occupees), dplyr::desc(abondance_totale_dans_modalite), typologie_fine) %>%
  dplyr::group_by(espece) %>%
  dplyr::mutate(rang = dplyr::row_number()) %>%
  dplyr::slice_head(n = top_n) %>%
  dplyr::ungroup()

top_typologies_analytiques <- grille_complete %>%
  dplyr::filter(abondance > 0, !is.na(typologie_analysee)) %>%
  dplyr::group_by(espece, typologie_analysee) %>%
  dplyr::summarise(
    n_stations_occupees = dplyr::n_distinct(station_id),
    abondance_totale_dans_modalite = sum(abondance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(espece, dplyr::desc(n_stations_occupees), dplyr::desc(abondance_totale_dans_modalite), typologie_analysee) %>%
  dplyr::group_by(espece) %>%
  dplyr::mutate(rang = dplyr::row_number()) %>%
  dplyr::slice_head(n = top_n) %>%
  dplyr::ungroup()

top_structures_fines <- grille_complete %>%
  dplyr::filter(abondance > 0) %>%
  dplyr::select(espece, station_id, abondance) %>%
  dplyr::inner_join(stations_structures_fines, by = "station_id") %>%
  dplyr::distinct(espece, station_id, structure_fine, abondance) %>%
  dplyr::group_by(espece, structure_fine) %>%
  dplyr::summarise(
    n_stations_occupees = dplyr::n_distinct(station_id),
    abondance_totale_dans_modalite = sum(abondance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(espece, dplyr::desc(n_stations_occupees), dplyr::desc(abondance_totale_dans_modalite), structure_fine) %>%
  dplyr::group_by(espece) %>%
  dplyr::mutate(rang = dplyr::row_number()) %>%
  dplyr::slice_head(n = top_n) %>%
  dplyr::ungroup()

top_structures_analytiques <- grille_complete %>%
  dplyr::filter(abondance > 0) %>%
  dplyr::select(espece, station_id, abondance) %>%
  dplyr::inner_join(stations_structures_analytiques, by = "station_id") %>%
  dplyr::distinct(espece, station_id, structure_analysee, abondance) %>%
  dplyr::group_by(espece, structure_analysee) %>%
  dplyr::summarise(
    n_stations_occupees = dplyr::n_distinct(station_id),
    abondance_totale_dans_modalite = sum(abondance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(espece, dplyr::desc(n_stations_occupees), dplyr::desc(abondance_totale_dans_modalite), structure_analysee) %>%
  dplyr::group_by(espece) %>%
  dplyr::mutate(rang = dplyr::row_number()) %>%
  dplyr::slice_head(n = top_n) %>%
  dplyr::ungroup()

top_structures_niveau_aquatique <- grille_complete %>%
  dplyr::filter(abondance > 0) %>%
  dplyr::select(espece, station_id, abondance) %>%
  dplyr::inner_join(stations_structures_niveau_aquatique, by = "station_id") %>%
  dplyr::distinct(espece, station_id, structure_niveau_aquatique, abondance) %>%
  dplyr::group_by(espece, structure_niveau_aquatique) %>%
  dplyr::summarise(
    n_stations_occupees = dplyr::n_distinct(station_id),
    abondance_totale_dans_modalite = sum(abondance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(espece, dplyr::desc(n_stations_occupees), dplyr::desc(abondance_totale_dans_modalite), structure_niveau_aquatique) %>%
  dplyr::group_by(espece) %>%
  dplyr::mutate(rang = dplyr::row_number()) %>%
  dplyr::slice_head(n = top_n) %>%
  dplyr::ungroup()

top_combos_fins <- grille_complete %>%
  dplyr::filter(abondance > 0) %>%
  dplyr::select(espece, station_id, abondance) %>%
  dplyr::inner_join(stations_combos_fins, by = "station_id") %>%
  dplyr::distinct(espece, station_id, combinaison_fine, typologie_fine, structure_fine, abondance) %>%
  dplyr::group_by(espece, combinaison_fine, typologie_fine, structure_fine) %>%
  dplyr::summarise(
    n_stations_occupees = dplyr::n_distinct(station_id),
    abondance_totale_dans_modalite = sum(abondance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(espece, dplyr::desc(n_stations_occupees), dplyr::desc(abondance_totale_dans_modalite), combinaison_fine) %>%
  dplyr::group_by(espece) %>%
  dplyr::mutate(rang = dplyr::row_number()) %>%
  dplyr::slice_head(n = top_n) %>%
  dplyr::ungroup()

top_combos_analytiques <- grille_complete %>%
  dplyr::filter(abondance > 0) %>%
  dplyr::select(espece, station_id, abondance) %>%
  dplyr::inner_join(stations_combos_analytiques, by = "station_id") %>%
  dplyr::distinct(espece, station_id, combinaison_analysee, typologie_analysee, structure_analysee, abondance) %>%
  dplyr::group_by(espece, combinaison_analysee, typologie_analysee, structure_analysee) %>%
  dplyr::summarise(
    n_stations_occupees = dplyr::n_distinct(station_id),
    abondance_totale_dans_modalite = sum(abondance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(espece, dplyr::desc(n_stations_occupees), dplyr::desc(abondance_totale_dans_modalite), combinaison_analysee) %>%
  dplyr::group_by(espece) %>%
  dplyr::mutate(rang = dplyr::row_number()) %>%
  dplyr::slice_head(n = top_n) %>%
  dplyr::ungroup()

top_combos_niveau_aquatique <- grille_complete %>%
  dplyr::filter(abondance > 0) %>%
  dplyr::select(espece, station_id, abondance) %>%
  dplyr::inner_join(stations_combos_niveau_aquatique, by = "station_id") %>%
  dplyr::distinct(espece, station_id, combinaison_niveau_aquatique, typologie_analysee, structure_niveau_aquatique, abondance) %>%
  dplyr::group_by(espece, combinaison_niveau_aquatique, typologie_analysee, structure_niveau_aquatique) %>%
  dplyr::summarise(
    n_stations_occupees = dplyr::n_distinct(station_id),
    abondance_totale_dans_modalite = sum(abondance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(espece, dplyr::desc(n_stations_occupees), dplyr::desc(abondance_totale_dans_modalite), combinaison_niveau_aquatique) %>%
  dplyr::group_by(espece) %>%
  dplyr::mutate(rang = dplyr::row_number()) %>%
  dplyr::slice_head(n = top_n) %>%
  dplyr::ungroup()

# -----------------------------
# 9) Fonctions generiques d'analyse
# -----------------------------
climate_vars <- c("bio1", "bio2", "bio5", "bio12", "pca1")

run_analysis_typologies <- function(top_table, category_col) {

  results <- vector("list", nrow(top_table) * (1 + length(climate_vars)))
  idx <- 1L

  for (i in seq_len(nrow(top_table))) {
    one_species  <- top_table$espece[i]
    one_category <- top_table[[category_col]][i]
    one_rank     <- top_table$rang[i]

    variants <- c("brut", climate_vars)

    for (v in variants) {
      covar_used <- if (v == "brut") NULL else v

      df_sp <- grille_complete %>%
        dplyr::filter(espece == one_species) %>%
        dplyr::mutate(indicateur = dplyr::if_else(.data[[category_col]] == one_category, 1L, 0L))

      stats_out <- fit_indicator_model(df_sp, indicator_var = "indicateur", covar_var = covar_used)

      results[[idx]] <- tibble::tibble(
        espece = one_species,
        rang = one_rank,
        categorie = one_category,
        modele_variant = v,
        covariable = ifelse(is.null(covar_used), "aucune", covar_used)
      ) %>% dplyr::bind_cols(stats_out)

      idx <- idx + 1L
    }
  }

  dplyr::bind_rows(results) %>%
    dplyr::group_by(espece, rang, categorie) %>%
    dplyr::mutate(
      p_adj_bh = stats::p.adjust(p_value, method = "BH"),
      significatif_modalite = dplyr::if_else(!is.na(p_adj_bh) & p_adj_bh < alpha_level, "oui", "non", missing = NA_character_),
      interpretation_modalite = interpret_effect(rate_ratio, p_adj_bh, alpha_level)
    ) %>%
    dplyr::ungroup()
}

run_analysis_structures <- function(top_table, category_col, membership_table, membership_col) {

  results <- vector("list", nrow(top_table) * (1 + length(climate_vars)))
  idx <- 1L

  for (i in seq_len(nrow(top_table))) {
    one_species  <- top_table$espece[i]
    one_category <- top_table[[category_col]][i]
    one_rank     <- top_table$rang[i]

    stations_target <- membership_table %>%
      dplyr::filter(.data[[membership_col]] == one_category) %>%
      dplyr::transmute(station_id, indicateur = 1L)

    variants <- c("brut", climate_vars)

    for (v in variants) {
      covar_used <- if (v == "brut") NULL else v

      df_sp <- grille_complete %>%
        dplyr::filter(espece == one_species) %>%
        dplyr::left_join(stations_target, by = "station_id") %>%
        dplyr::mutate(indicateur = tidyr::replace_na(indicateur, 0L))

      stats_out <- fit_indicator_model(df_sp, indicator_var = "indicateur", covar_var = covar_used)

      results[[idx]] <- tibble::tibble(
        espece = one_species,
        rang = one_rank,
        categorie = one_category,
        modele_variant = v,
        covariable = ifelse(is.null(covar_used), "aucune", covar_used)
      ) %>% dplyr::bind_cols(stats_out)

      idx <- idx + 1L
    }
  }

  dplyr::bind_rows(results) %>%
    dplyr::group_by(espece, rang, categorie) %>%
    dplyr::mutate(
      p_adj_bh = stats::p.adjust(p_value, method = "BH"),
      significatif_modalite = dplyr::if_else(!is.na(p_adj_bh) & p_adj_bh < alpha_level, "oui", "non", missing = NA_character_),
      interpretation_modalite = interpret_effect(rate_ratio, p_adj_bh, alpha_level)
    ) %>%
    dplyr::ungroup()
}

run_analysis_combos <- function(top_table, combo_col, membership_table, membership_combo_col) {

  results <- vector("list", nrow(top_table) * (1 + length(climate_vars)))
  idx <- 1L

  for (i in seq_len(nrow(top_table))) {
    one_species  <- top_table$espece[i]
    one_combo    <- top_table[[combo_col]][i]
    one_rank     <- top_table$rang[i]

    stations_target <- membership_table %>%
      dplyr::filter(.data[[membership_combo_col]] == one_combo) %>%
      dplyr::transmute(station_id, indicateur = 1L)

    variants <- c("brut", climate_vars)

    for (v in variants) {
      covar_used <- if (v == "brut") NULL else v

      df_sp <- grille_complete %>%
        dplyr::filter(espece == one_species) %>%
        dplyr::left_join(stations_target, by = "station_id") %>%
        dplyr::mutate(indicateur = tidyr::replace_na(indicateur, 0L))

      stats_out <- fit_indicator_model(df_sp, indicator_var = "indicateur", covar_var = covar_used)

      results[[idx]] <- tibble::tibble(
        espece = one_species,
        rang = one_rank,
        categorie = one_combo,
        modele_variant = v,
        covariable = ifelse(is.null(covar_used), "aucune", covar_used)
      ) %>% dplyr::bind_cols(stats_out)

      idx <- idx + 1L
    }
  }

  dplyr::bind_rows(results) %>%
    dplyr::group_by(espece, rang, categorie) %>%
    dplyr::mutate(
      p_adj_bh = stats::p.adjust(p_value, method = "BH"),
      significatif_modalite = dplyr::if_else(!is.na(p_adj_bh) & p_adj_bh < alpha_level, "oui", "non", missing = NA_character_),
      interpretation_modalite = interpret_effect(rate_ratio, p_adj_bh, alpha_level)
    ) %>%
    dplyr::ungroup()
}

make_resume <- function(results_df, category_name) {
  results_df %>%
    dplyr::group_by(espece, rang, categorie) %>%
    dplyr::summarise(
      sig_brut = any(modele_variant == "brut" & significatif_modalite == "oui"),
      n_modeles_ajustes_sig = sum(modele_variant != "brut" & significatif_modalite == "oui"),
      quels_modeles_ajustes_sig = paste(unique(covariable[modele_variant != "brut" & significatif_modalite == "oui"]), collapse = ", "),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      conclusion = dplyr::case_when(
        sig_brut & n_modeles_ajustes_sig > 0 ~ "effet robuste",
        sig_brut & n_modeles_ajustes_sig == 0 ~ "effet brut uniquement",
        !sig_brut & n_modeles_ajustes_sig > 0 ~ "effet visible apres ajustement",
        TRUE ~ "pas d'effet detecte"
      )
    ) %>%
    dplyr::rename(!!category_name := categorie)
}

# -----------------------------
# 10) Analyses principales
# -----------------------------
resultats_typologies <- run_analysis_typologies(
  top_table = top_typologies_analytiques,
  category_col = "typologie_analysee"
)

resultats_structures <- run_analysis_structures(
  top_table = top_structures_analytiques,
  category_col = "structure_analysee",
  membership_table = stations_structures_analytiques,
  membership_col = "structure_analysee"
)

resultats_combos <- run_analysis_combos(
  top_table = top_combos_analytiques,
  combo_col = "combinaison_analysee",
  membership_table = stations_combos_analytiques,
  membership_combo_col = "combinaison_analysee"
)

resultats_structures_niveau_aquatique <- run_analysis_structures(
  top_table = top_structures_niveau_aquatique,
  category_col = "structure_niveau_aquatique",
  membership_table = stations_structures_niveau_aquatique,
  membership_col = "structure_niveau_aquatique"
)

resultats_combos_niveau_aquatique <- run_analysis_combos(
  top_table = top_combos_niveau_aquatique,
  combo_col = "combinaison_niveau_aquatique",
  membership_table = stations_combos_niveau_aquatique,
  membership_combo_col = "combinaison_niveau_aquatique"
)

resume_typologies <- make_resume(resultats_typologies, "typologie")
resume_structures <- make_resume(resultats_structures, "structure")
resume_combos <- make_resume(resultats_combos, "combinaison")
resume_structures_niveau_aquatique <- make_resume(resultats_structures_niveau_aquatique, "structure")
resume_combos_niveau_aquatique <- make_resume(resultats_combos_niveau_aquatique, "combinaison")

# -----------------------------
# 11) Tables de tracabilite
# -----------------------------
trace_typologies <- dat %>%
  dplyr::distinct(typologie_fine, typologie_analysee) %>%
  dplyr::arrange(typologie_analysee, typologie_fine)

trace_structures <- dplyr::bind_rows(
  dat %>% dplyr::transmute(structure_fine = structure_1_fine, structure_analysee = structure_1_analysee, structure_niveau_aquatique = structure_1_niveau_aquatique),
  dat %>% dplyr::transmute(structure_fine = structure_2_fine, structure_analysee = structure_2_analysee, structure_niveau_aquatique = structure_2_niveau_aquatique)
) %>%
  dplyr::filter(!is.na(structure_fine), structure_fine != "") %>%
  dplyr::distinct() %>%
  dplyr::arrange(structure_niveau_aquatique, structure_analysee, structure_fine)

parametres_analyse <- tibble::tibble(
  parametre = c(
    "input_file", "output_dir", "alpha_level", "top_n",
    "min_stations_modalite", "min_stations_hors_modalite", "min_stations_occupees_espece",
    "regroupement_arbustif_lineaire"
  ),
  valeur = c(
    input_file, output_dir, alpha_level, top_n,
    min_stations_modalite, min_stations_hors_modalite, min_stations_occupees_espece,
    "Broussailles / formations arbustives + Structures lineaires arborees dans les paysages agricoles -> Structures ligneuses semi-ouvertes"
  )
)

diagnostic_general <- tibble::tibble(
  indicateur = c(
    "lignes_importees",
    "lignes_gardees",
    "nombre_especes",
    "nombre_stations",
    "nombre_typologies_analytiques",
    "nombre_structures_analytiques",
    "nombre_structures_niveau_aquatique"
  ),
  valeur = c(
    nrow(raw),
    nrow(dat),
    dplyr::n_distinct(dat$espece),
    dplyr::n_distinct(stations$station_id),
    dplyr::n_distinct(na.omit(dat$typologie_analysee)),
    dplyr::n_distinct(na.omit(c(dat$structure_1_analysee, dat$structure_2_analysee))),
    dplyr::n_distinct(na.omit(c(dat$structure_1_niveau_aquatique, dat$structure_2_niveau_aquatique)))
  )
)

diagnostic_modeles <- dplyr::bind_rows(
  resultats_typologies %>% dplyr::mutate(type_analyse = "typologies"),
  resultats_structures %>% dplyr::mutate(type_analyse = "structures"),
  resultats_combos %>% dplyr::mutate(type_analyse = "combos"),
  resultats_structures_niveau_aquatique %>% dplyr::mutate(type_analyse = "structures_niveau_aquatique"),
  resultats_combos_niveau_aquatique %>% dplyr::mutate(type_analyse = "combos_niveau_aquatique")
) %>%
  dplyr::count(type_analyse, modele_variant, modele, name = "n_tests")

# -----------------------------
# 12) Exports CSV
# -----------------------------
utils::write.csv(top_typologies_analytiques, file.path(output_dir, "top_typologies_analytiques.csv"), row.names = FALSE)
utils::write.csv(top_structures_analytiques, file.path(output_dir, "top_structures_analytiques.csv"), row.names = FALSE)
utils::write.csv(top_combos_analytiques, file.path(output_dir, "top_combos_analytiques.csv"), row.names = FALSE)

utils::write.csv(top_structures_niveau_aquatique, file.path(output_dir, "top_structures_niveau_aquatique.csv"), row.names = FALSE)
utils::write.csv(top_combos_niveau_aquatique, file.path(output_dir, "top_combos_niveau_aquatique.csv"), row.names = FALSE)

utils::write.csv(resultats_typologies, file.path(output_dir, "resultats_typologies.csv"), row.names = FALSE)
utils::write.csv(resultats_structures, file.path(output_dir, "resultats_structures.csv"), row.names = FALSE)
utils::write.csv(resultats_combos, file.path(output_dir, "resultats_combos.csv"), row.names = FALSE)

utils::write.csv(resultats_structures_niveau_aquatique, file.path(output_dir, "resultats_structures_niveau_aquatique.csv"), row.names = FALSE)
utils::write.csv(resultats_combos_niveau_aquatique, file.path(output_dir, "resultats_combos_niveau_aquatique.csv"), row.names = FALSE)

utils::write.csv(resume_typologies, file.path(output_dir, "resume_typologies.csv"), row.names = FALSE)
utils::write.csv(resume_structures, file.path(output_dir, "resume_structures.csv"), row.names = FALSE)
utils::write.csv(resume_combos, file.path(output_dir, "resume_combos.csv"), row.names = FALSE)

utils::write.csv(resume_structures_niveau_aquatique, file.path(output_dir, "resume_structures_niveau_aquatique.csv"), row.names = FALSE)
utils::write.csv(resume_combos_niveau_aquatique, file.path(output_dir, "resume_combos_niveau_aquatique.csv"), row.names = FALSE)

# -----------------------------
# 13) Export Excel principal
# -----------------------------
writexl::write_xlsx(
  list(
    parametres_analyse = parametres_analyse,
    diagnostic_general = diagnostic_general,
    diagnostic_modeles = diagnostic_modeles,

    trace_typologies = trace_typologies,
    trace_structures = trace_structures,

    stations = stations,
    grille_complete = grille_complete,

    top_typologies_fines = top_typologies_fines,
    top_typologies_analytiques = top_typologies_analytiques,

    top_structures_fines = top_structures_fines,
    top_structures_analytiques = top_structures_analytiques,
    top_structures_niveau_aquatique = top_structures_niveau_aquatique,

    top_combos_fins = top_combos_fins,
    top_combos_analytiques = top_combos_analytiques,
    top_combos_niveau_aquatique = top_combos_niveau_aquatique,

    resultats_typologies = resultats_typologies,
    resultats_structures = resultats_structures,
    resultats_combos = resultats_combos,
    resultats_structures_niveau_aquatique = resultats_structures_niveau_aquatique,
    resultats_combos_niveau_aquatique = resultats_combos_niveau_aquatique,

    resume_typologies = resume_typologies,
    resume_structures = resume_structures,
    resume_combos = resume_combos,
    resume_structures_niveau_aquatique = resume_structures_niveau_aquatique,
    resume_combos_niveau_aquatique = resume_combos_niveau_aquatique
  ),
  path = file.path(output_dir, "resultats_micromammiferes_UNINE_script_complet.xlsx")
)

cat("\nAnalyse terminee.\n")
cat("Dossier de sortie :", output_dir, "\n")
cat("Fichier principal :", file.path(output_dir, "resultats_micromammiferes_UNINE_script_complet.xlsx"), "\n")


# ============================================================
# 14) Seconde passe hierarchique ciblee
# ============================================================
# Cette seconde passe est conditionnee par les resultats de
# l'analyse harmonisee principale.
#
# Logique retenue :
# - typologies : on approfondit uniquement les grandes
#   categories significatives ou visibles apres ajustement,
#   parmi "Milieux humides" et "Milieux forestiers"
# - structures : on approfondit uniquement les grandes
#   categories significatives ou visibles apres ajustement,
#   parmi "Lisieres forestieres" et
#   "Milieux aquatiques sans vegetation"
#
# La comparaison se fait a l'interieur de la grande categorie
# parente, et non contre tout le jeu de donnees.
# ============================================================

fit_secondpass_nb <- function(df, fine_var, climate_var = NULL,
                              min_modalite = 5, min_hors = 5) {

  df <- df %>% dplyr::filter(!is.na(.data[[fine_var]]))

  if (!is.null(climate_var)) {
    df <- df %>% dplyr::filter(!is.na(.data[[climate_var]]))
  }

  out <- lapply(sort(unique(df[[fine_var]])), function(modalite) {

    d <- df %>%
      dplyr::mutate(indicateur = ifelse(.data[[fine_var]] == modalite, 1, 0))

    n_in <- sum(d$indicateur == 1, na.rm = TRUE)
    n_out <- sum(d$indicateur == 0, na.rm = TRUE)

    if (n_in < min_modalite || n_out < min_hors) {
      return(tibble::tibble(
        modalite_fine = modalite,
        modele = "non_testable",
        estimate = NA_real_,
        std.error = NA_real_,
        rate_ratio = NA_real_,
        rate_ratio_low = NA_real_,
        rate_ratio_high = NA_real_,
        p_value = NA_real_,
        n_stations_modalite = n_in,
        n_stations_hors_modalite = n_out
      ))
    }

    form <- if (is.null(climate_var)) {
      stats::as.formula("abondance ~ indicateur")
    } else {
      stats::as.formula(paste0("abondance ~ indicateur + ", climate_var))
    }

    mod <- tryCatch(
      suppressWarnings(MASS::glm.nb(form, data = d)),
      error = function(e) NULL
    )

    if (is.null(mod)) {
      mod <- tryCatch(
        suppressWarnings(stats::glm(form, family = stats::poisson(link = "log"), data = d)),
        error = function(e) NULL
      )
      modele_nom <- ifelse(is.null(mod), "non_testable", "poisson_fallback")
    } else {
      modele_nom <- "negative_binomial"
    }

    if (is.null(mod)) {
      return(tibble::tibble(
        modalite_fine = modalite,
        modele = "non_testable",
        estimate = NA_real_,
        std.error = NA_real_,
        rate_ratio = NA_real_,
        rate_ratio_low = NA_real_,
        rate_ratio_high = NA_real_,
        p_value = NA_real_,
        n_stations_modalite = n_in,
        n_stations_hors_modalite = n_out
      ))
    }

    coefs <- tryCatch(summary(mod)$coefficients, error = function(e) NULL)

    if (is.null(coefs) || !("indicateur" %in% rownames(coefs))) {
      return(tibble::tibble(
        modalite_fine = modalite,
        modele = "non_testable",
        estimate = NA_real_,
        std.error = NA_real_,
        rate_ratio = NA_real_,
        rate_ratio_low = NA_real_,
        rate_ratio_high = NA_real_,
        p_value = NA_real_,
        n_stations_modalite = n_in,
        n_stations_hors_modalite = n_out
      ))
    }

    est <- unname(coefs["indicateur", "Estimate"])
    se <- unname(coefs["indicateur", "Std. Error"])
    pcol <- intersect(colnames(coefs), c("Pr(>|z|)", "Pr(>|t|)"))
    pval <- if (length(pcol) == 1) unname(coefs["indicateur", pcol]) else NA_real_

    tibble::tibble(
      modalite_fine = modalite,
      modele = modele_nom,
      estimate = est,
      std.error = se,
      rate_ratio = exp(est),
      rate_ratio_low = exp(est - 1.96 * se),
      rate_ratio_high = exp(est + 1.96 * se),
      p_value = pval,
      n_stations_modalite = n_in,
      n_stations_hors_modalite = n_out
    )
  })

  dplyr::bind_rows(out)
}

# -----------------------------
# 14a) Seconde passe - typologies
# -----------------------------
secondpass_parents_typologies <- resume_typologies %>%
  dplyr::filter(
    conclusion != "pas d'effet detecte",
    typologie %in% c("Milieux humides", "Milieux forestiers")
  ) %>%
  dplyr::arrange(espece, typologie)

secondpass_typologies <- dplyr::bind_rows(lapply(seq_len(nrow(secondpass_parents_typologies)), function(i) {

  sp <- secondpass_parents_typologies$espece[i]
  parent <- secondpass_parents_typologies$typologie[i]
  conclusion_parent <- secondpass_parents_typologies$conclusion[i]

  df_parent <- grille_complete %>%
    dplyr::filter(espece == sp, typologie_analysee == parent)

  brut <- fit_secondpass_nb(df_parent, fine_var = "typologie_fine", climate_var = NULL) %>%
    dplyr::mutate(modele_variant = "brut")

  ajustes <- dplyr::bind_rows(lapply(climate_vars, function(cv) {
    fit_secondpass_nb(df_parent, fine_var = "typologie_fine", climate_var = cv) %>%
      dplyr::mutate(modele_variant = cv)
  }))

  dplyr::bind_rows(brut, ajustes) %>%
    dplyr::mutate(
      espece = sp,
      categorie_parent = parent,
      conclusion_parent = conclusion_parent
    )
}))

secondpass_typologies <- secondpass_typologies %>%
  dplyr::group_by(espece, categorie_parent, modalite_fine) %>%
  dplyr::mutate(
    p_adj_bh = stats::p.adjust(p_value, method = "BH"),
    significatif = ifelse(!is.na(p_adj_bh) & p_adj_bh < alpha_level, "oui", "non")
  ) %>%
  dplyr::ungroup()

secondpass_resume_typologies <- secondpass_typologies %>%
  dplyr::group_by(espece, categorie_parent, modalite_fine) %>%
  dplyr::summarise(
    sig_brut = any(modele_variant == "brut" & significatif == "oui"),
    n_modeles_ajustes_sig = sum(modele_variant != "brut" & significatif == "oui"),
    quels_modeles_ajustes_sig = paste(unique(modele_variant[modele_variant != "brut" & significatif == "oui"]), collapse = ", "),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    conclusion = dplyr::case_when(
      sig_brut & n_modeles_ajustes_sig > 0 ~ "effet robuste",
      sig_brut & n_modeles_ajustes_sig == 0 ~ "effet brut uniquement",
      !sig_brut & n_modeles_ajustes_sig > 0 ~ "effet visible apres ajustement",
      TRUE ~ "pas d'effet detecte"
    )
  )

# -----------------------------
# 14b) Seconde passe - structures
# -----------------------------
secondpass_parents_structures <- resume_structures %>%
  dplyr::filter(
    conclusion != "pas d'effet detecte",
    structure %in% c("Lisières forestières", "Milieux aquatiques sans végétation")
  ) %>%
  dplyr::arrange(espece, structure)

structures_fines_long <- dplyr::bind_rows(
  stations %>%
    dplyr::transmute(
      station_id,
      structure_fine = structure_1_fine,
      structure_parent = structure_1_analysee
    ),
  stations %>%
    dplyr::transmute(
      station_id,
      structure_fine = structure_2_fine,
      structure_parent = structure_2_analysee
    )
) %>%
  dplyr::filter(!is.na(structure_fine), !is.na(structure_parent)) %>%
  dplyr::distinct()

secondpass_structures <- dplyr::bind_rows(lapply(seq_len(nrow(secondpass_parents_structures)), function(i) {

  sp <- secondpass_parents_structures$espece[i]
  parent <- secondpass_parents_structures$structure[i]
  conclusion_parent <- secondpass_parents_structures$conclusion[i]

  stations_parent <- structures_fines_long %>%
    dplyr::filter(structure_parent == parent) %>%
    dplyr::select(station_id, structure_fine) %>%
    dplyr::distinct()

  df_parent <- grille_complete %>%
    dplyr::filter(espece == sp) %>%
    dplyr::inner_join(stations_parent, by = "station_id")

  brut <- fit_secondpass_nb(df_parent, fine_var = "structure_fine", climate_var = NULL) %>%
    dplyr::mutate(modele_variant = "brut")

  ajustes <- dplyr::bind_rows(lapply(climate_vars, function(cv) {
    fit_secondpass_nb(df_parent, fine_var = "structure_fine", climate_var = cv) %>%
      dplyr::mutate(modele_variant = cv)
  }))

  dplyr::bind_rows(brut, ajustes) %>%
    dplyr::mutate(
      espece = sp,
      categorie_parent = parent,
      conclusion_parent = conclusion_parent
    )
}))

secondpass_structures <- secondpass_structures %>%
  dplyr::group_by(espece, categorie_parent, modalite_fine) %>%
  dplyr::mutate(
    p_adj_bh = stats::p.adjust(p_value, method = "BH"),
    significatif = ifelse(!is.na(p_adj_bh) & p_adj_bh < alpha_level, "oui", "non")
  ) %>%
  dplyr::ungroup()

secondpass_resume_structures <- secondpass_structures %>%
  dplyr::group_by(espece, categorie_parent, modalite_fine) %>%
  dplyr::summarise(
    sig_brut = any(modele_variant == "brut" & significatif == "oui"),
    n_modeles_ajustes_sig = sum(modele_variant != "brut" & significatif == "oui"),
    quels_modeles_ajustes_sig = paste(unique(modele_variant[modele_variant != "brut" & significatif == "oui"]), collapse = ", "),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    conclusion = dplyr::case_when(
      sig_brut & n_modeles_ajustes_sig > 0 ~ "effet robuste",
      sig_brut & n_modeles_ajustes_sig == 0 ~ "effet brut uniquement",
      !sig_brut & n_modeles_ajustes_sig > 0 ~ "effet visible apres ajustement",
      TRUE ~ "pas d'effet detecte"
    )
  )

# -----------------------------
# 14c) Exports de seconde passe
# -----------------------------
utils::write.csv(secondpass_parents_typologies, file.path(output_dir, "secondpass_parents_typologies.csv"), row.names = FALSE)
utils::write.csv(secondpass_typologies, file.path(output_dir, "secondpass_typologies_detail.csv"), row.names = FALSE)
utils::write.csv(secondpass_resume_typologies, file.path(output_dir, "secondpass_resume_typologies.csv"), row.names = FALSE)

utils::write.csv(secondpass_parents_structures, file.path(output_dir, "secondpass_parents_structures.csv"), row.names = FALSE)
utils::write.csv(secondpass_structures, file.path(output_dir, "secondpass_structures_detail.csv"), row.names = FALSE)
utils::write.csv(secondpass_resume_structures, file.path(output_dir, "secondpass_resume_structures.csv"), row.names = FALSE)

writexl::write_xlsx(
  list(
    secondpass_parents_typologies = secondpass_parents_typologies,
    secondpass_typologies_detail = secondpass_typologies,
    secondpass_resume_typologies = secondpass_resume_typologies,
    secondpass_parents_structures = secondpass_parents_structures,
    secondpass_structures_detail = secondpass_structures,
    secondpass_resume_structures = secondpass_resume_structures
  ),
  path = file.path(output_dir, "resultats_secondes_passes_UNINE.xlsx")
)

cat("\nSeconde passe hierarchique terminee.\n")
cat("Fichier supplementaire :", file.path(output_dir, "resultats_secondes_passes_UNINE.xlsx"), "\n")
# ============================================================
# 15) Tableaux synthetiques pour le memoire
# ============================================================

format_num <- function(x, digits = 2) {
  ifelse(
    is.na(x),
    NA_character_,
    format(round(x, digits), nsmall = digits, trim = TRUE)
  )
}

format_q <- function(x) {
  ifelse(
    is.na(x),
    NA_character_,
    ifelse(x < 0.001, "<0.001", format(round(x, 4), nsmall = 4, trim = TRUE))
  )
}

format_ci <- function(low, high) {
  ifelse(
    is.na(low) | is.na(high),
    NA_character_,
    paste0(format_num(low, 2), "-", format_num(high, 2))
  )
}

# ------------------------------------------------------------
# Table 1. Dataset structure and sampling coverage
# ------------------------------------------------------------

table1_dataset_structure <- tibble::tibble(
  Indicator = c(
    "Imported rows",
    "Conserved rows after filtering",
    "Number of species",
    "Number of stations",
    "Number of cantons",
    "Study years",
    "Altitude range",
    "Median altitude"
  ),
  Value = c(
    nrow(raw),
    nrow(dat),
    dplyr::n_distinct(dat$espece),
    dplyr::n_distinct(stations$station_id),
    dplyr::n_distinct(stations$canton, na.rm = TRUE),
    paste0(min(dat$annee, na.rm = TRUE), "-", max(dat$annee, na.rm = TRUE)),
    paste0(
      round(min(stations$altitude, na.rm = TRUE), 0),
      "-",
      round(max(stations$altitude, na.rm = TRUE), 0),
      " m"
    ),
    paste0(round(stats::median(stations$altitude, na.rm = TRUE), 0), " m")
  )
)

# ------------------------------------------------------------
# Table 2. Number of stations and captures per species
# ------------------------------------------------------------

table2_species_sampling <- grille_complete %>%
  dplyr::group_by(espece) %>%
  dplyr::summarise(
    Occupied_stations = sum(abondance > 0, na.rm = TRUE),
    Total_abundance = sum(abondance, na.rm = TRUE),
    Mean_abundance_per_occupied_station = mean(abondance[abondance > 0], na.rm = TRUE),
    Max_abundance_at_one_station = max(abondance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(Total_abundance)) %>%
  dplyr::mutate(
    Mean_abundance_per_occupied_station = round(Mean_abundance_per_occupied_station, 2)
  ) %>%
  dplyr::rename(Species = espece)

# ------------------------------------------------------------
# Table 3. Harmonized habitat categories used in the analysis
# ------------------------------------------------------------

table3a_typology_harmonization <- trace_typologies %>%
  dplyr::filter(!is.na(typologie_fine), !is.na(typologie_analysee)) %>%
  dplyr::group_by(typologie_analysee) %>%
  dplyr::summarise(
    Fine_categories = paste(sort(unique(typologie_fine)), collapse = "; "),
    Number_of_fine_categories = dplyr::n_distinct(typologie_fine),
    .groups = "drop"
  ) %>%
  dplyr::mutate(Variable = "Typology") %>%
  dplyr::rename(Harmonized_category = typologie_analysee)

table3b_structure_harmonization <- trace_structures %>%
  dplyr::filter(!is.na(structure_fine), !is.na(structure_analysee)) %>%
  dplyr::group_by(structure_analysee) %>%
  dplyr::summarise(
    Fine_categories = paste(sort(unique(structure_fine)), collapse = "; "),
    Number_of_fine_categories = dplyr::n_distinct(structure_fine),
    .groups = "drop"
  ) %>%
  dplyr::mutate(Variable = "Structure") %>%
  dplyr::rename(Harmonized_category = structure_analysee)

table3_harmonized_categories <- dplyr::bind_rows(
  table3a_typology_harmonization,
  table3b_structure_harmonization
) %>%
  dplyr::select(
    Variable,
    Harmonized_category,
    Number_of_fine_categories,
    Fine_categories
  ) %>%
  dplyr::arrange(Variable, Harmonized_category)

# ------------------------------------------------------------
# Preparation commune pour les tableaux statistiques
# ------------------------------------------------------------

all_main_results <- dplyr::bind_rows(
  resultats_typologies %>%
    dplyr::mutate(Analysis_level = "Typology"),
  
  resultats_structures %>%
    dplyr::mutate(Analysis_level = "Structure"),
  
  resultats_combos %>%
    dplyr::mutate(Analysis_level = "Typology x structure"),
  
  resultats_structures_niveau_aquatique %>%
    dplyr::mutate(Analysis_level = "Structure, overall aquatic level"),
  
  resultats_combos_niveau_aquatique %>%
    dplyr::mutate(Analysis_level = "Typology x structure, overall aquatic level")
) %>%
  dplyr::rename(
    Species = espece,
    Category = categorie
  ) %>%
  dplyr::mutate(
    Is_significant = !is.na(p_adj_bh) & p_adj_bh < alpha_level
  ) %>%
  dplyr::group_by(Analysis_level, Species, Category) %>%
  dplyr::mutate(
    Significant_raw = any(modele_variant == "brut" & Is_significant),
    Number_significant_adjusted_models = sum(modele_variant != "brut" & Is_significant),
    Significant_adjusted_models = paste(
      unique(covariable[modele_variant != "brut" & Is_significant]),
      collapse = ", "
    ),
    Conclusion = dplyr::case_when(
      Significant_raw & Number_significant_adjusted_models > 0 ~ "Robust effect",
      Significant_raw & Number_significant_adjusted_models == 0 ~ "Raw effect only",
      !Significant_raw & Number_significant_adjusted_models > 0 ~ "Visible after adjustment",
      TRUE ~ "No detected effect"
    )
  ) %>%
  dplyr::ungroup()

# ------------------------------------------------------------
# Table 4. Summary of statistically significant habitat effects
# ------------------------------------------------------------

table4_significant_effects <- all_main_results %>%
  dplyr::filter(Is_significant) %>%
  dplyr::group_by(Analysis_level, Species, Category) %>%
  dplyr::arrange(
    dplyr::if_else(modele_variant == "brut", 0, 1),
    p_adj_bh
  ) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Direction = dplyr::case_when(
      rate_ratio > 1 ~ "Positive",
      rate_ratio < 1 ~ "Negative",
      TRUE ~ "Neutral"
    ),
    Model_reported = dplyr::case_when(
      modele_variant == "brut" ~ "Raw model",
      TRUE ~ paste0("Adjusted model: ", toupper(modele_variant))
    ),
    Climate_robustness = dplyr::case_when(
      Conclusion == "Robust effect" ~ paste0("Persists after adjustment for: ", Significant_adjusted_models),
      Conclusion == "Visible after adjustment" ~ paste0("Detected only after adjustment for: ", Significant_adjusted_models),
      Conclusion == "Raw effect only" ~ "Significant only in raw model",
      TRUE ~ "No detected effect"
    )
  ) %>%
  dplyr::transmute(
    Species,
    Analysis_level,
    Category,
    Direction,
    Model_reported,
    RR = format_num(rate_ratio, 2),
    CI_95 = format_ci(rate_ratio_low, rate_ratio_high),
    qBH = format_q(p_adj_bh),
    Conclusion,
    Climate_robustness
  ) %>%
  dplyr::arrange(Species, Analysis_level, Category)

# ------------------------------------------------------------
# Table 5. Climate-adjusted robustness of significant effects
# ------------------------------------------------------------

table5_climate_robustness <- all_main_results %>%
  dplyr::filter(Conclusion != "No detected effect") %>%
  dplyr::mutate(
    Model = dplyr::recode(
      modele_variant,
      "brut" = "Raw",
      "bio1" = "BIO1",
      "bio2" = "BIO2",
      "bio5" = "BIO5",
      "bio12" = "BIO12",
      "pca1" = "PCA1"
    ),
    Cell = dplyr::case_when(
      Is_significant ~ paste0("Yes; RR=", format_num(rate_ratio, 2), "; q=", format_q(p_adj_bh)),
      TRUE ~ "No"
    )
  ) %>%
  dplyr::select(
    Species,
    Analysis_level,
    Category,
    Conclusion,
    Model,
    Cell
  ) %>%
  tidyr::pivot_wider(
    names_from = Model,
    values_from = Cell
  ) %>%
  dplyr::arrange(Species, Analysis_level, Category)

# ------------------------------------------------------------
# Table 6. Results of the second hierarchical pass
# ------------------------------------------------------------

secondpass_all_resume <- dplyr::bind_rows(
  secondpass_resume_typologies %>%
    dplyr::mutate(
      Analysis_level = "Typology second pass",
      Parent_category = categorie_parent,
      Fine_category = modalite_fine
    ),
  
  secondpass_resume_structures %>%
    dplyr::mutate(
      Analysis_level = "Structure second pass",
      Parent_category = categorie_parent,
      Fine_category = modalite_fine
    )
) %>%
  dplyr::mutate(
    Conclusion_english = dplyr::case_when(
      conclusion == "effet robuste" ~ "Robust effect",
      conclusion == "effet brut uniquement" ~ "Raw effect only",
      conclusion == "effet visible apres ajustement" ~ "Visible after adjustment",
      TRUE ~ "No detected effect"
    )
  )

table6_second_hierarchical_pass <- secondpass_all_resume %>%
  dplyr::group_by(espece, Analysis_level, Parent_category) %>%
  dplyr::summarise(
    Number_of_fine_categories_tested = dplyr::n_distinct(Fine_category),
    Fine_categories_with_detected_effect = paste(
      unique(Fine_category[Conclusion_english != "No detected effect"]),
      collapse = "; "
    ),
    Summary = dplyr::case_when(
      any(Conclusion_english == "Robust effect") ~ "At least one robust fine-level effect",
      any(Conclusion_english == "Visible after adjustment") ~ "Fine-level effect visible only after adjustment",
      any(Conclusion_english == "Raw effect only") ~ "Fine-level effect only in raw model",
      TRUE ~ "No fine-level effect detected"
    ),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    Fine_categories_with_detected_effect = dplyr::if_else(
      Fine_categories_with_detected_effect == "",
      "None",
      Fine_categories_with_detected_effect
    )
  ) %>%
  dplyr::rename(Species = espece) %>%
  dplyr::arrange(Species, Analysis_level, Parent_category)

# ------------------------------------------------------------
# Table 7. Species without robust statistical effect
# ------------------------------------------------------------

table7_species_without_robust_effect <- all_main_results %>%
  dplyr::distinct(Species, Analysis_level, Category, Conclusion) %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(
    Robust_effect_detected = dplyr::if_else(any(Conclusion == "Robust effect"), "Yes", "No"),
    Any_statistical_signal_detected = dplyr::if_else(any(Conclusion != "No detected effect"), "Yes", "No"),
    Non_robust_signals = paste(
      unique(paste(
        Analysis_level,
        Category,
        Conclusion,
        sep = " | "
      )[Conclusion != "No detected effect" & Conclusion != "Robust effect"]),
      collapse = "; "
    ),
    .groups = "drop"
  ) %>%
  dplyr::filter(Robust_effect_detected == "No") %>%
  dplyr::mutate(
    Non_robust_signals = dplyr::if_else(
      Non_robust_signals == "",
      "None",
      Non_robust_signals
    )
  ) %>%
  dplyr::arrange(Species)

# ------------------------------------------------------------
# Export des tableaux pour le memoire
# ------------------------------------------------------------

tables_memoire_dir <- file.path(output_dir, "tables_memoire")
dir.create(tables_memoire_dir, showWarnings = FALSE, recursive = TRUE)

writexl::write_xlsx(
  list(
    Table1_dataset_structure = table1_dataset_structure,
    Table2_species_sampling = table2_species_sampling,
    Table3_harmonized_categories = table3_harmonized_categories,
    Table4_significant_effects = table4_significant_effects,
    Table5_climate_robustness = table5_climate_robustness,
    Table6_second_pass = table6_second_hierarchical_pass,
    Table7_no_robust_effect = table7_species_without_robust_effect
  ),
  path = file.path(tables_memoire_dir, "tables_memoire_UNINE.xlsx")
)

utils::write.csv(table1_dataset_structure, file.path(tables_memoire_dir, "table1_dataset_structure.csv"), row.names = FALSE)
utils::write.csv(table2_species_sampling, file.path(tables_memoire_dir, "table2_species_sampling.csv"), row.names = FALSE)
utils::write.csv(table3_harmonized_categories, file.path(tables_memoire_dir, "table3_harmonized_categories.csv"), row.names = FALSE)
utils::write.csv(table4_significant_effects, file.path(tables_memoire_dir, "table4_significant_effects.csv"), row.names = FALSE)
utils::write.csv(table5_climate_robustness, file.path(tables_memoire_dir, "table5_climate_robustness.csv"), row.names = FALSE)
utils::write.csv(table6_second_hierarchical_pass, file.path(tables_memoire_dir, "table6_second_hierarchical_pass.csv"), row.names = FALSE)
utils::write.csv(table7_species_without_robust_effect, file.path(tables_memoire_dir, "table7_species_without_robust_effect.csv"), row.names = FALSE)

cat("\nTableaux pour le memoire exportes.\n")
cat("Dossier :", tables_memoire_dir, "\n")
cat("Fichier Excel :", file.path(tables_memoire_dir, "tables_memoire_UNINE.xlsx"), "\n")
# ============================================================
# 16) Figures pour le memoire
# ============================================================

figures_dir <- file.path(output_dir, "figures_memoire")
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------
# Fonctions utilitaires
# ------------------------------------------------------------

scientific_name <- function(x) {
  stringr::str_trim(stringr::str_replace(x, "\\s*\\(.*\\)$", ""))
}

translate_category <- function(x) {
  dplyr::recode(
    x,
    "Prairies" = "Grasslands",
    "Milieux humides" = "Wetlands",
    "Milieux forestiers" = "Forest habitats",
    "Lisières forestières" = "Forest edges",
    "Milieux aquatiques avec végétation" = "Aquatic habitats with vegetation",
    "Milieux aquatiques sans végétation" = "Aquatic habitats without vegetation",
    "Milieux aquatiques" = "Aquatic habitats",
    "Structures ligneuses semi-ouvertes" = "Semi-open woody structures",
    "Milieux rocheux" = "Rocky habitats",
    "Boisements isolés" = "Isolated wooded patches",
    "Milieux anthropiques bâtis" = "Built anthropogenic habitats",
    .default = x
  )
}

translate_level <- function(x) {
  dplyr::recode(
    x,
    "Typology" = "Typology",
    "Structure" = "Structure",
    "Structure, overall aquatic level" = "Structure, overall aquatic level",
    "Typology x structure" = "Typology x structure",
    "Typology x structure, overall aquatic level" = "Typology x structure, overall aquatic level",
    .default = x
  )
}

make_effect_label <- function(species, level, category) {
  paste0(
    scientific_name(species),
    " | ",
    translate_level(level),
    " | ",
    translate_category(category)
  )
}

# ------------------------------------------------------------
# Figure 1. Map of sampling stations
# ------------------------------------------------------------

stations_map <- stations %>%
  dplyr::mutate(
    coord_x_num = suppressWarnings(as.numeric(coord_x)),
    coord_y_num = suppressWarnings(as.numeric(coord_y)),
    canton = dplyr::recode(canton, "OB" = "OW", .default = canton)
  ) %>%
  dplyr::filter(!is.na(coord_x_num), !is.na(coord_y_num))

stations_sf <- sf::st_as_sf(
  stations_map,
  coords = c("coord_x_num", "coord_y_num"),
  crs = 21781,
  remove = FALSE
) %>%
  sf::st_transform(4326)

switzerland <- rnaturalearth::ne_countries(
  country = "Switzerland",
  scale = "medium",
  returnclass = "sf"
)

fig1_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = switzerland, fill = "grey95", color = "grey40", linewidth = 0.4) +
  ggplot2::geom_sf(
    data = stations_sf,
    ggplot2::aes(color = canton),
    size = 1.8,
    alpha = 0.85
  ) +
  ggplot2::coord_sf(xlim = c(5.8, 10.6), ylim = c(45.7, 47.9), expand = FALSE) +
  ggplot2::labs(
    title = "Sampling stations across Switzerland",
    subtitle = paste0("n = ", dplyr::n_distinct(stations$station_id), " stations"),
    color = "Canton",
    x = "Longitude",
    y = "Latitude"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    legend.position = "right",
    panel.grid.major = ggplot2::element_line(color = "grey85", linewidth = 0.2),
    plot.title = ggplot2::element_text(face = "bold")
  )

ggplot2::ggsave(
  filename = file.path(figures_dir, "figure1_sampling_stations_map.png"),
  plot = fig1_map,
  width = 8,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# Figure 2. Species barplot before Results
# ------------------------------------------------------------

fig2_data <- table2_species_sampling %>%
  dplyr::mutate(
    Species_short = scientific_name(Species)
  ) %>%
  dplyr::select(
    Species_short,
    Occupied_stations,
    Total_abundance
  ) %>%
  tidyr::pivot_longer(
    cols = c(Occupied_stations, Total_abundance),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  dplyr::mutate(
    Metric = dplyr::recode(
      Metric,
      "Occupied_stations" = "Occupied stations",
      "Total_abundance" = "Total abundance"
    ),
    Species_short = forcats::fct_reorder(Species_short, Value, .fun = max)
  )

fig2_barplot_species <- ggplot2::ggplot(
  fig2_data,
  ggplot2::aes(x = Species_short, y = Value)
) +
  ggplot2::geom_col(width = 0.75) +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(~ Metric, scales = "free_x") +
  ggplot2::labs(
    title = "Sampling coverage and abundance by species",
    x = NULL,
    y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    strip.text = ggplot2::element_text(face = "bold")
  )

ggplot2::ggsave(
  filename = file.path(figures_dir, "figure2_species_sampling_barplot.png"),
  plot = fig2_barplot_species,
  width = 8,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# Figure 3. Forest plot of significant RR
# ------------------------------------------------------------

table4_plot <- table4_significant_effects %>%
  dplyr::mutate(
    RR_num = suppressWarnings(as.numeric(RR)),
    CI_low = suppressWarnings(as.numeric(stringr::str_extract(CI_95, "^[0-9.]+"))),
    CI_high = suppressWarnings(as.numeric(stringr::str_extract(CI_95, "(?<=-)[0-9.]+"))),
    Category_en = translate_category(Category),
    Level_en = translate_level(Analysis_level),
    Species_short = scientific_name(Species),
    Effect_label = paste0(Species_short, " | ", Category_en),
    Effect_label = forcats::fct_reorder(Effect_label, RR_num)
  ) %>%
  dplyr::filter(!is.na(RR_num), !is.na(CI_low), !is.na(CI_high))

fig3_forest_rr <- ggplot2::ggplot(
  table4_plot,
  ggplot2::aes(x = RR_num, y = Effect_label)
) +
  ggplot2::geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.5) +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = CI_low, xmax = CI_high),
    height = 0.2,
    linewidth = 0.5
  ) +
  ggplot2::geom_point(size = 2.2) +
  ggplot2::scale_x_log10() +
  ggplot2::labs(
    title = "Effect sizes of significant habitat associations",
    subtitle = "Rate ratios estimated by negative binomial models",
    x = "Rate ratio, log scale",
    y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    axis.text.y = ggplot2::element_text(size = 7)
  )

ggplot2::ggsave(
  filename = file.path(figures_dir, "figure3_forest_plot_significant_RR.png"),
  plot = fig3_forest_rr,
  width = 9,
  height = 8,
  dpi = 300
)

# ------------------------------------------------------------
# Figure 4. Heatmap species x habitats
# ------------------------------------------------------------

heatmap_habitats_data <- table4_significant_effects %>%
  dplyr::filter(
    Analysis_level %in% c(
      "Typology",
      "Structure",
      "Structure, overall aquatic level"
    )
  ) %>%
  dplyr::mutate(
    Species_short = scientific_name(Species),
    Habitat = translate_category(Category),
    RR_num = suppressWarnings(as.numeric(RR)),
    log_RR = log(RR_num),
    Direction_symbol = dplyr::case_when(
      RR_num > 1 ~ "+",
      RR_num < 1 ~ "-",
      TRUE ~ "0"
    )
  ) %>%
  dplyr::filter(!is.na(RR_num)) %>%
  dplyr::group_by(Species_short, Habitat) %>%
  dplyr::slice_max(order_by = abs(log_RR), n = 1, with_ties = FALSE) %>%
  dplyr::ungroup()

fig4_heatmap_species_habitats <- ggplot2::ggplot(
  heatmap_habitats_data,
  ggplot2::aes(x = Habitat, y = Species_short, fill = log_RR)
) +
  ggplot2::geom_tile(color = "white", linewidth = 0.5) +
  ggplot2::geom_text(ggplot2::aes(label = Direction_symbol), size = 4) +
  ggplot2::scale_fill_gradient2(
    low = "#4C78A8",
    mid = "white",
    high = "#E45756",
    midpoint = 0,
    name = "log(RR)"
  ) +
  ggplot2::labs(
    title = "Species-specific responses to harmonized habitat categories",
    x = NULL,
    y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
    panel.grid = ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = file.path(figures_dir, "figure4_heatmap_species_habitats.png"),
  plot = fig4_heatmap_species_habitats,
  width = 9,
  height = 5.5,
  dpi = 300
)

# ------------------------------------------------------------
# Figure 5. Climate robustness heatmap
# ------------------------------------------------------------

climate_models <- c("Raw", "BIO1", "BIO2", "BIO5", "BIO12", "PCA1")

heatmap_climate_data <- table5_climate_robustness %>%
  dplyr::mutate(
    Species_short = scientific_name(Species),
    Category_en = translate_category(Category),
    Effect_label = paste0(Species_short, " | ", Category_en)
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(climate_models),
    names_to = "Model",
    values_to = "Result"
  ) %>%
  dplyr::mutate(
    Significant = dplyr::if_else(Result != "No" & !is.na(Result), "Significant", "Not significant"),
    Model = factor(Model, levels = climate_models),
    Effect_label = forcats::fct_rev(factor(Effect_label))
  )

fig5_heatmap_climate <- ggplot2::ggplot(
  heatmap_climate_data,
  ggplot2::aes(x = Model, y = Effect_label, fill = Significant)
) +
  ggplot2::geom_tile(color = "white", linewidth = 0.4) +
  ggplot2::scale_fill_manual(
    values = c("Significant" = "#2CA25F", "Not significant" = "grey90"),
    name = NULL
  ) +
  ggplot2::labs(
    title = "Robustness of significant habitat effects after climatic adjustment",
    x = NULL,
    y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    axis.text.y = ggplot2::element_text(size = 7),
    panel.grid = ggplot2::element_blank(),
    legend.position = "bottom"
  )

ggplot2::ggsave(
  filename = file.path(figures_dir, "figure5_heatmap_climate_robustness.png"),
  plot = fig5_heatmap_climate,
  width = 8,
  height = 8,
  dpi = 300
)

cat("\nFigures pour le memoire exportees dans :\n")
cat(figures_dir, "\n")
cat("Fichiers crees :\n")
print(list.files(figures_dir))

# ============================================================
# 17) Session information for reproducibility
# ============================================================
# LEF archival guideline:
# Run this script once in the FINAL R/RStudio environment used for the thesis.
# The command below prints the complete session information and also saves it
# as "session_info.txt" in the output directory.
#
# IMPORTANT BEFORE ARCHIVING:
# Copy-paste the printed output of sessioninfo::session_info() into the
# commented block at the end of this script, as requested by the LEF guideline.

session_details <- sessioninfo::session_info()
print(session_details)
capture.output(
  session_details,
  file = file.path(output_dir, "session_info.txt")
)

# ------------------------------------------------------------
# PASTE FINAL sessioninfo::session_info() OUTPUT BELOW
# after running this script in the thesis analysis environment.
# ------------------------------------------------------------
# [Paste output here before the final GitHub archive]

