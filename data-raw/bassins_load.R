library(tidyverse)
library(sf)

## Download data
# CAMELS-FR : https://doi.org/10.57745/WH7FJR
# On ne récupère que les attributs des bassins (CAMELS_FR_attributes.zip, ~10 Mo),
# décompressés dans data-raw/CAMELS_FR_attributes
if (!dir.exists(here::here("data-raw", "CAMELS_FR_attributes"))) {
  zip_file <- here::here("data-raw", "CAMELS_FR_attributes.zip")
  download.file(
    "https://entrepot.recherche.data.gouv.fr/api/access/datafile/621683",
    zip_file,
    mode = "wb"
  )
  unzip(zip_file, exdir = here::here("data-raw"))
  file.remove(zip_file)
}


## Read data
# Une ligne par station hydrométrique, identifiée par sta_code_h3
# Les fichiers ont « ; » comme séparateur de colonnes et « . » comme séparateur décimal
# Pas de variables sol_* ni cli_prec_intensity : leur licence interdit la redistribution

# Cible : indice de débit de base (part du débit venant des écoulements lents)
signatures <- read_delim(
  here::here(
    "data-raw",
    "CAMELS_FR_attributes",
    "time_series_statistics",
    "CAMELS_FR_hydrological_signatures.csv"
  ),
  delim = ";",
  show_col_types = FALSE
) |>
  select(sta_code_h3, hyd_bfi_lfstat)

# Climat : précipitations, aridité, part de neige, fréquence des fortes pluies
climat <- read_delim(
  here::here(
    "data-raw",
    "CAMELS_FR_attributes",
    "time_series_statistics",
    "CAMELS_FR_climatic_statistics.csv"
  ),
  delim = ";",
  show_col_types = FALSE
) |>
  select(
    sta_code_h3,
    cli_prec_mean,
    cli_aridity_ou,
    cli_psol_frac_safran,
    cli_prec_freq_high
  )

# Station : position de l'exutoire (Lambert 93) et surface du bassin
station <- read_delim(
  here::here(
    "data-raw",
    "CAMELS_FR_attributes",
    "static_attributes",
    "CAMELS_FR_station_general_attributes.csv"
  ),
  delim = ";",
  show_col_types = FALSE
) |>
  select(sta_code_h3, sta_x_l93_snap, sta_y_l93_snap, sta_area_snap)

# Topographie : altitude, pente, densité de drainage
topographie <- read_delim(
  here::here(
    "data-raw",
    "CAMELS_FR_attributes",
    "static_attributes",
    "CAMELS_FR_topography_general_attributes.csv"
  ),
  delim = ";",
  show_col_types = FALSE
) |>
  select(sta_code_h3, top_altitude_mean, top_slo_mean, top_drainage_density)

# Hydrogéologie : karst, perméabilité, socle
hydrogeologie <- read_delim(
  here::here(
    "data-raw",
    "CAMELS_FR_attributes",
    "static_attributes",
    "CAMELS_FR_hydrogeology_attributes.csv"
  ),
  delim = ";",
  show_col_types = FALSE
) |>
  select(sta_code_h3, hgl_krs_karstic, hgl_permeability, hgl_thm_bedrock)

# Géologie : classe lithologique dominante (GLiM)
geologie <- read_delim(
  here::here(
    "data-raw",
    "CAMELS_FR_attributes",
    "static_attributes",
    "CAMELS_FR_geology_attributes.csv"
  ),
  delim = ";",
  show_col_types = FALSE
) |>
  select(sta_code_h3, geo_dom_class)

# Occupation du sol (CORINE Land Cover 2018, niveau 1) :
# 1 = territoires artificialisés, 3 = forêts et milieux semi-naturels
occupation_sol <- read_delim(
  here::here(
    "data-raw",
    "CAMELS_FR_attributes",
    "static_attributes",
    "CAMELS_FR_land_cover_attributes.csv"
  ),
  delim = ";",
  show_col_types = FALSE
) |>
  select(sta_code_h3, clc_2018_lvl1_3, clc_2018_lvl1_1)

# Barrages : volume des retenues rapporté à la surface du bassin
barrages <- read_delim(
  here::here(
    "data-raw",
    "CAMELS_FR_attributes",
    "static_attributes",
    "CAMELS_FR_human_influences_dams.csv"
  ),
  delim = ";",
  show_col_types = FALSE
) |>
  select(sta_code_h3, dam_influence)


## Join data
# On part des signatures (654 stations) et on ajoute les attributs de chaque station
camels <- signatures |>
  left_join(climat, by = "sta_code_h3") |>
  left_join(station, by = "sta_code_h3") |>
  left_join(topographie, by = "sta_code_h3") |>
  left_join(hydrogeologie, by = "sta_code_h3") |>
  left_join(geologie, by = "sta_code_h3") |>
  left_join(occupation_sol, by = "sta_code_h3") |>
  left_join(barrages, by = "sta_code_h3")
summary(camels)


## Remove catchments without target
# 42 stations n'ont pas d'indice de débit de base : il en reste 612
camels <- camels |> filter(!is.na(hyd_bfi_lfstat))


## Convert coordinates
# Lambert 93 (EPSG:2154) -> longitude / latitude WGS84 (EPSG:4326)
coords <- camels |>
  st_as_sf(coords = c("sta_x_l93_snap", "sta_y_l93_snap"), crs = 2154) |>
  st_transform(4326) |>
  st_coordinates()

camels <- camels |>
  mutate(longitude = coords[, "X"], latitude = coords[, "Y"]) |>
  select(-sta_x_l93_snap, -sta_y_l93_snap)


## Geological classes
# Codes GLiM (voir CAMELS_FR_attributes/static_attributes/00_description_geology_classes.txt),
# seules les 8 classes présentes comme classe dominante sont gardées
camels <- camels |>
  mutate(
    geo_dom_class = factor(
      geo_dom_class,
      levels = c("su", "ss", "sc", "sm", "va", "vb", "pa", "mt"),
      labels = c(
        "sediments_non_consolides",
        "roches_sedimentaires_siliciclastiques",
        "roches_carbonatees",
        "roches_sedimentaires_mixtes",
        "volcaniques_acides",
        "volcaniques_basiques",
        "plutoniques_acides",
        "metamorphiques"
      )
    )
  )

# Vérifie qu'aucun code n'a été oublié (il deviendrait NA)
stopifnot(!anyNA(camels$geo_dom_class))


## Colnames
bassins <- camels |>
  select(
    code_station = sta_code_h3,
    longitude,
    latitude,
    precip_moy = cli_prec_mean,
    aridite = cli_aridity_ou,
    frac_neige = cli_psol_frac_safran,
    freq_fortes_pluies = cli_prec_freq_high,
    surface = sta_area_snap,
    altitude_moy = top_altitude_mean,
    pente_moy = top_slo_mean,
    densite_drainage = top_drainage_density,
    karst = hgl_krs_karstic,
    permeabilite = hgl_permeability,
    socle = hgl_thm_bedrock,
    geologie = geo_dom_class,
    foret = clc_2018_lvl1_3,
    artificialise = clc_2018_lvl1_1,
    influence_barrages = dam_influence,
    bfi = hyd_bfi_lfstat
  )
bassins


## Use data
usethis::use_data(bassins, overwrite = TRUE)
