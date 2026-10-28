#' Indice de débit de base de bassins versants français
#'
#' Jeu de données décrivant 612 bassins versants de France métropolitaine
#' (continent et Corse) par leur climat, leur relief, leur sous-sol, leur
#' occupation du sol et l'influence des barrages. La variable \code{bfi}
#' (indice de débit de base) mesure la part du débit d'une rivière qui provient
#' des écoulements lents (nappes, soutien d'étiage) : proche de 0, le bassin
#' réagit vite aux pluies, proche de 1, il amortit les crues et soutient
#' les étiages.
#'
#' Les variables sont calculées à l'échelle du bassin versant, sauf
#' \code{longitude} et \code{latitude}, qui situent l'exutoire (la station de
#' mesure). Les variables climatiques sont des moyennes de long terme
#' (1970-2021). Les variables de sol et d'intensité horaire des pluies de
#' CAMELS-FR ne sont pas incluses, leur licence interdisant la redistribution.
#'
#' @format Un tibble avec 612 lignes et 19 variables :
#' \describe{
#'   \item{code_station}{character. Code de la station hydrométrique
#'   (référentiel Hydroportail).}
#'   \item{longitude}{double. Longitude de l'exutoire en degrés (WGS84,
#'   EPSG:4326).}
#'   \item{latitude}{double. Latitude de l'exutoire en degrés (WGS84,
#'   EPSG:4326).}
#'   \item{precip_moy}{double. Précipitations moyennes en mm/jour.}
#'   \item{aridite}{double. Indice d'aridité : rapport entre
#'   l'évapotranspiration potentielle (méthode d'Oudin) et les précipitations.
#'   Au-delà de 1, l'évaporation potentielle dépasse les apports de pluie.}
#'   \item{frac_neige}{double. Part des précipitations tombant sous forme de
#'   neige (entre 0 et 1).}
#'   \item{freq_fortes_pluies}{double. Nombre moyen de jours par an où les
#'   précipitations dépassent 5 fois la moyenne journalière.}
#'   \item{surface}{double. Surface du bassin versant en km².}
#'   \item{altitude_moy}{double. Altitude moyenne du bassin en mètres.}
#'   \item{pente_moy}{double. Pente moyenne du bassin en degrés.}
#'   \item{densite_drainage}{double. Densité de drainage : longueur totale
#'   des cours d'eau divisée par la surface du bassin, en km/km².}
#'   \item{karst}{double. Pourcentage de la surface du bassin en zone
#'   karstique.}
#'   \item{permeabilite}{double. Perméabilité moyenne du sous-sol, en
#'   log10(m²). Plus la valeur est élevée (moins négative), plus le sous-sol
#'   est perméable.}
#'   \item{socle}{double. Pourcentage de la surface du bassin sur un socle
#'   (roches cristallines ou métamorphiques peu perméables).}
#'   \item{geologie}{factor. Classe lithologique dominante du bassin
#'   (classification GLiM) : \code{"sediments_non_consolides"},
#'   \code{"roches_sedimentaires_siliciclastiques"},
#'   \code{"roches_carbonatees"}, \code{"roches_sedimentaires_mixtes"},
#'   \code{"volcaniques_acides"}, \code{"volcaniques_basiques"},
#'   \code{"plutoniques_acides"}, \code{"metamorphiques"}.}
#'   \item{foret}{double. Pourcentage de la surface du bassin en forêts et
#'   milieux semi-naturels (CORINE Land Cover 2018).}
#'   \item{artificialise}{double. Pourcentage de la surface du bassin en
#'   territoires artificialisés (CORINE Land Cover 2018).}
#'   \item{influence_barrages}{double. Volume total des retenues du bassin
#'   rapporté à sa surface. Vaut 0 pour la grande majorité des bassins
#'   (aucune retenue recensée).}
#'   \item{bfi}{double. Indice de débit de base (méthode lfstat), entre 0 et
#'   1 : part du débit provenant des écoulements lents.}
#' }
#'
#' @source
#' Delaigue O., Guimarães G. M., Brigode P., Génot B., Perrin C., Soubeyroux
#' J.-M., Janet B., Addor N. et Andréassian V. (2025),
#' *CAMELS-FR dataset: a large-sample hydroclimatic dataset for France to
#' explore hydrological diversity and support model benchmarking*,
#' Earth System Science Data, 17, 1461–1479.
#' (\href{https://doi.org/10.5194/essd-17-1461-2025}{DOI: 10.5194/essd-17-1461-2025})
#'
#' Jeu de données CAMELS-FR (INRAE), (\href{https://doi.org/10.57745/WH7FJR}{DOI: 10.57745/WH7FJR}).
#' Licence Creative Commons Attribution 4.0 International (CC BY 4.0).
#'
#' Sources des attributs utilisés :
#' \itemize{
#'   \item Hydrométrie : Hydroportail (SCHAPI), Licence Ouverte 2.0 (Etalab).
#'   \item Climat : SAFRAN / SIM2 (Météo-France), Licence Ouverte 2.0 (Etalab).
#'   \item Topographie : SRTM (NASA et NGA), CC BY 4.0, et BD ALTI v1.0 (IGN)
#'   pour la Corse, Licence Ouverte 2.0 (Etalab).
#'   \item Géologie : GLiM v1.0 (Hartmann et Moosdorf, 2012), CC BY 3.0.
#'   \item Perméabilité : GLHYMPS v2.0 (Huscroft et al., 2018), CC BY 4.0.
#'   \item Karst et socle : BDLISA v3.0 (BRGM), Licence Ouverte 2.0 (Etalab).
#'   \item Occupation du sol : CORINE Land Cover 2018, Licence Ouverte 1.0 (Etalab).
#'   \item Indices calculés par INRAE (HYCAR) : Licence Ouverte 2.0 (Etalab).
#' }
#'
"bassins"
