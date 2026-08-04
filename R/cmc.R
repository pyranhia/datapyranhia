#' Données de choix de méthode contraceptive (CMC)
#'
#' Sous-ensemble de l'enquête nationale indonésienne sur la prévalence de la
#' contraception de 1987 (National Indonesia Contraceptive Prevalence
#' Survey). L'échantillon est composé de femmes mariées qui n'étaient pas
#' enceintes ou ne savaient pas si elles l'étaient au moment de l'entretien.
#' L'objectif est de prédire la méthode contraceptive utilisée à partir de
#' caractéristiques démographiques et socio-économiques, ce qui en fait un
#' exemple classique de classification multi-classe.
#'
#' @format Un tibble avec 1473 lignes et 10 variables :
#' \describe{
#'   \item{wife_age}{int Âge de l'épouse (années)}
#'
#'   \item{wife_edu}{factor ordonné. Niveau d'éducation de l'épouse :
#'   \code{"low"}, \code{"medium_low"}, \code{"medium_high"}, \code{"high"}.}
#'
#'   \item{husband_edu}{factor ordonné. Niveau d'éducation du mari :
#'   \code{"low"}, \code{"medium_low"}, \code{"medium_high"}, \code{"high"}.}
#'
#'   \item{num_children}{int Nombre d'enfants nés}
#'
#'   \item{wife_religion}{factor Religion de l'épouse :
#'   \code{"non_islam"}, \code{"islam"}.}
#'
#'   \item{wife_working}{factor L'épouse travaille-t-elle actuellement :
#'   \code{"yes"}, \code{"no"}.}
#'
#'   \item{husband_occupation}{factor Catégorie professionnelle du mari :
#'   \code{"cat1"}, \code{"cat2"}, \code{"cat3"}, \code{"cat4"} (catégories
#'   anonymisées par la source d'origine).}
#'
#'   \item{standard_of_living_index}{factor ordonné. Indice de niveau de vie
#'   du foyer : \code{"low"}, \code{"medium_low"}, \code{"medium_high"},
#'   \code{"high"}.}
#'
#'   \item{media_exposure}{factor Exposition aux médias :
#'   \code{"good"}, \code{"not_good"}.}
#'
#'   \item{contraceptive_method}{factor Méthode contraceptive utilisée
#'   (variable cible) : \code{"no_use"}, \code{"long_term"},
#'   \code{"short_term"}.}
#' }
#'
#' @source
#' Lim, T. (1999). *Contraceptive Method Choice* \[Dataset\]. UCI Machine
#' Learning Repository.
#' (\href{https://doi.org/10.24432/C59W2D}{DOI: 10.24432/C59W2D})
#' \url{https://archive.ics.uci.edu/dataset/30/contraceptive+method+choice}.
#' Licence Creative Commons Attribution 4.0 International (CC BY 4.0).
"cmc"
