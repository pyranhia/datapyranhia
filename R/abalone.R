#' Données d'ormeaux (abalone)
#'
#' Mesures physiques d'ormeaux utilisées pour prédire leur âge. L'âge est
#' obtenu en comptant au microscope le nombre d'anneaux sur une coupe de la
#' coquille, une opération longue et fastidieuse. Ce jeu de données permet
#' d'estimer le nombre d'anneaux (et donc l'âge) à partir de mesures
#' physiques plus simples à obtenir, ce qui en fait un exemple classique de
#' régression.
#'
#' L'âge en années s'obtient en ajoutant 1.5 au nombre d'anneaux
#' (\code{rings}).
#'
#' @format Un tibble avec 4177 lignes et 9 variables :
#' \describe{
#'   \item{sex}{factor Sexe de l'ormeau : \code{"male"}, \code{"female"}, \code{"infant"}}
#'   \item{length}{dbl Longueur de la coquille la plus longue (mm)}
#'   \item{diameter}{dbl Diamètre, perpendiculaire à la longueur (mm)}
#'   \item{height}{dbl Hauteur, avec la chair dans la coquille (mm)}
#'   \item{whole_weight}{dbl Poids total de l'ormeau (g)}
#'   \item{shucked_weight}{dbl Poids de la chair seule (g)}
#'   \item{viscera_weight}{dbl Poids des viscères après saignée (g)}
#'   \item{shell_weight}{dbl Poids de la coquille après séchage (g)}
#'   \item{rings}{int Nombre d'anneaux (+1.5 donne l'âge en années)}
#' }
#'
#' @source
#' Nash, W., Sellers, T., Talbot, S., Cawthorn, A., & Ford, W. (1994).
#' *Abalone* \[Dataset\]. UCI Machine Learning Repository.
#' (\href{https://doi.org/10.24432/C55C7W}{DOI: 10.24432/C55C7W})
#' \url{https://archive.ics.uci.edu/dataset/1/abalone}.
#' Licence Creative Commons Attribution 4.0 International (CC BY 4.0).
"abalone"
