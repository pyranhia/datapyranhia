#' Données de performance des sportifs adapté pour les CART.
#'
#' Jeu de données synthétiques pour illustrer les concepts des arbres de régression et de classification (CART).
#'
#' @format Un tibble avec 500 lignes et 4 variables :
#' \describe{
#'   \item{heures}{dbl Heures d’entraînement par semaine}
#'   \item{experience}{dbl Annés d’expérience}
#'   \item{score}{dbl Score de performance}
#'   \item{set}{chr Dataset split : `"Train"` ou `"Test"`}
#' }
#'
"sportifs_cart"
