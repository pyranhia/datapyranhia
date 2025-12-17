#' Données Titanic
#'
#' Jeu de données des passagers du Titanic, comprenant des informations
#' démographiques et sur la survie. Ce dataset est couramment utilisé
#' pour des exemples de régression, classification et apprentissage statistique.
#'
#' @format Un tibble avec 714 lignes et 8 variables :
#' \describe{
#'   \item{survived}{int 0 = non, 1 = oui (survie)}
#'   \item{pclass}{int Classe du billet (1, 2, 3)}
#'   \item{name}{chr Nom du passager}
#'   \item{sex}{chr Sexe du passager (male/female)}
#'   \item{age}{dbl Âge en années}
#'   \item{fare}{dbl Tarif payé}
#'   \item{sibsp}{int Nombre de frères/soeurs ou conjoints à bord}
#'   \item{parch}{int Nombre de parents/enfants à bord}
#' }
#'
#' @source
#' Dataset Titanic original provenant du package R \pkg{titanic} :
#' \url{https://cran.r-project.org/package=titanic}
"titanic"
