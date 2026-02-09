#' Dataset MNIST - Chiffres manuscrits
#'
#' Le dataset MNIST contient 70 000 images de chiffres manuscrits (0-9)
#' en niveaux de gris, divisées en ensemble d'entraînement (60 000) et
#' de test (10 000). C'est un dataset classique pour l'introduction aux
#' réseaux de neurones et CNN.
#'
#' @format Une liste avec deux éléments principaux :
#' \describe{
#'   \item{train}{Liste contenant :}
#'   \itemize{
#'     \item \code{x} : Array 3D (60000, 28, 28) - Images d'entraînement
#'           en niveaux de gris (valeurs 0-255)
#'     \item \code{y} : Vecteur entier (60000) - Labels des chiffres (0-9)
#'   }
#'   \item{test}{Liste contenant :}
#'   \itemize{
#'     \item \code{x} : Array 3D (10000, 28, 28) - Images de test
#'           en niveaux de gris (valeurs 0-255)
#'     \item \code{y} : Vecteur entier (10000) - Labels des chiffres (0-9)
#'   }
#' }
#'
#' @details
#' Chaque image est une matrice 28x28 pixels. Les valeurs de pixels
#' vont de 0 (noir) à 255 (blanc).
#'
#'
#' @source
#' UCI Machine Learning Repository (\href{https://doi.org/10.24432/C5WW7V}{DOI: 10.24432/C5WW7V}).
#' Créateurs : Y. LeCun, C. Cortes, C. J. Burges.
#'
#' @examples
#' # Charger le dataset
#' data(mnist)
#'
#' # Extraire les ensembles
#' x_train <- mnist$train$x
#' y_train <- mnist$train$y
#' x_test <- mnist$test$x
#' y_test <- mnist$test$y
#'
#' # Dimensions
#' dim(x_train)  # 60000 28 28
#' length(y_train)  # 60000
#'
#' # Visualiser une image
#' image(
#'   t(x_train[1, 28:1, ]), col = grey.colors(255),
#'   axes = FALSE,  asp = 1,
#'   main = paste("Chiffre:", y_train[1])
#' )
#'
"mnist"
