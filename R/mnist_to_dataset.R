# Déclaration des variables globales pour R CMD check
utils::globalVariables("mnist")

#' Préparer MNIST pour Keras/TensorFlow
#'
#' Convertit le dataset MNIST en datasets TensorFlow compatibles avec
#' validation split et batching.
#'
#' @param validation_split Proportion des données d'entraînement à utiliser
#'   pour la validation (par défaut 0.2)
#' @param batch_size Nombre d'exemples par batch (par défaut 64)
#' @param seed Graine aléatoire pour la reproductibilité (par défaut 123)
#'
#' @return Une liste avec trois éléments :
#' \describe{
#'   \item{train}{Dataset d'entraînement (tf_dataset)}
#'   \item{validation}{Dataset de validation (tf_dataset)}
#'   \item{test}{Dataset de test (tf_dataset)}
#' }
#'
#' @details
#' Cette fonction effectue automatiquement :
#' \itemize{
#'   \item La normalisation des pixels (division par 255)
#'   \item L'ajout de la dimension canal (grayscale)
#'   \item L'encodage one-hot des labels
#'   \item La création du split train/validation
#'   \item Le batching et shuffling
#' }
#'
#' Les datasets retournés sont des objets \code{tf_dataset} prêts à être
#' utilisés directement avec \code{fit()}, \code{evaluate()}, et
#' \code{predict()}. Les données sont des tensors TensorFlow et ne
#' nécessitent pas de conversion.
#'
#' Pour inspecter le contenu d'un batch (à des fins de débogage),
#' utilisez \code{as.array()} pour convertir les tensors en arrays R :
#' \preformatted{
#' iter <- reticulate::as_iterator(datasets$train)
#' batch <- iter_next(iter)
#' batch_x <- as.array(batch[[1]])  # Images
#' batch_y <- as.array(batch[[2]])  # Labels
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(keras3)
#'
#' # Utilisation basique
#' datasets <- mnist_to_dataset()
#'
#' # Personnaliser les paramètres
#' datasets <- mnist_to_dataset(
#'   validation_split = 0.2,
#'   batch_size = 64
#' )
#'
#' # Entraîner un modèle
#' model |>  fit(
#'   datasets$train,
#'   validation_data = datasets$validation,
#'   epochs = 10
#' )
#' }
mnist_to_dataset <- function(validation_split = 0.2,
                             batch_size = 64,
                             seed = 123) {

  # Vérifier que keras3 est disponible
  if (!requireNamespace("keras3", quietly = TRUE)) {
    stop("Le package 'keras3' est requis. Installez-le avec : install.packages('keras3')")
  }

  # Vérifier que tfdatasets est disponible
  if (!requireNamespace("tfdatasets", quietly = TRUE)) {
    stop("Le package 'tfdatasets' est requis. Installez-le avec : install.packages('tfdatasets')")
  }

  set.seed(seed)

  # Charger et normaliser MNIST
  x_train <- mnist$train$x / 255
  y_train <- keras3::to_categorical(mnist$train$y, 10)
  x_test <- mnist$test$x / 255
  y_test <- keras3::to_categorical(mnist$test$y, 10)

  # Ajouter dimension canal (grayscale) : 28x28 -> 28x28x1
  x_train <- keras3::array_reshape(x_train, c(dim(x_train), 1))
  x_test <- keras3::array_reshape(x_test, c(dim(x_test), 1))

  # Split train/validation
  n_samples <- dim(x_train)[1]
  n_train <- floor((1 - validation_split) * n_samples)
  train_indices <- sample(1:n_samples, size = n_train)
  val_indices <- setdiff(1:n_samples, train_indices)

  # Créer les datasets TensorFlow
  train_ds <- tfdatasets::tensor_slices_dataset(list(
    x_train[train_indices, , , , drop = FALSE],
    y_train[train_indices, ]
  )) |>
    tfdatasets::dataset_shuffle(buffer_size = 1000) |>
    tfdatasets::dataset_batch(batch_size)

  val_ds <- tfdatasets::tensor_slices_dataset(list(
    x_train[val_indices, , , , drop = FALSE],
    y_train[val_indices, ]
  )) |>
    tfdatasets::dataset_batch(batch_size)

  test_ds <- tfdatasets::tensor_slices_dataset(list(
    x_test,
    y_test
  )) |>
    tfdatasets::dataset_batch(batch_size)

  list(
    train = train_ds,
    validation = val_ds,
    test = test_ds
  )
}

