# Déclaration des variables globales pour R CMD check
utils::globalVariables("mnist")

#' @importFrom reticulate iter_next as_iterator
NULL

#' Préparer MNIST pour Keras/TensorFlow
#'
#' Convertit le dataset MNIST en datasets TensorFlow compatibles avec
#' validation split, redimensionnement et batching.
#'
#' @param validation_split Proportion des données d'entraînement à utiliser
#'   pour la validation (par défaut 0.2)
#' @param image_size Vecteur de taille 2 : (hauteur, largeur) en pixels.
#'   Par défaut c(28, 28). Utilisez c(48, 48) pour MobileNetV2.
#' @param to_rgb Booléen. Si TRUE, convertit les images grayscale en RGB
#'   (3 canaux). Par défaut FALSE.
#' @param batch_size Nombre d'exemples par batch (par défaut 64)
#' @param subsample Liste optionnelle avec éléments \code{train} et \code{test}
#'   pour sous-échantillonner les données. Par exemple :
#'   \code{list(train = 10000, test = 2000)}. Par défaut NULL (pas de sous-échantillonnage).
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
#'   \item L'ajout de la dimension canal (grayscale ou RGB)
#'   \item Le redimensionnement des images si nécessaire
#'   \item Le sous-échantillonnage si nécessaire
#'   \item La conversion en RGB si demandé
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
#' # Utilisation basique (28×28, grayscale)
#' datasets <- mnist_to_dataset()
#'
#' # Utilisation avec MobileNetV2 (48×48, RGB)
#' datasets <- mnist_to_dataset(
#'   image_size = c(48, 48),
#'   to_rgb = TRUE,
#'   batch_size = 64
#' )
#'
#' # Version sous-échantillonnée pour entraînement rapide
#' datasets <- mnist_to_dataset(
#'   image_size = c(48, 48),
#'   to_rgb = TRUE,
#'   subsample = list(train = 10000, test = 2000),
#'   seed = 123
#' )
#'
#' # Entraîner un modèle
#' model |> fit(
#'   datasets$train,
#'   validation_data = datasets$validation,
#'   epochs = 10
#' )
#' }
mnist_to_dataset <- function(validation_split = 0.2,
                             image_size = c(28, 28),
                             to_rgb = FALSE,
                             batch_size = 64,
                             subsample = NULL,
                             seed = 123) {

  # Vérifier que keras3 est disponible
  if (!requireNamespace("keras3", quietly = TRUE)) {
    stop("Le package 'keras3' est requis. Installez-le avec : install.packages('keras3')")
  }

  # Vérifier que tfdatasets est disponible
  if (!requireNamespace("tfdatasets", quietly = TRUE)) {
    stop("Le package 'tfdatasets' est requis. Installez-le avec : install.packages('tfdatasets')")
  }

  # Vérifier que tensorflow est disponible si redimensionnement ou RGB
  needs_tf <- !all(image_size == c(28, 28)) || to_rgb
  if (needs_tf && !requireNamespace("tensorflow", quietly = TRUE)) {
    stop("Le package 'tensorflow' est requis pour le redimensionnement ou la conversion RGB. Installez-le avec : install.packages('tensorflow')")
  }

  set.seed(seed)

  # Charger et normaliser MNIST
  x_train <- mnist$train$x / 255
  y_train <- keras3::to_categorical(mnist$train$y, 10)
  x_test <- mnist$test$x / 255
  y_test <- keras3::to_categorical(mnist$test$y, 10)

  # Sous-echantillonner si demande
  if (!is.null(subsample)) {
    if (!is.list(subsample) || !all(c("train", "test") %in% names(subsample))) {
      stop("subsample doit etre une liste avec les elements 'train' et 'test'")
    }

    # Sous-echantillonner train
    if (subsample$train < nrow(x_train)) {
      train_sample_idx <- sample(1:nrow(x_train), subsample$train)
      x_train <- x_train[train_sample_idx, , ]
      y_train <- y_train[train_sample_idx, ]
    }

    # Sous-echantillonner test
    if (subsample$test < nrow(x_test)) {
      test_sample_idx <- sample(1:nrow(x_test), subsample$test)
      x_test <- x_test[test_sample_idx, , ]
      y_test <- y_test[test_sample_idx, ]
    }
  }

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

  # Appliquer redimensionnement et/ou conversion RGB si nécessaire
  if (!all(image_size == c(28, 28)) || to_rgb) {
    transform_fn <- make_transform_fn(image_size, to_rgb)
    train_ds <- train_ds |> tfdatasets::dataset_map(transform_fn)
    val_ds <- val_ds |> tfdatasets::dataset_map(transform_fn)
    test_ds <- test_ds |> tfdatasets::dataset_map(transform_fn)
  }

  list(
    train = train_ds,
    validation = val_ds,
    test = test_ds
  )
}


#' Créer une fonction de transformation pour redimensionner et convertir en RGB
#'
#' @param target_size Vecteur c(hauteur, largeur)
#' @param to_rgb Booléen, convertir en RGB ?
#'
#' @return Fonction de transformation pour dataset_map
#' @keywords internal
#' @noRd
make_transform_fn <- function(target_size, to_rgb) {
  function(x, y) {
    # Redimensionner si nécessaire
    if (!all(target_size == c(28, 28))) {
      x <- tensorflow::tf$image$resize(x, size = as.integer(target_size))
    }

    # Convertir en RGB si demandé
    if (to_rgb) {
      x <- tensorflow::tf$image$grayscale_to_rgb(x)
    }

    list(x, y)
  }
}



#' Extraire images et/ou labels d'un tf_dataset
#'
#' Convertit un tf_dataset en arrays R pour inspection ou évaluation.
#' Utile pour récupérer les labels du test set ou visualiser des images.
#'
#' @param dataset Un tf_dataset (provenant de mnist_to_dataset())
#' @param what Chaîne de caractères indiquant ce qu'il faut extraire :
#'   "both" (défaut), "x" (images seulement), ou "y" (labels seulement)
#' @param labels_format Format des labels : "onehot" (défaut) ou "integer" (0-9)
#' @param max_samples Nombre maximum d'échantillons à extraire.
#'   NULL pour tout extraire (défaut NULL).
#'
#' @return Selon le paramètre \code{what} :
#' \describe{
#'   \item{both}{Liste avec \code{x} (images) et \code{y} (labels)}
#'   \item{x}{Array des images uniquement}
#'   \item{y}{Array ou vecteur des labels uniquement}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(keras3)
#'
#' # Créer les datasets
#' datasets <- mnist_to_dataset(
#'   image_size = c(96, 96),
#'   to_rgb = TRUE,
#'   subsample = list(train = 1000, test = 200)
#' )
#'
#' # Extraire images et labels (défaut : one-hot)
#' test_data <- extract_dataset(datasets$test)
#' dim(test_data$x)  # 200 96 96 3
#' dim(test_data$y)  # 200 10
#'
#' # Extraire seulement les images
#' x_test <- extract_dataset(datasets$test, what = "x")
#'
#' # Extraire seulement les labels en format entier (0-9)
#' y_test <- extract_dataset(datasets$test, what = "y", labels_format = "integer")
#'
#' # Extraire un échantillon pour inspection rapide
#' sample <- extract_dataset(datasets$train, max_samples = 100)
#'
#' # Visualiser une image
#' img <- sample$x[1, , , 1]
#' label <- which.max(sample$y[1, ]) - 1
#' image(t(img[96:1, ]), col = gray.colors(255),
#'       axes = FALSE, main = paste("Chiffre:", label))
#' }
extract_dataset <- function(dataset,
                            what = c("both", "x", "y"),
                            labels_format = c("onehot", "integer"),
                            max_samples = NULL) {

  what <- match.arg(what)
  labels_format <- match.arg(labels_format)

  all_x <- list()
  all_y <- list()
  n_samples <- 0

  iter <- reticulate::as_iterator(dataset)

  repeat {
    batch <- iter_next(iter, completed = NULL)
    if (is.null(batch)) break

    # Convertir en arrays R selon ce qui est demandé
    if (what %in% c("both", "x")) {
      batch_x <- as.array(batch[[1]])
      all_x[[length(all_x) + 1]] <- batch_x
    }

    if (what %in% c("both", "y")) {
      batch_y <- as.array(batch[[2]])
      all_y[[length(all_y) + 1]] <- batch_y
    }

    n_samples <- n_samples + dim(batch[[1]])[1]

    # Arrêter si on a atteint max_samples
    if (!is.null(max_samples) && n_samples >= max_samples) break
  }

  # Combiner les batches
  result <- list()

  if (what %in% c("both", "x")) {
    x <- do.call(abind::abind, c(all_x, list(along = 1)))

    # Tronquer si necessaire
    if (!is.null(max_samples) && dim(x)[1] > max_samples) {
      x <- x[1:max_samples, , , , drop = FALSE]
    }

    if (what == "x") {
      return(x)
    }
    result$x <- x
  }

  if (what %in% c("both", "y")) {
    y <- do.call(rbind, all_y)

    # Tronquer si necessaire
    if (!is.null(max_samples) && nrow(y) > max_samples) {
      y <- y[1:max_samples, , drop = FALSE]
    }

    # Convertir en entier si demande
    if (labels_format == "integer") {
      y <- apply(y, 1, which.max) - 1
    }

    if (what == "y") {
      return(y)
    }
    result$y <- y
  }

  result
}
