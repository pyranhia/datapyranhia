library(keras3)

mnist <- dataset_mnist()

## Use data
usethis::use_data(mnist, overwrite = TRUE)
