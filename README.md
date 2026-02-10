# datapyranhia

Datasets pour les formations [Pyranhia](https://www.pyranhia.eu).

[![R-CMD-check](https://github.com/pyranhia/datapyranhia/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pyranhia/datapyranhia/actions/workflows/R-CMD-check.yaml)

## Installation

``` r
# Méthode recommandée (plus rapide)
# install.packages("pak")
pak::pak("pyranhia/datapyranhia")

# Alternative
# install.packages("remotes")
remotes::install_github("pyranhia/datapyranhia")
```

## Utilisation

``` r
library(datapyranhia)

# Les datasets sont directement accessibles
data(titanic)
head(titanic)

# Pour MNIST avec Keras
library(keras3)
datasets <- mnist_to_dataset(validation_split = 0.2, batch_size = 64)
```

## Datasets disponibles

### Formation : Machine Learning avec R

| Dataset | Description | Taille | Tâche ML |
|-----------------|----------------------|-----------------|-----------------|
| `adult` | Revenus et caractéristiques démographiques | 32561 × 15 | Classification binaire |
| `housing` | Prix de l'immobilier en Californie | 20640 × 9 | Régression |
| `mnist` | Chiffres manuscrits 0-9 (images 28×28) | 70000 images | Classification 10 classes |
| `sportif` | Données de performances de sportifs (exemple pédagogique) | 500 × 3 | Démonstration ML |
| `titanic` | Survie des passagers du Titanic | 891 × 12 | Classification binaire |
| `wine` | Qualité des vins | 6497 × 12 | Régression/Classification |

**Fonction utilitaire :** `mnist_to_dataset()` prépare MNIST pour Keras (normalisation, split train/val, batching).

## Documentation

Chaque dataset est documenté avec sa source et ses conditions d'utilisation :

``` r
?adult
?mnist
?mnist_to_dataset
# etc.
```

## Contact

Pour toute question sur les formations : [contact\@pyranhia.eu](mailto:contact@pyranhia.eu) ou [www.pyranhia.eu](https://www.pyranhia.eu)
