# datapyranhia

Datasets pour les formations [Pyranhia](https://www.pyranhia.eu).

[![R-CMD-check](https://github.com/pyranhia/datapyranhia/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pyranhia/datapyranhia/actions/workflows/R-CMD-check.yaml)

## Installation

``` r
# Méthode recommandée
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

# Récupérer les données de test sous forme d'arrays R
x_test <- extract_dataset(datasets$test, what = "x")
y_test <- extract_dataset(datasets$test, what = "y", labels_format = "integer")
```

## Datasets disponibles

### Formation : Machine Learning avec R

| Dataset | Description | Taille | Tâche ML |
|-----------------|----------------------|-----------------|-----------------|
| `abalone` | Mesures physiques d'ormeaux pour prédire leur âge | 4 177 × 9 | Régression ou classification |
| `adult` | Revenus et caractéristiques démographiques | 48 842 × 13 | Classification binaire |
| `ames` | Prix de l'immobilier à Ames, Iowa | 2 930 × 74 | Régression |
| `bike_sharing` | Locations quotidiennes de vélos (Capital Bikeshare, Washington D.C., 2011-2012) | 731 × 14 | Régression |
| `cmc` | Choix de méthode contraceptive (enquête Indonésie 1987) | 1 473 × 10 | Classification multi-classe |
| `credit` | Défauts de paiement de cartes de crédit (banque taïwanaise, 2005) | 30 000 × 24 | Classification binaire (classes déséquilibrées) |
| `energy` | Charges thermiques des bâtiments | 768 × 10 | Régression |
| `fires` | Feux de forêt dans le parc de Montesinho | 517 × 13 | Régression |
| `heart` | Diagnostic de maladie cardiaque | 303 × 14 | Classification binaire |
| `housing` | Prix de l'immobilier en Californie | 20 640 × 10 | Régression |
| `mnist` | Chiffres manuscrits 0-9 (images 28×28) | 70 000 images | Classification 10 classes |
| `sportifs` | Performances de sportifs (données synthétiques, split train/test inclus) | 500 × 4 | Régression |
| `students` | Résultats scolaires des étudiants en mathématiques | 395 × 14 | Régression ou classification |
| `titanic` | Survie des passagers du Titanic | 714 × 8 | Classification binaire |
| `wine` | Qualité de vins rouges | 1 143 × 12 | Régression ou classification |

**Fonctions utilitaires :**

- `mnist_to_dataset()` prépare MNIST pour Keras : normalisation, split train/validation, batching, sous-échantillonnage optionnel (`subsample`), redimensionnement (`image_size`) et conversion en RGB (`to_rgb`), par exemple pour du transfer learning avec MobileNetV2.
- `extract_dataset()` convertit un `tf_dataset` renvoyé par `mnist_to_dataset()` en arrays R, pratique pour inspecter les données ou calculer des métriques.

## Documentation

Chaque dataset est documenté avec sa source et ses conditions d'utilisation :

``` r
?adult
?mnist
# etc.
```

## Contact

Pour toute question sur les formations : [contact\@pyranhia.eu](mailto:contact@pyranhia.eu) ou [www.pyranhia.eu](https://www.pyranhia.eu)
