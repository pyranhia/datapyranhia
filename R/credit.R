#' Données de défaut de paiement de cartes de crédit (default of credit card clients)
#'
#' Informations sur 30 000 clients d'une banque taïwanaise titulaires d'une
#' carte de crédit, observés en septembre 2005 : montant du crédit accordé,
#' caractéristiques démographiques, retard de remboursement, montant facturé
#' et montant remboursé sur le mois.
#' L'objectif est de prédire si le client fera défaut de paiement le mois
#' suivant, ce qui en fait un exemple classique de classification binaire
#' avec des classes déséquilibrées (environ 22 % de défauts).
#'
#' Ce jeu de données est adapté de la version UCI, qui contient l'historique
#' des six mois d'avril à septembre 2005 : seul le mois le plus récent
#' (septembre 2005) est conservé, et la variable \code{months_late} est
#' dérivée du statut de remboursement d'origine (\code{PAY_0}). Tous les
#' montants sont exprimés en nouveaux dollars taïwanais (NT$).
#'
#' @format Un tibble avec 30 000 lignes et 9 variables :
#' \describe{
#'   \item{limit_bal}{dbl. Montant du crédit accordé (NT$), incluant le crédit
#'   individuel du client et le crédit complémentaire de son foyer.}
#'
#'   \item{sex}{factor. Sexe : \code{"male"}, \code{"female"}.}
#'
#'   \item{education}{factor. Niveau d'éducation :
#'   \code{"graduate_school"}, \code{"university"}, \code{"high_school"},
#'   \code{"other"}. La modalité \code{"other"} regroupe la modalité
#'   « autre » de la source d'origine et les codes non documentés (0, 5, 6).}
#'
#'   \item{marriage}{factor. Statut marital : \code{"married"},
#'   \code{"single"}, \code{"other"}. La modalité \code{"other"} regroupe la
#'   modalité « autre » de la source d'origine et le code non documenté (0).}
#'
#'   \item{age}{dbl. Âge du client (années).}
#'
#'   \item{months_late}{dbl. Retard de remboursement en septembre 2005, en
#'   nombre de mois (\code{0} si le client est à jour). Dérivé du statut de
#'   remboursement d'origine : les codes \code{-2}, \code{-1} et \code{0}
#'   (aucune consommation, remboursé dans les temps, paiement minimum
#'   effectué) sont ramenés à \code{0}. Dans la source, la valeur \code{9}
#'   correspond à 9 mois ou plus.}
#'
#'   \item{bill_amt}{dbl. Montant du relevé de facturation de septembre 2005
#'   (NT$). Peut être négatif (trop-perçu en faveur du client).}
#'
#'   \item{paid_amt}{dbl. Montant remboursé en septembre 2005 (NT$).}
#'
#'   \item{default}{factor. Défaut de paiement le mois suivant (variable
#'   cible) : \code{"no"}, \code{"yes"}.}
#' }
#'
#' @source
#' Yeh, I.-C. (2009). *Default of Credit Card Clients* \[Dataset\]. UCI
#' Machine Learning Repository.
#' (\href{https://doi.org/10.24432/C55S3H}{DOI: 10.24432/C55S3H})
#' \url{https://archive.ics.uci.edu/dataset/350/default+of+credit+card+clients}.
#' Licence Creative Commons Attribution 4.0 International (CC BY 4.0).
#' Données modifiées : colonnes d'historique réduites au dernier mois,
#' \code{months_late} dérivé du statut de remboursement.
#'
#' @references
#' Yeh, I.-C., & Lien, C.-H. (2009). The comparisons of data mining techniques
#' for the predictive accuracy of probability of default of credit card
#' clients. *Expert Systems with Applications*, 36(2), 2473-2480.
#' (\href{https://doi.org/10.1016/j.eswa.2007.12.020}{DOI: 10.1016/j.eswa.2007.12.020})
"credit"
