#' Données de défaut de paiement de cartes de crédit (default of credit card clients)
#'
#' Informations sur 30 000 clients d'une banque taïwanaise titulaires d'une
#' carte de crédit, observés d'avril à septembre 2005 : montant du crédit
#' accordé, caractéristiques démographiques, historique des retards de
#' paiement, montants facturés et montants remboursés sur six mois.
#' L'objectif est de prédire si le client fera défaut de paiement le mois
#' suivant, ce qui en fait un exemple classique de classification binaire
#' avec des classes déséquilibrées (environ 22 % de défauts).
#'
#' Tous les montants sont exprimés en nouveaux dollars taïwanais (NT$). Les
#' variables suffixées de \code{1} à \code{6} remontent dans le temps :
#' \code{1} correspond à septembre 2005, \code{2} à août 2005, ..., \code{6}
#' à avril 2005.
#'
#' @format Un tibble avec 30 000 lignes et 24 variables :
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
#'   \item{pay_1, pay_2, pay_3, pay_4, pay_5, pay_6}{dbl. Statut de
#'   remboursement de septembre 2005 (\code{pay_1}) à avril 2005
#'   (\code{pay_6}) : \code{-1} = remboursé dans les temps, \code{1} à
#'   \code{9} = nombre de mois de retard (\code{9} = 9 mois ou plus). Les
#'   valeurs \code{-2} et \code{0}, présentes dans les données mais non
#'   documentées par la source, sont généralement interprétées comme
#'   « aucune consommation » et « paiement minimum effectué (crédit
#'   renouvelable) ». La variable \code{pay_1} s'appelle \code{PAY_0} dans
#'   le fichier d'origine.}
#'
#'   \item{bill_amt1, bill_amt2, bill_amt3, bill_amt4, bill_amt5, bill_amt6}{dbl.
#'   Montant du relevé de facturation (NT$) de septembre 2005
#'   (\code{bill_amt1}) à avril 2005 (\code{bill_amt6}). Peut être négatif
#'   (trop-perçu en faveur du client).}
#'
#'   \item{pay_amt1, pay_amt2, pay_amt3, pay_amt4, pay_amt5, pay_amt6}{dbl.
#'   Montant remboursé (NT$) en septembre 2005 (\code{pay_amt1}) à avril
#'   2005 (\code{pay_amt6}).}
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
#'
#' @references
#' Yeh, I.-C., & Lien, C.-H. (2009). The comparisons of data mining techniques
#' for the predictive accuracy of probability of default of credit card
#' clients. *Expert Systems with Applications*, 36(2), 2473-2480.
#' (\href{https://doi.org/10.1016/j.eswa.2007.12.020}{DOI: 10.1016/j.eswa.2007.12.020})
"credit"
