#########################################################################################################
#' # Panel dynamique
rm(list = ls())
#' Dans certains modèles (généralement en macroéconomie), la variable dépendante peut dépendre de son passé.
#' Ces modèles peuvent être estimés en utilisant la fonction dpd du package dynpanel
library(dynpanel)
data(Produc)   # Des données qui sont fournies avec le package
? Produc     # Pour en savoir plus sur les variables

#' p = 1 est le nombre de retards de la variable dépendante
#' meth = 4 correspond à Estimation GMM Arellano Bond (1991) 
reg <- dpd(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,Produc,index=c("state","year"), p = 1, meth = 4)
summary(reg)

#' Certaines variables macroéconomiques ne sont pas stationnaires. Elles sont intégrées d'ordre 1. 
#' L'utilisation de ces variables, sans faire aucune transformation au préalable, conduit à des régressions dites fallacieuses.
#' Il faut d'abord les stationnariser. 
#' Nous ne couvrons pas les tests de stationnarité en panel dans ce cours.