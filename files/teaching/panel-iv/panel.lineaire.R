rm(list = ls())
library(dplyr)
library(tidyr)
library(plm)
library(ggplot2) # pour faire des graphiques
#' Dans cette application, nous allons utiliser les données de Cornwell and Rupert (1988).
#' Elles sont disponibles sur le site du cours.
#' Nous pouvons directement charger la base de données depuis le site.
#' Elle est en format csv.

data_orig        <- read.csv(url("https://ahoundetoungan.com/files/teaching/panel-iv/WAGES.csv")) 
data_orig        <- as_tibble(data_orig)
head(data_orig)

#' Description de la base
#' ID  = Individual's ID
#' Year = Observation year
#' EXP = years of full time work experience,
#' WKS = weeks worked,
#' OCC = 1 if blue-collar occupation, 0 if not,
#' IND = 1 if the individual works in a manufacturing industry, 0 if not,
#' SOUTH = 1 if the individual resides in the south, 0 if not,
#' SMSA = 1 if the individual resides in an SMSA, 0 if not,
#' MS = 1 if the individual is married, 0 if not,
#' UNION = 1 if the individual wage is set by a union contract, 0 if not,
#' ED = years of education,
#' FEM = 1 if the individual is female, 0 if not,
#' BLK = 1 if the individual is black, 0 if not.
#' LWAGE = logarithm of the wage

#' Ajout des variables Expérience au carré et Education au carré
data_orig     <- data_orig %>% mutate(EXP2 = EXP^2, ED2 = ED^2)

#' Graphique
#' On sélectionne de façon aléatoire 20 individus
ggplot(data = data_orig %>% filter(ID %in% sample(ID, 20)), aes(x = YEAR, y = LWAGE, colour = ID)) +
  geom_line()

#' Modèle pooled
pooled        <- plm(LWAGE ~ ED + ED2 + EXP + EXP2 + WKS + OCC + IND + SOUTH + SMSA + MS + UNION + FEM + BLK,
                     data = data_orig, model = "pooling")
summary(pooled)

#' Between
between       <- plm(LWAGE ~ ED + ED2 + EXP + EXP2 + WKS + OCC + IND + SOUTH + SMSA + MS + UNION + FEM + BLK,
                     data = data_orig,  index = c("ID", "YEAR"), model="between")
summary(between)

#' Within ou effets fixes
fixed         <- plm(LWAGE ~ -1 + ED + ED2 + EXP + EXP2 + WKS + OCC + IND + SOUTH + SMSA + MS + UNION + FEM + BLK,
                     data = data_orig,  index = c("ID", "YEAR"), model="within")  
#' remarquer qu'il y a -1 dans la formule
summary(fixed) # Le coefficient de certaines variables n'a pas pu être calculé (Problème d'identification)

#' # Une autre manière d'estimer le modèle à effets fixe est d'estimer pooling avec des variables muettes individuelles.
fixed.prime   <- plm(LWAGE ~ -1 + ED + ED2 + EXP + EXP2 + WKS + OCC + IND + SOUTH + SMSA + MS + UNION + FEM + BLK + factor(ID),
                     data = data_orig,  index = c("ID", "YEAR"), model="pooling")  
summary(fixed.prime) # C'est plus intéressant comme présentation car le coefficient de l'éducation est calculé. Expliquer cette différence.

#' Première différence
FDiff         <- plm(LWAGE ~ -1 + ED + ED2 + EXP + EXP2 + WKS + OCC + IND + SOUTH + SMSA + MS + UNION + FEM + BLK,
                     data = data_orig,  index = c("ID", "YEAR"), model="fd") # remarquer le -1 dans la formule
summary(FDiff) # Le coefficient de certaines variables n'a pas pu être calculé (Problème d'identification)

#' Effets aléatoires
random        <- plm(LWAGE ~ ED + ED2 + EXP + EXP2 + WKS + OCC + IND + SOUTH + SMSA + MS + UNION + FEM + BLK,
                     data = data_orig,  index = c("ID", "YEAR"), model="random")
summary(random)

#' Two way fixed effects: Effets individuels et effets temporels
twoways       <- plm(LWAGE ~ -1 + ED + ED2 + EXP + EXP2 + WKS + OCC + IND + SOUTH + SMSA + MS + UNION + FEM + BLK,
                     data = data_orig,  index = c("ID", "YEAR"), model="within", effect = "twoways")
summary(twoways) # Un nouveau coefficient disparu (Expérience). Expliquer la raison.

#'# Une autre manière d'estimer le twoway est d'utiliser un modèle avec effets fixes et ajouter des variables muettes temporelles.
twoways.prime  <- plm(LWAGE ~ -1 + ED + ED2 + EXP + EXP2 + WKS + OCC + IND + SOUTH + SMSA + MS + UNION + FEM + BLK + factor(YEAR),
                      data = data_orig,  index = c("ID", "YEAR"), model="within")
summary(twoways.prime) # Le coefficient est pourtant calculé ici. Expliquer la raison.

#' Quel modèle choisir?
#' Puisque le modèle avec effets fixes est généralement plus efficace que le between et la première différence,
#' notre choix se fera entre pooling, within, random et twoways.
#' Oui, pooling aussi parce que ce n'est pas parce qu'on a des données de panel que la dimension panel compte.
#' Dans certains cas, le pooling peut être même le modèle privilégié

#' Le choix de fait de cette façon:
#' ETAPE 1: Choisir entre pooling et effets fixes.
#' Le choix ne se fait pas entre pooling et effets aléatoires car les effets fixes sont toujours convergents.
#' Donc s'il y a hétérogénéité individuelle, le pooling devrait être biaisé et donc très différent des effets fixes.
#' Deux tests possibles

#' Test 1: Test de significativité des effets (test de fisher)
pFtest(fixed, pooled)   # Il compare fixed et pooled
pFtest(fixed.prime, pooled)   # On peut aussi l'effectuer avec fixed.prime

#' Test 2: Test de multiplicateur de lagrange
#' Il teste une mauvaise spécification (est-ce qu'il manque des effets individuels?)
plmtest(pooled, c("individual"), type=("bp")) # on rejette l'hypothèse nulle d'absence d'hétérogénéité

#' ETAPE 2: A cette étape, on sait que le modèle pooled ne sera pas considéré (deux tests l'ont confirmé). 
#' On se pose donc la question de savoir si l'hétérogénéité individuelle est fixe ou aléatoire
#' Nous allons comparer fixed avec random (test de Hausman)
phtest(fixed, random) # un modèle ne converge pas, c'est sans doute les effets aléatoires. Les effets sont donc fixes

#' ETAPE 3: On sait maintenant que les effets sont fixes. Le modèle random est aussi rejeté.
#' Il faut donc comparer les effets individuels fixes à effets fixes individuels et temporels (twoways fixed effects)
#' Deux tests sont possibles.

#' Test 1: Test de significativité des effets (test de fisher)
pFtest(twoways, fixed)       # on rejette le oneway car les effets temporels sont significatifs
pFtest(twoways.prime, fixed) # on peut aussi faire le test avec twoways.prime

#' Test 2: Test de multiplicateur de lagrange
#' Il teste une mauvaise spécification dans le fixed (est-ce qu'il manque des effets temporels?)
plmtest(fixed, c("time"), type = ("bp"))

#' Le modèle à garder est donc le modèle twoways.
#' Une question importante. Est-ce que la variance est robuste?
#' On teste d'abord s'il y a autocorrélation des erreurs.
pbgtest(twoways)  # Oui les erreurs sont autocorrélées
#' On teste ensuite s'il y a hétéroscédasticité
bptest(twoways)  # Il y a aussi hétéroscédasticité

#' Il faut corriger la variance avec une variante robuste à l'autocorrélation et l'hétéroscédasticité.
coeftest(twoways)  # variances originales
coeftest(twoways, vcovHC) # Variances robustes à l'autocorrélation et l'hétéroscédasticité.

#' Puisqu'on n'a pas le coefficient de l'expérience, c'est plus intéressant d'utiliser twoways.prime
coeftest(twoways.prime, vcovHC) # Variances robustes à l'autocorrélation et l'hétéroscédasticité.

#' Si la taille d'échantillon (dimension individuelle) est faible comme dans notre cas,
#' on ignore parfois l'autocorrélatoin et on traite seulement l'hétéroscédasticité comme ci-dessous.
coeftest(twoways, vcovHC(random, type = "HC3"))

#' Dans une certaine mesure, on peut aussi garder le modèle à effets fixes (one way).
#' En effet, dans le twoway, les tests montrent que l'ensemble des périodes compte.
#' Mais individuellement, aucune période n'est significative. On peut le constater avec
#' twoways.prime. Il devient donc discutable de rejeter le modèle oneway ou non.
#' De toutes les façons, les deux donnent à peu près les mêmes résultats.

pbgtest(fixed)  # Oui les erreurs sont autocorrélées
bptest(fixed)   # Il y a aussi hétéroscédasticité
coeftest(fixed, vcovHC) # Variances robustes à l'autocorrélation et l'hétéroscédasticité.
coeftest(fixed, vcovHC(random, type = "HC3")) # Variances seulement robustes à l'hétéroscédasticité.



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
