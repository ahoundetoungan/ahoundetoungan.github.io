rm(list = ls())
library(dplyr)
library(tidyr)
library(AER)    # pour estimer des modèles avec variables instrumentales
library(lmtest) # pour faire des tests de spécification
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

#' Methode des MCO
mco <- lm(WKS ~ LWAGE + ED + UNION + FEM, data = data_orig)
summary(mco)

#' Méthode de variables instrumentales 
#' On soupçonne que le salaire est endogène. 
#' On a deux lots d'instruments pour cette variable.
#' On peut utiliser IND ou encore IND et SMSA
#' Dans le premier cas, la matrice d'instrument est Z1 = [IND ED UNION FEM]
#' Et dans le second cas,  Z2 = [IND SMSA ED UNION FEM].

#' Nous allons utiliser la méthode en 2 étapes: la variable endogène est régressée
#' sur la matrice d'instruments
#' # instrument Z1
iv1.step1     <- lm(LWAGE ~ IND + ED + UNION + FEM, data = data_orig)
summary(iv1.step1)
#' # instrument Z2
iv2.step1     <- lm(LWAGE ~ IND + SMSA + ED + UNION + FEM, data = data_orig)
summary(iv2.step1)

#' Ensuite on récupère la variable dépendante prédite
#' # instrument Z1
LWAGE1.fitted <- iv1.step1$fitted.values  # Ceci est donc la projection de LWAGE dans l'espace de Z1
#' # instrument Z2
LWAGE2.fitted <- iv2.step1$fitted.values  # Ceci est donc la projection de LWAGE dans l'espace de Z2

#' LWAGE sera remplacé par sa valeur prédite
#' # instrument Z1
iv1           <- lm(WKS ~ LWAGE1.fitted + ED + UNION + FEM, data = data_orig)
summary(iv1)
#' # instrument Z2
iv2           <- lm(WKS ~ LWAGE2.fitted + ED + UNION + FEM, data = data_orig)
summary(iv2)

#' On peut aussi utiliser la fonction ivreg de la librairie AER
#' la fonction exécute les différentes étapes et nous montre le résultat final.
#' # instrument Z1
tsls1         <- ivreg(formula     = WKS ~ LWAGE + ED + UNION + FEM,
                       instruments = ~ IND + ED + UNION + FEM, 
                       data        = data_orig)
summary(tsls1, diagnostics = TRUE)  #'nous permet aussi de faire des tests de diagnostic
#' # instrument Z2
tsls2         <- ivreg(formula     = WKS ~ LWAGE + ED + UNION + FEM,
                       instruments = ~ IND + SMSA + ED + UNION + FEM, 
                       data        = data_orig)
summary(tsls2, diagnostics = TRUE)

#' Test d'hétéroscédasticité
bptest(tsls1) # on rejette H0 donc il y a hétéscédasticité. Les tests de significativités ci-dessus ne sont pas valides
bptest(tsls2) # on rejette H0 donc il y a hétéscédasticité. Les tests de significativités ci-dessus ne sont pas valides

#' Correction des variances (variances robustes)
coeftest(tsls1, vcov = vcovHC)  
coeftest(tsls2, vcov = vcovHC)