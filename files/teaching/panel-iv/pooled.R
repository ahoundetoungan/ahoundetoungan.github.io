rm(list = ls())
#' Dans cette application, nous allons utiliser les données NLS disponible sur le site du cours.
#' Dans un premier temps, nous allons télécharger la base de données et la sauvegarder dans 
#' un dossier de travail. 
#' Sur mon ordinateur, mon dossier de travail est "~/Dropbox/Teaching/Données de Panel et IV/Data"
#' 
#' ATTENTION, si vous travaillez sous Windows, il faudra changer les "\" du chemin vers votre dossier de travail 
#' par des "/". En effet, R ne reconnait pas "\" dans un chemin.
#' 
#' 
#' Nous allons ensuite définir notre dossier de travail dans R
setwd("~/Dropbox/Teaching/Données de Panel et IV/Data")

#' Etant donné que la base de données est dans le dossier de travail, on peut l'importer
#' R peut importer plusieurs types de données, .xlsx, .csv, .dat, .RDS, .RDA etc.
#' Dans cette application, les données sont sous format RDS. Nous utilisons donc la fonction readRDS

data_origin    <- readRDS("NLSData.RDS") 

#' Nous pouvons aussi charger la base de données directement depuis mon site sans la télécharger.

datatmp        <- readRDS(url("https://ahoundetoungan.com/files/teaching/panel-iv/NLSData.RDS")) 
rm(list = "datatmp") # Nous allons garder une seule base de données dans la mémoire au risque de la surcharger

#' Visualiser quelques lignes
head(data_origin)
#' Plus de 355000 individus, avec beaucoup de données manquantes.
#' Nous allons restreindre l'analyse sur la période 1979-1985

#' Actuellement, notre base de données est un data.frame standard. 
#' La librairie dplyr offre plusieurs outils pour manipuler facilement une base de données.
#' Mais il faut que cette base soit dans un format spécial qui est tibble.
#' Nous allons d'abord charger la librairie dplyr
library(dplyr) # veuillez l'installer si vous ne l'avez pas encore.
library(tidyr) # Il faut aussi charger cette librairie qui offre des fonctions supplémentaires

#' Nous allons ensuite convertir la base de data.frame vers la classe tibble qui offre plus de flexibilité.
data  <- as_tibble(data_origin)
class(data_origin)
class(data)

#' La présentation est différente. On a une meilleure présentation sous tibble
head(data_origin)
head(data)

#' On garde seulement les périodes 1979-1985 ainsi que ceux qui ont un salaires différent de NA et supérieur à 0
data  <- data %>% filter((year >= 1979 &  year <= 1986) & !(is.na(wage) | wage == 0))

#' On supprime aussi ceux qui ont des valeurs manquantes pour l'éducation
data  <- data %>% filter(!is.na(educ))

#' Convertir l'éducation en numérique
#' Actuellement elle a un format factor. On peut voir ses différents niveaux
levels(data$educ)
table(data$educ)
#' C'est comme si UNGRADED est le meilleur niveau
#' Il faut donc qu'on le recode comme NONE
data  <- data %>% mutate(educ = recode(educ, "UNGRADED" = "NONE"))
levels(data$educ)
table(data$educ)
data  <- data %>% mutate(educ_num = as.numeric(educ))

#' Nous allons également créer des variables muettes ou dummies, comme
#' Femme, Vivant au Sud, Vivant dans un SMSA, Marié, noir, Fonctionnaire public, travailleur manuel
#' age^2, education^2 pour capter des relations non-linéaires
data  <- data %>% mutate(femme     = (sex      == "FEMALE"),
                         sud       = (region   == "SOUTH"),
                         smsa      = (SMSA     == "Yes"),
                         marie     = (marital  == "MARRIED"),
                         noir      = (race     == "BLACK"),
                         fpub      = (work.cat == "GOVERNMENT"),
                         manue     = (occup %in% c("401 TO 575: 401-575 CRAFTSMEN,FOREMEN AND KINDRED", 
                                               "740 TO 785: 740-785 LABORERS, EXCEPT FARM",
                                               "801 TO 802: 801-802 FARMERS AND FARM MANAGERS",
                                               "821 TO 824: 821-824 FARM LABORERS AND FOREMAN",
                                               "901 TO 965: 901-965 SERVICE WORKERS, EXCEPT PRIVATE HOUSEHOLD",
                                               "980 TO 984: 980-984 PRIVATE HOUSEHOLD")),
                         age2      = age^2,
                         educ_num2 = educ_num^2)

#' Replacer NAs des variables muettes par FALSE
data  <- data %>% replace_na(list(femme = FALSE, sud = FALSE, smsa = FALSE,
                                  marie = FALSE, noir = FALSE, fpub = FALSE,
                                  manue = FALSE))

#'estimation du modèle pooled
pooled1 <- lm(wage ~ educ_num + educ_num2 + age + age2 + femme + sud + smsa + marie + noir + fpub + manue + week_worked, 
              data = data)
summary(pooled1)

#' Il y a également le package plm qui est dédié uniquement aux modèles avec les données de panel.
#' On peut l'utiliser en précisant que notre modèle est le "naïf" pooled.
#' L'avantage de ce package est qu'il permet d'estimer d'autres modèles de panel, autres que le pooled.
#' Ces modèles seront étudiés dans le chapitre 3.
library(plm)
pooled2 <- plm(wage ~ educ_num + educ_num2 + age + age2 + femme + sud + smsa + marie + noir + fpub + manue + week_worked, 
               data = data, model = "pooling")
summary(pooled2)
