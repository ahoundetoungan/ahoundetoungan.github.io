rm(list = ls())
library(dplyr)
library(tidyr)
library(pglm)
library(ggplot2)
library(lme4)
library(bife)
#' Dans cette application, nous allons utiliser les données German Health Care, 
#' Ce sont des données de panel non équilibré, Panel disponibles sur le site du cours.
#' Nous pouvons directement charger la base de données depuis le site.
#' Elle est en format RDS
#' 
data_orig  <- readRDS(url("https://ahoundetoungan.com/files/teaching/panel-iv/rwm.RDS"))
data_orig        <- as_tibble(data_orig)
head(data_orig)

#' Pour en savoir plus sur la base de données, visiter
#' http://qed.econ.queensu.ca/jae/2003-v18.4/riphahn-wambach-million/readme.rwm.txt

#' Description des variables
#' id	       person - identification number
#' female	   female = 1; male = 0
#' year	     calendar year of the observation
#' age	     age in years
#' hsat	     health satisfaction, coded 0 (low) - 10 (high)
#' handdum	 handicapped = 1; otherwise = 0
#' handper	 degree of handicap in percent (0 - 100)
#' hhninc	   household nominal monthly net income in German marks / 1000
#' hhkids	   children under age 16 in the household = 1; otherwise = 0
#' educ	     years of schooling
#' married	 married = 1; otherwise = 0
#' haupts	   highest schooling degree is Hauptschul degree = 1; otherwise = 0
#' reals	   highest schooling degree is Realschul degree = 1; otherwise = 0
#' fachhs	   highest schooling degree is Polytechnical degree = 1; otherwise = 0
#' abitur	   highest schooling degree is Abitur = 1; otherwise = 0
#' univ	     highest schooling degree is university degree = 1; otherwise = 0
#' working	 employed = 1; otherwise = 0
#' bluec	   blue collar employee = 1; otherwise = 0
#' whitec	   white collar employee = 1; otherwise = 0
#' self	     self employed = 1; otherwise = 0
#' beamt	   civil servant = 1; otherwise = 0
#' docvis	   number of doctor visits in last three months
#' hospvis	 number of hospital visits in last calendar year
#' public	   insured in public health insurance = 1; otherwise = 0
#' addon	   insured by add-on insurance = 1; otherswise = 0

#' Nombre d'individus 
length(unique(data_orig$id))
#' Nombre de pétiodes
table(data_orig$year)

#' Nous allons modéliser les visites chez le médecin.
#' La variable docvis compte le nombre de visites
#' On peut faire un graphique pour voir l'évolution de cette variable pour 10 individus
#' pris de façon aléatoire.
ggplot(data = data_orig %>% filter(id %in% sample(id, 10)), aes(x = year, y = docvis, colour = factor(id))) +
  geom_point() + geom_line()

#' Nous allons transformer cette variable en binaire. 1 s'il y a visite et 0 sinon
data_orig <- data_orig %>% mutate(visite = ifelse(docvis > 0, 1, 0))

#' Comme dans le cas du modèle linéaire, le modèle binaire peut être aussi pooled.
#' la distribution des erreurs peut être normale (modèle probit)
#' ou logistique (modèle logit)
pooled.probit  <- pglm(visite ~ age + hhninc + hhkids + educ + married, family = binomial('probit'),
                     model = "pooling", index = c("id", "year"), data = data_orig)
summary(pooled.probit)

pooled.logit   <- pglm(visite ~ age + hhninc + hhkids + educ + married, family = binomial('logit'),
                     model = "pooling", index = c("id", "year"), data = data_orig)
summary(pooled.logit)

#' Malheureusement, la fonction pglm ne permet pas d'estimer avec effets fixes
#' Nous faisons recourt à une autre function (glmer) du package lme4, 
#' Model à effets aléatoires. La partie (1 | id) indique que l'intercept varie en fonction de l'id.
#' Avec la condition effets aléatoires
#' Modèle probit
random.probit  <- glmer(visite ~ age + hhninc + hhkids + educ + married + (1 | id), family = binomial('probit'),
                        data = data_orig)
summary(random.probit)

#' Modèle logit
random.logit   <- glmer(visite ~ age + hhninc + hhkids + educ + married + (1 | id), family = binomial('logit'),
      data = data_orig)
summary(random.logit)

#' Effets fixes
#' Nous utilisons la fonction bife du package bife
fixed.probit   <- bife(visite ~ age + hhninc + hhkids + educ + married | id, data = data_orig, model = "probit")
fixed.logit    <- bife(visite ~ age + hhninc + hhkids + educ + married | id, data = data_orig, model = "logit")

#' Comme nous l'avons vu en cours, les modèles non linéaires ne sont pas convergents car ils souffrent
#' d'un problème de paramètres incidents. La vraisemblance de chamberlain peut être utilisée seulement dans 
#' le cas des modèles logit. Toutefois, l'estimation pécédende n'utilise pas la vraisemblance de chamberlain
#' Ils sont tous biaisés 
#' Les auteurs du package bike ont proposé une méthode de correction du biais. Cette méthode fonctionne
#' avec le modèle probit et le modèle logit
#' Modèle probit
fixed.probit.c <- bias_corr(fixed.probit)
summary(fixed.probit.c)

#' Modèle logit
fixed.logit.c  <- bias_corr(fixed.logit)
summary(fixed.logit.c)

#' Effets marginaux
#' Modèle probit
summary(get_APEs(fixed.probit.c))

#' Modèle plogit
summary(get_APEs(fixed.logit.c))
