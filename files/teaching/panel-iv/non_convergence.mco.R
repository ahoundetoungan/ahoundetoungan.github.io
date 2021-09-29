#' Nous allons montrer que la méthode des MCO ne fonctionne pas lorsque H3 n'est pas vérifiée
#' Pour ce faire, nous allons nous mettre dans un contexte théorique comme dans le TP convergence.mco.R.
#' Nous allons simuler des données de sorte qu'une variable explicative soit endogène.
#' Ensuite, nous allons utiliser les données générées et estimer le modèle avec la méthode des MCO.
#' L'objectif de comparer les valeurs estimées aux paramètres que nous avons définis

################################# PREMIER EXEMPLE ############################################
#' Dans cet exemple nous allons imposer une corrélation entre X2 et l'erreur
#' 
#' Génération des données
N  <- 500 # taille de l'échantillon
#' # Deux variables explicatives
set.seed(2021)
X1 <- rpois(N, 2)

#' # On simule X2 et l'erreur ensemble afin d'imposer la corrélation entre les deux.
#' # Pour ce faire nous allons utiliser la librairie MASS. Cette librairie permet 
#' # de simuler des données provenant d'une loi normale multivarieée
library(MASS)
mu     <- c(0, 0)  # le vecteur de moyennes des deux variables
varX2  <- 1        # la variance de la variable X2
vare   <- 0.7^2    # la variance de l'erreur
coreX2 <- 0.8      # la corrélation linéaire entre X2 et e

#' # Nous pouvons maintenant définir la matrice de covariance de (X2, e)
#' # La matrice de covariance de deux variables s'écrit comme suit:
#' #                |__________________________________________________________|
#' #                |VarX2                         sqrt(varX2)sqrt(vare)coreX2 |
#' #                |sqrt(varX2)sqrt(vare)coreX2                          Vare |
#' #                |__________________________________________________________|

coveX2 <- matrix(c(varX2, sqrt(varX2)*sqrt(vare)*coreX2, sqrt(varX2)*sqrt(vare)*coreX2, vare), nrow = 2)

#' # On simule X2 et e ensemble
X2e    <- mvrnorm(n = N, mu = mu, Sigma = coveX2)

#' # Extraire X2 et e du vecteur X2e
X2     <- X2e[,1]
e      <- X2e[,2]

#' # coefficients
b0    <- 1
b1    <- 0.7
b2    <- -1.2

#' # y est ensuite généré comme y = b0 + b1 x X1 + b2 x X2 + e
y     <- b0 + b1*X1 + b2*X2 + e

#' # Des graphiques
plot(X1, y)
plot(X2, y)

#' # Nous allons constituer la base de données.
#' # La fonction data.frame permet d'assembler toutes les variables nécessaires dans une même base.

base  <- data.frame(y = y, X1 = X1, X2 = X2)

#' # Nous allons ensuite supprimer tous les objets et ne garder que base car, c'est elle qui nous
#' # intéresse
rm(list = ls()[ls() != "base"])

#' # Nous allons ensuite estimer le modèle par MCO en utilisant la commande lm

model <- lm(y ~ X1 + X2, data = base)
summary(model)

#' # En augmentant la taille de l'échantillon, on peut remarquer que les valeurs estimées
#' # sont différentes des valeurs définies
#' # Ceci est une illustration de la non convergence en probabilité de l'estimateur des MCO,
#' # car X2 et corrélée à e. L'hypothèse H3 n'est pas vérifiée



rm(list = ls())
###################################### DEUXIEME EXEMPLE #####################################
#' Dans cet exemple, nous simulons le patrimoine (y) qui va dépendre du niveau d'éducation (X1)
#' et du salaire (X2). Nous allons aussi imposer une corrélation entre le niveau d'éducation
#' et le salaire

#' Génération des données
N       <- 500 
mu      <- c(16, 2000)  # le vecteur de moyennes de X1 et X2
varX1   <- 3^2          # la variance de X1
varX2   <- 300^2        # la variance de X2
corX1X2 <- 0.8          # la corrélation linéaire entre X2 et X1
#' # Nous pouvons maintenant définir la matrice de covariance de (X1, X2)
covX1X2 <- matrix(c(varX1, sqrt(varX1)*sqrt(varX2)*corX1X2, sqrt(varX1)*sqrt(varX2)*corX1X2, varX2), nrow = 2)

#' # On simule l'éducation et le salaire ensemble
X1X2    <- mvrnorm(n = N, mu = mu, Sigma = covX1X2)

#' # Extraire l'éducation et le salaire
education    <- X1X2[,1]
salaire      <- X1X2[,2]

#' # coefficients
b0           <- 300
b1           <- 150
b2           <- 1.2
sigma        <- 100

#' # le patrimoine est ensuite généré comme patrimoine = b0 + b1 x education + b2 x salaire + e
patrimoine  <- b0 + b1*education + b2*salaire + rnorm(N, 0, sigma)

#' # Des graphiques
plot(education, patrimoine)
plot(salaire, patrimoine)
plot(salaire, education)

#' # Nous allons constituer la base de données.
#' # La fonction data.frame permet d'assembler toutes les variables nécessaires dans une même base.
base  <- data.frame(education = education, salaire = salaire, patrimoine = patrimoine)

#' # Nous allons ensuite supprimer tous les objets et ne garder que base car, c'est elle qui nous
#' # intéresse
rm(list = ls()[ls() != "base"])

#' # Nous allons ensuite estimer le modèle par MCO en utilisant la commande lm
#' # Supposons que l'économètre oublie d'ajouter l'éducation comme variable explicative
#' # Celle-ci sera donc captée par les erreurs qui sont donc corrélées au salaire
#' # L'estimateur des MCO ne sera pas convergent car il y a endogénéité

model <- lm(patrimoine ~ salaire, data = base)
summary(model)