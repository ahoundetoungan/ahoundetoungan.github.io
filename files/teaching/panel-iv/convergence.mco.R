#' Nous allons montrer que la méthode des MCO fonctionne bien lorsque les hypothèses sont vérifiées
#' Pour ce faire, nous allons nous mettre dans un contexte théorique.
#' Nous n'allons pas utiliser des données réelles. Mais nous allons les simuler.
#' En simulant les données, nous avons la posibilité de définir nous-même la 
#' valeur des coefficients beta et sigma. 
#' Ensuite, nous allons utiliser les données générées et estimer le modèle avec la méthode des MCO.
#' L'objectif de comparer les valeurs estimées aux paramètres que nous avons définis

#' Génération des données
N  <- 500 # taille de l'échantillon
#' # Deux variables explicatives
set.seed(2021)
X1 <- rnorm(N, 0, 1)
X2 <- rpois(N, 2)

#' # coefficients
b0    <- 1
b1    <- 0.7
b2    <- -1.2
sigma <- 0.5

#' # y est ensuite généré comme y = b0 + b1 x X1 + b2 x X2 + rnorm(N, 0, sigma)
y     <- b0 + b1*X1 + b2*X2 + rnorm(N, 0, sigma)

#' # Des graphiques
plot(X1, y)
plot(X2, y)

#' # Nous allons constituer la base de données.
#' # La fonction data.frame permet d'assembler toutes les variables nécessaires dans une même base.

base  <- data.frame(y = y, X1 = X1, X2 = X2)

#' # Nous allons ensuite supprimer tous les objects et ne garder que base car, c'est elle qui nous
#' # intéresse
rm(list = ls()[ls() != "base"])

#' # Nous allons ensuite estimer le modèle par MCO en utilisant la commande lm

model <- lm(y ~ X1 + X2, data = base)
summary(model)

#' # En augmentant la taille de l'échantillon, on peut remarque que les valeurs estimées
#' # s'approchent de plus en plus des vraies valeurs. 
#' # Ceci est une illustration de la convergence en probabilité de l'estimateur des MCO

#' # A faire par vous-même : Construisez un code pour illustrer que l'estimateur des MCO 
#' # sans biais. 
#' Indication : Vous devez utiliser une boucle dans laquelle vous allez gérérer et estimer 
#' le modèle par MCO. Vous devez aussi stocker la valeur estimée à chaque boucle.
#' La moyenne arithmétique de chaque estimateur le long de la bouble devrait être très proche
#' des vraies valeurs.