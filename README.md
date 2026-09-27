ridge
Description

ridge est un package R permettant d'ajuster un modèle de régression Ridge avec un paramètre de régularisation lambda.

La fonction publique du package est ridge_fit(). Elle ajuste le modèle et retourne un objet de classe ridge_model.

La fonction interne ridge() réalise les calculs nécessaires mais n'est pas exportée directement par le package.

Installation

Le package peut être installé directement depuis GitHub avec :

install.packages("remotes")
remotes::install_github("Mustapha-Labhar/ridge")


Puis charger le package :

library(ridge)

Utilisation

Un exemple simple :

x <- matrix(
  c(1, 2,
    2, 3,
    3, 5,
    4, 7,
    5, 11),
  ncol = 2,
  byrow = TRUE
)

y <- c(3, 5, 8, 11, 17)

model <- ridge_fit(x, y, lambda = 1)

model


Le résultat est :

Ridge Regression Model
----------------------
Lambda: 1

Coefficients:
(Intercept)          x1          x2
  0.0443459   0.5055432   1.2926829

Fonction principale
ridge_fit()
ridge_fit(x, y, lambda = 1)


Arguments :

x : matrice numérique des variables explicatives ;

y : vecteur numérique de la variable réponse ;

lambda : paramètre de régularisation non négatif.

La fonction retourne un objet de classe ridge_model contenant les coefficients estimés et la valeur de lambda.

Tests

Le package utilise testthat pour vérifier le fonctionnement de ridge_fit().

Les tests couvrent notamment :

le calcul des coefficients ;

le cas lambda = 0 ;

la validation des arguments ;

les valeurs invalides de lambda ;

les noms des coefficients ;

la conservation des noms des variables explicatives ;

la classe de l'objet retourné.

Les tests peuvent être exécutés avec :

devtools::test()

Vérification du package

La commande suivante permet de vérifier le package :

devtools::check()


La dernière vérification effectuée a donné :

0 errors
0 warnings
0 notes

Organisation du package
ridge/
├── DESCRIPTION
├── LICENSE
├── LICENSE.md
├── NAMESPACE
├── R/
│   ├── ridge.R
│   ├── ridge_fit.R
│   └── print_ridge_model.R
├── man/
│   ├── ridge_fit.Rd
│   └── print.ridge_model.Rd
├── tests/
│   └── testthat/
│       └── test-ridge_fit.R
└── README.md

Licence

Ce projet est distribué sous licence MIT.
