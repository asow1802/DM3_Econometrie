setwd("C:/Users/souad/Downloads")
getwd()
df = read.csv("C:/Users/souad/Downloads/temps_reaction.csv", header = T, sep = ";", dec = ",")
View(df)
df

## Question 1 
#voir pdf
## Question 2 
model_1 <- lm(Y ~ A.seul + C.seul + A.et.C, data = df)
summary(model_1)
# commentons
# voir pdf
model_2 <- lm(Y ~0 + ni.A.ni.C + A.seul + C.seul + A.et.C, data = df)
summary(model_2)
# Partie2
b = coef(model_1)  # coefficients du modèle avec constante
a = coef(model_2)  # coefficients du modèle sans constante
a
b

# Vérifions le lien
c(
  alpha1 = b["(Intercept)"],
  alpha2 = b["(Intercept)"] + b["A.seul"],
  alpha3 = b["(Intercept)"] + b["C.seul"],
  alpha4 = b["(Intercept)"] + b["A.et.C"]
)
#Ceci correspond parfaitement aux résultats réels des alpha i

## Question 4 
# colonne de 1 : première colonne de X 
un = matrix(1, ncol = 1 , nrow = dim(df)[1])
un
# Matrice X
X = as.matrix(cbind(df$ni.A.ni.C,df$A.seul,df$C.seul,df$A.et.C))
X
# La matrice Y appelée ici V
Y = as.matrix(df$Y)
Y

# calcul à la main 
solve(t(X) %% X) %% t(X) %*% Y


## Question 5  
library(car)
linearHypothesis(model_1,c("A.seul = 0", "C.seul = 0", "A.et.C = 0"))
linearHypothesis(model_2,c("A.seul = 0", "C.seul = 0", "A.et.C = 0"))

# Interprétation dansle fichier pdf



### EXERCICE 2

cheddar = read.csv("./Documents/miashs2026/econometrie/DM3_Econometrie/cheddar.csv", header=TRUE, sep = ",", dec = ".")
head(cheddar, 125)

## Question 1 : MODELE ADEQUATE

# TASTE = b0*1n + b1*ACETIC + b2*H2S + b3*LACTIC + b4*ORG + EPSILON = X%*%BETA + EPSILON
# X = cbind(1n, ACETIC, H2S, LACTIC, ORG) et BETA = rbind(b0, b1, b2, b3, b4)

## Question 2 : Effet
"
Voir pdf
"
## Question 3 : R
# a Coefficients du modèle
MODELE = lm(formula = TASTE ~ ACETIC + H2S + LACTIC + ORG , data = cheddar)

beta_hat = MODELE$coefficients
beta_hat
"
Interprétation : voir pdf
"
# b l'estimation de la variance de Y

S_MODELE =  summary(MODELE)
S_MODELE

sigma_2_hat = S_MODELE$sigma*S_MODELE$sigma
sigma_2_hat

# c f-statistic

S_MODELE$fstatistic[1]


## Question 4 : Estimation par moindre carré

modele_2 = lm(formula = TASTE ~ H2S + LACTIC + ORG , data = cheddar)
s_modele_2 = summary(modele_2)
s_modele_2
"
Interprétation voir pdf
"

## Question 5 : Estimatiion par moindre carré sans la constante

modele_3 = lm(formula = TASTE ~ 0 + ACETIC + H2S + LACTIC + ORG, data = cheddar)
s_modele_3 = summary(modele_3)
s_modele_3
"
interprétation : voir pdf
"

## Question 6 : Test d'hypothèse
# a Test au niveau 2%
alpha_a = 0.02

S_MODELE
t_values = S_MODELE$coefficients[,"t value"]
t_values
abs(t_values)

quantil_t_a = qt(1 - alpha_a/2, dim(cheddar)[1] - dim(cheddar)[2] - 1)
quantil_t_a 

"
interprétation : voir pdf
"
# b Test au niveau 1% : beta_1 + beta_2 = 0
install.packages('car')
library('car')

H0 = c("ACETIC + H2S = 0")

test_fisher = linearHypothesis(MODELE, H0)
test_fisher
test_fisher$F
test_fisher$`Pr(>F)`
"
interprétation : voir pdf
"
# c Test au niveau 1% : (beta_1 + beta_2 + beta_3)^2 = 10*beta_4
alpha_c = 0.01
coeff <- coef(MODELE)

# Estimation de la variance de Beta_hat|X
V <- vcov(MODELE)
# Somme des trois coefficients du modèle :
s <- coeff["ACETIC"] + coeff["H2S"] + coeff["LACTIC"]
# On définit la fonction g par :
g_hat <- (s^2) - 10 * coeff["ORG"]
# Calcul du gradient de g
grad_g <- c(0, 2*s, 2*s, 2*s, -10)
# calcul de la variance approchée de g_hat(beta_hat)
var_g <- as.numeric(t(grad_g) %*% V %*% grad_g)
se_g <- sqrt(var_g)
# calcul de la stat de wald
stat_g <- (g_hat^2) / var_g
stat_g
p_value <- 1 - pchisq(stat_g, df = 1)
p_value
quantil_Khi2 = qchisq(1 - alpha_c, df = 1)
quantil_Khi2


