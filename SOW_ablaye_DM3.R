### Exercice 1
temps_reaction = read.csv("./Documents/miashs2026/econometrie/DM3_Econometrie/temps_reaction.csv", header=TRUE, sep = ";", dec = ",")
head(temps_reaction, 125)

### Question 1
## Pour le modèle sans constante
# Soit Y notre vecteur de temps de réaction qu'on souhaite estimer à l'aide de la consommation d'alcool et de café
# On définit notre modèle par : Y = b1*ni.A.ni.C + b2*A.seul + b3.C.seul + b4.A.et.C + Epsilon
# Notons par X = cbind(1_n, ni.A.ni.C + A.seul, C.seul, A.et.C, Const) et Beta = c(b0, b1, b2, b3, b4, b5)
# Ainsi on a : Y = X@Beta + Epsilon
# X@Beta = Partie de Y expliquée par notre modèle
# Epsilon = Partie de Y non expliquée par notre modèle

## Pour le modèle avec constante
# Soit Y notre vecteur de temps de réaction qu'on souhaite estimer à l'aide de la consommation d'alcool et de café
# On définit notre modèle par : Y = b0*1_n + b1*ni.A.ni.C + b2*A.seul + b3.C.seul + b4.A.et.C + Epsilon
# Notons par X = cbind(1_n, A.ni.C + A.seul, C.seul, A.et.C) et Beta = c(b0, b1, b2, b3, b4)
# Ainsi on a : Y = X@Beta + Epsilon
# X@Beta = Partie de Y expliquée par notre modèle
# Epsilon = Partie de Y non expliquée par notre modèle

# Les coefficients bi, i £ {2,3,4,5} décrivent l'effet de la consommation par rapport au temps de réaction
# Si le bi est nul, cela signifie que la variable associée n'apporte aucune information complémentaire sachant
# que l'on connait les autres variables
# b1 décrit la variation du temps de réaction causé par la variabilité de la variable associée à b1

model_M3 = lm(formula = Y ~ A.seul + C.seul + A.et.C, data = temps_reaction)
summary(model_M3)



