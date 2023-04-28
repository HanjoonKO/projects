#Partiel
#Hanjoon Ko

library(readr)
titanic_partiel <- read_csv("~/titanic.csv")
View(titanic_partiel)


#1

summary(titanic_partiel)

# Il y avait 887 personnes dans Titanice.
# Le moyen de leur ages est 29.46

# Je veux focaliser sur les données de personnes décédées

titanic_decedees <- subset(titanic_partiel, Survived=="No")
library(skimr)
skim(titanic_decedees$Age)

# 545 personnes sont décédées et le moyen de leur age est 30.1

hist(titanic_decedees$Age, breaks = 8, main = "L'age de personnes décédées", xlab="age", ylab="nombres", col="skyblue")

# Selon l'histogramme, on peut voir que le nombre de personnes qui ont entre 20 ans ~ 30 ans est le plus et celui de personnes qui ont plus de 70 ans est le plus moins


# 2
# Est-ce que l'age influence à la morte ?

library(gplots)
plotmeans(titanic_partiel$Age ~ titanic_partiel$Survived)

# Dans la diagramme, on peut voir le moyen d'age de personnes décédés sont presaue plus d'un ans que celui de personnes survecues


# H0: Il y a une relation entre l'age et la morte
# Ha: Il n'y a pas de relation entre l'age et la morte


titanic_survecu <- subset(titanic_partiel, Survived=="Yes")

# On a déjà un subset de personnes qui sont mortes
# On dois voir s'il y a une différence significative dans chaque subset

shapiro.test(titanic_survecu$Age)
# p-value est petite(<0.05), donc la différence est significative
shapiro.test(titanic_decedees$Age)
# p-value est très petite(<0.001), donc la différence est très significative

# On n'a pas une distribution normale dans chaque groupe, donc on va faire T-test de Wilcoxon

wilcox.test(titanic_partiel$Age ~ titanic_partiel$Survived)

# p-value est grande (0.3761)
# On accepte H0. Il n'y a pas de relation entre l'age et la survecu



# 2
# Je veux voir s'il y a une realtion entre le nombre de parents et des enfants et la classe de places de Titanic

# H0: Il n'a pas de relation entre le nombre de parents/enfants et la classe de places
# Ha: Il y a d'une relation entre le nombre de parents/enfants et la classe de places

# Il y a plus de deux choses de comparer, donc on fait ANOVA, pas T-test

anova_nombre_class <- aov(titanic_partiel$`Parents/Children Aboard` ~ titanic_partiel$Passenger_class)

shapiro.test(anova_nombre_class$residuals)

# p-value est tres petite, donc on va faire kruskal test

kruskal.test(titanic_partiel$`Parents/Children Aboard` ~ titanic_partiel$Passenger_class)

# p-values est grande (0.6272)

# On accepte H0. Il n'y a pas de relation entre le nombre de parents/enfants et la classe de places



# 3
# Je veux voir une relation entre la classe et la morte
# C'est une comparaison de deux donnees qualitatives, donc je fais chi2

library(ade4)

tab_titanic2 <- table(titanic_partiel$Survived, titanic_partiel$Passenger_class)       
mosaicplot(tab_titanic2, shade=T, las=2)

# class first -> Yes (survived) : il y a fort representation
# class first -> No (decede) : il y a fort sous representation)
# class third -> Yes : il y a fort sous representation)
# class third -> No : il y a representation (pas fort

chisq.test(tab_titanic2)

#p-value est tres petite.
# C'est significative -> il y a une difference entre les deux