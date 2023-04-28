# Deuxième partiel
# Hanjoon Ko

library(readr)
vgsales_partiel <- read_csv("vgsales_partiel.csv")
View(vgsales_partiel)


# Analyses et visualisations univariées ---------------------------------------------------------------------

library(skimr)
skim(vgsales_partiel)

# Le jeu de données porte sur les jeux vidéo de plusieurs genres et de plusieurs éditeurs, et sur ses ventes
# Dans ces données, 16 594 jeux vidéo ont été interrogés et été classés en fonction du volume de vente au total
# Certains classements (ex. #654) n’existent pas sur la liste. Il peut s'agir d'une erreur de saisie ou de données intentionnellement exclues. S'il est supposé qu'il s'agisse d'une erreur, les lignes manquantes seront ajoutées, sinon les données seront analysées en fonction des données fournies. Dans cette analyse, on utilise les données fournies sans changement
# Il compte 12 colonnes

vgsales_partiel$Year <- as.character(vgsales_partiel$Year) # On a changé Year en tant que les données qualitatives

# 5 qualitatives : Platform, Year, Genre, Publisher, Top_Ten_Publisher
# 6 quantitatives : Rank, NA_Sales, EU_Sales, JP_Sales, Other_Sales et Global_sales
# 1 textuelle : Name


# Jetons un coup d'oeil dans l'ordre

library(tidyverse)
vgsales_partiel %>%
  count(Platform) %>%
  arrange(-n)

# Il y a 31 plateformes de jeux vidéo
# Plus qu'un quart de jeux vidéo utilisent DS ou PS2 en tant qu'une plateforme. Les deux plateformes sont dinosaures sur le marché du jeu vidéo

table(vgsales_partiel$Platform)
barplot(table(vgsales_partiel$Platform), las = 2)
# On peut facilement voir que DS et PS2 sont les plateformes les plus populaires dans le graphe en barre

table(vgsales_partiel$Year)
barplot(table(vgsales_partiel$Year), las = 2)

############sort(barplot(table(vgsales_partiel$Year), las = 2))
############On aurait pu reordonner les barres avec sort()

# On traite les jeux vidéo publiés de 1980 à 2016
# Les années 2008 et 2009 sont l'âge d'or du jeu vidéo, mais après, la publication des jeux vidéo a commencé à descendre
# Une grande raison d'une chute de la publication des jeux vidéo serait liée à l'universalisation du smartphone

table(vgsales_partiel$Genre)
barplot(table(vgsales_partiel$Genre))

# Il y a 12 genres des jeux vidéo dans le jeu de données
# Un genre de presque 20 % de jeux vidéo est l'action
# Un genre le moins publié est le puzzle

vgsales_partiel %>%
  count(Publisher) %>%
  arrange(-n)

table(vgsales_partiel$Publisher)
barplot(table(vgsales_partiel$Publisher))

# 579 éditeurs, mais dans le graphe, on peut voir que les grands éditeurs publient des jeux vidéos beaucoup plus que les petites éditeurs
# C'est pourquoi le jeu de données offre une colonne de Top_Ten_Publisher, les dix meilleurs éditeurs. On va les analyser dans la ligne suivante.

vgsales_partiel %>%
  filter(Top_Ten_Publisher == "TRUE") %>%
  select(Rank, Name, Publisher) -> vgsales_topten # On fait une nouvelle colonne afin de voir seulement les jeux vidéo qui sont publiés par les dix meilleurs éditeurs

table(vgsales_topten$Publisher)
barplot(table(vgsales_topten$Publisher), las = 2)

mean(table(vgsales_topten$Publisher))

# Les dix meilleurs éditeurs ont publié 816.1 jeux vidéo en moyenne
# Electronic Arts a publié le plus (1 351) et Take-Two Interactive a publié le moins (413)
# La vente des dix meilleurs éditeurs est-elle exactement proportionnelle à la quantité de publications ? 

# Quel éditeur est le meilleur dans le marché de l'Amérique du Nord ?
vgsales_partiel %>%
  filter(Top_Ten_Publisher == "TRUE") %>% 
  select(Publisher, NA_Sales, Global_Sales) %>%
  mutate(Proportion = NA_Sales / Global_Sales) %>%
  group_by(Publisher) %>%
  summarise(Proportion_by_publisher = sum(Proportion) / n()) %>%
  arrange(-Proportion_by_publisher)

# Activision est le meilleur en Amérique du Nord suivis par THQ et par Take-Two Interactive

table(vgsales_partiel$Top_Ten_Publisher)
barplot(table(vgsales_partiel$Top_Ten_Publisher))

# 8 161 publiés par les dix meilleurs éditeurs, 8 433 publiés par les autres éditeurs
# Presque la moitié de jeux vidéo sont publiés par les dix meilleurs éditeurs
# On a déjà vu les noms des dix meilleurs éditeurs ci-dessus : table(vgsales_topten$Publisher)


vgsales_partiel$Rank
# On a 16 600 rangs, mais il y a 16 594 lignes
# Il y a 6 rangs manquants (ex. #654)

skim(vgsales_partiel$NA_Sales)
hist(vgsales_partiel$NA_Sales, breaks = 50)

# La vente médiane de l'Amérique du Nord est 0,08 million (80 000), mais la moyenne est 0,265 (265 000) qui est plus que trois fois la médiane. Donc, son histogramme sera très étalé vers la droite
# L'étendue va des zéros à 41,5 millions

vgsales_partiel %>%
  select(Rank, Name, NA_Sales) %>%
  filter(NA_Sales < 2)

# 16 275 sur 16 594 de jeux vidéo gagnent moins de 2 millions en Amérique du Nord, même si le jeu vidéo le plus vendu (Wii Sports) gagne 41,5 millions
# On peut dire que certaines petites minorités de jeux vidéo dominent le marché

skim(vgsales_partiel$EU_Sales)
hist(vgsales_partiel$EU_Sales, breaks = 50)

# La vente médiane de l'UE est 0,02 million (20 000). La moyenne est 0,147 million (147 000)
# L'histogramme est très étalé vers la droite (la moyenne > la médiane)
# Certaines petites minorités dominent le marché (1er : Wii Sports)
####-> Certains jeux dominent largement le marché

skim(vgsales_partiel$JP_Sales)
hist(vgsales_partiel$JP_Sales, breaks = 50)

# La vente médiane du Japon est 0. La moyenne est 0,0778 million (77 800)
# L'histogramme est très étalé vers la droite
# Certaines petites minorités dominent le marché (1er : Pokemon Red/Pokemon Blue)

skim(vgsales_partiel$Other_Sales)
hist(vgsales_partiel$Other_Sales, breaks = 50)

# La vente médiane des autres régions est 0,01 million. La moyenne est 0,0481 million
# L'histogramme est très étalé vers la droite
# Certaines petites minorités dominent le marché (1er : Grand Theft Auto: San Andreas)

# En moyenne, les jeux vidéo sont vendus les plus en Amérique du Nord, suivis par l'UE, le Japon et les autres régions

skim(vgsales_partiel$Global_Sales)
hist(vgsales_partiel$Global_Sales, breaks = 50)

# Au total, la vente médiane est 0,17 million (170 000) et la moyenne est 0,538 million (538 000)
# L'histogramme est très étalé vers la droite
# Globalement, quelques jeux vidéo sont vendus plus beaucoup que la plupart des jeux vidéo


vgsales_partiel$Name
# Il n'y a pas de règles pour analyser avec les noms de jeux vidéo
# On peut trouver quelques mots pareils dans différents jeux vidéo (ex. Wii, Mario, FIFA). Ça semble des différentes versions d'un même jeu vidéo




# Analyses et visualisations bivariées ------------------------------------

# facteurs de vente mondiale
#
# Peut-on prévoir beaucoup plus de profits mondialement en investissant davantage en Amérique du Nord ?
# Y a-t-il une corrélation entre le genre et la vente mondiale ?
# 
# autres questions
#
# Pour devenir les dix meilleurs éditeurs, doit-on se concentrer sur certains genres du jeu vidéo ?

### Peut-on prévoir beaucoup plus de profits mondialement en investissant davantage en Amérique du Nord ? 
# NA_Sales : quanti
# Global_Sales : quanti
# H0: Il n'y a pas de corrélation entre la vente moyenne de l'Amérique du Nord et celle du monde au total
# H1: Il y a une corrélation entre la vente moyenne de l'Amérique du Nord et celle du monde au total

library(gplots)
plot(vgsales_partiel$Global_Sales ~ vgsales_partiel$NA_Sales)
NA_Global <- lm(vgsales_partiel$Global_Sales ~ vgsales_partiel$NA_Sales)
abline(NA_Global, col = "red")

# Le nuage de points et abline montrent une dépendance linéaire positive, mais quelques données sont éclatées à la fin
# Il y un point qui est très loin des autres points, on va voir si ce point falsifie le résultat (Wii Sports)

cor(vgsales_partiel$Global_Sales, vgsales_partiel$NA_Sales)

# Le coefficient de corrélation est 0.941047. C'est une valeur très positive parce qu'elle est près d'un qui veut dire qu'il y a une forte corrélation entre les deux colonnes
# Mais, serait-il aussi très positive(-> fort) sans une donnée qui est beaucoup plus loin que les autres ? (Wii Sports)
# On va voir un coefficient de corrélation sans Wii Sports

vg_1 <- vgsales_partiel[-c(1),] # On a supprimé Wii Sports
plot(vg_1$Global_Sales ~ vg_1$NA_Sales)
cor(vg_1$Global_Sales, vg_1$NA_Sales)

# Bien que les points éclatent un peu à la fin, on peut voir une dépendance linéaire positive
# L'efficient de corrélation est aussi très positif (0.9299777)
# Donc, une donnée atypique (Wii Sports) ne falsifie pas le résultat qui montre une corrélation positive entre la vente de l'Amérique du Nord et celle du monde au total (On rejette H0)

# Le vente des autres régions a aussi une corrélation avec la vente mondiale ?
cor(vgsales_partiel$Global_Sales, vgsales_partiel$EU_Sales) # R = 0.9028345
cor(vgsales_partiel$Global_Sales, vgsales_partiel$JP_Sales) # R = 0.6118112
cor(vgsales_partiel$Global_Sales, vgsales_partiel$Other_Sales) # R = 0.748327

# Globalement, la vente de toutes les régions ont une corrélation positive avec la vente mondiale
# Pour un grand profit, les entreprise de jeu vidéo peut faire une stratégie qui est efficace au marché en Amérique du Nord et en EU, parce que les deux marchés sont les plus corrélés avec la vente mondiale

# Si une entreprise doit choisir un seul marché, où doit-on choisir ?
vgsales_partiel %>%
  summarise(proportion_NA = mean(NA_Sales / Global_Sales),
            proportion_EU = mean(EU_Sales / Global_Sales),
            proportion_JP = mean(JP_Sales / Global_Sales),
            proportion_AUTRE = mean(Other_Sales / Global_Sales))

# La proportion de chaque région est 0.456, 0.229, 0.242 et 0.0646. Si une entreprise du jeu de vidéo doit choisir un seul marché, elle peut choisir celui de l'Amérique du Nord, parce que le plus de revenus sont générés en Amérique du Nord


### Y a-t-il une corrélation entre le genre et la vente mondiale ? 
# Genre : quali
# Global_Sales : quanti
# H0: La vente est le même pour les genres différents
# H1: La vente n'est pas le même pour les genres différents

vgsales_partiel %>%
  sample_n(3000) -> vgsales_sample # Afin de faire un test de l'ANOVA, on extrait moins de 3 000 échantillons (< 5 000)

plotmeans(vgsales_sample$Global_Sales ~ vgsales_sample$Genre, las = 2)
# Un genre qui génère le plus de revenus en moyenne est Racing
# On voit des différences de vente selon le genre de jeux vidéo, mais quelques intervalles de confiance se chevauchent
# On fait un test de l'ANOVA pour voir s'il y a une différence significative globalement de vente selon le genre

anova_vente_genre <- aov(vgsales_sample$Global_Sales ~ vgsales_sample$Genre)

shapiro.test(anova_vente_genre$residuals) # p-value est très petite, donc on va faire kruskal test
kruskal.test(vgsales_sample$Global_Sales ~ vgsales_sample$Genre) # p-value est très petite

# On rejette H0. Le test confirme qu'il y a une différence très significative de vente mondiale entre les genres


### Pour devenir les dix meilleurs éditeurs, doit-on concentrer sur certains genres du jeu vidéo ?
# Genre: quali
# Publisher: quali
# H0: Il n'y a pas de relation entre les genres et le fait d'être les dix meilleurs éditeurs
# H1: Il y a une relation entre les genres et le fait d'être les dix meilleurs éditeurs

tab_genre_top.publisher <- table(vgsales_partiel$Top_Ten_Publisher, vgsales_partiel$Genre)
# Genre se positionne en deuxième parce qu'on veut voir l'impacte du genre sur les éditeurs

mosaicplot(tab_genre_top.publisher, shade = T, las = 1)
# Presque la moitié de jeux vidéos sont publiés par les dix meilleurs éditeurs et les autres sont publiés par les autres éditeurs (largeur des barres)
# Les dix meilleurs éditeurs publient beaucoup de jeux vidéo de l'Action et le Sports
# Parmi les jeux vidéo publiés par les dix meilleurs, les jeux vidéo du Sport et du Misc sont fortement sur-représentés (résidus > 4)
# Par contre, ceux de l'Aventure, de la Platform et du Racing sont fortement sous-représentés (résidus < 4)
# Vu que les résidus sont importants, il ne faut pas faire un test, mais on va le faire quand même

chisq.test(tab_genre_top.publisher)
# P-value est très petite

# On rejette H0. Il y a une forte relation entre les genres et le fait d'être les dix meilleurs éditeurs


# Conclusion --------------------------------------------------------------
# Le jeu de données montre les chiffres de ventes d'un continent, de l'UE et d'un pays. Le petit chiffre pour Other_Sales suggère qu'il n'y a pas beaucoup de ventes dans d'autres pays Asiatiques et Afriques. Le plus grand marché qui rapporte le plus de revenus aux éditeurs de jeux vidéo est l'Amérique du Nord et les jeux d'action sont publiés le plus. En moyenne, les plus grandes ventes proviennent des jeux de Racing, et les dix meilleurs éditeurs publient beaucoup de jeux d'action et de sport. La concurrence sur le marché du jeu vidéo est très grave et un petit nombre de jeux monopolisent une grande partie du marché. Les entreprises qui souhaitent démarrer un business de jeux vidéo devront tenir compte de la région et du genre avant d'entrer sur le marché. En outre, les dix meilleurs éditeurs représentent la moitié du marché. On peut concevoir s'il y a collusion entre eux et s'il y a un risque de diminution de la diversité des jeux vidéo

