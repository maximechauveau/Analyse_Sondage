####################################
### 1 - Importation des packages ###
####################################

options(encoding = "UTF-8")

#install.packages('pacman')
pacman::p_load('pacman')

pacman::p_load('corrplot')

# MASS pour la fonction stepAIC
pacman::p_load('MASS')

# Visualisation des données
pacman::p_load('ggplot2')
pacman::p_load('plotly')

# Manipulation des données
pacman::p_load('dplyr')

#Analyse stat
pacman::p_load('questionr')

#Représentation graphique du modèle
pacman::p_load('forestmodel')

#Distance de Cook
pacman::p_load('car')



############################################
### 2 - Import des données & Préparation ###
############################################


load(file='./data/df_RegLog.RData')


#Conversion en num
df_RegLog[] <- lapply(df_RegLog, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})


#Matrice de corélation
mcor <- cor(df_RegLog)
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)

load(file='./data/df_RegLog.RData')

#Conversion en num
df_RegLog[] <- lapply(df_RegLog, function(x) {
  if(as.numeric(x)) as.factor(x) else x
})

df_RegLog$Age <- as.numeric(df_RegLog$Age)


###############################
### Statistique Descriptive ###
###############################

# categerie socio Professionnel
Categorie_Socio <- freq(df_RegLog$Categorie_Socio)

Categorie_Socio$Labels <- c('Agriculteur','Demandeur d\'emploi', 'Étudiant, lycéen', 'Autres', 'Artisan', 'Chef d\'entreprise', 'Profession libérale', 'Cadre ou profession intellectuelle supérieure',
                            'Profession intermédiaire', 'Employé', 'Ouvrier', 'Retraité')


fig <- plot_ly(Categorie_Socio, values = ~n, labels = ~Labels, type = 'pie')
fig <- fig %>% layout(title = 'Répartition des différentes catégories socioprofessionnelles')
fig


# Age Histogramme
x <- list(title = "Âge")

y <- list(title = "Fréquence")


fig <- plot_ly(alpha = 0.6)
fig <- fig %>% add_histogram(x = df_RegLog$Age)
fig <- fig %>% layout(barmode = "overlay") %>%
  layout(title = 'Répartition de l\'échantillon en fontion de l\'âge', xaxis = x, yaxis = y)
fig


# Sexe
freq(df_RegLog$Sexe)
summary(df_RegLog$Age)
freq(df_RegLog$Enfant)

freq(df_RegLog$Permis_Conduire)
freq(df_RegLog$Zone_Habitation)



Choix_VA <- freq(df_RegLog$Choix_VA)



# Distinction VA
Distinction_VA <- freq(df_RegLog$Distinction_VA)
 
fig <- plot_ly(Distinction_VA, values = ~n, type = 'pie')
fig <- fig %>% layout(title = 'Choix sur la distinction des voitures autonomes')
fig


# confiance technologie VA
Confiance_Tech <- freq(df_RegLog$Confiance_Tech)

Confiance_Tech$Labels <- c('Non', 'Oui mais dans certaines conditions (parking, embouteillages)', 'Oui dans toutes les conditions de circulation')

fig <- plot_ly(Confiance_Tech, values = ~n, labels = ~Labels , type = 'pie')
fig <- fig %>% layout(title = 'Répartition de la confiance accordées aux technologies concernant la conduite')
fig


# Surete VA
Surete_VA <- freq(df_RegLog$Surete_VA)

Surete_VA$Labels <- c('Légèrement plus haut', 'Égal', '2 fois plus bas', '5 fois plus bas', '10 fois plus bas')

fig <- plot_ly(Surete_VA, values = ~n, labels = ~Labels , type = 'pie')
fig <- fig %>% layout(title = 'Avis sur le niveau de sureté des voitures autonomes par rapport aux humains')
fig

# Google
fig <- plot_ly(
  y = Google_VA$n,
  x = Google_VA$Labels,
  name = "SF Zoo",
  type = "bar",
  text = Google_VA$`val%`,
  textposition = 'auto',
)
fig <- fig %>%layout(title = 'Positionnement par rapport au choix de Google')

fig


#  









################
#Regression
############

df_test <- df_RegLog


modelSature <- glm(formula = Choix_VA ~ Categorie_Socio + Sexe+ Age + Enfant + Permis_Conduire +
                   Zone_Habitation + Etude + Frequence_VTC + Image_Voiture,
                   family = binomial,
                   data = df_RegLog, maxit = 100)
summary(modelSature)


testmodel <- glm(formula = Choix_VA ~ Categorie_Socio + Sexe + Age + Enfant + 
      Permis_Conduire + Zone_Habitation + Etude + Frequence_VTC + 
      Image_Voiture, family = binomial, 
    data = df_RegLog, maxit = 100)


stepAIC(modelSature)

modelretenu <- glm(formula = Choix_VA ~ Sexe + Permis_Conduire + Frequence_VTC, 
                   family = binomial, data = df_RegLog, maxit = 100)

summary(modelretenu)

Age + Enfant + Permis_Conduire + Zone_Habitation + 
  Frequence_VTC + Image_Voiture + Surete_VA + Securite


Confiance_Tech

Futur_Automobile
Achat_VA


suppressWarnings(forest_model(modelretenu))


infIndexPlot(modelretenu, vars = 'Cook', id = TRUE, grid = TRUE, main = 'Les points influents')


##############################
### Catégorie SocioPro
fig <- plot_ly(alpha = 0.6)
fig <- fig %>% add_histogram(x = df_RegLog$Categorie_Socio[df_RegLog$Choix_VA == 0], name = 'Contre')
fig <- fig %>% add_histogram(x = df_RegLog$Categorie_Socio[df_RegLog$Choix_VA == 1], name = 'Pour')
fig <- fig %>% layout(barmode = "overlay")

fig


##############################
###  Sexe
fig <- plot_ly(alpha = 0.6)
fig <- fig %>% add_histogram(x = df_RegLog$Sexe[df_RegLog$Choix_VA == 0], name = 'Contre')
fig <- fig %>% add_histogram(x = df_RegLog$Sexe[df_RegLog$Choix_VA == 1], name = 'Pour')
fig <- fig %>% layout(barmode = "overlay", title = 'Avis sur les voitures autonomes en fonction du sexe')

fig


##############################
###  Permis de conduire
fig <- plot_ly(alpha = 0.6)
fig <- fig %>% add_histogram(x = df_RegLog$Permis_Conduire[df_RegLog$Choix_VA == 0], name = 'Contre')
fig <- fig %>% add_histogram(x = df_RegLog$Permis_Conduire[df_RegLog$Choix_VA == 1], name = 'Pour')
fig <- fig %>% layout(barmode = "overlay", title = 'Avis sur les voitures autonomes en fonction du permis de conduire')

fig

##############################
###  Zone_Habitation
fig <- plot_ly(alpha = 0.6)
fig <- fig %>% add_histogram(x = df_RegLog$Zone_Habitation[df_RegLog$Choix_VA == 0], name = 'Contre')
fig <- fig %>% add_histogram(x = df_RegLog$Zone_Habitation[df_RegLog$Choix_VA == 1], name = 'Pour')
fig <- fig %>% layout(barmode = "overlay")

fig


##############################
###  Frequence_VTC
fig <- plot_ly(alpha = 0.6)
fig <- fig %>% add_histogram(x = df_RegLog$Frequence_VTC[df_RegLog$Choix_VA == 0], name = 'Contre')
fig <- fig %>% add_histogram(x = df_RegLog$Frequence_VTC[df_RegLog$Choix_VA == 1], name = 'Pour')
fig <- fig %>% layout(barmode = "overlay")

fig


##############################
###  Age
fig <- plot_ly(alpha = 0.6)
fig <- fig %>% add_histogram(x = df_RegLog$Age[df_RegLog$Choix_VA == 0], name = 'Contre')
fig <- fig %>% add_histogram(x = df_RegLog$Age[df_RegLog$Choix_VA == 1], name = 'Pour')
fig <- fig %>% layout(barmode = "overlay")

fig


##############################
###  Frequence_VTC
fig <- plot_ly(alpha = 0.6)
fig <- fig %>% add_histogram(x = df_RegLog$Frequence_VTC[df_RegLog$Choix_VA == 0], name = 'Contre')
fig <- fig %>% add_histogram(x = df_RegLog$Frequence_VTC[df_RegLog$Choix_VA == 1], name = 'Pour')
fig <- fig %>% layout(barmode = "overlay", title = 'Avis sur les voitures autonomes en fonction de la fréquence d\'utilisation des VTC')

fig


# confiance technologie VA
Confiance_Tech <- freq(df_RegLog$Confiance_Tech)

Confiance_Tech$Labels <- c('Non', 'Oui mais dans certaines conditions (parking, embouteillages)', 'Oui dans toutes les conditions de circulation')

fig <- plot_ly(Confiance_Tech, values = ~n, labels = ~Labels , type = 'pie')
fig <- fig %>% layout(title = 'Répartition de la confiance accordées aux technologies concernant la conduite')
fig

fig <- plot_ly(alpha = 0.6)
fig <- fig %>% add_histogram(x = df_RegLog$Etude[df_RegLog$Confiance_Tech == 0], name = 'Non')
fig <- fig %>% add_histogram(x = df_RegLog$Etude[df_RegLog$Confiance_Tech == 1], name = 'Oui mais')
fig <- fig %>% add_histogram(x = df_RegLog$Etude[df_RegLog$Confiance_Tech == 2], name = 'Oui')
#fig <- fig %>% layout(barmode = "overlay", title = 'Avis sur les voitures autonomes en fonction de la fréquence d\'utilisation des VTC')

fig












