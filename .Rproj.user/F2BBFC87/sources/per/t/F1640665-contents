####################################
### 1 - Importation des packages ###
####################################

install.packages('pacman')
pacman::p_load('pacman')

pacman::p_load('corrplot')


############################################
### 2 - Import des données & Préparation ###
############################################


load(file='./data/df_RegLog.RData')

df_RegLog <- df_RegLog[,-16]

#Conversion en num
df_RegLog[] <- lapply(df_RegLog, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})


#Matrice de corélation
mcor <- cor(df_RegLog)
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)


















