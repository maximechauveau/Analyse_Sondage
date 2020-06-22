####################################
### 1 - Importation des packages ###
####################################
install.packages('pacman')
pacman::p_load('pacman')

pacman::p_load('tidyr')
pacman::p_load('dplyr')

#ggplot pour les représentations graphiques
pacman::p_load('ggplot2')
pacman::p_load('plotly')

# MASS pour la fonction stepAIC
pacman::p_load('MASS')

#
pacman::p_load('questionr')

# pour fonction sample.split
pacman::p_load('caTools')

pacman::p_load('data.table')

pacman::p_load('lubridate')

pacman::p_load('eeptools')


############################################
### 2 - Import des données & Préparation ###
############################################


df_sondage <- read.csv("./data/Questionnaire sur les voitures autonomes.csv")

#Suppression 1er colonne
df_sondage <- df_sondage[,-1]

#Renommage colonne
df_sondage <- df_sondage %>% rename(Categorie_Socio = Dans.quelle.catÃ.gorie.socio.professionnelle.vous.situez.vous..,
                                    Sexe = Quel.est.votre.sexe..,
                                    Age = Quel.est.votre.date.de.naissance..,
                                    Enfant = Avez.vous.des.enfants..,
                                    Permis_Conduire = Avez.vous.le.permis.de.conduire..,
                                    Zone_Habitation = OÃ¹.habitez.vous..,
                                    Etude = Quel.est.votre.niveau.dâ..Ã.tude..,
                                    Frequence_VTC = Ã..quelle.frÃ.quence.utilisez.vous.les.VTC....Uber..Taxi....,
                                    Image_Voiture = Quelle.image.avez.vous.de.votre.voiture..,
                                    Choix_VA = Instinctivement..Ãªtes.vous.plutÃ.t.pour.ou.contre.les.voitures.autonomes..,
                                    IA = Savez.vous.ce.que.câ..est.que.lâ..intelligence.artificielle..I.A....,
                                    Confiance_Tech = ÃŠtes.vous.prÃªts.Ã..faire.confiance.Ã..la.technologie.pour.vous.assister.dans.votre.conduite.ou.vous.transporter..,
                                    Question_Test = Veuillez.rÃ.pondre.Â..Un.peu.Â..Ã..cette.question.,
                                    Distinction_VA = Selon.vous..les.voitures.autonomes.devraient.elles.avoir.une.distinction.particuliÃ.re.afin.de.les.reconnaÃ.tre.sur.le.rÃ.seau.automobile..,
                                    Surete_VA = En.admettant.que.sur.le.rÃ.seau.routier..il.y.ait.6.accidents.mortels.par.milliard.de.Km.parcourus..Pour.que.vous.acceptiez.que.les.voitures.autonomes.soient.lÃ.galisÃ.es..il.faudrait.que.ce.chiffre.soit..,
                                    Monter_VA = Admettons.qu.une.sociÃ.tÃ..X.sorte.son.nouveau.vÃ.hicule.autonome..rÃ.putÃ..plus.fiable.que.l.Ãªtre.humain..Vous.ferez.parti.des.premiers.utilisateurs.de.cette.voiture..monteriez.vous.dedans..,
                                    Activite_VA = Une.fois.dans.une.voiture.autonome..que.ferez.vous..,
                                    Partage_VA = Seriez.vous.prÃªt.Ã..partager.ce.type.de.voiture.avec.des.inconnus.lors.de.vos.trajets....Comme.dans.les.transports.en.commun.,
                                    Google_VA = Google.a.pour.objectif.de.ne.pas.mettre.de.volant..ni.de.pÃ.dale.dans.ces.voitures..Juste.un.bouton.dâ..arrÃªt.dâ..urgence..ÃŠtes.vous.d.accord.avec.cela..,
                                    Securite = Classer.de.1.Ã..7..1...secondaire..7...prioritaire.....vos.attentes.pour.le.vÃ.hicule.de.demain....Tous.types.de.voitures...La.sÃ.curitÃ..,
                                    Environnement = Classer.de.1.Ã..7..1...secondaire..7...prioritaire.....vos.attentes.pour.le.vÃ.hicule.de.demain....Tous.types.de.voitures...L.impact.environnemental.,
                                    Plaisir_Conduite = Classer.de.1.Ã..7..1...secondaire..7...prioritaire.....vos.attentes.pour.le.vÃ.hicule.de.demain....Tous.types.de.voitures...Le.plaisir.de.conduire.,
                                    Cout = Classer.de.1.Ã..7..1...secondaire..7...prioritaire.....vos.attentes.pour.le.vÃ.hicule.de.demain....Tous.types.de.voitures...Le.coÃ.t.,
                                    Necessite = Classer.de.1.Ã..7..1...secondaire..7...prioritaire.....vos.attentes.pour.le.vÃ.hicule.de.demain....Tous.types.de.voitures...Une.nÃ.cessitÃ..,
                                    liberte = Classer.de.1.Ã..7..1...secondaire..7...prioritaire.....vos.attentes.pour.le.vÃ.hicule.de.demain....Tous.types.de.voitures...La.libertÃ..,
                                    Statut_Social = Classer.de.1.Ã..7..1...secondaire..7...prioritaire.....vos.attentes.pour.le.vÃ.hicule.de.demain....Tous.types.de.voitures...Le.statut.social.,
                                    Futur_Automobile = ConsidÃ.rez.vous.que.le.futur.de.l.automobile.passe.par.les.voitures.autonomes..,
                                    Achat_VA = Si.un.jour..le.prix.des.voitures.autonomes.correspond.Ã..votre.budget.actuel.pour.vos.vÃ.hicules.et.que.leurs.sÃ.retÃ.s.a.Ã.tÃ..testÃ.es.et.approuvÃ.es...achÃ.teriez.vous.une.voiture.autonome..,
                                    Commentaire = Merci.pour.le.temps.que.vous.avez.consacrÃ..Ã..cette.enquÃªte..Si.vous.avez.des.commentaires..des.remarques....Vous.pouvez.les.laisser.ci.dessous..Si.vous.avez.des.questions.auxquelles.vous.voulez.avoir.une.rÃ.ponse..envoyez.moi.plutÃ.t.un.mail.)

#Suppression de 2 colonnes inutiles
df_sondage <- df_sondage[,-31]
df_sondage <- df_sondage[,-30]

#Filtres sur la question test
df_sondage <- df_sondage[(df_sondage$Question_Test == "4" | df_sondage$Question_Test == "Un peu"),]

#Suppression question
df_sondage <- df_sondage[,-13]

# Modification Categorie_Socio
df_sondage$Categorie_Socio <- as.character(df_sondage$Categorie_Socio)

df_sondage$Categorie_Socio[df_sondage$Categorie_Socio == 'Agriculteur'] <- '1'
df_sondage$Categorie_Socio[df_sondage$Categorie_Socio == 'Artisan'] <- '2'
df_sondage$Categorie_Socio[df_sondage$Categorie_Socio == 'Chef dâ€™entreprise'] <- '3'
df_sondage$Categorie_Socio[df_sondage$Categorie_Socio == 'Profession libÃ©rale'] <- '4'
df_sondage$Categorie_Socio[df_sondage$Categorie_Socio == 'Cadre ou profession intellectuelle supÃ©rieure'] <- '5'
df_sondage$Categorie_Socio[df_sondage$Categorie_Socio == 'Profession intermÃ©diaire'] <- '6'
df_sondage$Categorie_Socio[df_sondage$Categorie_Socio == 'EmployÃ©'] <- '7'
df_sondage$Categorie_Socio[df_sondage$Categorie_Socio == 'Ouvrier'] <- '8'
df_sondage$Categorie_Socio[df_sondage$Categorie_Socio == 'RetraitÃ©'] <- '9'
df_sondage$Categorie_Socio[df_sondage$Categorie_Socio == 'Demandeur dâ€™emploi'] <- '10'
df_sondage$Categorie_Socio[df_sondage$Categorie_Socio == 'Etudiant, lycÃ©en'] <- '11'
df_sondage$Categorie_Socio[df_sondage$Categorie_Socio == 'Autres'] <- '12'


df_sondage$Categorie_Socio <- as.factor(df_sondage$Categorie_Socio)

#La fonction relevel me permet de choisir quel element sera le referenciel ici la
#df_sondage$Categorie_Socio <- relevel(df_sondage$Categorie_Socio, 'Femme')

#levels(df_sondage$Categorie_Socio)
#freq(df_sondage$Categorie_Socio)


#Calcule de lage
df_sondage$Age <- ymd(df_sondage$Age)
df_sondage$Age <- age_calc(df_sondage$Age, enddate = Sys.Date(), units = "years", precise = TRUE)
df_sondage$Age <- round(df_sondage$Age, 0)


#Suppression des commentaires
df_sondage <- df_sondage[,-28]

#Suppression des valeurs nulles
df_sondage <- df_sondage %>% drop_na()

#Suppression if age = 0
df_sondage <- df_sondage[!(df_sondage$Age == "0"),]


############################################
# Modification 0 = Femme // 1 = Homme
df_sondage$Sexe <- as.character(df_sondage$Sexe)

df_sondage$Sexe[df_sondage$Sexe=='FÃ©minin'] <- '0'
df_sondage$Sexe[df_sondage$Sexe=='Masculin'] <- '1'
df_sondage$Sexe <- as.factor(df_sondage$Sexe)

#La fonction relevel me permet de choisir quel element sera le referenciel ici la
df_sondage$Sexe <- relevel(df_sondage$Sexe, '0')

#levels(df_sondage$Sexe)
freq(df_sondage$Sexe)


############################################
# Modification Enfant
df_sondage$Enfant <- as.character(df_sondage$Enfant)

df_sondage$Enfant[df_sondage$Enfant=='Non'] <- '0'
df_sondage$Enfant[df_sondage$Enfant=='Oui'] <- '1'
df_sondage$Enfant <- as.factor(df_sondage$Enfant)

#La fonction relevel me permet de choisir quel element sera le referenciel ici la
df_sondage$Enfant <- relevel(df_sondage$Enfant, '0')

levels(df_sondage$Enfant)
freq(df_sondage$Enfant)


############################################
# Modification Permis_Conduire
df_sondage$Permis_Conduire <- as.character(df_sondage$Permis_Conduire)

df_sondage$Permis_Conduire[df_sondage$Permis_Conduire=='Non'] <- '0'
df_sondage$Permis_Conduire[df_sondage$Permis_Conduire=='Oui'] <- '1'
df_sondage$Permis_Conduire <- as.factor(df_sondage$Permis_Conduire)

#La fonction relevel me permet de choisir quel element sera le referenciel ici la
df_sondage$Permis_Conduire <- relevel(df_sondage$Permis_Conduire, '0')

levels(df_sondage$Permis_Conduire)
freq(df_sondage$Permis_Conduire)


############################################
# Modification Zone_Habitation
df_sondage$Zone_Habitation <- as.character(df_sondage$Zone_Habitation)

df_sondage$Zone_Habitation[df_sondage$Zone_Habitation=='En milieu urbain'] <- '0'
df_sondage$Zone_Habitation[df_sondage$Zone_Habitation=='En milieu pÃ©riurbain'] <- '1'
df_sondage$Zone_Habitation[df_sondage$Zone_Habitation=='En campagne'] <- '2'
df_sondage$Zone_Habitation <- as.factor(df_sondage$Zone_Habitation)

#La fonction relevel me permet de choisir quel element sera le referenciel ici la
df_sondage$Zone_Habitation <- relevel(df_sondage$Zone_Habitation, '0')

levels(df_sondage$Zone_Habitation)
freq(df_sondage$Zone_Habitation)


############################################
# Modification Etude
df_sondage$Etude <- as.character(df_sondage$Etude)

df_sondage$Etude[df_sondage$Etude=='Brevet des collÃ¨ges'] <- '0'
df_sondage$Etude[df_sondage$Etude=='CAP/BEP (autres diplÃ´mes techniques)'] <- '1'
df_sondage$Etude[df_sondage$Etude=='Bac (gÃ©nÃ©ral, pro et technologique)'] <- '2'
df_sondage$Etude[df_sondage$Etude=='Bac+2 (BTS ou autre)'] <- '3'
df_sondage$Etude[df_sondage$Etude=='Bac+3/4 (Licence, MaÃ®trise)'] <- '4'
df_sondage$Etude[df_sondage$Etude=='Bac+5 (Master, Ã©coles d\'ingÃ©, Ã©coles d\'arts...)'] <- '5'
df_sondage$Etude[df_sondage$Etude=='Bac+7 etc (Doctorat, post-doc, thÃ¨se)'] <- '6'

df_sondage$Etude <- as.factor(df_sondage$Etude)

#La fonction relevel me permet de choisir quel element sera le referenciel ici la
df_sondage$Etude <- relevel(df_sondage$Etude, '0')

levels(df_sondage$Etude)
freq(df_sondage$Etude)


############################################
# Modification Frequence_VTC
df_sondage$Frequence_VTC <- as.character(df_sondage$Frequence_VTC)

df_sondage$Frequence_VTC[df_sondage$Frequence_VTC=='Jamais'] <- '0'
df_sondage$Frequence_VTC[df_sondage$Frequence_VTC=='Quelques fois par an'] <- '1'
df_sondage$Frequence_VTC[df_sondage$Frequence_VTC=='Quelques fois par mois'] <- '2'
df_sondage$Frequence_VTC[df_sondage$Frequence_VTC=='Une fois par semaine'] <- '3'
df_sondage$Frequence_VTC[df_sondage$Frequence_VTC=='Plusieurs fois par semaine'] <- '4'
df_sondage$Frequence_VTC[df_sondage$Frequence_VTC=='Tous les jours'] <- '5'

df_sondage$Frequence_VTC <- as.factor(df_sondage$Frequence_VTC)

#La fonction relevel me permet de choisir quel element sera le referenciel ici la
df_sondage$Frequence_VTC <- relevel(df_sondage$Frequence_VTC, '0')

levels(df_sondage$Frequence_VTC)
freq(df_sondage$Frequence_VTC)


############################################
# Modification Image_Voiture
df_sondage$Image_Voiture <- as.character(df_sondage$Image_Voiture)

df_sondage$Image_Voiture[df_sondage$Image_Voiture == 'Un simple moyen de locomotion'] <- '0'
df_sondage$Image_Voiture[df_sondage$Image_Voiture == 'Un objet avec une valeur sentimentale'] <- '1'
df_sondage$Image_Voiture[df_sondage$Image_Voiture == 'Elle reflÃ¨te qui je suis'] <- '2'
df_sondage$Image_Voiture[df_sondage$Image_Voiture == 'Un bien prÃ©cieux'] <- '3'
df_sondage$Image_Voiture[df_sondage$Image_Voiture == 'Je n\'ai pas de voiture'] <- '4'
df_sondage$Image_Voiture[df_sondage$Image_Voiture == 'LibertÃ©'] <- '5'
df_sondage$Image_Voiture[df_sondage$Image_Voiture == 'Un simple moyen de locomotion dans lequel je peux Ã©couter de la musique'] <- '5'
df_sondage$Image_Voiture[df_sondage$Image_Voiture == 'Un polluant'] <- '5'

df_sondage$Image_Voiture <- as.factor(df_sondage$Image_Voiture)

#La fonction relevel me permet de choisir quel element sera le referenciel ici la
df_sondage$Image_Voiture <- relevel(df_sondage$Image_Voiture, '0')

levels(df_sondage$Image_Voiture)
freq(df_sondage$Image_Voiture)


############################################
# Modification Choix_VA
df_sondage$Choix_VA <- as.character(df_sondage$Choix_VA)

df_sondage$Choix_VA[df_sondage$Choix_VA == 'Contre'] <- '0'
df_sondage$Choix_VA[df_sondage$Choix_VA == 'Pour'] <- '1'

df_sondage$Choix_VA <- as.factor(df_sondage$Choix_VA)

#La fonction relevel me permet de choisir quel element sera le referenciel ici la
df_sondage$Choix_VA <- relevel(df_sondage$Choix_VA, '0')

levels(df_sondage$Choix_VA)
freq(df_sondage$Choix_VA)


############################################
# Modification IA
df_sondage$IA <- as.character(df_sondage$IA)

df_sondage$IA[df_sondage$IA == 'Jamais entendu parler'] <- '0'
df_sondage$IA[df_sondage$IA == 'J\'en ai entendu parler, mais je ne sais pas ce que c\'est'] <- '1'
df_sondage$IA[df_sondage$IA == 'Je connais mais je ne pratique pas'] <- '2'
df_sondage$IA[df_sondage$IA == 'Je connais et je pratique'] <- '3'

df_sondage$IA <- as.factor(df_sondage$IA)

#La fonction relevel me permet de choisir quel element sera le referenciel ici la
df_sondage$IA <- relevel(df_sondage$IA, '0')

levels(df_sondage$IA)
freq(df_sondage$IA)


############################################
# Modification Confiance_Tech
df_sondage$Confiance_Tech <- as.character(df_sondage$Confiance_Tech)

df_sondage$Confiance_Tech[df_sondage$Confiance_Tech == 'Non'] <- '0'
df_sondage$Confiance_Tech[df_sondage$Confiance_Tech == 'Oui mais dans certaines conditions (parking, embouteillages)'] <- '1'
df_sondage$Confiance_Tech[df_sondage$Confiance_Tech == 'Oui dans toutes les conditions de circulation'] <- '2'

df_sondage$Confiance_Tech <- as.factor(df_sondage$Confiance_Tech)

#La fonction relevel me permet de choisir quel element sera le referenciel ici la
df_sondage$Confiance_Tech <- relevel(df_sondage$Confiance_Tech, '0')

levels(df_sondage$Confiance_Tech)
freq(df_sondage$Confiance_Tech)


############################################
# Modification Distinction_VA
df_sondage$Distinction_VA <- as.character(df_sondage$Distinction_VA)

df_sondage$Distinction_VA[df_sondage$Distinction_VA == 'Non'] <- '0'
df_sondage$Distinction_VA[df_sondage$Distinction_VA == 'Oui'] <- '1'

df_sondage$Distinction_VA <- as.factor(df_sondage$Distinction_VA)

#La fonction relevel me permet de choisir quel element sera le referenciel ici la
df_sondage$Distinction_VA <- relevel(df_sondage$Distinction_VA, '0')

levels(df_sondage$Distinction_VA)
freq(df_sondage$Distinction_VA)


############################################
# Modification Surete_VA
df_sondage$Surete_VA <- as.character(df_sondage$Surete_VA)

df_sondage$Surete_VA[df_sondage$Surete_VA == 'LÃ©gÃ¨rement supÃ©rieur'] <- '0'
df_sondage$Surete_VA[df_sondage$Surete_VA == 'Ã‰gal'] <- '1'
df_sondage$Surete_VA[df_sondage$Surete_VA == '2 fois infÃ©rieur'] <- '2'
df_sondage$Surete_VA[df_sondage$Surete_VA == '5 fois infÃ©rieur'] <- '3'
df_sondage$Surete_VA[df_sondage$Surete_VA == '10 fois infÃ©rieur'] <- '4'

df_sondage$Surete_VA <- as.factor(df_sondage$Surete_VA)

#La fonction relevel me permet de choisir quel element sera le referenciel ici la
df_sondage$Surete_VA <- relevel(df_sondage$Surete_VA, '0')

levels(df_sondage$Surete_VA)
freq(df_sondage$Surete_VA)


############################################
# Modification Futur_Automobile
df_sondage$Futur_Automobile <- as.character(df_sondage$Futur_Automobile)

df_sondage$Futur_Automobile[df_sondage$Futur_Automobile == 'Non'] <- '0'
df_sondage$Futur_Automobile[df_sondage$Futur_Automobile == 'Oui'] <- '1'

df_sondage$Futur_Automobile <- as.factor(df_sondage$Futur_Automobile)

#La fonction relevel me permet de choisir quel element sera le referenciel ici la
df_sondage$Futur_Automobile <- relevel(df_sondage$Futur_Automobile, '0')

levels(df_sondage$Futur_Automobile)
freq(df_sondage$Futur_Automobile)


############################################
# Modification Achat_VA
df_sondage$Achat_VA <- as.character(df_sondage$Achat_VA)

df_sondage$Achat_VA[df_sondage$Achat_VA == 'Non'] <- '0'
df_sondage$Achat_VA[df_sondage$Achat_VA == 'Peut-Ãªtre'] <- '1'
df_sondage$Achat_VA[df_sondage$Achat_VA == 'Oui'] <- '2'

df_sondage$Achat_VA <- as.factor(df_sondage$Achat_VA)

#La fonction relevel me permet de choisir quel element sera le referenciel ici la
df_sondage$Achat_VA <- relevel(df_sondage$Achat_VA, '0')

levels(df_sondage$Achat_VA)
freq(df_sondage$Achat_VA)

df_sondage <- df_sondage[,-16]


df_RegLog <- df_sondage

#######################
### 3 - Sauvegarde  ###
#######################

save(df_RegLog, file = 'data/df_RegLog.RData')















