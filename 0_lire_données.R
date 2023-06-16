# charger les dépendences ------------------------------------------------------
library('jsonlite')
library('readxl')

# 
noms_fichier <- list.files(path = '../Données/')

# lire le fichier de données de croissance -------------------------------------
d <- fromJSON('../Données/ringdata-Megan Giguère_Acer Saccharum_SN_2023-05-17_2023-06-05-10-19-00_5HpUnBIWjPDqQUkTPf1FQbqFmQzWK7mA_2023-06-05-104231.json',
              flatten = TRUE)
cc <- d$markerData
annees <- cc$year
croissance <- cc$growth
age <- annees - min(annees) + 1 
arbre <- d$sampleID

# graphique --------------------------------------------------------------------
plot(x = age, y = croissance/1000, typ = 'b', 
     xlab = 'Âge estimé', ylab = 'Croissance (mm)',
     las = 1, col = '#106470', pch = 19, axes = FALSE,
     main = arbre)
axis (side = 1)
axis (side = 2, las = 1)

# correlation 
mod <- lm(croissance/1000 ~ age)

summary(mod)

# ajouter la correlation à la graphique
abline(mod, lty = 2, col = '#91b4a1', lwd = 2)
#legend(x = 0, y = 6, legend = '')
