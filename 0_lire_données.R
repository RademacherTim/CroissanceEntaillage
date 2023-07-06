# charger les dépendences ------------------------------------------------------
library('jsonlite')
library('readxl')

# 
noms_fichier <- list.files(path = '../Données/')

# lire le fichier de données de croissance -------------------------------------
d_SN <- fromJSON('../Données/ringdata-Megan Giguère_Acer Saccharum_SN_2023-05-17_2023-06-05-10-19-00_5HpUnBIWjPDqQUkTPf1FQbqFmQzWK7mA_2023-06-05-104231.json',
               flatten = TRUE)
cc_SN <- d_SN$markerData
annees_SN <- cc_SN$year [cc_SN$type == 'Normal']
croissance_SN <- cc_SN$growth [cc_SN$type == 'Normal']
age_SN <- annees_SN - min(annees_SN) + 1 
arbre_SN <- d_SN$sampleID

d_AS_25A <- fromJSON('../Données/ringdata-Acer Saccharinum_AS_2023-06-15_25A_Megan Giguère_2023-07-06-09-02-59_QUoHOiD3U3Ly5wUfdO3p0t5juJTNx1tT_2023-07-06-092052.json',
               flatten = TRUE)
cc_25A <- d_AS_25A$markerData
annees_25A <- cc_25A$year [cc_25A$type == 'Normal']
croissance_25A <- cc_25A$growth [cc_25A$type == 'Normal']
age_25A <- annees_25A - min(annees_25A) + 1 
arbre_25A <- d_AS_25A$sampleID

d_AS_25B <- fromJSON('../Données/ringdata-Acer Saccharinum_AS_2023-06-15_25B_Megan Giguère_2023-07-06-09-21-41_u4mUQmOEsdAaPJUVWL1bISnWqmW9ZOA6_2023-07-06-093257.json',
               flatten = TRUE)
cc_25B <- d_AS_25B$markerData
annees_25B <- cc_25B$year [cc_25B$type == 'Normal']
croissance_25B <- cc_25B$growth [cc_25B$type == 'Normal']
age_25B <- annees_25B - min(annees_25B) + 1 
arbre_25B <- d_AS_25B$sampleID

d_AS_26A <- fromJSON('../Données/ringdata-Acer Saccharum_AS_2023-06-15_26A_Megan Giguère_2023-07-06-09-33-51_8X2VmbbW4jI8ob8bO1Q82m2DopyM6FLq_2023-07-06-094420.json',
               flatten = TRUE)
cc_26A <- d_AS_26A$markerData
annees_26A <- cc_26A$year [cc_26A$type == 'Normal']
croissance_26A <- cc_26A$growth [cc_26A$type == 'Normal']
age_26A <- annees_26A - min(annees_26A) + 1 
arbre_26A <- d_AS_26A$sampleID

d_AS_26B <- fromJSON('../Données/ringdata-Acer Saccharum_AS_2023-06-15_26B_Megan Giguère_2023-07-06-09-44-58_9BiiKIrCHHJKoLMpwY5echNL9PwLugSt_2023-07-06-095215.json',
               flatten = TRUE)
cc_26B <- d_AS_26B$markerData
annees_26B <- cc_26B$year [cc_26B$type == 'Normal']
croissance_26B <- cc_26B$growth [cc_26B$type == 'Normal']
age_26B <- annees_26B - min(annees_26B) + 1 
arbre_26B <- d_AS_26B$sampleID

d_AS_27A <- fromJSON('../Données/ringdata-Acer Saccharum_AS_2023-06-15_27A_Megan Giguère_2023-07-06-09-53-12_StNbnFXCYbSCL3qR85qzXvcIhxE6RQH2_2023-07-06-100459.json',
               flatten = TRUE)
cc_27A <- d_AS_27A$markerData
annees_27A <- cc_27A$year [cc_27A$type == 'Normal']
croissance_27A <- cc_27A$growth [cc_27A$type == 'Normal']
age_27A <- annees_27A - min(annees_27A) + 1 
arbre_27A <- d_AS_27A$sampleID

d_AS_27B <- fromJSON('../Données/ringdata-Acer Saccharum_AS_2023-06-15_27B_Megan Giguère_2023-07-06-13-29-24_hhey4m8D74zpyduBZnFM0TtIYTqnEXMe_2023-07-06-135750.json',
               flatten = TRUE)
cc_27B <- d_AS_27B$markerData
annees_27B <- cc_27B$year [cc_27B$type == 'Normal']
croissance_27B <- cc_27B$growth [cc_27B$type == 'Normal']
age_27B <- annees_27B - min(annees_27B) + 1 
arbre_27B <- d_AS_27B$sampleID


# graphique --------------------------------------------------------------------
plot(x = age, y = croissance/1000, typ = 'l', 
     xlab = 'Âge estimé', ylab = 'Croissance (mm)',
     las = 1, col = '#106470', pch = 19, axes = FALSE,
     main = arbre)
axis (side = 1)
axis (side = 2, las = 1)

# graphique : croissance par année ---------------------------------------------
par(mfrow=c(3, 1))
plot(x = annees_SN, y = croissance_SN/1000, typ = 'l', 
     xlab = 'Année', ylab = 'Croissance (mm)',
     las = 1, col = 'white', pch = 19, axes = FALSE,
     main = '25', xlim = c(1945, 2025), ylim = c(0, 7))
axis (side = 1)
axis (side = 2, las = 1)
lines(x = annees_25A, y = croissance_25A/1000, col = 'red', lwd = 2)
lines(x = annees_25B, y = croissance_25B/1000, col = 'darkred', lwd = 2)

plot(x = annees_26A, y = croissance_26A/1000, typ = 'l', 
     xlab = 'Année', ylab = 'Croissance (mm)',
     las = 1, col = 'red', pch = 19, axes = FALSE,
     main = '26', xlim = c(1945, 2025), ylim = c(0, 7))
axis (side = 1)
axis (side = 2, las = 1)
lines(x = annees_26B, y = croissance_26B/1000, col = 'darkred', lwd = 2)
par(mfrow=c(1, 1))
plot(x = annees_27A, y = croissance_27A/1000, typ = 'l', 
     xlab = 'Année', ylab = 'Croissance (mm)',
     las = 1, col = 'purple', pch = 19, axes = FALSE, lwd = 2, 
     main = '27', xlim = c(1945, 2025), ylim = c(0, 5))
axis (side = 1)
axis (side = 2, las = 1)
lines(x = annees_27B, y = croissance_27B/1000, col = 'green', lwd = 2)

# correlation 
mod <- lm(croissance/1000 ~ age)

summary(mod)

# ajouter la correlation à la graphique
abline(mod, lty = 2, col = '#91b4a1', lwd = 2)
#legend(x = 0, y = 6, legend = '')
