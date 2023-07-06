# dépendence
library('tidyverse')


dir <- '../Données/SN/'
noms_fichiers <- list.files(path = dir)

for(c in 1:length(noms_fichiers)) {
  # lire le fichier
  tmp <- fromJSON(paste0(dir, noms_fichiers[c]),
                     flatten = TRUE)
  
  # extrait
  cc_tmp <- tmp$markerData %>% 
    filter(type %in% c('Normal','Pith')) %>% 
    select(-c(no, x, y, relx, rely, pixels)) %>% 
    add_column(site = 'SN',
               id = tmp$sampleID)
  
  if (c == 1) {
    d <-  cc_tmp
  } else {
    d <- rbind(d, cc_tmp)
  }
}

d %>% group_by(id) 

y_max <- 6

# arbre 1 à 4
par(mfrow = c(4, 1))
for (c in 1:4){
  # fait une graphique
  plot(x = d$year[d$id == paste0(c,'A')], 
       y = d$growth[d$id == paste0(c,'A')]/1000, 
     typ = 'l', 
     xlab = 'Années', ylab = 'Croissance (mm)',
     las = 1, col = '#106470', axes = FALSE,
     xlim = c(1850, 2023), ylim = c(0, y_max), main = c)
  axis(1)
  axis(2, las = 1)
  lines(d$year[d$id == paste0(c, 'B')], 
        d$growth[d$id == paste0(c,'B')]/1000, col = 'red')
}

# arbre 5 à 8
par(mfrow = c(4, 1))
for (c in 5:8){
  plot(x = d$year[d$id == paste0(c,'A')], 
       y = d$growth[d$id == paste0(c,'A')]/1000, 
       typ = 'l', 
       xlab = 'Années', ylab = 'Croissance (mm)',
       las = 1, col = '#106470', axes = FALSE,
       xlim = c(1850, 2023), ylim = c(0, y_max), main = c)
  axis(1)
  axis(2, las = 1)
  lines(d$year[d$id == paste0(c, 'B')], 
        d$growth[d$id == paste0(c,'B')]/1000,
        col = 'red')
}

# arbre 11 à 15
par(mfrow = c(5, 1))
for (c in 11:15){
  plot(x = d$year[d$id == paste0(c,'A')], 
       y = d$growth[d$id == paste0(c,'A')]/1000, 
       typ = 'l', 
       xlab = 'Années', ylab = 'Croissance (mm)',
       las = 1, col = '#106470', axes = FALSE,
       xlim = c(1850, 2023), ylim = c(0, 7), main = c)
  axis(1)
  axis(2, las = 1)
  lines(d$year[d$id == paste0(c, 'B')], 
        d$growth[d$id == paste0(c,'B')]/1000, col = 'red')
}

# arbre 16 à 20
par(mfrow = c(5, 1))
for (c in 16:20){
  plot(x = d$year[d$id == paste0(c,'A')], 
       y = d$growth[d$id == paste0(c,'A')]/1000, 
       typ = 'l', 
       xlab = 'Années', ylab = 'Croissance (mm)',
       las = 1, col = '#106470', axes = FALSE,
       xlim = c(1850, 2023), ylim = c(0, 7), main = c)
  axis(1)
  axis(2, las = 1)
  lines(d$year[d$id == paste0(c, 'B')], 
        d$growth[d$id == paste0(c,'B')]/1000, lwd = 0.5)
}
