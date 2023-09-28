# 

# source les données de croissance ---------------------------------------------
source ('0_lire_données.R')

# extrait les id des arbres pour AS --------------------------------------------
arbres <- 1:45

# calcule la croissance moyenne pour chaque année ------------------------------
m <- d %>% group_by(year, spp) %>% 
  summarise(g = mean(g, na.rm = TRUE),
            n = sum(!is.na(g)), .groups = 'keep')


# faire une graphique de toute les courbes de croissance et la moyenne ---------
par(mar = c(5, 5, 1, 1))
plot(x = as.numeric(levels(d$year[d$tree == 1]))[d$year[d$tree == 1]], 
     y = d$g[d$tree == 1] / 1e3, 
     typ = 'l', 
     xlab = 'Année', ylab = 'Croissance radiale (mm)', 
     xlim = c(1860, 2025), ylim = c (0.0, 10.0),
     lwd = 0.5, axes = FALSE,
     col = ifelse(d$spp[d$tree == 1] == 'ACSH', 
                  '#444444', '#F38D48'))
for(t in 2:45){
  lines(x = as.numeric(levels(d$year[d$tree == arbres[t]]))[d$year[d$tree == arbres[t]]], 
        y = d$g[d$tree == arbres[t]] / 1e3, 
        lwd = 0.5, 
        col = ifelse(d$spp[d$tree == arbres[t]] == 'ACSH', 
                     '#444444', '#F38D48'))  
}
axis (side = 1, seq(1860, 2020, by = 40))
axis(side = 2, 0:10, las = 1)
lines(x = as.numeric(levels(m$year[m$spp == 'ACSA']))[m$year[m$spp == 'ACSA']], 
      y = m$g[m$spp == 'ACSA'] / 1e3, 
      lwd = 3, col = '#cb4f00', lty = 1)
lines(x =as.numeric(levels(m$year[m$spp == 'ACSH']))[m$year[m$spp == 'ACSH']], 
      y = m$g[m$spp == 'ACSH'] / 1e3, 
      lwd = 3, col = '#444444', lty = 1)

# add sample depth -------------------------------------------------------------
par(new = TRUE)
plot(x = m$year[m$spp == 'Acer Saccharum'], 
     y = m$n[m$spp == 'Acer Saccharum'], 
     xla = '', ylab = '',
     col = '#cb4f00', typ = 'l', lwd = 1.5, axes = FALSE)
lines(x = m$year[m$spp == 'Acer Saccharinum'], 
      y = m$n[m$spp == 'Acer Saccharinum'])
axis(side = 4, las = 1)

# plot previous year's growth versus sap yield ---------------------------------
plot(x = data$p01_g,
     y = data$sap_volume)

# plot current year's growth versus sap yield ----------------------------------
plot(x = data$p01_g,
     y = data$sap_brix)
