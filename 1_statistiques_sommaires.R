#===============================================================================
#
#-------------------------------------------------------------------------------

# source les données de croissance ---------------------------------------------
source ('0_lire_données.R')

# enlève les données de 2023 pour le moment ------------------------------------
# TR - Il va falloir qu'on réflichit à leur utilisation éventuellement
d$g[d$year == 2023] <- NA
data$g[data$year == 2023] <- NA

# calcule la croissance moyenne pour chaque année ------------------------------
m <- d %>% group_by(year, spp, site) %>% 
  summarise(n = sum(!is.na(g)),
            g = mean(g, na.rm = TRUE), .groups = 'keep')
m$n[m$year == 2023] <- NA # TR - Voir commentaire ci-dessus concernant la croissance de 2023

# faire une graphique de toute les courbes de croissance et la moyenne pour AS -
par(mar = c(5, 5, 1, 5))
plot(x = as.numeric(levels(d$year[d$site == 1 & d$tree == 1]))[d$year[d$site == 1 & d$tree == 1]], 
     y = d$g[d$site == 1 & d$tree == 1] / 1e3, 
     typ = 'l', 
     xlab = 'Année', ylab = 'Croissance radiale (mm)', 
     xlim = c(1860, 2025), ylim = c (0.0, 10.0),
     lwd = 0.5, axes = FALSE,
     col = ifelse(d$spp[d$site == 1 & d$tree == 1] == 'ACSH', 
                  '#444444', '#F38D48'))
# boucle sur les arbres
s <- 1; arbres <- 1:45
for (a in arbres) {
    con <- d$site == s & d$tree == a
  lines(x = as.numeric(levels(d$year[con]))[d$year[con]], 
        y = d$g[con] / 1e3, 
        lwd = 0.5, 
        col = ifelse(d$spp[con] == 'ACSH', 
                     '#444444', '#F38D48'))  
} # fin boucle arbres
axis (side = 1, seq(1860, 2020, by = 40))
axis(side = 2, 0:10, las = 1)
lines(x = as.numeric(levels(m$year[m$spp == 'ACSA' & m$site == 1]))[m$year[m$spp == 'ACSA' & m$site == 1]], 
      y = m$g[m$spp == 'ACSA' & m$site == 1] / 1e3, 
      lwd = 3, col = '#cb4f00', lty = 1)
lines(x = as.numeric(levels(m$year[m$spp == 'ACSH' & m$site == 1]))[m$year[m$spp == 'ACSH' & m$site == 1]], 
      y = m$g[m$spp == 'ACSH' & m$site == 1] / 1e3, 
      lwd = 3, col = "#444444", lty = 1)

# add sample depth -------------------------------------------------------------
par(new = TRUE)
plot(x = as.numeric(levels(m$year[m$spp == 'ACSA' & m$site == 1]))[m$year[m$spp == 'ACSA' & m$site == 1]], 
     y = m$n[m$spp == 'ACSA' & m$site == 1], 
     xlab = '', ylab = '', lty = 2, 
     col = "#cb4f00", typ = 'l', lwd = 1.5, axes = FALSE)
lines(x = as.numeric(levels(m$year[m$spp == 'ACSH' & m$site == 1]))[m$year[m$spp == 'ACSH' & m$site == 1]], 
      y = m$n[m$spp == 'ACSH' & m$site == 1], lty = 2, lwd = 1.5)
axis(side = 4, las = 1)
mtext(side = 4, line = 3, text = "Échantillon (nombre d'arbres)")

# faire une graphique de toute les courbes de croissance et la moyenne pour VA -
par(mar = c(5, 5, 1, 5))
plot(x = as.numeric(levels(d$year[d$site == 3 & d$tree == 1]))[d$year[d$site == 3 & d$tree == 1]], 
     y = d$g[d$site == 3 & d$tree == 1] / 1e3, 
     typ = 'l', 
     xlab = 'Année', ylab = 'Croissance radiale (mm)', 
     xlim = c(1860, 2025), ylim = c (0.0, 10.0),
     lwd = 0.5, axes = FALSE,
     col = ifelse(d$spp[d$site == 1 & d$tree == 1] == 'ACRU', 
                  'darkred', '#F38D48'))
# boucle sur les arbres
s <- 3; arbres <- c(1:5, 7:9, 19, 21, 25, 27, 30)
for (a in arbres) {
  con <- d$site == s & d$tree == a
  lines(x = as.numeric(levels(d$year[con]))[d$year[con]], 
        y = d$g[con] / 1e3, 
        lwd = 0.5, 
        col = ifelse(d$spp[con] == 'ACRU', 
                     'darkred', '#F38D48'))  
} # fin boucle arbres
axis (side = 1, seq(1860, 2020, by = 40))
axis(side = 2, 0:10, las = 1)
lines(x = as.numeric(levels(m$year[m$spp == 'ACSA' & m$site == 3]))[m$year[m$spp == 'ACSA' & m$site == 3]], 
      y = m$g[m$spp == 'ACSA' & m$site == 3] / 1e3, 
      lwd = 3, col = '#cb4f00', lty = 1)
lines(x = as.numeric(levels(m$year[m$spp == 'ACRU' & m$site == 1]))[m$year[m$spp == 'ACRU' & m$site == 1]], 
      y = m$g[m$spp == 'ACRU' & m$site == 1] / 1e3, 
      lwd = 3, col = "darkred", lty = 1)

# add sample depth -------------------------------------------------------------
par(new = TRUE)
plot(x = as.numeric(levels(m$year[m$spp == 'ACSA' & m$site == 3]))[m$year[m$spp == 'ACSA' & m$site == 3]], 
     y = m$n[m$spp == 'ACSA' & m$site == 3], 
     xlab = '', ylab = '', lty = 2, 
     col = "#cb4f00", typ = 'l', lwd = 1.5, axes = FALSE)
lines(x = as.numeric(levels(m$year[m$spp == 'ACRU' & m$site == 1]))[m$year[m$spp == 'ACRU' & m$site == 1]], 
      y = m$n[m$spp == 'ACRU' & m$site == 1], lty = 2, lwd = 1.5)
axis(side = 4, las = 1)
mtext(side = 4, line = 3, text = "Échantillon (nombre d'arbres)")

# plot previous year's growth versus sap yield ---------------------------------
par(mar = c(5, 5, 1, 1))
con <- data$spp == "ACSA" & data$year == 2022 & data$site == 1
plot(x = data$p01_g[con],
     y = data$sap_volume[con],
     xlab = "Croissance radiale de l'année précédente (mm)", 
     ylab = "Volume de sève (litres)",
     axes = FALSE, pch = 17, col = "#cb4f0099", 
     xlim = c(0, 5), ylim = c(0, 120), cex = 1.2)
con <- data$spp == "ACSA" & data$year == 2023 & data$site == 1
points(x = data$p01_g[con],
       y = data$sap_volume[con], 
       pch = 2, lwd = 2, col = "#cb4f00aa", cex = 1.2)
con <- data$spp == "ACSA" & data$year == 2022 & data$site == 3
points(x = data$p01_g[con],
       y = data$sap_volume[con], 
       pch = 19, col = "#cb4f00aa", cex = 1.2)
con <- data$spp == "ACSA" & data$year == 2023 & data$site == 3
points(x = data$p01_g[con],
       y = data$sap_volume[con], 
       pch = 1, lwd = 2, col = "#cb4f00aa", cex = 1.2)
con <- data$spp == "ACSH" & data$year == 2022 & data$site == 1
points(x = data$p01_g[con],
       y = data$sap_volume[con], 
       pch = 17, col = "#44444499", cex = 1.2)
con <- data$spp == "ACSH" & data$year == 2023 & data$site == 1
points(x = data$p01_g[con],
       y = data$sap_volume[con], 
       pch = 2, lwd = 2, col = "#444444aa", cex = 1.2)
axis(side = 1)
axis(side = 2, las = 1)
# abline(lm(sap_volume ~ p01_g, subset = spp == "ACSA",
#           data = data), col = "#cb4f00", lwd = 3, lty = 2)
# abline(lm(sap_volume ~ p01_g, subset = spp == "ACSH",
#           data = data), col = "#444444", lwd = 3, lty = 2)
legend(x = 3.6, y = 115, 
       legend = c("Érable à sucre; AS", "Érable argenté; AS", "Érable à sucre; VA"), 
       pch = c(2, 2, 1), box.lty = 0, 
       col = c("#cb4f00aa", "#444444aa", "#cb4f00aa"), lwd = 2, lty = 0, cex = 1.2)
legend(x = 3.4, y = 115, 
       legend = c("", "", ""), 
       pch = c(17, 17, 19), box.lty = 0, 
       col = c("#cb4f00aa", "#444444aa", "#cb4f00aa"),  lty = 0, cex = 1.2)
text("2022", x = 3.4, y = 115, cex = 1.2)
text("2023", x = 3.75, y = 115, cex = 1.2)
rect(xleft = 3.2, ybottom = 95, xright = 5, ytop = 120)

# plot current year's growth versus sap yield ----------------------------------
par(mar = c(5, 5, 1, 1))
con <- data$spp == "ACSA" & data$year == 2022 & data$site == 1
plot(x = data$g[con],
     y = data$sap_volume[con],
     xlab = "Croissance radiale de l'année (mm)", 
     ylab = "Volume de sève (litres)",
     axes = FALSE, pch = 17, col = "#cb4f0099", 
     xlim = c(0, 5), ylim = c(0, 120), cex = 1.2)
# TR - Nous n'avons pas assez de données pour ACSH!!!
# con <- data$spp == "ACSH" & data$year == 2022 & data$site == 1
# points(x = data$g[con],
#        y = data$sap_volume[con], 
#        pch = 17, col = "#44444499")
con <- data$spp == "ACSA" & data$year == 2022 & data$site == 3
points(x = data$g[con],
       y = data$sap_volume[con], 
       pch = 19, col = "#cb4f00aa", cex = 1.2)
axis(side = 1)
axis(side = 2, las = 1)
# abline(lm(sap_volume ~ g, subset = spp == "ACSA",
#           data = data), col = "#cb4f0066", lwd = 3, lty = 2)
# abline(lm(sap_volume ~ g, subset = spp == "ACSH",
#           data = data), col = "#44444466", lwd = 3, lty = 2)

# plot average growth of previous five years versus sap yield ------------------
par(mar = c(5, 5, 1, 1))
con <- data$spp == "ACSA" & data$year == 2022 & data$site == 1
plot(x = data$p05_g[con],
     y = data$sap_volume[con],
     xlab = "Croissance radiale moyenne des 5 dernières années (mm)", 
     ylab = "Volume de sève (litres)",
     axes = FALSE, pch = 17, col = "#cb4f0099", 
     xlim = c(0, 5), ylim = c(0, 120), cex = 1.2)
con <- data$spp == "ACSH" & data$year == 2022 & data$site == 1
points(x = data$p05_g[con],
       y = data$sap_volume[con], 
       pch = 17, col = "#44444499", cex = 1.2)
con <- data$spp == "ACSA" & data$year == 2023 & data$site == 1
points(x = data$p05_g[con],
       y = data$sap_volume[con], 
       pch = 2, lwd = 2, col = "#cb4f00aa", cex = 1.2)
con <- data$spp == "ACSH" & data$year == 2023 & data$site == 1
points(x = data$p05_g[con],
       y = data$sap_volume[con], 
       pch = 2, lwd = 2, col = "#444444aa", cex = 1.2)
con <- data$spp == "ACSA" & data$year == 2022 & data$site == 3
points(x = data$p05_g[con],
       y = data$sap_volume[con], 
       pch = 19, col = "#cb4f00aa", cex = 1.2)
con <- data$spp == "ACSA" & data$year == 2023 & data$site == 3
points(x = data$p05_g[con],
       y = data$sap_volume[con], 
       pch = 1,  lwd = 2, col = "#cb4f00aa", cex = 1.2)
axis(side = 1)
axis(side = 2, las = 1)
# abline(lm(sap_volume ~ p05_g, subset = spp == "ACSA",
#           data = data), col = "#cb4f00", lwd = 3, lty = 2)
# abline(lm(sap_volume ~ p05_g, subset = spp == "ACSH",
#           data = data), col = "#444444", lwd = 3, lty = 2)

# plot average growth of previous ten years versus sap yield ------------------
par(mar = c(5, 5, 1, 1))
con <- data$spp == "ACSA" & data$year == 2022 & data$site == 1
plot(x = data$p10_g[con],
     y = data$sap_volume[con],
     xlab = "Croissance radiale moyenne des 10 dernières années (mm)",
     ylab = "Volume de sève (litres)",
     axes = FALSE, pch = 17, col = "#cb4f0099",
     xlim = c(0, 5), ylim = c(0, 120), cex = 1.2)
con <- data$spp == "ACSH" & data$year == 2022 & data$site == 1
points(x = data$p10_g[con],
       y = data$sap_volume[con],
       pch = 17, col = "#44444499", cex = 1.2)
con <- data$spp == "ACSA" & data$year == 2023 & data$site == 1
points(x = data$p10_g[con],
       y = data$sap_volume[con],
       pch = 2, lwd = 2, col = "#cb4f00aa", cex = 1.2)
con <- data$spp == "ACSH" & data$year == 2023 & data$site == 1
points(x = data$p10_g[con],
       y = data$sap_volume[con],
       pch = 2, lwd = 2, col = "#444444aa", cex = 1.2)
con <- data$spp == "ACSA" & data$year == 2022 & data$site == 3
points(x = data$p10_g[con],
       y = data$sap_volume[con],
       pch = 19, col = "#cb4f00aa", cex = 1.2)
con <- data$spp == "ACSA" & data$year == 2023 & data$site == 3
points(x = data$p10_g[con],
       y = data$sap_volume[con],
       pch = 1, lwd = 2, col = "#cb4f00aa", cex = 1.2)
axis(side = 1)
axis(side = 2, las = 1)
# abline(lm(sap_volume ~ p10_g, subset = spp == "ACSA",
#           data = data), col = "#cb4f00", lwd = 3, lty = 2)
# abline(lm(sap_volume ~ p10_g, subset = spp == "ACSH",
#           data = data), col = "#444444", lwd = 3, lty = 2)

# plot sap yield over dbh ------------------------------------------------------
con <- data$spp == "ACSA" & data$year == 2022 & data$site == 1
plot(x = data$dbh[con],
     y = data$sap_volume[con],
     xlab = "Diamètre à l'hauteur de la poitrine (cm)", 
     ylab = "Volume de sève (litres)",
     axes = FALSE, pch = 17, col = "#cb4f0099", 
     xlim = c(20, 85), ylim = c(0, 120), cex = 1.2)
con <- data$spp == "ACSH" & data$year == 2022 & data$site == 1
points(x = data$dbh[con],
       y = data$sap_volume[con], 
       pch = 17, col = "#44444499", cex = 1.2)
con <- data$spp == "ACSA" & data$year == 2023 & data$site == 1
points(x = data$dbh[con],
       y = data$sap_volume[con], 
       pch = 2, lwd = 2, col = "#cb4f00aa", cex = 1.2)
con <- data$spp == "ACSH" & data$year == 2023 & data$site == 1
points(x = data$dbh[con],
       y = data$sap_volume[con], 
       pch = 2, lwd = 2, col = "#444444aa", cex = 1.2)
con <- data$spp == "ACSA" & data$year == 2022 & data$site == 3
points(x = data$dbh[con],
       y = data$sap_volume[con], 
       pch = 19, col = "#cb4f00aa", cex = 1.2)
con <- data$spp == "ACSA" & data$year == 2023 & data$site == 3
points(x = data$dbh[con],
       y = data$sap_volume[con], 
       pch = 1, lwd = 2, col = "#cb4f00aa", cex = 1.2)
axis(side = 1)
axis(side = 2, las = 1)
# abline(lm(sap_volume ~ dbh + 0, subset = spp == "ACSA",
#           data = data), col = "#cb4f00", lwd = 3, lty = 2)
# abline(lm(sap_volume ~ dbh + 0, subset = spp == "ACSH",
#           data = data), col = "#444444", lwd = 3, lty = 2)

# plot previous year's growth versus sap sugar content -------------------------
con <- data$spp == "ACSA" & data$year == 2022 & data$site == 1
plot(x = data$p01_g[con],
     y = data$sap_brix[con],
     xlab = "Croissance radiale de l'année précédente (mm)", 
     ylab = expression(paste("Concentration de sucre (",degree,"Brix)", sep = "")),
     axes = FALSE, pch = 17, col = "#cb4f0099", 
     xlim = c(0, 5), ylim = c(0, 5), cex = 1.2)
con <- data$spp == "ACSH" & data$year == 2022 & data$site == 1
points(x = data$p01_g[con],
       y = data$sap_brix[con],
       pch = 17, col = "#44444499", cex = 1.2)
con <- data$spp == "ACSA" & data$year == 2023 & data$site == 1
points(x = data$p01_g[con],
       y = data$sap_brix[con],
       pch = 2, lwd = 2, col = "#cb4f00aa", cex = 1.2)
con <- data$spp == "ACSH" & data$year == 2023 & data$site == 1
points(x = data$p01_g[con],
       y = data$sap_brix[con],
       pch = 2, lwd = 2, col = "#444444aa", cex = 1.2)
con <- data$spp == "ACSA" & data$year == 2022 & data$site == 3
points(x = data$p01_g[con],
       y = data$sap_brix[con],
       pch = 19, col = "#cb4f00aa", cex = 1.2)
con <- data$spp == "ACSA" & data$year == 2023 & data$site == 3
points(x = data$p01_g[con],
       y = data$sap_brix[con],
       pch = 1, lwd = 2, col = "#cb4f00aa", cex = 1.2)
axis(side = 1)
axis(side = 2, las = 1)
# abline(lm(sap_brix ~ p01_g, subset = spp == "ACSA", data = data), 
#        col = "#cb4f0066", lwd = 3, lty = 2)
# abline(lm(sap_brix ~ p01_g, subset = spp == "ACSH", data = data), 
#        col = "#44444466", lwd = 3, lty = 2)

# plot current year's growth versus sap yield ----------------------------------
con <- data$spp == "ACSA" & data$year == 2022 & data$site == 1
plot(x = data$g[con],
     y = data$sap_brix[con],
     xlab = "Croissance radiale de l'année (mm)", 
     ylab = expression(paste("Concentration de sucre (",degree,"Brix)", sep = "")),
     axes = FALSE, pch = 17, col = "#cb4f0099", 
     xlim = c(0, 5), ylim = c(0, 5), cex = 1.2)
con <- data$spp == "ACSH" & data$year == 2022 & data$site == 1
points(x = data$g[con],
       y = data$sap_brix[con],
       pch = 17, col = "#44444499", cex = 1.2)
con <- data$spp == "ACSA" & data$year == 2023 & data$site == 1
points(x = data$g[con],
       y = data$sap_brix[con],
       pch = 2, lwd = 2, col = "#cb4f00aa", cex = 1.2)
con <- data$spp == "ACSH" & data$year == 2023 & data$site == 1
points(x = data$g[con],
       y = data$sap_brix[con],
       pch = 2, lwd = 2, col = "#444444aa", cex = 1.2)
con <- data$spp == "ACSA" & data$year == 2022 & data$site == 3
points(x = data$g[con],
       y = data$sap_brix[con],
       pch = 19, col = "#cb4f00aa", cex = 1.2)
con <- data$spp == "ACSA" & data$year == 2023 & data$site == 3
points(x = data$g[con],
       y = data$sap_brix[con],
       pch = 1, lwd = 2, col = "#cb4f00aa", cex = 1.2)
axis(side = 1)
axis(side = 2, las = 1)
# abline(lm(sap_brix ~ g, subset = spp == "ACSA", data = data), 
#        col = "#cb4f00", lwd = 3, lty = 2)
# abline(lm(sap_brix ~ g, subset = spp == "ACSH", data = data), 
#        col = "#444444", lwd = 3, lty = 2)

# plot average growth of previous 5 years versus sap yield ---------------------
con <- data$spp == "ACSA" & data$year == 2022 & data$site == 1
plot(x = data$p05_g[con],
     y = data$sap_brix[con],
     xlab = "Croissance radiale moyenne des 5 dernières années (mm)", 
     ylab = expression(paste("Concentration de sucre (",degree,"Brix)", sep = "")),
     axes = FALSE, pch = 17, col = "#cb4f0099", 
     xlim = c(0, 5), ylim = c(0, 5), cex = 1.2)
con <- data$spp == "ACSH" & data$year == 2022 & data$site == 1
points(x = data$p05_g[con],
       y = data$sap_brix[con],
       pch = 17, col = "#44444499", cex = 1.2)
con <- data$spp == "ACSA" & data$year == 2023 & data$site == 1
points(x = data$p05_g[con],
       y = data$sap_brix[con],
       pch = 2, lwd = 2, col = "#cb4f00aa", cex = 1.2)
con <- data$spp == "ACSH" & data$year == 2023 & data$site == 1
points(x = data$p05_g[con],
       y = data$sap_brix[con],
       pch = 2, lwd = 2, col = "#444444aa", cex = 1.2)
con <- data$spp == "ACSA" & data$year == 2022 & data$site == 3
points(x = data$p05_g[con],
       y = data$sap_brix[con],
       pch = 19, col = "#cb4f00aa", cex = 1.2)
con <- data$spp == "ACSA" & data$year == 2023 & data$site == 3
points(x = data$p05_g[con],
       y = data$sap_brix[con],
       pch = 1, lwd = 2, col = "#cb4f00aa", cex = 1.2)
axis(side = 1)
axis(side = 2, las = 1)
# abline(lm(sap_brix ~ p05_g, subset = spp == "ACSA", data = data), 
#        col = "#cb4f0066", lwd = 3, lty = 2)
# abline(lm(sap_brix ~ p05_g, subset = spp == "ACSH", data = data), 
#        col = "#44444466", lwd = 3, lty = 2)

# plot average growth of previous 10 years versus sap yield ---------------------
con <- data$spp == "ACSA" & data$year == 2022 & data$site == 1
plot(x = data$p10_g[con],
     y = data$sap_brix[con],
     xlab = "Croissance radiale moyenne des 10 dernières années (mm)", 
     ylab = expression(paste("Concentration de sucre (",degree,"Brix)", sep = "")),
     axes = FALSE, pch = 17, col = "#cb4f0099", 
     xlim = c(0, 5), ylim = c(0, 5), cex = 1.2)
con <- data$spp == "ACSH" & data$year == 2022 & data$site == 1
points(x = data$p10_g[con],
       y = data$sap_brix[con],
       pch = 17, col = "#44444499", cex = 1.2)
con <- data$spp == "ACSA" & data$year == 2023 & data$site == 1
points(x = data$p10_g[con],
       y = data$sap_brix[con],
       pch = 2, lwd = 2, col = "#cb4f00aa", cex = 1.2)
con <- data$spp == "ACSH" & data$year == 2023 & data$site == 1
points(x = data$p10_g[con],
       y = data$sap_brix[con],
       pch = 2, lwd = 2, col = "#444444aa", cex = 1.2)
con <- data$spp == "ACSA" & data$year == 2022 & data$site == 3
points(x = data$p10_g[con],
       y = data$sap_brix[con],
       pch = 19, col = "#cb4f00aa", cex = 1.2)
con <- data$spp == "ACSA" & data$year == 2023 & data$site == 3
points(x = data$p10_g[con],
       y = data$sap_brix[con],
       pch = 1, lwd = 2, col = "#cb4f00aa", cex = 1.2)
axis(side = 1)
axis(side = 2, las = 1)
# abline(lm(sap_brix ~ p10_g, subset = spp == "ACSA", data = data), 
#        col = "#cb4f0066", lwd = 3, lty = 2)
# abline(lm(sap_brix ~ p10_g, subset = spp == "ACSH", data = data), 
#        col = "#44444466", lwd = 3, lty = 2)

#===============================================================================

