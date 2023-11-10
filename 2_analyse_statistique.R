#===============================================================================
#
#-------------------------------------------------------------------------------

# charger les dépendances ------------------------------------------------------
library("brms")


# source les données de croissance ---------------------------------------------
source ('0_lire_données.R')

# enlever les données inutilisées ----------------------------------------------
d1 <- data %>% filter(!is.na(dbh)) %>% 
  select(tree, tap, year, site, spp, sap_volume, dbh, g, p01_g) %>%
  mutate(spp = factor(spp))

# effet de la croissance de l'année précédente sur le volume de la coulée en 
# tennant compte du dhp --------------------------------------------------------
# utilise une distribution lognormal 
mod1.1 <- brms::brm(brms::bf(sap_volume ~
                             dbh +         # effet dhp
                             p01_g +
                             (1 | spp)  + # effet espèce 
                             (1 | site / tree) + # effet du site et interindivduel
                             (1 | year)), # effet année
                      data = d1 %>% filter(!is.na(sap_volume)),
                      family = lognormal(), 
                    # Je dois vérifier le choix des priors
                      prior = c(set_prior("normal(3.7, 10)", class = "Intercept"), # Corresponds to roughly 40L of sap or 1L of syrup with 40:1 conversion
                                set_prior("exponential(1)", class = "sigma"),
                                set_prior("normal(0, 2)", class = "b"),
                                set_prior("normal(0, 2)", class = "sd")),
                      cores = 4, chains = 4,
                      #control = list(adapt_delta = 0.98, max_treedepth = 11),
                      iter = 6000,
                      seed = 1352,
                      backend = "cmdstanr")

# posterior distribution checks ------------------------------------------------
plot(mod1.1)
plot(conditional_effects(mod1.1)) [[1]] + ggplot2::ylim(0, 120)
plot(conditional_effects(mod1.1)) [[2]] + ggplot2::ylim(0, 60)

# additional posterior distribution checks -------------------------------------
pp_check(mod1.1, ndraws = 100)
pp_check(mod1.1, type = "error_hist",  ndraws = 10)
pp_check(mod1.1, type = "scatter_avg", ndraws = 100)
# Error in the posterior distribution looks normally-distributed

# get model summary and coefficients -------------------------------------------
summary(mod1.1)
ranef(mod1.1)$spp [, , "Intercept"]
ranef(mod1.1)$year [, , "Intercept"]
ranef(mod1.1)$site [, , "Intercept"]

# effet de la croissance de l'année sur le volume de la coulée en tennant compte du dhp --------------------------------------------------------
# utilise une distribution lognormal 
mod1.2 <- brms::brm(brms::bf(sap_volume ~
                               dbh +         # effet dhp
                               g +
                               (1 | spp) +   # effet espèce 
                               (1 | site / tree) + # effet du site et interindividuel
                               (1 | year)),  # effet année
                    data = d1 %>% filter(!is.na(sap_volume)),
                    family = lognormal(), 
                    # Je dois vérifier le choix des priors
                    prior = c(set_prior("normal(3.7, 10)", class = "Intercept"), # Corresponds to roughly 40L of sap or 1L of syrup with 40:1 conversion
                              set_prior("exponential(1)", class = "sigma"),
                              set_prior("normal(0, 2)", class = "b"),
                              set_prior("normal(0, 2)", class = "sd")),
                    cores = 4, chains = 4,
                    #control = list(adapt_delta = 0.98, max_treedepth = 11),
                    iter = 6000,
                    seed = 1352,
                    backend = "cmdstanr")

# posterior distribution checks ------------------------------------------------
plot(mod1.2)
plot(conditional_effects(mod1.2)) [[1]] + ggplot2::ylim(0, 120)

# additional posterior distribution checks -------------------------------------
pp_check(mod1.2, ndraws = 100)
pp_check(mod1.2, type = "error_hist",  ndraws = 10)
pp_check(mod1.2, type = "scatter_avg", ndraws = 100)
# Error in the posterior distribution looks normally-distributed

# get model summary and coefficients -------------------------------------------
summary(mod1.2)
ranef(mod1.2)$spp [, , "Intercept"]
ranef(mod1.2)$year [, , "Intercept"]
ranef(mod1.2)$site [, , "Intercept"]

# enlever les données inutilisées ----------------------------------------------
d2 <- data %>% filter(!is.na(dbh)) %>% 
  select(tree, tap, year, spp, site, sap_brix, dbh, g, p01_g) %>%
  mutate(spp = factor(spp))

# effet de la croissance de l'année précédente sur la teneur en sucre de la 
# coulée en tennant compte du dhp ----------------------------------------------
# utilise une distribution normal
mod2.1 <- brms::brm(brms::bf(sap_brix ~
                               (1 | year) +  # effet année
                               p01_g +
                               dbh + 
                               (1 | spp) +   # effet espèce 
                               (1 | site / tree)),  # effet interindividuel
                    data = d2 %>% filter(!is.na(sap_brix)),
                    family = gaussian(), 
                    # Je dois vérifier le choix des priors
                    prior = c(set_prior("normal(2, 1)", class = "Intercept"),
                              set_prior("exponential(1)", class = "sigma"),
                              set_prior("normal(0, 2)", class = "b"),
                              set_prior("normal(0, 2)", class = "sd")),
                    cores = 4, chains = 4,
                    #control = list(adapt_delta = 0.98, max_treedepth = 11),
                    iter = 6000,
                    seed = 1352,
                    backend = "cmdstanr")

# posterior distribution checks ------------------------------------------------
plot(mod2.1)

# additional posterior distribution checks -------------------------------------
pp_check(mod2.1, ndraws = 100)
pp_check(mod2.1, type = "error_hist",  ndraws = 10)
pp_check(mod2.1, type = "scatter_avg", ndraws = 100)
# Error in the posterior distribution looks normally-distributed

# get model summary and coefficients -------------------------------------------
summary(mod2.1)
ranef(mod2.1)$spp [, , "Intercept"]
ranef(mod2.1)$year [, , "Intercept"]
ranef(mod2.1)$site [, , "Intercept"]

# effet de la croissance de l'année précédente sur la teneur en sucre de la 
# coulée en tennant compte du dhp ----------------------------------------------
# utilise une distribution normal
mod2.2 <- brms::brm(brms::bf(sap_brix ~
                               (1 | year) +  # effet année
                               g +
                               dbh +
                               (1 | spp) +   # effet espèce 
                               (1 | site / tree)),  # effet du site et interindividuel
                    data = d2 %>% filter(!is.na(sap_brix)),
                    family = gaussian(), 
                    # Je dois vérifier le choix des priors
                    prior = c(set_prior("normal(2, 1)", class = "Intercept"),
                              set_prior("exponential(1)", class = "sigma"),
                              set_prior("normal(0, 2)", class = "b"),
                              set_prior("normal(0, 2)", class = "sd")),
                    cores = 4, chains = 4,
                    #control = list(adapt_delta = 0.98, max_treedepth = 11),
                    iter = 6000,
                    seed = 1352,
                    backend = "cmdstanr")

# posterior distribution checks ------------------------------------------------
plot(mod2.2)

# additional posterior distribution checks -------------------------------------
pp_check(mod2.2, ndraws = 100)
pp_check(mod2.2, type = "error_hist",  ndraws = 10)
pp_check(mod2.2, type = "scatter_avg", ndraws = 100)
# Error in the posterior distribution looks normally-distributed

# get model summary and coefficients -------------------------------------------
summary(mod2.2)
ranef(mod2.2)$spp [, , "Intercept"]
ranef(mod2.2)$year [, , "Intercept"]
ranef(mod2.2)$site [, , "Intercept"]
