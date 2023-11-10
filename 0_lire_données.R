#===============================================================================
# Lire les données de croissance et de coulée 
#-------------------------------------------------------------------------------

# À faire :
#-------------------------------------------------------------------------------
#     - Corrige l'espèces pour VA (érables rouges ne sont pas bien identifié 
#       dans les données de croissance)
#     - Il y a seulement des données de croissance pour 13 des 30 arbres de VA.


# charger les dépendences ------------------------------------------------------
if (!existsFunction("fromJSON")) library("jsonlite")
if (!existsFunction("%>%")) library ("tidyverse")
if (!existsFunction("yday")) library ("lubridate")
if (!existsFunction("read_sheet")) library ("googlesheets4")
if (!existsFunction("read_excel")) library ("readxl")
if (!existsFunction("add.alpha")) library ("prettyGraphs")
if (!existsFunction("str_sub")) library("stringr")
if (!existsFunction("rollmean")) library("zoo")

# boucle pour lire les données de différents stations --------------------------
for (s in c("AS","SN","VA")){
  
  # chemin pour dossier avec les fichiers de données ---------------------------
  dossier <- paste0("../Données/1_données_WIAD/",s,"/")
  
  # dossier avec les données pour l'Assomption ---------------------------------
  noms_fichier <- list.files(path = dossier)  

  # boucle pour lire les différents fichier de WIAD ----------------------------
  for (f in 1:length(noms_fichier)){
    
    # lire le fichier
    tmp <- fromJSON(paste0(dossier, noms_fichier[f]), flatten = TRUE)
    
    # réduit à seulement les informations importantes --------------------------
    t <- tibble(tmp$markerData, spp = tmp$species, tree = tmp$sampleID, 
                plot = tmp$plotID, site = s, h = tmp$sampleHeight, 
                a = tmp$sampleAzimuth, date = tmp$sampleDate, 
                p = tmp$pithInImage) %>% 
      filter(type != "Linker") %>%
      select(-no, -x, -y, -relx, -rely, -pixels)
    
    # ajoute âge estimé au données ---------------------------------------------
    t <- t %>% mutate(age = year - min(year)) # TR - I ought to do this for both cores together instead of individually.
    
    # Ajoute les données -------------------------------------------------------
    if(f == 1 & s == "AS"){
      d <- t
    } else {
      d <- add_row(d, t)
    }
  } # fin boucle fichier
} # fin boucle stations

# divise les IDs de l'arbre et de la carotte -----------------------------------
d <- d %>% mutate(carotte = str_sub(tree, -1, -1),
                  tree = str_sub(tree, 1, nchar(tree)-1))

# obtient la age maximal pour chaque arbre (plutôt que celle de la carotte) ----
d <- d %>% group_by(tree) %>% mutate(age_max = max(age))

# calculer les statistiques sommaires pour les stations ------------------------
d %>% group_by(site, spp) %>% filter(type == "Normal") %>%
  summarise(c_moy = mean(growth, na.rm = T),
            c_sd = sd(growth, na.rm = T),
            vieux = max(age_max), .groups = "keep")
d %>% group_by(site, spp, tree) %>% filter(type == "Normal") %>%
  summarise(age = mean(age_max), .groups = "keep") %>% 
  group_by(site, spp) %>% 
  summarise(a_moy = mean(age), a_sd = sd(age), .groups = "keep")

# Enlever tous les données qui ne sont pas des cernes --------------------------
d <- d %>% filter(!is.na(growth) & type == 'Normal') %>% select(-type, -plot)
# TR - je dois corriger les cernes les plus vielles pour des pith pour SN et 
# possiblement VA

# Renommer les stations --------------------------------------------------------
d <- d %>% mutate(site = case_when(site == "AS" ~ "1",
                                   site == "VA" ~ "3",
                                   site == "SN" ~ "6"))

# Renommer les espèces --------------------------------------------------------
d <- d %>% mutate(spp = case_when(spp == "Acer Saccharinum" ~ "ACSH",
                                  spp == "Acer Saccharum" ~ "ACSA",
                                  spp == "Acer Rubrum" ~ "ACRU"))
# TR - Les érables rouges à VA ne sont pas bien identifier en ce moment dans 
# les données de croissance

# seulement regarde l'Assomption et Vallée-Jonction pour le moment -------------
d <- d %>% filter(site %in% c("1","3"))

# arranger les entrées ---------------------------------------------------------
d <- d %>% arrange(tree, year, site, spp, h, a, date, p, age, age_max, growth) %>% 
  relocate(tree, year, site, spp, h, a, date, p, age, age_max, growth)

# calcule la croissance moyenne à travers les deux carottes --------------------
d <- d %>% group_by(tree, year, site, spp) %>% 
  summarise(g = mean(growth, na.rm = TRUE), 
            .groups = "drop") 

# ajoute la croissance de l'année précédente -----------------------------------
tmp <- d %>% group_by(tree) %>% 
  reframe(g = g, 
          p01_g = dplyr::lag(g, n = 1),
          p05_g = rollmean(g, k = 5, fill = NA, align = 'right'),
          p10_g = rollmean(g, k = 10, fill = NA, align = 'right'),
          p15_g = rollmean(g, k = 15, fill = NA, align = 'right'),
          p20_g = rollmean(g, k = 20, fill = NA, align = 'right'),
          p25_g = rollmean(g, k = 25, fill = NA, align = 'right'),
          p30_g = rollmean(g, k = 30, fill = NA, align = 'right')) 
d <- tibble(cbind(d, tmp[, 3:9]))

# convertir année, arbre, et station en facteur --------------------------------
d <- d %>% dplyr::mutate(year = factor(year),
                         tree = factor(tree),
                         site = factor(site))
                
#===============================================================================
# Télécharger et lire les données de coulée pour 2022 et 2023 ------------------
#-------------------------------------------------------------------------------

# authenthicate for spreadsheet and load the data from the acer-web sheet ------
gs4_auth(cache = ".secrets", email = "rademacher.tim@gmail.com")

# set url to google spreadsheet ------------------------------------------------
sheet_url <- "https://docs.google.com/spreadsheets/d/1Iup_x-uyfN-vk9oK7bQVfSP7XrihXAtJRVygnFNazig/edit#gid=1317380413"

# get acer-wab data from online sheet ------------------------------------------
AW_data_s <- read_sheet (ss = sheet_url, sheet = "01_sap_data",  
                         na = "NA",
                         col_types = "icccDciildddddddlicd")
AW_data_w <- read_sheet (ss = sheet_url, sheet = "04_wound_data",  
                         na = "NA",
                         col_types = "iccciDddddcdccccccc")
AW_data_t <- read_sheet (ss = sheet_url, sheet = "06_tree_data",  
                         na = "NA",
                         col_types = "iccciccdddddddddc")
AW_site_data <- read_sheet (ss = sheet_url, sheet = "07_site_data",  
                            na = "NA",
                            col_types = "iccdddDcDic")

# calculate an average bark thickness for each tree ----------------------------
bark <- AW_data_w %>% group_by(year, site, tree, tap) %>% 
  summarise(bark_thickness = mean(bark_thickness, na.rm = TRUE), 
            .groups = "drop")

# add bark_thickness to tree_data from wound_data ------------------------------
AW_data_t <- dplyr::left_join(AW_data_t, bark, 
                              by = c("year", "site", "tree", "tap")) %>%
  dplyr::select(-comment) 
# TR - Why are there two tap depth columns in the tree and wound data?

# add tapping date and tap removal date ----------------------------------------
AW_data_s <- dplyr::left_join(AW_data_s, AW_site_data, by = c("year", "site")) %>% 
  dplyr::select(-site_name, -n_trees, -comments, -comment)

# create datetime column, convert time column, and add year as factor ----------
AW_data_s <- AW_data_s %>% 
  dplyr::mutate(
    datetime = make_datetime (year = year(date), 
                              month = month(date),
                              day = day(date), hour, minute),
    time = format(datetime, "%H:%M"),
    year = factor (lubridate::year(date)),
    tree = factor(tree),
    site = factor(site),
    tap = factor(tap))
AW_data_t <- AW_data_t %>% 
  dplyr::mutate(year = factor(year),
                tree = factor(tree),
                site = factor(site),
                tap = factor(tap),
                dbh = dbh,
                tap_height = h_tap_ground)

# calculate mean sap succrose concentration (°Brix) ----------------------------
AW_data_s <- AW_data_s %>% 
  dplyr::mutate(sap_brix = rowMeans(dplyr::select(., sap_brix_1, sap_brix_2, sap_brix_3), 
                                    na.rm = TRUE),
                bucket_brix = rowMeans(dplyr::select(., bucket_brix_1, bucket_brix_2, bucket_brix_3), 
                                       na.rm = TRUE))

# add day of year column to AW data --------------------------------------------
AW_data_s <- AW_data_s %>% dplyr::mutate(doy = yday(date))

# combine the two data sets (tree-level data and sap flow data) ----------------
AW_data <- dplyr::left_join(
  AW_data_s, 
  AW_data_t, by = c("year", "site", "tree", "tap")) %>%
  dplyr::select(-cbh, -sap_brix_1, -sap_brix_2, -sap_brix_3, -bucket_brix_1, 
                -bucket_brix_2, -bucket_brix_3, -ice, -percent_ice, -c_tap, 
                -h_tap_ground, -h_tap_root_collar, -tap_time, -species, -running)

# re-arrange AW data for ease of comparison ------------------------------------
AW_data <- AW_data %>% 
  dplyr::relocate(site, tree, tap, date, time, hour, minute, datetime, year, 
                  doy, tap_date, tap_removal, lat, lon, alti, spp, sap_volume, 
                  sap_brix, bucket_brix, n_taps, tap_bearing, tap_height, 
                  tap_depth, tap_width, d_tap, dbh, bark_thickness)

# remove outliers on 2022-03-12 due to most sap being frozen and 2022-03-14, as 
# there was only very little sap (i.e., 50 or 100 ml with one tree at 300 ml) --
AW_data <- AW_data %>% 
  dplyr::filter(!(site== "1" & date %in% as_date(c("2022-03-12","2022-03-14"))))

# remove outliers due to rain water getting into the bucket --------------------
sap_data <- AW_data %>% 
  dplyr::filter(
    !(site == "1" & date == as_date("2022-03-07") & tree == 27),
    !(site == "1" & date == as_date("2022-03-18") & tree %in% c(16, 25, 27)),
    !(site == "1" & date == as_date("2022-04-18") & tree %in% c(15, 16, 27)),
    !(site == "1" & date == as_date("2022-04-19") & tree %in% c(1:3, 5:8, 14, 25, 32, 33)),
    !(site == "1" & date == as_date("2022-04-30") & tree %in% c(15)))

# add days since tapping column to data ----------------------------------------
sap_data <- sap_data %>% dplyr::mutate(
  days_since_tapping = as.integer(difftime(date, tap_date, units = "days"))
)

# create a seasonal summary for each tap ---------------------------------------
seasonal_data <- sap_data %>% 
  dplyr::group_by(site, tree, tap, year) %>%
  dplyr::summarise(
    lat = mean(lat, na.rm = TRUE),
    spp = unique(spp),
    sap_volume = sum(sap_volume, na.rm = TRUE) / 1e3, # in litres
    sap_brix = mean(sap_brix, na.rm = TRUE),
    tap_depth = mean(tap_depth, na.rm = TRUE),
    tap_width = mean(tap_width, na.rm = TRUE),
    n_taps = as.integer(mean(n_taps, na.rm = TRUE)),
    tap_bearing = mean(tap_bearing, na.rm = TRUE),
    tap_height = mean(tap_height, na.rm = TRUE),
    dbh = mean(dbh, na.rm = TRUE),
    .groups = "drop")

# remove the data for taps that did have no sap flow at all --------------------
seasonal_data <- seasonal_data %>% dplyr::filter(sap_volume > 0)

# create log_yield variable to avoid fitting a log-normal-----------------------
seasonal_data$log_yield <- log(seasonal_data$sap_volume)

# remove single data point from Norway maple -----------------------------------
seasonal_data <- seasonal_data %>% filter(spp != "ACPL")

# group by sites and year to get sao run dates for each location ---------------
mid_season <- sap_data %>% 
  dplyr::group_by(site, year) %>% 
  dplyr::filter(sap_volume > 0) %>% 
  dplyr::select(site, year, doy) %>% 
  dplyr::distinct() %>% 
  dplyr::summarise(median_doy = floor(median(doy)), .groups = "drop")

# add median doy for sugaring season to sap_data -------------------------------
sap_data <- dplyr::left_join(sap_data, mid_season, by = c("site", "year"))

# aggregate early-season data --------------------------------------------------
early_data <- sap_data %>% 
  dplyr::filter(doy <= median_doy) %>% # only days before or on the median sap run day
  dplyr::group_by(site, tree, tap, year) %>%
  dplyr::summarise(sap_volume_e = sum(sap_volume, na.rm = TRUE) / 1e3, # in litres
                   sap_brix_e = mean(sap_brix, na.rm = TRUE),
                   .groups = "drop")

# aggregate late-season data ---------------------------------------------------
late_data <- sap_data %>% 
  dplyr::filter(doy > median_doy) %>% # only days after the median sap run day
  dplyr::group_by(site, tree, tap, year) %>%
  dplyr::summarise(sap_volume_l = sum(sap_volume, na.rm = TRUE) / 1e3, # in litres
                   sap_brix_l = mean(sap_brix, na.rm = TRUE),
                   .groups = "drop")

# add early- and late-season data to seasonal_data -----------------------------
seasonal_data <- seasonal_data %>% 
  dplyr::left_join(early_data, by = c("site", "tree", "tap", "year")) %>%
  dplyr::left_join(late_data, by = c("site", "tree", "tap", "year"))

# fusionner les données de croissance et de coulée -----------------------------
data <- left_join(seasonal_data, d, by = c("site", "tree", "year")) %>%  
# l'espèce est également dans les données de la coulée -------------------------
 select(-spp.y) %>% rename("spp" = spp.x)

# convertir la croissance en mm ------------------------------------------------
data <- data %>% mutate(g = g / 1e3,
                        p01_g = p01_g / 1e3,
                        p05_g = p05_g / 1e3,
                        p10_g = p10_g / 1e3,
                        p15_g = p15_g / 1e3,
                        p20_g = p20_g / 1e3,
                        p25_g = p25_g / 1e3,
                        p30_g = p30_g / 1e3)

# get some basic stats for intro -----------------------------------------------
sap_data %>% dplyr::filter(sap_volume > 0 & !is.na (sap_volume)) %>% dplyr::count() # number of daily sap volume measurements
sap_data %>% dplyr::filter(!is.na (sap_brix)) %>% dplyr::count() # number of daily sugar content measurements
sap_data %>% dplyr::group_by(site, tree, tap, year) %>% dplyr::n_groups() # number of taps, as taps differ by year 
sap_data %>% dplyr::group_by(site, tree, year) %>% dplyr::n_groups() # number of tree years
sap_data %>% dplyr::group_by(site, tree) %>% dplyr::n_groups() # number of different tree years
sap_data %>% dplyr::group_by(site) %>% dplyr::n_groups() # number of sites
sap_data %>% dplyr::group_by(year) %>% dplyr::n_groups() # number of years
sap_data %>% dplyr::filter(spp == "ACSA") %>% dplyr::group_by(site, tree) %>% dplyr::n_groups() # number of sugar maples
sap_data %>% dplyr::filter(spp == "ACRU") %>% dplyr::group_by(site, tree) %>% dplyr::n_groups() # number of red maples
sap_data %>% dplyr::filter(spp == "ACSH") %>% dplyr::group_by(site, tree) %>% dplyr::n_groups() # number of silver maples
sap_data %>% dplyr::filter(spp == "ACPL") %>% dplyr::group_by(site, tree) %>% dplyr::n_groups() # number of Norway maples

# fait le ménage ---------------------------------------------------------------
rm(tmp, t, s, f, noms_fichier, dossier, late_data, early_data, mid_season, bark, 
   AW_data, AW_data_s, AW_data_t, AW_data_w, AW_site_data, sheet_url, sap_data)
#===============================================================================

