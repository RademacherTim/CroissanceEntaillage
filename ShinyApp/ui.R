#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("shiny")
library("tibble")

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Estimateur de la durabilité des pratiques d'entaillage"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("initial_dbh",
                        "Diamètre à hauteur de poitrine (cm) :",
                        min = 10,
                        max = 150,
                        value = 20),
          sliderInput("dropline",
                      "Longueur de la chute (cm) :",
                      min = 15,
                      max = 150,
                      value = 80.28),
          selectInput("w_tap",
                      label = "Diamètre du chalumeau (cm) :",
                      choices = list(`1/4`   =  1 /  4 * 2.54, 
                                     `5/16`  =  5 / 16 * 2.54, 
                                     `19/64` = 19 / 64 * 2.54, 
                                     `7/16`  =  7 / 16 * 2.54),
                      selected = "0.79375"),
          sliderInput("d_tap",
                      "Profondeur de l'entaille (cm) :",
                      min = 1.0,
                      max = 8,
                      value = 3.81,
                      step = 0.01),
          sliderInput("n_taps",
                      "Nombre d'entaille :",
                      min = 0,
                      max = 5,
                      value = 2),
          sliderInput("n_years",
                      "Nombre d'année d'entaillage :",
                      min = 0,
                      max = 200,
                      value = 0),
          sliderInput("m_rw",
                      "Croissance radiale moyenne (mm) :",
                      min = 0,
                      max = 8,
                      value = 1.2),
          sliderInput("sd_rw",
                      "Écart type de la croissance radiale (mm) :",
                      min = 0,
                      max = 1,
                      value = 0.1, 
                      step = 0.1)
        ),
        
        # Show a series of plots of the generated data
        mainPanel(plotOutput("calc_prob"))
    )
)
