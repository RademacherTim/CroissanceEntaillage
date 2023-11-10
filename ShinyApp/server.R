#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("tidyverse")
library("shiny")

# Define server logic required to draw a histogram
function(input, output, session) {
    
  output$calc_prob <- renderPlot({
    
    # set seed for reproducibility ---------------------------------------------
    set.seed(1253)
      
    # initialise parameters ----------------------------------------------------
    n_yrs      <- 200.0 # number of years in simulation
    max_dbh    <- 150.0 # maximum dbh that can be achieved (cm)
    max_rw     <-   3.0 # maximum annual growth (mm)
    multiplier <-  75.0 # factor to estimate non-conductive wood due to tap hole
    # According to van den Berg this should be set to 75
      
    # create tibble with data ------------------------------------------------
    d <- tibble(yr = 0:n_yrs, 
                # simulate annual growth (cm)
                rw = pmax(rnorm(201, mean = input$m_rw, sd = input$sd_rw), 0),
                # calculate cumulative growth over time (cm)
                cum_rw = cumsum(rw),
                # estimate tree diameter over time (cm) 
                dbh = input$initial_dbh + 2 * cum_rw / 10, 
                # estimate tree circumference over time (cm) 
                cbh = dbh * pi, 
                # volume of wood in the tappable zone over time (cm3) 
                v_w = NA, 
                # volume of a tap hole (cm3)
                v_tap = input$d_tap * pi * (as.numeric(input$w_tap)/2)**2.0)
    
    # calculate the volume of wood in tapping zone (v_w; cm3) ----------------
    d <- d %>% mutate(v_w = case_when(
      input$dropline >= d$cbh / 2 ~ cbh * input$dropline * input$d_tap,
      input$dropline <  d$cbh / 2 ~ (input$dropline**2.0 * pi) / 2 * input$d_tap
    ))
    # TR - There should be a transition case between these two, which would 
    # result in a smoother transition
    
    # estimate volume of non-conductive wood added each year (cm3) -----------
    d <- d %>% mutate(d1_ncw = v_tap * input$n_taps * multiplier)
    
    # estimate the volume of non-conductive wood that is removed from the 
    # tappable zone each year due to radial growth (cm3) ---------------------
    d <- d %>% mutate(d2_ncw = d1_ncw * (rw / input$d_tap))
    
    # estimate the cumulative remaining non-conductive wood in the tappable zone
    d <- d %>% mutate(v_ncw = cumsum(d1_ncw) - cumsum(d2_ncw))
      
    # estimate the proportion of non-conductive wood in the tappable zone ------
    d <- d %>% mutate(p_ncw = v_ncw / v_w)
    
    par(mfrow = c(3, 1))
    # plot tree rw (mm) over time --------------------------------------------
    plot(x = d$yr, y = d$rw, col = "#91b9a4", axes = FALSE, typ = "l",
         xlab = "Temps (année)", ylab = "Diamètre à hauteur de poitrine (cm)",
         xlim = c(0, n_yrs), ylim = c(0, max_rw))
    axis(side = 1)
    axis(side = 2, las = 1)
    abline(v = input$n_years, col = "darkred", lwd = 2)
 
    # plot dbh (cm) evolution over time --------------------------------------
    plot(x = d$yr, y = d$dbh, col = "#91b9a4", axes = FALSE, typ = "l",
         xlab = "Temps (année)", ylab = "Croissance radiale (mm)",
         xlim = c(0, n_yrs), ylim = c(0, max_dbh))
    axis(side = 2, las = 1)
    axis(side = 1)
    abline(v = input$n_years, col = "darkred", lwd = 2)
    
    # plot the proportion of non-conductive wood over time ---------------------
    plot(x = d$yr, y = (1 - d$p_ncw) * 100, col = "#106470", axes = FALSE, 
         typ = "l",
         xlab = "Temps (année)", ylab = "Proportion de bois sain (%)",
         xlim = c(0, n_yrs), ylim = c(0, 100))
    axis(side = 2, las = 1)
    axis(side = 1)
    abline(v = input$n_years, col = "darkred", lwd = 2)
    
    print(d, n = 201)
  })
}
