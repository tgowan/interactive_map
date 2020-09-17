# https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/

library(shiny)

#setwd("C:/...")

#runExample("01_hello")

# Create directory (App-1) that includes 'app.R' script. This script defines the 'ui' and 'server' and calls shinyApp() function
# Launch the app with runApp()
runApp("App-1-0")
#runApp("App-1-0", display.mode = "showcase")

#built in examples
runExample("01_hello")      # a histogram
runExample("02_text")       # tables and data frames
runExample("03_reactivity") # a reactive expression
runExample("04_mpg")        # global variables
runExample("05_sliders")    # slider bars
runExample("06_tabsets")    # tabbed panels
runExample("07_widgets")    # help text and submit buttons
runExample("08_html")       # Shiny app built from HTML
runExample("09_upload")     # file upload wizard
runExample("10_download")   # file download wizard
runExample("11_timer")      # an automated timer




#Example with Control Widgets
runApp("App-1")

#Another example
runApp("census-app", display.mode = "showcase")

#Manually
library(maps)
library(mapproj)

#read in some data
counties <- readRDS("census-app/data/counties.rds")
head(counties)

#use source code to create clorpleth map
source("census-app/helpers.R")
percent_map(counties$white, "darkgreen", "% White")

