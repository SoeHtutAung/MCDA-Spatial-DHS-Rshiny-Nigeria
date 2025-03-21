#########################
# Application function
# 
##########################

# load necessary r files
source ("data/process.R") # to load necessary packages and datasets
source ("ui.R")
source ("server.R")
# run app
shinyApp(ui = ui, server = server)
