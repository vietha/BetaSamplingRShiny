Depoloy this R-Shiny app to shinyapps.io
# Install rsconnect package
> install.packages("rsconnect")
# Load it
> library(rsconnect)
# Setup token (from shinyapps.io)
rsconnect::setAccountInfo(name = "yourname",
                          token = "your_token",
                          secret = "your_secret")
# Deploy app: set the working directory to the folder that contains ui.R and server.R, then run:
> rsconnect::deployApp("PATH_TO_APP_FOLDER")
