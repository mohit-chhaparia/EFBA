remotes::install_github("mohit-chhaparia/EFBA" , dependencies = TRUE , force = TRUE)
library(EFBA)
shiny::runApp(EFBA::launchApp() , launch.browser = FALSE)
