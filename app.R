
#rmarkdown::run("SSDapp_tabset.Rmd")

library(shiny)

file <- 'SSDapp_tabset.Rmd'
dir <- dirname(file)

ui <- rmarkdown:::rmarkdown_shiny_ui(dir, file)
render_args <- list()
render_args$envir <- parent.frame()
server <- rmarkdown:::rmarkdown_shiny_server(dir, file, T, render_args)

shinyApp(ui, server)
