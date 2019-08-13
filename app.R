
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(knitr)

rmdfiles <- c("SSDapp_tabset.rmd")
sapply(rmdfiles, knit, quiet = T)

ui <- shinyUI(
  fluidPage(
    includeMarkdown("SDapp_tabset.md")
  )
)
server <- function(input, output) { }

shinyApp(ui, server)
