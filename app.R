
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
library(rmarkdown)

run("SSDapp_tabset.Rmd")
#rmdfiles <- c("SSDapp_tabset.Rmd")
#sapply(rmdfiles, run)

#ui <- shinyUI(
#  fluidPage(
#    includeMarkdown("SSDapp_tabset.md")
#  )
#)
#server <- function(input, output) { }
#
#shinyApp(ui, server)
