#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Species Sensitivity Distribution with error and HC5 and LCL"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("mean",
                     "mean Log:",
                     min = 0.001,
                     max = 10,
                     value = 1),
         sliderInput("sd",
                     "sd Log:",
                     min = 0.001,
                     max = 10,
                     value = 0.1),
         sliderInput("samp",
                     "Sample Size:",
                     min = 5,
                     max = 50,
                     value = 10)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot", width=450, height=450)
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     n <- input$samp
     ave <- input$mean
     disp <- input$sd
     
     
     data <- data.frame(stringsAsFactors=FALSE,
                        NOEC=rlnorm(n,meanlog=ave,sdlog=disp),
                        idnum=seq(1:n)
     )
     
     
     
     data <- data[order(data$NOEC,decreasing=T), ]
 
     data$percorder <- 1+(1/n)-order(data$NOEC, decreasing=T)/max(data$idnum)
     fitobj <- MASS::fitdistr(data$NOEC,densfun="lognormal")
     meandist2 <- rnorm(100,mean=fitobj$estimate[1],sd=fitobj$sd[[1]])
     sddist2 <- rnorm(100,mean=fitobj$estimate[[2]],sd=fitobj$sd[[2]])
     probvec <- c(0.01,0.025,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.975,0.99)
     dfbigtest <- expand.grid(mean=meandist2,sd=sddist2,prob=probvec)
     dfbigtest$estimate <- qlnorm(dfbigtest$prob,meanlog=dfbigtest$mean,sdlog=dfbigtest$sd, lower.tail=T)
     library(tidyverse)
     dfbig95 <- dfbigtest %>%
       group_by(prob) %>%
       summarise(medest = quantile(estimate, probs=0.5, na.rm=T, names=F),
                 lowCI = quantile(estimate, probs=0.025, na.rm=T, names=F),
                 highCI = quantile(estimate, probs=0.975, na.rm=T, names=F), 
                 maxest = max(estimate),
                 minest = min(estimate))
     
     plot(prob~medest, data=dfbig95, type="l", lty=2, lwd=2, log="x", xlim=c(min(dfbig95$minest),max(dfbig95$maxest)), xlab="NOEC",ylab="Percentile")
     lines(prob~lowCI, data=dfbig95,lty=2, col="blue")
     lines(prob~highCI, data=dfbig95,lty=2, col="blue")
     lines(prob~maxest, data=dfbig95,lty=2, col="red")
     lines(prob~minest, data=dfbig95,lty=2, col="red")
     points(data$percorder~data$NOEC,pch=16,col="red")
     segments(0.001,0.05,qlnorm(0.05,meanlog=fitobj$estimate[[1]],sdlog=fitobj$estimate[[2]],lower.tail=T),0.05)
     segments(qlnorm(0.05,meanlog=fitobj$estimate[[1]],sdlog=fitobj$estimate[[2]],lower.tail=T),0,qlnorm(0.05,meanlog=fitobj$estimate[[1]],sdlog=fitobj$estimate[[2]],lower.tail=T),0.05)
     text(min(data$NOEC),0.5, bquote(.(round(dfbig95$lowCI[dfbig95$prob==0.05],3))), col="blue")
     text(min(data$NOEC),0.25, bquote(.(round(qlnorm(0.05,meanlog=fitobj$estimate[[1]],sdlog=fitobj$estimate[[2]],lower.tail=T),3))), col="black")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

