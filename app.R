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
    
    # App title ----
    titlePanel("SSDfit, Species Sensitivity Distribution and Effect of Sample Size"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            h2("Enter Data, Hit Run"),
            p("Choose mean, error, sample size."),
            #p("Model is fit to entered data and stop point checked."),
            #p("If stop point not met, model is used to pick treatments at important response levels."),
            #p("After each testing stage, add entire dataset and re-run."),
            #
            textInput("samp", label = h3("Sample Size"), value = "10"),
            textInput("mean", label=h3("Mean Log"), value="1"),
            textInput("sd", label=h3("SD Log"), value="0.1"),
            
            p("Be Patient, Simulation Takes Time."),
            actionButton("runplot",label=h3("SSD Plot")),
            br(),
            textInput("maxsamp", label=h3("Max Sample Size to Simulate"), value="50"),
            textInput("nreps", label=h3("Number of Simulations"), value="100"),
            p("Minimum sample size is set to 3"),
            p("Be Patient, Simulation Takes Time."),
            actionButton("runsim", label=h3("Simulate Sample Size Effect")),
            br(),
            br(),
            br(),
            p("Written by A. East"),
            p(a("GitHub",href="https://github.com/eastandrew/SWPB"))
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Species Sensitivity Distribution", plotOutput("SSDplot")),
                        tabPanel("Effect of Sample Size", plotOutput("sampsizeplot"))
                        
            )
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    # Reactive expression to generate the requested distribution ----
    # This is called whenever the inputs change. The output functions
    # defined below then use the value computed from this expression
    d <- eventReactive(input$runplot, {
        
        n <- as.numeric(unlist(strsplit(input$samp,",")))
        ave <- as.numeric(unlist(strsplit(input$mean,",")))
        disp <- as.numeric(unlist(strsplit(input$sd,",")))
        
        data <- data.frame(stringsAsFactors=F,
            NOEC=rlnorm(n=n,meanlog=ave,sdlog=disp),
            idnum=seq(1:n)
            )
        data <- data[order(data$NOEC,decreasing=T), ]
        data$percorder <- 1+(1/n)-order(data$NOEC, decreasing=T)/max(data$idnum)
        fitobj <- fitdistr(data$NOEC,densfun="lognormal")
        meandist2 <- rnorm(100,mean=fitobj$estimate[[1]],sd=fitobj$sd[[1]])
        sddist2 <- rnorm(100,mean=fitobj$estimate[[2]],sd=fitobj$sd[[2]])
        probvec <- c(0.01,0.025,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.975,0.99)
        dfbigtest <- expand.grid(mean=meandist2,sd=sddist2,prob=probvec)
        dfbigtest$estimate <- qlnorm(dfbigtest$prob,meanlog=dfbigtest$mean,sdlog=dfbigtest$sd, lower.tail=T)
        dfbig95 <- dfbigtest %>%
            group_by(prob) %>%
            summarise(medest = quantile(estimate, probs=0.5, na.rm=T, names=F),
                      lowCI = quantile(estimate, probs=0.025, na.rm=T, names=F),
                      highCI = quantile(estimate, probs=0.975, na.rm=T, names=F), 
                      maxest = max(estimate),
                      minest = min(estimate))
        plot(prob~medest, data=dfbig95, type="l", lty=2, lwd=2, log="x", xlim=c(min(dfbig95$minest),max(dfbig95$maxest)), xlab="NOEC, log scaled",ylab="Percentile")
        lines(prob~lowCI, data=dfbig95,lty=2, col="blue")
        lines(prob~highCI, data=dfbig95,lty=2, col="blue")
        lines(prob~maxest, data=dfbig95,lty=2, col="red")
        lines(prob~minest, data=dfbig95,lty=2, col="red")
        points(data$percorder~data$NOEC,pch=16,col="red")
        segments(0.001,0.05,qlnorm(0.05,meanlog=fitobj$estimate[[1]],sdlog=fitobj$estimate[[2]],lower.tail=T),0.05)
        segments(qlnorm(0.05,meanlog=fitobj$estimate[[1]],sdlog=fitobj$estimate[[2]],lower.tail=T),0,qlnorm(0.05,meanlog=fitobj$estimate[[1]],sdlog=fitobj$estimate[[2]],lower.tail=T),0.05)
        
        
    })
    
    e <- eventReactive(input$runsim, {
        
       
        simulate <- function(x) {
            n <- x
            ave <- as.numeric(unlist(strsplit(input$mean,",")))
            disp <- as.numeric(unlist(strsplit(input$sd,",")))
            
            fitobj <- MASS::fitdistr(rlnorm(n,meanlog=ave,sdlog=disp),densfun="lognormal")
            
            dfbigtest <- expand.grid(mean=rnorm(10,mean=fitobj$estimate[[1]],sd=fitobj$sd[[1]]),sd=rnorm(10,mean=fitobj$estimate[[2]],sd=fitobj$sd[[2]]))
            
            return(median(qlnorm(0.05,meanlog=dfbigtest$mean,sdlog=dfbigtest$sd, lower.tail=T)))
        }
        
        
        sampmax <- as.numeric(unlist(strsplit(input$maxsamp,",")))
        nreps <- as.numeric(unlist(strsplit(input$nreps,",")))
        minsamp <- 3
        
        m <- matrix(data=1:sampmax, nrow=sampmax, ncol=nreps)
        m <- m[-(1:(minsamp-1)),]
        m <- matrix(sapply(m,simulate),nrow=sampmax-(minsamp-1),ncol=nreps)
        
        rownames(m) <- minsamp:sampmax
        m <- data.frame(m)
        m$sampsize <- as.numeric(rownames(m))
        m <- gather(m, key=runnumber, value=value, -sampsize)
        m2 <- m %>%
            group_by(sampsize) %>%
            summarise(medianv = median(value, na.rm=T),
                      meanv = mean(value, na.rm=T),
                      highCI = quantile(value, probs=0.975, na.rm=T, names=F),
                      lowCI = quantile(value, probs=0.025, na.rm=T, names=F)) %>%
            arrange(sampsize)
        
        plot(medianv~sampsize, data=m2, type="l", ylim=c(min(m2$lowCI, na.rm=T),max(m2$highCI)), log="", ylab="Estimated log(HC5)", xlab="Sample Size", col="blue")
        points(meanv~sampsize, data=m2, type="l", col="red")
        points(highCI~sampsize, data=m2, type="l", lty=2)
        points(lowCI~sampsize, data=m2, type="l", lty=2)
        abline(h=m2$medianv[m2$sampsize==sampmax])
    })
    #
    #f <- eventReactive(input$run, {
    #    
    #    up <- ED(d(), 0.5, type="absolute", interval="delta", display=F)[,4]
    #    down <- ED(d(), 0.5, type="absolute", interval="delta", display=F)[,3]
    #    ld50 <- ED(d(), 0.5, type="absolute", interval="delta", display=F)[,1]
    #    
    #    
    #    df <- data.frame(Intra_CI_Range=up-down, Twice_LD50=ld50*2)
    #    df$Stop_Ratio_Value <- df$Intra_CI_Range/df$Twice_LD50
    #    df$'<=0.4?' <- ifelse(df$Stop_Ratio_Value<=0.4, "Yes, Stop","No, Repeat")
    #    df
    #    
    #})
    #
    #library(drc)
    # Generate a plot of the data ----
    # Also uses the inputs to build the plot label. Note that the
    # dependencies on the inputs and the data reactive expression are
    # both tracked, and all expressions are called in the sequence
    # implied by the dependency graph.
    
    # Generate an HTML table view of the data ----
    
    library(tidyverse)
    library(MASS)
    
    output$SSDplot <- renderPlot({
        d()
        
    }, width=600, height=500, units="px")
    
    # Generate a summary of the data ----
    output$sampsizeplot <- renderPlot({
        e()
    }, width=600, height=500, units="px")
    #
    ## Generate an HTML table view of the data ----
    #output$lc50 <- renderTable({
    #    #ED(d(), c(0.01,0.05,0.5,0.95,0.99), type="absolute", interval="delta")
    #    
    #    resplist <- c(0.01,0.1,0.25,0.5,0.75,0.9,0.99)
    #    EDlist <- c(ED(d(),resplist, type="absolute", display=F)[,1])
    #    EDlistlower <- c(ED(d(),resplist, type="absolute", interval="delta", display=F)[,3])
    #    EDlistupper <- c(ED(d(),resplist, type="absolute", interval="delta", display=F)[,4])
    #    dfpred <- data.frame(LD_value=resplist, Estimated_Dose=EDlist, ED_lower=EDlistlower, ED_upper=EDlistupper)
    #})
    #
    #output$stoptest <- renderTable({
    #    f()
    #})
    #
    #output$predictions <- renderTable({
    #    
    #    EDlist <- c(ED(d(),c(0.1,0.9), type="absolute", display=F)[,1])
    #    vect <- c()
    #    numbertreat <- as.numeric(unlist(strsplit(input$numtreats,",")))
    #    vect[1] <- min(EDlist)
    #    multiplier <- (exp((log(max(EDlist))-log(min(EDlist)))/(numbertreat-1)))
    #    for (i in 2:(numbertreat)){
    #        vect[i] <- vect[i-1]*multiplier
    #    }
    #    
    #    
    #    dfpred <- data.frame(New_Planning_Doses=vect, Estimated_Mortality=predict(d(),newdata=data.frame(vect)))
    #})
    
}

# Create Shiny app ----
shinyApp(ui, server)

