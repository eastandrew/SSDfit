# SSDfit
Fitting a species-sensitivity distribution and estimating error.  

See code for examples but also run:  
`library("shiny")`  
`runGitHub("SSDfit","eastandrew")`  
To see dashboard with fake data run used in simulation to estimate HC5, LCL of HC5, and plot data, model, and CI+max/min.  

Files in repo are organized such that 'runGitHub()' calls 'app.R' which calls 'SSDapp_tabset.Rmd' which is the actual flexdashboard as an R markdown document containing shiny apps to create dynamic plots.  

Importantly, each of the output figures/tables are dynamically using sidebar slider selections and rerun simulation each time the sliders are moved.  In order to retain clarity each has 'set.seed()' to start which allows only one selection of random values.  
i.e. each re-run of the same sample size should return the sample random values in simulation dataset.  


## Work in Progress!!
