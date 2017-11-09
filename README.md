# driftR
population genetic simulations with R and Shiny, inspired by popG (http://evolution.gs.washington.edu/popgen/popg.html)

To run on the web: 
https://cjbattey.shinyapps.io/driftR/

(This server will time out at 100 active hours per month. Rehost or run locally for large classroom settings)

cite driftR! <a href="https://zenodo.org/badge/latestdoi/74150401"><img src="https://zenodo.org/badge/74150401.svg" alt="DOI"></a>

To run locally: 

1. install R (https://www.r-project.org/) and rstudio (https://www.rstudio.com/products/rstudio/download/#download), and open RStudio

2. (In RStudio) copy/paste the following code block into the terminal (bottom left window): 

        pkgs <- c("plyr","reshape","ggplot2","magrittr","viridis")
        dl_pkgs <- subset(pkgs,!pkgs %in% rownames(installed.packages()))
        if(length(dl_pkgs)!=0){
          for(i in dl_pkgs) install.packages(i)
        }
        library(plyr);library(reshape);library(ggplot2);library(magrittr);library(viridis)
        library(shiny)
        runGitHub(username="cjbattey",repo="driftR")

