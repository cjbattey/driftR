# driftR
population genetic simulations with R and Shiny, inspired by popG (http://evolution.gs.washington.edu/popgen/popg.html)

To run on the web: 
https://cjbattey.shinyapps.io/driftR/

(note this is on free shinyapps.io servers and will time out at 100 active hours per month. Rehost or run locally for classroom settings)

cite driftR! <a href="https://zenodo.org/badge/latestdoi/74150401"><img src="https://zenodo.org/badge/74150401.svg" alt="DOI"></a>

To run locally: 

1. install R (https://www.r-project.org/) and rstudio (https://www.rstudio.com/products/rstudio/download/#download), and open RStudio

2. (In RStudio) copy/paste the following code block into the terminal (bottom left window): 

        install.packages('shiny')
        install.packages('plyr')
        install.packages('reshape')
        install.packages('ggplot2')
        install.packages('magrittr')
        library(shiny)
        runGitHub(username="cjbattey",repo="driftR")

