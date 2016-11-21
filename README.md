# driftR
population genetic simulations with R and Shiny, inspired by popG (http://evolution.gs.washington.edu/popgen/popg.html)

To run on the web: 
https://cjbattey.shinyapps.io/driftR/

(note this is on free shinyapps.io servers and will time out at 25 active hours per month. Rehost or run locally for classroom settings)

To run locally: 

1. install R & rstudio, and open rStudio

2. install required packages by running the following line: 

  install.packages('shiny');install.packages('plyr');install.packages('reshape');install.packages('ggplot2')
  
3. load shiny:

  library(shiny)
  
4. download and run the app:

  runGitHub(username="cjbattey",repo="driftR")

