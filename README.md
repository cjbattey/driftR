# driftR
population genetic simulations with R and Shiny, inspired by popG (http://evolution.gs.washington.edu/popgen/popg.html)

To run on the web: 
https://cjbattey.shinyapps.io/driftR/

(This server will time out at 100 active hours per month. Rehost or run locally for large classroom settings)

To run locally: 

1. Install R (https://www.r-project.org/). Optionally, install rstudio (https://www.rstudio.com/products/rstudio/download/).

2. Paste the following code block into an R console: 

        pkgs <- c("plyr","reshape","ggplot2","magrittr","viridis")
        dl_pkgs <- subset(pkgs,!pkgs %in% rownames(installed.packages()))
        if(length(dl_pkgs)!=0){
          for(i in dl_pkgs) install.packages(i)
        }
        library(shiny)
        runGitHub(username="cjbattey",repo="driftR")


driftR is a population-genetic simulation web application inspired by the Java program popG (http://evolution.gs.washington.edu/popgen/popg.html). It is intended primarily for use in undergraduate courses in genetics and evolution, and can be accessed online at https://cjbattey.shinyapps.io/driftR/. 

The software simulates change in allele frequency across multiple populations under the island model described in Wright (1931). Each diploid genotype is associated with a user-defined fitness, and relationships between genotype and fitness are uniform across populations. Population size is constant through time, but can vary across populations. Populations exchange migrants randomly at each generation at a rate set by the user. 

For each generation and each population, the simulation (1) adjusts allele frequencies for mutation and migration, (2) calculates genotype frequencies after selection, and (3) simulates genetic drift by drawing the genotype counts for the next generation from a binomial distribution with a "success" cutoff equal to the post-selection expected frequency. DriftR stores allele frequencies and summary statistics (He, Ho, Fst, Fis) each generation, then plots the result using the faceting features of the R package "ggplot2". 

driftR was written by CJ Battey and is free for educational and other use under the Gnu General Public License (https://www.gnu.org/licenses/quick-guide-gplv3.html). 

*Wright, Sewall. "Evolution in Mendelian populations." Genetics 16, no. 2 (1931): 97-159.

citation: <a href="https://zenodo.org/badge/latestdoi/74150401"><img src="https://zenodo.org/badge/74150401.svg" alt="DOI"></a>
