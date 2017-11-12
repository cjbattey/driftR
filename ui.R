shinyUI(fluidPage(
  titlePanel("driftR: Population Genetic Simulations in R"),
  sidebarLayout(
    sidebarPanel(
      actionButton("go","Run Simulation",width="100%"),
      br(),br(),
      textInput("p","Starting allele frequency A",value="0.1, 0.9"),
      sliderInput("Uab","Mutation Rate",value=0,min=0,max=0.1),
      sliderInput("Waa","Fitness of genotype AA",value=1,min=0,max=1),
      sliderInput("Wab","Fitness of genotype AB",value=0.95,min=0,max=1),
      sliderInput("Wbb","Fitness of genotype BB",value=0.90,min=0,max=1),
      sliderInput("m","Migration Rate",0.01,min=0,max=0.2),
      numericInput("nPop","Number of Populations",10,min=1,max=100),
      textInput("n","Population Size",value=100),
      numericInput("gen","Number of Generations",100,min=1,max=5000),
      checkboxInput("infinitePop","Infinite Population (no drift)",value = F),
      checkboxGroupInput(inputId="plotStats",label="plot:",choices=c("p","He","Hs","Ht","Fst","W"),inline=T,selected="p"),
      checkboxInput("legend","Legend",value = F),
      # helpText("key:\n
      #          p: allele frequency of A                          
      #          He: expected heterozygosity per population        
      #          Hs: mean heterozygosity within populations      
      #          Ht: mean heterozygosity across populations         
      #          Fst: fixation index                                
      #          W: population mean fitness"),
      div(helpText("driftR simulates allele and genotype frequencies for a single biallelic locus in biological populations
                    under a Wright-Fisher island model with symmetrical migration between all populations (weighted by population size). 
                    Core functions were adapted from the Java version of popG (http://evolution.gs.washington.edu/popgen/popg.html). 
                    Code available on github: https://github.com/cjbattey/driftR"),style="font-size:75%")
      ),
    
    mainPanel(#style="position:fixed;margin-left:32vw;",
      plotOutput("plot"),
      textOutput("nLost"),
      div(style="height:5vh;"),
      div("Final Generation State:"),
      tableOutput("endStateTable"),
      # actionButton("continue","Continue Simulation"),
      # div(style="border-top:1px solid black;"),
      br(),
      h3("Replicate Runs",style="border-top:1px solid black;"),
      helpText("Click the button below to run repeated simulations of 2 populations using the current 
               parameters (useful for estimating variance)."),
      actionButton("run_replicates","Run Replicate Simulations"),
      numericInput("nreps","number of replicates",value=10),
      tableOutput("meanTable"),
      tableOutput("varTable"),
      plotOutput("rep_plot"),
      div(tableOutput("sumTable"), style = "font-size: 75%; width: 75%;")
    )
    )
))
