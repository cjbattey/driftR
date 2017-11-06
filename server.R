

shinyServer(function(input,output,session){
  
  source("./dev.R")
  
  #if >1 allele freq or pop size are specificed, divide values evenly across the total input nPop
  p <- reactive({
    if(grepl(", ",input$p,fixed=T)){
      lapply(as.numeric(unlist(strsplit(input$p,", "))),
             function(e) rep(e,input$nPop/length(as.numeric(unlist(strsplit(input$p,", ")))))) %>% unlist()
    } else if(grepl(",| ",input$p)){
      lapply(as.numeric(unlist(strsplit(input$p,",| "))),
             function(e) rep(e,input$nPop/length(as.numeric(unlist(strsplit(input$p,",| ")))))) %>% unlist()
    } else {
      as.numeric(input$p)
    }
    })
  n <- reactive({
    if(grepl(", ",input$n,fixed=T)){
      lapply(as.numeric(unlist(strsplit(input$n,", "))),
             function(e) rep(e,input$nPop/length(as.numeric(unlist(strsplit(input$n,", ")))))) %>% unlist()
    } else if(grepl(",| ",input$n)){
      lapply(as.numeric(unlist(strsplit(input$n,",| "))),
             function(e) rep(e,input$nPop/length(as.numeric(unlist(strsplit(input$n,",| ")))))) %>% unlist()
    } else {
      as.numeric(input$n)
    }
  })
  nPop <- reactive({
    if(input$nPop%%length(p())!=0){
      input$nPop - input$nPop%%length(p())
    } else {
      input$nPop
    }
  })
  
  #run single sim
  sim.data <- eventReactive(input$go,ignoreNULL = F,{
    validate(
      #need((input$nPop==length(p())|length(p())==1),"number of populations must equal number of starting allele frequencies."),
      need(input$gen<1001,"Please select < 1001 generations."),
      need(input$nPop<101,"Please select < 101 populations"),
      #need(input$n<1000001,"Please select n < 1,000,000"),
      need(input$plotStats!="","Select a variable to plot.")
      )
    tmp <- runPopSim(gen=input$gen,p=p(),Waa=input$Waa,Wab=input$Wab,Wbb=input$Wbb,n=n(),
              nPop=nPop(),m=input$m,stats=input$plotStats,infinitePop=input$infinitePop,Uab=input$Uab,Uba=input$Uab,
              continue=F)
    tmp
  })
  
  #continue sim
  # sim.data <- eventReactive(input$continue,ignoreNULL = F,{
  #   runPopSim(gen=input$gen,p=input$p,Waa=input$Waa,Wab=input$Wab,Wbb=input$Wbb,n=input$n,
  #             nPop=input$nPop,m=input$m,stats=input$plotStats,infinitePop=input$infinitePop,Uab=input$Uab,Uba=input$Uab,
  #             continue=T)
  # })
  
  plot.data <- eventReactive(sim.data(),{
    meltPlotData(allele.freq.df = sim.data(),gen=input$gen,nPop=nPop(),stats=input$plotStats)
    })
  
  output$plot <- renderPlot({
    plotSingleRun(plot.data(),nPop=nPop(),gen=input$gen,legend=input$legend)
  })
  
  nLost.text <- eventReactive(sim.data(),{
    p <- sim.data()[input$gen+1,1:nPop()]
    nFixed <- length(p[p==1])
    nLost <- length(p[p==0])
    paste0("Fixed: ",nFixed,".  Lost: ",nLost,".")
  })
  
  output$nLost <- renderText({
    nLost.text()
  })
  
  endStateTable <- eventReactive(sim.data(),{
    #pNames <- c()
    #for(i in 1:input$nPop){pNames[i] <- paste0("p",i)}
    sim.data()[(input$gen+1),c("Fis","Hs","Ht","Fst")]
  })
  
  output$endStateTable <- renderTable({
    endStateTable()
  })
  
  sumTable <- eventReactive(input$run_replicates,{
    validate(
      need(input$n<=100000,"Please select n <= 100,000")
    )
    sumTable <- data.frame(matrix(ncol=16))
    withProgress(message="simulating populations...",value=0,{
      for(i in 1:100){
        df <- runPopSim2(gen=input$gen,p=p(),Waa=input$Waa,Wab=input$Wab,Wbb=input$Wbb,n=n(),
                        nPop=2,m=input$m,stats=input$plotStats,infinitePop=input$infinitePop,Uab=input$Uab,Uba=input$Uab,
                        continue=F)
        #df <- runPopSim2() #for debug
        names(sumTable) <- names(df)
        sumTable[i,] <- df[nrow(df),]
        incProgress(1/100)
      }
    })
    sumTable
  })

  meanTable <- reactive({
    tbl <- colMeans(sumTable(),na.rm=T)
    tbl <- tbl[c("Fis","Hs","Ht","Fst")]
    t(tbl)
  })

  varTable <- reactive({
    tbl <- apply(sumTable(),2,function(e) var(e,na.rm=T))
    tbl <- tbl[c("Fis","Hs","Ht","Fst")]
    t(tbl)
  })

  output$meanTable <- renderTable(meanTable(),colnames = T,digits=4,caption = "Mean state at final generation:",
                              caption.placement = getOption("xtable.caption.placement", "top"),
                              caption.width = getOption("xtable.caption.width", NULL))

  output$varTable <- renderTable(varTable(),colnames = T,digits=4,caption = "Variance:",
                                 caption.placement = getOption("xtable.caption.placement", "top"),
                                 caption.width = getOption("xtable.caption.width", NULL))

  output$sumTable <- renderTable(sumTable(),caption = "Final Generation States:",
                                 caption.placement = getOption("xtable.caption.placement", "top"),
                                 caption.width = getOption("xtable.caption.width", NULL))
})
