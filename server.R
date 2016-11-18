

shinyServer(function(input,output,session){
  
  source("./dev.R")
  
  sim.data <- eventReactive(input$go,ignoreNULL = F,{
    validate(
      need(input$gen<=5000,"Please select < 5000 generations."),
      need(input$nPop<=100,"Please select < 100 populations"),
      need(input$n<1000000,"Please select n < 1,000,000"),
      need(input$plotStats!="","Select a variable to plot.")
      )
    runPopSim(gen=input$gen,p=input$p,Waa=input$Waa,Wab=input$Wab,Wbb=input$Wbb,n=input$n,
              nPop=input$nPop,m=input$m,stats=input$plotStats,drift=input$drift,Uab=input$Uab,Uba=input$Uab)
  })
  
  plot.data <- eventReactive(sim.data(),{
    meltPlotData(allele.freq.df = sim.data(),gen=input$gen,nPop=input$nPop,stats=input$plotStats)
    })
  
  output$plot <- renderPlot({
    plotSingleRun(plot.data(),nPop=input$nPop,gen=input$gen)
  })
  
  nLost.text <- eventReactive(sim.data(),{
    p <- sim.data()[input$gen+1,1:input$nPop]
    nFixed <- length(p[p==1])
    nLost <- length(p[p==0])
    paste0("Fixed: ",nFixed,".  Lost: ",nLost,".")
  })
  
  output$nLost <- renderText({
    nLost.text()
  })
  
  sumTable <- eventReactive(input$runSim,{
    validate(
      need(input$n<=100000,"Please select n <= 100,000")
    )
    sumTable <- data.frame(matrix(ncol=14))
    withProgress(message="simulating populations...",value=0,{
      for(i in 1:100){
        df <- runPopSim(gen=100,p=input$p,Waa=input$Waa,Wab=input$Wab,Wbb=input$Wbb,n=input$n,
                               nPop=2,m=input$m,drift=input$drift,Uab=input$Uab,Uba=input$Uab)
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
