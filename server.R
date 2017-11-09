

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
  
  #adjust nPop to be evenly divisible by the number of starting allele frequencies or population sizes.
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
  
  ######################## replicate runs for 2-population simulations ###########################
  n2 <- eventReactive(input$n,{
    if(grepl(", ",input$n,fixed=T)){
      as.numeric(unlist(strsplit(input$n,", ")))
    } else if(grepl(",| ",input$n)){
      as.numeric(unlist(strsplit(input$n,",| ")))
    } else {
      as.numeric(input$n)
    }
  })
  p2 <- reactive({
    if(grepl(", ",input$p,fixed=T)){
      as.numeric(unlist(strsplit(input$p,", ")))
    } else if(grepl(",| ",input$p)){
      as.numeric(unlist(strsplit(input$p,",| ")))
    } else {
      as.numeric(input$p)
    }
  })
  
  rep_sims <- eventReactive(input$run_replicates,{
    validate(
      need(n2()<=10000,"Please select n <= 10,000"),
      need(input$gen<=200,"Please select gen <=100")
    )
    rep_sims <- data.frame(matrix(ncol=17))
    withProgress(message="simulating populations...",value=0,{
      for(i in 1:input$nreps){
        df <- runPopSim2(gen=input$gen,p=p2(),Waa=input$Waa,Wab=input$Wab,Wbb=input$Wbb,n=n2(),
                       nPop=2,m=input$m,stats=input$plotStats,infinitePop=input$infinitePop,Uab=input$Uab,Uba=input$Uab,
                       continue=F)
        #df <- runPopSim2() #for debug
        df$rep <- i
        names(rep_sims) <- names(df)
        rep_sims <- rbind(rep_sims,df)
        incProgress(1/input$nreps)
      }
    })
    rep_sims <- subset(rep_sims[-1,],gen!=0)
    rep_sims
  })
  
  rep_plot_data <- eventReactive(rep_sims(),{
    melt_rep_sims <- melt(rep_sims(),id.vars = c("rep","gen")) 
    melt_rep_sims$stat <- gsub("[[:digit:]]","",melt_rep_sims$variable)
    melt_rep_sims <- subset(melt_rep_sims,stat %in% input$plotStats)
    melt_rep_sims$grp <- factor(paste(melt_rep_sims$rep, melt_rep_sims$variable))
    melt_rep_sims$rep <- factor(melt_rep_sims$rep)
    melt_rep_sims
  })
  
  rep_plot <- eventReactive(rep_plot_data(),{
    print(
      ggplot(data=rep_plot_data(),
           aes(x=gen,y=value,group=grp,col=rep))+
      theme_bw()+
      theme(panel.grid.minor=element_blank(),
            axis.text=element_text(size=12),
            axis.title=element_text(size=12),
            strip.background = element_blank(),
            strip.text=element_text(size=12),
            legend.position = "none")+
      scale_color_viridis(discrete=T)+
      facet_wrap(~stat,scales="free",ncol=2)+
      geom_path()
    )
  })
  
  output$rep_plot <- renderPlot(rep_plot())

  sumTable <- eventReactive(rep_sims(),{
    subset(rep_sims(),gen==input$gen)
  })
  
  meanTable <- reactive({
    tbl <- colMeans(sumTable(),na.rm=T)
    tbl <- tbl[c("mean.p","Fis","Hs","Ht","Fst")]
    names(tbl) <- c("p","Fis","Hs","Ht","Fst")
    t(tbl)
  })

  varTable <- reactive({
    tbl <- apply(sumTable(),2,function(e) var(e,na.rm=T))
    tbl <- tbl[c("mean.p","Fis","Hs","Ht","Fst")]
    names(tbl) <- c("p","Fis","Hs","Ht","Fst")
    t(tbl)
  })
  
  plot.data2 <- eventReactive(sim.data(),{
    meltPlotData(allele.freq.df = sim.data(),gen=input$gen,nPop=nPop(),stats=input$plotStats)
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
