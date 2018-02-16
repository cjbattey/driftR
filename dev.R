#development script for popgen simulations (+/- R translation of PopG, adding more visualization and summary stats)
library(plyr);library(reshape);library(ggplot2);library(magrittr);library(viridis)

 # gen=100;p=0.5;Waa=1;Wab=1;Wbb=1;n=100;nPop=2;m=0;stats=c("p","Fst");Uab=0;Uba=0;
 # infinitePop=F;continue=F

#main simulation function
runPopSim <- function(gen=100,p=0.5,Waa=1,Wab=1,Wbb=1,n=100,nPop=2,m=0,stats=c("p","Fst"),Uab=0,Uba=0,
                      infinitePop=F,continue=F){
  if(continue==T){ #continue function currently broken... will update when possible. 
    #allele.freq <- endState
  } else {
    allele.freq <- data.frame(matrix(ncol=4*nPop)) #initialize summary stat matrix
    if(length(p)>1){
      allele.freq[1,(1:nPop)] <- p
    } else {
      allele.freq[1,(1:nPop)] <- rep(p,nPop) #starting allele freqs
    }
  }
  
  withProgress(message="simulating populations...",value=0,{
  for(i in 1:gen){ 
    if(length(n)>1){ #weight mean allele freq (used for migration) by relative population size if different
      ps <- allele.freq[i,(1:nPop)] %>% unlist()
      mean.p <- weighted.mean(ps,n)
    } else {
      mean.p <- as.numeric(rowMeans(allele.freq[i,(1:nPop)]))
    }
    for(j in 1:nPop){
      p <- allele.freq[i,j]
      if(length(n)>1){ #get population-specific pop size if multiple listed
        n2 <- n[j]
      } else {
        n2 <- n
      }
      p <- (1 - Uab) * p + Uba * (1 - p) #mutation
      p <- p*(1-m)+m*mean.p # migration
      q <- 1-p
      if(p>0 && p<1){ #if alleles are not fixed
        w <- p*p*Waa+2*p*q*Wab+q*q*Wbb #population average fitness
        freq.aa <- (p*p*Waa)/w #post-selection genotype frequencies (weighted by relative fitness)
        freq.ab <- (2*p*q*Wab)/w
        if(infinitePop==F){ 
            Naa <- rbinom(1,n2,freq.aa)
          if(freq.aa<1){ 
            Nab <- rbinom(1,(n2-Naa),(freq.ab/(1-freq.aa)))
          }
          else {
            Nab <- 0
          }
          p <- ((2*Naa)+Nab)/(2*n2)
          q <- 1-p
          allele.freq[(i+1),j] <- p #new p after drift in columns 1:nPop
          allele.freq[(i+1),(j+nPop)] <- Nab/n2 #Ho in columns (nPop+1):(nPop*2)
          allele.freq[(i+1),(j+2*nPop)] <- 2*p*q #He in columns (nPop*2+1):nPop*3
          allele.freq[(i+1),(j+3*nPop)] <- w #pop mean fitness in last columns
        } 
        else { #no drift (infinite population) conditions
          p <- freq.aa+(freq.ab/2)
          q <- 1-p
          allele.freq[(i+1),j] <- p
          allele.freq[(i+1),(j+nPop)] <- freq.ab 
          allele.freq[(i+1),(j+2*nPop)] <- 2*p*q
          allele.freq[(i+1),(j+3*nPop)] <- w
        }
      } else { #if alleles are fixed
        if(p<=0){
          p <- 0
          w <- p*p*Waa+2*p*q*Wab+q*q*Wbb
        } else {
          p <- 1
          w <- p*p*Waa+2*p*q*Wab+q*q*Wbb
        }
        allele.freq[(i+1),j] <- p
        allele.freq[(i+1),(j+nPop)] <- 0
        allele.freq[(i+1),(j+2*nPop)] <- 0
        allele.freq[(i+1),(j+3*nPop)] <- w
      }
    } #end populations loop
    incProgress(1/gen)
  } #end generations loop
  }) #end progress bar
  #summary stats
  names <- c()
  for(i in 1:nPop){names[i]<-paste0("p",i)}
  for(i in (nPop+1):(2*nPop)){names[i]<-paste0("Ho",i-nPop)}
  for(i in (nPop*2+1):(3*nPop)){names[i]<-paste0("He",i-2*nPop)}
  for(i in (nPop*3+1):(4*nPop)){names[i]<-paste0("W",i-3*nPop)}
  colnames(allele.freq) <- names
  allele.freq$meanHo <- rowMeans(allele.freq[(nPop+1):(nPop*2)])
  allele.freq$meanHe <- rowMeans(allele.freq[(nPop*2+1):(nPop*3)])
  allele.freq$Fis <- 1-(allele.freq$meanHo/allele.freq$meanHe)
  allele.freq$mean.p <- rowMeans(allele.freq[1:nPop]) 
  allele.freq$Hs <- rowMeans(allele.freq[(nPop*2+1):(nPop*3)])
  allele.freq$Ht <- 2*allele.freq$mean.p*(1 - allele.freq$mean.p)
  allele.freq$Fst <- (allele.freq$Ht-allele.freq$Hs)/allele.freq$Ht
  allele.freq$Fst[allele.freq$Fst<0] <- 0
  allele.freq$gen <- 0:gen
  allele.freq$Fst[allele.freq$gen == 0] <- NA
  return(allele.freq)
}

#format for plotting
meltPlotData <- function(allele.freq.df=allele.freq.df,gen=100,nPop=2,stats=c("p","Fst")){
  df <- melt(allele.freq.df,id.vars = "gen")
  df$dataType <- c(rep("p",(nPop*(gen+1))),rep("Ho",nPop*(gen+1)),rep("He",nPop*(gen+1)),rep("W",nPop*(gen+1)),rep("meanHo",(gen+1)),rep("meanHe",(gen+1)),
                   rep("Fis",(gen+1)),rep("mean.p",(gen+1)),rep("Hs",(gen+1)),rep("Ht",(gen+1)),rep("Fst",(gen+1)))
  df <- subset(df,dataType %in% stats)
  return(df)
}

#plotting function
plotSingleRun <- function(df,nPop,gen,legend,scales){
  if(legend==T & scales=="fixed"){
    print(ggplot(df,aes(x=gen,y=value,col=variable))+facet_wrap(~dataType,scales=scales,ncol=2)+
            theme_bw()+ylim(0,1)+
            theme(panel.grid.minor=element_blank(),
                  axis.text=element_text(size=12),
                  axis.title=element_text(size=12),
                  strip.background = element_blank(),
                  strip.text=element_text(size=12))+
            scale_color_viridis(discrete=T)+
            xlab("Generations")+ylab("")+
            geom_line())
  } else if (legend==T & scales=="free_y"){
    print(ggplot(df,aes(x=gen,y=value,col=variable))+facet_wrap(~dataType,scales=scales,ncol=2)+
            theme_bw()+
            theme(panel.grid.minor=element_blank(),
                  axis.text=element_text(size=12),
                  axis.title=element_text(size=12),
                  strip.background = element_blank(),
                  strip.text=element_text(size=12))+
            scale_color_viridis(discrete=T)+
            xlab("Generations")+ylab("")+
            geom_line())
  } else if (legend==F & scales=="fixed"){
    print(ggplot(df,aes(x=gen,y=value,col=variable))+facet_wrap(~dataType,scales=scales,ncol=2)+
            theme_bw()+ylim(0,1)+
            theme(legend.position="none",
                  panel.grid.minor=element_blank(),
                  axis.text=element_text(size=12),
                  axis.title=element_text(size=12),
                  strip.background = element_blank(),
                  strip.text=element_text(size=12))+
            scale_color_viridis(discrete=T)+
            xlab("Generations")+ylab("")+
            geom_line())
  } else if (legend==F & scales=="free_y") {
    print(ggplot(df,aes(x=gen,y=value,col=variable))+facet_wrap(~dataType,scales=scales,ncol=2)+
            theme_bw()+
            theme(legend.position="none",
                  panel.grid.minor=element_blank(),
                  axis.text=element_text(size=12),
                  axis.title=element_text(size=12),
                  strip.background = element_blank(),
                  strip.text=element_text(size=12))+
            scale_color_viridis(discrete=T)+
            xlab("Generations")+ylab("")+
            geom_line())  }
  
}

#simulation function w/o progress bar for debug+replicate runs
runPopSim2 <- function(gen=100,p=0.5,Waa=1,Wab=1,Wbb=1,n=100,nPop=2,m=0,stats=c("p","Fst"),Uab=0,Uba=0,
                      infinitePop=F,continue=F){
  if(continue==T){ #continue function currently broken... will update when possible. 
    #allele.freq <- endState
  } else {
    allele.freq <- data.frame(matrix(ncol=4*nPop)) #initialize summary stat matrix
    if(length(p)>1){
      allele.freq[1,(1:nPop)] <- p
    } else {
      allele.freq[1,(1:nPop)] <- rep(p,nPop) #starting allele freqs
    }
  }
    for(i in 1:gen){ 
      if(length(n)>1){ #weight mean allele freq (used for migration) by relative population size if different
        ps <- allele.freq[i,(1:nPop)] %>% unlist()
        mean.p <- weighted.mean(ps,n)
      } else {
        mean.p <- as.numeric(rowMeans(allele.freq[i,(1:nPop)]))
      }
      for(j in 1:nPop){
        p <- allele.freq[i,j]
        if(length(n)>1){ #get population-specific pop size if multiple listed
          n2 <- n[j]
        } else {
          n2 <- n
        }
        p <- (1 - Uab) * p + Uba * (1 - p) #mutation
        p <- p*(1-m)+m*mean.p # migration
        q <- 1-p
        if(p>0 && p<1){ #if alleles are not fixed
          w <- p*p*Waa+2*p*q*Wab+q*q*Wbb #population average fitness
          freq.aa <- (p*p*Waa)/w #post-selection genotype frequencies (weighted by relative fitness)
          freq.ab <- (2*p*q*Wab)/w
          if(infinitePop==F){ 
            Naa <- rbinom(1,n2,freq.aa)
            if(freq.aa<1){ 
              Nab <- rbinom(1,(n2-Naa),(freq.ab/(1-freq.aa)))
            }
            else {
              Nab <- 0
            }
            p <- ((2*Naa)+Nab)/(2*n2)
            q <- 1-p
            allele.freq[(i+1),j] <- p #new p after drift in columns 1:nPop
            allele.freq[(i+1),(j+nPop)] <- Nab/n2 #Ho in columns (nPop+1):(nPop*2)
            allele.freq[(i+1),(j+2*nPop)] <- 2*p*q #He in columns (nPop*2+1):nPop*3
            allele.freq[(i+1),(j+3*nPop)] <- w #pop mean fitness in last columns
          } 
          else { #no drift (infinite population) conditions
            p <- freq.aa+(freq.ab/2)
            q <- 1-p
            allele.freq[(i+1),j] <- p
            allele.freq[(i+1),(j+nPop)] <- freq.ab 
            allele.freq[(i+1),(j+2*nPop)] <- 2*p*q
            allele.freq[(i+1),(j+3*nPop)] <- w
          }
        } else { #if alleles are fixed
          if(p<=0){
            p <- 0
            w <- p*p*Waa+2*p*q*Wab+q*q*Wbb
          } else {
            p <- 1
            w <- p*p*Waa+2*p*q*Wab+q*q*Wbb
          }
          allele.freq[(i+1),j] <- p
          allele.freq[(i+1),(j+nPop)] <- 0
          allele.freq[(i+1),(j+2*nPop)] <- 0
          allele.freq[(i+1),(j+3*nPop)] <- w
        }
      } #end populations loop
    } #end generations loop
  #summary stats
  names <- c()
  for(i in 1:nPop){names[i]<-paste0("p",i)}
  for(i in (nPop+1):(2*nPop)){names[i]<-paste0("Ho",i-nPop)}
  for(i in (nPop*2+1):(3*nPop)){names[i]<-paste0("He",i-2*nPop)}
  for(i in (nPop*3+1):(4*nPop)){names[i]<-paste0("W",i-3*nPop)}
  colnames(allele.freq) <- names
  allele.freq$meanHo <- rowMeans(allele.freq[(nPop+1):(nPop*2)])
  allele.freq$meanHe <- rowMeans(allele.freq[(nPop*2+1):(nPop*3)])
  allele.freq$Fis <- 1-(allele.freq$meanHo/allele.freq$meanHe)
  allele.freq$mean.p <- rowMeans(allele.freq[1:nPop]) 
  allele.freq$Hs <- rowMeans(allele.freq[(nPop*2+1):(nPop*3)])
  allele.freq$Ht <- 2*allele.freq$mean.p*(1 - allele.freq$mean.p)
  allele.freq$Fst <- (allele.freq$Ht-allele.freq$Hs)/allele.freq$Ht
  allele.freq$Fst[allele.freq$Fst<0] <- 0
  allele.freq$gen <- 0:gen
  allele.freq$Fst[allele.freq$gen == 0] <- NA
  return(allele.freq)
}
