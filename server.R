#library(shiny)
#install.packages("shiny", lib="C:/TFS/Rlib/")
#library(shiny,  lib.loc="C:/TFS/Rlib/")
require(shiny)

#install.packages("knitr", lib="C:/TFS/Rlib/")
#library(knitr,  lib.loc="C:/TFS/Rlib/")
#require(knitr)

# install.packages("plyr", lib="C:/TFS/Rlib/")
library(plyr,  lib.loc="C:/TFS/Rlib/")
#require(plyr)

#install.packages("dplyr", lib="C:/TFS/Rlib/")
library(dplyr,  lib.loc="C:/TFS/Rlib/")
#require(dplyr)

#install.packages("lazyeval", lib="C:/TFS/Rlib/")
library(lazyeval,  lib.loc="C:/TFS/Rlib/")
#require(lazyeval)

#install.packages("tidyr", lib="C:/TFS/Rlib/")
library(tidyr,  lib.loc="C:/TFS/Rlib/")
#require(tidyr)
#library(plyr, lib.loc="C:/tfs/rlib")

#library(reshape,  lib.loc="C:/TFS/Rlib/")
require(reshape2)

require(ggplot2)
require(xlsx)
require(extrafont)
#library(xlsx)
#library(extrafont)
#loadfonts()
#loadfonts(device = "win")
# Define server logic required to draw a histogram
a <- c(1)

runcount = 0

dsg2 <- data.frame(Person=character(), Project=character(), MinWeek=as.numeric(), MaxWeek=as.numeric(), Hours=as.numeric(), ymin=as.numeric(),
                   stringsAsFactors=FALSE) 

kobe_theme <- function() {
  theme(
    #achtergrond
    plot.background = element_rect(fill = "#666161", colour = "#666161"), #achtergron
    panel.background = element_rect(fill = "#E2E2E3"), #vlakker in de chart
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(colour = "#E7A922", family = "Impact"),
    plot.title = element_text(colour = "#E7A922",  size = 25, family = "Impact"),
    axis.title = element_text(colour = "#552683", face = "bold", size = 13, family = "Impact"),
    panel.grid.major.x = element_line(colour = "#E7A922"), #geel
    panel.grid.minor.x = element_line(colour = "#666161"), #donkergrijs
    #    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_line(colour = "#666161"), #donkergrijs,
    strip.text = element_text(family = "Impact", colour = "white"),
    strip.background = element_rect(fill = "#E7A922"),
    axis.ticks = element_line(colour = "#E7A922")
  )
}
#C:/TFS/Additional/R/Planning/Planning/
pad_excel = "Planning2.xlsx"
dest_excel = "Allocational.xlsx"

persons <<- data.frame(read.xlsx(pad_excel, sheetName="Persons"), stringsAsFactors=FALSE)
persons[,1] = as.character(persons[,1])      

shinyServer(function(input, output) {
  
  output$sliders <- renderUI({
    numIndividuals <- 2
    lapply(1:length(persons[,1]), function(i) {
      sliderInput(paste("Person",i, sep=""),persons[i,1],min = 0,max = persons[i,4],value = persons[i,3], width=150)
    })
  })
  
  data <- reactive({ 
    
    prios <<- data.frame(read.xlsx(pad_excel, sheetName="Prios"), stringsAsFactors=FALSE)
    prios[,1] = as.character(prios[,1])
    prios[,2] = as.character(prios[,2])
    
    projects <<- data.frame(read.xlsx(pad_excel, sheetName="Projects"), stringsAsFactors=FALSE)
    projects[,1] = as.character(projects[,1])
    projects[,4] = as.character(projects[,4])
    
    persons <- data.frame(read.xlsx(pad_excel, sheetName="Persons"), stringsAsFactors=FALSE)
    persons[,1] <- as.character(persons[,1])      
    
    linput <<- reactiveValuesToList(input, all.names = FALSE);
    t= 1;
    for (person in linput) {
      persons[t,2] = person;
      t = t + 1;
    }
    persons[,1] <- as.character(persons[,1])      
    
    weeks <- c(1:60) 
    allocation <<- data.frame("Week" = numeric(0), "Project" = character(0), "Person" = character(0), "Allocation" = numeric(0), "Fixed" = numeric(0), "PC" = numeric(0), "Grouping" = character(0), stringsAsFactors=FALSE)
    
    for(f in c(1,0)) {  
      #loop door de weken
      for(w in 1:length(weeks)) 
      {
        #loop door de personen
        for(p in 1:length(persons[,1])) 
        {
          #     #Project met prio voor deze persoon
          if (length(prios[prios$Person==persons[p,1] & prios$Marker==0 & prios$Fixed==f,]$Prio) > 0 ) 
          {
            Tprios = prios[prios$Person==persons[p,1] & prios$Marker==0 & prios$Fixed==f,]; #Filter on person and marker
            #moet eigenlijk door de prios loopen
            prc = 0 #prio counter
            for (first_prio in Tprios[order(Tprios$Prio),]$Prio) {
              TPrioProject = Tprios[Tprios$Prio==first_prio,] #Filter on project(s) with prio
              #if (prc > 0) { print(TPrioProject$Project)}  
              for (person_project in TPrioProject$Project) { # for each project
                # if (prc > 0) { print(person_project)}
                remaining_hours <- projects[projects$Project==person_project,]$Remaining
                #book hours
                i = length(allocation[,1]) + 1
                #FIXED
                if (length(TPrioProject[TPrioProject$Project == person_project & TPrioProject$Fixed ==1,]$Fixed) > 0) {
                  hours <- TPrioProject[TPrioProject$Project == person_project]$Hours;  
                } else {
                  #hier komt het stuk, NIET FIXED
                  #dealing with fixed allocation; alleen doen als de andere allocatie fixed is. dan kijken naar resterende uren
                  if(length(allocation[allocation$Week==w & allocation$Person == persons[p,1] & allocation$Fixed==1, ]$Allocation)>0) {
                    phours = persons[p,2] - sum(allocation[allocation$Week==w & allocation$Person == persons[p,1] & allocation$Fixed==1, ]$Allocation);
                    if (phours < 0) { phours = 0 }
                  } else {
                    phours = persons[p,2];
                  }
                  #dealing with finalized shared projects#if project with same prio already finalized, add to percentage
                  if (length(prios[prios$Person==persons[p,1] & prios$Marker==1 & prios$Fixed==f & prios$Prio==first_prio ,]$Percentage) > 0) {
                    pc <- prios[prios$Person==persons[p,1] & prios$Marker==1 & prios$Fixed==f & prios$Prio==first_prio ,]$Percentage;
                    pc = sum(pc)
                    phours = persons[p,2] - sum(allocation[allocation$Week==w & allocation$Person == persons[p,1], ]$Allocation);
                  } else {
                    pc <- 0;
                  }
                  hours <- phours * ((TPrioProject[TPrioProject$Project == person_project & TPrioProject$Fixed == f,]$Percentage + pc)/100);
                  
                  
                } #ALLOCATION HOURS!
                if (remaining_hours < hours ) {
                  hours = remaining_hours;
                }
                if (prc > 0) { 
                  if (f == 1) {
                    hours = hours - sum(allocation[allocation$Week==w & allocation$Person==persons[p,1],]$Allocation)
                  } else {
                    if (hours > (persons[p,2] - sum(allocation[allocation$Week==w & allocation$Person==persons[p,1],]$Allocation))) {
                      hours = persons[p,2] - sum(allocation[allocation$Week==w & allocation$Person==persons[p,1],]$Allocation)
                    }
                  }
                }
                
                allocation[i,c(1,4)] <- c(w,hours); allocation[i,2:3] <- c(person_project, persons[p,1]); 
                allocation[i,5] <- c(f); allocation[i,7] <- c(projects[projects$Project==person_project,4]); 
                
                allocation[["Person"]] = as.character(allocation[["Person"]])
                projects[projects$Project==person_project,]$Remaining = remaining_hours - (hours * (TPrioProject[TPrioProject$Project==person_project,]$Efficiency/100))
                i = i + 1; # dit nog nagaan
                
                if (projects[projects$Project==person_project,]$Remaining < 1) {
                  prios[prios$Project==person_project,4] = rep(1,length(prios[prios$Project==person_project,4]))              
                } #else {first_prio = first_prio + 100       }
              }
              #als er niet afgeronde taken zijn dan break, prio=p, persoon=p
              if (length(prios[prios$Person==persons[p,1]&prios$Prio==first_prio& prios$Marker==0&prios$Fixed==f,]$Marker) > 0) {
                break
              } else {
                prc = prc + 1
              }
            } #loop prio
          } #if meerdere prios
        } #loop persoon
      }
    }
    
    write.xlsx(allocation, file=dest_excel, sheetName="Allocation")
    allocation[,7] = as.character(allocation[,7])   
    allocation
    
  })
  
  create_dsg = function()
  {
    allocation <<- data()
    length(allocation)
    dsg <- cbind(aggregate(Week ~ Person + Project + Grouping, allocation[allocation$Allocation > 0,], function(x) min(x) -1),              
                 aggregate(Week ~ Person + Project + Grouping, allocation[allocation$Allocation > 0,], function(x) max(x))[,4])
    dsg = cbind(dsg, aggregate(Allocation ~ Person + Project + Grouping, allocation[allocation$Allocation > 0,], function(x) as.integer(mean(x)))[,4]) 
    colnames(dsg) <- c("Person","Project", "Grouping", "MinWeek", "MaxWeek", "Hours")
    
    # offset tasks if > 1 per day
    dsg$ymin <- c(rep(0, nrow(dsg)))
    t <- table(dsg$Grouping)
    for(Grouping in rownames(t)) {
      if(t[[Grouping]] > 1) {
        ss <- dsg[dsg$Grouping == Grouping,]
        y  <- 0
        for(i in as.numeric(rownames(ss))) {
          dsg[i,]$ymin <- y
          y <- y + 100#(80 * (dsg$Hours/28)) + 10
        }
      }
    }
    
    
         if (runcount == 2) {
              dsg2 <<- as.data.frame(dsg)
         }
    
    dsg
  } 
  
  
  output$distPlot <- renderPlot({
    runcount <<- runcount + 1;
    dsg <<- create_dsg();
    mw = max(dsg$MaxWeek) +1;
    ggplot(dsg, aes(xmin = MinWeek, xmax = MaxWeek, ymin = ymin, ymax = ymin + 80 * (Hours/28), fill = factor(Person))) +
      geom_rect() + 
      facet_grid(Grouping~., scales = "free_y") + 
      xlab("Week") +
      ggtitle(paste("runcount", runcount, "  Additional Cost Ulrik:", sum(((dsg[dsg$Person=="Ulrik",]$MaxWeek - dsg[dsg$Person=="Ulrik",]$MinWeek) + 1) * dsg[dsg$Person=="Ulrik",]$Hours ) *130,
                    "Additional Cost Vedran:", sum(((dsg[dsg$Person=="Vedran",]$MaxWeek - dsg[dsg$Person=="Vedran",]$MinWeek) + 1) *
                                                     dsg[dsg$Person=="Vedran",]$Hours )*130)) +
      theme(plot.title = element_text(lineheight=.8)) +
      #geom_text(aes(label = MaxWeek,  y = ymin + 24, x = MaxWeek + 0.3), size = 5) + 
      geom_text(aes(label = paste(Project, " (",Person, ", hrs:",Hours, ") WK:", MaxWeek , sep=""),  y = ymin + 24, x = MaxWeek - 2), size = 5) + 
      geom_rect(data=dsg2, alpha=0.2) +
      kobe_theme() + 
      xlim(0,max(dsg[["MaxWeek"]]) + 2) +
      #ylim(0, max(dsg[["ymin"]]) +50) +
      geom_text(aes(label = Grouping,  y = 80, x = mw, size=20)); #+
    #coord_fixed(ratio = 0.005)
    
    
  })
  
  
  # Generate an HTML table view of the data
  output$summary <- renderTable({
    data.frame(x=create_dsg())
  })
  
  output$allocation <- renderTable({
    data.frame(x=data())
  })
  
  output$personweek <- renderTable({
    x=data()
    result = ddply(x,~Person+Week,summarise,sum=sum(Allocation))
    result %>% spread(Week, sum)
  })
  
})