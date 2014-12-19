#Install required packages
ListofPackages= c('shiny','ggplot2','scales')
NewPackages= ListofPackages[!(ListofPackages %in% installed.packages()[,'Package'])]
if(length(NewPackages)>0) install.packages(NewPackages)

#Load required packages
lapply(ListofPackages,require,character.only=TRUE)

#Load source code
source('jobsearch.R',local=TRUE)


shinyServer(function(input,output,session){
    
    #Server side search for the choices argument of selectizeInput in ui.R
    updateSelectizeInput(session, 'c', choices = as.character(df$Company), server = TRUE)
    updateSelectizeInput(session, 'l', choices = as.character(df$Location), server = TRUE)
    
    #Create a reactive function to look up the indices correponding to the user's inputs and return the search results by returning the indices found
    search <- reactive({
        
        #Look up the indices according to the user's inputs for job,company, location, days, and source
        ind.j <- if(input$j=='') NULL else grep(input$j,df[,'Job'],ignore.case = T)
        ind.c <- {tmp<-lapply(input$c, function(x) {which(df[,'Company']==x)}); Reduce(union,tmp)}
        ind.l <- {tmp<-lapply(input$l, function(x) {which(df[,'Location']==x)}); Reduce(union,tmp)}
        ind.d <- which(df[,'Posted']<=input$d)
        ind.s <- {tmp<-lapply(input$s, function(x) {which(df[,'Source']==x)}); Reduce(union,tmp)}
        
        #Store all the indices found above in one list
        ind.all <- list(ind.j,ind.c,ind.l,ind.d,ind.s)
        
        #Apply the intersect function recursively to ind.all to find the common indices in all the sublists of ind.all
        ind <- if(is.null(ind.s)) NULL else{Reduce(intersect,ind.all[!sapply(ind.all,is.null)])}
        
        #Return the search results
        df[ind,] 
    })

    #Create a reactive function to generate the data for the 1st graph
    p1 <- reactive({
      t <- table(search()$Location)
      if(max(t)>10) {t <- t[t>10]} else {t <- t[t>1]}
      t <- t[order(t,decreasing = T)]
      data.frame(City=names(t),Postings=t,row.names=NULL)
    })
    
    #Create a reactive function to generate the data for the 2nd graph
    p2 <- reactive({
      t <- table(search()$Company)
      if(max(t)>5) {t <- t[t>5]} else {t <- t[t>1]}
      t <- t[order(t,decreasing = T)]
      data.frame(Company=names(t),Postings=t,row.names=NULL)
    })
    
    #Create a reactive function to generate the data for the 3rd graph
    p3 <- reactive({
      t <- table(search()$Job)
      if(max(t)>5) {t <- t[t>5]} else {t <- t[t>1]}
      t <- t[order(t,decreasing = T)]
      data.frame(Job=names(t),Postings=t,row.names=NULL) 
    })
   
    #Send the 1st graph to ui.R
    output$bp1 <- renderPlot({
        input$action1 #plot triggered only when button is pressed
        if(input$action1==0) return() 
        else{isolate({
           g <-ggplot(p1(),aes(x=factor(City,levels=City),y=Postings))
           g+geom_bar(stat='identity')+labs(x=NULL,title='Location by Number of Postings')+
           theme(text=element_text(face='bold'),axis.text.x=element_text(angle=45,hjust=1),axis.text.y=element_text(angle=45,hjust=1),axis.title.y=element_text(vjust=0.3))     
        })
        }
    })
    
    
    #Send the 2nd graph to ui.R
    output$bp2 <- renderPlot({
      input$action2 #plot triggered only when button is pressed
      if(input$action2==0) return() 
      else{isolate({
        g <-ggplot(p2(),aes(x=factor(Company,levels=Company),y=Postings))
        g+geom_bar(stat='identity')+labs(x=NULL,title='Company by Number of Postings')+
          theme(text=element_text(face='bold'),axis.text.x=element_text(angle=45,hjust=1),axis.text.y=element_text(angle=45,hjust=1),axis.title.y=element_text(vjust=0.3)) 
      })
      }
    })
                
    #Send the 3rd graph to ui.R
    output$bp3 <- renderPlot({
      input$action3 #plot triggered only when button is pressed
      if(input$action3==0) return() 
      else{isolate({
        g <-ggplot(p3(),aes(x=factor(Job,levels=Job),y=Postings))
        g+geom_bar(stat='identity')+labs(x=NULL,title='Job Title by Number of Postings')+
          theme(text=element_text(face='bold'),axis.text.x=element_text(angle=45,hjust=1),axis.text.y=element_text(angle=45,hjust=1),axis.title.y=element_text(vjust=0.3))
      })
      }
    })
    
    #Send the searchresult table to ui.R
    output$searchresult <- renderDataTable({
      input$action6 #triggered only when button is pressed
      if(input$action6==0) return() 
      else{isolate({
        search()
      })
      }
    }, option=list(autoWidth=FALSE,pageLength=100,
                   columnDefs = list(list(targets =c(5,6) -1, searchable = FALSE),list(sWidth=c("100px"))),
                   "dom" = 'T<"clear">lfrtip',
                   "oTableTools" = list(
                   "sSwfPath" = "//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf",
                   "aButtons" = list("copy","print",list("sExtends" = "csv","sButtonText" = "Export","aButtons" = "csv")))
                   )
    
    )
      
    
})
