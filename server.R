#Install required packages
ListofPackages= c('shiny','ggplot2','scales')
NewPackages= ListofPackages[!(ListofPackages %in% installed.packages()[,'Package'])]
if(length(NewPackages)>0) install.packages(NewPackages)

#Load required packages
lapply(ListofPackages,require,character.only=TRUE)

#Load source code
source('C:/Users/Bahae.Omid/Desktop/PowerSearch App/jobsearch.R',local=TRUE)


shinyServer(function(input,output){
    
    #Create a reactive function to generate the data for the 1st graph
    p1 <- reactive({
        t <- table(df$Location)
        t <- t[t>10]
        t <- t[order(t,decreasing = T)]
        data.frame(City=names(t),Postings=t,row.names=NULL)
        
    })
    
    #Create a reactive function to generate the data for the 2nd graph
    p2 <- reactive({
      t <- table(df$Company)
      t <- t[t>5]
      t <- t[order(t,decreasing = T)]
      data.frame(Company=names(t),Postings=t,row.names=NULL)
      
    })
    
    #Create a reactive function to generate the data for the 3rd graph
    p3 <- reactive({
      t <- table(df$Job)
      t <- t[t>5]
      t <- t[order(t,decreasing = T)]
      data.frame(Job=names(t),Postings=t,row.names=NULL)
      
    })
    
    #Create a reactive function to deal with inputs of the Search tab
    search <- reactive({
      if(length(input$j)>0) {ind <- grep(input$j,df[,'Job'],ignore.case = T); df <- df[ind,] }
      if(length(input$c)>0) {ind <- grep(input$c,df[,'Company'],ignore.case = T); df <- df[ind,]}
      if(length(input$l)>0) {ind <- grep(input$l,df[,'Location'],ignore.case = T); df <- df[ind,]}
      if(input$d >=0) {ind <- df[,'Posted.Since.Days.Ago']<=input$d ; df <- df[ind,]}
      if(length(input$s)==1) {ind <- df[,'Link.Source']==input$s; df <- df[ind,]}
      else if(is.null(input$s)) {df <- df[0,]}
      df
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
                   columnDefs = list(list(sWidth=c("100px")))))
    
    #Allow user to download the data via downloadhandler
    output$down <- downloadHandler(
        filename='filtered.csv',
        content=function(file){write.csv(search(),file,row.names=FALSE)}
    )
    
    
    
})
