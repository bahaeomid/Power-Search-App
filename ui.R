shinyUI(fluidPage(
    
    #Display datatable filters on top
    tags$head(tags$style("tfoot {display: table-header-group;}")),        
    
    #Add a title
    h1('Power Search'),
    img(src="indeed.jpg", height = 100, width = 100),
    img(src="glassdoor.png", height = 120, width = 120),
    
    #Use the Sidebar layout
    sidebarLayout(
        sidebarPanel(
            
                        
            #Add action buttons to plot the graphs. up to 3 graphs is permissible. the buttons are only visible in the first tab
            conditionalPanel(condition="input.tabs=='Plots'",
                             h5('Note: Running the app takes a little while to run at startup.'),
                             h5('The graphs are updated based on the search filters.
                                      Make sure to clear the filters, in the Search tab, if you want to see the results for the entire dataset.'),
                             helpText('Top jobs by number of postings:'),
                             actionButton('action3','Graph 1'),
                             br(),
                             br(),
                             helpText('Top cities by number of postings:'),
                             actionButton('action1','Graph 2'),
                             br(), 
                             br(),
                             helpText('Top companies by number of postings:'),
                             actionButton('action2','Graph 3')
            ),    
            
            #Add fields to search by and download button to allow exporting search results to csv.
            conditionalPanel(condition="input.tabs=='Search'",
                             h5('Note: Running the app takes a little while to run at startup.'),
                             helpText('Job:'),
                             textInput('j',''),
                             helpText('Company:'),
                             textInput('c',''),
                             helpText('Location:'),
                             textInput('l',''),
                             sliderInput('d','Posted (days ago)',min = 0,max = 60,step = 5,value = 60),
                             checkboxGroupInput('s','',choices = c('Indeed','Glassdoor'),selected = c('Indeed','Glassdoor')),
                             actionButton('action6','Search!'), 
                             br(),
                             br(),
                             helpText('Click below to download the results of your search:'),
                             downloadButton('down','Download')
                                                             
            )
            
        ),
        
        
        mainPanel(
            #Add tabs to display results seperately
            tabsetPanel(id='tabs',type='tab',
                        tabPanel(class='g','Search',dataTableOutput('searchresult')),
                        tabPanel(class='g','Plots',plotOutput('bp3'),plotOutput('bp1'),plotOutput('bp2'))
            )   
            
        )
        
    )
        ))
