#for backward compatibility (when shiny upgraded to bootstrap 3, old code needs upgrading too)
withBootstrap2({
  
  shinyUI(fluidPage(
    
    #Display datatable filters on top
    tags$head(tags$style("tfoot {display: table-header-group;}")),        
    
    #Add the app title, logos, and
    h1('Power Search'),
    img(src="indeed.jpg", height = 100, width = 100),
    img(src="glassdoor.png", height = 120, width = 120),
    
    #Add required JS libraries
    tagList(
      singleton(tags$head(tags$script(src='//cdn.datatables.net/1.10.4/js/jquery.dataTables.min.js',type='text/javascript'))),
      singleton(tags$head(tags$script(src='//cdn.datatables.net/tabletools/2.2.3/js/dataTables.tableTools.min.js',type='text/javascript'))),
      singleton(tags$head(tags$link(href='//cdn.datatables.net/tabletools/2.2.3/css/dataTables.tableTools.css',rel='stylesheet',type='text/css'))),
      singleton(tags$script(HTML("if (window.innerHeight < 400) alert('Screen too small');")))
    ),
    
    #Use the Sidebar layout
    sidebarLayout(
      sidebarPanel(
        
        
        #Add action buttons to plot the graphs. up to 3 graphs is permissible. the buttons are only visible in the first tab
        conditionalPanel(condition="input.tabs=='Plots'",
                         h5('Note: Running the app takes a little while at startup.'),
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
                         h5('Note: Running the app takes a little while at startup.'),
                         helpText('Job:'),
                         textInput('j',''),
                         helpText('Company:'),
                         selectizeInput('c','',choices=NULL,multiple=T),
                         helpText('Location:'),
                         selectizeInput('l','',choices=NULL,multiple=T),
                         sliderInput('d','Posted (days ago)',min = 0,max = 60,step = 5,value = 60),
                         checkboxGroupInput('s','',choices = c('Indeed','Glassdoor'),selected = c('Indeed','Glassdoor')),
                         actionButton('action6','Search!'), 
                         br(),
                         br(),
                         helpText('Click below to download the results of your search:')
                         
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
  
})