#Load required libraries
packages <- list('XML')
lapply(packages, function(x) {if(!(x %in% installed.packages())) install.packages(x) else require(x,character.only = TRUE)})

#Define initial links
glassdoor <- list('http://www.glassdoor.ca/Job/engineer-jobs-SRCH_KO0,8.htm',"http://www.glassdoor.ca/Job/project-controls-jobs-SRCH_KO0,16.htm")
indeed <- list("http://ca.indeed.com/jobs?q=engineer","http://ca.indeed.com/jobs?q=project+controls")

#Define a function to generate a full list of links to process later
links <- function (linklist) {
    
    output <- list()
    
    for (link in linklist) {
        if (grepl("glassdoor",link)) {
          counter <- 1:50
          newlink <- sub ('.htm','',link)
          for (num in counter) {output <- c(output,paste0(newlink,'_IP',num,'.htm'))}
        
        }
        else {
          counter <- seq(0,1000,by=10)
          for (num in counter) {output <- c(output, paste0(link,'&start=',num))}
        }
    }    
    return (output)
    
}

#A variable to hold the list of links generated from the previous function
listoflinks <- links(c(indeed,glassdoor))

#Initialize a dataframe to store the results of the web scraping
df <- data.frame()

#loop to scrape desired elements from the links generated above recursively
for (link in listoflinks) {
  
  if (grepl("indeed",link)) {    
    
    doc <- htmlTreeParse(link, useInternalNodes = TRUE)
    jobs <- xpathSApply(doc,"//h2[@class='jobtitle']/a",function(x) xmlGetAttr(x,"title"))
    comps <- xpathSApply(doc,"//span[@class='company']/span",xmlValue)
    locs <- xpathSApply(doc,"//span[@class='location']/span",xmlValue)
    dates <- xpathSApply(doc,"//td[@class='snip']//span[@class='date']",xmlValue)
    Link.Source <- rep('Indeed',length(jobs))
  }
  else {
     fileurl <- readLines(link)  
     doc <- htmlTreeParse(fileurl, useInternalNodes = TRUE)
     jobs <- xpathSApply(doc,"//h3[@itemprop='title']/a/tt[@class='notranslate']",xmlValue)
     comps <- xpathSApply(doc,"//span[@class='employerName']//tt[@class='i-emp']",xmlValue)
     locs <- xpathSApply(doc,"//span[@itemprop='jobLocation']//tt[@class='i-loc']",xmlValue)
     dates <- xpathSApply(doc,"//div[@class='logo floatLt']//div[@class='minor']",xmlValue)
     Link.Source <- rep('Glassdoor',length(jobs))
  }
    #handle error
    possibleError <- tryCatch(rbind(df,data.frame(jobs,comps,locs,dates,Link.Source,stringsAsFactors=FALSE)),error=function(e) e)
    if(inherits(possibleError, "error")) next
    else df <- rbind(df,data.frame(jobs,comps,locs,dates,Link.Source,stringsAsFactors=FALSE))
}

#Name the columns of the output dataframe 
names(df) <- c('Job','Company','Location','Posted.Since.Days.Ago','Link.Source')

#Convert the date column to numeric
df[,'Posted.Since.Days.Ago'] <- data.frame(sapply(df[,'Posted.Since.Days.Ago'],function(x) {if ("hours" %in% strsplit(x," ")[[1]]|"hrs" %in% strsplit(x," ")[[1]]) { x <- 1}else{x <-sub('Sponsored',0,x);x <-gsub(' |\\+|[aA-zZ]*','',x);as.numeric(x)}}))

#Remove the province from location column
df[,'Location'] <- data.frame(sapply(df[,'Location'], function(x){x <- gsub('\\, .*','',x)}))

#Convert the columns of the output dataframe to appropriate classes
df[c('Job','Company','Location','Link.Source')] <- data.frame(lapply(c('Job','Company','Location','Link.Source'),function(x) factor(df[,x]))) 

#Remove irrelevant jobs
removejobs <- list('senior','sr.','electric','sale','civil','water','purchas','superintendent','environm','procur'
,'complian','manager','journey','intern','co-op','welder','director','clerk')

for (job in removejobs) {
ind <- grep(job,df[,'Job'],ignore.case = T)
if (length(ind)!=0) df <-  df[-ind,]
else df <- df
}

#Remove duplicate entries (only based on the values in the first three columns)
dup <- duplicated(df[,1:3])
df <- df[!dup,]

#Reset rownames
row.names(df) <- NULL
