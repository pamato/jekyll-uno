
###########################################################################
# R function that retrieves data from a Web of Science search             #
# The function takes on two arguments:                                    #
#     (1) the URL from the first resulting article from the search;       #
#     (2) the total number of results in the search;                      #
###########################################################################

swoc <- function(x,k){
  ### Packages
  ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
      install.packages(new.pkg, dependencies = TRUE, repos = "http://cran.us.r-project.org")
    sapply(pkg, require, character.only = TRUE)
  }
  # usage
  packages <- c("rvest", "curl", "plyr")
  ipak(packages)
  ### Trim leading function
  trim.leading <- function (x)  sub("^\\s+", "", x)
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  ### Arguments
  res.length <- k
  ###                     ###
  ### Save webpages
  print(paste("Downloading", res.length, "html files (one for each article entry in WoS)..."))
  for(i in 1:res.length){
    ptm <- proc.time()
    print(paste(i, "/", k))
    u <- paste(substr(x, 1, nchar(x)-1),i,sep='')
    webpage <- curl_download(u, paste0("article", i, ".html"), handle=new_handle())
    Sys.sleep(2)
    time <- proc.time() - ptm
    print(paste("Finished in", round(time[3]), "seconds"))
  }
  ###                     ###
  ### Loop through saved files
  ws.df <- NULL
  print(paste("Looping through", res.length, "html files, scraping content and building final data frame..."))
  for (i in 1:res.length){
    ptm <- proc.time()
    print(paste(i, "/", k))
    final.data <- data.frame(page = i, author=NA, title=NA, journal=NA, year=NA, funding_agency = NA, cited=NA,
                             keywords = NA, grant_number = NA, funding_text=NA, woscat = NA, abstract=NA)
    ws.page <- read_html(paste0("article", i, ".html"))
    ## Information blocks
    page <- as.matrix(as.list(ws.page %>% html_nodes(".FR_field") %>% html_text()))
    rownames(page) <- apply(page, 1 , function(x) gsub("(.*\n)(.*)(:.*)" ,"\\2", x))
    rownames(page) <- noquote(rownames(page))
    block <- as.matrix(as.list(ws.page %>% html_nodes(".block-record-info") %>% html_text()))
    rownames(block) <- apply(block, 1 , function(x) gsub("(.*\n)(.*)(\n\n\n.*)" ,"\\2", x))
    ### Build data frames
    if (length(gsub('.*:(.*)','\\1',page[which(rownames(page) == "Web of Science Categories"),]) > 0)){
      final.data$woscat <- gsub('.*:(.*)','\\1',page[which(rownames(page) == "Web of Science Categories"),])}
    if (length(gsub(c("\n"), "",gsub(c("\240"), "",gsub(c("\302"), "", gsub('.*text(.*)','\\1',block[grep("Funding Agency", block),]))))) > 0){
      final.data$funding_text <- gsub(c("\n"), "",gsub(c("\240"), "",gsub(c("\302"), "", gsub('.*text(.*)','\\1',block[grep("Funding Agency", block),]))))
    }else{final.data$funding_text <- NA}
    final.data$journal <- gsub("(.*\n)(.*)(\n.*)", "\\2" ,((ws.page %>% html_nodes(".sourceTitle") %>% html_text())[1]))
    final.data$institutions <- paste(unique(ws.page %>% html_nodes("preferred_org") %>% html_text()), collapse=",")
    final.data$title <- gsub("(.*\n)(.*)(\n.*)", "\\2" ,(ws.page %>% html_nodes(".title") %>% html_text()))
    final.data$year <- gsub("(.*:\n)(.*)(\n.*)" ,"\\2", page[which(rownames(page)=="Published"),])
    final.data$author <- gsub("(.*\nBy:)(.*)(\n.*)" ,"\\2", page[which(rownames(page)=="By"),])
    final.data$author <- gsub("(?:\\(.*?\\)|\\.)(*SKIP)(*F)|[\\w' ,\\\"]+", " ", final.data$author, perl=TRUE) # remove text outside parentheses
    final.data$author <- gsub("\\)\\[\n \\]\n ",'', final.data$author, perl=TRUE)
    final.data$author <- gsub(" -", "", gsub("\\(",'', gsub("\n :","", gsub("\\)", "", final.data$author))))
    final.data$author <- trim.leading(final.data$author)
    if (length(gsub("(.*:\n)(.*)(\n.*)" ,"\\2", page[which(rownames(page)=="Times Cited in Web of Science Core Collection"),])) > 0){
      final.data$cited <-  as.numeric(gsub("(.*:\n)(.*)(\n.*)" ,"\\2", page[which(rownames(page)=="Times Cited in Web of Science Core Collection"),]))}
    if(length(which(rownames(page)=="KeyWords Plus")) > 0) {
      final.data$keywords <- tolower(gsub("(.*:)(.*)(\n.*)" ,"\\2", page[which(rownames(page)=="KeyWords Plus"),]))
    }
    if(length(which(rownames(page)=="Author Keywords")) > 0) {
      final.data$keywords <- tolower(gsub("(.*:)(.*)(\n.*)" ,"\\2", page[which(rownames(page)=="Author Keywords"),]))
    }
    if(length(gsub('(.*)(\\(C\\).*)', '\\1', gsub(c("\n"), "", gsub(c("\nAbstract"), "", gsub('.*text(.*)','\\1',block[grep("Abstract", block),]))))) > 0){
      final.data$abstract <- gsub('(.*)(\\(C\\).*)', '\\1', gsub(c("\n"), "", gsub(c("\nAbstract"), "", gsub('.*text(.*)','\\1',block[grep("Abstract", block),]))))
    } else {final.data$abstract <- NA}
    funding.info <- as.list(gsub("\240", "", gsub("\302", "", ws.page %>% html_nodes(".FR_table_borders") %>% html_nodes(".fr_data_row") %>% html_text())))
    ## Funding information
    if(length(funding.info) > 0){
      final.data <- final.data[rep(seq_len(nrow(final.data)), length(funding.info)), ]
      dat <- data.frame(funding_agency = rep(NA, length(funding.info)), grant_number=  rep(NA, length(funding.info)))
      for (j in 1:length(funding.info)){
        fa <- as.matrix(unlist(strsplit(gsub("\n\n", "", funding.info[j]),"\n")))
        fa <- as.matrix(fa[-which(fa==""),])
        if(nrow(fa)== 1){dat$funding_agency[j] <- fa[1]
        } else { 
          dat$funding_agency[j] <- fa[1]
          dat$grant_number[j] <- fa[2]
        }
      }
      final.data$funding_agency <- dat$funding_agency
      final.data$grant_number <- dat$grant_number
    }
    ws.df <- rbind(ws.df, final.data)
    time <- proc.time() - ptm
    print(paste("Finished in", round(time[3]), "seconds"))
  }
  return(ws.df)
}

## Worked Example ##

## Set parameters
url <- "https://apps.webofknowledge.com/full_record.do?product=WOS&search_mode=AdvancedSearch&qid=1&SID=Y15nyBc71mXqBiA2M7h&page=1&doc=1"
k <- 779

## Retrieve data

wos.search <- swoc(url,k)
rm(list=setdiff(ls(),"wos.search"))

