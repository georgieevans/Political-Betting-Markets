# load relevant packages
pkgs <- c("rvest", "magrittr", "httr", "stringr", "rjson")
for (pkg in pkgs) {
  if (!require(pkg, character.only = T)) {
    install.packages(pkg)
    library(pkg)
  }
}

#function to scrape all odds from wayback 
scrape_bets_wayback <- function(url) {
  # call json file from API
  file.url <- paste0("http://labs.mementoweb.org/timemap/json/", url)
  # read in json file
  data <- rjson::fromJSON(file = file.url)
  # store in list
  urls <- list()
  htmls <- list()
  print('Reading HTML as XML', quote = FALSE)
  for (i in 1:length(data$mementos$list)) {
    urls[[i]] <- data$mementos$list[[i]]$uri
    htmls[[i]] <- tryCatch({
      read_html(urls[[i]])
    }, error = function(c) {
      return("NA")
    })
    t <- round(i/length(data$mementos$list)*100)
    if(t %in% seq(0, 100, 5)){print(paste0(t, "%"), quote = FALSE)}
  }
  # xpath
  selector <- '//*[@id="container"]/script[1]/text()'   
  selector2 <- '//*[@id="outer-container"]/script[1]/text()'  
  #
  x <- list()
  n <- c(1:length(htmls))
  # convert pi chart into dataframe!
  print('Converting XML to data frame', quote = FALSE)
  data.list <- lapply(n, FUN = function(i) {
    if(typeof(htmls[[i]]) == "list"){
      x <- htmls[[i]] %>%
        html_nodes(xpath = selector) %>%
        html_text()
      if(length(x) != 1){
        x <- htmls[[i]] %>%
          html_nodes(xpath = selector2) %>%
          html_text()
      }
    }else{
      x <- c("NA", "NA")
    }
    # if pi chart data is found
    if (length(x) == 1) {
      text <- strsplit(x, split = '\"data\": ')[[1]][2]
      text2 <- strsplit(text, "],")[[1]]
      text3 <- strsplit(text2, '",')
      data.frame <- as.data.frame(do.call(rbind, lapply(1:length(text3), FUN = function(j) {
        a <- strsplit(text3[[j]][1], " ")
        b <- gsub("[^0-9A-Za-z///' ]", "'", a[[1]], ignore.case = TRUE)
        c <- gsub("'", "", b, ignore.case = TRUE)
        d <- gsub(",", "", toString(paste(c[1:(length(c) - 1)])))
        return(c(d, c[length(c)]))
      })))
      data.frame$datetime <- data$mementos$list[[i]]$datetime
      data.frame$url <- urls[[i]]
    }
    else {
      data.frame <- data.frame(V1 = NA, V2 = NA, datetime = data$mementos$list[[i]]$datetime, url = "NA")
    }
    return(data.frame)
  })
  
  # data frame!
  var <- strsplit(url, "/")[[1]]
  var.name <- toString(var[c(length(var), length(var) - 1)])
  final.data <- as.data.frame(do.call(rbind, data.list))
  colnames(final.data) <- c("bet", "odds", "datetime", "url")
  final.data$bet.subject <- var.name
  final.data$time.of.day <- NA
  return(final.data)
}


#function to create win probabilities 

win_prob_data <- function(data){
  string <- strsplit(as.character(data$odds), "/")
  
  data$numerator.bet <- sapply(1:nrow(data), FUN = function(i) 
    as.numeric(string[[i]][1]))
  
  data$denominator.bet <- sapply(1:nrow(data), FUN = function(i) 
    as.numeric(string[[i]][2]))
  
  data$probability <- round(data$denominator.bet/(data$denominator.bet + data$numerator.bet), 3)
  data$date <- as.Date(substr(data$datetime, 1, 10))
  return(data)
}


# scrape today's odds
scrape_bets_today <- function(url) {
  print('Reading HTML as XML', quote = FALSE)
  html <- tryCatch({
    read_html(url)
  }, error = function(c) {
    return("NA")
  })
  # xpath
  selector <- '//*[@id="container"]/script[1]/text()'   
  selector2 <- '//*[@id="outer-container"]/script[1]/text()'  
  # convert pi chart into dataframe
  print('Converting XML to data frame', quote = FALSE)
  data.list <-  if(typeof(html) == "list"){
    x <- html %>%
      html_nodes(xpath = selector) %>%
      html_text()
    if(length(x) != 1){
      x <- html  %>%
        html_nodes(xpath = selector2) %>%
        html_text()
    }
  }else{
    x <- c("NA", "NA")
  }
  # if pi chart data is found
  if (length(x) == 1) {
    text <- strsplit(x, split = '\"data\": ')[[1]][2]
    text2 <- strsplit(text, "],")[[1]]
    text3 <- strsplit(text2, '",')
    data.frame <- as.data.frame(do.call(rbind, lapply(1:length(text3), FUN = function(j) {
      a <- strsplit(text3[[j]][1], " ")
      b <- gsub("[^0-9A-Za-z///' ]", "'", a[[1]], ignore.case = TRUE)
      c <- gsub("'", "", b, ignore.case = TRUE)
      d <- gsub(",", "", toString(paste(c[1:(length(c) - 1)])))
      return(c(d, c[length(c)]))
    })))
    data.frame$datetime <- as.Date(Sys.Date())
    data.frame$url <- url
  }
  else {
    data.frame <- data.frame(V1 = NA, V2 = NA, datetime = Sys.Date(), url = "NA")
  }
  # data frame!
  var <- strsplit(url, "/")[[1]]
  var.name <- toString(var[c(length(var), length(var) - 1)])
  final.data <- data.frame
  colnames(final.data) <- c("bet", "odds", "datetime", "url")
  final.data$bet.subject <- var.name
  final.data$time.of.day <- as.numeric(gsub(":", "", substr(Sys.time(), 12, 16)))
  return(final.data)
}



