#' Get JSON Data
#'
#' Downloads JSON format data from a specified URL. This function is specific to scraping page view data from Wikipedia logs.
#' Returns a data frame.
#' @param url String containing the Internet location of JSON data.
#' @keywords json, wikipedia
#' @export
#' @examples
#' getData(str_URL)
getWikiData <- function(url){
  #function to download data in json format
  require(rjson)
  raw.data <- readLines(url, warn="F")
  rd  <- fromJSON(raw.data)
  rd.views <- rd$daily_views
  rd.views <- unlist(rd.views)
  rd <- as.data.frame(rd.views)
  rd$date <- rownames(rd)
  rownames(rd) <- NULL
  return(rd)
}

#' Right
#'
#' Returns the right n characters of a specified string.
#' @param str String value to subset.
#' @param n Number of characters to return from right side of string.
#' @keywords right, left, subset
#' @export
#' @examples
#' right('Hello World', 5)
right <- function(str, n){
  substr(str, nchar(str) - n + 1, nchar(str))
}

#' Get Wikipedia URLs
#'
#' This function returns the monthly page view URLs for specific Wikipedia articles stored at http://stats.grok.se
#' @param y1 Beginning Year.
#' @param y2 Ending Year.
#' @param term This is the unique part of the Wikipedia URL representing the article. This may be a vector representing more than one article.
#' @keywords wikipedia
#' @export
#' @examples
#' getWikiURLs(2010, 2014, c("NASA", "Orion_(spacecraft)", "Delta_IV_Heavy", "Exploration_Flight_Test_1"))
getWikiURLs <- function(y1, y2, term){
  #function to create a list of urls given a term and a start and endpoint
  urls <- NULL
  for (year in y1:y2){
    for (month in 1:12){
      urls <- c(urls, (paste("http://stats.grok.se/json/en/", year, right(paste0("00", month), 2), "/", term, sep="")))
    }
  }
  return(urls)
}

#' Get Wikipedia Stats
#'
#' This function downloads daily page view data for each Wikipedia term passed in.
#' Data are downloaded from http://stats.grok.se in monthly increments.
#' Returns a data frame containing Views, Date, and Wikipedia (article titles).
#' @param y1 Beginning Year.
#' @param y2 Ending Year.
#' @param term This is the unique part of the Wikipedia URL representing the article. This may be a vector representing more than one article.
#' @keywords wikipedia, pageviews
#' @export
#' @examples
#' wiki_data <- getWikiStats(2013, 2014, c("NASA", "Orion_(spacecraft)", "Delta_IV_Heavy", "Exploration_Flight_Test_1"))
getWikiStats <- function(y1, y2, terms){
  #function to download data for each term
  #returns a dataframe
  output <- NULL
  for (term in terms){
    urls <- getWikiURLs(y1, y2, term)

    results <- NULL
    for (url in urls){
      print(url)
      results <- rbind(results, getWikiData(url))
    }
    results$term <- term

    output <- rbind(output,results)
  }
  output$date <- as.Date(output$date)
  names(output) <- c("Views", "Date", "Wikipedia")
  return(output)
}

#' Wikipedia Plot
#'
#' Plots Wikipedia page view data over time.
#' @param input Wikipedia page view data frame from getWikiStats() function. Columns requires are Views, Date, and Wikipedia (article titles).
#' @keywords wikipedia, plot
#' @export
#' @examples
#' WikiPVPlot(wiki_data)
WikiPVPlot<- function(input){
  #function to plot data from the getWikiStats function
  require(lubridate)
  require(ggplot2)
  input$Date <- as.Date(input$Date)
  names(input) <- c("Views", "Date", "Wikipedia")
  ggplot(input, aes(Date, Views, colour=Wikipedia)) + geom_line() + theme(legend.position="top") #+ geom_smooth(alpha=0.5)
}

#' Wikipedia URL
#'
#' This function searches the internet using Google for the search term and the word 'Wikipedia'. Returns the URL of the first result.
#' @param term Search term to find a relevant Wikipedia page.
#' @keywords wikipedia, google, search
#' @export
#' @examples
#' wikiURL('Orion capsule')
wikiURL <- function(term){
  library(RCurl)
  library(RJSONIO)  # or library(rjson)
  val <- getForm("http://ajax.googleapis.com/ajax/services/search/web", q = paste0("Wikipedia ", term), v = "1.0")
  results <- fromJSON(val)
  url <- results$responseData$results[[1]][3]
  print(url)
  return(url)
}

#' Google
#'
#' Searches the Internet using Google and returns the URL of the first result.
#' @param term Search term to find a relevant page.
#' @keywords google, search
#' @export
#' @examples
#' function_call()
google <- function(term){
  library(RCurl)
  library(RJSONIO)  # or library(rjson)
  val <- getForm("http://ajax.googleapis.com/ajax/services/search/web", q = term, v = "1.0")
  results <- fromJSON(val)
  url <- results$responseData$results[[1]][3]
  print(url)
  return(url)
}