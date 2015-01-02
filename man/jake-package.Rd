\name{jake-package}
\alias{jake-package}
\alias{jake}
\docType{package}
\title{
General Purpose R Utilities
}
\description{
This is a general purpose repository for R code that I've found useful.
}
\details{
\tabular{ll}{
Package: \tab jake\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-12-23\cr
License: \tab Open Source\cr
}
Wikipedia Scrape - This set of functions makes it easy to scrape Wikipedia for page views on specific articles and then view those pageviews over time.
}
\author{
Jake Gaecke
}
\references{

}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
\seealso{
}
\examples{
#Wikipedia Scrape
url <- google("Orion capsule")
article <- wikiURL("Orion capsule")
wiki_data <- getWikiStats(2013, 2014, c("NASA", "Orion_(spacecraft)"))
WikiPVPlot(na.omit(wiki_data), TRUE)

}
