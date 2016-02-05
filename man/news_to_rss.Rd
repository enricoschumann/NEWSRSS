\name{NEWSRSS-package}
\alias{NEWSRSS-package}
\alias{NEWSRSS}
\alias{news_to_rss}
\alias{rss_feed}
\concept{RSS}
\concept{NEWS}
\title{
  Convert NEWS File to RSS Feed
}
\description{
  Convert package NEWS file to RSS items.
}
\usage{
news_to_rss(package, file,
            guid = function(version, date, ...) paste0(package, "_", version),
            link, 
            guid.isPermaLink = FALSE, \dots)

rss_feed(items, title, description, language, link, atom.href, copyright)
}
\arguments{
  \item{package}{
    character
}
  \item{file}{
    character
}
  \item{guid}{
    function
}
  \item{link}{
    character. The URL of the feed.
}
  \item{items}{
    character. The items of the feed, in \acronym{XML} markup.
}
  \item{title}{
    character
}
  \item{description}{
    character
}
  \item{language}{
    character
}
  \item{copyright}{
    character
}
  \item{atom.href}{
    character. See \url{http://www.rssboard.org/rss-profile#namespace-elements-atom-link}
}
  \item{guid.isPermaLink}{
    logical
}
  \item{\dots}{
    more arguments
}
}
\details{

xxx
  
}
\value{
  character vector
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link[utils]{news}}
}
\examples{
\dontrun{

require("NEWSRSS")
copyright <- "Copyright 2015, Enrico Schumann"
title <- "NMOF &#8211; Numerical Methods and Optimization in Finance"
link <- "http://enricoschumann.net/R/packages/NMOF/"

description <- "News about package NMOF"
language <- "en-gb"
atom.href <- "http://enricoschumann.net/R/packages/NMOF/NMOF_news.xml"


cat(rss_feed(news_to_rss("NMOF", link = link), title, description,
             language, link, atom.href, copyright),
    file = "~/tmp/NMOF_news.xml")

## pretty print
system("xmllint -o ~/ESdotnet/R/packages/NMOF/NMOF_news.xml --format ~/tmp/NMOF_news.xml")

}}
