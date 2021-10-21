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
news_to_rss(package, file = NULL,
            guid = function(version, date, ...)
                            paste0(package, "_", version, "_", date),
            link,
            guid.digest = FALSE,
            guid.isPermaLink = FALSE,
            \dots)

rss_feed(items, title, description, language,
         link, atom.href, copyright)
}
\arguments{
  \item{package}{
    character
}
  \item{file}{
    character
}
  \item{guid}{ a function: the first two arguments are the version and
    date as specified in the NEWS file

}
  \item{link}{
    character. For \code{rss_feed}, the \acronym{URL} of the feed.
    For \code{news_to_rss}, item-specific links.

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
  \item{atom.href}{character.
    See \url{http://www.rssboard.org/rss-profile#namespace-elements-atom-link}
}
  \item{guid.digest}{
    logical: use \pkg{digest} package to compute guid?
  }
  \item{guid.isPermaLink}{
    logical
}
  \item{\dots}{
    more arguments
}
}
\details{

  Converts the NEWS file of an R package and converts
  the NEWS entries into an RSS feed (i.e. XML format).

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
\donttest{

out.file <- tempfile()
require("NEWSRSS")
copyright <- "Copyright 2015, Enrico Schumann"
title <- "NMOF &#8211; Numerical Methods and Optimization in Finance"
link <- "http://enricoschumann.net/R/packages/NMOF/"

description <- "News about package NMOF"
language <- "en-gb"
atom.href <- "http://enricoschumann.net/R/packages/NMOF/NMOF_news.xml"


cat(rss_feed(news_to_rss("NMOF", link = link), title, description,
             language, link, atom.href, copyright),
    file = out.file)

## pretty print with 'xmllint'
## system("xmllint -o ~/ESdotnet/R/packages/NMOF/NMOF_news.xml --format ~/tmp/NMOF_news.xml")

}}
