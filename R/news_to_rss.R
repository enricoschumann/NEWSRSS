## -*- truncate-lines: t; fill-column: 65; comment-column: 50; -*-

news_to_rss  <- function(package, file = NULL,
                         guid = function(version, date, ...) paste0(package, "_", version),
                         link, 
                         guid.isPermaLink = FALSE,
                         ... ) {
    if (!is.null(file))
        .NotYetUsed("file")

    .HTML_encode <- function(s) {
        s <- gsub("<", "&lt;", s)
        s <- gsub(">", "&gt;", s)
        s
    }

    ## ns <- tools:::.news_reader_default(file)
    ns <- news(package = package)
    if (is.null(ns))
        stop("no NEWS file found")
    
    ans <- aggregate(trim(ns$Text),
                     by = list(ns$Version),
                     FUN = function(x)
        paste0(.HTML_encode("<li>"), x, .HTML_encode("</li>"),
               collapse = "\n"))

    dates <- aggregate(ns$Date,
                       by = list(ns$Version),
                       FUN = tail, 1)

    ## browser()
    g <- guid(dates[[1]], dates[[2]], ...)
    items  <- paste0("<item>\n",
                     "<title>", ans[[1]],"</title>\n",
                     if (!missing(link))
                         paste0("<link>", link, "</link>\n"),
                     ifelse(is.na(dates[[2]]), "", paste0("<pubDate>", rfc822t(dates[[2]]) ,"</pubDate>\n")),
                     "<guid isPermaLink=\"false\">", g , "</guid>\n",
                     "<description>\n&lt;ul&gt;\n", ans[[2]], "\n&lt;/ul&gt;\n</description>\n",
                     "</item>\n")
    items
}


rss_feed <- function(items, title, description,
                     language, link,
                     atom.href, copyright) {
    
    head <-
        '<?xml version="1.0" encoding="UTF-8" ?>
     <rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">
       <channel>
         <title>#title#</title> 
         <link>#link#</link> 
         <description>#description#</description> 
         <language>#language#</language> 
         <copyright>#copyright#</copyright>
         <atom:link href="#atom.href#"
                    rel="self" type="application/rss+xml" />'
    
    for (s in c("title", "description", "language", "link",
                "atom.href", "copyright")) {
        head <- gsub(paste0("#", s, "#"), get(s), head)
    }
    c(head, items, "</channel>", "</rss>\n")
}

