## -*- truncate-lines: t; -*-

news_to_rss  <- function(package, file = NULL,
                         guid = function(version, date, ...)
                             paste0(package, "_", version, "_", date),
                         link,
                         guid.digest = FALSE,
                         guid.isPermaLink = FALSE,
                         ... ) {
    if (!is.null(file))
        .NotYetUsed("file")

    .url.re <- "(.*)(https?://\\S+)(.*)"

    .HTML_encode <- function(s) {
        s <- gsub("&", "&amp;", s, perl = TRUE)
        s <- gsub("<", "&lt;", s)
        s <- gsub(">", "&gt;", s)
        s
    }

    ## ns <- tools:::.news_reader_default(file)
    ns <- news(package = package)
    if (is.null(ns)) {
        warning("no NEWS file found")
        return(invisible(NULL))
    }

    txt <- trim(ns$Text)

    ii <- grep(.url.re, txt, perl = TRUE)
    if (length(ii))
        txt[ii] <- gsub(.url.re, "\\1<a href=\"\\2\">\\2</a>\\3",
                        txt[ii], perl = TRUE)
    txt <- .HTML_encode(txt)

    ans <- aggregate(txt,
                     by = list(ns$Version),
                     FUN = function(x)
        paste0(.HTML_encode("<li>"), x, .HTML_encode("</li>"),
               collapse = "\n"))

    vers_dates <- aggregate(ns$Date,
                            by = list(ns$Version),
                            FUN = tail, 1)

    g <- guid(vers_dates[[1]], vers_dates[[2]], ...)
    if (guid.digest) {
        if (!requireNamespace("digest"))
            warning(sQuote("guid.digest"),
                    " is TRUE but package ", sQuote("digest"),
                    " not available")
        d <- character(length(g))
        for (i in seq_along(d))
            d[i] <- digest::digest(ans$x[i], algo = "sha1")
        names(d) <- ans[[1L]]
        g <- paste0(g, "_", d)
    }
    items  <- paste0("<item>\n",
                     "<title>", ans[[1]],"</title>\n",
                     if (!missing(link))
                         paste0("<link>", link, "</link>\n"),
                     ifelse(is.na(vers_dates[[2]]), "",
                            paste0("<pubDate>",
                                   rfc822t(vers_dates[[2]]) ,
                                   "</pubDate>\n")),
                     "<guid isPermaLink=\"", tolower(guid.isPermaLink), "\">", g , "</guid>\n",
                     "<description>\n&lt;ul&gt;\n", ans[[2]], "\n&lt;/ul&gt;\n</description>\n",
                     "</item>\n")
    items
}


rss_feed <- function(items,
                     title,
                     description,
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
             <atom:link href="#atom.href#" rel="self" type="application/rss+xml" />'
    tail <-
'\n        </channel>
     </rss>\n
'
    for (s in c("title",
                "description",
                "language",
                "link",
                "atom.href",
                "copyright")) {
        head <- gsub(paste0("#", s, "#"), get(s, inherits = FALSE), head)
    }
    c(head, items, tail)
}
