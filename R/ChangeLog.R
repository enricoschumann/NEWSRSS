## -*- truncate-lines: t; fill-column: 65; comment-column: 50; -*-

ChangeLog <- function(file) {

    .date.re <- "[12][0-9][0-9][0-9]-[0-1]?[0-9]-[0-3]?[0-9]"

    txt <- readLines(file)
    start <- grep(paste0("^", .date.re, " .*"), txt)
    end <- c(start[-1L]-1L, length(txt))

    n <- length(start)
    results <- vector("list", n)

    dates0 <- gsub(paste0("^(",.date.re,") .*"),
                   "\\1", txt[start])
    dates <- as.Date(dates0)
    dates[is.na(dates)] <- dates0[is.na(dates)]

    authors0 <- gsub(paste0("^", .date.re, " ", "([^<]+) (<.*)$"),
                    "\\1%%split%%\\2", txt[start])
    tmp <- strsplit(authors0, "%%split%%", fixed = TRUE)
    authors <- trim(unlist(lapply(tmp, `[`, 1L)))
    emails <- trim(unlist(lapply(tmp, `[`, 2L)))

    for (i in seq_len(n)) {
        itxt <- txt[(start[i]+1):(end[i])]
        itxt <- gsub("^[\t ]*", "", itxt)
        if (itxt[1] == "")
            itxt <- itxt[-1]
        if (itxt[length(itxt)] == "")
            itxt <- itxt[-length(itxt)]
        
        results[[i]] <- list(date = dates[i],
                             author = authors[i],
                             email = emails[i],
                             txt = itxt)
    }
    names(results) <- paste("ChangeLog entry", seq_len(n))
    results
}
