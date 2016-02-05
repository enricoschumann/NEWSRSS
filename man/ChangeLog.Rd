\name{ChangeLog}
\alias{ChangeLog}
\title{
  Parse GNU/Emacs-style ChangeLog File
}
\description{
  Read and parse an Emacs ChangeLog file.  
}
\usage{
ChangeLog(file)
}
\arguments{
  \item{file}{
    character. The path to the ChangeLog file.
}
}
\details{

  The function is not very lenient: it expects the standard format.
   
}
\value{
  A list of lists; each \sQuote{inner} list contains information about
  an entry 
  \item{date}{entry date}
  \item{author}{the name of the author}
  \item{email}{the email address of the author}
  \item{txt}{the actual txt}
}
\references{
  GNU coding standards \url{http://www.gnu.org/prep/standards/standards.html#Change-Logs}

  GNU coding standards \url{http://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Change-Log}

}
\author{
  Enrico Schumann
}
\examples{
\dontrun{
file <- "http://enricoschumann.net/R/packages/NMOF/ChangeLog"
ChangeLog(file)
}
}
