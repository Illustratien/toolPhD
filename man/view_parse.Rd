% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/view_parse.R
\name{view_parse}
\alias{view_parse}
\title{view parse text}
\usage{
view_parse(parse.vec)
}
\arguments{
\item{parse.vec}{character vector of parse text}
}
\value{
view the parse text immediately
}
\description{
\code{view_parse} view the parse text immediately
}
\examples{
  pacman::p_load(ggplot2,toolPhD)
  view_parse( c('P[i,DM["straw, 87"]]'))
  view_parse( c('P[i,DM["straw, 87"]]','bolditalic(P)["i, DM"["straw, 87"]]'))
}
\author{
Tien-Cheng Wang
}
\keyword{parse}
