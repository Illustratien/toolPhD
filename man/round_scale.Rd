% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/round_scale.R
\name{round_scale}
\alias{round_scale}
\title{tidy digit of numbers}
\usage{
round_scale(vec)
}
\arguments{
\item{vec}{a vector, which may include different scale of numeric number.}
}
\value{
a character vector, which adjust format unit for each element
}
\description{
\code{round_scale} display the tidy unit of your observations
}
\examples{
vec <- c(100.1,10.1,1.1,0.1,0.01,0.001,0.0001,0.00001)
round_scale(vec)
}
\author{
Tien-Cheng Wang
}
\keyword{display}
\keyword{for}
\keyword{format}
\keyword{scientific}
