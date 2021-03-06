% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/snd.R
\name{snd.getPointIndexLowerThanTime}
\alias{snd.getPointIndexLowerThanTime}
\title{snd.getPointIndexLowerThanTime}
\usage{
snd.getPointIndexLowerThanTime(snd, time)
}
\arguments{
\item{snd}{Sound object}

\item{time}{time which is going to be found in samples}
}
\value{
integer
}
\description{
Returns index of sample which is nearest the given time from left, i.e.
sampleTime <= \code{time}.
}
\examples{
snd <- snd.sample()
snd.getPointIndexLowerThanTime(snd, 0.5)
}
\seealso{
\code{\link{snd.getPointIndexNearestTime}}, \code{\link{snd.getPointIndexHigherThanTime}}
}
