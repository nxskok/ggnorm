% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code.R
\name{ggnormplot}
\alias{ggnormplot}
\title{Draw normal quantile plot with line}
\usage{
ggnormplot(z)
}
\arguments{
\item{z}{A vector}
}
\value{
Normal quantile plot with line
}
\description{
Draw normal quantile plot with line
}
\examples{
set.seed(1)
ggnormplot(rnorm(100))
ggnormplot(rchisq(100,3))
}

