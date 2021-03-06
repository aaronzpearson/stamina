% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crit.speed.R
\name{cs.results.model}
\alias{cs.results.model}
\alias{cs.move.ave}
\title{Critical Speed}
\usage{
cs.results.model(speed, dur = 600, sample.rate = 10, algo = "slow")

cs.move.ave(x, n)
}
\arguments{
\item{speed}{Athlete's speed}

\item{dur}{Duration of the velocity-duration curve}

\item{sample.rate}{Sample rate in Hz}

\item{algo}{Speed of the function}
}
\description{
Models a player's critical speed and returns a `data.frame` with the player's raw and modelled data over time.
}
\details{
`dur` is set to 600 s (10 minutes). It is not advised to change this value.

`algo` is the algorithm used. Default is slow which can take 10 - 60 s to run, depending on the machine. When set to fast, the algorithm will return a rough estimate of an athlete's critical speed.
}
\section{Functions}{
\itemize{
\item \code{cs.move.ave}: A moving average helper function
}}

