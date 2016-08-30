\name{lineplot}
\alias{lineplot}
\title{
Plot the fixation probabilities for specific details
}
\description{
The function lineplot provides quick tools for plotting more detailed fixation probabilities for specific condition(s) and region(s). The function generates 2-dimensional line plots with "Time" as x, and "Fixation Probability" as y. 
}
\usage{
lineplot(data, Region = "All", Condition = "All")
}
\arguments{
  \item{data}{
is the data frame returned by get_pattern function.
}
  \item{Region}{
is the intended region(s) to plot. It can be a string ("All"), a number (e.g., 1), or a vector (e.g., c(1,2)).
}
  \item{Condition}{
is the intended condition(s) to plot.It can be a string ("All"), a number (e.g., 1), or a vector (e.g., c(1,2)). 
}
}
\author{
Jinlu Cao
}
\seealso{
\code{\link{get_pattern}}, \code{\link{ft2fp}}, \code{\link{plot_pattern}}
}
\examples{
data(pattern)
lineplot(pattern)
lineplot(pattern, Region="All", Condition=1)
lineplot(pattern, Condition=c(1,2))
lineplot(pattern, Region=2)
lineplot(pattern, Region=c(2,3), Condition=c(3,4,5))
}
