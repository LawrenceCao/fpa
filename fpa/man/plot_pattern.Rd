\name{plot_pattern}
\alias{plot_pattern}
\title{
Plot the general fixation pattern
}
\description{
The plot_pattern() function provides a quick tool to plot the fixation pattern for conditions. It generates 3-dimensional data, with x of "Time", y of "Region", and the colors representing the value of fixation probabilities.
}
\usage{
plot_pattern(data, Condition = "All")
}
\arguments{
  \item{data}{
the data frame returned by the get_pattern function.
}
  \item{Condition}{
the conditions which the user would like to plot. It can be a string ("All"), a number (e.g., 1), or a vector (e.g., c(1,2)). The default value is "All", meaning all conditions will be plotted.
}
}
\author{
Jinlu Cao
}

\seealso{
\code{\link{get_pattern}}, \code{\link{ft2fp}}
}
\examples{
data(pattern)
plot_pattern(pattern)
plot_pattern(pattern, Condition=1)
plot_pattern(pattern, Condition=c(1,2))
}
