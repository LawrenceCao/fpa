\name{get_pattern}
\alias{get_pattern}
\title{
Get the general fixation pattern
}
\description{
The get_pattern() function aggregates the data so that the general fixation pattern can be shown for each condition. Users should provide the data frame returned in ft2fp() function. Users can use the returned data frame of this function to make plots on the pattern by themselves, or use plot_pattern() and lineplot() functions.
}
\usage{
get_pattern(data)
}
\arguments{
  \item{data}{
is the data frame returned by the ft2fp function.
}
}
\value{
a data frame which shows the averaged fixation probabilities for each spatio-temporal unit for each condition.
}
\author{
Jinlu Cao
}

\seealso{
\code{\link{ft2fp}}, \code{\link{plot_pattern}}, \code{\link{lineplot}}
}
\examples{
data(newdata)
pattern <- get_pattern(newdata)
}
