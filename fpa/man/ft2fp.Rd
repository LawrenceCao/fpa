\name{ft2fp}
\alias{ft2fp}
\title{
Convert fixation time to fixation probability
}
\description{
The ft2fp function transforms the fixation time (start and end time for each fixation) data to fixation probability data. The function can finish all the preprocessing of using FPA to analyze eye movement data. Users should provide the fixation time data, the critical region number, the intended time course of interest, and the intended time interval. The user can also adjust the results by input other requests like if the user wants to normalize fixation durations, if the user wants to exclude the trials with no regression, and if the user wants to exclude the information on first pass on critical region. 
}
\usage{
ft2fp(data,CriticalRegion,TimeCourse,TimeInterval,Normalize=TRUE,ExcludeNoRegress=FALSE,ExcludeFirstPass=TRUE)
}
\arguments{
  \item{data}{
the raw eye movement data provided by user. It is a data frame which contains variables of "List", "Subject", "Item", "Condition", "Region", "Fix_Start", and "Fix_End". The names and number of variables in your data should be exactly same with above.
}
  \item{CriticalRegion}{
the No. of region in which the researcher is interested. All fixation information before the first-pass on that region will be discarded for each trial.
}
  \item{TimeCourse}{
the time course to be analyzed after the first-pass of critical region. The unit is millisecond.
}
  \item{TimeInterval}{
the time interval (or bin) to show in the time course of interest. The unit is millisecond, and the value should be smaller than the value for TimeCourse.
}
  \item{Normalize}{
to choose whether to normalize the fixation duration according to each subject's mean duration and general mean duration. When Normalize is TRUE, the fixation durations are adjusted for each subject's reading rate. The default value if TRUE.
}
  \item{ExcludeNoRegress}{
to choose whether to exclude the trials with no regression after the first-pass on critical region. The default value is FALSE.
}
  \item{ExcludeFirstPass}{
to choose whether to exclude the fixations at the first pass (or Gaze duration) on critical region. The default value is TRUE.
}
}

\value{
a data frame with the variables of "list", "subject", "condition", "region", "time", "fix_prob" (fixation probability), "y" (number of trials with fixation) and "N" (number of total valid trials).
}

\author{
Jinlu Cao
}

\seealso{
\code{\link{fpa}}
}
\examples{
data(rawdata)
newdata <- ft2fp (rawdata, 4, 3000, 100)
newdata <- ft2fp (rawdata,4,3000,100,Normalize=TRUE,ExcludeNoRegress=TRUE,ExcludeFirstPass=FALSE) 
}