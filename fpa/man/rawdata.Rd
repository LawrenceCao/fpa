\docType{data}
\name{rawdata}
\alias{rawdata}
\title{Fixation time data of an eye movement experiment}
\format{In the data frame each row represents one fixation of the eyes.  
  Fixations are ordered chronologically within trial.  The data frame has 
  the following columns:
    \describe{
      \item{\code{List}}{the id of the list}
      \item{\code{Subject}}{the id of the subject}
      \item{\code{Condition}}{the id of the condition}
      \item{\code{Item}}{the id of the item}
      \item{\code{Region}}{the id of the region being fixed on}
      \item{\code{Fix_Start}}{the start time of the fixation}
      \item{\code{Fix_End}}{the end time of the fixation}}}
\usage{
  data(rawdata)
}
\description{
  This data set is the eye movement data recorded during reading sentences. 
  The sentences consists of 8 regions defined by the researcher. The 
  experiment consists of 4 lists (8 items in each list), 23 subjects, and 
  4 condtions. Each row contains the information of one fixation. This data
  frame is a template for the data to be provided by users of fpa-package,
  and the number and names of variables should be same with this data frame.
  If the user does not have several lists in his/her experiment, lease use
  the id 1 for every row.
}
\keyword{datasets}

