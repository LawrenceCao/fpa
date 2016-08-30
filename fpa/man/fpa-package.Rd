\name{fpa-package}
\alias{fpa-package}
\alias{fpa}
\docType{package}
\title{
Spatio-temporal Fixation Pattern Analysis
}
\description{
"Spatio-temporal Fixation Pattern Analysis" (FPA) is a new method of analyzing eye movement data, developed by Mr. Jinlu Cao under the supervision of Prof. CHEN Hsuan-Chih at the Department of Psychology, The Chinese Univeristy of Hong Kong, and Prof. Wang Suiping at the School of Psychology, South China Normal Univeristy.The method provides a new way to inspect the spato-temporal fixation patterns of eye movements. 
}
\details{
\tabular{ll}{
Package: \tab fpa\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2016-08-13\cr
License: \tab GPL-2\cr
}
The package "fpa" is a R implementation which makes FPA analysis much easier. There are four major functions in the package: ft2fp, get_pattern, plot_pattern, and lineplot. The function ft2fp is the core function, which can complete all the preprocessing within seconds or minutes. The other three functions are supportive functions which visualize 
the eye fixation patterns.
}
\author{
Jinlu Cao

Maintainer: Jinlu Cao <caojinlu@gmail.com>
}

\keyword{ eye movements }
\keyword{ fixation probability }
\keyword{ package }
\seealso{
\code{\link{ft2fp}}, \code{\link{get_pattern}}, \code{\link{plot_pattern}}, \code{\link{lineplot}}
} 
\examples{
data(rawdata)
newdata <- ft2fp (rawdata, 4, 3000, 100)
pattern <- get_pattern(newdata)
plot_pattern(pattern)
}
