#' This package is built for the spatio-temporal fixation probability analysis (FPA)
#' for eye movement data. The method serves to provide the spatio-temporal pattern
#' in eye movement experiments.
#'
#' @name fpa
#' @docType package
#' @title Conversion from fixation time to fixation probability
#' @author Cao Jinlu \email{caojinlu@gmail.com}
#' @keywords eye movements fixation probability R package
#' @seealso \code{\link{ft2fp}}, \code{\link{get_pattern}}, \code{\link{plot_pattern}}, \code{\link{lineplot}}

 
 

# exempt from R check because those variables from data frame input by user
condition <- region <- Region <- Item <- Condition <- Subject <- List <- subject <- NULL
 
#' The plot_pattern function provides a quick tool to plot the fixation pattern for 
#' conditions. The generated plots 3-dimensional data, with x of "Time", y of "Region",
#' and the colors representing the value of fixation probabilities. 
#' 
#' @title Plot the general fixation pattern
#' @param data is the data frame returned by the get_pattern function.
#' @param Condition is the conditions which the user would like to plot. It can be a 
#' string ("All"), a number (e.g., 1), or a vector (e.g., c(1,2)). The default value
#' is "All", meaning all conditions will be plotted.
#' @export
#' @importFrom fields image.plot
#' @examples 
#' data(pattern)
#' plot_pattern(pattern)
#' plot_pattern(pattern, Condition=1)
#' plot_pattern(pattern, Condition=c(1,2))
#' 

plot_pattern <- function (data, Condition="All") {
  # get the maximum value for fixation probability 
    zmax <- max(data[,-1:-2])
  
  # get other important information for plots
    TimeCourse <- as.integer(names(data)[ncol(data)])
    TimeInterval <- TimeCourse/(ncol(data)-3)
    nregion <- max(data$region)
    
  # if the input is "All" or no input for Condition 
    if (Reduce("&",Condition == "All")) {
      
      #determine the number of subplots and set up the general picture
        ncols <- length(levels(factor(data$condition)))/2
        if (!is.integer(ncols)) ncols <- ncols + 0.5 
        par(mfrow=c(ncols,2))
      
      # subset data and plot
        for (i in levels(factor(data$condition))) {
          cond <- subset(data, condition == i)
          cond <- as.matrix(cond[,-1:-2])
          image.plot(seq(0, TimeCourse, TimeInterval), seq(1,nregion,1), t(cond), zlim=c(0,zmax), 
               xlab = "Time", ylab = "Region",main=paste("Condition",i))
        }
    }
    
  # if the input is a number or a vector
    else if (class(Condition)=="numeric") {
      
      # if the input is a number (one condition)
        if(length(Condition)==1) {
            cond <- subset(data, condition == Condition)
            cond <- as.matrix(cond[,-1:-2])
            image.plot(seq(0, TimeCourse, TimeInterval), seq(1,nregion,1), t(cond), zlim=c(0,zmax), 
                       xlab = "Time", ylab = "Region", main=paste("Condition",Condition))
        }
      # if the input is a vector (more than one condition)
        else if (length(Condition)>=2){
            ncols <- length(Condition)/2
            if (!is.integer(ncols)) ncols <- ncols + 0.5 
            par(mfrow=c(ncols,2))
            for (i in Condition) {
                cond <- subset(data, condition == i)
                cond <- as.matrix(cond[,-1:-2])
                image.plot(seq(0, TimeCourse, TimeInterval), seq(1,nregion,1), t(cond), zlim=c(0,zmax), 
                           xlab = "Time", ylab = "Region",main=paste("Condition",i))
            } 
        }
    }
    else stop("Bad input of data or 'Condition'.")
} 





#' The function lineplot provides quick tools for plotting more detailed fixation 
#' probabilities for specific condition(s) and region(s). The function generates
#' 2-dimensional line plots with "Time" as x, and "Fixation Probability" as y. 
#' 
#' @title Plot the fixation probabilities for specific details
#' @param data is the data frame returned by get_pattern function.
#' @param Region is the intended region(s) to plot. It can be a string ("All"), a 
#' number (e.g., 1), or a vector (e.g., c(1,2)).
#' @param Condition is the intended condition(s) to plot.It can be a string ("All"), 
#' a number (e.g., 1), or a vector (e.g., c(1,2)). 
#' @export
#' @examples 
#' data(pattern)
#' lineplot(pattern)
#' lineplot(pattern, Region="All", Condition=1)
#' lineplot(pattern, Condition=c(1,2))
#' lineplot(pattern, Region=2)
#' lineplot(pattern, Region=c(2,3), Condition=c(3,4,5))
#' 
lineplot <- function (data, Region="All", Condition="All") {
    
  # get the needed values from data
    TimeCourse <- as.integer(names(data)[ncol(data)])
    TimeInterval <- TimeCourse/(ncol(data)-3)
    ymax <- max(data[,-1:-2])
    
  # if no input or "All" for Region parameter
    if (Reduce("&", Region == "All")) {
      
    # set up the subplotting layout
      ncols <- length(levels(factor(data$region)))/2
      if (!is.integer(ncols)) ncols <- ncols + 0.5 
      par(mfrow=c(ncols,2))
      
    # if no input or "All" for Condition parameter 
      if (Reduce("&",Condition == "All")) {
          for (j in levels(factor(data$region))) {
              reg <- subset(data, region == j)
              reg <- reg[,-1:-2]
              .linetemplate(TimeCourse, TimeInterval,t(reg),j,levels(factor(data$condition)))
          }
      }
      
      else if (class(Condition)=="numeric") {
        # if single number input for Condition parameter 
          if(length(Condition)==1) {
              for (j in levels(factor(data$region))) {
                  reg <- subset(data, region == j)
                  reg <- subset(reg, condition==Condition)
                  reg <- reg[,-1:-2]
                  .linetemplate(TimeCourse, TimeInterval,t(reg),j,levels(factor(data$condition)))
                  }
          }
        # if vector input for Condition parameter 
          else if (length(Condition)>=1) {
              reg2 <- .subsetNcond(data,Condition)  
              for (j in levels(factor(data$region))) {
                  reg <- subset(reg2, region == j)
                  reg <- reg[,-1:-2]
                  .linetemplate(TimeCourse, TimeInterval,t(reg),j,Condition)
                  }
              } 
          }
    }
    
    else if (class(Region)=="numeric"){
      # if single number input for Region parameter
        if (length(Region)==1) {
            reg2 <- subset(data, region==Region)
            # if no input for "All" for Condition parameter
            if (Reduce("&",Condition == "All")) {
                reg <- reg2[,-1:-2]
                .linetemplate(TimeCourse, TimeInterval,t(reg),Region,levels(factor(data$condition)))
            }
            # if single number input for Condition parameter
            else if (length(Condition)==1) {
                reg <- subset(reg2, condition==Condition)
                reg <- reg[,-1:-2]
                .linetemplate(TimeCourse, TimeInterval,t(reg),Region,Condition)
            }
            # if vector input for Condition parameter
            else if (length(Condition>=1)) {
                reg <- .subsetNcond(reg2,Condition)
                reg <- reg[,-1:-2]
                .linetemplate(TimeCourse, TimeInterval,t(reg),Region,Condition)
            }
        }
      
      # if vector input for Region parameter
        else if (length(Region>=1)) {
            ncols <- length(Region)/2
            if (!is.integer(ncols)) ncols <- ncols + 0.5 
            par(mfrow=c(ncols,2))
            
            reg2 <- .subsetNreg(data, Region)
            
            # if no input for "All" for Condition parameter
            if (Reduce("&",Condition == "All")) {
              for (n in 1:length(Region)) {
                reg <- subset(reg2, region==Region[n])
                reg3 <- reg[,-1:-2]
                .linetemplate(TimeCourse, TimeInterval,t(reg3),Region[n],levels(factor(data$condition)))
              }
            }
            # if single number input for Condition parameter
            else if (length(Condition)==1) {
              reg <- subset(reg2, condition==Condition)
              for (n in 1:length(Region)) {
                regg <- subset(reg, region==Region[n])
                reg3 <- regg[,-1:-2]
                .linetemplate(TimeCourse, TimeInterval,t(reg3),Region[n],Condition)
                }
            }
            # if vector input for Condition parameter
            else if (length(Condition>=1)) {
              reg <- .subsetNcond(reg2,Condition) 
              for (n in 1:length(Region)) {
                  reg2 <- subset(reg, region==Region[n])
                  reg3 <- reg2[,-1:-2]
                  .linetemplate(TimeCourse, TimeInterval,t(reg3),Region[n],Condition)
              }
            }
        }
    }
    else stop("Bad input of arguments.")
}


# private function: a template for drawing lines 
.linetemplate <- function(TimeCourse, TimeInterval,data,RegionNum,ConditionNum) {
    matplot(x=seq(0,TimeCourse,TimeInterval),data, type="l",pch=1, col=1:nrow(data),
            xlab = "Time", ylab = "Fixation Probability", main = paste("Region", RegionNum))
    legend("topright", legend = paste("Condition", ConditionNum), 
           col=1:nrow(data), pch=1)    
}

# private function: a function to subset when more than one conditions for subsetting needed
.subsetNcond <- function(Rawdata, Conditions) {
    newdata <- data.frame()  
    for (i in Conditions) {
        dat <- subset(Rawdata, condition == i)
        newdata <- rbind(newdata, dat)
    }
    return(newdata)
}

# private function: a function to subset when more than one conditions for subsetting needed
.subsetNreg <- function(Rawdata, Regions) {
  newdata <- data.frame()
  for (i in Regions) {
    dat <- subset(Rawdata, region == i)
    newdata <- rbind(newdata, dat)
  }
  return(newdata)
}