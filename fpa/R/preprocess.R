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



# exempt from R check because those variables are from data frame input by user
condition <- region <- Region <- Item <- Condition <- Subject <- List <- subject <- NULL

#' The ft2fp function transforms the fixation time (start and end time for each fixation)
#' data to fixation probability data. The function can finish all the preprocessing of using
#' FPA to analyze eye movement data. Users should provide the fixation time data, the
#' critical region number, the intended time course of interest, and the intended time interval.
#' The user can also adjust the results by input other requests like if the user wants to
#' normalize fixation duration, if the user wants to exclude the trials with no regression,
#' and if the user wants to exclude the information on first pass.
#'
#' @title Convert fixation time to fixation probability
#' @param data is the raw eye movement data provided by user. It is a data frame which contains
#' variables of "List", "Subject", "Item", "Condition", "Region", "Fix_Start", and "Fix_End". The
#' names and number of variables in your data should be exactly same with above.
#' @param CriticalRegion is the No. of region in which the researcher is interested. All fixation
#' information before the first-pass on that region will be discarded for each trial.
#' @param TimeCourse is the time course to be analyzed after the first-pass of critical region. The
#' unit is millisecond.
#' @param Interval is the time interval (or bin) to show in the time course of interest. The
#' unit is millisecond, and the value should be smaller than the value for TimeCourse.
#' @param norm is to choose whether to normalize the fixation duration according to each
#' subject's mean duration and general mean duration. When norm is TRUE, the fixation durations
#' are adjusted for each subject's reading rate. The default value if TRUE.
#' @param rm.nr is to choose whether to exclude the trials with no regression after the
#' first-pass on critical region. The default value is FALSE.
#' @param rm.1p is to choose whether to exclude the fixations at the first pass (or Gaze
#' duration) on critical region. The default value is TRUE.
#' @return a data frame with the variables of "list", "subject", "condition", "region", "time",
#' "fix_prob" (fixation probability), "y" (number of trials with fixation) and "N" (number of total
#' valid trials).
#' @export
#' @importFrom reshape melt
#' @importFrom reshape cast
#' @examples
#' data(rawdata)
#' newdata <- ft2fp (rawdata, 4, 3000, 100)
#' newdata <- ft2fp (rawdata, 4, 3000, 100, norm=TRUE, rm.nr=TRUE, rm.1p=FALSE)


ft2fp <- function (data, CriticalRegion, TimeCourse, Interval, norm=TRUE, rm.nr=FALSE, rm.1p=TRUE) {

  # clean up the missing values
    data <- na.omit(data)

  # keep the data in order
    data <- data[order(data$Subject, data$Condition, data$Item, data$Fix_Start),]

  # relabel the trials so that they will not repeat
    data$trial[1] <- 1
    for (j in 2:nrow(data)) {
        if (data$Item[j] == data$Item[j-1])
            data$trial[j] <- data$trial[j-1]
        else
            data$trial[j] <- data$trial[j-1] + 1

    }
    rm(j)


    data2 <- data.frame()
    for (k in levels(factor(data$trial))) {
        dat <- subset(data, data$trial == k)

      # exclude prior regions
        if (CriticalRegion >= min(data$Region) && CriticalRegion <= max(data$Region)){
            dat$mark <- 1
            for (m in 1:nrow(dat)) {
                if (max(dat$Region[1:m]) < CriticalRegion)
                    dat$mark[m] <- 0
            }
            dat <- dat[dat$mark == 1,]
        }

      # exclude first-pass
        if (nrow(dat) != 0 && rm.1p==TRUE) {
            dat$mark <- 0
            for (n in 1:nrow(dat)) {
                if (min(dat$Region[1:n]) < CriticalRegion || max(dat$Region[1:n]) > CriticalRegion)
                    dat$mark[n] <- 1
            }
            dat <- dat[dat$mark == 1,]
        }

      # exclude trials with no regression after first-pass on critical region
        if (nrow(dat) != 0 && rm.nr==TRUE) {
            if (dat$Region[1] > CriticalRegion) dat <- NULL
        }

      # re-align the data to new start time
        StartTime <- dat$Fix_Start[1]
        dat$Fix_Start <- dat$Fix_Start - StartTime
        dat$Fix_End <- dat$Fix_End - StartTime

        data2 <- rbind(data2, dat)
        rm(dat)
    }
    rm(k,m,n,StartTime)
    data2$mark <- NULL



    # normalize the start and end time for each fixation

    if (norm==TRUE) {
        data2 <- .normalizer(data2) # use the private function .normalizer
    }

  # calculate the fixation probabilities
    for (t in 1:(as.integer(TimeCourse/Interval)+1)) {
        time <- (t - 1) * Interval
        time_var <- as.character(time)
        fix_status <- vector()
        for (i in 1:nrow(data2)) {
            fix_status[i] <- as.integer(data2$Fix_Start[i] <= time && time <= data2$Fix_End[i])
        }
        data2[[time_var]] <- fix_status
    }
    rm(t, time, time_var, i, fix_status)
    
    dataY <- aggregate(data2, by=list(region = data2$Region, item = data2$Item, condition = data2$Condition, subject = data2$Subject, list = data2$List), FUN=sum, na.rm=TRUE)
    dataY$Region <- dataY$Item <- dataY$Condition <- dataY$Subject <- dataY$List <- dataY$trial <- dataY$Fix_Start <- dataY$Fix_End <- NULL

  # re-arrange the data to make it easier for analysis
    dataY <- melt(dataY, id=(c("list","subject","condition", "item", "region")))

    dataYN <- data.frame()
    for (subj in levels(factor(dataY$subject))) {
        for (cond in levels(factor(dataY$condition))) {
            dat <- subset(dataY, subject == subj & condition == cond)
            if (nrow(dat)>= 1) {
                dat$N <- as.integer(length(levels(factor(dat$item))))
                dataYN <- rbind(dataYN, dat)
                rm(dat)
            }
        }
    }
    rm(dataY,subj)

    dataYN$item <- NULL

  # aggragate by the trial dimension
    dataYN <- cast(dataYN, list + subject + condition + region + N ~ variable, sum)


  # make up the unlooked regions (else underestimation of some regions will occur)
    full <- seq(1,max(dataYN$region),1)
    datayN <- data.frame()
    for (subj in levels(factor(dataYN$subject))) {
        for (cond in levels(factor(dataYN$condition))) {
            dat <- subset(dataYN, subject == subj & condition == cond)
            if (nrow(dat) >= 1) {
                count <- length(full) - nrow(dat)
                if (count >= 1) {
                    dat[(nrow(dat)+1):(nrow(dat)+count),] <- dat[1,]
                    dat$region[(nrow(dat)-count+1):nrow(dat)] <- full[! full %in% dat$region]
                    dat[(nrow(dat)-count+1):nrow(dat),-1:-5] <- 0
                }
            }
           datayN <- rbind(datayN, dat)
           rm(dat)
        }
    }
    rm(dataYN,subj,count)

    datayN <- as.data.frame(datayN)
    datayN <- melt(datayN, id=(c("list","subject","condition", "region", "N")))
    datayN <- rename(datayN, c(variable = "Time", value = "y"))
    datayN <- datayN[order(datayN$Time, datayN$subject, datayN$condition, datayN$region),]
    datayN$fix_prob <- datayN$y/datayN$N

    return(datayN)

}


#' The get_pattern function aggragates the data so that the general fixation pattern can be shown
#' for each condition. Users should provide the data frame returned in ft2fp function. Users can
#' use the returned data frame of this function to make plots on the pattern by themselves.
#'
#' @title Get the general fixation pattern
#' @param data is the data frame returned by the ft2fp function.
#' @return a data frame which shows the avraged fixation probabilities for each spatio-temporal unit
#' for each condition.
#' @export
#' @importFrom reshape cast
#' @examples
#' data(newdata)
#' pattern <- get_pattern(newdata)

get_pattern <- function (data) {

    data <- data[,-1]
    data$y <- data$N <- data$list <- NULL

    pattern <- cast(data, condition + region ~ Time, mean, value='fix_prob')

    return(pattern)

}


# private function for normalizing fixation durations for ft2fp function
.normalizer <- function (data2) {
  # calculate fixation durations
    data2$Fix_Dur <- data2$Fix_End - data2$Fix_Start
  # calculate grand mean of fixation durations
    GrandMean <- mean(data2$Fix_Dur)

    data3 <- data.frame()
    for (subject in levels(factor(data2$Subject))) {
        dat <- subset(data2, data2$Subject == subject)
        SubjectMean <- mean(dat$Fix_Dur)
      # calculated the adjusted (normalized) fixation durations
        dat$Norm_Fix_Dur <- dat$Fix_Dur*(GrandMean/SubjectMean)
        data3 <- rbind(data3, dat)
        rm(dat)
    }

    data4 <- data.frame()
    for (y in levels(factor(data3$trial))) {
        dat <- subset(data3, data3$trial == y)

      # if there is only one fixation in that trial
        if (nrow(dat) >= 1) {
            dat$Norm_Fix_Start <- 0
            dat$Norm_Fix_End <- dat$Norm_Fix_Dur
            dat$Saccade_Dur <- 0
        }

      # if there are more than one fixations in that trial
        if (nrow(dat) >= 2) {
            for (fix in 2:nrow(dat)) {
              # calculate the adjusted (normalized) start and end time for each fixation
                dat$Saccade_Dur[fix] <- dat$Fix_Start[fix] - dat$Fix_End[fix - 1]
                dat$Norm_Fix_Start[fix] <- dat$Norm_Fix_End[fix - 1] + dat$Saccade_Dur[fix]
                dat$Norm_Fix_End[fix] <- dat$Norm_Fix_Start[fix] + dat$Norm_Fix_Dur[fix]
            }
        }

        data4 <- rbind(data4, dat)
        rm(dat)
    }
    rm(subject,y,GrandMean, SubjectMean)

    data4$Fix_Start <- data4$Fix_End <- data4$Fix_Dur <- data4$Norm_Fix_Dur <- data4$Saccade_Dur <- NULL
    data4 <- rename(data4, c(Norm_Fix_Start="Fix_Start", Norm_Fix_End="Fix_End"))
    data2 <- data4
    rm(data3,data4)

    return(data2)
}
