\name{eegsmooth}
\alias{eegsmooth}
\title{
Spatial and/or Temporal Smoothing of EEG Data
}
\description{
Smooths single- or multi-channel electroencephalography (EEG) with respect to space and/or time. Uses the \code{\link[bigsplines]{bigspline}}, \code{\link[bigsplines]{bigtps}}, and \code{\link[bigsplines]{bigssa}} functions (from \code{bigsplines} package) for smoothing.
}
\usage{
eegsmooth(voltage, space = NULL, time = NULL, nknots = NULL,
          rparm = NULL, lambdas = NULL, skip.iter = TRUE,
          se.fit = FALSE, rseed = 1234)
}
\arguments{
  \item{voltage}{
Vector of recorded EEG voltage at each row in \code{space}.
}
  \item{space}{
Matrix of electrode coordinates (in three-dimensions) at which EEG was recorded. If \code{space=NULL}, data are temporally smoothed only.
}
  \item{time}{
Vector of time points at which EEG was recorded. If \code{time=NULL}, data are spatially smoothed only.
}
  \item{nknots}{
Number of knots to sample for smoothing. Positive integer.
}
  \item{rparm}{
Rounding parameter(s) to use for smoothing. See Notes and Examples.
}
  \item{lambdas}{
Smoothing parameter(s) to use for smoothing.
}
  \item{skip.iter}{
If \code{FALSE}, iterative spatial-temporal smoothing is skipped. Ignored if \code{space=NULL} or \code{time=NULL}.
}
  \item{se.fit}{
If \code{TRUE}, standard errors of smoothed values are calculated.
}
  \item{rseed}{
Random seed to use for knot selection. Set \code{rseed=NULL} to obtain different knots each time, or set \code{rseed} to any positive integer to use a different random seed.
}
}
\value{
For temporal smoothing only: an object of class "bigspline" (see \code{\link[bigsplines]{bigspline}}).

For spatial smoothing only: an object of class "bigtps" (see \code{\link[bigsplines]{bigtps}}).

For spatial-temporal smoothing: an object of class "bigssa" (see \code{\link[bigsplines]{bigssa}}).

}
\references{
  Helwig, N. E. (2013). \emph{Fast and stable smoothing spline analysis of variance models for large samples with applications to electroencephalography data analysis}. Unpublished doctoral dissertation. University of Illinois at Urbana-Champaign. https://www.ideals.illinois.edu/items/44427
  
  Helwig, N.E. (2015). \emph{bigsplines: Smoothing Splines for Large Samples}. \doi{10.32614/CRAN.package.bigsplines}, R package version
  1.1-1, https://CRAN.R-project.org/package=bigsplines
  
  Helwig, N. E. & Ma, P. (2015). Fast and stable multiple smoothing parameter selection in smoothing spline analysis of variance models with large samples. \emph{Journal of Computational and Graphical Statistics, 24}(3), 715-732. \doi{10.1080/10618600.2014.926819}
  
  Helwig, N. E. & Ma, P. (2016). Smoothing spline ANOVA for super large samples: Scalable computation via rounding parameters. \emph{Statistics and Its Interface, 9}(4), 433-444. \doi{10.4310/SII.2016.v9.n4.a3}
}
\author{
Nathaniel E. Helwig <helwig@umn.edu>
}
\note{
For temporal smoothing only (i.e., \code{space=NULL}), the input \code{rparm} should be a positive scalar less than 1. Larger values produce faster (but less accurate) approximations. Default is 0.01, which I recommend for temporal smoothing; \code{rparm=0.005} may be needed for particuarly rough signals, and \code{rparm=0.02} could work for smoother signals.

For spatial smoothing only (i.e., \code{time=NULL}), the input \code{rparm} should be a positive scalar giving the rounding unit for the spatial coordinates. For example, \code{rparm=0.1} rounds each coordinate to the nearest 0.1 (same as \code{round(space,1)}).

For spatial-temporal smoothing (i.e., both \code{space} and \code{time} are non-null), the input \code{rparm} should be a list of the form \code{rparm=list(space=0.1,time=0.01)}, where the 0.1 and 0.01 can be replaced by your desired rounding parameters.

Setting \code{rparm=NA} will use the full data solution; this is more computationally expensive, and typically produces a solution very similar to using \code{rparm=0.01} (see references).

}

\examples{
##########   EXAMPLE 1: Temporal    ##########

# get "PZ" electrode of "c" subjects in "eegdata" data
data(eegdata)
idx <- which(eegdata$channel=="PZ" & eegdata$group=="c")
eegdata <- eegdata[idx,]

# temporal smoothing
eegmod <- eegsmooth(eegdata$voltage,time=eegdata$time)

# define data for prediction
time <- seq(min(eegdata$time),max(eegdata$time),length.out=100)
yhat <- predict(eegmod,newdata=time,se.fit=TRUE)

# plot results using eegtime
eegtime(time*1000/255,yhat$fit,voltageSE=yhat$se.fit,ylim=c(-4,4),main="Pz")



##########   EXAMPLE 2: Spatial   ##########

# get time point 65 (approx 250 ms) of "c" subjects in "eegdata" data
data(eegdata)
idx <- which(eegdata$time==65L & eegdata$group=="c")
eegdata <- eegdata[idx,]

# remove ears, nose, and reference (Cz)
idx <- c(which(eegdata$channel=="X"),which(eegdata$channel=="Y"),
         which(eegdata$channel=="nd"),which(eegdata$channel=="Cz"))
eegdata <- eegdata[-idx,]

# match to eeg coordinates
data(eegcoord)
cidx <- match(eegdata$channel,rownames(eegcoord))

# spatial smoothing
eegmod <- eegsmooth(eegdata$voltage,space=eegcoord[cidx,1:3])

# use dense cap for prediction
mycap <- levels(factor(eegdata$channel))
ix <- eegcapdense(mycap,type="2d",index=TRUE)
data(eegdense)
space <- eegdense[ix,1:3]
yhat <- predict(eegmod,newdata=space)

# plot results using eegspace
#eegspace(space,yhat)
eegspace(eegdense[ix,4:5],yhat)



##########   EXAMPLE 3: Spatial-Temporal (not run)   ##########

# # get "c" subjects of "eegdata" data
# data(eegdata)
# idx <- which(eegdata$group=="c")
# eegdata <- eegdata[idx,]

# # remove ears, nose, and reference (Cz)
# idx <- c(which(eegdata$channel=="X"),which(eegdata$channel=="Y"),
#          which(eegdata$channel=="nd"),which(eegdata$channel=="Cz"))
# eegdata <- eegdata[-idx,]

# # match to eeg coordinates
# data(eegcoord)
# cidx <- match(eegdata$channel,rownames(eegcoord))

# # spatial-temporal smoothing
# eegmod <- eegsmooth(eegdata$voltage,space=eegcoord[cidx,1:3],time=eegdata$time)

# # time main effect
# newdata <- list(time=seq(min(eegdata$time),max(eegdata$time),length.out=100))
# yhat <- predict(eegmod,newdata=newdata,se.fit=TRUE,include="time")
# eegtime(newdata$time,yhat$fit,voltageSE=yhat$se.fit,ylim=c(-2,4),main="Time Main Effect")

# # space main effect
# mycap <- levels(factor(eegdata$channel))
# ix <- eegcapdense(mycap,type="2d",index=TRUE)
# data(eegdense)
# newdata <- list(space=eegdense[ix,1:3])
# yhat <- predict(eegmod,newdata=newdata,include="space")
# eegspace(newdata$space,yhat)

# # interaction effect (spatial map at time point 65)
# newdata <- list(space=eegdense[ix,1:3],time=rep(65,nrow(eegdense[ix,])))
# yhat <- predict(eegmod,newdata=newdata,include="space:time")
# eegspace(newdata$space,yhat)

# # full prediction (spatial map at time point 65)
# newdata <- list(space=eegdense[ix,1:3],time=rep(65,nrow(eegdense[ix,])))
# yhat <- predict(eegmod,newdata=newdata)
# eegspace(newdata$space,yhat)

}
