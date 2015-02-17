\name{eegtime}
\alias{eegtime}
\title{
Plots Single-Channel EEG Time Course
}
\description{
Creates plot of single-channel electroencephalography (EEG) time course with optional confidence interval. User can control the plot orientation, line types, line colors, etc.
}
\usage{
eegtime(time,voltage,flipvoltage=TRUE,vlty=1,vlwd=2,vcol="blue",
        voltageSE=NULL,slty=NA,slwd=1,scol="cyan",salpha=0.65,conflevel=0.95,
        plotzero=TRUE,zlty=1,zlwd=0.5,zcol="black",xlim=NULL,ylim=NULL,
        xlab=NULL,ylab=NULL,nxtick=6,nytick=6,add=FALSE,...)
}
\arguments{
  \item{time}{
Vector of time points at which EEG was recorded.
}
  \item{voltage}{
Vector of recorded EEG voltage at each point in \code{time}.
}
  \item{flipvoltage}{
If \code{TRUE}, negative voltages are plotted upwards.
}
  \item{vlty}{
Line type for \code{voltage}.
}
  \item{vlwd}{
Line width for \code{voltage}.
}
  \item{vcol}{
Line color for \code{voltage}.
}
 \item{voltageSE}{
Vector of standard errors of EEG voltage at each point in \code{time}.
}
  \item{slty}{
Line type for \code{voltageSE}. If \code{slty=NA} (default) shaded polygons are plotted.
}
  \item{slwd}{
Line width for \code{voltageSE}. Ignored if \code{slty=NA}.
}
  \item{scol}{
Polygon or line color for \code{voltageSE}.
}
   \item{salpha}{
Transparency value for \code{voltageSE} polygon (only used if \code{slty=NA}).
}
  \item{conflevel}{
Confidence level to use for confidence intervals. Default forms 95\% CI.   
}
  \item{plotzero}{
If \code{TRUE}, horizontal reference line is plotted at 0 volts.
}
  \item{zlty}{
Line type for reference line. Ignored if \code{plotzero=FALSE}.
}
  \item{zlwd}{
Line width for reference line. Ignored if \code{plotzero=FALSE}.
}
  \item{zcol}{
Line color for reference line. Ignored if \code{plotzero=FALSE}.
}
  \item{xlim}{
Plot limits for \code{time}.  
}
  \item{ylim}{
Plot limits for \code{voltage}. 
}
  \item{xlab}{
Plot label for \code{time}.  
}
  \item{ylab}{
Plot label for \code{voltage}. 
}
  \item{nxtick}{
Approximate number of axis ticks for \code{time}.  
}
  \item{nytick}{
Approximate number of axis ticks \code{voltage}. 
}
  \item{add}{
If \code{TRUE}, lines are added to current plot; otherwise a new plot is created.  
}
  \item{...}{
  Optional inputs for \code{plot} or \code{lines} function.
}

}
\value{
Produces plot of EEG time course with \code{NULL} return value.
}
\references{
  Bache, K. & Lichman, M. (2013). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.

  Begleiter, H. \emph{Neurodynamics Laboratory}. State University of New York Health Center at Brooklyn. http://www.downstate.edu/hbnl/
  
  Ingber, L. (1997). Statistical mechanics of neocortical interactions: Canonical momenta indicatros of electroencephalography. \emph{Physical Review E, 55}, 4578-4593.
  
  Ingber, L. (1998). Statistical mechanics of neocortical interactions: Training and testing canonical momenta indicators of EEG. \emph{Mathematical Computer Modelling, 27}, 33-64.
}
\author{
Nathaniel E. Helwig <helwig@umn.edu>
}
\note{
Confidence intervals are formed using the normal (Gaussian) distribution.
}

\examples{
##########   EXAMPLE   ##########

# get "PZ" electrode from "eegdata" data
data(eegdata)
idx <- which(eegdata$channel=="PZ")
eegdata <- eegdata[idx,]

# get average and standard error (note se=sd/sqrt(n))
eegmean <- tapply(eegdata$voltage,list(eegdata$time,eegdata$group),mean)
eegse <- tapply(eegdata$voltage,list(eegdata$time,eegdata$group),sd)/sqrt(50) 

# plot results with legend
tseq <- seq(0,1000,length.out=256)
eegtime(tseq,eegmean[,2],voltageSE=eegse[,2],ylim=c(-10,6),main="Pz")
eegtime(tseq,eegmean[,1],vlty=2,vcol="red",voltageSE=eegse[,1],scol="pink",add=TRUE)
legend("bottomright",c("controls","alcoholics"),lty=c(1,2),
       lwd=c(2,2),col=c("blue","red"),bty="n")

}
