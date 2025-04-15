\name{eegtimemc}
\alias{eegtimemc}
\title{
Plots Multi-Channel EEG Time Course
}
\description{
Creates plot of multi-channel electroencephalography (EEG) time courses with subplots positioned according to electrode locations. User can control the plot orientation, line types, line colors, etc.
}
\usage{
eegtimemc(time, voltmat, channel, size = c(0.75,0.75),
          vadj = 0.5, hadj = 0.5, xlab = "", ylab = "",
          voltSE = NULL, vlty = 1, slty = NA, vlwd = 1,
          slwd = 1, vcol = "blue", scol = "cyan", ...)
}
\arguments{
  \item{time}{
Vector of time points at which EEG was recorded.
}
  \item{voltmat}{
Matrix of multi-channel EEG voltages (time by channel). 
}
  \item{channel}{
Character vector giving name of channel for each column of \code{voltmat}. 
}
  \item{size}{
Relative size of each subplot.
}
  \item{vadj}{
Vertical adjustment for each subplot.
}
  \item{hadj}{
Horizontal adjustment for each subplot.
}
  \item{xlab}{
X-axis label for each subplot.
}
  \item{ylab}{
Y-axis label for each subplot.
}
  \item{voltSE}{
Matrix of voltage standard errors (same size as \code{voltmat}).
}
  \item{vlty}{
Line type for \code{voltmat}.
}
  \item{slty}{
Line type for \code{voltSE}. If \code{slty=NA} (default) shaded polygons are plotted.
}
  \item{vlwd}{
Line width for \code{voltmat}.
}
  \item{slwd}{
Line width for \code{voltSE}. Ignored if \code{slty=NA}.
}
  \item{vcol}{
Line color for \code{voltmat}.
}
  \item{scol}{
Polygon or line color for \code{voltSE}.
}
  \item{...}{
  Optional inputs for \code{eegtime} function.
}

}
\value{
Produces plot of EEG time course with \code{NULL} return value.
}
\references{
  Begleiter, H. (1995). EEG Database [Dataset]. UCI Machine Learning Repository. \doi{10.24432/C5TS3D}
  
  Harrell Jr F (2025). Hmisc: Harrell Miscellaneous. \doi{10.32614/CRAN.package.Hmisc}, R package version 5.2-3,
  https://CRAN.R-project.org/package=Hmisc
}
\author{
Nathaniel E. Helwig <helwig@umn.edu>
}
\note{
Currently supports 84 scalp electrodes (plus ears and nose):
A1   A2   AF1  AF2  AF3  AF4  AF5  AF6  AF7  AF8  AFZ  C1   C2   C3   C4   
C5   C6   CP1  CP2  CP3  CP4  CP5  CP6  CPZ  CZ   F1   F2   F3   F4   F5   
F6   F7   F8   F9   F10  FC1  FC2  FC3  FC4  FC5  FC6  FCZ  FP1  FP2  FPZ  
FT7  FT8  FT9  FT10 FZ   I1   I2   IZ   NZ   O1   O2   OZ   P1   P2   P3   
P4   P5   P6   P7   P8   P9   P10  PO1  PO2  PO3  PO4  PO5  PO6  PO7  PO8  
PO9  PO10 POZ  PZ   T7   T8   T9   T10  TP7  TP8  TP9  TP10

Subplots are created using \code{eegtime}, so input \code{...} can be any optional input for \code{eegtime}.

Inspired by Frank Harrell's subplot function (in Hmisc package).
}

\examples{
##########   EXAMPLE   ##########

# # get control ("c") data from "eegdata" data
# data(eegdata)
# idx <- which(eegdata$group=="c")
# eegdata <- eegdata[idx,]

# # get average 
# eegmean <- tapply(eegdata$voltage,list(eegdata$time,eegdata$channel),mean)
# eegse <- tapply(eegdata$voltage,list(eegdata$time,eegdata$channel),sd)/sqrt(50)

# # plot time course for all electrodes
# dev.new(height=15,width=15, noRStudioGD = TRUE)
# tseq <- seq(0,1000,length.out=256)
# eegtimemc(tseq,eegmean,colnames(eegmean),ylim=c(-11,14),voltSE=eegse)

}
