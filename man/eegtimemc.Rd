\name{eegtimemc}
\alias{eegtimemc}
\title{
Plots Multi-Channel EEG Time Course
}
\description{
Creates plot of multi-channel electroencephalography (EEG) time courses with subplots positioned according to electrode locations. User can control the plot orientation, line types, line colors, etc.
}
\usage{
eegtimemc(time,voltmat,channel,size=c(0.75,0.75),
          vadj=0.5,hadj=0.5,xlab="",ylab="",...)
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
  \item{...}{
  Optional inputs for \code{eegtime} function.
}

}
\value{
Produces plot of EEG time course with \code{NULL} return value.
}
\references{
  Bache, K. & Lichman, M. (2013). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.

  Begleiter, H. \emph{Neurodynamics Laboratory}. State University of New York Health Center at Brooklyn. 
  
  Harrell, F., Dupont, C., and Others. \emph{Hmisc: Harrell Miscellaneous}. http://CRAN.R-project.org/package=Hmisc
  
  Ingber, L. (1997). Statistical mechanics of neocortical interactions: Canonical momenta indicatros of electroencephalography. \emph{Physical Review E, 55}, 4578-4593.
  
  Ingber, L. (1998). Statistical mechanics of neocortical interactions: Training and testing canonical momenta indicators of EEG. \emph{Mathematical Computer Modelling, 27}, 33-64.
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
}

\examples{
##########   EXAMPLE   ##########

# get control ("c") data from "eegdata" data
data(eegdata)
idx=which(eegdata$group=="c")
eegdata=eegdata[idx,]

# get average 
eegmean=tapply(eegdata$voltage,list(eegdata$time,eegdata$channel),mean)

# plot time course for all electrodes
#x11(height=15,width=15)
tseq=(0:255)*1000/255
eegtimemc(tseq,eegmean,colnames(eegmean),ylim=c(-11,14))

}
