\name{eegsim}
\alias{eegsim}
\title{
Simulate Event-Related Potential EEG Data
}
\description{
Simulates event-related potential EEG data from hypothetical visual-stimulus ERP study. Data are simulated using a linear combination of five spatiotemporal component functions: P100, N100, P200, N200, and P300 components. User can control the coefficient (weight) given to each component, as well as the time shift (delay) of each component.
}
\usage{
eegsim(channel,time,coefs=rep(1,5),tshift=rep(0,5))
}
\arguments{
  \item{channel}{
Character vector of length \code{n} giving EEG channel of simulated data.
}
  \item{time}{
Numeric vector of length \code{n} giving time point of simulated data (should be in interval [0,1]).
}
  \item{coefs}{
Numeric vector of length 5 giving the coefficients (weights) to use for P100, N100, P200, N200, and P300 components (respectively).
}
  \item{tshift}{
Numeric vector of length 5 giving the time shifts (delays) to use for P100, N100, P200, N200, and P300 components (respectively).  
}
}
\value{
Returns a vector of simulated EEG data corresponding to the input channel(s), time point(s), coefficients, and time shifts.
}
\references{
Created by Nathaniel E. Helwig (2014) using data from:

  Bache, K. & Lichman, M. (2013). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.

  Begleiter, H. \emph{Neurodynamics Laboratory}. State University of New York Health Center at Brooklyn. http://www.downstate.edu/hbnl/
  
  Ingber, L. (1997). Statistical mechanics of neocortical interactions: Canonical momenta indicatros of electroencephalography. \emph{Physical Review E, 55}, 4578-4593.
  
  Ingber, L. (1998). Statistical mechanics of neocortical interactions: Training and testing canonical momenta indicators of EEG. \emph{Mathematical Computer Modelling, 27}, 33-64.
}
\author{
Nathaniel E. Helwig <helwig@umn.edu>
}
\note{
Simulates data for 39 parietal and occipital electrodes:
CP1  CP2  CP3  CP4  CP5  CP6  CPZ  I1   I2   IZ   O1   O2   OZ   
P1   P2   P3   P4   P5   P6   P7   P8   P9   P10  PO1  PO2  PO3  
PO4  PO5  PO6  PO7  PO8  PO9  PO10 POZ  PZ   TP7  TP8  TP9  TP10

Returns simulated value of 0 for other electrodes.
}
\examples{
##########   EXAMPLE   ##########

### plot spatiotemporal component functions

# data(eegcoord)
# chnames <- rownames(eegcoord)
# tseq <- seq(0,1,length.out=200)

# quartz(width=18,height=6)
# layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,11), 2, 6, byrow = TRUE))

# eegspace(eegcoord[,4:5],p1s(chnames),cex.point=1,main=expression(psi[p1]),cex.main=2,vlim=c(-3,9))
# eegtime(tseq,p1t(tseq),ylim=c(-1,1),asp=1/2,main=expression(tau[p1]),cex.main=2,
#         xlab="Time After Stimulus (sec)")
# eegspace(eegcoord[,4:5],p2s(chnames),cex.point=1,main=expression(psi[p2]),cex.main=2,vlim=c(-3,9))
# eegtime(tseq,p2t(tseq),ylim=c(-1,1),asp=1/2,main=expression(tau[p2]),cex.main=2,
#         xlab="Time After Stimulus (sec)")
# eegspace(eegcoord[,4:5],p3s(chnames),cex.point=1,main=expression(psi[p3]),cex.main=2,vlim=c(-3,9))
# eegtime(tseq,p3t(tseq),ylim=c(-1,1),asp=1/2,main=expression(tau[p3]),cex.main=2,
#         xlab="Time After Stimulus (sec)")
# eegspace(eegcoord[,4:5],n1s(chnames),cex.point=1,main=expression(psi[n1]),cex.main=2,vlim=c(-3,9))
# eegtime(tseq,n1t(tseq),ylim=c(-1,1),asp=1/2,main=expression(tau[n1]),cex.main=2,
#         xlab="Time After Stimulus (sec)")
# eegspace(eegcoord[,4:5],n2s(chnames),cex.point=1,main=expression(psi[n2]),cex.main=2,vlim=c(-3,9))
# eegtime(tseq,n2t(tseq),ylim=c(-1,1),asp=1/2,main=expression(tau[n2]),cex.main=2,
#         xlab="Time After Stimulus (sec)")
# plot(seq(-10,10),seq(-10,10),type="n",axes=FALSE,xlab="",ylab="")
# text(0,8,labels=expression(omega[p1]*" = "*psi[p1]*tau[p1]),cex=2)
# text(0,4,labels=expression(omega[n1]*" = "*psi[n1]*tau[n1]),cex=2)
# text(0,0,labels=expression(omega[p2]*" = "*psi[p2]*tau[p2]),cex=2)
# text(0,-4,labels=expression(omega[n2]*" = "*psi[n2]*tau[n2]),cex=2)
# text(0,-8,labels=expression(omega[p3]*" = "*psi[p3]*tau[p3]),cex=2)



### plot simulated data at various time points

# quartz(width=15,height=3)
# tseq <- c(50,150,250,350,450)/1000
# par(mfrow=c(1,5))
# for(j in 1:5){
#   eegspace(eegcoord[,4:5],eegsim(chnames,rep(tseq[j],87)),vlim=c(-6.8,5.5),
#            main=paste(tseq[j]*1000," ms"),cex.main=2)
# }

}
