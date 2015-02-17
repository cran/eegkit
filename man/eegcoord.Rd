\name{eegcoord}
\alias{eegcoord}
\docType{data}
\title{
EEG Cap Coordinates (3d and 2d Projection)
}
\description{
Three-dimensional electroencephalography (EEG) electrode coordinates (measured in cm), and corresponding projection onto two-dimensional xy plane. Contains 84 scalp electrodes, as well as nose and ears.
}
\usage{
data(eegcoord)
}
\format{
  A data frame with 87 observations and the following 5 variables:
 \describe{  
  \item{x}{
x-coordinate of 3d cap (numeric).
}
  \item{y}{
y-coordinate of 3d cap (numeric).
}
  \item{z}{
z-coordinate of 3d cap (numeric).
}
  \item{xproj}{
Projected x-coordinate of 2d cap (numeric).
}
  \item{yproj}{
Projected y-coordinate of 2d cap (numeric).
}
}

Electrode channel name labels can be obtained using \code{rownames(eegcoord)}.

}
\source{
Created by Nathaniel E. Helwig (2014) using:

  Adler, D., Murdoch, D., and others (2014). \emph{rgl: 3D visualization device system
  (OpenGL)}. http://CRAN.R-project.org/package=rgl

  Oostenveld, R., and Praamstra, P. (2001). The Five percent electrode system for high-resolution EEG and ERP measurements. \emph{Clinical Neurophysiology, 112}, 713-719.
  
  Schlager, S. & authors of VCGLIB. (2014). Rvcg: Manipulations of triangular meshes (smoothing, quadric edge collapse decimation, im- and export of various mesh file-formats, cleaning, etc.) based on the VCGLIB API. R packge version 0.7.1. http://CRAN.R-project.org/package=Rvcg.
}
\author{
Nathaniel E. Helwig <helwig@umn.edu>
}

\examples{
##########   EXAMPLE   ##########

data(eegcoord)
enames <- rownames(eegcoord)
# plot3d(eegcoord[,1],eegcoord[,2],eegcoord[,3],size=10,col="green")
# text3d(eegcoord[,1],eegcoord[,2],eegcoord[,3],texts=enames,col="blue")
plot(eegcoord[,4],eegcoord[,5],cex=2,col="green",pch=19)
text(eegcoord[,4],eegcoord[,5],labels=enames,col="blue")

}
\keyword{datasets}
