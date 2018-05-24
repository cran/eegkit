\name{eegdense}
\alias{eegdense}
\docType{data}
\title{
Dense EEG Cap Coordinates
}
\description{
Dense (hypothetical) three-dimensional electroencephalography (EEG) electrode coordinates, and corresponding projection onto two-dimensional plane. Dense cap spans the 84 scalp electrodes defined in \code{\link{eegcoord}}.
}
\usage{
data(eegdense)
}
\format{
  A data frame with 977 observations and the following 5 variables:
  
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
}}
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

data(eegdense)
# plot3d(eegdense[,1],eegdense[,2],eegdense[,3],size=10,col="green")
plot(eegdense[,4],eegdense[,5],cex=1,col="green",pch=19)

}
\keyword{datasets}

