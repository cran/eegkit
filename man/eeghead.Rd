\name{eeghead}
\alias{eeghead}
\docType{data}
\title{
Dummy Head for 3d EEG Plots
}
\description{
Contains mesh3d object of dummy head, which is used in the plotting functions \code{\link{eegcap}} and \code{\link{eegspace}}. This is a transformed (translated, rotated, and rescaled) vesion of the \code{dummyhead} object from the \code{Rvcg} package.
}
\usage{
data(eeghead)
}
\format{
  mesh3d object
}
\source{
  Created by Nathaniel E. Helwig (2014) using:

  Adler, D., Murdoch, D., and others (2014). \emph{rgl: 3D visualization device system
  (OpenGL)}. http://CRAN.R-project.org/package=rgl
  
  Schlager, S. & authors of VCGLIB. (2014). Rvcg: Manipulations of triangular meshes (smoothing, quadric edge collapse decimation, im- and export of various mesh file-formats, cleaning, etc.) based on the VCGLIB API. R packge version 0.7.1. http://CRAN.R-project.org/package=Rvcg.
}
\author{
Nathaniel E. Helwig <helwig@umn.edu>
}

\examples{
##########   EXAMPLE   ##########

data(eeghead)
shade3d(eeghead)
eeghead$material$color=rep("black",length(eeghead$material$color))
wire3d(eeghead)

}
\keyword{datasets}
