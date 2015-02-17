\name{eegmesh}
\alias{eegmesh}
\docType{data}
\title{
EEG Cap for Dense Coordinates
}
\description{
Contains mesh3d object of \code{\link{eegdense}}, which is used in the plotting function \code{\link{eegspace}}.
}
\usage{
data(eegmesh)
}
\format{
  mesh3d object
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

# data(eegmesh)
# wire3d(eegmesh)
# eegmesh$material$color <- rep("red",length(eegmesh$material$color))
# shade3d(eegmesh)

}
\keyword{datasets}
