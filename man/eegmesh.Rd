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

  Murdoch, D., and Adler, D. (2025). \emph{rgl: 3D Visualization Using OpenGL}. \doi{10.32614/CRAN.package.rgl}, R package version 1.3.18, http://CRAN.R-project.org/package=rgl
  
  Oostenveld, R., and Praamstra, P. (2001). The Five percent electrode system for high-resolution EEG and ERP measurements. \emph{Clinical Neurophysiology, 112}, 713-719. \doi{10.1016/S1388-2457(00)00527-7}
  
  Schlager S (2017). Morpho and Rvcg - Shape Analysis in R. In Zheng G, Li S, Szekely G (eds.), Statistical Shape and Deformation Analysis, 217-256. Academic Press. ISBN 9780128104934.
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
