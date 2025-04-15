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

  Murdoch, D., and Adler, D. (2025). \emph{rgl: 3D Visualization Using OpenGL}. \doi{10.32614/CRAN.package.rgl}, R package version 1.3.18, http://CRAN.R-project.org/package=rgl
  
  Schlager S (2017). Morpho and Rvcg - Shape Analysis in R. In Zheng G, Li S, Szekely G (eds.), Statistical Shape and Deformation Analysis, 217-256. Academic Press. ISBN 9780128104934.
}
\author{
Nathaniel E. Helwig <helwig@umn.edu>
}

\examples{
##########   EXAMPLE   ##########

# data(eeghead)
# shade3d(eeghead)
# eeghead$material$color <- rep("black",length(eeghead$material$color))
# wire3d(eeghead)

}
\keyword{datasets}
