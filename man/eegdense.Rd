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

  Murdoch, D., and Adler, D. (2025). \emph{rgl: 3D Visualization Using OpenGL}. \doi{10.32614/CRAN.package.rgl}, R package version 1.3.18, http://CRAN.R-project.org/package=rgl
  
  Oostenveld, R., and Praamstra, P. (2001). The Five percent electrode system for high-resolution EEG and ERP measurements. \emph{Clinical Neurophysiology, 112}, 713-719. \doi{10.1016/S1388-2457(00)00527-7}
  
  Schlager S (2017). Morpho and Rvcg - Shape Analysis in R. In Zheng G, Li S, Szekely G (eds.), Statistical Shape and Deformation Analysis, 217-256. Academic Press. ISBN 9780128104934.
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

