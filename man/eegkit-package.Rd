\name{eegkit-package}
\alias{eegkit-package}
\alias{eegkit}
\docType{package}
\title{Toolkit for Electroencephalography Data}
\description{
Analysis and visualization tools for electroencephalography (EEG) data. Includes functions for plotting (a) EEG caps, (b) single- and multi-channel EEG time courses, and (c) EEG spatial maps. Also includes smoothing and Independent Component Analysis functions for EEG data analysis, and a function for simulating event-related potential EEG data.
}
\details{
%\tabular{ll}{
%Package: \tab eegkit\cr
%Type: \tab Package\cr
%Version: \tab 1.0\cr
%Date: \tab 2013-08-31\cr
%License: \tab GPL (>=2)\cr
%}
The functions \code{\link{eegcap}} and \code{\link{eegcapdense}} plot EEG caps (according to 10-20 system) in two or three dimensions. The functions \code{\link{eegtime}} and \code{\link{eegtimemc}} plot EEG time courses. The function \code{\link{eegspace}} plots EEG spatial maps. The function \code{\link{eegica}} calculates temporal or spatial ICA decomposition of EEG data. The function \code{\link{eegsmooth}} performs temporal and/or spatial smoothing of EEG data. The function \code{\link{eegsim}} can be used to simulate event-related potential EEG data.
}
\author{
Nathaniel E. Helwig <helwig@umn.edu>

Maintainer: Nathaniel E. Helwig <helwig@umn.edu>
}
\references{
  Adler, D., Murdoch, D., and others (2014). \emph{rgl: 3D visualization device system
  (OpenGL)}. http://CRAN.R-project.org/package=rgl

  Bache, K. & Lichman, M. (2013). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.

  Begleiter, H. \emph{Neurodynamics Laboratory}. State University of New York Health Center at Brooklyn. http://www.downstate.edu/hbnl/

  Bell, A.J. & Sejnowski, T.J. (1995). An information-maximization approach to blind separation and blind deconvolution. \emph{Neural Computation, 7}, 1129-1159.

  Cardoso, J.F., & Souloumiac, A. (1993). Blind beamforming for non-Gaussian signals. \emph{IEE Proceedings-F, 140}, 362-370.
  
  Cardoso, J.F., & Souloumiac, A. (1996). Jacobi angles for simultaneous diagonalization. \emph{SIAM Journal on Matrix Analysis and Applications, 17}, 161-164.

  Harrell, F., Dupont, C., and Others. \emph{Hmisc: Harrell Miscellaneous}. http://CRAN.R-project.org/package=Hmisc

  Helwig, N.E. (in prep). On the relationship between FastICA and Infomax: Fast and robust fixed point algorithms for information-maximization.
  
  Helwig, N. E. (2013). \emph{Fast and stable smoothing spline analysis of variance models for large samples with applications to electroencephalography data analysis}. Unpublished doctoral dissertation. University of Illinois at Urbana-Champaign.
  
  Helwig, N.E. (2015). \emph{bigsplines: Smoothing Splines for Large Samples}. http://CRAN.R-project.org/package=bigsplines

  Helwig, N.E. (2014). \emph{ica: Independent Component Analysis}. http://CRAN.R-project.org/package=ica

  Helwig, N.E. & Hong, S. (2013). A critique of Tensor Probabilistic Independent Component Analysis: Implications and recommendations for multi-subject fMRI data analysis. \emph{Journal of Neuroscience Methods, 213}, 263-273.
  
  Helwig, N. E. and Ma, P. (in prep). Smoothing spline ANOVA for super large samples: Scalable computation via rounding parameters. 
  
  Helwig, N. E. and Ma, P. (in press). Fast and stable multiple smoothing parameter selection in smoothing spline analysis of variance models with large samples. \emph{Journal of Computational and Graphical Statistics}.

  Ingber, L. (1997). Statistical mechanics of neocortical interactions: Canonical momenta indicatros of electroencephalography. \emph{Physical Review E, 55}, 4578-4593.
  
  Ingber, L. (1998). Statistical mechanics of neocortical interactions: Training and testing canonical momenta indicators of EEG. \emph{Mathematical Computer Modelling, 27}, 33-64.
  
  Oostenveld, R., and Praamstra, P. (2001). The Five percent electrode system for high-resolution EEG and ERP measurements. \emph{Clinical Neurophysiology, 112}, 713-719.
  
  Schlager, S. & authors of VCGLIB. (2014). Rvcg: Manipulations of triangular meshes (smoothing, quadric edge collapse decimation, im- and export of various mesh file-formats, cleaning, etc.) based on the VCGLIB API. R packge version 0.7.1. http://CRAN.R-project.org/package=Rvcg.
}
\keyword{ package }
\examples{
# See eegcap, eegcapdense, eegica, eegsim, eegsmooth, eegspace, eegtime, and eegtimemc
}
