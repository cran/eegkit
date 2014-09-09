\name{eegcap}
\alias{eegcap}
\title{
Draws EEG cap with selected electrodes (3d or 2d)
}
\description{
Creates two- or three-dimensional plot of electroencephalography (EEG) cap with user-input electrodes. Three-dimensional plots are created using the \code{\link{eegcoord}} data and the \code{\link[rgl]{plot3d}} function (from \code{rgl} package). Currently supports 84 scalp electrodes, and plots according to the international 10-10 system. Includes customization options (e.g., each electrode can have a unique plotting color, size, label color, etc.).
}
\usage{
eegcap(electrodes="10-10",type=c("3d","2d"),plotlabels=TRUE,
       plotaxes=FALSE,main="",xyzlab=NULL,cex.point=NULL,
       col.point=NULL,cex.label=NULL,col.label=NULL,nose=TRUE,
       ears=TRUE,head=TRUE,col.head="AntiqueWhite",...)
}
\arguments{
  \item{electrodes}{
Character vector with electrodes to plot. Each element of \code{electrodes} must match one of the 89 reference electrodes (see Notes). Mismatches are ignored (not plotted). Input is NOT case sensitive. Default plots all available electrodes (full 10-10 system).
}
  \item{type}{
Type of plot to create: \code{type="3d"} produces three-dimensional plot, whereas \code{type="2d"} produces two-dimensional projection plot (bird's eye view).
}
  \item{plotlabels}{
If \code{TRUE}, the electrode labels are plotted.
}
  \item{plotaxes}{
If \code{TRUE}, the axes are plotted.
}
  \item{main}{
Title to use for plot. Default is no title
}
    \item{xyzlab}{
Axis labels to use for plot. If \code{type="2d"}, then \code{xyzlab} should be two-element character vector giving x and y axis labels. If \code{type="3d"}, then \code{xyzlab} should be three-element character vector giving x, y, and z axis labels.
}
  \item{cex.point}{
Size of electrode points. Can have a unique size for each electrode.
}
  \item{col.point}{
Color of electrode points. Can have a unique color for each electrode.
}
  \item{cex.label}{
Size of electrode labels. Can have a unique size for each electrode label. Input is ignored if \code{plotlabels=FALSE} is used.  
}
  \item{col.label}{
Color of electrode labels. Can have a unique color for each electrode label. Input is ignored if \code{plotlabels=FALSE} is used.
}
  \item{nose}{
If \code{TRUE}, triangle is plotted to represent the subject's nose. Ignored if \code{type="3d"}.
}
  \item{ears}{
If \code{TRUE}, ovals are plotted to represent the subject's ears. Ignored if \code{type="3d"}.
}
  \item{head}{
If \code{TRUE}, head is plotted. Ignored if \code{type="2d"}.
}
  \item{col.head}{
Color for dummy head in 3d plot. Ignored if \code{type="2d"}.
}
  \item{...}{
  Optional inputs for \code{plot} or \code{plot3d} function.
}

}
\value{
Produces plot of EEG cap with \code{NULL} return value.
}
\references{
  Adler, D., Murdoch, D., and others (2014). \emph{rgl: 3D visualization device system
  (OpenGL)}. http://CRAN.R-project.org/package=rgl
  
  Oostenveld, R., and Praamstra, P. (2001). The Five percent electrode system for high-resolution EEG and ERP measurements. \emph{Clinical Neurophysiology, 112}, 713-719. 
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

See \code{\link{eegcoord}} for the coordinates used to create plot.

To save three-dimensional plots, use the \code{\link[rgl]{rgl.postscript}} function (from \code{rgl} package).
}

\examples{
##########   EXAMPLE 1   ##########

# plot 10-10 system (default):

# plot full cap 3d (default options)
eegcap()

# plot full cap 2d (default options)
eegcap(type="2d")

# plot full cap 3d (different color for ears and nose)
data(eegcoord)
mycols=rep("black",87)
mycols[rownames(eegcoord)=="A1"]="green"
mycols[rownames(eegcoord)=="A2"]="light blue"
mycols[rownames(eegcoord)=="NZ"]="pink"
eegcap(col.point=mycols)

# plot full cap 2d (different color for ears and nose)
mycols=rep("white",87)
mycols[rownames(eegcoord)=="A1"]="green"
mycols[rownames(eegcoord)=="A2"]="light blue"
mycols[rownames(eegcoord)=="NZ"]="pink"
eegcap(type="2d",col.point=mycols)




##########   EXAMPLE 2   ##########

# plot 10-20 system:

# plot 3d cap with labels
eegcap(electrodes="10-20")

# plot 3d cap without labels
eegcap("10-20",plotlabels=FALSE)

# plot 2d cap with labels
eegcap("10-20","2d")

# plot 2d cap without labels
eegcap("10-20","2d",plotlabels=FALSE)



##########   EXAMPLE 3   ##########

# plot custom subset of electrodes
myelectrodes=c("FP1","FP2","FPZ","F7","F3","FZ",
               "F4","F8","T7","C3","CZ","C4","T8",
               "P7","P3","PZ","P4","P8","O1","O2")
eegcap(myelectrodes,"2d")

}
