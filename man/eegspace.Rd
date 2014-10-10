\name{eegspace}
\alias{eegspace}
\title{
Plots Multi-Channel EEG Spatial Map
}
\description{
Creates plot of multi-channel electroencephalography (EEG) spatial map. User can control the plot type (2d or 3d), the colormap, color, etc.
}
\usage{
eegspace(space,voltage,vlim=NULL,mycolors=NULL,ncolor=100,
         colorbar=TRUE,nctick=5,rtick=1,cex.axis=1,barloc=NULL,
         colorlab=NULL,cex.lab=1,plotaxes=FALSE,main="",
         xyzlab=NULL,cex.point=1,cex.main=1,nose=TRUE,ears=TRUE,
         head=TRUE,col.head="AntiqueWhite",mar=NULL,...)
}
\arguments{
  \item{space}{
Matrix of input electrode coordinates (3d or 2d). 
}
  \item{voltage}{
Vector of recorded EEG voltage at each row in \code{space}.
}
  \item{vlim}{
Two-element vector giving the limits to use when mapping \code{voltage} to colors in \code{mycolors}. Default is \code{vlim=range(voltage)}.
}
  \item{mycolors}{
Character vector of colors to use for color mapping (such that \code{length(mycolors)<=ncolor}). Default uses a reverse rainbow color scheme: \code{mycolors=rev(rainbow(ncolor,end=3/4))}. 
}
  \item{ncolor}{
Number of colors to use in mapping (positive integer).
}
  \item{colorbar}{
If \code{TRUE}, colorbar is plotted.
}
 \item{nctick}{
Approximate number of ticks for colorbar. Ignored if \code{colorbar=FALSE}.
}
  \item{rtick}{
Round tick labels to given decimal. Ignored if \code{colorbar=FALSE}.
}
  \item{cex.axis}{
Cex of axis ticks for colorbar. Ignored if \code{colorbar=FALSE}.
}
  \item{barloc}{
Character vector giving location of color bar. See Notes.
}
  \item{colorlab}{
Character vector giving label for color bar. Ignored if \code{colorbar=FALSE}.
}
  \item{cex.lab}{
Cex of axis labels for colorbar. Ignored if \code{colorbar=FALSE}.
}
  \item{plotaxes}{
If \code{TRUE}, axes labels are plotted. Ignored for 3d plots.
}
  \item{main}{
Plot title. Default is no title.
}
  \item{xyzlab}{
Axis labels to use for plot. If \code{type="2d"}, then \code{xyzlab} should be two-element character vector giving x and y axis labels. If \code{type="3d"}, then \code{xyzlab} should be three-element character vector giving x, y, and z axis labels.
}
  \item{cex.point}{
Cex for plotted electrodes.
}
  \item{cex.main}{
Cex for plot title. Ignored if \code{main=NULL}.
}
  \item{nose}{
If \code{TRUE}, triangle is plotted to represent the subject's nose. Ignored if \code{ncol(space)==3}.
}
  \item{ears}{
If \code{TRUE}, ovals are plotted to represent the subject's ears.  Ignored if \code{ncol(space)==3}.
}
  \item{head}{
If \code{TRUE}, head is plotted. Ignored if \code{type="2d"}.
}
  \item{col.head}{
Color for dummy head in 3d plot. Ignored if \code{type="2d"}.
}
  \item{mar}{
Margins to use for plot (see \code{par}).
}
  \item{...}{
  Optional inputs for \code{plot} or \code{lines} function.
}

}
\value{
Produces plot of EEG spatial map with \code{NULL} return value.
}
\references{
  Bache, K. & Lichman, M. (2013). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.

  Begleiter, H. \emph{Neurodynamics Laboratory}. State University of New York Health Center at Brooklyn. 
  
  Ingber, L. (1997). Statistical mechanics of neocortical interactions: Canonical momenta indicatros of electroencephalography. \emph{Physical Review E, 55}, 4578-4593.
  
  Ingber, L. (1998). Statistical mechanics of neocortical interactions: Training and testing canonical momenta indicators of EEG. \emph{Mathematical Computer Modelling, 27}, 33-64.
}
\author{
Nathaniel E. Helwig <helwig@umn.edu>
}
\note{
For 3d plots, \code{barloc} can be one of four options: \code{"backright"}, \code{"backleft"}, \code{"frontright"}, or \code{"frontleft"}. For 2d plots, \code{barloc} can be either \code{"right"} or \code{"left"}.

Currently supports spatial maps registered to the 84-channel cap produced by \code{\link{eegcap}} and \code{\link{eegcoord}}.
}

\examples{
##########   EXAMPLE   ##########

# get time point 65 (approx 250 ms) from "eegdata" data
data(eegdata)
idx=which(eegdata$time==65L)
eegdata=eegdata[idx,]

# get average spatial map
eegmean=tapply(eegdata$voltage,list(eegdata$channel,eegdata$group),mean)

# remove ears and nose
acnames=rownames(eegmean)
idx=c(which(acnames=="X"),which(acnames=="Y"),which(acnames=="nd"))
eegmean=eegmean[-idx,]

# match to eeg coordinates
data(eegcoord)
cidx=match(rownames(eegmean),rownames(eegcoord))

# plot average control voltage in 3d
open3d()
eegspace(eegcoord[cidx,1:3],eegmean[,2])

# plot average control voltage in 2d
eegspace(eegcoord[cidx,4:5],eegmean[,2])

# change 3d bar location and use play3d to rotate (not run)
open3d()
par3d(windowRect=c(0,0,600,600))
eegspace(eegcoord[cidx,1:3],eegmean[,2],barloc="frontleft")
#play3d(spin3d(axis=c(0,0,1),rpm=5),duration=20)

# change 2d bar location
eegspace(eegcoord[cidx,4:5],eegmean[,2],barloc="left")


}
