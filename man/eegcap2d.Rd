\name{eegcap2d}
\alias{eegcap2d}
\title{
Draws 2D EEG Cap
}
\description{
Creates two-dimensional plot of electroencephalography (EEG) cap with user-input electrodes. Currently supports 84 scalp electrodes, and plots according to the international 10-10 system. Includes customization options (e.g., each electrode can have a unique plotting color, size, label color, etc.).
}
\usage{
eegcap2d(electrodes = "10-10", axes = FALSE, asp = 1, 
         cex.point = 2.75, col.point = "green", pch.point = 19,
         cex.border = 2.75, col.border = "black", pch.border = 21,
         cex.label = 0.5, col.label = "black", 
         head = TRUE, nose = TRUE, ears = TRUE, 
         main = "", xlab = "", ylab = "", 
         xlim = c(-13.7, 13.7), ylim = c(-13.7, 13.7), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{electrodes}{
Character vector with electrodes to plot. Each element of \code{electrodes} must match one of the 89 reference electrodes (see Details). Mismatches are ignored (not plotted). Input is NOT case sensitive. Default plots all available electrodes (full 10-10 system).
}
  \item{axes}{
If \code{FALSE} (default), no axes are plotted.
}
  \item{asp}{
Aspect ratio for plot (defaults to 1).
}
  \item{cex.point}{
Character EXpansion value for electrodes. Set to a negative value to suppress the electrode plotting.
}
  \item{col.point}{
Color for electrodes. Ignored if \code{cex.point < 0}.
}
  \item{pch.point}{
Plotting character for electrodes. Ignored if \code{cex.point < 0}.
}
  \item{cex.border}{
Character EXpansion value for electrode borders. Set to a negative value to suppress the electrode border plotting.
}
  \item{col.border}{
Color for electrode borders. Ignored if \code{cex.border < 0}.
}
  \item{pch.border}{
Plotting character for electrode borders. Ignored if \code{cex.border < 0}.
}
  \item{cex.label}{
Character EXpansion value for electrode labels. Set to a negative value to suppress the electrode label plotting.
}
  \item{col.label}{
Color for electrode labels. Ignored if \code{cex.label < 0}.
}
  \item{head}{
If \code{TRUE}, a circle is plotted to represent the subject's head.
}
  \item{nose}{
  If \code{TRUE}, a triangle is plotted to represent the subject's nose.
}
  \item{ears}{
  If \code{TRUE}, two ovals are plotted to represent the subject's ears.
}
  \item{main}{
Title to use for plot. Default is no title.
}
  \item{xlab, ylab}{
x-axis and y-axis labels for the plot. Default is no axis labels.
}
  \item{xlim, ylim}{
x-axis and y-axis limits for the plot.
}
  \item{...}{
  Optional inputs for \code{plot} function.
}
}
\details{
Currently supports 84 scalp electrodes (plus ears and nose):
A1   A2   AF1  AF2  AF3  AF4  AF5  AF6  AF7  AF8  AFZ  C1   C2   C3   C4   
C5   C6   CP1  CP2  CP3  CP4  CP5  CP6  CPZ  CZ   F1   F2   F3   F4   F5   
F6   F7   F8   F9   F10  FC1  FC2  FC3  FC4  FC5  FC6  FCZ  FP1  FP2  FPZ  
FT7  FT8  FT9  FT10 FZ   I1   I2   IZ   NZ   O1   O2   OZ   P1   P2   P3   
P4   P5   P6   P7   P8   P9   P10  PO1  PO2  PO3  PO4  PO5  PO6  PO7  PO8  
PO9  PO10 POZ  PZ   T7   T8   T9   T10  TP7  TP8  TP9  TP10

See \code{\link{eegcoord}} for the coordinates used to create plot.
}
\value{
Produces plot of EEG cap.
}
\references{
Oostenveld, R., and Praamstra, P. (2001). The Five percent electrode system for high-resolution EEG and ERP measurements. \emph{Clinical Neurophysiology, 112}, 713-719. 
}
\author{
Nathaniel E. Helwig <helwig@umn.edu>
}
\note{
Unlike the \code{\link{eegcap}} function, this function does not use \code{par$plt} for the figure positioning. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See \code{\link{eegcap}} for a similar implementation, which also supports 3d EEG cap plotting.
}
\examples{

##########   EXAMPLE 1   ##########

# plot 10-10 system (default):

# plot full cap (default options)
eegcap2d()

# plot full cap (different color for ears and nose)
data(eegcoord)
mycols <- rep(NA, 87)
enames <- rownames(eegcoord)
mycols[enames=="A1"] <- "green"
mycols[enames=="A2"] <- "light blue"
mycols[enames=="NZ"] <- "pink"
eegcap2d(col.point = mycols)



##########   EXAMPLE 2   ##########

# plot 10-20 system:

# plot cap with labels
eegcap2d("10-20")

# plot cap without labels
eegcap2d("10-20", cex.label = -1)



##########   EXAMPLE 3   ##########

# plot custom subset of electrodes
myelectrodes <- c("FP1","FP2","FPZ","F7","F3","FZ",
                  "F4","F8","T7","C3","CZ","C4","T8",
                  "P7","P3","PZ","P4","P8","O1","O2")
eegcap2d(myelectrodes)

}
