\name{eegica}
\alias{eegica}
\title{
Independent Component Analysis of EEG data
}
\description{
Computes temporal (default) or spatial ICA decomposition of EEG data. Can use Infomax (default), FastICA, or JADE algorithm. ICA computations are conducted via \code{\link[ica]{icaimax}}, \code{\link[ica]{icafast}}, or \code{\link[ica]{icajade}} from the \code{ica} package.
}
\usage{
eegica(X,nc,center=TRUE,maxit=100,tol=1e-6,Rmat=diag(nc),
       type=c("time","space"),method=c("imax","fast","jade"),...)
}
\arguments{
  \item{X}{
Data matrix with \code{n} rows (channels) and \code{p} columns (time points).
}
  \item{nc}{
Number of components to extract.
}
  \item{center}{
If \code{TRUE}, columns of \code{X} are mean-centered before ICA decomposition.
}
  \item{maxit}{
Maximum number of algorithm iterations to allow.
}
  \item{tol}{
Convergence tolerance.
}
  \item{Rmat}{
Initial estimate of the \code{nc}-by-\code{nc} orthogonal rotation matrix.
}
  \item{type}{
Type of ICA decomposition: \code{type="time"} extracts temporally independent components, and \code{type="space"} extracts spatially independent components.
}
  \item{method}{
Method for ICA decomposition: \code{method="imax"} uses Infomax, \code{method="fast"} uses FastICA, and \code{method="jade"} uses JADE.
}
  \item{...}{
Additional inputs to \code{\link[ica]{icaimax}} or \code{\link[ica]{icafast}} function.
}
}
\value{
\item{S}{Matrix of source signal estimates (\code{S=Y\%*\%R}).}
\item{M}{Estimated mixing matrix.}
\item{W}{Estimated unmixing matrix (\code{W=crossprod(R,Q)}).}
\item{Y}{Whitened data matrix.}
\item{Q}{Whitening matrix.}
\item{R}{Orthogonal rotation matrix.}
\item{vafs}{Variance-accounted-for by each component.}
\item{iter}{Number of algorithm iterations.}
\item{type}{ICA type (same as input).}
\item{method}{ICA method (same as input).}
}
\references{
Bell, A.J. & Sejnowski, T.J. (1995). An information-maximization approach to blind separation and blind deconvolution. \emph{Neural Computation, 7}, 1129-1159.

Cardoso, J.F., & Souloumiac, A. (1993). Blind beamforming for non-Gaussian signals. \emph{IEE Proceedings-F, 140}, 362-370.
  
Cardoso, J.F., & Souloumiac, A. (1996). Jacobi angles for simultaneous diagonalization. \emph{SIAM Journal on Matrix Analysis and Applications, 17}, 161-164.

Helwig, N.E. (in prep). On the relationship between FastICA and Infomax: Fast and robust fixed point algorithms for information-maximization.

Helwig, N.E. (2014). \emph{ica: Independent Componenet Analysis}. http://CRAN.R-project.org/package=ica

Helwig, N.E. & Hong, S. (2013). A critique of Tensor Probabilistic Independent Component Analysis: Implications and recommendations for multi-subject fMRI data analysis. \emph{Journal of Neuroscience Methods, 213}, 263-273.

Hyvarinen, A. (1999). Fast and robust fixed-point algorithms for independent component analysis. \emph{IEEE Transactions on Neural Networks, 10}, 626-634.
}
\author{
Nathaniel E. Helwig <helwig@umn.edu>
}
\details{
\bold{ICA Model}
The ICA model can be written as \code{X=tcrossprod(S,M)+E}, where columns of \code{S} contain the source signals, \code{M} is the mixing matrix, and columns of \code{E} contain the noise signals. Columns of \code{X} are assumed to have zero mean. The goal is to find the unmixing matrix \code{W} such that columns of \code{S=tcrossprod(X,W)} are independent as possible.

\bold{Whitening}
Without loss of generality, we can write \code{M=P\%*\%R} where \code{P} is a tall matrix and \code{R} is an orthogonal rotation matrix. Letting \code{Q} denote the pseudoinverse of \code{P}, we can whiten the data using \code{Y=tcrossprod(X,Q)}. The goal is to find the orthongal rotation matrix \code{R} such that the source signal estimates \code{S=Y\%*\%R} are as independent as possible. Note that \code{W=crossprod(R,Q)}.

\bold{Infomax}
The Infomax approach finds the orthogonal rotation matrix \code{R} that (approximately) maximizes the joint entropy of a nonlinear function of the estimated source signals. See Bell and Sejnowski (1995) and Helwig (in prep) for specifics of algorithms.

\bold{FastICA}
The FastICA algorithm finds the orthogonal rotation matrix \code{R} that (approximately) maximizes the negentropy of the estimated source signals. Negentropy is approximated using \deqn{J(s) = [E\{G(s)\}-E\{G(z)\} ]^2} where \emph{E} denotes the expectation, \emph{G} is the contrast function, and \emph{z} is a standard normal variable. See Hyvarinen (1999) for specifics of fixed-point algorithm.

\bold{JADE}
The JADE approach finds the orthogonal rotation matrix \code{R} that (approximately) diagonalizes the cumulant array of the source signals. See Cardoso and Souloumiac (1993,1996) and Helwig and Hong (2013) for specifics of the JADE algorithm.
}

\note{
If \code{type="time"}, the data matrix is transposed before calling ICA algorithm (i.e., \code{X=t(X)}), and the columns of the tranposed data matrix are centered.
}
\examples{
##########   EXAMPLE   ##########

# get "c" subjects of "eegdata" data
data(eegdata)
idx=which(eegdata$group=="c")
eegdata=eegdata[idx,]

# get average data (across subjects)
eegmean=tapply(eegdata$voltage,list(eegdata$channel,eegdata$time),mean)

# remove ears and nose
acnames=rownames(eegmean)
idx=c(which(acnames=="X"),which(acnames=="Y"),which(acnames=="nd"))
eegmean=eegmean[-idx,]

# get spatial coordinates (for plotting)
data(eegcoord)
cidx=match(rownames(eegmean),rownames(eegcoord))

# temporal ICA with 4 components
icatime=eegica(eegmean,4)
#x11(width=5,height=10)
par(mfrow=c(4,2))
tseq=(0:255)*1000/255
for(j in 1:4){
  par(mar=c(5.1,4.6,4.1,2.1))
  sptitle=bquote("VAF:  "*.(round(icatime$vafs[j],4)))
  eegtime(tseq,icatime$S[,j],main=bquote("Component  "*.(j)),cex.main=1.5)
  eegspace(eegcoord[cidx,4:5],icatime$M[,j],main=sptitle)
}

# spatial ICA with 4 components
icaspace=eegica(eegmean,4,type="space")
#x11(width=5,height=10)
par(mfrow=c(4,2))
tseq=(0:255)*1000/255
for(j in 1:4){
  par(mar=c(5.1,4.6,4.1,2.1))
  sptitle=bquote("VAF:  "*.(round(icaspace$vafs[j],4)))
  eegtime(tseq,icaspace$M[,j],main=bquote("Component  "*.(j)),cex.main=1.5)
  eegspace(eegcoord[cidx,4:5],icaspace$S[,j],main=sptitle)
}


}
