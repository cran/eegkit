\name{eegpsd}
\alias{eegpsd}
\title{
Plots Power Spectral Density of EEG Data
}
\description{
Uses a fast discrete Fourier transform (\code{\link{eegfft}}) to estimate the power spectral density of EEG data, and plots the power esimate using the \code{\link{plot}} (single channel) or \code{\link{imagebar}} (multi-channel) function.
}
\usage{
eegpsd(x, Fs, lower, upper, units = "dB", 
       xlab = NULL, ylab = NULL, zlab = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Vector or matrix (time by channel) of EEG data with \code{n} time points.
}
  \item{Fs}{
Sampling rate of \code{x} in Hz.
}
  \item{lower}{
Lower band in Hz. Smallest frequency to keep.
}
  \item{upper}{
Upper band in Hz. Largest frequency to keep.
}
  \item{units}{
Units for plot. Options include "dB" for decibals (default), "mV" for microvolts, and "mV^2" for squared microvolts. Note dB = 10*log10(mV^2).
}
  \item{xlab}{
x-axis label for the plot/image.
}
  \item{ylab}{
y-axis label for the plot/image.
}
  \item{zlab}{
z-axis label for the plot/image.
}
  \item{...}{
Optional inputs for the \code{\link{plot}} or \code{\link{imagebar}} function.
}
  
}
\value{
Produces a plot (single channel) or image (multi-channel).
}
\references{
Cooley, James W., and Tukey, John W. (1965) An algorithm for the machine calculation of complex Fourier series, Math. Comput. 19(90), 297-301.

Singleton, R. C. (1979) Mixed Radix Fast Fourier Transforms, in Programs for Digital Signal Processing, IEEE Digital Signal Processing Committee eds. IEEE Press.
}
\author{
Nathaniel E. Helwig <helwig@umn.edu>
}

\examples{

##########   EXAMPLE   ##########

# create data generating signals
n <- 1000                             # 1000 Hz signal
s <- 2                                # 2 seconds of data
t <- seq(0, s, length.out = s * n)    # time vector
s1 <- sin(2*pi*t)                     # 1 Hz sinusoid
s5 <- sin(2*pi*t*5)                   # 5 Hz sinusoid
s10 <- sin(2*pi*t*10)                 # 10 Hz sinusoid
s20 <- sin(2*pi*t*20)                 # 20 Hz sinusoid

# create data
set.seed(1)                           # set random seed
e <- rnorm(s * n, sd = 0.25)          # Gaussian error
mu <- s1 + s5 + s10 + s20             # 1 + 5 + 10 + 20 Hz mean
y <- mu + e                           # data = mean + error

# plot psd (single channel)
eegpsd(y, Fs = n, upper = 30, t = "b")

# plot psd (multi-channel)
ym <- cbind(s1, s5, s10, s20)
eegpsd(ym, Fs = n, upper = 30, units = "mV")

}