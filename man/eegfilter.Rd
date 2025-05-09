\name{eegfilter}
\alias{eegfilter}
\title{
Filters EEG Data
}
\description{
Low-pass, high-pass, or band-pass filter EEG data using either a Butterworth filter (default) or a finite impulse response (FIR) filter.
}
\usage{
eegfilter(x, Fs, lower, upper, method = "butter",
          order = 3L, forwardreverse = TRUE, 
          scale = FALSE, plot = FALSE)
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
  \item{method}{
Filtering method. Either \code{"butter"} for a Butterworth filter or \code{"fir1"} for a FIR filter.
}
  \item{order}{
Order of the filter. See corresponding argument of \code{\link[signal]{butter}} or \code{\link[signal]{fir1}}.
}
  \item{forwardreverse}{
If \code{TRUE} (default), the data are forward and reverse filtered via \code{\link[signal]{filtfilt}}. Otherwise the data are (forward) filtered via \code{\link{filter}}.
}
  \item{scale}{
If \code{FALSE} (default), the filter is not normalized. Otherwise the magnitude of the center of the first passband is normalized to 1.
}
  \item{plot}{
If \code{TRUE}, the filter is plotted via \code{\link[signal]{freqz_plot}}.
}
}
\details{
For a low-pass filter, only enter the \code{upper} frequency to keep. For a high-pass filter, only enter the \code{lower} frequency to keep. For a band-pass filter, enter both the \code{lower} and \code{upper} frequency bounds.
}
\value{
Filtered version of input data.
}
\references{
http://en.wikipedia.org/wiki/Butterworth_filter

http://en.wikipedia.org/wiki/Fir_filter
}
\author{
Nathaniel E. Helwig <helwig@umn.edu>
}
\seealso{
\code{\link[signal]{filter}}, \code{\link[signal]{filtfilt}}, \code{\link[signal]{butter}}, \code{\link[signal]{fir1}}
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

# 4-th order Butterworth filter (2 to 15 Hz band-pass)
yf.but <- eegfilter(y, Fs = n, lower = 2, upper = 15, method = "butter", order = 4)

# 350-th order FIR filter (2 to 15 Hz band-pass)
yf.fir <- eegfilter(y, Fs = n, lower = 2, upper = 15, method = "fir1", order = 350)

# check quality of results
yftrue <- s5 + s10                    # true (filtered) mean signal
mean((yf.but - yftrue)^2)             # mse between yf.but and yftrue
mean((yf.fir - yftrue)^2)             # mse between yf.fir and yftrue

# plot true and estimated filtered signals
plot(t, yftrue, type = "l", lty = 1, lwd = 2, ylim = c(-3, 3))
lines(t, yf.but, col = "blue", lty = 2, lwd = 2)
lines(t, yf.fir, col = "red", lty = 3, lwd = 2)
legend("topright", legend = c("Truth", "Butterworth", "FIR"), 
       lty = 1:3, lwd = 2, col = c("black", "blue", "red"), bty = "n")

# power spectral density before and after filtering (dB)
par(mfrow=c(1,3), mar = c(5, 4.5, 4, 2) + 0.1)
eegpsd(y, Fs = n, upper = 50, t = "b",
       main = "Before Filtering", lwd = 2)
rect(2, -63, 15, 1, col = rgb(0.5,0.5,0.5,1/4))
legend("topright", legend = "2-15 Hz Filter", 
       fill = rgb(0.5,0.5,0.5,1/4), bty = "n")
eegpsd(yf.but, Fs = n, upper = 50, t = "b",
       main = "After Butterworth Filter", lwd = 2)
eegpsd(yf.fir, Fs = n, upper = 50, t = "b",
       main = "After FIR Filter", lwd = 2)
       
# power spectral density before and after filtering (mv^2)
par(mfrow=c(1,3), mar = c(5, 4.5, 4, 2) + 0.1)
eegpsd(y, Fs = n, upper = 50, unit = "mV^2", t = "b",
       main = "Before Filtering", lwd = 2)
rect(2, 0, 15, 1.05, col = rgb(0.5,0.5,0.5,1/4))
legend("topright", legend = "2-15 Hz Filter", 
       fill = rgb(0.5,0.5,0.5,1/4), bty = "n")
eegpsd(yf.but, Fs = n, upper = 50, unit = "mV^2", t = "b",
       main = "After Butterworth Filter", lwd = 2)
eegpsd(yf.fir, Fs = n, upper = 50, unit = "mV^2", t = "b",
       main = "After FIR Filter", lwd = 2)

}
