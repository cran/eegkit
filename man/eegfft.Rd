\name{eegfft}
\alias{eegfft}
\title{
Fast Fourier Transform of EEG Data
}
\description{
Finds the strength (amplitude) and phase shift of the input signal(s) at a particular range of frequencies via a Discrete Fast Fourier Transform (FFT). Can input single or multi-channel data.
}
\usage{
eegfft(x, Fs, lower, upper)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Vector or matrix (time by channel) of EEG data with \code{n} time points.
}
  \item{Fs}{
Sampling rate of \code{x} in Hz such that \code{n = s * Fs} where \code{s} is the number of seconds of input data (some positive integer).
}
  \item{lower}{
Lower band in Hz. Smallest frequency to keep (defaults to \code{0}).
}
  \item{upper}{
Upper band in Hz. Largest frequency to keep (defaults to \code{Fs/2 - Fs/n}).
}
}
\details{
The \code{\link{fft}} function (or \code{\link{mvfft}} function) is used to implement the FFT (or multivatiate FFT). Given the FFT, the \emph{strength} of the signal is the modulus (\code{\link{Mod}}), and the \emph{phase.shift} is the angle (\code{\link{Arg}}).
}
\value{
If \code{x} is a vector, returns a data frame with variables:
\item{frequency }{vector of frequencies}
\item{strength }{strength (amplitude) of signal at each frequency}
\item{phase.shift }{phase shift of signal at each frequency}

If \code{x} is a matrix with \code{J} channels, returns a list with elements:
\item{frequency }{vector of frequencies of length \code{F}}
\item{strength }{\code{F} by \code{J} matrix: strength (amplitude) of signal at each frequency and channel}
\item{phase.shift }{\code{F} by \code{J} matrix: phase shift of signal at each frequency and channel}
}
\references{
Cooley, James W., and Tukey, John W. (1965) An algorithm for the machine calculation of complex Fourier series, Math. Comput. 19(90), 297-301.

Singleton, R. C. (1979) Mixed Radix Fast Fourier Transforms, in Programs for Digital Signal Processing, IEEE Digital Signal Processing Committee eds. IEEE Press.
}
\author{
Nathaniel E. Helwig <helwig@umn.edu>
}
\note{
The strength of the signal has the same unit as the input (typically microvolts), and the phase shift is measured in radians (range -\eqn{pi} to \eqn{pi}).
}
%\seealso{
%\code{\link{eegfilter}}
%}
\examples{

##########   EXAMPLE   ##########

### Data Generation ###

# parameters for signal
Fs <- 1000                             # 1000 Hz signal
s <- 3                                 # 3 seconds of data
t <- seq(0, s - 1/Fs, by = 1/Fs)       # time sequence
n <- length(t)                         # number of data points
freqs <- c(1, 5, 10, 20)               # frequencies
amp <- c(2, 1.5, 3, 1.75)              # strengths (amplitudes)
phs <- c(0, pi/6, pi/4, pi/2)          # phase shifts

# create data generating signals
mu <- rep(0, n)
for(j in 1:length(freqs)){
  mu <- mu + amp[j] * sin(2*pi*t*freqs[j] + phs[j])
}
set.seed(1)                           # set random seed
e <- rnorm(n)                         # Gaussian error
y <- mu + e                           # data = mean + error


### FFT of Noise-Free Data ###

# fft of noise-free data
ef <- eegfft(mu, Fs = Fs, upper = 40)
head(ef)
ef[ef$strength > 0.25,]

# plot frequency strength
par(mfrow = c(1,2))
plot(x = ef$frequency, y = ef$strength, t = "b",
     xlab = "Frequency (Hz)", 
     ylab = expression("Strength (" * mu * "V)"),
     main = "FFT of Noise-Free Data")

# compare to data generating parameters
cbind(amp, ef$strength[ef$strength > 0.25])
cbind(phs - pi/2, ef$phase[ef$strength > 0.25])


### FFT of Noisy Data ###

# fft of noisy data
ef <- eegfft(y, Fs = Fs, upper = 40)
head(ef)
ef[ef$strength > 0.25,]

# plot frequency strength
plot(x = ef$frequency, y = ef$strength, t = "b",
     xlab = "Frequency (Hz)", 
     ylab = expression("Strength (" * mu * "V)"),
     main = "FFT of Noisy Data")

# compare to data generating parameters
cbind(amp, ef$strength[ef$strength > 0.25])
cbind(phs - pi/2, ef$phase[ef$strength > 0.25])

}
