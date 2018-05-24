\name{eegresample}
\alias{eegresample}
\title{
Change Sampling Rate of EEG Data
}
\description{
Turn a signal of length \code{N} into a signal of length \code{n} via linear interpolation.
}
\usage{
eegresample(x, n)
}
\arguments{
  \item{x}{
Vector or matrix (time by channel) of EEG data with \code{N} time points.
}
  \item{n}{
Number of time points for the resampled data.
}
}
\details{
Data are resampled using the "Linear Length Normalization" approach described in Helwig et al. (2011). Let \eqn{\mathbf{x} = (x_1, \ldots, x_N)'} denote the input vector of length \eqn{N}, and define a vector \eqn{\mathbf{t} = (t_1, \ldots, t_n)} with entries \deqn{t_i = 1 + (i - 1) \delta} for \eqn{i = 1, \ldots, n} where \eqn{\delta = (N - 1) / (n - 1)}. The resampled vector is calculated as \deqn{y_i = x_{\lfloor t_i \rfloor} + (x_{\lceil t_i \rceil} - x_{\lfloor t_i \rfloor}) ( t_i - \lfloor t_i \rfloor)  } for \eqn{i = 1, \ldots, n} where \eqn{\lfloor \cdot \rfloor} and \eqn{\lceil \cdot \rceil} denote the floor and ceiling functions.
}
\value{
Resampled version of input data with \code{n} time points.
}
\references{
Helwig, N. E., Hong, S., Hsiao-Wecksler E. T., & Polk, J. D. (2011). Methods to temporally align gait cycle data. Journal of Biomechanics, 44(3), 561-566.
}
\author{
Nathaniel E. Helwig <helwig@umn.edu>
}
\note{
Typical usage is to down-sample (i.e., decrease the sampling rate of) a signal: \code{n < N}.
}

\examples{

##########   EXAMPLE 1   ##########

# create vector with N = 200 time points
N <- 200
x <- sin(4 * pi * seq(0, 1, length.out = N))

# down-sample (i.e., decrease sampling rate) to n = 100
y <- eegresample(x, n = 100)
mean((y - sin(4 * pi * seq(0, 1, length.out = 100)))^2)

# up-sample (i.e., increase sampling rate) to n = 500
z <- eegresample(x, n = 500)
mean((z - sin(4 * pi * seq(0, 1, length.out = 500)))^2)

# plot results
par(mfrow = c(1,3))
plot(x, main = "Original (N = 200)")
plot(y, main = "Down-sampled (n = 100)")
plot(z, main = "Up-sampled (n = 500)")


##########   EXAMPLE 2   ##########

# create matrix with N = 500 time points and 2 columns
N <- 500
x <- cbind(sin(2 * pi * seq(0, 1, length.out = N)),
           sin(4 * pi * seq(0, 1, length.out = N)))
           
# down-sample (i.e., decrease sampling rate) to n = 250
y <- eegresample(x, n = 250)
ytrue <- cbind(sin(2 * pi * seq(0, 1, length.out = 250)),
               sin(4 * pi * seq(0, 1, length.out = 250)))
mean((y - ytrue)^2)

# up-sample (i.e., increase sampling rate) to n = 1000
z <- eegresample(x, n = 1000)
ztrue <- cbind(sin(2 * pi * seq(0, 1, length.out = 1000)),
               sin(4 * pi * seq(0, 1, length.out = 1000)))
mean((z - ztrue)^2)

# plot results
par(mfrow = c(1,3))
plot(x[,1], main = "Original (N = 500)", cex = 0.5)
points(x[,2], pch = 2, col = "blue", cex = 0.5)
plot(y[,1], main = "Down-sampled (n = 250)", cex = 0.5)
points(y[,2], pch = 2, col = "blue", cex = 0.5)
plot(z[,1], main = "Up-sampled (n = 1000)", cex = 0.5)
points(z[,2], pch = 2, col = "blue", cex = 0.5)

}