eegfft <-
  function(x, Fs, lower, upper){
    # Fast Fourier Transform for EEG data
    # Nathaniel E. Helwig (helwig@umn.edu)
    # last updated: May 21, 2018
    
    # check dimensions of x
    x <- as.matrix(x)
    ntime <- nrow(x)
    nchan <- ncol(x)
    
    # check lower and upper
    if(missing(lower)){
      lower <- 0
    } else {
      lower <- as.numeric(lower[1])
      if(lower < 0 | lower > Fs/2 - Fs/ntime) stop("Need 0 <= lower <= Fs/2 - Fs/n")
    }
    if(missing(upper)){
      upper <- Fs/2 - Fs/ntime
    } else {
      upper <- as.numeric(upper[1])
      if(upper < lower | upper > Fs/2 - Fs/ntime) stop("Need 0 <= lower <= upper <= Fs/2 - Fs/n")
    }
    
    # frequencies of interest
    freqs <- seq(0, Fs/2 - Fs/ntime, by = Fs/ntime)
    indx <- which(freqs >= lower & freqs <= upper)
    freqs <- freqs[indx]
    
    # fft of each column of x
    nnew <- ntime / 2
    if(nchan == 1L){
      xfft <- fft(x)[1:nnew] / ntime
      strength <- Mod(xfft)
      strength[2:nnew] <- 2 * strength[2:nnew]
      phase <- Arg(xfft)
      strength <- strength[indx]
      phase <- phase[indx]
      return(data.frame(frequency = freqs, strength = strength, phase.shift = phase))
    } else {
      xfft <- mvfft(x)[1:nnew,] / ntime
      strength <- Mod(xfft)
      strength[2:nnew, ] <- 2 * strength[2:nnew, ]
      phase <- apply(xfft, 2, Arg)
      strength <- strength[indx,]
      phase <- phase[indx,]
      return(list(frequency = freqs, strength = strength, phase.shift = phase))
    } # end if(nchan == 1L)
    
  }