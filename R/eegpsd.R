eegpsd <- 
  function(x, Fs, lower, upper, units = "dB", 
           xlab = NULL, ylab = NULL, zlab = NULL, ...){
    ###### Plot Power Spectral Density of EEG Data
    ###### Nathaniel E. Helwig (helwig@umn.edu)
    ###### Last modified: August 9, 2018
    
    # fft of data
    xfft <- eegfft(x, Fs, lower, upper)
    
    # transform units?
    units <- units[1]
    if(units == "dB"){
      xfft$strength <- 10 * log10(xfft$strength^2)
    } else if(units == "mV^2"){
      xfft$strength <- xfft$strength^2
    }
    
    # plot
    if(is.data.frame(xfft)){
      if(is.null(xlab)) xlab <- "Frequency (Hz)"
      if(is.null(ylab)) ylab <- paste0("Power (", units, ")")
      plot(xfft$frequency, xfft$strength, 
           xlab = xlab, ylab = ylab, ...)
    } else {
      if(is.null(xlab)) xlab <- "Frequency (Hz)"
      if(is.null(ylab)) ylab <- "Channel"
      if(is.null(zlab)) zlab <- paste0("Power (", units, ")")
      nc <- ncol(xfft$strength)
      imagebar(xfft$frequency, 1:nc, xfft$strength, 
               xlab = xlab, ylab = ylab, zlab = zlab, ...)
    }
    
  } # end eegpsd.R