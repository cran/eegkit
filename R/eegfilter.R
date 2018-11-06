eegfilter <-
  function(x, Fs, lower, upper, method = "butter",
          order = 3L, forwardreverse = TRUE, 
          scale = FALSE, plot = FALSE){
    ###### Filters EEG Data at Specified Frequencies
    ###### Nathaniel E. Helwig (helwig@umn.edu)
    ###### Last modified: August 8, 2018
    
    # check 'x'
    x <- as.matrix(x)
    n <- nrow(x)
    
    # check filter type
    if(!missing(lower) && !missing(upper)){
      type <- "pass"
    } else if(!missing(lower)){
      type <- "high"
    } else if(!missing(upper)){
      type <- "low"
    } else {
      stop("You need to provide either:\n 'lower' frequency to keep (for high-pass filter),\n 'upper' frequency to keep (for a low-pass filter), or\n 'lower' and 'upper' (for a band-pass filter).")
    }
    
    # check 'lower' and 'upper'
    if(missing(lower)){
      lower <- NULL
    } else {
      lower <- as.numeric(lower[1])
      if(lower <= 0 | lower > Fs/2) stop("Need 0 < lower <= Fs/2")
    }
    if(missing(upper)){
      upper <- NULL
    } else {
      upper <- as.numeric(upper[1])
      if(type == "pass"){
        if(upper < lower | upper > Fs/2) stop("Need 0 <= lower <= upper <= Fs/2")
      } else {
        if(upper <= 0 | upper > Fs/2) stop("Need 0 < upper <= Fs/2")
      }
    }
    
    # check 'method'
    method <- method[1]
    if(!any(method == c("butter", "fir1"))) stop("Input 'method' must be either 'butter' or 'fir1'.")
    
    # check 'order'
    if(is.null(order)){
      order <- ifelse(method == "butter", 3L, 10L)
    } else {
      order <- as.integer(order[1])
      if(order < 1L) stop("Input 'order' must be a positive integer.")
    }
    
    # create filter
    flims <- c(lower, upper) / (Fs / 2)
    if(method == "butter"){
      thefilter <- butter(order, W = flims, type = type, scale = scale)
    } else if(method == "fir1"){
      thefilter <- fir1(order, w = flims, type = type, scale = scale)
    }
    
    # plot filter
    if(plot){
      xx <- freqz(thefilter, Fs = Fs, n = Fs)
      ix <- which(xx$f < upper * 1.2)
      freqz_plot(xx$f[ix], xx$h[ix])
    }
    
    # filter data
    if(forwardreverse){
      for(j in 1:ncol(x)) x[,j] <- filtfilt(thefilter, x = x[,j])
    } else {
      for(j in 1:ncol(x)) x[,j] <- signal::filter(thefilter, x = x[,j])
    }
    x
    
  } # end eegfilt.R