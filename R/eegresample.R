eegresample <- 
  function(x, n){
    ###### Change Sampling Rate of EEG Data
    ###### Nathaniel E. Helwig (helwig@umn.edu)
    ###### Last modified: May 23, 2018
    
    x <- as.matrix(x)
    n <- as.integer(n[1])
    if(n < 2L) stop("Input 'n' must be a positive integer greater than 1.")
    N <- nrow(x)
    d <- (N - 1) / (n - 1)
    t <- seq(1, N, by = d)
    tf <- floor(t)
    tc <- ceiling(t)
    x[tf,] + (x[tc,] - x[tf,]) * (t - tf)
    
}