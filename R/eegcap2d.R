eegcap2d <- 
  function(electrodes = "10-10", axes = FALSE, asp = 1, 
           cex.point = 2.75, col.point = "green", pch.point = 19,
           cex.border = 2.75, col.border = "black", pch.border = 21,
           cex.label = 0.5, col.label = "black", 
           head = TRUE, nose = TRUE, ears = TRUE, 
           main = "", xlab = "", ylab = "", 
           xlim = c(-13.7, 13.7), ylim = c(-13.7, 13.7), ...){
    ###### Plots 2D EEG Cap with Selected Electrodes
    ###### Nathaniel E. Helwig (helwig@umn.edu)
    ###### Last modified: July 11, 2018
    
    ### initial checks
    eegcoord <- NULL
    data(eegcoord, envir = environment())
    enames <- rownames(eegcoord)
    if(electrodes[1] == "10-10"){
      eegidx <- 1:87
    } else if (electrodes[1] == "10-20"){
      eegidx <- match(c("A1","A2","FP1","FP2", "FPZ", "F7","F3","FZ",
                        "F4","F8", "NZ","T7","C3","CZ","C4","T8",
                        "P7","P3","PZ","P4","P8","O1","O2", "OZ"), enames)
    } else {
      eegidx <- match(toupper(electrodes), enames)
    }
    
    ### plot head (or create blank plot)
    rad <- 12.5
    xx <- rad * cos(seq(0, 2*pi, length.out = 360))
    yy <- rad * sin(seq(0, 2*pi, length.out = 360))
    plot(xx, yy, asp = asp, xlim = xlim, ylim = ylim, 
         type = ifelse(head, "l", "n"), main = main,
         xlab = xlab, ylab = ylab, axes = axes, ...)
    
    ### add nose
    if(nose){
      lines(c(xx[81], 0), c(yy[81], rad * 1.175))
      lines(c(-xx[81], 0), c(yy[81], rad * 1.175))
    }
    
    ### add ears
    if(ears){
      xx <- 0.5 * cos(seq(0, 2*pi, length.out = 360))
      yy <- 2.5 * sin(seq(0, 2*pi, length.out = 360))
      lines(xx - 13, yy - 1.5)
      lines(xx + 13, yy - 1.5)
    }
    
    ### add points
    if(cex.point > 0){
      points(x = eegcoord[eegidx,4], y = eegcoord[eegidx,5], 
             cex = cex.point, col = col.point, pch = pch.point)
      
    }
    
    ### add borders
    if(cex.border > 0){
      points(x = eegcoord[eegidx,4], y = eegcoord[eegidx,5], 
             cex = cex.border, col = col.border, pch = pch.border)
    }
    
    ### add labels
    if(cex.label > 0){
      text(x = eegcoord[eegidx,4], y = eegcoord[eegidx,5], 
           label = enames[eegidx], cex = cex.label, col = col.label)
    }
    
}