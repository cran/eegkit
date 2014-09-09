voltcol <-
  function(z,zmin=min(z),zmax=max(z),col=NULL,ncolor=100){
    if(is.null(col[1])){
      col=rev(rainbow(ncolor,end=3/4))
    } else {col=colorRampPalette(col)(ncolor)}
    col[as.integer(1+pmax(0,pmin(floor(ncolor*((z-zmin)/(zmax-zmin))),ncolor-1L)))]
  }