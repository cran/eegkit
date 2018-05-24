voltcol <-
  function(z,zmin=min(z),zmax=max(z),col=NULL,ncolor=25){
    if(is.null(col[1])){
      #col <- colorRampPalette(c("blueviolet","blue","cyan","green","yellow","orange","red"))(ncolor)
      col <- colorRampPalette(c("darkblue", rainbow(12)[c(9,8,7,5,3,2,1)], "darkred"))(ncolor)
    } else {
      col <- colorRampPalette(col)(ncolor)
    }
    col[.bincode(z,seq(zmin,zmax,length.out=(ncolor+1L)),right=TRUE,include.lowest=TRUE)]
  }