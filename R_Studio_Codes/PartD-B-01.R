p_X2<-function(x){
  stopifnot(is.numeric(x))
  if (x %in% c(0:100)){
    (0.0787762*sqrt(x))/(8+x)
  }
  else{
    0
  }
}

F_X2<-function(x){
  stopifnot(is.numeric(x))
  a <- as.numeric(cut(x, c(-Inf,1:100,Inf),right=FALSE))
  n<-0
  if (x>=1){
    if (x>=100){
      1
    }
    else{
      for(i in 1:x){
        n<-n+p_X2(i)
      }
      return(n)
    }
  }
  else
    0
}
