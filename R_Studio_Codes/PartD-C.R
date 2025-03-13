MinE<-function(x,y){
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(y))
  if(x>=y){return(y)}
  else{return(x)}
}
Cost<-function(x,y){
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(y))
  if(y>x){return(y-x)}
  else{return(0)}
}

G_X2<-function(x,y){
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(y))
  1500*MinE(x,y)+600*Cost(x,y)-900*y
}

