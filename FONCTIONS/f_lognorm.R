#Minicode f_lognorm

f_lognorm <- function(in.VARz){
  #test fonction : in.VARz <- (A_zi[,i+1])
  
  #log-transformation log[X+1] 
  y <- apply(as.matrix(c(in.VARz)),c(1),function(x) log(x+1.))
  # normalisation entre 0 et 1
  imin   <- min(y,na.rm=TRUE)
  irange <- range(y,na.rm=TRUE)
  if(diff(irange)==0){
    out <- y
  } else {
    out <- (y-imin)/diff(irange)
  }
  return(out)
}
