#Minicode f_linnorm

f_linnorm <- function(in.VARz){
  irange <- range(in.VARz,na.rm=TRUE)
  if(diff(irange)==0){
    out <- in.VARz
  } else {
    out <- (in.VARz - irange[1])/diff(irange)
  }
  return(out)
}
