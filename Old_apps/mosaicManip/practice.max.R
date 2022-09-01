rFun <- function(vars=~x&y, seed=NULL, n=0) {
  if( class(vars) != "formula" )
    stop("Must provide a formula, e.g. ~x&y, to identify the variables")
  nmaxes <- ifelse(n==0, ceiling( runif(1,min=4,max=10)), n)
  varnames = all.vars(vars)
  nvars <- length(varnames)
  if (nvars > 3 ) stop("Number of vars limited to 3")
  locs <- list()
  for (k in 1:nvars) locs[[k]] <- runif(nmaxes, min=-3,max=3)
  signsmax <- runif(nmaxes,min=3,max=10)*sign( runif(nmaxes,min=-1,max=1) )
  xscales <- runif( nmaxes, min=.1, max=5 )
  if( nvars == 1 ) {
    f <- function() {
      x <- eval(parse(text=varnames[1]))
      res <- 0  
      for(k in 1:nmaxes){
        res <- res +  signsmax[k]*exp(-(xscales[k]*(x-locs[[1]][k])^2)/9) 
      }
      return(res)
    }
  }
  if( nvars == 2 ) {
    f <- function() {
      x <- eval(parse(text=varnames[1]))
      y <- eval(parse(text=varnames[2]))
      res <- 0  
      for(k in 1:nmaxes){
        res <- res +  signsmax[k]*exp(-(xscales[k]*(x-locs[[1]][k])^2 + (y-locs[[2]][k])^2)/9) 
      }
      return(res)
    }
  }
  tmp <- paste( "alist( ", paste(varnames, "=",collapse=",",sep=""),")")
  tmp <- eval(parse(text=tmp))
  formals(f) <- tmp
  return(f)
}
  
practice.max=function(seed=NULL, n=0) {
  if( !is.null(seed) ) set.seed(seed)
  if (n==0) nmaxes = ceiling( runif(1,min=4,max=10))
  else nmaxes = n
  xlocs = runif( nmaxes, min=-3,max=3 )
  ylocs = runif( nmaxes, min=-3,max=3 )
  signsmax = runif(nmaxes,min=3,max=10)*sign( runif(nmaxes,min=-1,max=1) )
  xscales = runif( nmaxes, min=.1, max=5 )
  f = function(x,y) {
    res=0  
    for(k in 1:nmaxes){
      res = res +  signsmax[k]*exp(-(xscales[k]*(x-xlocs[k])^2 + (y-ylocs[k])^2)/9) 
    }

    return(res)

  }
  return(f)
}
