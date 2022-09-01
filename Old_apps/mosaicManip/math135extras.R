print.function = function(x, useSource=TRUE, ...) {
  kind = attr(x,"mosaicType")
  if( is.null(kind) )
    
  else {
    if( kind == "Numerical finite-difference process") {
      body(x) = "Numerical finite-difference process"
    }
  }
  base::print(x, useSource=useSource, ...)
}