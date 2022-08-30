# Adding a plotting function for the lognormal distm based on norm.plot.f

#' @param Nrand, the number of random values to make
#' @param Edist.a
#' @param Edist.b
#' @param Emean.shift
#' @param E.var.inc
#' @keywords
#' @export
#' @examples
lnorm.plot.f= function(Nrand, Edist.a, Edist.b, Emean.shift, E.var.inc){
  nval= rlnorm(Nrand,mean=Edist.a,sd=Edist.b*E.var.inc)+Emean.shift
  nval
}
