#' Draw normal quantile plot with line
#'
#' @param z A vector
#' @return Normal quantile plot with line
#' @examples
#' set.seed(1)
#' ggnormplot(rnorm(100))
#' ggnormplot(rchisq(100,3))
#' @export
ggnormplot=function(z) {
  # idea from 1st answer to http://stackoverflow.com/questions/4357031/qqnorm-and-qqline-in-ggplot2
  z=z[!is.na(z)]
  y=quantile(z, c(0.25, 0.75))
  x=qnorm(c(0.25, 0.75))
  b=(y[2]-y[1])/(x[2]-x[1])
  a=y[1]-b*x[1]

  d <- data.frame(z)

  ggplot2::ggplot(d, aes(sample=z)) + stat_qq() +
     geom_abline(slope = b, intercept = a)

}

