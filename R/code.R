ggnorm=function(z) {
  z=z[!is.na(z)]
  y=quantile(z, c(0.25, 0.75))
  x=qnorm(c(0.25, 0.75))
  b=(y[2]-y[1])/(x[2]-x[1])
  a=y[1]-slope*x[1]

  d <- data.frame(z)

  ggplot2::ggplot(d, aes(sample = z)) + stat_qq() +
     geom_abline(slope = b, intercept = a)

}
