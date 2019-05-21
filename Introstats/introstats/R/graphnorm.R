graphnorm <- function(stat, a = -inf, b = inf, mu, sigma, printz = FALSE, gcol = "black", shade = "lightblue1"){
  #stat: name of statistic you would like to plot
  #a: right end point
  #b: left end point
  #mean: mean of distribution
  #sigma: standard deviation of distribution,
  #printz: optional argument to produce z graph as well
  a <- max(a, mu - 4 * sigma)
  b <- min(b, mu + 4 * sigma)

  stat.pic <- function(stat, a, b, mu, sigma){

    x <- seq(-4 * sigma + mu, 4 * sigma + mu, length = 200)
    y <- dnorm(x, mean = mu, sd = sigma)
    plot(x, y, type = "l", lwd = 2, col = gcol, xaxt = "n", xlab = stat)

    za <- (a - mu) / sigma
    if (za > -4){
      axis(1, at = a, las = 2)
    }

    zb  <- (b - mu) / sigma
    if (zb < 4){
      axis(1, at = b, las = 2)
    }

    axis(1, at = mu, las = 2)

    x <- seq(a, b, length = 200)
    y <- dnorm(x, mean = mu, sd = sigma)
    polygon(c(a, x, b), c(0, y, 0), col = shade)
  }

  z.pic <- function(a, b, mu, sigma){

    z <- seq(-4, 4, length = 200)
    y <- dnorm(z, mean = 0, sd = 1)
    plot(z, y, type = "l", lwd = 2, col = gcol, xaxt = "n", xlab = "z")

    za <- (a - mu) / sigma
    if (za > -4){
      axis(1, at = round(za, 2), las = 2)
    }

    zb <- (b - mu) / sigma
    if (zb < 4){
      axis(1, at = round(zb, 2), last = 2)
    }

    axis(1, at = 0, las = 2)

    z <- seq(za, zb, length = 200)
    y <- dnorm(z, mean = 0, sd = 1)
    polygon(c(za, z, zb), c(0, y, 0), col = shade)

  }

  if (printz == FALSE){
    stat.pic(stat, a, b, mu, sigma)
  }

  if (printz == TRUE){
    par(mfrow = c(1, 2))
    stat.pic(stat, a, b, mu, sigma)
    z.pic(a, b, mu, sigma)
  }

}
