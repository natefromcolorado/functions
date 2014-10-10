#FUNCTION KEASEN FROM USGS SPLUS LIBRARY (must be pulled from the S-plus running function in S-plus)

kensen = function(y, t){
  vark <- function(y)
  {
    #   Define the variance of S for Kendall's tau
    #    (The time variable in this portion of the cor.test code
    #     has been eliminated as we verify that time is strictly increasing.)
    ties.y <- rle(sort(y))$lengths
    n <- length(y)
    t1 <- n * (n - 1) * (2 * n + 5)
    t2 <- sum(ties.y * (ties.y - 1) * (2 * ties.y + 5))
    v1 <- (t1 - t2)/18
    return(v1)
  }
  # Handle time series classes its, cts, & rts.
  # >>> Need to write this section yet. <<<
  # Error checking.
  # Make sure we have enough data.
  n <- length(y)
  if(n < 3)
    stop("y and t should effectively be longer than 2.")
  # Make sure every data value has a time value and vice versa.
  if(any(is.na(y) != is.na(t))) stop(
    "Each data value must have a time value and vice versa."
  )
  # Make sure time is strictly increasing
  if(any(diff(t) <= 0)) stop("Time vector must be strictly increasing.")
  # Calculate the statistics
  # a fast, efficient way to compute concordant pairs:
  concordant.sum <- sum(unlist(lapply(seq(along = y), function(i, y,
                                                               t)
    sum(sign((y[i] - y[1:i]) * (t[i] - t[1:i]))), y, t)))
  # Kendall's tau:
  tau <- (2 * concordant.sum)/(n * (n - 1))
  # z statistic
  #stddv <- sqrt((n * (n - 1) * (2 * n + 5))/18) This works only if no ties.
  # Note that the cor.test version does not include the continuity correction
  #    recommended by Kendall, Rank Correlation Method, 1975, page 53.
  #    This code does include it.
  stat <- (concordant.sum - sign(concordant.sum))/sqrt(vark(y))
  # the p value
  p.value <- 2 * pnorm( - abs(stat))
  # The Sen slope estimator.
  sen.slope <- median(unlist(lapply(seq(along = y), function(i, y, t)
    ((y[i] - y[1:i])/(t[i] - t[1:i])), y, t)), na.rm = T)
  # Median of the data values.
  median.data <- median(y)
  # Median of the time values.
  median.time <- median(t)
  # A line representing the trend of the data then is given by
  #
  #    y(t) = sen.slope*(t-med.time)+med.data
  #
  #    This line has the slope of the trend and passes through
  #       the point (t=med.time, y=med.data)
  # Return the statistics.
  method <- "Kendall's tau with the Sen slope estimator"
  z <- list(method = method, tau = tau, p.value = p.value, sen.slope = 
              sen.slope, median.data = median.data, median.time = 
              median.time)
  z$data.name <- paste(deparse(substitute(y)), "and", deparse(substitute(
    t)))
  return(z)
}