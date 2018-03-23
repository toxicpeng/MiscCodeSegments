#### Written by D. Hall from the U of T Peng Group on 20180323. davidross.hall@mail.utoronto.ca
#### Confidence interval code derived heavily from the of RPubs found here https://goo.gl/J5WZYe
#### Code to generate example scatter plot taken from R. Kabacoff (2017) found here https://goo.gl/CsE2Eb

#### Code generates Confidence interval bars on a linear fit. 

#### Function inputs: GenPlot <- previously generated scatter plot
#                     x <- x value of GenPlot
#                     y <- y value of GenPlot
#                     CISignificance <- numerical value of condience interval. i.e 0.95 means a 95% confidence level. 

LinearFitCI <- function(GenPlot, x, y, CISignificance){
  n <- length(y) # Find length of y to use as sample size
  lm.model <- lm(y ~ x) # Fit linear model
  
  # Extract fitted coefficients from model object
  b0 <- lm.model$coefficients[1]
  b1 <- lm.model$coefficients[2]
  
  # Find SSE and MSE
  sse <- sum((y - lm.model$fitted.values)^2)
  mse <- sse / (n - 2)
  
  t.val <- qt(CISignificance, n - 2) # Calculate critical t-value
  
  # Fit linear model with extracted coefficients
  x_new <- min(x):max(x)
  y.fit <- b1 * x_new + b0
  
  # Find the standard error of the regression line
  se <- sqrt(sum((y - y.fit)^2) / (n - 2)) * sqrt(1 / n + (x - mean(x))^2 / sum((x - mean(x))^2))
  
  # Fit a new linear model that extends past the given data points (for plotting)
  x_new2 <- 1:max(x + 100)
  y.fit2 <- b1 * x_new2 + b0
  
  # Warnings of mismatched lengths are suppressed
  slope.upper <- suppressWarnings(y.fit2 + t.val * se)
  slope.lower <- suppressWarnings(y.fit2 - t.val * se)
  
  # Collect the computed confidence bands into a data.frame and name the colums
  bands <- data.frame(cbind(slope.lower, slope.upper))
  colnames(bands) <- c('Lower Confidence Band', 'Upper Confidence Band')
  
  # Plot the fitted linear regression line and the computed confidence bands
  GenPlot
  lines(y.fit2, col = 'black', lwd = 1)
  lines(bands[1], col = 'black', lty = 2, lwd = 1)
  lines(bands[2], col = 'black', lty = 2, lwd = 1)
}
####-------------------------------

#### Example code of linear fit with Confidence Intervals #####

attach(mtcars)

# Generate a simple scatter plot from the "cars" dataset
x <- plot(wt, mpg, main="Scatterplot Example",
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19) 

LinearFitCI(x, wt, mpg, 0.95)