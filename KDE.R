#Sys 6018
#wdr5ft
#KDE Function

library(ggplot2)
library(rgl)
library(lattice)

#########################################################################################################
#                                            UNIVARIATE KDE
#########################################################################################################

#-------------------------------------------creating function--------------------------------------------

#helper function to get the y-value from the standard normal distribution
pFunc = function(x) {
  term1 = 1/(sqrt(2*pi))
  term2 = exp((-x^2)/2)
  return(term1*term2)
}

#outer function that applies inner function to range of sample
kde.full = function(samp, h, step = .1) {
  
  #function that takes a x* and returns the point on the KDE function
  est.kde.univariate = function(x, sample, bandwidth) {
    #get distances between x* and the points in the sample
    distances = (x - sample)/bandwidth
    #put distances into k()
    kvals = lapply(distances, pFunc)
    ksum = do.call(sum, kvals)
    return(ksum/(length(sample)*bandwidth))
  }

  #get range of sample with user-specified granularity
  xrange = seq(min(samp), max(samp), step)
  
  #apply inner function to each x value in range
  kdeCurve = lapply(xrange, function(q) {est.kde.univariate(q, samp, h)})
  
  #put into df
  distDF = as.data.frame(cbind(xrange, kdeCurve))
  colnames(distDF) = c('xval', 'yval')
  
  return(distDF)
}

#-------------------------------------------plotting test data-------------------------------------------

#create bimodal data
dist1 = rnorm(500, mean = 5, sd = 3)
dist2 = rnorm(500, mean = 15, sd = 3)
bimodal = c(dist1, dist2)

#get KDE curves with varying bandwidth
testcurve_2 = kde.full(bimodal, .2, .1)
testcurve_5 = kde.full(bimodal, .5, .1)
testcurve1 = kde.full(bimodal, 1, .1)
testcurve2 = kde.full(bimodal, 2, .1)
testcurve3 = kde.full(bimodal, 3, .1)

#plot
ggplot() + geom_histogram(data = as.tibble(bimodal), aes(value, ..density..), binwidth = .5, color = 'white', fill = 'grey60') +
           geom_path(data = testcurve1, aes(x = unlist(xval), y = unlist(yval), colour = 'h = 1')) +
           geom_path(data = testcurve2, aes(x = unlist(xval), y = unlist(yval), colour = 'h = 2')) +
           geom_path(data = testcurve3, aes(x = unlist(xval), y = unlist(yval), colour = 'h = 3')) +
           geom_path(data = testcurve_2, aes(x = unlist(xval), y = unlist(yval), colour = 'h = .2')) +
           geom_path(data = testcurve_5, aes(x = unlist(xval), y = unlist(yval), colour = 'h = .5')) +
           scale_colour_manual('', values = c('h = 1'='red', 'h = 2'='blue', 'h = 3'='purple',
                                              'h = .2'='green4', 'h = .5'='orange'))


#########################################################################################################
#                                            BIVARIATE KDE
#########################################################################################################

#-------------------------------------------creating function--------------------------------------------

#k function for bivariate kde
biPFunc = function(x, y) {
  term1 = 1/(2*pi)
  z = (x^2)+(y^2)
  term2 = exp(-z/2)
  return(term1*term2)
}

#outer function to apply innder KDE function
bi.kde.full = function(samp, h, step = .1) {
  
  #KDE function that takes a single (x, y) pair, a sample which is a matrix of x and y, and bandwidth
  est.kde.bivariate = function(x, y, sample, bandwidth) {
    #get distances
    xvals = (x - sample[,1])/bandwidth
    yvals = (y - sample[,2])/bandwidth
    #give distances to k function
    kvals = mapply(biPFunc, xvals, yvals)
    ksum = sum(kvals)
    return(ksum/(nrow(sample)*bandwidth))
  }
  
  samp = as.tibble(samp)
  
  #create evenly spaced grid to apply KDE function to
  range = seq(min(samp), max(samp), step)
  plane = as.data.frame(expand.grid(range, range))
  colnames(plane) = c('x', 'y')
  
  #apply KDE function to plane
  zvals = numeric()
  
  for(i in 1:nrow(plane)) {
    z = est.kde.bivariate(plane$x[i], plane$y[i], samp, h)
    zvals = c(zvals, z)
  }
  
  output = as.data.frame(cbind(plane, unlist(zvals)))
  colnames(output) = c('x', 'y', 'z')
  
  return(output)
}

#-------------------------------------------plotting test data-------------------------------------------

#generate 2D data centered around 2 points, each (very) roughly normal
x1 = rnorm(100, mean = 1, sd = 2)
y1 = rnorm(100, mean = 1, sd = 2)
x2 = rnorm(100, mean = 7, sd = 2)
y2 = rnorm(100, mean = 7, sd = 2)

xFin = c(x1, x2)
yFin = c(y1, y2)

finalSample = cbind(xFin, yFin)

#apply function with different values of h (takes a while)
biH1 = bi.kde.full(finalSample, 1)
biH5 = bi.kde.full(finalSample, 5)
biH_2 = bi.kde.full(finalSample, .2)

#plot results
wireframe(biH1$z ~ biH1$x*biH1$y, shade = TRUE, aspect = c(1, 1),
          light.source = c(10,10,10), main = "h = 1",
          scales = list(z.ticks=.1, arrows=FALSE, col="black", font=10, tck=0.5),
          screen = list(z = 0, x = -75, y = 0), xlab = list('x'), ylab = list('y'), zlab = list('density'))

wireframe(biH5$z ~ biH5$x*biH5$y, shade = TRUE, aspect = c(1, 1),
          light.source = c(10,10,10), main = "h = 5",
          scales = list(z.ticks=.1, arrows=FALSE, col="black", font=10, tck=0.5),
          screen = list(z = 0, x = -75, y = 0), xlab = list('x'), ylab = list('y'), zlab = list('density'))

wireframe(biH_2$z ~ biH_2$x*biH_2$y, shade = TRUE, aspect = c(1, 1),
          light.source = c(10,10,10), main = "h = .2",
          scales = list(z.ticks=.1, arrows=FALSE, col="black", font=10, tck=0.5),
          screen = list(z = 0, x = -75, y = 0), xlab = list('x'), ylab = list('y'), zlab = list('density'))



