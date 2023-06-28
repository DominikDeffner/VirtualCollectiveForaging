
###
##
# FUNCTIONS AND PACKAGES
##
###

#Load or install required packaged
packages <- c("cmdstanr", "rethinking", "scales", "RColorBrewer", "readr", "trajr", "momentuHMM",
              "tidyverse", "sp", "pracma", "gridExtra", "ggpubr", "berryFunctions")
lapply(packages, require, character.only = TRUE)

###
##
# Functions for movement analysis
##
###

Euclidian_distance <- function(x1,x2,y1,y2) sqrt( (x1-x2)^2 + (y1-y2)^2 )

Conv <- function(mu, kappa) list(xangle = cos(mu)*kappa, yangle = sin(mu)*kappa)

Conv2 <- function(xangle, yangle){
  mu = atan2(yangle,xangle)
  kappa = sqrt(xangle*xangle + yangle*yangle)
  return(list(mu = mu, kappa = kappa))
} 

dvonmises <- function(y, mu, kappa, log = FALSE) {
  if (any(kappa < 0)) {
    stop("kappa must be non-negative")
  }
  be <- besselI(kappa, nu = 0, expon.scaled = TRUE)
  out <- - log(2 * pi * be) + kappa * (cos(y - mu) - 1)
  if (!log) {
    out <- exp(out)
  }
  out
}


heading_to_radians <- function(ForwardX, ForwardZ){
  angle <- atan2(ForwardZ, ForwardX)
  if (angle < 0) angle <- angle + 2*pi
  return(angle)
}

degree_to_radians <- function(degree) return(degree * pi/180)

dvonmises <- function(y, mu, kappa, log = FALSE) {
  if (any(kappa < 0)) {
    stop("kappa must be non-negative")
  }
  be <- besselI(kappa, nu = 0, expon.scaled = TRUE)
  out <- - log(2 * pi * be) + kappa * (cos(y - mu) - 1)
  if (!log) {
    out <- exp(out)
  }
  out
}


###
##
# Plotting functions
##
###

#color stuff
x <- seq(from=0, to=1, by=0.2) # fake data
col.pal <- brewer.pal(length(x), "Set1") #create a pallette which you loop over for corresponding values

# Function to compute 90% HPD intervals and plot regression line
plot_regression_line <- function(samples, data, environment, color){
  
  s <- samples
  x_min <- min(data)
  x_max <- max(data)  
  
  # define sequence of weights to compute predictions
  x_seq <- seq( from=x_min , to=x_max , by=0.01 )
  
  # use link to compute mu
  # for each sample from posterior
  # and for each weight in weight.seq
  mu <- matrix(NA, length(s$lp__), length(x_seq))
  
  for (i in 1: length(s$lp__)) {
    for (j in 1:length(x_seq)) {
      mu[i,j] <- exp(s$alpha[i,environment] + s$weight[i,environment]*x_seq[j])
    }
  }
  
  # summarize the distribution of mu
  mu.mean <- apply( mu , 2 , mean )
  mu.PI <- apply( mu , 2 , PI , prob=0.9 )
  
  # plot a shaded region for 89% PI
  shade( mu.PI , x_seq , col =  alpha(color, alpha = 0.4))
  
  # plot the MAP line, aka the mean mu for each weight
  lines( x_seq , mu.mean, col = color )
  
  
}

###
##
# Other useful functions
##
###

#Get right session name 
get_session <- function(session, round, data) paste("session_",session,"_round_",round,"_",data, sep = "")

#Get minimum positive value
minpositive = function(x) min(x[x > 0])

#(Un)standarize variables 
standardize <- function(x) {
  x <- scale(x)
  z <- as.numeric(x)
  attr(z,"scaled:center") <- attr(x,"scaled:center")
  attr(z,"scaled:scale") <- attr(x,"scaled:scale")
  return(z)
}
unstandardize <- function(x) {
  scale <- attr(x,"scaled:scale")
  center <- attr(x,"scaled:center")
  z <- x*scale + center
  return( as.numeric(z) )
}

#Get most frequent element of numeric vector
Mode <- function(x){ as.numeric(names(sort(-table(x)))[1]) }
