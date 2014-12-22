# Version 1.0, March 2000
# Copyright (C) 2014 Dubiel Julien.
# 14 Rue Voltaire, 94276 Kremlin Bicêtre, France
#
# Everyone is permitted to copy and distribute verbatim copies
# of this license document, and changing it is allowed as long
# as the name is changed.

# Ok, the purpose of this license is simple
# and you just DO WHAT THE FUCK YOU WANT TO.


set.seed(849)
# Just use set.seed() to always get the same random generated numbers
#
# For instance:
#
#   set.seed(24)      ~ Seed rdm with 24
#   a <- rnorm(20)    ~ Generates 20 rdm numbers
#   b <- rnorm(20)    ~ Generates 20 rdm numbers
#   set.seed(33)      ~ Seed rdm with 33
#   c <- rnorm(20)    ~ Generates 20 rdm numbers
#
# Here a = b & a != c


##############
# Parameters #
##############

x_min <- -1
x_max <- 10

nb_of_hidden_neurons <- 4

nb_rdm_values <- 100
maxiterations <- 100
forby <- 1

points_color <- "purple"
line_color <- "red"

###################
# Generating DATA #
###################

x <- runif(nb_rdm_values, x_min, x_max) # ~ Generate rdm_values numbers from x_min to x_max
x <- sort(x)                            # ~ Order them and we obtain values for X axis
y <- cos(x) + 0.2*rnorm(x)              # ~ Generates Y corresponding to Y=sin(X) and add approximation error

library(nnet)


################################
# Plot the result in real-time #
################################
initial_weights = rnorm(nb_of_hidden_neurons+nb_of_hidden_neurons+nb_of_hidden_neurons+1)
for(i in seq(from=0, to=maxiterations, by=forby)) {
  
  cat("\014")  # clear console
  
  # Initializing nnet with following parameters: (http://cran.r-project.org/web/packages/nnet/nnet.pdf)
  #   
  #   size   = number of units in the hidden layer. Can be zero if there are skip-layer units
  #   maxit  = maximum number of iterations. Default 100.
  #   linout = switch for linear output units. Default logistic output units.  
  #   Wts    = initial parameter vector. If missing chosen at random. (initial weights)
  nn <- nnet(x, y, 
             size=nb_of_hidden_neurons, 
             maxit=i, 
             linout=TRUE,
             Wts=initial_weights
        )
  
  plot(x, y, col=points_color, pch=20)                      # Plot dots at (x,y)
  plot(cos, x_min, x_max, add=TRUE)                         # Plot function between x_min and x_max
  x1 <- seq(x_min, x_max, by=0.1)                           # Creating vect of values from x_min to x_max
  lines(x1, predict(nn, data.frame(x=x1)), col=line_color)  # Create the line corresponding to the prediction of nnet with x1
  title(main = paste(i,"iterations", sep=" "))
  Sys.sleep(0.2)
}

##################################
# Visualizing the neural network #
##################################

nn <- nnet(x, y, size=nb_of_hidden_neurons, maxit=maxiterations, linout=TRUE)
#library(devtools)
#source('nnet_plot.r')
#plot.nnet(nn)
