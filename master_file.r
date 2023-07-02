##################################################################################

#Quantifying latent social decision-making in unconstrained human collectives
#authored by Dominik Deffner (deffner@mpib-berlin.mpg.de)

#MASTER FILE to source all other scripts
#Running this file should allow you to reproduce the entire project workflow from raw Unity data to plots in the manuscript.

#However, note that the data preparation as well as the stan models can take quite some time (i.e., a few days). 
#If you have access to a multi-core cluster, the Hidden Markov stan models should take less than a day; otherwise, they take considerably longer. 
#You can of course reduce the number of iterations to make it run faster.

##################################################################################

#Set working directory (this might be a different location for you)
setwd("~/GitHub/VirtualCollectiveForaging/")

###
##
# Data preparation
##
###

#Source functions and packages (if you get error messages, please install packages that cause problems; see Readme for infos on Stan-related things)
source("Scripts/functions.R")

#Run full data preparation script to construct dataframes used for analysis from raw Unity data
source("Scripts/data_prep.R")

#Run preparation script for extended data file and Social Hidden Markov model data
source("Scripts/HiddenMarkovModels_prep.R")

#Compute behavioral summary statistics for each player/group and round
source("Scripts/Behavior_prep.R")

###
##
# Behavioral analyses and plots
##
###

#Runs and plots behavioral analyses at individual level
source("Scripts/Behavior_individual_level.R")

#Runs and plots behavioral analyses at group level
source("Scripts/Behavior_group_level.R")

#Runs and plots behavioral analyses for solo control condition
source("Scripts/Behavior_solitary.R")

###
##
# Computational analyses and plots
##
###


#Runs and plots computational analyses for baseline SOCIAL HIDDEN MARKOV DECISION MODEL
source("Scripts/HiddenMarkovModels_baseline.R")

#Computes most likely state sequences for all participants through Viterbi algorithm
source("Scripts/Viterbi.r")

#Plots additional computational results
source("Scripts/HiddenMarkovModels_ESM_Plots.R")

#Runs and plots computational analyses for temporal SOCIAL HIDDEN MARKOV DECISION MODEL
source("Scripts/HiddenMarkovModels_temporal.R")

#Runs and plots collective visual-spatial dynamics analyses using time-lagged Gaussian-process regressions
source("Scripts/CollectiveDynamics.R")


#THE END
##################################################################################


