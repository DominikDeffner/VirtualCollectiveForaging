
# Main analysis script for collective foraging experiment ("Coin Scrounge")



#Set working directory
setwd("~/GitHub/CoinScrounge")


#Source functions and packages
source("functions.r")

#Run full data preparation script to construct dataframes used for analysis from raw outputs
#source("data_prep.r")

#Alternatively, you can directly load prepared data
for(i in list.files(path = "data",  pattern="dat_*")) load(paste0("data/",i))

#Run preparation script for extended data file and SHMDM data
source("HiddenMarkovModels_prep.R")

#Compute behavioral summary statistics for each player/group and round
source("Behavior_prep.R")

#Runs and plots behavioral analyses for solitary foraging control condition
source("Behavior_individual_level.R")

#Runs and plots behavioral analyses for solitary foraging control condition
source("Behavior_group_level.R")

#Runs and plots behavioral analyses for solitary foraging control condition
source("Behavior_solitary.R")





