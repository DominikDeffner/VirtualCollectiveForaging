# Quantifying latent social decision-making in unconstrained human collectives

This repository contains the scripts to reproduce all analyses and figures in 

****Deffner, D., Mezey, D., Kahl, B., Schakowski, A., Wu, C., Romanczuk, P. & Kurvers, R. (submitted): Quantifying latent social decision-making in unconstrained
human collectives****

The preprint can be found here:

"master_file.r" sources all other scripts and implements the entire project workflow from raw Unity data to plots in the manuscript.

The "Scripts" folder contains all relevant R scripts for data processing and analysis:

- "functions.R" loads all required functions and packages
- "data_prep.R" runs data preparation script to construct dataframes for analysis from raw Unity outputs
- "HiddenMarkovModels_prep.R" contructs extended dataframe and stan data list for Social Hidden Markov Decision model
- "Behavior_prep.R" computes behavioral summary statistics for each player/group and round
- "Behavior_individual_level.R" runs and plots behavioral analyses at individual level
- "Behavior_group_level.R" runs and plots behavioral analyses at group level
- "Behavior_solitary.R" runs and plots behavioral analyses for solo control condition


  
The "Data" folder contains all raw data for both "Group" and "Solo"


***Software requirements***
The analysis code was written in R 4.0.3. Statistical models are fit using the Stan MCMC engine via the rstan (2.21.2) and cmdstanr (0.5.3) packages, which require a C++ compiler. Installation  instructions are available at https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started and https://mc-stan.org/cmdstanr/articles/cmdstanr.html. See also the Stan user guide at https://mc-stan.org/users/documentation. The rethinking package (2.12) is required to process fitted model outputs (installation instructions at http://xcelab.net/rm/software/).
