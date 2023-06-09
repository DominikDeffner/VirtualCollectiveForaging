# Quantifying latent social decision-making in unconstrained human collectives

This repository contains the scripts to reproduce all analyses and figures in 

**Deffner, D., Mezey, D., Kahl, B., Schakowski, A., Wu, C., Romanczuk, P. & Kurvers, R. (submitted):**

****"Quantifying latent social decision-making in unconstrained human collectives"****

"master_file.r" sources all other scripts and implements the entire project workflow from raw Unity data to plots in the manuscript.

The "Scripts" folder contains all relevant R scripts for data processing and analysis:

- "functions.R" loads all required functions and packages
- "data_prep.R" runs data preparation script to construct dataframes for analysis from raw Unity outputs
- "HiddenMarkovModels_prep.R" contructs extended dataframe and stan data list for Social Hidden Markov Decision model
- "Behavior_prep.R" computes behavioral summary statistics for each player/group and round
- "Behavior_individual_level.R" runs and plots behavioral analyses at individual level
- "Behavior_group_level.R" runs and plots behavioral analyses at group level
- "Behavior_solitary.R" runs and plots behavioral analyses for solo control condition
- "HiddenMarkovModels_baseline.R" runs baseline Social Hidden Markov Decision model and produces main text plots
- "Viterbi.r" computes most likely state sequences for all participants through Viterbi algorithm
- "HiddenMarkovModels_ESM_Plots.R" produces supplementary HMM plots
- "HiddenMarkovModels_temporal.R" runs and plots temporal Social Hidden Markov Decision model
- "CollectiveDynamics.R" runs and plots collective visual-spatial dynamics analyses using time-lagged Gaussian-process regressions

The "Stan model code" folder contains stan files for the (baseline and time-varying) Social Hidden Markov Decision model as well as the time-lagged Gaussian-process model

- "m_SHMDM.stan" is the baseline HMM (called in "HiddenMarkovModels_baseline.R")
- "m_SHMDM_temporal.stan" is the time-varying HMM (called in "HiddenMarkovModels_temporal.R")
- "m_temporal_success.stan" is monotonic-effect model for success over time (called in "HiddenMarkovModels_temporal.R")
- "m_time_laggedGP.stan" is the time-lagged Gaussian-process model to analyze collective visual-spatial dynamics (called in "CollectiveDynamics.R")

The "Data" folder contains all raw data for both "Group" and "Solo" conditions. Each sub-folder contains data from one experimental session. There are separate .txt files for participant demographics and for player and patch data from each round (indexing starts at 0). Variable names should be quite descriptive, but please get in touch in case anything is unclear.

***Software requirements***
The analysis code was written in R 4.0.3. Statistical models are fit using the Stan MCMC engine via the rstan (2.21.2) and cmdstanr (0.5.3) packages, which require a C++ compiler. Installation  instructions are available at https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started and https://mc-stan.org/cmdstanr/articles/cmdstanr.html. See also the Stan user guide at https://mc-stan.org/users/documentation. The rethinking package (2.12) is required to process fitted model outputs (installation instructions at http://xcelab.net/rm/software/).
