# Collective incentives reduce over-exploitation of social information in unconstrained human groups

This repository contains the full data and scripts to reproduce all analyses and figures as well as the experimental game itself used in 

**Deffner, D., Mezey, D., Kahl, B., Schakowski, A., Romanczuk, P., Wu, C. & Kurvers, R. (in press at Nature Communications):**

****"Collective incentives reduce over-exploitation of social information in unconstrained human groups"****

[![DOI](https://zenodo.org/badge/648584677.svg)](https://zenodo.org/doi/10.5281/zenodo.10650332)

The preprint can be found here: https://psyarxiv.com/p3bj7/

"master_file.r" sources all other scripts and implements the entire project workflow from raw Unity data to plots in the manuscript.

The "Scripts" folder contains all relevant R scripts for data processing and analysis:

- "functions.R" loads all required functions and packages
- "data_prep.R" runs data preparation script to construct dataframes for analysis from raw Unity outputs
- "HiddenMarkovModels_prep.R" contructs extended dataframe and stan data list for Social Hidden Markov Decision model
- "Behavior_prep.R" computes behavioral summary statistics for each player/group and round
- "Behavior_individual_level.R" runs and plots behavioral analyses at individual level
- "Behavior_group_level.R" runs and plots behavioral analyses at group level
- "Behavior_main_plot" produces main behavioral plot (Fig.2)
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

***Unity experiment files***

TLDR: If you just want to play the experimental game (as a solo participant), extract all files in "CoinScrounge_ClientBuild.zip" and open "CoinScrounge.exe". After the application has started, click "Start Game". If you want anything else, read on!

"CoinScrounge_Source.zip" is the Unity project (using version Unity 2020.3.21f1). Note that it has been stripped of all 3D Models, animations and textures that were not made by ourselves. This means that opening and running it will result in missing/blank visuals, errors and missing assets. If you want to actually run the experiment, use the built executables instead. Nevertheless, all prefabs, scenes and scripts are included, which lets you see the entire project setup and source code.
The code and prefabs are separated into modules inside the Assets/ directory. The project starts with the "StartingScene" scene.

"CoinScrounge_ClientBuild.zip" acts as the application that the participant interacts with. It can also be used as a host, or server, but the dedicated "CoinScrounge_ServerBuild.zip" runs as a console application without any of the visuals. Both builds include a "settings.avr" file which can be used to change parameters/settings as well as reveal some hidden options. Pressing RightCtrl+Tab within the client will bring up a debug-screen with some additional options.
