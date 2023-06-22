
####
###
##
# Script to compute state-dependent variables, state predictors and other variables for SOCIAL HIDDEN MARKOV DECISION MODEL
##
###
####

#Load relevant data files
load("data/dat_players")
load("data/dat_patches")

#Exclude tracks with missing data; there are two rounds (out of 160) where we have gaps in the time series due to technical error
dat_players <- dat_players[-which(dat_players$Group %in% c(2,21) & dat_players$Round == 3),]


####
###
##
# Compute state-dependent distributions
# a) Turning angles
# b) (Smallest) closed angles / Relative Bearing
# c) (Smallest) change in distance to exploiting players
##
###
####

#a) Calculate turning angles using momentuHMM package

dat_players$angle <- NA
for (id in unique(dat_players$track_id)) {
  d <- momentuHMM::prepData(dat_players[dat_players$track_id == id, c("PositionX","PositionZ")],coordNames=c("PositionX","PositionZ"))
  dat_players$angle[dat_players$track_id == id] <- d$angle
  print(id)
}



# b) Calculate smallest closed angle (called "relative bearing" in the ms) between focal player and every other player
#The closed angle is defined as the absolute angle between the focal player's orientation vector and the vector between 
#the center of both agents. If a focal agent faces directly towards another agent the closed angle will be zero. 

dat_players$closed <- NA

for (ID in 1:max(dat_players$id)) {
  print(ID)
  for (round in 1:4){
    if(length(dat_players$PositionX[which(dat_players$id == ID & dat_players$Round == round)]) > 0){
      
      for (t in 1:max(dat_players$t)) {
        print(t)
        idx_agent <- which(dat_players$id==ID & dat_players$Round == round & dat_players$t == t)
        
        orient_agent <- dat_players$orient[idx_agent]
        x_agent <- dat_players$PositionX[idx_agent]
        y_agent <- dat_players$PositionZ[idx_agent]
        
        group <- unique(dat_players$Group[dat_players$id==ID])
        
        closed_angles <- c()
        ids <- unique(dat_players$id[which(dat_players$Group == group)])
        
        for (target_ID in ids[-which(ids==ID)]) {
          
          x_dist <-  dat_players$PositionX[which(dat_players$id==target_ID & dat_players$Round == round   & dat_players$t == t)] - x_agent
          y_dist <-  dat_players$PositionZ[which(dat_players$id==target_ID & dat_players$Round == round   & dat_players$t == t)] - y_agent
          
          angle <- heading_to_radians(x_dist, y_dist)
          closed_angles <- c(closed_angles, pi - abs(abs(angle - orient_agent) - pi) )
        }
        
        dat_players$closed[idx_agent] <- min(closed_angles)
        
      }#t
    }
  }#round
}#ID



# c) Compute smallest change in distance to all (exploiting) players
# If an agent moves directly towards exploiting agent, their relative change in distance will be equal to the maximum velocity (2 m/s)

dist <- matrix(0, nrow = nrow(dat_players), ncol = 3)
deltadist <-  matrix(0, nrow = nrow(dat_players), ncol = 3)

for (ID in unique(dat_players$id)) {
  
  print(ID)
  group <- unique(dat_players$Group[dat_players$id==ID])
  ids <- unique(dat_players$id[which(dat_players$Group == group)])
  
  for (round in 1:4){
    for (t in dat_players$t[which(dat_players$id == ID & dat_players$Round == round & dat_players$IsAllowedToMove =="True")]  ) {
      print(t)
      
      #right index for focal player
      index <- which(dat_players$id==ID  & dat_players$Round == round & dat_players$t == t)
      
      if (length(index) >0){
        
        #Create vector for distance to each other player
        dist[index,] <- sapply(ids[-which(ids == ID)], function(x) Euclidian_distance(dat_players$PositionX[index],
                                                                                      dat_players$PositionX[which(dat_players$id==x & dat_players$Round == round   & dat_players$t == t)], 
                                                                                      dat_players$PositionZ[which(dat_players$id==ID & dat_players$Round == round  & dat_players$t == t)],
                                                                                      dat_players$PositionZ[which(dat_players$id==x & dat_players$Round == round   & dat_players$t == t)]))
        
        
        #Now we have distance to all other group members; we want for each time, change in distance to any exploiting player
        
        if(t > 1){
          if(dat_players$id[index] == dat_players$id[which(dat_players$id==ID  & dat_players$Round == round   & dat_players$t == t-1)]){
            
            deltadist[index,]     <- sapply(1:3, function(x){
              if (dat_players$IsExtracting[which(dat_players$id== ids[-which(ids == ID)][x]  & dat_players$Round == round   & dat_players$t == t-1)] == "True"){
                dist[index,x] - dist[which(dat_players$id==ID  & dat_players$Round == round   & dat_players$t == t-1), x] 
              }else{
                NA
              }
            } )
            
          } else {
            deltadist[index,] <- NA
          }
        } else {
          deltadist[index,] <- NA
        }
      }
    }#round
    print(t)
  }#t
}#ID


#Now we compute for each time step the smallest change in distance to any exploiting player
dat_players$mindeltadist <- sapply(1:nrow(deltadist), function(i){
  if (dat_players$t[i] >1){
    #Here we need to make sure that a focal player hasn't just stopped exploiting
    if ( (dat_players$IsAllowedToMove[i] == "True" & dat_players$IsAllowedToMove[i-1] != "True" )| all(is.na(deltadist[i,]))==TRUE){
      NA
    } else{
      min(deltadist[i,], na.rm = TRUE)
    } 
  } else {
    NA
  } }  )





####
###
##
# Compute state-switching predictors
# a) Visibility
# b) Time since last success
# c) Distance to closest visible patch
# d) Number of others at closest visible patch
##
###
####



# a) Compute binary indicator variables for 1) whether they are currently seeing any other player and 2) another exploiting agent

dat_players$exploit_visible <- NA
dat_players$any_visible <- NA

for (ID in 1:max(dat_players$id)) {
  print(ID)
  for (round in 1:4){
    if(length(dat_players$PositionX[which(dat_players$id == ID & dat_players$Round == round)]) > 0){
      
      for (t in 1:max(dat_players$t)) {
        idx_agent <- which(dat_players$id==ID & dat_players$Round == round & dat_players$t == t)
        visible <- dat_players$VisibleOthers[idx_agent][[1]]
        
        #Is anyone visible?
        dat_players$any_visible[idx_agent] <- ifelse(sum(visible) > 0, 1, 0)
        
        #Is exploiting player visible?
        if (sum(visible) > 0){
          dat_players$exploit_visible[idx_agent] <- ifelse(any(dat_players$IsExtracting[which(dat_players$PlayerID %in% visible &
                                                                                                dat_players$Round == round & 
                                                                                                dat_players$Group == unique(dat_players$Group[idx_agent]) &
                                                                                                dat_players$t == t)] == "True"), 1, 0)
        } else {
          dat_players$exploit_visible[idx_agent] <- 0
        }
      }#t
    }
  }#round
}#ID


# b) Compute time since last successful exploitation
dat_players$TimeSinceExploit <- NA

for (t in which(dat_players$IsAllowedToMove == "True")){
  print(t)
  
  t_exploit <- dat_players$t[which(dat_players$id == dat_players$id[t] & dat_players$Round == dat_players$Round[t] & dat_players$IsAllowedToMove == "False")]
  
  if (dat_players$t[t] <= min(t_exploit)){
    dat_players$TimeSinceExploit[t] <- dat_players$t[t]
  }  else {
    dat_players$TimeSinceExploit[t] <- dat_players$t[t] - max(t_exploit[which(t_exploit <= dat_players$t[t] )])
    
  }
  
}


#Compute c) distance and d) number of other players at nearest visible patch

#First, we need to store which patch players are currently exploiting
dat_players$Patch_Exploit <- 0

for (t in which(dat_players$IsExtracting == "True")) {
  print(t)
  
  patch <- dat_patches$unique_patch_id[which(dat_patches$id == dat_players$id[t] &
                                               dat_patches$Round == dat_players$Round[t] &
                                               dat_patches$PatchEvent %in% c("EXTRACT", "DISCOVER") &
                                               ( abs(dat_players$Time[t] - dat_patches$Time) < 2))]
  
  
  dat_players$Patch_Exploit[t] <- ifelse(length(patch)==0, 0, patch)
}


# Next, we store which patch exploitation events (which players and which patches?) players observed

dat_players$VisibleExploitPlayers <- vector(mode = "list", length = nrow(dat_players))
dat_players$VisibleExploitPatches <- vector(mode = "list", length = nrow(dat_players))

for (t in which(dat_players$IsAllowedToMove == "True" & sapply(1: nrow(dat_players), function(i)  sum(dat_players$VisibleOthers[i][[1]] )) > 0 ) ){
  print(t)
  group <- dat_players$Group[t]
  
  round <- dat_players$Round[t]
  id    <- dat_players$id[t]
  time    <- dat_players$t[t]
  
  ids_others <- unique(dat_players$id[which(dat_players$Group == unique(dat_players$Group[dat_players$id == id ] ))])
  ids_others <- ids_others[-which(ids_others ==  id)]
  player_ids <- sapply(ids_others, function(i) unique(dat_players$PlayerID[dat_players$id == i]) )
  
  
  dat_players$VisibleExploitPlayers[[t]] <- sapply(dat_players$VisibleOthers[t][[1]][dat_players$VisibleOthers[t][[1]] > 0], function(i){
    if(dat_players$Patch_Exploit[dat_players$PlayerID == i & 
                                 dat_players$Round == round &
                                 dat_players$Group == group &
                                 dat_players$t == time] > 0 ){
      return(i) }
    else{return(0)}} )
  
  
  if (sum(dat_players$VisibleExploitPlayers[[t]])>0){
    
    dat_players$VisibleExploitPatches[[t]] <- unlist(sapply(dat_players$VisibleExploitPlayers[t][[1]], function(i){
      return( dat_players$Patch_Exploit[dat_players$PlayerID == i & dat_players$Round == round & dat_players$t == time &  dat_players$Group == group]    )
    } ))
    
  } 
  
}#t


# Now  we can compute distance and number of other players at nearest visible patch
dat_players$DistPatch <- NA
dat_players$NumberPatch <- NA

for (t in  which(sapply(1: nrow(dat_players), function(i)  sum(dat_players$VisibleExploitPlayers[[i]] )) > 0)   ){
  print(t)
  Visible_exploiting_players <- dat_players$VisibleExploitPlayers[[t]]
  Visible_exploiting_players <- Visible_exploiting_players[Visible_exploiting_players != 0]
  
  Visible_exploited_patches <- dat_players$VisibleExploitPatches[[t]]
  
  ID    <- dat_players$PlayerID[t]
  group <- dat_players$Group[t]
  round <- dat_players$Round[t]
  time  <- dat_players$t[t]
  
  #Compute distances to all visible exploiting players
  distances <- sapply( Visible_exploiting_players , function(x)  Euclidian_distance(dat_players$PositionX[which(dat_players$PlayerID==ID & dat_players$Group == group & dat_players$Round == round & dat_players$t == time)],
                                                                                    dat_players$PositionX[which(dat_players$PlayerID==x & dat_players$Group == group & dat_players$Round == round & dat_players$t == time)], 
                                                                                    dat_players$PositionZ[which(dat_players$PlayerID==ID & dat_players$Group == group & dat_players$Round == round & dat_players$t == time)],
                                                                                    dat_players$PositionZ[which(dat_players$PlayerID==x & dat_players$Group == group & dat_players$Round == round & dat_players$t == time)]))  

  #Distance to nearest visible patch
  dat_players$DistPatch[t] <- min(distances)
  
  #Players at nearest visible patch
  dat_players$NumberPatch[t] <- length(which(Visible_exploited_patches == Visible_exploited_patches[which.min(distances)]))
  
  }


#Recode to 0 corresponds to default where only one other player is exploiting a patch
dat_players$NumberPatch <- dat_players$NumberPatch-1


####
###
##
# Compute some other indicator variables
##
###
####



#Create indicator variable for whether player has exploited a patch in the last time step
#This is used later to "force" agent into individual exploration because we know they could not observe any social information

dat_players$Exploit <- rep(0, nrow(dat_players))

for (t in 2:nrow(dat_players)) {
  if(dat_players$track_id[t] == dat_players$track_id[t-1] && dat_players$IsAllowedToMove[t-1] == "False"){
    dat_players$Exploit[t] = 1
  }
}

#Create indicator variable for whether someone in group is currently exploiting or not
dat_players$any_exploit <- sapply(1:nrow(dat_players), function (i) {
  print(i)
  group <- dat_players$Group[i]
  round <- dat_players$Round[i]
  t <- dat_players$t[i]
  ids <- unique(dat_players$id[which(dat_players$Group == group)])
  ids <- ids[-which(ids == dat_players$id[i])]
  
  ifelse( any(dat_players$IsExtracting[which(dat_players$id %in% ids &
                                               dat_players$Round == round &
                                               dat_players$t == t)] == "True")  , 1, 0)
  
})


#Create indicator variable for whether any other player has exploited a patch in the last time step
#This is used to determine whether any social information was available

dat_players$No_Exploit <- rep(0, nrow(dat_players))

for (t in 2:nrow(dat_players)) {
  print(t)
  if(dat_players$track_id[t] == dat_players$track_id[t-1] && dat_players$any_exploit[t-1] == 0){
    dat_players$No_Exploit[t] = 1
  }
}


#Create indicator variable for whether round is first round in a given environment

dat_players$First <- c()

for (i in 1:nrow(dat_players)) {
  print(i)
  if (dat_players$Round[i]==1){
    dat_players$First[i] <- 1
  } else {
    if (dat_players$Env[i] %in% unique(dat_players$Env[dat_players$id == dat_players$id[i] & dat_players$Round < dat_players$Round[i] ]  )){
      dat_players$First[i] <- 0
    } else {
      dat_players$First[i] <- 1
    }
  }
}

#Save extended dat_players data file for further analyses
save(dat_players, file = "data/dat_players_extended")



####
###
##
# Prepare data for stan model
##
###
####

load("data/dat_players_extended")

#Remove all time points where players exploit a patch; we don't want to model those time steps
dat_players <- dat_players[-which(dat_players$IsAllowedToMove == "False"),]


#Standardize predictors
dat_players$DistPatch        <- standardize(dat_players$DistPatch)
dat_players$TimeSinceExploit <- standardize(dat_players$TimeSinceExploit)

# set NAs to out-of-range values
dat_players$angle[is.na(dat_players$angle)] <- -10
dat_players$closed[is.na(dat_players$closed)] <- -10
dat_players$mindeltadist[is.na(dat_players$mindeltadist)] <- -10


dat_players$DistPatch[is.na(dat_players$DistPatch)] <- -10
dat_players$NumberPatch[is.na(dat_players$NumberPatch)] <- -10
dat_players$TimeSinceExploit[is.na(dat_players$TimeSinceExploit)] <- -10


#Remove time points where no group member was currently exploiting
dat_players <- dat_players[dat_players$any_exploit == 1,]

#Store data as list (required by stan)
stan.data <- list(T=nrow(dat_players),
                  track_id = as.numeric(dat_players$track_id),
                  id = dat_players$id, 
                  N_id = length(unique(dat_players$id)),
                  group = dat_players$Group,
                  N_group = length(unique(dat_players$Group)),
                  angles=dat_players$angle, 
                  deltadist = dat_players$mindeltadist,
                  closed=dat_players$closed, 
                  Exploit = dat_players$Exploit,
                  exploit_visible = dat_players$exploit_visible,
                  Patch_Prox = dat_players$DistPatch,
                  Patch_Number = dat_players$NumberPatch, 
                  TimeSinceExploit = dat_players$TimeSinceExploit,
                  any_exploit = dat_players$any_exploit,
                  No_Exploit = dat_players$No_Exploit,
                  TimeInRound = ceiling(dat_players$t/60),
                  N=2) 

#Create unique ids for each track
stan.data$track_id <- sapply(1: stan.data$T, function(x){
print(x)  
which(unique(stan.data$track_id) == stan.data$track_id[x]) 
})

#Create indices for environment and incentive structure
stan.data$Environment <- as.integer(ifelse(dat_players$Env == "C", 1, 2))
stan.data$Incentives <- as.integer(ifelse(dat_players$Pay == "Coop", 1, 2))

#Create variables needed for parallelized computation of likelihood
stan.data$tracks <- unique(stan.data$track_id)
stan.data$N_tracks <- length(stan.data$tracks)
stan.data$index  <- sapply(stan.data$tracks, function(id) which(stan.data$track_id == id)[1]-1)
stan.data$T_track <- sapply(stan.data$tracks, function(id) length(which(stan.data$track_id == id)))

save(stan.data, file = "data/stan.data")


#######
#####
###
# FIT MODELS IN STAN
###
#####
#######


#Specify initial values for state-dependent variables
inits <- function () list(mu_closed = c(1, 0.5),mu_distance = c(0,-1), xangle=c(5,30), yangle=c(0,0), sigma_closed = c(0.5,0.5), sigma_distance = c(0.5,0.5))

#Fit baseline model
m_parallel <- cmdstan_model("m_SHMDM.stan", cpp_options = list(stan_threads = TRUE))
fit_parallel <- m_parallel$sample(stan.data, chains = 2, parallel_chains = 2, threads_per_chain = 50, refresh = 10, init = inits, iter_warmup = 1500 ,adapt_delta = 0.8, iter_sampling = 1000)
fit <- rstan::read_stan_csv(fit_parallel$output_files())
save(fit, file = "fit_HMM")
s <- extract.samples(fit)

#Calculate individual weights
baseline_weight <- data.frame(id = 1:160, Incentives = NA,  Concentrated = NA, Distributed = NA)
stay_weight <- data.frame(id = 1:160, Incentives = NA,  Concentrated = NA, Distributed = NA)
dist_weight <- data.frame(id = 1:160, Incentives = NA,  Concentrated = NA, Distributed = NA)
time_weight <- data.frame(id = 1:160, Incentives = NA,  Concentrated = NA, Distributed = NA)
numb_weight <- data.frame(id = 1:160, Incentives = NA,  Concentrated = NA, Distributed = NA)

#Loop over all individuals
for (id in 1:160) {
  print(id)
  incentives <- unique(stan.data$Incentives[stan.data$id==id])
  group <- unique(stan.data$group[stan.data$id==id])
  baseline_weight$Incentives[id] <- incentives
  time_weight$Incentives[id] <- incentives
  dist_weight$Incentives[id] <- incentives
  numb_weight$Incentives[id] <- incentives
  
  baseline_weight$Concentrated[id] <- mean(inv_logit((s$logit_p_I_S[,incentives,1] + s$offset_ID[,id,     ifelse(incentives==1, 1, 3) ] + s$offset_Group[,group,ifelse(incentives==1, 1, 3) ] ) +
                                                       (s$b_vis[,incentives,1] + s$offset_ID[,id, 4 + ifelse(incentives==1, 1, 3) ] + s$offset_Group[,group,4 +ifelse(incentives==1, 1, 3) ] )   ))
  
  baseline_weight$Distributed[id]  <- mean(inv_logit((s$logit_p_I_S[,incentives,2] + s$offset_ID[,id, ifelse(incentives==1, 2, 4) ]+ s$offset_Group[,group,ifelse(incentives==1, 2, 4) ] ) +
                                                       (s$b_vis[,incentives,2] + s$offset_ID[,id, 4 + ifelse(incentives==1, 2, 4) ] + s$offset_Group[,group,4 +ifelse(incentives==1, 2, 4) ])   ))
  
  stay_weight$Concentrated[id] <- mean(inv_logit(s$logit_p_S_S[,incentives,1] + s$offset_ID[,id, 20 + ifelse(incentives==1, 1, 3) ]+ s$offset_Group[,group,20 +ifelse(incentives==1, 1, 3) ] ) )
  stay_weight$Distributed[id]  <- mean(inv_logit(s$logit_p_S_S[,incentives,2] + s$offset_ID[,id, 20 + ifelse(incentives==1, 2, 4) ]+ s$offset_Group[,group,20 +ifelse(incentives==1, 2, 4) ]  ) )
  
  time_weight$Concentrated[id] <- mean(s$b_time[,incentives,1] + s$offset_ID[,id, 8 + ifelse(incentives==1, 1, 3) ] + s$offset_Group[,group,8 +ifelse(incentives==1, 1, 3) ]) 
  time_weight$Distributed[id]  <- mean(s$b_time[,incentives,2] + s$offset_ID[,id, 8 + ifelse(incentives==1, 2, 4) ] + s$offset_Group[,group,8 +ifelse(incentives==1, 2, 4) ]) 
  
  dist_weight$Concentrated[id] <- mean(s$b_dist[,incentives,1] + s$offset_ID[,id, 12 + ifelse(incentives==1, 1, 3) ] + s$offset_Group[,group,12 +ifelse(incentives==1, 1, 3) ] ) 
  dist_weight$Distributed[id]  <- mean(s$b_dist[,incentives,2] + s$offset_ID[,id, 12 + ifelse(incentives==1, 2, 4) ] + s$offset_Group[,group,12 +ifelse(incentives==1, 2, 4) ] ) 
  
  numb_weight$Concentrated[id] <- mean(s$b_numb[,incentives,1] + s$offset_ID[,id, 16 + ifelse(incentives==1, 1, 3) ]  + s$offset_Group[,group,16 +ifelse(incentives==1, 1, 3) ]) 
  numb_weight$Distributed[id]  <- mean(s$b_numb[,incentives,2] + s$offset_ID[,id, 16 + ifelse(incentives==1, 2, 4) ] + s$offset_Group[,group, 16 + ifelse(incentives==1, 2, 4) ] ) 
}

#save(baseline_weight, file = "mean_baseline_weight")
#save(dist_weight, file = "mean_dist_weight")
#save(numb_weight, file = "mean_numb_weight")
#save(time_weight, file = "mean_time_weight")
#save(stay_weight, file = "mean_stay_weight")


#Fit temporal model
m_parallel <- cmdstan_model("m_SHMDM_temporal.stan", cpp_options = list(stan_threads = TRUE))
fit_parallel <- m_parallel$sample(stan.data, chains = 2, parallel_chains = 2, threads_per_chain = 50, refresh = 10, init = inits, iter_warmup = 1500 ,adapt_delta = 0.8, iter_sampling = 1000)
fit_temporal <- rstan::read_stan_csv(fit_parallel$output_files())
save(fit_temporal, file = "fit_HMM_temporal")


