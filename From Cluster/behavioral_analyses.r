# Behavioral analyses

#Load dat_patches, dat_players files and extended dat_players version
load("~/CoinScroung/dat_patches")
load("~/CoinScroung/dat_players_extended")
dat_players_extended <- dat_players
load("~/CoinScroung/dat_players")

####
###
##
# Individual-level analyses
##
###
####

### For each time series get index of joinings and discoveries as well as proximity, visibility and movement statistics

d <- data.frame(id = rep(1:160, 4), track_id =NA, Group = NA, round = rep(1:4, each =160),Coins = NA, Exploit = NA, discoveries = NA, joining = NA, scrounging = NA,joining_real = NA,scrounging_real = NA, exploit_observed=NA, MeanDistance = NA, SDDistance = NA, Visibility = NA, Sinuosity = NA, DC =NA, SDDC = NA, ConvexHull = NA, Pay = NA, Env = NA)
for (id in unique(dat_players$id)) {
  for (round in 1:4) {
    print(paste0("ID ", id, " Round ", round))
    
    #Get all patch discoveries  
    all_discoveries <- which(dat_patches$id==id & dat_patches$Round==round & dat_patches$PatchEvent=="DISCOVER")
    
    #Vectors for joinings and discoveries
    joining          <- 0
    discoveries <- 0
    
    for (i in all_discoveries) {
      if ( dat_patches$Time[i] == min(dat_patches$Time[which(dat_patches$unique_patch_id == dat_patches$unique_patch_id[i] & dat_patches$Round==round & dat_patches$PatchEvent=="DISCOVER")])){
        discoveries <- discoveries + 1
      } else {
        joining          <- joining + 1
  
      }
    }
    
    d$joining[d$id == id & d$round == round]      <- joining
    d$discoveries[d$id == id & d$round == round] <- discoveries
    
    # For the following metrics we omit rounds with missing data (8 out of 640)
    if(( id %in% c(5,6,7,8,81,82,83,84) & round == 3) == FALSE){
      
      #Get proportion of visible patches that agent joined
      
      times <- which(dat_players_extended$id == id & 
                       dat_players_extended$Round==round & 
                       dat_players_extended$IsAllowedToMove=='True' )
      
      #When were exploting players observed?
      observation_times <- times[which(sapply(times, function(i)  sum(dat_players_extended$VisibleExploitPlayers[[i]] ) > 0))]
      
      #Which exploited patches did player see?
      all_observed_exploits <- unique(unlist(sapply(observation_times, function(i) dat_players_extended$VisibleExploitPatches[[i]] )))
      all_observed_exploits <- all_observed_exploits[all_observed_exploits>0]                                                                           
      
      #Which of these observed patches did agent end up joining?
      joining_obs <- which(all_observed_exploits %in% dat_patches$unique_patch_id[all_discoveries])                          
      d$joining_real[d$id == id & d$round == round]      <- length(joining_obs)
      d$scrounging_real[d$id == id & d$round == round]  <- length(joining_obs)/length(all_observed_exploits)
      d$exploit_observed[d$id == id & d$round == round]  <- length(all_observed_exploits)
      
    
    
    #Calculate time spent exploiting in given round
    d$Exploit[which(d$id== id & d$round == round)] <- sum(na.omit(dat_players$IsAllowedToMove[which(dat_players$id== id & dat_players$Round == round)]) == "False") / 
                                                   length(na.omit(dat_players$IsAllowedToMove[which(dat_players$id== id & dat_players$Round == round)]))
    
    

     # Average distance to other players and visibility
     others_id <- unique(dat_players$id[dat_players$Group == dat_players$Group[dat_players$id == id & dat_players$Round]])[-id]
    
     dist_times <- c()
     vis_times <- c()
    
     for (t in unique(dat_players$t)) {
       x_agent <- dat_players$PositionX[dat_players$id == id & dat_players$Round == round & dat_players$t == t]
       y_agent <- dat_players$PositionZ[dat_players$id == id & dat_players$Round == round & dat_players$t == t]
    
    
       avg_dist_t <- mean(sapply(others_id, function(alter){ Euclidian_distance(x1 = x_agent,
                                                                                x2 = dat_players$PositionX[dat_players$id == alter & dat_players$Round == round & dat_players$t == t],
                                                                                y1 = y_agent,
                                                                                y2 = dat_players$PositionZ[dat_players$id == alter & dat_players$Round == round & dat_players$t == t])
       }#alter
       ) )
    
    
       avg_vis_t <- length(which(dat_players$VisibleOthers[dat_players$id == id & dat_players$Round == round & dat_players$t == t][[1]]>0))
    
    
       dist_times <- c(dist_times, avg_dist_t)
       vis_times  <- c(vis_times, avg_vis_t)
    
     }
 
     #We want to remove times where agent was extracting
     extraction_times <- which(dat_players$IsExtracting[dat_players$id == id & dat_players$Round == round] == "True")
     dist_times <- dist_times[-extraction_times]
     vis_times <- vis_times[-extraction_times]
    
     #Store average (and variability in) distance and average number of others in field of view
     d$MeanDistance[which(d$id== id & d$round == round)] <- mean(dist_times)
     d$SDDistance[which(d$id== id & d$round == round)]   <- sd(dist_times)
     d$Visibility[which(d$id== id & d$round == round)]   <- mean(vis_times)
    
    
    #Movement metrics
    Trajectory <- data.frame(x = dat_players$PositionX[which(dat_players$id== id & dat_players$Round == round)],
                             y = dat_players$PositionZ[which(dat_players$id== id & dat_players$Round == round)],
                          times = dat_players$t[which(dat_players$id== id & dat_players$Round == round)])
 

    d$Sinuosity[d$id == id & d$round == round] <- ifelse(any(is.na(Trajectory$x)), NA, TrajSinuosity2(TrajFromCoords(Trajectory)))
    d$DC[d$id == id & d$round == round] <- ifelse(any(is.na(Trajectory$x)), NA, mean(TrajDirectionalChange(TrajFromCoords(Trajectory))))
    d$SDDC[d$id == id & d$round == round] <- ifelse(any(is.na(Trajectory$x)), NA, sd(TrajDirectionalChange(TrajFromCoords(Trajectory))))
    
    #Area covered (area of convex hull polygon)
    if (any(is.na(Trajectory$x))){
      d$ConvexHull[d$id == id & d$round == round] <- NA
    } else {
    hpts <- chull(x = Trajectory$x, y = Trajectory$y)
    hpts <- c(hpts, hpts[1])
    xy.coords <- cbind(Trajectory$x, Trajectory$y)
    chull.coords <- xy.coords[hpts,]
    chull.poly <- Polygon(chull.coords, hole=F)
    d$ConvexHull[d$id == id & d$round == round] <- chull.poly@area
    }
    
  
    }#end if
    
    
    #Get collected coins
    index <- tail(which(dat_players$id == id & dat_players$Round == round),1)
    if (round == 1){
      d$Coins[which(d$id== id & d$round == round)] <- dat_players$CoinAmount[index]
    } else{
      d$Coins[which(d$id== id & d$round == round)] <- dat_players$CoinAmount[index] - dat_players$CoinAmount[tail(which(dat_players$id == id & dat_players$Round == round-1),1)]
    }
    
    #Store other information
    d$id[which(d$id== id & d$round == round)] <-  id
    d$round[which(d$id== id & d$round == round)] <- round
    d$Group[which(d$id== id & d$round == round)] <- unique(na.omit(dat_players$Group[dat_players$id == id] ))
    
    d$Pay[which(d$id== id & d$round == round)]      <- unique(dat_players$Pay[dat_players$id == id & dat_players$Round == round] )
    d$Env[which(d$id== id & d$round == round)]      <- unique(dat_players$Env[dat_players$id == id & dat_players$Round == round] )
    d$track_id[which(d$id== id & d$round == round)] <- unique(dat_players$track_id[which(dat_players$id== id & dat_players$Round == round)] )
    
    
  }
}


#Compute scrounging rate as number of joining events/number of potential joining events

for (id in unique(dat_players$id)) {
  print(id)
  for (round in 1:4) {
    ids_others <- unique(d$id[which(d$Group == unique(d$Group[d$id == id]))])
    ids_others <- ids_others[-which(ids_others == id)]
    discoveries_others <- sum(d$discoveries[which(d$id %in% ids_others & d$round == round)])
    d$scrounging[d$id == id & d$round == round] <- d$joining[d$id == id & d$round == round] / discoveries_others
     }
}



save(d, file = "d_rounds")

####
###
##
# Solo rounds
##
###
####

### For each time series get index of joinings and discoveries as well as proximity, visibility and movement statistics

d_solo <- data.frame(id = rep(1:40, 4), track_id =NA, round = rep(1:4, each =40),Coins = NA, Exploit = NA, discoveries = NA, Sinuosity = NA, DC =NA, SDDC = NA, ConvexHull = NA,  Env = NA)
for (id in unique(dat_solo_players$id)) {
  for (round in 1:4) {
    print(paste0("ID ", id, " Round ", round))

    #Get all patch discoveries  
    d_solo$discoveries[d_solo$id == id & d_solo$round == round] <- length(which(dat_solo_patches$id==id & dat_solo_patches$Round==round & dat_solo_patches$PatchEvent=="DISCOVER"))

    
    #Get collected coins
    index <- tail(which(dat_solo_players$id == id & dat_solo_players$Round == round),1)
    

    if (round == 1){
      d_solo$Coins[which(d_solo$id== id & d_solo$round == round)] <- dat_solo_players$CoinAmount[index]
    } else{
      d_solo$Coins[which(d_solo$id== id & d_solo$round == round)] <- dat_solo_players$CoinAmount[index] - dat_solo_players$CoinAmount[tail(which(dat_solo_players$id == id & dat_solo_players$Round == round-1),1)]
    }
    
    
    #Calculate time spent exploiting in given round
    d_solo$Exploit[which(d_solo$id== id & d_solo$round == round)] <- sum(na.omit(dat_solo_players$IsAllowedToMove[which(dat_solo_players$id== id & dat_solo_players$Round == round)]) == "False") / 
      length(na.omit(dat_solo_players$IsAllowedToMove[which(dat_solo_players$id== id & dat_solo_players$Round == round)]))
    
    
    
    
    #Movement metrics
    Trajectory <- data.frame(x = dat_solo_players$PositionX[which(dat_solo_players$id== id & dat_solo_players$Round == round)],
                             y = dat_solo_players$PositionZ[which(dat_solo_players$id== id & dat_solo_players$Round == round)],
                             times = dat_solo_players$t[which(dat_solo_players$id== id & dat_solo_players$Round == round)])
    
    
    d_solo$Sinuosity[d_solo$id == id & d_solo$round == round] <- ifelse(any(is.na(Trajectory$x)), NA, TrajSinuosity2(TrajFromCoords(Trajectory)))
    d_solo$DC[d_solo$id == id & d_solo$round == round] <- ifelse(any(is.na(Trajectory$x)), NA, mean(TrajDirectionalChange(TrajFromCoords(Trajectory))))
    d_solo$SDDC[d_solo$id == id & d_solo$round == round] <- ifelse(any(is.na(Trajectory$x)), NA, sd(TrajDirectionalChange(TrajFromCoords(Trajectory))))
    
    #Area covered (area of convex hull polygon)
    
    if (any(is.na(Trajectory$x))){
      d_solo$ConvexHull[d_solo$id == id & d_solo$round == round] <- NA
    } else {
      hpts <- chull(x = Trajectory$x, y = Trajectory$y)
      hpts <- c(hpts, hpts[1])
      xy.coords <- cbind(Trajectory$x, Trajectory$y)
      chull.coords <- xy.coords[hpts,]
      chull.poly <- Polygon(chull.coords, hole=F)
      
      d_solo$ConvexHull[d_solo$id == id & d_solo$round == round] <- chull.poly@area
    }
    
    
    #Other information
    d_solo$id[d_solo$id == id & d_solo$round == round] <-  id
    d_solo$round[d_solo$id == id & d_solo$round == round] <- round
    d_solo$Env[which(d_solo$id== id & d_solo$round == round)] <- unique(dat_solo_players$Env[dat_solo_players$id == id & dat_solo_players$Round == round] )
    d_solo$track_id[which(d_solo$id== id & d_solo$round == round)] <- unique(dat_solo_players$track_id[which(dat_solo_players$id== id & dat_solo_players$Round == round)]   )
    
  }
}




save(d_solo, file = "d_rounds_solo")









### Plot Joinings/Dicsoveries

plot1 <- ggboxplot(d, x = "Pay", y = "discoveries", color = "Env",
          palette = c("#00AFBB", "#E7B800"), add = "jitter", xlab = "Payoff condition")
plot2 <- ggboxplot(d, x = "Pay", y = "joining", color = "Env",
          palette = c("#00AFBB", "#E7B800"), add = "jitter", xlab = "Payoff condition")
plot3 <- ggboxplot(d, x = "Pay", y = "scrounging", color = "Env",
                   palette = c("#00AFBB", "#E7B800"), add = "jitter", xlab = "Payoff condition")

plot4 <- ggboxplot(d, x = "Pay", y = "joining_real", color = "Env",
                   palette = c("#00AFBB", "#E7B800"), add = "jitter", xlab = "Payoff condition")
plot5 <- ggboxplot(d, x = "Pay", y = "exploit_observed", color = "Env",
                   palette = c("#00AFBB", "#E7B800"), add = "jitter", xlab = "Payoff condition")


plot6 <- ggboxplot(d, x = "Pay", y = "Coins", color = "Env",
          palette = c("#00AFBB", "#E7B800"), add = "jitter", xlab = "Payoff condition")
grid.arrange(plot1, plot2, plot3,plot4,plot5, plot6, ncol=3)



### Plot Movement characteristics
plot1 <- ggboxplot(d, x = "Pay", y = "Sinuosity", color = "Env",
                   palette = c("#00AFBB", "#E7B800"), add = "jitter", xlab = "Payoff condition")
plot2 <- ggboxplot(d, x = "Pay", y = "DC", color = "Env",
                   palette = c("#00AFBB", "#E7B800"), add = "jitter", xlab = "Payoff condition")
plot3 <- ggboxplot(d, x = "Pay", y = "MeanDistance", color = "Env",
                   palette = c("#00AFBB", "#E7B800"), add = "jitter", xlab = "Payoff condition")
plot4 <- ggboxplot(d, x = "Pay", y = "SDDistance", color = "Env",
                   palette = c("#00AFBB", "#E7B800"), add = "jitter", xlab = "Payoff condition")
plot5 <- ggboxplot(d, x = "Pay", y = "ConvexHull", color = "Env",
                   palette = c("#00AFBB", "#E7B800"), add = "jitter", xlab = "Payoff condition")

plot6 <- ggboxplot(d, x = "Pay", y = "Visibility", color = "Env",
                   palette = c("#00AFBB", "#E7B800"), add = "jitter", xlab = "Payoff condition")



grid.arrange(plot1, plot2, plot3, plot4,plot5, plot6, ncol=3)











####
###
##
# Group-level analyses
##
###
####

   
d_group <- data.frame(id = rep(1:40, each = 4), round = rep(1:4,40), Coins = NA, Exploit = NA, DC = NA,  Distance = NA, Visibility = NA, discoveries = NA, joining = NA, joining_real = NA, scrounging = NA, scrounging_real =NA, density = NA, Pay = NA, Env = NA )

for (group in unique(d$Group)) {
  for (round in 1:4) {
    d_group$Coins[d_group$id == group & d_group$round == round] <- mean(d$Coins[d$Group == group & d$round == round])
    d_group$Exploit[d_group$id == group & d_group$round == round] <- mean(d$Exploit[d$Group == group & d$round == round])
  
    d_group$discoveries[d_group$id == group & d_group$round == round] <- mean(d$discoveries[d$Group == group & d$round == round])
    d_group$joining[d_group$id == group & d_group$round == round] <- mean(d$joining[d$Group == group & d$round == round])
    d_group$scrounging[d_group$id == group & d_group$round == round] <- mean(d$scrounging[d$Group == group & d$round == round])
    
    d_group$joining_real[d_group$id == group & d_group$round == round] <- mean(d$joining_real[d$Group == group & d$round == round])
    d_group$scrounging_real[d_group$id == group & d_group$round == round] <- mean(d$scrounging_real[d$Group == group & d$round == round])
    
    
    
    d_group$DC[d_group$id == group & d_group$round == round] <- mean(d$DC[d$Group == group & d$round == round])
    
    d_group$Distance[d_group$id == group & d_group$round == round] <- mean(d$MeanDistance[d$Group == group & d$round == round])
    d_group$Visibility[d_group$id == group & d_group$round == round] <- mean(d$Visibility[d$Group == group & d$round == round])
    
    d_group$Pay[d_group$id == group  & d_group$round == round] <- unique(d$Pay[d$Group == group & d$round == round])
    d_group$Env[d_group$id == group  & d_group$round == round] <- unique(d$Env[d$Group == group & d$round == round])
    
    
    #Calculate how many participants on average foraged at each discovered patch

    #Get all patch discoveries  
    discovered_patches <- unique(dat_patches$unique_patch_id[dat_patches$Group==group & dat_patches$Round==round & dat_patches$PatchEvent=="DISCOVER"])
    d_group$density[d_group$id == group  & d_group$round == round] <- mean(sapply(discovered_patches, function(i) length(unique( dat_patches$id[dat_patches$unique_patch_id == i & dat_patches$PatchEvent=="DISCOVER" ] ))  ))
    
  }
}


save(d_group, file = "d_group")


plot1 <- ggboxplot(d_group, x = "Pay", y = "discoveries", color = "Env",
                   palette = c("#00AFBB", "#E7B800"), add = "jitter", xlab = "Payoff condition")

plot2 <- ggboxplot(d_group, x = "Pay", y = "joining", color = "Env",
                   palette = c("#00AFBB", "#E7B800"), add = "jitter", xlab = "Payoff condition")

plot3 <- ggboxplot(d_group, x = "Pay", y = "scrounging", color = "Env",
                   palette = c("#00AFBB", "#E7B800"), add = "jitter", xlab = "Payoff condition")

plot4 <- ggboxplot(d_group, x = "Pay", y = "joining_real", color = "Env",
                   palette = c("#00AFBB", "#E7B800"), add = "jitter", xlab = "Payoff condition")

plot5 <- ggboxplot(d_group, x = "Pay", y = "scrounging_real", color = "Env",
                   palette = c("#00AFBB", "#E7B800"), add = "jitter", xlab = "Payoff condition")


plot6 <- ggboxplot(d_group, x = "Pay", y = "density", color = "Env",
                   palette = c("#00AFBB", "#E7B800"), add = "jitter", xlab = "Payoff condition")



grid.arrange(plot1, plot2, plot3,plot4,plot5, plot6, ncol=3)



plot1 <- ggboxplot(d_group, x = "Pay", y = "Distance", color = "Env",
                   palette = c("#00AFBB", "#E7B800"), add = "jitter", xlab = "Payoff condition")

plot2 <- ggboxplot(d_group, x = "Pay", y = "Visibility", color = "Env",
                   palette = c("#00AFBB", "#E7B800"), add = "jitter", xlab = "Payoff condition")


grid.arrange(plot1, plot2, ncol=2)







