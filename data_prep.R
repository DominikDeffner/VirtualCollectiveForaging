
#Data preparation script to get raw data into analyzable format for later processing and analysis

#Get all file names
Files_group <- list.files("~/CoinScrounge/data/Group")
Files_solo <- list.files("~/CoinScrounge/data/Solo")


#First we construct some useful variables for later use

#Determine session (0-11) from file names
Sessions_group <- as.numeric(sapply(Files_group, function(x){ 
  string <- list.files(paste("~/CoinScrounge/data/Group/",x, sep = ""))[1]
  if (nchar(string) == 18){
    as.numeric(substr(string,9,9)) 
  } else {
    as.numeric(substr(string,9,10)) 
  }
}
))

Sessions_solo <- as.numeric(sapply(Files_solo, function(x){ 
  string <- list.files(paste("~/CoinScrounge/data/Solo/",x, sep = ""))[1]
  if (nchar(string) == 18){
    as.numeric(substr(string,9,9)) 
  } else {
    as.numeric(substr(string,9,10)) 
  }
}
))


# Session 0-5 Competitive
# Session 6-11 Cooperative
Condition_group <- ifelse(Sessions_group<6, "Comp", "Coop")


#Which rounds are concentrated in each session (indices + 1)?
Concentrated <- list()

Concentrated[[1]]  <- c(1,2)
Concentrated[[2]]  <- c(1,3)
Concentrated[[3]]  <- c(1,4)
Concentrated[[4]]  <- c(2,3)
Concentrated[[5]]  <- c(2,4)
Concentrated[[6]]  <- c(3,4)
Concentrated[[7]]  <- c(1,2)
Concentrated[[8]]  <- c(1,3)
Concentrated[[9]]  <- c(1,4)
Concentrated[[10]] <- c(2,3)
Concentrated[[11]] <- c(2,4)
Concentrated[[12]] <- c(3,4)



#Check which files have higher sampling rate (we reduced the sampling rate from 50Hz to 25Hz after a couple of sessions)

Higher_sampling_group <- sapply(1:length(Files_group), function(i) {
  file_name <- list.files(paste0("~/CoinScrounge/data/Group/",Files_group[i] ) )[3]
  ifelse(length(count.fields(paste0("~/CoinScrounge/data/Group/",Files_group[i],"/", file_name ))) > 80000, TRUE, FALSE)
}   
)


Higher_sampling_solo <- sapply(1:length(Files_solo), function(i) {
  file_name <- list.files(paste0("~/CoinScrounge/data/Solo/",Files_solo[i] ) )[3]
  ifelse(length(count.fields(paste0("~/CoinScrounge/data/Solo/",Files_solo[i],"/", file_name ))) > 20000, TRUE, FALSE)
}   
)



#From raw data files, construct single large data sets

######
####
###
##
# Group rounds
##
###
####
#####

###
##
# Player data files
##
###

for (g in 1:40) {
  print(g)
  
  Group <- Files_group[g]
  Session <- Sessions_group[g]
  
  for (Round in 0:3) {
    print(Round)
    
    #Player data
    d <- read.csv2(paste("~/CoinScrounge/Data/Group/", Group,"/",get_session(session = Session, round = Round, data = "players"), ".log", sep = ""))
    
    char_columns <- c(1,3,4,5,6,7)             
    d[ , char_columns] <- apply(d[ , char_columns], 2, function(x) as.numeric(sub(",", ".", x, fixed = TRUE)))
    
    #Sort according to player ID
    d <- d[order(d$PlayerID),] 
    
    #Standardize Player IDS
    d$PlayerID <-d$PlayerID-min(d$PlayerID, na.rm = TRUE)+1
    
    #Downsampling (we sample at either 25 or 50 Hz, so interval = 25/50 corresponds to 1 datapoint per second)
    
    time_seq <- seq(0.5,719.5, 1)
    times <- sapply(time_seq, function(x) d$Time[which.min(abs(d$Time-x))] ) 
    
    d <- as.data.frame(d[d$Time %in% times,])
    
    duplicates <- which(sapply(times, function(x) length(which(d$Time == x)) > 4)) 
    for (x in duplicates){
      for (id in unique(d$PlayerID)) {
        No_dupl <- length(which(d$Time == times[x] & d$PlayerID ==id))
        d <- d[-which(d$Time == times[x] & d$PlayerID ==id)[2:No_dupl],]
      }
    }
    
    #Check if seconds are missing
    #We insert NAs to make sure all files have the same format
    if (nrow(d)<2880){
      misses_id <- which(sapply(1:4, function(i) length(d$Time[d$PlayerID==i])) < 720)
      missing_times <- setdiff(d$Time[d$PlayerID==sample(c(1:4)[-misses_id],1)]   ,d$Time[d$PlayerID==misses_id])
      index <- which((missing_times[1] - d$Time[d$PlayerID==misses_id] ) == minpositive(missing_times[1] - d$Time[d$PlayerID==misses_id] ))
      d <- insertRows(d, (index+1) : (index+length(missing_times)) , new = NA)
      d$Time[(index+1) : (index+length(missing_times))] <- missing_times  
      d$PlayerID[(index+1) : (index+length(missing_times))] <- misses_id  
      
      }
      
    
    #Construct simpler time count variable
    d$t <- sapply(1:nrow(d), function(i) which(unique(d$Time)==d$Time[i]))
    
    
    #Overall player ID
    d$id <- d$PlayerID + (g-1)*4
    
    #Track ID
    d$track_id <- (d$id-1)*4 + Round  + 1
    
    #Add other information
    d$Group <- g
    d$Round <- Round + 1
    d$Env   <- ifelse((Round + 1) %in% Concentrated[[Session+1]], "C", "D")
    d$Pay   <- Condition_group[g]
    
    if(Round == 0 & g == 1){
      dat_players <- d
    }else{
      dat_players <- rbind(dat_players, d)
    }
    
  }#Round
  
}#g

#Calculate players orientation in radians from orientation vector
# orient = 0/2pi means player is looking along the x-axis

dat_players$orient <- NA
dat_players$orient <- sapply( 1:nrow(dat_players), function(i) {
  print(i)
  if (is.na(dat_players$ForwardX[i])){
    return(NA)
  } else {
    return(heading_to_radians(dat_players$ForwardX[i],dat_players$ForwardZ[i]))
  }
} 
)

#Calculate which other players were visible at each point in time based on player orientation and horizontal field of view (108°)

dat_players$VisibleOthers <- vector(mode = "list", length = nrow(dat_players))

for (i in which(dat_players$IsAllowedToMove == "True")) {
  print(i)
  
  if ((dat_players$Group[i] %in% c(2,21) & dat_players$Round[i] == 3) == FALSE){
  
  orient_agent <- dat_players$orient[i]
  
  x_agent <- dat_players$PositionX[i]
  y_agent <- dat_players$PositionZ[i]
  
  group <- dat_players$Group[i]
  
  dat_players$VisibleOthers[[i]] <-  sapply(c(1:4)[-dat_players$PlayerID[i]], function(target_ID){
    
    x_dist <-  dat_players$PositionX[which(dat_players$Group == group & dat_players$PlayerID==target_ID & dat_players$Round == dat_players$Round[i]   & dat_players$t == dat_players$t[i])] - x_agent
    y_dist <-  dat_players$PositionZ[which(dat_players$Group == group & dat_players$PlayerID==target_ID & dat_players$Round == dat_players$Round[i]   & dat_players$t == dat_players$t[i])] - y_agent
    
    angle <- heading_to_radians(x_dist, y_dist)
    
    ifelse(pi - abs(abs(angle - orient_agent) - pi) <= degree_to_radians(54), return(target_ID), return(0))
    
  }     ) 
  
  }
  
}#i

#Remove wrong visibility data from Unity
dat_players$VisiblePlayers <- NULL


###
##
# Patch data files
##
###

for (g in 1:40) {
  print(g)
  
  Group <- Files_group[g]
  Session <- Sessions_group[g]
  
  for (Round in 0:3) {
    print(Round)
    
    #Patch data
    
    d <- read.csv2(paste("~/CoinScrounge/Data/Group/", Group,"/",get_session(session = Session, round = Round, data = "patches"),".log", sep = ""))
    char_columns <- c(1,3,4)             
    d[ , char_columns] <- apply(d[ , char_columns], 2, function(x) as.numeric(sub(",", ".", x, fixed = TRUE)))
    d$TriggerPlayer <-d$TriggerPlayer-min(d$TriggerPlayer, na.rm = TRUE)+1
    
    #Overall player ID
    d$id <- d$TriggerPlayer + (g-1)*4
    
    #Add other information
    d$Group <- g
    d$Round <- Round + 1
    d$Env   <- ifelse((Round + 1) %in% Concentrated[[Session+1]], "C", "D")
    d$Pay   <- Condition_group[g]
    
    if(Round == 0 & g == 1){
      dat_patches <- d
    }else{
      dat_patches <- rbind(dat_patches, d)
    }
    
  }#Round
  
}#g


#CoinAmount is counted for all players together in first two coop sessions, so we need to reconstruct those values from dat_patches

  for (i in which(dat_players$Group %in% c(7,9) )  ) {
           dat_players$CoinAmount[i]  <- length( which(dat_patches$PatchEvent=="EXTRACT"
                                                     & dat_patches$Group == dat_players$Group[i]
                                                     & dat_patches$id==dat_players$id[i] 
          & ((dat_patches$Round < dat_players$Round[i]) | (dat_patches$Round == dat_players$Round[i] & dat_patches$Time <= dat_players$Time[i] )   )
                                                     ) ) 
  }
  
#Compute overall patch ID

total_number_patches <- sapply(1:40, function(i) length(unique(dat_patches$PatchID[dat_patches$Group==i])) )
dat_patches$unique_patch_id <- NA
for (i in 1:40) {
  print(i)
  dat_patches$unique_patch_id[dat_patches$Group == i] <- sapply(which(dat_patches$Group == i) , function(x){
                                                   which(unique(dat_patches$PatchID[dat_patches$Group==i]) == dat_patches$PatchID[x]) + 
                                                  ifelse(i == 1, 0, sum(total_number_patches[1:(i-1)]) )
                                                     })
}  






###
##
# Demographics
##
###

dat_demographics <- c()
for (g in 1:40) {

  Group <- Files_group[g]
  Session <- Sessions_group[g]
    
    #Player data
    d <- read.csv2(paste("~/CoinScrounge/Data/Group/", Group,"/",paste("session_",Session,"_dems", sep = ""), ".log", sep = ""),header=FALSE)   
    d <- d[-1,2:5]
    
    if(nrow(d>4)) d <- d[1:4,]
    
    d[,1] <- as.numeric(d[,1])
    print(nrow(d))
    
    
    if(g == 1){
      dat_demographics <- d
    }else{
      dat_demographics <- rbind(dat_demographics, d)
    }


}


colnames(dat_demographics) <- c("Age", "Gender", "Experience", "Minecraft")
dat_demographics$Gender <- as.numeric( ifelse(dat_demographics$Gender == "MÃ¤nnlich", 1, 2) )
dat_demographics$Minecraft <- as.numeric( unlist( sapply(1:160, function(i){
  if (dat_demographics$Minecraft[i] == "Ja")          return(1)
  if (dat_demographics$Minecraft[i] == "WeiÃY Nicht") return(2)
  if (dat_demographics$Minecraft[i] == "Nein")        return(0)
  
})) )

dat_demographics$Experience[dat_demographics$Experience == "keine"]            <- 1
dat_demographics$Experience[dat_demographics$Experience == "wenig"]            <- 2
dat_demographics$Experience[dat_demographics$Experience == "durchschnittlich"] <- 3
dat_demographics$Experience[dat_demographics$Experience == "viel"]             <- 4
dat_demographics$Experience[dat_demographics$Experience == "sehr viel"]        <- 5

dat_demographics$Experience <- as.numeric(dat_demographics$Experience)




######
####
###
##
# Solo rounds
##
###
####
#####

###
##
# Player data files
##
###

for (g in 1:40) {
  print(g)
  
  Participant <- Files_solo[g]
  Session <- Sessions_solo[g]
  
  for (Round in 0:3) {
    print(Round)
    
    #Player data
    d <- read.csv2(paste("~/CoinScrounge/Data/Solo/", Participant,"/",get_session(session = Session, round = Round, data = "players"), ".log", sep = ""))
    
    char_columns <- c(1,3,4,5,6,7)             
    d[ , char_columns] <- apply(d[ , char_columns], 2, function(x) as.numeric(sub(",", ".", x, fixed = TRUE)))
    

    #Downsampling (we sample at either 25 or 50 Hz, so interval = 25/50 corresponds to 1 datapoint per second)
    
    time_seq <- seq(0.5,719.5, 1)
    times <- sapply(time_seq, function(x) d$Time[which.min(abs(d$Time-x))] ) 
    
    d <- as.data.frame(d[d$Time %in% times,])
    
    duplicates <- which(sapply(times, function(x) length(which(d$Time == x)) > 4)) 
    for (x in duplicates){
        No_dupl <- length(which(d$Time == times[x]))
        d <- d[-which(d$Time == times[x])[2:No_dupl],]
    }
    
    d$t <- sapply(1:nrow(d), function(i) which(unique(d$Time)==d$Time[i])) #Problem with missing seconds
    
    d$PlayerID <-d$PlayerID-min(d$PlayerID, na.rm = TRUE)+1
    
    #Overall player ID
    d$id <- g
    
    #Track ID
    d$track_id <- (d$id-1)*4 + Round  + 1

    #Add other information
    d$Round <- Round + 1
    d$Env   <- ifelse((Round + 1) %in% Concentrated[[Session+1]], "C", "D")

    
    #Remove unnecessary variables
    d[c("PlayerID", "VisiblePlayers","X")] <- NULL
    
    if(Round == 0 & g == 1){
      dat_solo_players <- d
    }else{
      dat_solo_players <- rbind(dat_solo_players, d)
    }
    
  }#Round
  
}#g




###
##
# Patch data files
##
###

for (g in 1:40) {
  print(g)
  
  Participant <- Files_solo[g]
  Session <- Sessions_solo[g]
  
  
  for (Round in 0:3) {
    print(Round)
    
    #Patch data
    
    d <- read.csv2(paste("~/CoinScrounge/Data/Solo/", Participant,"/",get_session(session = Session, round = Round, data = "patches"),".log", sep = ""))
    char_columns <- c(1,3,4)             
    d[ , char_columns] <- apply(d[ , char_columns], 2, function(x) as.numeric(sub(",", ".", x, fixed = TRUE)))
    d$TriggerPlayer <-d$TriggerPlayer-min(d$TriggerPlayer, na.rm = TRUE)+1
    
    #Overall player ID
    d$id <- g
    
    #Add other information
    d$Round <- Round + 1
    d$Env   <- ifelse((Round + 1) %in% Concentrated[[Session+1]], "C", "D")
    
    
    if(Round == 0 & g == 1){
      dat_solo_patches <- d
    }else{
      dat_solo_patches <- rbind(dat_solo_patches, d)
    }
    
  }#Round
  
}#g

#Compute overall patch ID

total_number_patches <- sapply(1:40, function(i) length(unique(dat_solo_patches$PatchID[dat_solo_patches$id==i])) )
dat_solo_patches$unique_patch_id <- NA
for (i in 1:40) {
  print(i)
  dat_solo_patches$unique_patch_id[dat_solo_patches$id == i] <- sapply(which(dat_solo_patches$id == i) , function(x){
    which(unique(dat_solo_patches$PatchID[dat_solo_patches$id==i]) == dat_solo_patches$PatchID[x]) + 
      ifelse(i == 1, 0, sum(total_number_patches[1:(i-1)]) )
  })
}  






###
##
# Demographics
##
###

dat_demographics_solo <- c()
for (g in 1:40) {
  
  Participant <- Files_solo[g]
  Session <- Sessions_solo[g]
  
  
  #Player data
  d <- read.csv2(paste("~/CoinScrounge/Data/Solo/", Participant,"/",paste("session_",Session,"_dems", sep = ""), ".log", sep = ""),header=FALSE)   
  d <- d[-1,2:5]
  
  d[,1] <- as.numeric(d[,1])
  print(nrow(d))
  
  
  if(g == 1){
    dat_demographics_solo <- d
  }else{
    dat_demographics_solo <- rbind(dat_demographics_solo, d)
  }
  
  
}


colnames(dat_demographics_solo) <- c("Age", "Gender", "Experience", "Minecraft")
dat_demographics_solo$Gender <- as.numeric( ifelse(dat_demographics_solo$Gender == "MÃ¤nnlich", 1, 2) )


dat_demographics_solo$Minecraft[dat_demographics_solo$Minecraft == "Ja"]            <- 1
dat_demographics_solo$Minecraft[dat_demographics_solo$Minecraft == "WeiÃY Nicht"]   <- 2
dat_demographics_solo$Minecraft[dat_demographics_solo$Minecraft == "Nein"] <- 0
dat_demographics_solo$Minecraft <- as.numeric(dat_demographics_solo$Minecraft)

dat_demographics_solo$Experience[dat_demographics_solo$Experience == "keine"]            <- 1
dat_demographics_solo$Experience[dat_demographics_solo$Experience == "wenig"]            <- 2
dat_demographics_solo$Experience[dat_demographics_solo$Experience == "durchschnittlich"] <- 3
dat_demographics_solo$Experience[dat_demographics_solo$Experience == "viel"]             <- 4
dat_demographics_solo$Experience[dat_demographics_solo$Experience == "sehr viel"]        <- 5
dat_demographics_solo$Experience <- as.numeric(dat_demographics_solo$Experience)



#Save dataframes
save(dat_players, file = "data/dat_players")
save(dat_patches, file = "data/dat_patches")
save(dat_demographics, file = "data/dat_demographics")

save(dat_solo_players, file = "data/dat_solo_players")
save(dat_solo_patches, file = "data/dat_solo_patches")
save(dat_demographics_solo, file = "data/dat_demographics_solo")

