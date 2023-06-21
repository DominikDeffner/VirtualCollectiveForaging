
#Fit Social HMMs to agent-based simulation matched to the parameters of the experiment


#Exclude tracks with missingness
dat_players <- dat_players[-which(dat_players$Group %in% c(2,21) & dat_players$Round == 3),]



#For exploration we only use some of the groups
#dat_players <- dat_players[which(dat_players$Group %in% 3:8),]





#Calculate turning angles and step lengths

dat_players$angle <- NA
dat_players$step <- NA

for (id in unique(dat_players$track_id)) {
  d <- momentuHMM::prepData(dat_players[dat_players$track_id == id, c("PositionX","PositionZ")],coordNames=c("PositionX","PositionZ"))
  dat_players$angle[dat_players$track_id == id] <- d$angle
  dat_players$step[dat_players$track_id == id] <- d$step 
  print(id)
}




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



#Calculate which other players were visible based on orientation

dat_players$VisibleOthers <- vector(mode = "list", length = nrow(dat_players))

for (i in which(dat_players$IsAllowedToMove == "True")) {
  print(i)
  
        orient_agent <- dat_players$orient[i]
        

        #Players had a 76? FOV in total
        #FOV_left  <- orient_agent + degree_to_radians(52) 
        #if (FOV_left > 2*pi) FOV_left <- FOV_left - 2*pi
        
        #FOV_right <- orient_agent - degree_to_radians(52) 
        #if (FOV_right < 0) FOV_right <- FOV_right + 2*pi
        
        x_agent <- dat_players$PositionX[i]
        y_agent <- dat_players$PositionZ[i]
        
        group <- dat_players$Group[i]
        
        dat_players$VisibleOthers[[i]] <-  sapply(c(1:4)[-dat_players$PlayerID[i]], function(target_ID){
          
          x_dist <-  dat_players$PositionX[which(dat_players$Group == group & dat_players$PlayerID==target_ID & dat_players$Round == dat_players$Round[i]   & dat_players$t == dat_players$t[i])] - x_agent
          y_dist <-  dat_players$PositionZ[which(dat_players$Group == group & dat_players$PlayerID==target_ID & dat_players$Round == dat_players$Round[i]   & dat_players$t == dat_players$t[i])] - y_agent
          
          angle <- heading_to_radians(x_dist, y_dist)

          ifelse(pi - abs(abs(angle - orient_agent) - pi) <= degree_to_radians(54), return(target_ID), return(0))
          
        }     ) 
        
        


}#i


#Closed angle with agent with smallest  absolute closed angle.
#The closed angle is defined by the absolute closed angle between the agent's orientation vector 
#(unit vector from center of agent towards the orientation of the agent) and the vector between the 2 agents center. 
#If a focal agents faces exactly to another the closed angle between them will be zero. This metric is asymmetric. 
#So the idea here is that we calculate this for each pair of agent and for each focal agent you choose the target agent as the one with 
#smallest closed angle with the focal one and record this closed angle.


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






#Compute whether they are currently seeing other exploiting agent


dat_players$exploit_visible <- NA

for (ID in 1:max(dat_players$id)) {
  print(ID)
  for (round in 1:4){
    if(length(dat_players$PositionX[which(dat_players$id == ID & dat_players$Round == round)]) > 0){
      
      for (t in 1:max(dat_players$t)) {
        idx_agent <- which(dat_players$id==ID & dat_players$Round == round & dat_players$t == t)
        visible <- dat_players$VisibleOthers[idx_agent][[1]]
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







#Compute smallest change in distance to all (exploiting) players

dist <- matrix(0, nrow = nrow(dat_players), ncol = 3)
deltadist <-  matrix(0, nrow = nrow(dat_players), ncol = 3)

deltadistEXPLOIT <-  matrix(0, nrow = nrow(dat_players), ncol = 3)



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
    
  
#Now we have distance to all other group members for each point in time; we want for each time, most negative change in distance, 
#for a) any player and b) any exploiting player

    if(t > 1){
      if(dat_players$id[index] == dat_players$id[which(dat_players$id==ID  & dat_players$Round == round   & dat_players$t == t-1)]){
        
        deltadist[index,]     <- sapply(1:3, function(x) dist[index,x] - dist[which(dat_players$id==ID  & dat_players$Round == round   & dat_players$t == t-1), x]  )
        
        
        deltadistEXPLOIT[index,]     <- sapply(1:3, function(x){
          
          if (dat_players$IsExtracting[which(dat_players$id== ids[-which(ids == ID)][x]  & dat_players$Round == round   & dat_players$t == t-1)] == "True"){
            dist[index,x] - dist[which(dat_players$id==ID  & dat_players$Round == round   & dat_players$t == t-1), x] 
          }else{
            0
          }
          
        } )
        
        } else {
        deltadist[index,] <- NA
        deltadistEXPLOIT[index,] <- NA
      }
    } else {
      deltadist[index,] <- NA
      deltadistEXPLOIT[index,] <- NA
    }
     }
   }#round
    print(t)
 }#t
}#ID



dat_players$mindeltadistEXPLOIT <- sapply(1: nrow(dat_players), function(i){
  if (dat_players$t[i] >1){
  if( dat_players$IsAllowedToMove[i] == "True" & dat_players$IsAllowedToMove[i-1] != "True"){
    NA
  } else{
    dat_players$mindeltadistEXPLOIT[i]  }
} else{
  NA
}
}
)

dat_players$mindeltadist <- sapply(1: nrow(dat_players), function(i){
  if (dat_players$t[i] >1){
    if( dat_players$IsAllowedToMove[i] == "True" & dat_players$IsAllowedToMove[i-1] != "True"){
      NA
    } else{
      dat_players$mindeltadist[i]    }
  } else{
    NA
  }
}
)













dat_players$Exploit <- rep(0, nrow(dat_players))

for (t in 2:nrow(dat_players)) {
  print(t)
  if(dat_players$track_id[t] == dat_players$track_id[t-1] && dat_players$IsAllowedToMove[t-1] == "False"){
    dat_players$Exploit[t] = 1
  }
}







#Prepare data for stan model
dat_players$step[dat_players$step>2] <- 2



# set NAs to out-of-range values
dat_players$step[is.na(dat_players$step)] <- -10
dat_players$angle[is.na(dat_players$angle)] <- -10
dat_players$closed[is.na(dat_players$closed)] <- -10
dat_players$mindeltadistEXPLOIT[is.na(dat_players$mindeltadistEXPLOIT)] <- -10


dat_players$step <- dat_players$step/(max(dat_players$step)+0.001)+0.00001




#Remove exploit
dat_players <- dat_players[-which(dat_players$IsAllowedToMove == "False"),]



#Keep only subset
dat_players_backup <- dat_players
dat_players <- dat_players[which(dat_players$Group %in% 1:5),]

stan.data <- list(T=nrow(dat_players),track_id = as.numeric(dat_players$track_id), steps=dat_players$step, angles=dat_players$angle, deltadist = dat_players$mindeltadistEXPLOIT, closed=dat_players$closed, Exploit = dat_players$Exploit ,exploit_visible =  dat_players$exploit_visible, N=2)

stan.data$track_id <- sapply(1: stan.data$T, function(x) which(unique(stan.data$track_id) == stan.data$track_id[x]) )

#Create indices for environment and incentive structure
stan.data$Environment <- as.integer(ifelse(dat_players$Env == "C", 1, 2))
stan.data$Incentives <- as.integer(ifelse(dat_players$Pay == "Coop", 1, 2))

#Create variables needed for parallelized computation of likelihood
stan.data$tracks <- unique(stan.data$track_id)
stan.data$N_tracks <- length(stan.data$tracks)
stan.data$index  <- sapply(stan.data$tracks, function(id) which(stan.data$track_id == id)[1]-1)
stan.data$T_track <- sapply(stan.data$tracks, function(id) length(which(stan.data$track_id == id)))





save(stan.data, file = "stan.data190123")








#rm(list=setdiff(ls(), "stan.data"))
iter = 2000
inits <- list(list(mu=c(1,3), mu_step = c(1,3), mu_closed = c(2, 0.2), xangle=c(10,40), yangle=c(0,0)))


library(rstan)
fit <- stan(file = "HMM_socialInfo.stan", data=stan.data,
            control=list(adapt_delta=0.8), refresh=1, chains=1, cores = 1)



s <- extract.samples(fit)
precis(fit, 3)


save(fit, file = "fittotal_sample")








































graphics.off()
png("StateDepDistrlow.png", res = 900, height = 25, width = 15, units = "cm")




#Set parameter values to posterior means

#Beta distribution for step length
alpha    <- c( mean( inv_logit(s$mu_step[,1]) * s$phi_step[,1] )    ,mean( inv_logit(s$mu_step[,2]) * s$phi_step[,2] )  )
beta     <- c( mean( (1-inv_logit(s$mu_step[,1])) * s$phi_step[,1] )    ,mean( (1-inv_logit(s$mu_step[,2])) * s$phi_step[,2] )  )

#Von Mises distribution for turning angles
xangle <- c(mean(s$xangle[,1]),mean(s$xangle[,2]))
yangle <- c(mean(s$yangle[,1]),mean(s$yangle[,2]))

#Gamma for smallest closed angle
mu_angle <- c(mean(s$mu_closed[,1]),mean(s$mu_closed[,2]))
sigma_angle <- c(mean(s$sigma_closed[,1]),mean(s$sigma_closed[,2]))


par(mfrow = c(3,2), 
    oma=c(0,0,0,0), 
    mar=c(2,4,2,0))


library(ConnMatTools)

for (i in 1:2) {
  
  x <- seq(0,1,length.out=50)
  
  # Plot gamma and normal distributions - for sd << mean, the two should be very close
  plot(x,dbeta(x, alpha[i],beta[i]),
       main= paste("(Beta) step length State",i),type="l", ylab = "Density", xlab = "Step length", col = "indianred", lwd=2)
  
  #dens(dat_players$step[dat_players$State==ifelse(i==1,1,3)] ,add = TRUE)
  
  
}

#Von Mises distribution for Turning angles

for (i in 1:2) {
  
  l <- Conv2(xangle = xangle[i], yangle = yangle[i])
  x <- seq(-pi,pi,length.out=50)
  
  # Plot gamma and normal distributions - for sd << mean, the two should be very close
  plot(x,dvonmises(x,l$mu, l$kappa), main=paste("(Von Mises) turning angle State",i),type="l", 
       ylim = c(0,3), ylab = "Density", xlab = "Turning angles", col = "indianred", lwd=2)
  
  
  dens(dat_players$angle[dat_players$State==ifelse(i==1,1,3)] ,add = TRUE)
  
}




for (i in 1:2) {
  
  l <- gammaParamsConvert(mean=mu_angle[i],sd=sigma_angle[i])
  x <- seq(0,5,length.out=50)
  
  # Plot gamma and normal distributions - for sd << mean, the two should be very close
  plot(x,dgamma(x,l$shape,rate=1/l$scale),
       main= paste("(Gamma) delta angle State",i),type="l", ylab = "Density", xlab = "Step length", col = "indianred", lwd=2)
  
  dens(dat_players$closed[dat_players$State==ifelse(i==1,1,3)] ,add = TRUE)
  
  
}



dev.off()






States <- matrix(0, nrow = stan.data$T, ncol = iter )

for (t in 1:stan.data$T) {
  States[t,] <- s$viterbi[,t]
  
}

Mode_State = sapply(1:stan.data$T, function(x) as.integer(names(sort(table(States[x,]), decreasing = TRUE)[1]))     )





graphics.off()
png("StateRecMatched.png", res = 300, height = 24, width = 30, units = "cm")


par(mfrow = c(4,4), 
    oma=c(0,4.5,1,0.1), 
    mar=c(2,2,0,0))

library(scales)

for (id in unique(dat_players$id)){
  plot(dat_players$x[which(dat_players$id==id)], dat_players$y[which(dat_players$id==id)],  pch= ifelse(dat_players$State[which(dat_players$id==id)]==1,16,17), type= "b", ylab="", xlab= "")
  points(dat_players$x[which(dat_players$id==id)], dat_players$y[which(dat_players$id==id)], pch = 21, cex =2.5,
         bg = alpha("darkgreen", alpha = ifelse(Mode_State[which(dat_players$id==id)]==2, 0.2,0 )), col=ifelse(Mode_State[which(dat_players$id==id)]==2,alpha("darkgreen", alpha = 0.2) ,alpha("darkgreen", alpha = 0)  ))            
  
  if (id %in% c(1,9)) mtext(expression(paste(epsilon," = 3")), side = 2, line = 3, outer = FALSE)
  if (id %in% c(5,13)) mtext(expression(paste(epsilon," = 6")), side = 2, line = 3, outer = FALSE)
  
  
  }

mtext("       Distributed                                                      Concentrated", side = 2, line = 3, outer = TRUE, cex = 1.2)

dev.off()


table(dat_players$State, Mode_State)[1,]/sum(table(dat_players$State, Mode_State)[1,])
table(dat_players$State, Mode_State)[2,]/sum(table(dat_players$State, Mode_State)[2,])
