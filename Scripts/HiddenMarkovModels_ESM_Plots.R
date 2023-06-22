
####
##
# Plotting code for SHMDM supplementary figures
##
###


##
# Plot example HMM trajectory with recovered viterbi states
##

s <- extract.samples(fit)

load("dat_players_extended")
load("viterbi_states")

track_id =1

state_seq_filled <- rep(NA, length(which(dat_players$track_id==track_id)))
for (i in which(dat_players$track_id==track_id)) {
  if (dat_players$IsAllowedToMove[i] == "False"){
    state_seq_filled[which(dat_players$track_id==track_id)==i] <- 1
  }
  else if (dat_players$any_exploit[i]==0){
    state_seq_filled[which(dat_players$track_id==track_id)==i] <- 1
  } else{
    state_seq_filled[which(dat_players$track_id==track_id)==i] <- viterbi_states[which(which(dat_players$track_id==track_id & dat_players$IsAllowedToMove=="True" & dat_players$any_exploit==1)==i)]
  }
}

#graphics.off()
#pdf("Tracks.pdf", height = 9, width = 10)

any <- dat_players$any_exploit[dat_players$track_id == track_id] 
start_noone <-c(1, unlist(sapply(2:length(any), function(x) if(any[x-1] == 1 & any[x]==0){return(x)} )))
end_noone   <- unlist(sapply(2:length(any), function(x) if(any[x-1] == 0 & any[x]==1){return(x)} ))

start_social <-     unlist(sapply(2:length(state_seq_filled), function(x) if(state_seq_filled[x-1] == 2 & state_seq_filled[x]==1){return(x)} ))
end_social   <-     unlist(sapply(2:length(state_seq_filled), function(x) if(state_seq_filled[x-1] == 1 & state_seq_filled[x]==2){return(x)} ))

exploit <-         ifelse(dat_players$IsAllowedToMove[dat_players$track_id == track_id] == "False", 1, 0) 
start_exploit <-     unlist(sapply(2:length(exploit), function(x) if(exploit[x-1] == 0 & exploit[x]==1){return(x)} ))
end_exploit   <-     unlist(sapply(2:length(exploit), function(x) if(exploit[x-1] == 1 & exploit[x]==0){return(x)} ))


par(mfrow = c(7, 1), 
    mar = c(0.75,4,0,0), 
    oma = c(3,3.5,2.25,0))
plot(dat_players$angle[dat_players$track_id == track_id], ylim = c(-2,2), type = "l",yaxt ="n", ylab = "", bty = "n", xaxt = "n" , col = "black", lwd = 1.2) 
mtext(side = 2, "Turning Angles", cex = 0.8, line = 3, col = "black")
axis(side = 2, at = c(-2,0,2), col.axis = "black", col = "black")

for (i in 1:length(start_noone)) {
  rect(start_noone[i],-2,end_noone[i],2,col = alpha("black", alpha = 0.08), bty ="n", lty =0)
}

for (i in 1:length(start_social)) {
  rect(start_social[i],1.6,end_social[i],2,col = alpha(col.pal[5], alpha = 0.9), bty ="n", lty =0)
}

for (i in 1:length(start_social)) {
  rect(start_exploit[i],-2,end_exploit[i],2,col = alpha("black", alpha = 0.5), bty ="n", lty =0)
}

mtext(side = 3, "Inferred social relocation state                                                                                                       ", col = alpha(col.pal[5], alpha = 0.7), cex = 1.2)
mtext(side = 3, "                                   Player exploiting                                               ", col = alpha("black", alpha = 0.9), cex = 1.2)
mtext(side = 3, "                                                                             No group member(s) exploiting", col = alpha("black", alpha = 0.5), cex = 1.2)

dat_players$mindeltadist[dat_players$track_id == track_id] <- ifelse(dat_players$IsAllowedToMove[dat_players$track_id == track_id]  == "False", NA, dat_players$mindeltadist[dat_players$track_id == track_id] )
plot(dat_players$mindeltadist[dat_players$track_id == track_id],type = "l", ylim = c(-3,3), ylab = "" ,yaxt="n", bty = "n", xaxt = "n" , col = "black", lwd = 1.2)   
for (i in 1:length(start_noone)) {
  rect(start_noone[i],-3,end_noone[i],3,col = alpha("black", alpha = 0.08), bty ="n", lty =0)
}

for (i in 1:length(start_social)) {
  rect(start_social[i],2.4,end_social[i],3,col = alpha(col.pal[5], alpha = 0.9), bty ="n", lty =0)
}

for (i in 1:length(start_social)) {
  rect(start_exploit[i],-3,end_exploit[i],3,col = alpha("black", alpha = 0.5), bty ="n", lty =0)
}

mtext(side = 2, "Change in Dist.", cex = 0.8, line = 3, col = "black")
axis(side = 2, at = c(-3,0,3), col.axis = "black", col = "black")

mtext(side = 2, "State-dependent Variables", cex = 1.4, line = 6,font = 3, col = "black")

dat_players$closed[dat_players$track_id == track_id] <- ifelse(dat_players$IsAllowedToMove[dat_players$track_id == track_id]  == "False", NA, dat_players$closed[dat_players$track_id == track_id] )
plot(dat_players$closed[dat_players$track_id == track_id],type = "l", ylim = c(0,pi), yaxt ="n", ylab = "" , bty = "n", xaxt = "n" , col = "black", lwd = 1.2)   
for (i in 1:length(start_noone)) {
  rect(start_noone[i],0,end_noone[i],3,col = alpha("black", alpha = 0.08), bty ="n", lty =0)
}

for (i in 1:length(start_social)) {
  rect(start_social[i],2.7,end_social[i],3,col = alpha(col.pal[5], alpha = 0.9), bty ="n", lty =0)
}

for (i in 1:length(start_social)) {
  rect(start_exploit[i],0,end_exploit[i],3,col = alpha("black", alpha = 0.5), bty ="n", lty =0)
}

mtext(side = 2, "Relative Bearing", cex = 0.8, line = 3, col = "black")
axis(side = 2, at = c(0,1.5,3), col.axis = "black", col = "black")


plot(dat_players$exploit_visible[dat_players$track_id == track_id], type = "l", ylim = c(0,1), ylab = "", , bty = "n", yaxt = "n",xaxt = "n", col = "black" , lwd = 1.2)

for (i in 1:length(start_noone)) {
  rect(start_noone[i],0,end_noone[i],1,col = alpha("black", alpha = 0.08), bty ="n", lty =0)
}
for (i in 1:length(start_social)) {
  rect(start_exploit[i],0,end_exploit[i],1,col = alpha("black", alpha = 0.5), bty ="n", lty =0)
}


axis(side = 2, at = c(0,1),col.axis = "black", col = "black")
mtext(side = 2, "Exploit. Visible", cex = 0.8, line = 3, col = "black")

plot(dat_players$DistPatch[dat_players$track_id == track_id]+1, ylim = c(0,100), type = "l", ylab = "",yaxt="n",  bty = "n", xaxt = "n" , col = "black", lwd = 1.2)  
for (i in 1:length(start_noone)) {
  rect(start_noone[i],0,end_noone[i],100,col = alpha("black", alpha = 0.08), bty ="n", lty =0)
}
for (i in 1:length(start_social)) {
  rect(start_exploit[i],0,end_exploit[i],100,col = alpha("black", alpha = 0.5), bty ="n", lty =0)
}


mtext(side = 2, "Vis. Patch Dist.", cex = 0.8, line = 3, col = "black")
axis(side = 2, at = c(0,50,100),col.axis = "black", col = "black")

plot(dat_players$NumberPatch[dat_players$track_id == track_id]+1, ylim = c(1,3), type = "l", ylab = "",yaxt="n",  bty = "n", xaxt = "n" , col = "black", lwd = 1.2)  
mtext(side = 2, "Vis. Expl. Players", cex = 0.8, line = 3, col = "black")
for (i in 1:length(start_noone)) {
  rect(start_noone[i],1,end_noone[i],3,col = alpha("black", alpha = 0.08), bty ="n", lty =0)
}

for (i in 1:length(start_social)) {
  rect(start_exploit[i],1,end_exploit[i],3,col = alpha("black", alpha = 0.5), bty ="n", lty =0)
}
axis(side = 2, at = c(1,2,3), col.axis = "black", col = "black")



plot(dat_players$TimeSinceExploit[dat_players$track_id == track_id], type = "l",yaxt ="n",ylim = c(0,150), xaxt = "n",ylab = "", bty = "n", col = "black" , lwd = 1.2)  
for (i in 1:length(start_noone)) {
  rect(start_noone[i],0,end_noone[i],150,col = alpha("black", alpha = 0.08), bty ="n", lty =0)
}

for (i in 1:length(start_social)) {
  rect(start_exploit[i],0,end_exploit[i],150,col = alpha("black", alpha = 0.5), bty ="n", lty =0)
}

axis(side = 1, at = seq(0,720,60), cex.axis = 1.2)
mtext(side = 2, "Time since Success", cex = 0.8, line = 3, col = "black")
axis(side = 2, at = c(0,75,150), col.axis = "black", col = "black")
mtext(side = 2, "State Predictors                                                     ", cex = 1.4, line = 2,font = 3, outer = TRUE, col = "black")
mtext("Time [s]", side = 1,line = 2, outer = TRUE, cex = 1.4)

#dev.off()



##
# Plot HMM state-dependent distributions
##


#Set parameter values to posterior means
#Von Mises distribution for turning angles
xangle <- matrix(NA, 2, length(s$lp__))
for (i in 1:2) xangle[i,] <- s$xangle[,i]

yangle <- matrix(NA, 2, length(s$lp__))
for (i in 1:2) yangle[i,] <- s$yangle[,i]


#Lognormal for (smallest) relative bearing
mu_closed <- matrix(NA, 2, length(s$lp__))
for (i in 1:2) mu_closed[i,] <- s$mu_closed[,i]

sigma_closed <- matrix(NA, 2, length(s$lp__))
for (i in 1:2) sigma_closed[i,] <- s$sigma_closed[,i]

#Normal for (smallest) change in distance
mu_distance <- matrix(NA, 2, length(s$lp__))
for (i in 1:2) mu_distance[i,] <- s$mu_distance[,i]

sigma_distance <- matrix(NA, 2, length(s$lp__))
for (i in 1:2) sigma_distance[i,] <- s$sigma_distance[,i]

#graphics.off()
#pdf("HMMStateDepDist.pdf", height = 6, width = 5)

par(mfrow = c(3,2), 
    oma=c(1,2,2.2,0), 
    mar=c(2,3,0,0))

#Von Mises distribution for Turning angles

for (i in 1:2) {
  plot(1:10,type="n", ylim = c(0,3),xlim = c(-pi,pi),  bty = "n", ylab = "")
  x   <- seq(-pi,pi,length.out=1000)
  
  for (j in sample(1:length(s$lp__), 100) ) {
    l <- Conv2(xangle = xangle[i, j], yangle = yangle[i,j])
    lines(x,dvonmises(x,l$mu, l$kappa), col = alpha("black",alpha = 0.01), lwd=0.1)
  }
  if(i==1) mtext("Individual Exploration (State 1)", side = 3, line = 1)
  if(i==1) mtext("Turning Angles", side = 2, line = 3, cex = 1)
  
  if(i==2) mtext("Social Relocation (State 2)", side = 3, line = 1)
  
}

for (i in 1:2) {
  plot(1:10,type="n", xlim = c(-3, 3),ylim = c(0,5), bty = "n", ylab = "")
  x   <- seq(-3,3,length.out=1000)
  
  for (j in sample(1:length(s$lp__), 100) ) {
    
    lines(x,dnorm(x,mu_distance[i,j], sigma_distance[i,j]), col = alpha("black",alpha = 0.01), lwd=1)
  }
  if(i==1) mtext("Change in Distance", side = 2, line = 3, cex = 1)
  
}

for (i in 1:2) {
  plot(1:10,type="n", ylim = c(0,7),xlim = c(0,3), bty = "n", ylab = "")
  x   <- seq(0,3,length.out=1000)
  
  for (j in sample(1:length(s$lp__), 100) ) {
    
    lines(x,dlnorm(x,log(mu_closed[i,j]), sigma_closed[i,j]), col = alpha("black",alpha = 0.01), lwd=1)
  }
  if(i==1) mtext("Relative Bearing", side = 2, line = 3, cex = 1)
  
}

#dev.off()

