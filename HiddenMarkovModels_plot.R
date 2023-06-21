

#Script to process and plot HMM output

library(scales)
library(RColorBrewer)


#Create (colorblind friendly) color palette
x <- 1:8 # fake data
col.pal <- brewer.pal(length(x), "Set1") #create a palette which you loop over for corresponding values

# Compute stationary distribution
# We want posterior distributions of expected proportion in social state

Exp_social <- array(NA, dim = c(length(s$lp__),2,2))

for (i in 1:2){
  for (j in 1:2) {
    
    Exp_social[,i,j] <- sapply(1: length(s$lp__), function(x){
      
      P_II <- 1-s$p_I_S[x, i, j]
      P_IS <- s$p_I_S[x, i, j]
      P_SI <- 1-s$p_S_S[x, i, j]
      P_SS <- s$p_S_S[x, i, j]
      
      Gamma <- matrix(c(P_II,P_SI,P_IS,P_SS),2,2)
      return(solve(t(diag(2)-Gamma +1), rep(1, 2))[2])
    } 
    )
    
  }
}


graphics.off()
pdf("HMMExpSocial.pdf", height = 5, width = 8)

par(mfrow = c(2,3), mar = c(2,2,1,1), oma = c(3,5,2,0))

for (i in 1:2){
  for (j in 1:2) {
    
    dens <- density(Exp_social[,i,j])
    x1 <- min(which(dens$x >= quantile(Exp_social[,i,j], 0.025)))  
    x2 <- max(which(dens$x <  quantile(Exp_social[,i,j], 0.9725)))
    plot(dens, xlim = c(0.20,0.35), ylim = c(0,65), type="l", ann = FALSE, bty = "n", col=alpha("black",alpha = 0.9))
    with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha("black",alpha = 0.6), border = NA))
    
    if (i == 1 & j == 1)  mtext("Concentrated Environments",  cex = 1, side = 3, line = 1.5)
    if (i == 1 & j == 2)  mtext("Distributed Environments",  cex = 1, side = 3, line = 1.5)
    
    
    if (i == 1 & j == 1)  mtext("Group incentives",  cex = 1.2, side = 2, line = 3)
    if (i == 2 & j == 1)  mtext("Individual incentives",  cex = 1.2, side = 2, line = 3)
    
  }
  
  contr <- Exp_social[,i,1] - Exp_social[,i,2]
  dens <- density(contr)
  x1 <- min(which(dens$x >= quantile(contr, 0.025)))  
  x2 <- max(which(dens$x <  quantile(contr, 0.9725)))
  plot(dens, xlim = c(-0.10,0.10), ylim = c(0,65), type="l", ann = FALSE, bty = "n", col=alpha("black",alpha = 0.9))
  with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha("black",alpha = 0.6), border = NA))
  if (i == 1)  mtext("Contrast (Concentrated-Distributed)",  cex = 0.8, side = 3, line = 1.5)
  abline(v = 0, lty = 2, col = "grey")
  
}

mtext("Density",  cex = 1.2, side = 2, line = 3.5, outer = TRUE)
mtext("Expected proportion in social state",  cex = 1.2, side = 1, line = 1.5, outer = TRUE)

dev.off()















#Set parameter values to posterior means


#Von Mises distribution for turning angles

xangle <- matrix(NA, 2, length(s$lp__))
for (i in 1:2) xangle[i,] <- s$xangle[,i]

yangle <- matrix(NA, 2, length(s$lp__))
for (i in 1:2) yangle[i,] <- s$yangle[,i]




#Lognormal for smallest closed angle
mu_closed <- matrix(NA, 2, length(s$lp__))
for (i in 1:2) mu_closed[i,] <- s$mu_closed[,i]

sigma_closed <- matrix(NA, 2, length(s$lp__))
for (i in 1:2) sigma_closed[i,] <- s$sigma_closed[,i]


#Lognormal for smallest closed angle
mu_distance <- matrix(NA, 2, length(s$lp__))
for (i in 1:2) mu_distance[i,] <- s$mu_distance[,i]

sigma_distance <- matrix(NA, 2, length(s$lp__))
for (i in 1:2) sigma_distance[i,] <- s$sigma_distance[,i]

# Bernoulli for exploiting player in field of view
p_exploit_visible <- matrix(NA, 2, length(s$lp__))
for (i in 1:2) p_exploit_visible[i,] <- s$p_exploit_visible[,i]



graphics.off()
pdf("HMMStateDepDist.pdf", height = 6, width = 5)

par(mfrow = c(4,2), 
    oma=c(1,2,2.2,0), 
    mar=c(2,3,0,0))

#Von Mises distribution for Turning angles

for (i in 1:2) {
  plot(1:10,type="n", ylim = c(0,2),xlim = c(-pi,pi),  bty = "n", ylab = "")
  x   <- seq(-pi,pi,length.out=1000)
  
  for (j in sample(1:length(s$lp__), 100) ) {
    l <- Conv2(xangle = xangle[i, j], yangle = yangle[i,j])
    lines(x,dvonmises(x,l$mu, l$kappa), col = alpha("black",alpha = 0.3), lwd=0.1)
  }
  if(i==1) mtext("Individual Exploration", side = 3, line = 1)
  if(i==1) mtext("Turning Angles", side = 2, line = 4, cex = 0.8)
  
  if(i==2) mtext("Social Relocation", side = 3, line = 1)
}



for (i in 1:2) {
  plot(1:10,type="n", ylim = c(0,6),xlim = c(0,3), bty = "n", ylab = "")
  x   <- seq(0,3,length.out=1000)
  
  for (j in sample(1:length(s$lp__), 100) ) {
    
    lines(x,dlnorm(x,log(mu_closed[i,j]), sigma_closed[i,j]), col = alpha("black",alpha = 0.3), lwd=1)
  }
  if(i==1) mtext("Closed Angles", side = 2, line = 4, cex = 0.8)
  
}


for (i in 1:2) {
  plot(1:10,type="n", xlim = c(-3, 3),ylim = c(0,3), bty = "n", ylab = "")
  x   <- seq(-3,3,length.out=1000)
  
  for (j in sample(1:length(s$lp__), 100) ) {
    
    lines(x,dnorm(x,mu_distance[i,j], sigma_distance[i,j]), col = alpha("black",alpha = 0.3), lwd=1)
  }
  if(i==1) mtext("Change in Distance", side = 2, line = 4, cex = 0.8)
  
}


for (i in 1:2) {
  plot(1:2,type="n", xlim = c(-0.25, 1.25),ylim = c(-0.08,1.05), bty = "n", ylab = "", xaxt = "n",yaxt = "n", xlab = "")
  axis(1, c(0,1), c(0,1))
  axis(2, seq(0,1, by = 0.25), seq(0,1, by = 0.25))
  abline(h=0, lty = 2, col = "lightgrey")
  x   <- c(0,1)
  
  for (j in sample(1:length(s$lp__), 100) ) {
    
    points(x, dbern(x,prob =  p_exploit_visible[i,j]), col = alpha("black",alpha = 0.3), lwd=1,space = 0.5, pch = 16, cex = 1.5 )
  }
  if(i==1) mtext("Exploitation Visible", side = 2, line = 4, cex = 0.8)
  
}

dev.off()
















####
##
# Plot example HMM trajectories with recovered states
##
###

s <- extract.samples(fit)



States <- matrix(0, nrow = 6935, ncol = 2000 )

for (t in 1:6935) {
  States[t,] <- s$viterbi[,t]
  
}

Likely_State = sapply(1:6935, function(x) as.integer(names(sort(table(States[x,]), decreasing = TRUE)[1]))     )






load("~/GitHub/CoinScrounge/data/dat_players_extended")
track_id =1
dat_players <- dat_players[-which(dat_players$IsAllowedToMove == "False"),]

state_seq_filled <- rep(NA, length(which(dat_players$track_id==track_id)))
for (i in which(dat_players$track_id==track_id)) {
  if (dat_players$any_exploit[i]==0){
    state_seq_filled[which(dat_players$track_id==track_id)==i] <- 1
  } else{
    state_seq_filled[which(dat_players$track_id==track_id)==i] <- Likely_State[which(which(dat_players$track_id==track_id & dat_players$any_exploit==1)==i)]
  }
}



graphics.off()
pdf("Tracks3.pdf", height = 9, width = 10)

any <- dat_players$any_exploit[dat_players$track_id == track_id] 
start_noone <-c(1, unlist(sapply(2:length(any), function(x) if(any[x-1] == 1 & any[x]==0){return(x)} )))
end_noone   <- unlist(sapply(2:length(any), function(x) if(any[x-1] == 0 & any[x]==1){return(x)} ))


start_social <-     unlist(sapply(2:length(state_seq_filled), function(x) if(state_seq_filled[x-1] == 2 & state_seq_filled[x]==1){return(x)} ))
end_social   <-     unlist(sapply(2:length(state_seq_filled), function(x) if(state_seq_filled[x-1] == 1 & state_seq_filled[x]==2){return(x)} ))



par(mfrow = c(7, 1), 
    mar = c(0.75,4,0,0), 
    oma = c(3,3.5,2.25,0))
plot(dat_players$angle[dat_players$track_id == track_id], ylim = c(-2,2), type = "l",yaxt ="n", ylab = "", bty = "n", xaxt = "n" , col = "black", lwd = 1.2) 
abline(v=which(dat_players$Exploit[dat_players$track_id == track_id] == 1), lwd = 1.2,col= alpha("black", alpha = 0.7), lty = 2)
mtext(side = 2, "Turning Angles", cex = 0.8, line = 3, col = "black")
axis(side = 2, at = c(-2,0,2), col.axis = "black", col = "black")

for (i in 1:length(start_noone)) {
  rect(start_noone[i],-2,end_noone[i],2,col = alpha("black", alpha = 0.08), bty ="n", lty =0)
}

for (i in 1:length(start_social)) {
  rect(start_social[i],-2,end_social[i],2,col = alpha(col.pal[5], alpha = 0.3), bty ="n", lty =0)
}

mtext(side = 3, "Inferred social relocation state                                                                                                                      ", col = alpha(col.pal[5], alpha = 0.7), cex = 1.1)
mtext(side = 3, "                                                                                     No group member(s) exploiting", col = alpha("black", alpha = 0.2), cex = 1.1)


plot(dat_players$mindeltadist[dat_players$track_id == track_id],type = "l", ylim = c(-3,3), ylab = "" ,yaxt="n", bty = "n", xaxt = "n" , col = "black", lwd = 1.2)   
abline(v=which(dat_players$Exploit[dat_players$track_id == track_id] == 1), lwd = 1.2,col= alpha("black", alpha = 0.7), lty = 2)
for (i in 1:length(start_noone)) {
  rect(start_noone[i],-3,end_noone[i],3,col = alpha("black", alpha = 0.08), bty ="n", lty =0)
}
for (i in 1:length(start_social)) {
  rect(start_social[i],-3,end_social[i],3,col = alpha(col.pal[5], alpha = 0.3), bty ="n", lty =0)
}

mtext(side = 2, "Change in Dist.", cex = 0.8, line = 3, col = "black")
axis(side = 2, at = c(-3,0,3), col.axis = "black", col = "black")

mtext(side = 2, "State-dependent Variables", cex = 1.4, line = 6,font = 3, col = "black")


plot(dat_players$closed[dat_players$track_id == track_id],type = "l", ylim = c(0,pi), yaxt ="n", ylab = "" , bty = "n", xaxt = "n" , col = "black", lwd = 1.2)   
abline(v=which(dat_players$Exploit[dat_players$track_id == track_id] == 1), lwd = 1.2,col= alpha("black", alpha = 0.7), lty = 2)
for (i in 1:length(start_noone)) {
  rect(start_noone[i],0,end_noone[i],pi,col = alpha("black", alpha = 0.08), bty ="n", lty =0)
}
for (i in 1:length(start_social)) {
  rect(start_social[i],0,end_social[i],pi,col = alpha(col.pal[5], alpha = 0.3), bty ="n", lty =0)
}

mtext(side = 2, "Relative Bearing", cex = 0.8, line = 3, col = "black")
axis(side = 2, at = c(0,1.5,3), col.axis = "black", col = "black")





plot(dat_players$exploit_visible[dat_players$track_id == track_id], type = "l", ylim = c(0,1), ylab = "", , bty = "n", yaxt = "n",xaxt = "n", col = "black" , lwd = 1.2)  
abline(v=which(dat_players$Exploit[dat_players$track_id == track_id] == 1), lwd = 1.2,col= alpha("black", alpha = 0.7), lty = 2)
for (i in 1:length(start_noone)) {
  rect(start_noone[i],0,end_noone[i],1,col = alpha("black", alpha = 0.08), bty ="n", lty =0)
}



axis(side = 2, at = c(0,1),col.axis = "black", col = "black")
mtext(side = 2, "Exploit. Visible", cex = 0.8, line = 3, col = "black")




plot(dat_players$DistPatch[dat_players$track_id == track_id], ylim = c(0,100), type = "l", ylab = "",yaxt="n",  bty = "n", xaxt = "n", col = "black" , lwd = 1.2)  
for (i in 1:length(start_noone)) {
  rect(start_noone[i],0,end_noone[i],100,col = alpha("black", alpha = 0.08), bty ="n", lty =0)
}


mtext(side = 2, "Vis. Patch Dist.", cex = 0.8, line = 3, col = "black")
abline(v=which(dat_players$Exploit[dat_players$track_id == track_id] == 1), lwd = 1.2,col= alpha("black", alpha = 0.7), lty = 2)
axis(side = 2, at = c(0,50,100),col.axis = "black", col = "black")

plot(dat_players$NumberPatch[dat_players$track_id == track_id]+1, ylim = c(1,3), type = "l", ylab = "",yaxt="n",  bty = "n", xaxt = "n" , col = "black", lwd = 1.2)  



mtext(side = 2, "Vis. Expl. Players", cex = 0.8, line = 3, col = "black")
abline(v=which(dat_players$Exploit[dat_players$track_id == track_id] == 1), lwd = 1.2,col= alpha("black", alpha = 0.7), lty = 2)
for (i in 1:length(start_noone)) {
  rect(start_noone[i],1,end_noone[i],3,col = alpha("black", alpha = 0.08), bty ="n", lty =0)
}
axis(side = 2, at = c(1,2,3), col.axis = "black", col = "black")



plot(dat_players$TimeSinceExploit[dat_players$track_id == track_id], type = "l",yaxt ="n",ylim = c(0,150), xaxt = "n",ylab = "", bty = "n", col = "black" , lwd = 1.2)  
abline(v=which(dat_players$Exploit[dat_players$track_id == track_id] == 1), lwd = 1.2,col= alpha("black", alpha = 0.7), lty = 2)
for (i in 1:length(start_noone)) {
  rect(start_noone[i],0,end_noone[i],150,col = alpha("black", alpha = 0.08), bty ="n", lty =0)
}

axis(side = 1, at = seq(0,500,100))
mtext(side = 2, "Time since Success", cex = 0.8, line = 3, col = "black")
axis(side = 2, at = c(0,75,150), col.axis = "black", col = "black")
mtext(side = 2, "State Predictors                                                     ", cex = 1.4, line = 2,font = 3, outer = TRUE, col = "black")

mtext("Time step", side = 1,line = 2, outer = TRUE, cex = 1.4)

add_legend("top", legend=c("Player exploiting"),lty = 2, bty='n',lwd=1.2, text.col =alpha("black", alpha = 0.7), col= alpha("black", alpha = 0.7), cex=1.6)

dev.off()





