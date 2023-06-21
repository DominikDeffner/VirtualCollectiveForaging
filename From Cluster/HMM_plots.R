

#Script to process and plot HMM output


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


#Normal for change in distance
mu_distance <- matrix(NA, 2, length(s$lp__))
for (i in 1:2) mu_distance[i,] <- s$mu_distance[,i]

sigma_distance <- matrix(NA, 2, length(s$lp__))
for (i in 1:2) sigma_distance[i,] <- s$sigma_distance[,i]



graphics.off()
pdf("HMMStateDepDist.pdf", height = 5, width = 5)

par(mfrow = c(3,2), 
    oma=c(1,1.5,2.2,0), 
    mar=c(2,3,0,0))

#Von Mises distribution for Turning angles

for (i in 1:2) {
  upperlim <- ifelse(i == 1, 1, 3)
  plot(1:10,type="n", ylim = c(0,upperlim),xlim = c(-pi,pi),  bty = "n", ylab = "")
  x   <- seq(-pi,pi,length.out=1000)
  
  for (j in sample(1:length(s$lp__), 50) ) {
      l <- Conv2(xangle = xangle[i, j], yangle = yangle[i,j])
      lines(x,dvonmises(x,l$mu, l$kappa), col = alpha(col.pal[ifelse(i==1,4,5)],alpha = 0.6), lwd=0.1)
  }
  if(i==1) mtext("State 1: Individual Exploration", side = 3, line = 1, cex = 0.9)
  if(i==1) mtext("Turning Angles", side = 2, line = 3, cex = 1)
  
  if(i==2) mtext("State 2: Social Relocation", side = 3, line = 1, cex = 0.9)
}

for (i in 1:2) {
  upperlim <-  ifelse(i == 1, 1, 5)
  plot(1:10,type="n", xlim = c(-3, 3),ylim = c(0,upperlim), bty = "n", ylab = "")
  x   <- seq(-3,3,length.out=1000)
  
  for (j in sample(1:length(s$lp__), 50) ) {
    
    lines(x,dnorm(x,mu_distance[i,j], sigma_distance[i,j]), col = alpha(col.pal[ifelse(i==1,4,5)],alpha = 0.6), lwd=1)
  }
  if(i==1) mtext("Change in Distance", side = 2, line = 3, cex = 1)
  
}


for (i in 1:2) {
  upperlim <- ifelse(i == 1, 1, 7)
  
  plot(1:10,type="n", xlim = c(0,3),ylim = c(0,upperlim), bty = "n", ylab = "")
  x   <- seq(0,3,length.out=1000)
  
  for (j in sample(1:length(s$lp__), 50) ) {

    lines(x,dlnorm(x,log(mu_closed[i,j]), sigma_closed[i,j]), col = alpha(col.pal[ifelse(i==1,4,5)],alpha = 0.6), lwd=1)
  }
  if(i==1) mtext("Relative Bearing", side = 2, line = 3, cex = 1)
  
}



dev.off()







































States <- matrix(0, nrow = 6935, ncol = 2000 )

for (t in 1:6935) {
  States[t,] <- s$viterbi[,t]
  
}

Likely_State = sapply(1:6935, function(x) as.integer(names(sort(table(States[x,]), decreasing = TRUE)[1]))     )


