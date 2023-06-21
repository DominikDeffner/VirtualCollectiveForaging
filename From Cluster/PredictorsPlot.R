
# Script for Fig. 3: Computational modelling results: state predictors. 

graphics.off()
pdf("Predictors.pdf", height = 7.5, width = 6)

layout(matrix(c(1,1,1,2,2,2,3, 4,4, 5,5, 
                6,6,6,7,7,7,8,9,9,10,10,
                11,11,11,12,12,12,13,14,14,15,15,
                16,16,16,17,17,17,18,19,19,20,20), 4, 11, byrow = TRUE))

par(mar = c(3,0.5, 0.5,0.5), 
    oma = c(0,3,3.5,0))

#Baseline
post1 <- inv_logit(s$logit_p_I_S[,1,1] + s$b_vis[,1,1] )
dens <- density(post1)
x1 <- min(which(dens$x >= quantile(post1, 0.05)))  
x2 <- max(which(dens$x <  quantile(post1, 0.95)))
plot(dens, xlim = c(0.1,0.4),xaxt="n", ylim = c(0,55), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[2],alpha = 0.9), border = NA))
mtext('a', side=3, line=2.5, at=0.05)


x1 <- min(which(dens$x >= quantile(post1, 0)))  
x2 <- max(which(dens$x <  quantile(post1, 1)))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[2],alpha = 0.2), border = NA))
legend("topleft", title = "Environment", c("Concentrated", "Distributed"),col = c(alpha(col.pal[2],alpha = 0.9),alpha(col.pal[3],alpha = 0.9)), cex = 0.9, lwd = 8, lty = 1, bty = "n")
axis(side = 1, at = seq(0.1,0.4,0.1))
mtext(side = 3, "Group Incentives", line = 1, cex = 1)
mtext(side = 2, expression("Avg. Switching Prob. I->S"), line = 2, cex = 0.8)

par(new = TRUE)

post2 <- inv_logit(s$logit_p_I_S[,1,2] + s$b_vis[,1,2] )
dens <- density(post2)
x1 <- min(which(dens$x >= quantile(post2, 0.05)))  
x2 <- max(which(dens$x <  quantile(post2, 0.95)))
plot(dens, xlim = c(0.1,0.4),xaxt="n", ylim = c(0,55), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[3],alpha = 0.9), border = NA))

x1 <- min(which(dens$x >= quantile(post2, 0)))  
x2 <- max(which(dens$x <  quantile(post2, 1)))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[3],alpha = 0.2), border = NA))

post3 <- inv_logit(s$logit_p_I_S[,2,1] + s$b_vis[,2,1] )
dens <- density(post3)
x1 <- min(which(dens$x >= quantile(post3, 0.05)))  
x2 <- max(which(dens$x <  quantile(post3, 0.95)))
plot(dens, xlim = c(0.1,0.4),xaxt="n", ylim = c(0,55), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[2],alpha = 0.9), border = NA))

x1 <- min(which(dens$x >= quantile(post3, 0)))  
x2 <- max(which(dens$x <  quantile(post3, 1)))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[2],alpha = 0.2), border = NA))

legend("topright",title = "Incentives", c("Group", "Individual"), col = alpha("black", alpha = 0.6), pch = c(1,16),  lwd = 1, lty = 1, bty = "n", cex = 0.9)
axis(side = 1, at = seq(0.1,0.4,0.1))
mtext(side = 3, "Individual Incentives", line = 1, cex = 1)

par(new = TRUE)

post4 <- inv_logit(s$logit_p_I_S[,2,2] + s$b_vis[,2,2] )
dens <- density(post4)
x1 <- min(which(dens$x >= quantile(post4, 0.05)))  
x2 <- max(which(dens$x <  quantile(post4, 0.95)))
plot(dens, xlim = c(0.1,0.4),xaxt="n", ylim = c(0,55), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[3],alpha = 0.9), border = NA))

x1 <- min(which(dens$x >= quantile(post4, 0)))  
x2 <- max(which(dens$x <  quantile(post4, 1)))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[3],alpha = 0.2), border = NA))

plot.new()

#Concentrated
plot(dat_baseline$pred[dat_baseline$Environment==1], dat_baseline$coins[dat_baseline$Environment==1],  col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_baseline$Incentives[dat_baseline$Environment==1]==1, 1, 16), bty = "n", xlim = c(0.05,0.41), ylim = c(50,200) , xaxt = "n")
plot_regression_line(s_baseline, dat_baseline$pred, 1, color = col.pal[2])

mean <- round(mean(s_baseline$weight[,1]),2)
lower <- round(PI(s_baseline$weight[,1], 0.9)[1],2)
upper <- round(PI(s_baseline$weight[,1], 0.9)[2],2)
text(0.25, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 0.85, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))
axis(side = 1, at = seq(0.1,0.4,0.15), labels = c("0.1","0.25","0.4"))
mtext("Coins", side = 2, line = 2.5, cex = 0.8)

mtext('b', side=3, line=2.5, at=-0.15)


plot(dat_baseline$pred[dat_baseline$Environment==2], dat_baseline$coins[dat_baseline$Environment==2], yaxt = "n",col = alpha(col.pal[3], alpha = 0.4), pch = ifelse(dat_baseline$Incentives[dat_baseline$Environment==2]==1, 1, 16)  , bty = "n", xlim = c(0.05,0.41), ylim = c(50,200) , xaxt = "n")
plot_regression_line(s_baseline, dat_baseline$pred, 2, color = col.pal[3])
axis(side = 1, at = seq(0.1,0.4,0.15), labels = c("0.1","0.25","0.4"))

mean <- round(mean(s_baseline$weight[,2]),2)
lower <- round(PI(s_baseline$weight[,2], 0.9)[1],2)
upper <- round(PI(s_baseline$weight[,2], 0.9)[2],2)
text(0.25, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 0.85, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))
mtext("Ind. Weights -> Success", side = 3, at = 0, line=1)

#Distance
post1 <- s$b_dist[,1,1] 
dens <- density(post1)
x1 <- min(which(dens$x >= quantile(post1, 0.05)))  
x2 <- max(which(dens$x <  quantile(post1, 0.95)))
plot(dens, xlim = c(-0.6,0.6),xaxt="n", ylim = c(0,10), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[2],alpha = 0.9), border = NA))

x1 <- min(which(dens$x >= quantile(post1, 0)))  
x2 <- max(which(dens$x <  quantile(post1, 1)))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[2],alpha = 0.2), border = NA))
axis(side = 1, at = seq(-0.6,0.6,0.3))
mtext(side = 2, "Vis. Patch Dist.", line = 2, cex = 0.8)

par(new = TRUE)

post2 <-  s$b_dist[,1,2] 
dens <- density(post2)
x1 <- min(which(dens$x >= quantile(post2, 0.05)))  
x2 <- max(which(dens$x <  quantile(post2, 0.95)))
plot(dens, xlim = c(-0.6,0.6),xaxt="n", ylim = c(0,10), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[3],alpha = 0.9), border = NA))

x1 <- min(which(dens$x >= quantile(post2, 0)))  
x2 <- max(which(dens$x <  quantile(post2, 1)))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[3],alpha = 0.2), border = NA))
abline(v = 0, lty = 2, col = "grey")

post3 <- s$b_dist[,2,1] 
dens <- density(post3)
x1 <- min(which(dens$x >= quantile(post3, 0.05)))  
x2 <- max(which(dens$x <  quantile(post3, 0.95)))
plot(dens, xlim = c(-0.6,0.6),xaxt="n", ylim = c(0,10), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[2],alpha = 0.9), border = NA))

x1 <- min(which(dens$x >= quantile(post3, 0)))  
x2 <- max(which(dens$x <  quantile(post3, 1)))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[2],alpha = 0.2), border = NA))
axis(side = 1, at = seq(-0.6,0.6,0.3))

par(new = TRUE)

post4 <-  s$b_dist[,2,2] 
dens <- density(post4)
x1 <- min(which(dens$x >= quantile(post4, 0.05)))  
x2 <- max(which(dens$x <  quantile(post4, 0.95)))
plot(dens, xlim = c(-0.6,0.6),xaxt="n", ylim = c(0,10), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[3],alpha = 0.9), border = NA))

x1 <- min(which(dens$x >= quantile(post4, 0)))  
x2 <- max(which(dens$x <  quantile(post4, 1)))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[3],alpha = 0.2), border = NA))
abline(v = 0, lty = 2, col = "grey")

plot.new()

plot(dat_dist$pred[dat_dist$Environment==1], dat_dist$coins[dat_dist$Environment==1],  col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_baseline$Incentives[dat_baseline$Environment==1]==1, 1, 16)  , bty = "n", xlim = c(-0.7,0.2), ylim = c(50,200), xaxt = "n")
plot_regression_line(s_dist, dat_dist$pred, 1, color = col.pal[2])
#axis(side = 1, at = seq(-0.6,0.2,0.4))
segments(0,50,0,200, lty = 2, col = "grey")
axis(side = 1, at = seq(-0.6,0.2,0.4))

mean <- round(mean(s_dist$weight[,1]),2)
lower <- round(PI(s_dist$weight[,1], 0.9)[1],2)
upper <- round(PI(s_dist$weight[,1], 0.9)[2],2)
text(-0.2, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 0.85, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))
mtext("Coins", side = 2, line = 2.5, cex = 0.8)

plot(dat_dist$pred[dat_dist$Environment==2], dat_dist$coins[dat_dist$Environment==2],  col = alpha(col.pal[3], alpha = 0.4), pch = ifelse(dat_baseline$Incentives[dat_baseline$Environment==2]==1, 1, 16)  , bty = "n", xlim = c(-0.7,0.2), ylim = c(50,200) , yaxt = "n", xaxt = "n")
plot_regression_line(s_dist, dat_dist$pred, 2, color = col.pal[3])
axis(side = 1, at = seq(-0.6,0.2,0.4))
segments(0,50,0,200, lty = 2, col = "grey")

mean <- round(mean(s_dist$weight[,2]),2)
lower <- round(PI(s_dist$weight[,2], 0.9)[1],2)
upper <- round(PI(s_dist$weight[,2], 0.9)[2],2)
text(-0.2, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 0.85, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))


#Number
post1 <- s$b_numb[,1,1] 
dens <- density(post1)
x1 <- min(which(dens$x >= quantile(post1, 0.05)))  
x2 <- max(which(dens$x <  quantile(post1, 0.95)))
plot(dens, xlim = c(-0.6,0.6),xaxt="n", ylim = c(0,6), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[2],alpha = 0.9), border = NA))

x1 <- min(which(dens$x >= quantile(post1, 0)))  
x2 <- max(which(dens$x <  quantile(post1, 1)))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[2],alpha = 0.2), border = NA))
axis(side = 1, at = seq(-0.6,0.6,0.3))
mtext(side = 2, "Vis. Expl. Players", line = 2, cex = 0.8)

par(new = TRUE)

post2 <-  s$b_numb[,1,2] 
dens <- density(post2)
x1 <- min(which(dens$x >= quantile(post2, 0.05)))  
x2 <- max(which(dens$x <  quantile(post2, 0.95)))
plot(dens, xlim = c(-0.6,0.6),xaxt="n", ylim = c(0,6), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[3],alpha = 0.9), border = NA))

x1 <- min(which(dens$x >= quantile(post2, 0)))  
x2 <- max(which(dens$x <  quantile(post2, 1)))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[3],alpha = 0.2), border = NA))
abline(v = 0, lty = 2, col = "grey")

post3 <- s$b_numb[,2,1] 
dens <- density(post3)
x1 <- min(which(dens$x >= quantile(post3, 0.05)))  
x2 <- max(which(dens$x <  quantile(post3, 0.95)))
plot(dens, xlim = c(-0.6,0.6),xaxt="n", ylim = c(0,6), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[2],alpha = 0.9), border = NA))

x1 <- min(which(dens$x >= quantile(post3, 0)))  
x2 <- max(which(dens$x <  quantile(post3, 1)))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[2],alpha = 0.2), border = NA))
axis(side = 1, at = seq(-0.6,0.6,0.3))

par(new = TRUE)

post4 <-  s$b_numb[,2,2] 
dens <- density(post4)
x1 <- min(which(dens$x >= quantile(post4, 0.05)))  
x2 <- max(which(dens$x <  quantile(post4, 0.95)))
plot(dens, xlim = c(-0.6,0.6),xaxt="n", ylim = c(0,6), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[3],alpha = 0.9), border = NA))

x1 <- min(which(dens$x >= quantile(post4, 0)))  
x2 <- max(which(dens$x <  quantile(post4, 1)))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[3],alpha = 0.2), border = NA))
abline(v = 0, lty = 2, col = "grey")

plot.new()


plot(dat_numb$pred[dat_numb$Environment==1], dat_numb$coins[dat_numb$Environment==1],  col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_baseline$Incentives[dat_baseline$Environment==1]==1, 1, 16)  , bty = "n", xlim = c(-0.5,0.5), ylim = c(50,200), xaxt = "n")
plot_regression_line(s_numb, dat_numb$pred, 1, color = col.pal[2])
segments(0,50,0,200, lty = 2, col = "grey")
axis(side = 1, at = seq(-0.5,0.5,0.5))

mean <- round(mean(s_numb$weight[,1]),2)
lower <- round(PI(s_numb$weight[,1], 0.9)[1],2)
upper <- round(PI(s_numb$weight[,1], 0.9)[2],2)
text(0, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 0.85, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))
mtext("Coins", side = 2, line = 2.5, cex = 0.8)


plot(dat_numb$pred[dat_numb$Environment==2], dat_numb$coins[dat_numb$Environment==2],col = alpha(col.pal[3], alpha = 0.4), pch = ifelse(dat_baseline$Incentives[dat_baseline$Environment==2]==1, 1, 16)  , bty = "n", xlim = c(-0.5,0.5), ylim = c(50,200) , yaxt = "n", xaxt = "n")
plot_regression_line(s_numb, dat_numb$pred, 2, color = col.pal[3])
axis(side = 1, at = seq(-0.5,0.5,0.5))
segments(0,50,0,200, lty = 2, col = "grey")

mean <- round(mean(s_numb$weight[,2]),2)
lower <- round(PI(s_numb$weight[,2], 0.9)[1],2)
upper <- round(PI(s_numb$weight[,2], 0.9)[2],2)
text(0, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 0.85, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))


#Time
post1 <- s$b_time[,1,1] 
dens <- density(post1)
x1 <- min(which(dens$x >= quantile(post1, 0.05)))  
x2 <- max(which(dens$x <  quantile(post1, 0.95)))
plot(dens, xlim = c(-0.6,0.6),xaxt="n", ylim = c(0,12.5), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[2],alpha = 0.9), border = NA))

x1 <- min(which(dens$x >= quantile(post1, 0)))  
x2 <- max(which(dens$x <  quantile(post1, 1)))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[2],alpha = 0.2), border = NA))
axis(side = 1, at = seq(-0.6,0.6,0.3))
mtext(side = 2, "Time since Success", line = 2, cex = 0.8)

par(new = TRUE)

post2 <-  s$b_time[,1,2] 
dens <- density(post2)
x1 <- min(which(dens$x >= quantile(post2, 0.05)))  
x2 <- max(which(dens$x <  quantile(post2, 0.95)))
plot(dens, xlim = c(-0.6,0.6),xaxt="n", ylim = c(0,12.5), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[3],alpha = 0.9), border = NA))

x1 <- min(which(dens$x >= quantile(post2, 0)))  
x2 <- max(which(dens$x <  quantile(post2, 1)))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[3],alpha = 0.2), border = NA))
abline(v = 0, lty = 2, col = "grey")

post3 <- s$b_time[,2,1] 
dens <- density(post3)
x1 <- min(which(dens$x >= quantile(post3, 0.05)))  
x2 <- max(which(dens$x <  quantile(post3, 0.95)))
plot(dens, xlim = c(-0.6,0.6),xaxt="n", ylim = c(0,12.5), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[2],alpha = 0.9), border = NA))

x1 <- min(which(dens$x >= quantile(post3, 0)))  
x2 <- max(which(dens$x <  quantile(post3, 1)))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[2],alpha = 0.2), border = NA))

axis(side = 1, at = seq(-0.6,0.6,0.3))

par(new = TRUE)

post4 <-  s$b_time[,2,2] 
dens <- density(post4)
x1 <- min(which(dens$x >= quantile(post4, 0.05)))  
x2 <- max(which(dens$x <  quantile(post4, 0.95)))
plot(dens, xlim = c(-0.6,0.6),xaxt="n", ylim = c(0,12.5), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[3],alpha = 0.9), border = NA))

x1 <- min(which(dens$x >= quantile(post4, 0)))  
x2 <- max(which(dens$x <  quantile(post4, 1)))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[3],alpha = 0.2), border = NA))

abline(v = 0, lty = 2, col = "grey")

plot.new()

plot(dat_time$pred[dat_time$Environment==1], dat_time$coins[dat_time$Environment==1],col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_baseline$Incentives[dat_baseline$Environment==1]==1, 1, 16)  , bty = "n", xlim = c(-0.3,0.3), ylim = c(50,200) ,  xaxt = "n")
plot_regression_line(s_time, dat_time$pred, 1, color = col.pal[2])
segments(0,50,0,200, lty = 2, col = "grey")
axis(side = 1, at = seq(-0.3,0.3,0.3), labels = c("-0.3","0","0.3"))
mtext("Coins", side = 2, line = 2.5, cex = 0.8)

mean <- round(mean(s_time$weight[,1]),2)
lower <- round(PI(s_time$weight[,1], 0.9)[1],2)
upper <- round(PI(s_time$weight[,1], 0.9)[2],2)
text(0.05, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 0.85, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

plot(dat_time$pred[dat_time$Environment==2], dat_time$coins[dat_time$Environment==2], col = alpha(col.pal[3], alpha = 0.4), pch = ifelse(dat_baseline$Incentives[dat_baseline$Environment==2]==1, 1, 16)  , bty = "n", xlim = c(-0.3,0.3), ylim = c(50,200) , yaxt = "n", xaxt = "n")
plot_regression_line(s_time, dat_time$pred, 2, color = col.pal[3])
axis(side = 1, at = seq(-0.3,0.3,0.3), labels = c("-0.3","0","0.3"))
segments(0,50,0,200, lty = 2, col = "grey")

mean <- round(mean(s_time$weight[,2]),2)
lower <- round(PI(s_time$weight[,2], 0.9)[1],2)
upper <- round(PI(s_time$weight[,2], 0.9)[2],2)
text(0.05, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 0.85, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

dev.off()
