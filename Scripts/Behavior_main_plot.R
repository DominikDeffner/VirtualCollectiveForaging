
#graphics.off()
#pdf("Behavioral.pdf", height = 5.5, width = 10)

layout(matrix(c(1,1,1,
                2,   #gap
                3,3,3,
                4, #gap
                5,5,
                6,6,
                7,
                8,8,
                9,9,
                
                10,10,10,10,10,
                11,11,11,
                12,12,
                13,13,
                14,
                15,15, 
                16,16), 2, 17, byrow = TRUE))

par(mar = c(4,0.5,1.5,0), 
    oma = c(0,3.25,0,1.5))


#Coins
dat <- list(N = length(d$joining), 
            N_id = length(unique(d$id))  , 
            N_group = length(unique(d$Group))  , 
            coins = d$Coins, 
            Environment = ifelse(d$Env == "C",1,2), 
            Incentives = ifelse(d$Pay == "Coop",1,2),
            group = d$Group, 
            id = d$id )

behavioral_plotting_fct()
mtext('a', side=3, line=0, at=0.5)
mtext('Group', side=1, line=-1, at=1.75, cex = 0.8)
mtext('Individual', side=1, line=-1, at=3, cex = 0.8)
mtext('Incentives', side=1, line=0.25, at=2.35, cex = 0.8)


segments(2.75,200,3.25,200)
text(3,207, "8.4 [0.5, 16.7]")

plot.new()


# Scrounging

#Stable 
dens <- density(s_scrounging$scrounging_rate[,1,1])
x1 <- min(which(dens$x >= quantile(s_scrounging$scrounging_rate[,1,1], 0)))  
x2 <- max(which(dens$x <  quantile(s_scrounging$scrounging_rate[,1,1], 1)))
plot(dens, xlim = c(0.1,0.9),xaxt="n", ylim = c(0,20), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[2],alpha = 0.2), border = NA))
mtext(side = 1, "Scrounging Rate", line = 2.25, cex = 0.8)
legend("top",title = "Incentives", c("Group", "Individual"), col = c(alpha("black",alpha = 0.2),alpha("black",alpha = 0.9)), cex = 1, lwd = 8, lty = 1, bty = "n")
axis(side = 1, at = seq(0.1,0.9,0.4))

par(new = TRUE)

dens <- density(s_scrounging$scrounging_rate[,1,2])
x1 <- min(which(dens$x >= quantile(s_scrounging$scrounging_rate[,1,2], 0)))  
x2 <- max(which(dens$x <  quantile(s_scrounging$scrounging_rate[,1,2], 1)))
plot(dens, xlim = c(0.1,0.9),xaxt="n", ylim = c(0,20), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[3],alpha = 0.2), border = NA))

par(new = TRUE)

dens <- density(s_scrounging$scrounging_rate[,2,1])
x1 <- min(which(dens$x >= quantile(s_scrounging$scrounging_rate[,2,1], 0)))  
x2 <- max(which(dens$x <  quantile(s_scrounging$scrounging_rate[,2,1], 1)))
plot(dens, xlim = c(0.1,0.9),xaxt="n", ylim = c(0,20), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[2],alpha = 0.9), border = NA))

par(new = TRUE)

dens <- density(s_scrounging$scrounging_rate[,2,2])
x1 <- min(which(dens$x >= quantile(s_scrounging$scrounging_rate[,2,2], 0)))  
x2 <- max(which(dens$x <  quantile(s_scrounging$scrounging_rate[,2,2], 1)))
plot(dens, xlim = c(0.1,0.9),xaxt="n", ylim = c(0,20), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[3],alpha = 0.9), border = NA))


mtext('c', side=3, line=0, at=0.05)


plot.new()





#Individual Scrounging

plot(scrounging$Concentrated, scrounging$CoinsCon,col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(scrounging$Incentives==1, 1, 16), yaxt = "n", bty = "n",xlim = c(0.15,0.9), ylim = c(30, 230) , xaxt = "n", xlab = "")
axis(side = 1, c(0.3,0.6,0.9))
lower <- sapply(1:160 , function(i) PI(scrounging_con[i,])[1])
upper <- sapply(1:160 , function(i) PI(scrounging_con[i,])[2])
segments(lower,scrounging$CoinsCon,upper,scrounging$CoinsCon, col = alpha(col.pal[2], alpha = 0.2), lwd=1)
mtext('d', side=3, line=0, at=0)

plot_regression_line(s_scrounging, scrounging$Concentrated, 1, color = col.pal[2])
mean <- round(mean(s_scrounging$weight[,1]),2)
lower <- round(PI(s_scrounging$weight[,1], 0.9)[1],2)
upper <- round(PI(s_scrounging$weight[,1], 0.9)[2],2)
text(0.6, 220, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 0.9, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper)|any(c(sign(lower), sign(upper))==0), 1, 0.4 ) ))
axis(side = 2, at = seq(50, 200, 50))
mtext(side = 2, "Coins", line = 2.5, cex = 0.8)
mtext(side = 1,at = 1, "ID-specific Scrounging Rate", line = 2.25, cex = 0.8)

plot(scrounging$Distributed, scrounging$CoinsDist,col = alpha(col.pal[3], alpha = 0.4), pch = ifelse(scrounging$Incentives==1, 1, 16)  , bty = "n",xlim = c(0.05,0.6), ylim = c(30,230) , yaxt = "n", xlab = "", xaxt = "n")
axis(side = 1, c(0.1,0.3,0.5))

plot_regression_line(s_scrounging, scrounging$Distributed, 2, color = col.pal[3])
mean <- round(mean(s_scrounging$weight[,2]),2)
lower <- round(PI(s_scrounging$weight[,2], 0.9)[1],2)
upper <- round(PI(s_scrounging$weight[,2], 0.9)[2],2)
text(0.3, 220, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 0.9, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper)|any(c(sign(lower), sign(upper))==0), 1, 0.4 ) ))

lower <- sapply(1:160 , function(i) PI(scrounging_dist[i,])[1])
upper <- sapply(1:160 , function(i) PI(scrounging_dist[i,])[2])
segments(lower,scrounging$CoinsDist,upper,scrounging$CoinsDist, col = alpha(col.pal[3], alpha = 0.2), lwd=1)

plot.new()

#Distance
#Concentrated

plot(dat_distance$pred[dat_distance$Environment==1], dat_distance$coins[dat_distance$Environment==1],  col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_distance$Incentives[dat_distance$Environment==1]==1, 1, 16)  , bty = "n", xlim = c(-3.5,3.2), ylim = c(30,230), xlab = "", yaxt = "n", xaxt = "n")
plot_regression_line(s_distance, dat_distance$pred[-which(dat_distance$pred==-10)], 1, color = col.pal[2])
#axis(side = 1, at = seq(-0.3,0.1,0.2))
mtext('e', side=3, line=0, at=-5)
axis(side = 2, at = seq(50, 200, 50))
mtext(side = 2, "Coins", line = 2.5, cex = 0.8)
axis(side = 1, at = c(-3, 0,3))

mean <- round(mean(s_distance$weight[,1]),2)
lower <- round(PI(s_distance$weight[,1], 0.9)[1],2)
upper <- round(PI(s_distance$weight[,1], 0.9)[2],2)
text(0, 220, paste0(mean, " ","[",lower, ",", upper, "]"),cex = 0.9, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper)|any(c(sign(lower), sign(upper))==0)|any(c(sign(lower), sign(upper))==0), 1, 0.4 ) ))
mtext(side = 1,at = 4, "Distance to group members", line = 2.25, cex = 0.8)


#Distributed
plot(dat_distance$pred[dat_distance$Environment==2], dat_distance$coins[dat_distance$Environment==2],col = alpha(col.pal[3], alpha = 0.4), pch = ifelse(dat_distance$Incentives[dat_distance$Environment==2]==1, 1, 16)  , bty = "n", xlim = c(-3.5,3.2), ylim = c(30,230) , xlab = "", yaxt = "n", xaxt = "n")
plot_regression_line(s_distance, dat_distance$pred[-which(dat_distance$pred==-10)], 2, color = col.pal[3])
axis(side = 1, at = c(-3, 0,3))


mean <- round(mean(s_distance$weight[,2]),2)
lower <- round(PI(s_distance$weight[,2], 0.9)[1],2)
upper <- round(PI(s_distance$weight[,2], 0.9)[2],2)
text(0, 220, paste0(mean, " ","[",lower, ",", upper, "]"),cex = 0.9, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper)|any(c(sign(lower), sign(upper))==0), 1, 0.4 ) ))



mtext(side = 4, "Individual Success", line = 0.5, cex = 1)







#### Coins Temporal
plot(NA, type = "l", xlim = c(1,12), ylim = c(-0.07,0.07), xlab = "", ylab = "", bty = "n", yaxt = "n", xaxt = "n")

HPDI <- apply(contrast_con,2,quantile,probs=c(0.05,0.95))
Lower <- HPDI[1,]
Higher <- HPDI[2,]
points(1:12-0.2,apply(contrast_con,2,mean), col = col.pal[2], pch = 16)
segments(x0 = 1:12-0.2, y0 = Lower, x1=  1:12-0.2, y1 = Higher, lwd = 2, col = col.pal[2])
abline(h = 0, lty = 2, col = "grey")


HPDI <- apply(contrast_dist,2,quantile,probs=c(0.05,0.95))
Lower <- HPDI[1,]
Higher <- HPDI[2,]

points(1:12+0.2, apply(contrast_dist,2,mean), col = col.pal[3], pch = 16)
segments(x0 = 1:12+0.2, y0 = Lower, x1=  1:12+0.2, y1 = Higher, lwd = 2, col = col.pal[3])

mtext(side = 3, expression("     P(Exploit. Group Incentives) - \n P(Exploit. Individual Incentives)"), line = -2.5, cex = 0.8)

axis(side = 1,  seq(1,12,1) , at = seq(1,12,1), cex.axis=1)
axis(side = 2, at = c(-0.05,0,0.05))

mtext(side = 1, "Time in round [min]", line = 2.5, cex = 0.8)
mtext('b', side=3, line=-1, at=-1.3)

plot.new()
legend("topleft", title = "Environment", c("Concentrated", "Distributed"), col = c(alpha(col.pal[2],alpha = 0.9),alpha(col.pal[3],alpha = 0.9)), cex = 1.1, lwd = 8, lty = 1)
legend("bottom",title = "Incentives", c("Group", "Individual"), col = alpha("black", alpha = 0.6), pch = c(1,16),cex = 1, lwd = 1, lty = 1, bty = "n")



















#### Density

plot(dat_dens$pred[dat_dens$Environment==1], dat_dens$coins[dat_dens$Environment==1],col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_dens$Incentives[dat_dens$Environment==1]==1, 1, 16)  , bty = "n", xlim = c(1,4), ylim = c(50,200) , yaxt = "n", xaxt = "n", xlab = "")
plot_regression_line(s_dens, dat_dens$pred, 1, color = col.pal[2])

mean <- round(mean(s_dens$weight[,1]),2)
lower <- round(PI(s_dens$weight[,1], 0.9)[1],2)
upper <- round(PI(s_dens$weight[,1], 0.9)[2],2)
text(2.5, 190, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 0.9, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper)|any(c(sign(lower), sign(upper))==0), 1, 0.4 ) ))
mtext(side = 1, "                             Avg. Forager Density at Patch", line = 2.5, cex = 0.8)
axis(side = 1, at = seq(1,4,length.out = 3), labels = c("1","2.5","4"))
axis(side = 2, at = seq(50, 200, 50))
mtext(side = 2, "Avg. Coins", line = 2.5, cex = 0.8)

plot(dat_dens$pred[dat_dens$Environment==2], dat_dens$coins[dat_dens$Environment==2],col = alpha(col.pal[3], alpha = 0.4), pch = ifelse(dat_dens$Incentives[dat_dens$Environment==2]==1, 1, 16)  , bty = "n", xlim = c(1,4), ylim = c(50,200) , yaxt = "n", xaxt = "n", xlab = "")
plot_regression_line(s_dens, dat_dens$pred, 2, color = col.pal[3])
axis(side = 1, at = seq(1,4,length.out = 3), labels = c("1","2.5","4"))

mean <- round(mean(s_dens$weight[,2]),2)
lower <- round(PI(s_dens$weight[,2], 0.9)[1],2)
upper <- round(PI(s_dens$weight[,2], 0.9)[2],2)
text(2.5, 190, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 0.9, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper)|any(c(sign(lower), sign(upper))==0), 1, 0.4 ) ))

plot.new()




plot(dat_dist_group$pred[dat_dist_group$Environment==1], dat_dist_group$coins[dat_dist_group$Environment==1],  col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_dist_group$Incentives[dat_dist_group$Environment==1]==1, 1, 16)  , bty = "n", xlim = c(-3.5,3.2), ylim = c(50, 200), yaxt = "n", xaxt = "n", xlab = "")
plot_regression_line(s_distance_group, dat_dist_group$pred, 1, color = col.pal[2])
axis(side = 1, at = c(-3, 0,3))
mean <- round(mean(s_distance_group$weight[,1]),2)
lower <- round(PI(s_distance_group$weight[,1], 0.9)[1],2)
upper <- round(PI(s_distance_group$weight[,1], 0.9)[2],2)
text(0, 190, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 0.9, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper)|any(c(sign(lower), sign(upper))==0), 1, 0.4 ) ))
mtext(side = 1, "Inter-individual Distance", at = 4, line = 2.5, cex = 0.8)
axis(side = 2, at = seq(50, 200, 50))
mtext(side = 2, "Avg. Coins", line = 2.5, cex = 0.8)

plot(dat_dist_group$pred[dat_dist_group$Environment==2], dat_dist_group$coins[dat_dist_group$Environment==2],col = alpha(col.pal[3], alpha = 0.4), pch = ifelse(dat_dist_group$Incentives[dat_dist_group$Environment==2]==1, 1, 16)  , bty = "n", xlim = c(-3.5,3.2), ylim = c(50,200) , yaxt = "n", xaxt = "n", xlab = "")
plot_regression_line(s_distance, dat_dist_group$pred, 2, color = col.pal[3])
axis(side = 1, at = seq(10,50,20))
axis(side = 1, at = c(-3, 0,3))

mean <- round(mean(s_distance_group$weight[,2]),2)
lower <- round(PI(s_distance_group$weight[,2], 0.9)[1],2)
upper <- round(PI(s_distance_group$weight[,2], 0.9)[2],2)
text(0, 190, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 0.9, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper)|any(c(sign(lower), sign(upper))==0), 1, 0.4 ) ))


#mtext(side = 4, "Collective Success", line = 0.5, cex = 1)

#dev.off()
