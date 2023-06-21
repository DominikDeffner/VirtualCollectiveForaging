
#Temporal analyses

#Here we aim to analyze time trends of decision weights within rounds and compare them to time trends in collected coins


library(cmdstanr)
inits <- function () list(mu_closed = c(1, 0.5),mu_distance = c(0,-1), xangle=c(5,30), yangle=c(0,0), sigma_closed = c(0.5,0.5), sigma_distance = c(0.5,0.5))


m_parallel <- cmdstan_model("HMM_CoinScroungeTimeMono.stan", cpp_options = list(stan_threads = TRUE))
fit_parallel <- m_parallel$sample(stan.data, chains = 2, parallel_chains = 2, init = inits, threads_per_chain = 40, refresh = 1, iter_warmup = 1500 ,adapt_delta = 0.8, iter_sampling = 1000)
fit <- rstan::read_stan_csv(fit_parallel$output_files())
save(fit, file = "fit_HMMTime_040423")

s <- extract.samples(fit)

precis(fit, 3)
plot(precis(fit, pars = c("b_vis_time","b_dist_time", "b_numb_time", "b_time_time"), 3))




graphics.off()
pdf("TimeTrendPredictors.pdf", height = 7.5, width = 6.5)

N_draws = 100
par(mfrow = c(4,2),
    mar = c(1,1, 0.3,0.5), 
    oma = c(3.25,3.5,2.5,0))

#Baseline switching prob
draw = 1
curve(inv_logit( s$logit_p_I_S[draw,1,1] + (s$b_vis[draw,1,1] + s$b_vis_time[draw,1,1]*x )  ),type = "n", from = -2, 2, ylim = c(0.15,0.35),bty="n", xaxt = "n", xlab = "", ylab = "", col = alpha(col.pal[2], alpha = 0.075))
#axis(side = 1,  seq(0,720, length.out=4) , at = seq(-2,2,length.out = 4))
for (draw in sample(1:length(s$lp__),N_draws)) {
  curve(inv_logit( s$logit_p_I_S[draw,1,1] + (s$b_vis[draw,1,1] + s$b_vis_time[draw,1,1]*x )  ), from = -2, 2, ylim = c(0,0.8), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[2], alpha = 0.075))
  curve(inv_logit( s$logit_p_I_S[draw,1,2] + (s$b_vis[draw,1,2] + s$b_vis_time[draw,1,2]*x )  ), from = -2, 2, ylim = c(0,0.8), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[3], alpha = 0.075))
}
curve(inv_logit( mean(s$logit_p_I_S[,1,1]) + (mean(s$b_vis[,1,1]) + mean(s$b_vis_time[,1,1])*x )  ), from = -2, 2, ylim = c(0,0.8), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[2], alpha = 0.9), lwd = 2)
curve(inv_logit( mean(s$logit_p_I_S[,1,2]) + (mean(s$b_vis[,1,2]) + mean(s$b_vis_time[,1,2])*x )  ), from = -2, 2, ylim = c(0,0.8), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[3], alpha = 0.9), lwd = 2)

mean <- round(mean(s$b_vis_time[,1,1]),2)
lower <- round(PI(s$b_vis_time[,1,1], 0.9)[1],2)
upper <- round(PI(s$b_vis_time[,1,1], 0.9)[2],2)
text(0, 0.33, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.2, col = alpha(col.pal[2], alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

mean <- round(mean(s$b_vis_time[,1,2]),2)
lower <- round(PI(s$b_vis_time[,1,2], 0.9)[1],2)
upper <- round(PI(s$b_vis_time[,1,2], 0.9)[2],2)
text(0, 0.31, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.2, col = alpha(col.pal[3], alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))



mtext(side = 3, "Group Incentives", line = 1, cex = 1.2)
mtext(side = 2, "Baseline Exploit. Visible", line = 3, cex = 0.9)

curve(inv_logit( s$logit_p_I_S[draw,2,1] + (s$b_vis[draw,2,1] + s$b_vis_time[draw,2,1]*x )  ),type = "n", from = -2, 2, ylim = c(0.15,0.35),bty = "n", xaxt = "n", xlab = "", yaxt="n", ylab = "", col = alpha(col.pal[2], alpha = 0.075))
#axis(side = 1,  seq(0,720, length.out=4) , at = seq(-2,2,length.out = 4))
for (draw in sample(1:length(s$lp__),N_draws)) {
  curve(inv_logit( s$logit_p_I_S[draw,2,1] + (s$b_vis[draw,2,1] + s$b_vis_time[draw,2,1]*x )  ), from = -2, 2, ylim = c(0,0.8), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[2], alpha = 0.075))
  curve(inv_logit( s$logit_p_I_S[draw,2,2] + (s$b_vis[draw,2,2] + s$b_vis_time[draw,2,2]*x )  ), from = -2, 2, ylim = c(0,0.8), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[3], alpha = 0.075))
}
curve(inv_logit( mean(s$logit_p_I_S[,2,1]) + (mean(s$b_vis[,2,1]) + mean(s$b_vis_time[,2,1])*x )  ), from = -2, 2, ylim = c(0,1), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[2], alpha = 0.9), lwd = 2)
curve(inv_logit( mean(s$logit_p_I_S[,2,2]) + (mean(s$b_vis[,2,2]) + mean(s$b_vis_time[,2,2])*x )  ), from = -2, 2, ylim = c(0,1), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[3], alpha = 0.9), lwd = 2)

mean <- round(mean(s$b_vis_time[,2,1]),2)
lower <- round(PI(s$b_vis_time[,2,1], 0.9)[1],2)
upper <- round(PI(s$b_vis_time[,2,1], 0.9)[2],2)
text(-0.5, 0.33, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.2, col = alpha(col.pal[2], alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

mean <- round(mean(s$b_vis_time[,2,2]),2)
lower <- round(PI(s$b_vis_time[,2,2], 0.9)[1],2)
upper <- round(PI(s$b_vis_time[,2,2], 0.9)[2],2)
text(-0.5, 0.31, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.2, col = alpha(col.pal[3], alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))


mtext(side = 3, "Individual Incentives", line = 1, cex = 1.2)
legend("topright", title = "Environment", c("Concentrated", "Distributed"),col = c(alpha(col.pal[2],alpha = 0.9),alpha(col.pal[3],alpha = 0.9)), cex = 1, lwd = 8, lty = 1, bty = "n")


#Distance

draw = 1
curve( s$b_dist[draw,1,1] + s$b_dist_time[draw,1,1]*x ,type = "n", from = -2, 2, ylim = c(-1,0.2),bty="n", xaxt = "n", xlab = "", ylab = "", col = alpha(col.pal[2], alpha = 0.075))
#axis(side = 1,  seq(0,720, length.out=4) , at = seq(-2,2,length.out = 4))
abline(h = 0, lty = 2, col = "grey")

for (draw in sample(1:length(s$lp__),N_draws)) {
  curve(s$b_dist[draw,1,1] + s$b_dist_time[draw,1,1]*x , from = -2, 2, ylim = c(-1,0.2), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[2], alpha = 0.075))
  curve(s$b_dist[draw,1,2] + s$b_dist_time[draw,1,2]*x , from = -2, 2, ylim = c(-1,0.2), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[3], alpha = 0.075))
}
curve(mean(s$b_dist[,1,1]) + mean(s$b_dist_time[,1,1])*x   , from = -2, 2, ylim = c(-1,0.2), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[2], alpha = 0.9), lwd = 2)
curve(mean(s$b_dist[,1,2]) + mean(s$b_dist_time[,1,2])*x   , from = -2, 2, ylim = c(-1,0.2), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[3], alpha = 0.9), lwd = 2)

mtext(side = 2, "Vis. Patch Dist.", line = 3, cex = 0.9)

mean <- round(mean(s$b_dist_time[,1,1]),2)
lower <- round(PI(s$b_dist_time[,1,1], 0.9)[1],2)
upper <- round(PI(s$b_dist_time[,1,1], 0.9)[2],2)
text(0, 0.175, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.2, col = alpha(col.pal[2], alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

mean <- round(mean(s$b_dist_time[,1,2]),2)
lower <- round(PI(s$b_dist_time[,1,2], 0.9)[1],2)
upper <- round(PI(s$b_dist_time[,1,2], 0.9)[2],2)
text(0, 0.075, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.2, col = alpha(col.pal[3], alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))




draw = 1
curve( s$b_dist[draw,2,1] + s$b_dist_time[draw,2,1]*x ,type = "n", from = -2, 2, ylim = c(-1,0.2),bty="n", xaxt = "n", xlab = "", ylab = "",yaxt = "n", col = alpha(col.pal[2], alpha = 0.075))
#axis(side = 1,  seq(0,720, length.out=4) , at = seq(-2,2,length.out = 4))
abline(h = 0, lty = 2, col = "grey")

for (draw in sample(1:length(s$lp__),N_draws)) {
  curve(s$b_dist[draw,2,1] + s$b_dist_time[draw,2,1]*x , from = -2, 2, ylim = c(0,1), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[2], alpha = 0.075))
  curve(s$b_dist[draw,2,2] + s$b_dist_time[draw,2,2]*x , from = -2, 2, ylim = c(0,1), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[3], alpha = 0.075))
}
curve(mean(s$b_dist[,2,1]) + mean(s$b_dist_time[,2,1])*x   , from = -2, 2, ylim = c(0,1), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[2], alpha = 0.9), lwd = 2)
curve(mean(s$b_dist[,2,2]) + mean(s$b_dist_time[,2,2])*x   , from = -2, 2, ylim = c(0,1), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[3], alpha = 0.9), lwd = 2)

mean <- round(mean(s$b_dist_time[,2,1]),2)
lower <- round(PI(s$b_dist_time[,2,1], 0.9)[1],2)
upper <- round(PI(s$b_dist_time[,2,1], 0.9)[2],2)
text(0, 0.175, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.2, col = alpha(col.pal[2], alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

mean <- round(mean(s$b_dist_time[,2,2]),2)
lower <- round(PI(s$b_dist_time[,2,2], 0.9)[1],2)
upper <- round(PI(s$b_dist_time[,2,2], 0.9)[2],2)
text(0, 0.075, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.2, col = alpha(col.pal[3], alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))




#Number

draw = 1
curve( s$b_numb[draw,1,1] + s$b_numb_time[draw,1,1]*x ,type = "n", from = -2, 2, ylim = c(-1,1),bty="n", xaxt = "n", xlab = "", ylab = "", col = alpha(col.pal[2], alpha = 0.075))
#axis(side = 1,  seq(0,720, length.out=4) , at = seq(-2,2,length.out = 4))
abline(h = 0, lty = 2, col = "grey")

for (draw in sample(1:length(s$lp__),N_draws)) {
  curve(s$b_numb[draw,1,1] + s$b_numb_time[draw,1,1]*x , from = -2, 2, ylim = c(0,1), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[2], alpha = 0.075))
  curve(s$b_numb[draw,1,2] + s$b_numb_time[draw,1,2]*x , from = -2, 2, ylim = c(0,1), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[3], alpha = 0.075))
}
curve(mean(s$b_numb[,1,1]) + mean(s$b_numb_time[,1,1])*x   , from = -2, 2, ylim = c(-1,0.2), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[2], alpha = 0.9), lwd = 2)
curve(mean(s$b_numb[,1,2]) + mean(s$b_numb_time[,1,2])*x   , from = -2, 2, ylim = c(-1,0.2), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[3], alpha = 0.9), lwd = 2)

mean <- round(mean(s$b_numb_time[,1,1]),2)
lower <- round(PI(s$b_numb_time[,1,1], 0.9)[1],2)
upper <- round(PI(s$b_numb_time[,1,1], 0.9)[2],2)
text(0, 0.95, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.2, col = alpha(col.pal[2], alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

mean <- round(mean(s$b_numb_time[,1,2]),2)
lower <- round(PI(s$b_numb_time[,1,2], 0.9)[1],2)
upper <- round(PI(s$b_numb_time[,1,2], 0.9)[2],2)
text(0, 0.75, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.2, col = alpha(col.pal[3], alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))


mtext(side = 2, "Vis. Expl. Players", line = 3, cex = 0.9)



draw = 1
curve( s$b_numb[draw,2,1] + s$b_numb_time[draw,2,1]*x ,type = "n", from = -2, 2, ylim = c(-1,1),bty="n", xaxt = "n", xlab = "", ylab = "",yaxt = "n", col = alpha(col.pal[2], alpha = 0.075))
#axis(side = 1,  seq(0,720, length.out=4) , at = seq(-2,2,length.out = 4))
abline(h = 0, lty = 2, col = "grey")

for (draw in sample(1:length(s$lp__),N_draws)) {
  curve(s$b_numb[draw,2,1] + s$b_numb_time[draw,2,1]*x , from = -2, 2, ylim = c(0,1), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[2], alpha = 0.075))
  curve(s$b_numb[draw,2,2] + s$b_numb_time[draw,2,2]*x , from = -2, 2, ylim = c(0,1), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[3], alpha = 0.075))
}
curve(mean(s$b_numb[,2,1]) + mean(s$b_numb_time[,2,1])*x   , from = -2, 2, ylim = c(-1,0.2), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[2], alpha = 0.9), lwd = 2)
curve(mean(s$b_numb[,2,2]) + mean(s$b_numb_time[,2,2])*x   , from = -2, 2, ylim = c(-1,0.2), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[3], alpha = 0.9), lwd = 2)

mean <- round(mean(s$b_numb_time[,2,1]),2)
lower <- round(PI(s$b_numb_time[,2,1], 0.9)[1],2)
upper <- round(PI(s$b_numb_time[,2,1], 0.9)[2],2)
text(0, 0.95, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.2, col = alpha(col.pal[2], alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

mean <- round(mean(s$b_numb_time[,2,2]),2)
lower <- round(PI(s$b_numb_time[,2,2], 0.9)[1],2)
upper <- round(PI(s$b_numb_time[,2,2], 0.9)[2],2)
text(0, 0.75, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.2, col = alpha(col.pal[3], alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))




#Time

draw = 1
curve( s$b_time[draw,1,1] + s$b_time_time[draw,1,1]*x ,type = "n", from = -2, 2, ylim = c(-0.4,0.4),bty="n", xaxt = "n", xlab = "", ylab = "", col = alpha(col.pal[2], alpha = 0.075))
axis(side = 1,  seq(0,720, length.out=4) , at = seq(-2,2,length.out = 4), cex.axis = 1.25)
abline(h = 0, lty = 2, col = "grey")

for (draw in sample(1:length(s$lp__),N_draws)) {
  curve(s$b_time[draw,1,1] + s$b_time_time[draw,1,1]*x , from = -2, 2, ylim = c(0,1), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[2], alpha = 0.075))
  curve(s$b_time[draw,1,2] + s$b_time_time[draw,1,2]*x , from = -2, 2, ylim = c(0,1), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[3], alpha = 0.075))
}
curve(mean(s$b_time[,1,1]) + mean(s$b_time_time[,1,1])*x   , from = -2, 2, ylim = c(-1,0.2), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[2], alpha = 0.9), lwd = 2)
curve(mean(s$b_time[,1,2]) + mean(s$b_time_time[,1,2])*x   , from = -2, 2, ylim = c(-1,0.2), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[3], alpha = 0.9), lwd = 2)

mean <- round(mean(s$b_time_time[,1,1]),2)
lower <- round(PI(s$b_time_time[,1,1], 0.9)[1],2)
upper <- round(PI(s$b_time_time[,1,1], 0.9)[2],2)
text(0, 0.35, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.2, col = alpha(col.pal[2], alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

mean <- round(mean(s$b_time_time[,1,2]),2)
lower <- round(PI(s$b_time_time[,1,2], 0.9)[1],2)
upper <- round(PI(s$b_time_time[,1,2], 0.9)[2],2)
text(0, 0.26, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.2, col = alpha(col.pal[3], alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

mtext(side = 2, "Time since Success", line = 3, cex = 0.9)



draw = 1
curve( s$b_time[draw,2,1] + s$b_time_time[draw,2,1]*x ,type = "n", from = -2, 2, ylim = c(-0.4,0.4),bty="n", xaxt = "n", xlab = "", ylab = "",yaxt = "n", col = alpha(col.pal[2], alpha = 0.075))
axis(side = 1,  seq(0,720, length.out=4) , at = seq(-2,2,length.out = 4), cex.axis = 1.25)
abline(h = 0, lty = 2, col = "grey")

for (draw in sample(1:length(s$lp__),N_draws)) {
  curve(s$b_time[draw,2,1] + s$b_time_time[draw,2,1]*x , from = -2, 2, ylim = c(0,1), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[2], alpha = 0.075))
  curve(s$b_time[draw,2,2] + s$b_time_time[draw,2,2]*x , from = -2, 2, ylim = c(0,1), xaxt = "n", xlab = "", ylab = "", add = TRUE, col = alpha(col.pal[3], alpha = 0.075))
}
curve(mean(s$b_time[,2,1]) + mean(s$b_time_time[,2,1])*x   , from = -2, 2, add = TRUE, col = alpha(col.pal[2], alpha = 0.9), lwd = 2)
curve(mean(s$b_time[,2,2]) + mean(s$b_time_time[,2,2])*x   , from = -2, 2, add = TRUE, col = alpha(col.pal[3], alpha = 0.9), lwd = 2)
mean <- round(mean(s$b_time_time[,2,1]),2)
lower <- round(PI(s$b_time_time[,2,1], 0.9)[1],2)
upper <- round(PI(s$b_time_time[,2,1], 0.9)[2],2)
text(0, 0.35, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.2, col = alpha(col.pal[2], alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

mean <- round(mean(s$b_time_time[,2,2]),2)
lower <- round(PI(s$b_time_time[,2,2], 0.9)[1],2)
upper <- round(PI(s$b_time_time[,2,2], 0.9)[2],2)
text(0, 0.26, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.2, col = alpha(col.pal[3], alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))


mtext(side = 1, "Time in round [s]", line = 2, cex = 1.2, outer = TRUE)


dev.off()















































#Load relevant data files
load("CoinScroung/dat_players")


#Remove time points where no group member was currently exploiting
dat_players <- dat_players[is.na(dat_players$IsExtracting)==FALSE,]

dat_players <- dat_players[which(dat_players$t > 30),]


stan.data <- list(N=nrow(dat_players),
                  id = dat_players$id, 
                  N_id = length(unique(dat_players$id)),
                  group = dat_players$Group,
                  N_group = length(unique(dat_players$Group)),
                  extract=ifelse(dat_players$IsExtracting=="True",1,0), 
                  TimeInRound = standardize(dat_players$t))

#Create indices for environment and incentive structure
stan.data$Environment <- as.integer(ifelse(dat_players$Env == "C", 1, 2))
stan.data$Incentives <- as.integer(ifelse(dat_players$Pay == "Coop", 1, 2))



m_coins_time <- cmdstan_model("m_time_coins.stan", cpp_options = list(stan_threads = TRUE))
fit_coins_time <- m_coins_time$sample(stan.data, chains = 2, parallel_chains = 2, threads_per_chain = 30, refresh = 1, iter_warmup = 500 ,adapt_delta = 0.8, iter_sampling = 1000)
fit <- rstan::read_stan_csv(fit_coins_time$output_files())


save(fit, file = "fit_coins_time")


plot(precis(fit, pars = "weight",3))





























####
###
##
# Monotonic Effects
##
###
####


graphics.off()
pdf("TimeTrendPredictorsMono2.pdf", height = 7, width = 6)

load("~/CoinScrounge/fit_HMMTime_040423")
s <- extract.samples(fit)
N_draws = 100
par(mfrow = c(5,2),
    mar = c(0,0.5, 0.3,0.25), 
    oma = c(4.25,3.5,2.5,0))

#Baseline switching prob
draw = 1

values_con <- matrix(NA, length(s$lp__), 12 )
for (draw in 1:length(s$lp__)) {
deltas <- c(0,s$b_vis_time[draw,1,1,])
values_con[draw,] <- sapply(1:12, function(x) inv_logit( s$logit_p_I_S[draw,1,1] + s$b_vis[draw,1,1] + (s$b_vis_max[draw,1,1] * sum(deltas[1:x]) ) ) )
}

values_dist <- matrix(NA, length(s$lp__), 12 )
for (draw in 1:length(s$lp__)) {
  deltas <- c(0,s$b_vis_time[draw,1,2,])
  values_dist[draw,] <- sapply(1:12, function(x) inv_logit( s$logit_p_I_S[draw,1,2] + s$b_vis[draw,1,2] + (s$b_vis_max[draw,1,2] * sum(deltas[1:x]) ) ) )
}

plot(values_con[1,],type = "n",ylim = c(0.17,0.32),bty="n",yaxt="n", xaxt = "n", xlab = "", ylab = "", col = alpha(col.pal[2], alpha = 0.075))
axis(side = 2, at = c(0.2,0.25,0.3))

for (draw in sample(1:length(s$lp__),N_draws)) {
  lines(values_con[draw,], col = alpha(col.pal[2], alpha = 0.1))
  lines(values_dist[draw,], col = alpha(col.pal[3], alpha = 0.1))
}

lines(apply(values_con,2,mean), col = alpha(col.pal[2], alpha = 0.9), lwd=2)
lines(apply(values_dist,2,mean), col = alpha(col.pal[3], alpha = 0.9), lwd=2)

mtext(side = 3, "Group Incentives", line = 1, cex = 1.1)
mtext(side = 2, "Baseline Exploit. Visible", line = 3, cex = 0.75)
legend("topleft", title = "Environment", c("Concentrated", "Distributed"),col = c(alpha(col.pal[2],alpha = 0.9),alpha(col.pal[3],alpha = 0.9)), cex = 1, lwd = 8, lty = 1, bty = "n")



values_con <- matrix(NA, length(s$lp__), 12 )
for (draw in 1:length(s$lp__)) {
  deltas <- c(0,s$b_vis_time[draw,2,1,])
  values_con[draw,] <- sapply(1:12, function(x) inv_logit( s$logit_p_I_S[draw,2,1] + s$b_vis[draw,2,1] + (s$b_vis_max[draw,2,1] * sum(deltas[1:x]) ) ) )
}

values_dist <- matrix(NA, length(s$lp__), 12 )
for (draw in 1:length(s$lp__)) {
  deltas <- c(0,s$b_vis_time[draw,2,2,])
  values_dist[draw,] <- sapply(1:12, function(x) inv_logit( s$logit_p_I_S[draw,2,2] + s$b_vis[draw,2,2] + (s$b_vis_max[draw,2,2] * sum(deltas[1:x]) ) ) )
}

plot(values_con[1,],type = "n",ylim = c(0.17,0.32),bty="n", yaxt="n", xaxt = "n", xlab = "", ylab = "", col = alpha(col.pal[2], alpha = 0.075))
for (draw in sample(1:length(s$lp__),N_draws)) {
  lines(values_con[draw,], col = alpha(col.pal[2], alpha = 0.1))
  lines(values_dist[draw,], col = alpha(col.pal[3], alpha = 0.1))
}

lines(apply(values_con,2,mean), col = alpha(col.pal[2], alpha = 0.9), lwd=2)
lines(apply(values_dist,2,mean), col = alpha(col.pal[3], alpha = 0.9), lwd=2)


mtext(side = 3, "Individual Incentives", line = 1, cex = 1.1)


#Distance
draw = 1

values_con <- matrix(NA, length(s$lp__), 12 )
for (draw in 1:length(s$lp__)) {
  deltas <- c(0,s$b_dist_time[draw,1,1,])
  values_con[draw,] <- sapply(1:12, function(x) s$b_dist[draw,1,1] + (s$b_dist_max[draw,1,1] * sum(deltas[1:x]) ) ) 
}

values_dist <- matrix(NA, length(s$lp__), 12 )
for (draw in 1:length(s$lp__)) {
  deltas <- c(0,s$b_vis_time[draw,1,2,])
  values_dist[draw,] <- sapply(1:12, function(x) s$b_dist[draw,1,2] + (s$b_dist_max[draw,1,2] * sum(deltas[1:x]) ) ) 
}

plot(values_con[1,],type = "n",ylim = c(-0.7,0.1),bty="n",yaxt="n", xaxt = "n", xlab = "", ylab = "", col = alpha(col.pal[2], alpha = 0.075))
axis(side = 2, at = c(-0.6,-0.3,0))

for (draw in sample(1:length(s$lp__),N_draws)) {
  lines(values_con[draw,], col = alpha(col.pal[2], alpha = 0.1))
  lines(values_dist[draw,], col = alpha(col.pal[3], alpha = 0.1))
}

lines(apply(values_con,2,mean), col = alpha(col.pal[2], alpha = 0.9), lwd=2)
lines(apply(values_dist,2,mean), col = alpha(col.pal[3], alpha = 0.9), lwd=2)

abline(h = 0, lty = 2, col = "grey")
mtext(side = 2, "Vis. Patch Dist.", line = 3, cex = 0.75)




draw = 1

values_con <- matrix(NA, length(s$lp__), 12 )
for (draw in 1:length(s$lp__)) {
  deltas <- c(0,s$b_dist_time[draw,2,1,])
  values_con[draw,] <- sapply(1:12, function(x) s$b_dist[draw,2,1] + (s$b_dist_max[draw,2,1] * sum(deltas[1:x]) ) ) 
}

values_dist <- matrix(NA, length(s$lp__), 12 )
for (draw in 1:length(s$lp__)) {
  deltas <- c(0,s$b_vis_time[draw,2,2,])
  values_dist[draw,] <- sapply(1:12, function(x) s$b_dist[draw,2,2] + (s$b_dist_max[draw,2,2] * sum(deltas[1:x]) ) ) 
}

plot(values_con[1,],type = "n",ylim = c(-0.7,0.1),bty="n",  yaxt="n",xaxt = "n", xlab = "", ylab = "", col = alpha(col.pal[2], alpha = 0.075))
for (draw in sample(1:length(s$lp__),N_draws)) {
  lines(values_con[draw,], col = alpha(col.pal[2], alpha = 0.1))
  lines(values_dist[draw,], col = alpha(col.pal[3], alpha = 0.1))
}
abline(h = 0, lty = 2, col = "grey")

lines(apply(values_con,2,mean), col = alpha(col.pal[2], alpha = 0.9), lwd=2)
lines(apply(values_dist,2,mean), col = alpha(col.pal[3], alpha = 0.9), lwd=2)







#Number

draw = 1

values_con <- matrix(NA, length(s$lp__), 12 )
for (draw in 1:length(s$lp__)) {
  deltas <- c(0,s$b_numb_time[draw,1,1,])
  values_con[draw,] <- sapply(1:12, function(x) s$b_numb[draw,1,1] + (s$b_numb_max[draw,1,1] * sum(deltas[1:x]) ) ) 
}

values_dist <- matrix(NA, length(s$lp__), 12 )
for (draw in 1:length(s$lp__)) {
  deltas <- c(0,s$b_numb_time[draw,1,2,])
  values_dist[draw,] <- sapply(1:12, function(x) s$b_numb[draw,1,2] + (s$b_numb_max[draw,1,2] * sum(deltas[1:x]) ) ) 
}

plot(values_con[1,],type = "n",ylim = c(-0.7,0.7),bty="n", yaxt = "n",xaxt = "n", xlab = "", ylab = "", col = alpha(col.pal[2], alpha = 0.075))
axis(side = 2, at = c(-0.6,0,0.6))

for (draw in sample(1:length(s$lp__),N_draws)) {
  lines(values_con[draw,], col = alpha(col.pal[2], alpha = 0.1))
  lines(values_dist[draw,], col = alpha(col.pal[3], alpha = 0.1))
}
abline(h = 0, lty = 2, col = "grey")

lines(apply(values_con,2,mean), col = alpha(col.pal[2], alpha = 0.9), lwd=2)
lines(apply(values_dist,2,mean), col = alpha(col.pal[3], alpha = 0.9), lwd=2)


mtext(side = 2, "Vis. Expl. Players", line = 3, cex = 0.75)



draw = 1

values_con <- matrix(NA, length(s$lp__), 12 )
for (draw in 1:length(s$lp__)) {
  deltas <- c(0,s$b_numb_time[draw,2,1,])
  values_con[draw,] <- sapply(1:12, function(x) s$b_numb[draw,2,1] + (s$b_numb_max[draw,2,1] * sum(deltas[1:x]) ) ) 
}

values_dist <- matrix(NA, length(s$lp__), 12 )
for (draw in 1:length(s$lp__)) {
  deltas <- c(0,s$b_numb_time[draw,2,2,])
  values_dist[draw,] <- sapply(1:12, function(x) s$b_numb[draw,2,2] + (s$b_numb_max[draw,2,2] * sum(deltas[1:x]) ) ) 
}

plot(values_con[1,],type = "n",ylim = c(-0.7,0.7),bty="n",  yaxt="n",xaxt = "n", xlab = "", ylab = "", col = alpha(col.pal[2], alpha = 0.075))
for (draw in sample(1:length(s$lp__),N_draws)) {
  lines(values_con[draw,], col = alpha(col.pal[2], alpha = 0.1))
  lines(values_dist[draw,], col = alpha(col.pal[3], alpha = 0.1))
}
abline(h = 0, lty = 2, col = "grey")

lines(apply(values_con,2,mean), col = alpha(col.pal[2], alpha = 0.9), lwd=2)
lines(apply(values_dist,2,mean), col = alpha(col.pal[3], alpha = 0.9), lwd=2)




#Time

draw = 1

values_con <- matrix(NA, length(s$lp__), 12 )
for (draw in 1:length(s$lp__)) {
  deltas <- c(0,s$b_time_time[draw,1,1,])
  values_con[draw,] <- sapply(1:12, function(x) s$b_time[draw,1,1] + (s$b_time_max[draw,1,1] * sum(deltas[1:x]) ) ) 
}

values_dist <- matrix(NA, length(s$lp__), 12 )
for (draw in 1:length(s$lp__)) {
  deltas <- c(0,s$b_time_time[draw,1,2,])
  values_dist[draw,] <- sapply(1:12, function(x) s$b_time[draw,1,2] + (s$b_time_max[draw,1,2] * sum(deltas[1:x]) ) ) 
}

plot(values_con[1,],type = "n",ylim = c(-0.7,0.7),bty="n", yaxt="n", xaxt = "n", xlab = "", ylab = "", col = alpha(col.pal[2], alpha = 0.075))
axis(side = 2, at = c(-0.6,0,0.6))

for (draw in sample(1:length(s$lp__),N_draws)) {
  lines(values_con[draw,], col = alpha(col.pal[2], alpha = 0.1))
  lines(values_dist[draw,], col = alpha(col.pal[3], alpha = 0.1))
}
abline(h = 0, lty = 2, col = "grey")

lines(apply(values_con,2,mean), col = alpha(col.pal[2], alpha = 0.9), lwd=2)
lines(apply(values_dist,2,mean), col = alpha(col.pal[3], alpha = 0.9), lwd=2)

mtext(side = 2, "Time since Success", line = 3, cex = 0.75)




draw = 1

values_con <- matrix(NA, length(s$lp__), 12 )
for (draw in 1:length(s$lp__)) {
  deltas <- c(0,s$b_time_time[draw,2,1,])
  values_con[draw,] <- sapply(1:12, function(x) s$b_time[draw,2,1] + (s$b_time_max[draw,2,1] * sum(deltas[1:x]) ) ) 
}

values_dist <- matrix(NA, length(s$lp__), 12 )
for (draw in 1:length(s$lp__)) {
  deltas <- c(0,s$b_time_time[draw,2,2,])
  values_dist[draw,] <- sapply(1:12, function(x) s$b_time[draw,2,2] + (s$b_time_max[draw,2,2] * sum(deltas[1:x]) ) ) 
}

plot(values_con[1,],type = "n",ylim = c(-0.7,0.7),bty="n", yaxt="n", xaxt = "n", xlab = "", ylab = "", col = alpha(col.pal[2], alpha = 0.075))
for (draw in sample(1:length(s$lp__),N_draws)) {
  lines(values_con[draw,], col = alpha(col.pal[2], alpha = 0.1))
  lines(values_dist[draw,], col = alpha(col.pal[3], alpha = 0.1))
}
abline(h = 0, lty = 2, col = "grey")

lines(apply(values_con,2,mean), col = alpha(col.pal[2], alpha = 0.9), lwd=2)
lines(apply(values_dist,2,mean), col = alpha(col.pal[3], alpha = 0.9), lwd=2)

mtext(side = 1, "Time in round [min]", line = 3, cex = 1.1, outer = TRUE)





#Success

load("~/CoinScrounge/fit_extract_time")
s <- extract.samples(fit)
draw = 1

values_con <- matrix(NA, length(s$lp__), 12 )
for (draw in 1:length(s$lp__)) {
  deltas <- c(0,s$b_time[draw,1,1,])
  values_con[draw,] <- sapply(1:12, function(x) inv_logit( s$alpha[draw,1,1] + (s$b_time_max[draw,1,1] * sum(deltas[1:x]) ) ) )
}

values_dist <- matrix(NA, length(s$lp__), 12 )
for (draw in 1:length(s$lp__)) {
  deltas <- c(0,s$b_time[draw,1,2,])
  values_dist[draw,] <- sapply(1:12, function(x) inv_logit( s$alpha[draw,1,2] + (s$b_time_max[draw,1,2] * sum(deltas[1:x]) ) ) )
}

plot(values_con[1,],type = "n",ylim = c(0,0.45),yaxt="n", xaxt = "n", xlab = "", ylab = "", col = alpha(col.pal[2], alpha = 0.075))
axis(side = 2, at = c(0,0.2, 0.4))

for (draw in sample(1:length(s$lp__),N_draws)) {
  lines(values_con[draw,], col = alpha(col.pal[2], alpha = 0.1))
  lines(values_dist[draw,], col = alpha(col.pal[3], alpha = 0.1))
}

lines(apply(values_con,2,mean), col = alpha(col.pal[2], alpha = 0.9), lwd=2)
lines(apply(values_dist,2,mean), col = alpha(col.pal[3], alpha = 0.9), lwd=2)
axis(side = 1,  seq(1,12,length.out = 12) , at = seq(1,12,length.out = 12), cex.axis = 0.9)

mtext(side = 2, "P(Successful)", line = 3, cex = 0.75)

draw = 1

values_con <- matrix(NA, length(s$lp__), 12 )
for (draw in 1:length(s$lp__)) {
  deltas <- c(0,s$b_time[draw,2,1,])
  values_con[draw,] <- sapply(1:12, function(x) inv_logit( s$alpha[draw,2,1] + (s$b_time_max[draw,2,1] * sum(deltas[1:x]) ) ) )
}

values_dist <- matrix(NA, length(s$lp__), 12 )
for (draw in 1:length(s$lp__)) {
  deltas <- c(0,s$b_time[draw,2,2,])
  values_dist[draw,] <- sapply(1:12, function(x) inv_logit( s$alpha[draw,2,2] + (s$b_time_max[draw,2,2] * sum(deltas[1:x]) ) ) )
}

plot(values_con[1,],type = "n",ylim = c(0,0.45),yaxt="n", xaxt = "n", xlab = "", ylab = "", col = alpha(col.pal[2], alpha = 0.075))

for (draw in sample(1:length(s$lp__),N_draws)) {
  lines(values_con[draw,], col = alpha(col.pal[2], alpha = 0.1))
  lines(values_dist[draw,], col = alpha(col.pal[3], alpha = 0.1))
}

axis(side = 1,  seq(1,12,length.out = 12) , at = seq(1,12,length.out = 12), cex.axis = 0.9)

lines(apply(values_con,2,mean), col = alpha(col.pal[2], alpha = 0.9), lwd=2)
lines(apply(values_dist,2,mean), col = alpha(col.pal[3], alpha = 0.9), lwd=2)


















dev.off()

