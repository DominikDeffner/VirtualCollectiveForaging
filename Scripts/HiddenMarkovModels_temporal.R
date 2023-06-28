
#######
#####
###
# Temporal SOCIAL HIDDEN MARKOV DECISION MODEL
###
#####
#######

load("Data/stan.data")

m_parallel <- cmdstan_model("Stan model code/m_SHMDM_temporal.stan", cpp_options = list(stan_threads = TRUE))
fit_parallel <- m_parallel$sample(stan.data, chains = 2, parallel_chains = 2, threads_per_chain = 50, refresh = 10, init = inits, iter_warmup = 1500 ,adapt_delta = 0.8, iter_sampling = 1000)
fit_temporal <- rstan::read_stan_csv(fit_parallel$output_files())
s_temporal <- extract.samples(fit_temporal)


#Fit model for success over time
#Load relevant data files

load("Data/dat_players")

#Exclude tracks with missing data; there are two rounds (out of 160) where we have gaps in the time series due to technical error
dat_players <- dat_players[-which(dat_players$Group %in% c(2,21) & dat_players$Round == 3),]

stan.data.extract <- list(N=nrow(dat_players),
                          id = dat_players$id, 
                          N_id = length(unique(dat_players$id)),
                          group = dat_players$Group,
                          N_group = length(unique(dat_players$Group)),
                          extract=ifelse(dat_players$IsExtracting=="True",1,0), 
                          TimeInRound = ceiling(dat_players$t/60))

#Create indices for environment and incentive structure
stan.data.extract$Environment <- as.integer(ifelse(dat_players$Env == "C", 1, 2))
stan.data.extract$Incentives <- as.integer(ifelse(dat_players$Pay == "Coop", 1, 2))


m_coins_time <- cmdstan_model("Stan model code/m_temporal_success.stan", cpp_options = list(stan_threads = TRUE))
fit_extract_time <- m_coins_time$sample(stan.data.extract, chains = 2, parallel_chains = 2, threads_per_chain = 30, refresh = 1, iter_warmup = 1500 ,adapt_delta = 0.95, iter_sampling = 1000)
fit_extract <- rstan::read_stan_csv(fit_extract_time$output_files())
s_extract <- extract.samples(fit_extract)




####
###
##
# Monotonic Effects
##
###
####


#graphics.off()
#pdf("TimeTrendPredictorsMono.pdf", height = 7, width = 6)

N_draws = 100
par(mfrow = c(5,2),
    mar = c(0,0.5, 0.3,0.25), 
    oma = c(4.25,3.5,2.5,0))

#Baseline switching prob
draw = 1

values_con <- matrix(NA, length(s_temporal$lp__), 12 )
for (draw in 1:length(s_temporal$lp__)) {
  deltas <- c(0,s_temporal$b_vis_time[draw,1,1,])
  values_con[draw,] <- sapply(1:12, function(x) inv_logit( s_temporal$logit_p_I_S[draw,1,1] + s_temporal$b_vis[draw,1,1] + (s_temporal$b_vis_max[draw,1,1] * sum(deltas[1:x]) ) ) )
}

values_dist <- matrix(NA, length(s_temporal$lp__), 12 )
for (draw in 1:length(s_temporal$lp__)) {
  deltas <- c(0,s_temporal$b_vis_time[draw,1,2,])
  values_dist[draw,] <- sapply(1:12, function(x) inv_logit( s_temporal$logit_p_I_S[draw,1,2] + s_temporal$b_vis[draw,1,2] + (s_temporal$b_vis_max[draw,1,2] * sum(deltas[1:x]) ) ) )
}

plot(values_con[1,],type = "n",ylim = c(0.17,0.32),bty="n",yaxt="n", xaxt = "n", xlab = "", ylab = "", col = alpha(col.pal[2], alpha = 0.075))
axis(side = 2, at = c(0.2,0.25,0.3))

for (draw in sample(1:length(s_temporal$lp__),N_draws)) {
  lines(values_con[draw,], col = alpha(col.pal[2], alpha = 0.1))
  lines(values_dist[draw,], col = alpha(col.pal[3], alpha = 0.1))
}

lines(apply(values_con,2,mean), col = alpha(col.pal[2], alpha = 0.9), lwd=2)
lines(apply(values_dist,2,mean), col = alpha(col.pal[3], alpha = 0.9), lwd=2)

mtext(side = 3, "Group Incentives", line = 1, cex = 1.1)
mtext(side = 2, "Baseline Exploit. Visible", line = 3, cex = 0.75)
legend("topleft", title = "Environment", c("Concentrated", "Distributed"),col = c(alpha(col.pal[2],alpha = 0.9),alpha(col.pal[3],alpha = 0.9)), cex = 1, lwd = 8, lty = 1, bty = "n")



values_con <- matrix(NA, length(s_temporal$lp__), 12 )
for (draw in 1:length(s_temporal$lp__)) {
  deltas <- c(0,s_temporal$b_vis_time[draw,2,1,])
  values_con[draw,] <- sapply(1:12, function(x) inv_logit( s_temporal$logit_p_I_S[draw,2,1] + s_temporal$b_vis[draw,2,1] + (s_temporal$b_vis_max[draw,2,1] * sum(deltas[1:x]) ) ) )
}

values_dist <- matrix(NA, length(s_temporal$lp__), 12 )
for (draw in 1:length(s_temporal$lp__)) {
  deltas <- c(0,s_temporal$b_vis_time[draw,2,2,])
  values_dist[draw,] <- sapply(1:12, function(x) inv_logit( s_temporal$logit_p_I_S[draw,2,2] + s_temporal$b_vis[draw,2,2] + (s_temporal$b_vis_max[draw,2,2] * sum(deltas[1:x]) ) ) )
}

plot(values_con[1,],type = "n",ylim = c(0.17,0.32),bty="n", yaxt="n", xaxt = "n", xlab = "", ylab = "", col = alpha(col.pal[2], alpha = 0.075))
for (draw in sample(1:length(s_temporal$lp__),N_draws)) {
  lines(values_con[draw,], col = alpha(col.pal[2], alpha = 0.1))
  lines(values_dist[draw,], col = alpha(col.pal[3], alpha = 0.1))
}

lines(apply(values_con,2,mean), col = alpha(col.pal[2], alpha = 0.9), lwd=2)
lines(apply(values_dist,2,mean), col = alpha(col.pal[3], alpha = 0.9), lwd=2)


mtext(side = 3, "Individual Incentives", line = 1, cex = 1.1)


#Distance
draw = 1

values_con <- matrix(NA, length(s_temporal$lp__), 12 )
for (draw in 1:length(s_temporal$lp__)) {
  deltas <- c(0,s_temporal$b_dist_time[draw,1,1,])
  values_con[draw,] <- sapply(1:12, function(x) s_temporal$b_dist[draw,1,1] + (s_temporal$b_dist_max[draw,1,1] * sum(deltas[1:x]) ) ) 
}

values_dist <- matrix(NA, length(s_temporal$lp__), 12 )
for (draw in 1:length(s_temporal$lp__)) {
  deltas <- c(0,s_temporal$b_vis_time[draw,1,2,])
  values_dist[draw,] <- sapply(1:12, function(x) s_temporal$b_dist[draw,1,2] + (s_temporal$b_dist_max[draw,1,2] * sum(deltas[1:x]) ) ) 
}

plot(values_con[1,],type = "n",ylim = c(-0.7,0.1),bty="n",yaxt="n", xaxt = "n", xlab = "", ylab = "", col = alpha(col.pal[2], alpha = 0.075))
axis(side = 2, at = c(-0.6,-0.3,0))

for (draw in sample(1:length(s_temporal$lp__),N_draws)) {
  lines(values_con[draw,], col = alpha(col.pal[2], alpha = 0.1))
  lines(values_dist[draw,], col = alpha(col.pal[3], alpha = 0.1))
}

lines(apply(values_con,2,mean), col = alpha(col.pal[2], alpha = 0.9), lwd=2)
lines(apply(values_dist,2,mean), col = alpha(col.pal[3], alpha = 0.9), lwd=2)

abline(h = 0, lty = 2, col = "grey")
mtext(side = 2, "Vis. Patch Dist.", line = 3, cex = 0.75)




draw = 1

values_con <- matrix(NA, length(s_temporal$lp__), 12 )
for (draw in 1:length(s_temporal$lp__)) {
  deltas <- c(0,s_temporal$b_dist_time[draw,2,1,])
  values_con[draw,] <- sapply(1:12, function(x) s_temporal$b_dist[draw,2,1] + (s_temporal$b_dist_max[draw,2,1] * sum(deltas[1:x]) ) ) 
}

values_dist <- matrix(NA, length(s_temporal$lp__), 12 )
for (draw in 1:length(s_temporal$lp__)) {
  deltas <- c(0,s_temporal$b_vis_time[draw,2,2,])
  values_dist[draw,] <- sapply(1:12, function(x) s_temporal$b_dist[draw,2,2] + (s_temporal$b_dist_max[draw,2,2] * sum(deltas[1:x]) ) ) 
}

plot(values_con[1,],type = "n",ylim = c(-0.7,0.1),bty="n",  yaxt="n",xaxt = "n", xlab = "", ylab = "", col = alpha(col.pal[2], alpha = 0.075))
for (draw in sample(1:length(s_temporal$lp__),N_draws)) {
  lines(values_con[draw,], col = alpha(col.pal[2], alpha = 0.1))
  lines(values_dist[draw,], col = alpha(col.pal[3], alpha = 0.1))
}
abline(h = 0, lty = 2, col = "grey")

lines(apply(values_con,2,mean), col = alpha(col.pal[2], alpha = 0.9), lwd=2)
lines(apply(values_dist,2,mean), col = alpha(col.pal[3], alpha = 0.9), lwd=2)


#Number

draw = 1

values_con <- matrix(NA, length(s_temporal$lp__), 12 )
for (draw in 1:length(s_temporal$lp__)) {
  deltas <- c(0,s_temporal$b_numb_time[draw,1,1,])
  values_con[draw,] <- sapply(1:12, function(x) s_temporal$b_numb[draw,1,1] + (s_temporal$b_numb_max[draw,1,1] * sum(deltas[1:x]) ) ) 
}

values_dist <- matrix(NA, length(s_temporal$lp__), 12 )
for (draw in 1:length(s_temporal$lp__)) {
  deltas <- c(0,s_temporal$b_numb_time[draw,1,2,])
  values_dist[draw,] <- sapply(1:12, function(x) s_temporal$b_numb[draw,1,2] + (s_temporal$b_numb_max[draw,1,2] * sum(deltas[1:x]) ) ) 
}

plot(values_con[1,],type = "n",ylim = c(-0.7,0.7),bty="n", yaxt = "n",xaxt = "n", xlab = "", ylab = "", col = alpha(col.pal[2], alpha = 0.075))
axis(side = 2, at = c(-0.6,0,0.6))

for (draw in sample(1:length(s_temporal$lp__),N_draws)) {
  lines(values_con[draw,], col = alpha(col.pal[2], alpha = 0.1))
  lines(values_dist[draw,], col = alpha(col.pal[3], alpha = 0.1))
}
abline(h = 0, lty = 2, col = "grey")

lines(apply(values_con,2,mean), col = alpha(col.pal[2], alpha = 0.9), lwd=2)
lines(apply(values_dist,2,mean), col = alpha(col.pal[3], alpha = 0.9), lwd=2)


mtext(side = 2, "Vis. Expl. Players", line = 3, cex = 0.75)



draw = 1

values_con <- matrix(NA, length(s_temporal$lp__), 12 )
for (draw in 1:length(s_temporal$lp__)) {
  deltas <- c(0,s_temporal$b_numb_time[draw,2,1,])
  values_con[draw,] <- sapply(1:12, function(x) s_temporal$b_numb[draw,2,1] + (s_temporal$b_numb_max[draw,2,1] * sum(deltas[1:x]) ) ) 
}

values_dist <- matrix(NA, length(s_temporal$lp__), 12 )
for (draw in 1:length(s_temporal$lp__)) {
  deltas <- c(0,s_temporal$b_numb_time[draw,2,2,])
  values_dist[draw,] <- sapply(1:12, function(x) s_temporal$b_numb[draw,2,2] + (s_temporal$b_numb_max[draw,2,2] * sum(deltas[1:x]) ) ) 
}

plot(values_con[1,],type = "n",ylim = c(-0.7,0.7),bty="n",  yaxt="n",xaxt = "n", xlab = "", ylab = "", col = alpha(col.pal[2], alpha = 0.075))
for (draw in sample(1:length(s_temporal$lp__),N_draws)) {
  lines(values_con[draw,], col = alpha(col.pal[2], alpha = 0.1))
  lines(values_dist[draw,], col = alpha(col.pal[3], alpha = 0.1))
}
abline(h = 0, lty = 2, col = "grey")

lines(apply(values_con,2,mean), col = alpha(col.pal[2], alpha = 0.9), lwd=2)
lines(apply(values_dist,2,mean), col = alpha(col.pal[3], alpha = 0.9), lwd=2)


#Time

draw = 1

values_con <- matrix(NA, length(s_temporal$lp__), 12 )
for (draw in 1:length(s_temporal$lp__)) {
  deltas <- c(0,s_temporal$b_time_time[draw,1,1,])
  values_con[draw,] <- sapply(1:12, function(x) s_temporal$b_time[draw,1,1] + (s_temporal$b_time_max[draw,1,1] * sum(deltas[1:x]) ) ) 
}

values_dist <- matrix(NA, length(s_temporal$lp__), 12 )
for (draw in 1:length(s_temporal$lp__)) {
  deltas <- c(0,s_temporal$b_time_time[draw,1,2,])
  values_dist[draw,] <- sapply(1:12, function(x) s_temporal$b_time[draw,1,2] + (s_temporal$b_time_max[draw,1,2] * sum(deltas[1:x]) ) ) 
}

plot(values_con[1,],type = "n",ylim = c(-0.7,0.7),bty="n", yaxt="n", xaxt = "n", xlab = "", ylab = "", col = alpha(col.pal[2], alpha = 0.075))
axis(side = 2, at = c(-0.6,0,0.6))

for (draw in sample(1:length(s_temporal$lp__),N_draws)) {
  lines(values_con[draw,], col = alpha(col.pal[2], alpha = 0.1))
  lines(values_dist[draw,], col = alpha(col.pal[3], alpha = 0.1))
}
abline(h = 0, lty = 2, col = "grey")

lines(apply(values_con,2,mean), col = alpha(col.pal[2], alpha = 0.9), lwd=2)
lines(apply(values_dist,2,mean), col = alpha(col.pal[3], alpha = 0.9), lwd=2)

mtext(side = 2, "Time since Success", line = 3, cex = 0.75)

draw = 1

values_con <- matrix(NA, length(s_temporal$lp__), 12 )
for (draw in 1:length(s_temporal$lp__)) {
  deltas <- c(0,s_temporal$b_time_time[draw,2,1,])
  values_con[draw,] <- sapply(1:12, function(x) s_temporal$b_time[draw,2,1] + (s_temporal$b_time_max[draw,2,1] * sum(deltas[1:x]) ) ) 
}

values_dist <- matrix(NA, length(s_temporal$lp__), 12 )
for (draw in 1:length(s_temporal$lp__)) {
  deltas <- c(0,s_temporal$b_time_time[draw,2,2,])
  values_dist[draw,] <- sapply(1:12, function(x) s_temporal$b_time[draw,2,2] + (s_temporal$b_time_max[draw,2,2] * sum(deltas[1:x]) ) ) 
}

plot(values_con[1,],type = "n",ylim = c(-0.7,0.7),bty="n", yaxt="n", xaxt = "n", xlab = "", ylab = "", col = alpha(col.pal[2], alpha = 0.075))
for (draw in sample(1:length(s_temporal$lp__),N_draws)) {
  lines(values_con[draw,], col = alpha(col.pal[2], alpha = 0.1))
  lines(values_dist[draw,], col = alpha(col.pal[3], alpha = 0.1))
}
abline(h = 0, lty = 2, col = "grey")

lines(apply(values_con,2,mean), col = alpha(col.pal[2], alpha = 0.9), lwd=2)
lines(apply(values_dist,2,mean), col = alpha(col.pal[3], alpha = 0.9), lwd=2)

mtext(side = 1, "Time in round [min]", line = 3, cex = 1.1, outer = TRUE)


#Success
draw = 1

values_con <- matrix(NA, length(s_extract$lp__), 12 )
for (draw in 1:length(s_extract$lp__)) {
  deltas <- c(0,s_extract$b_time[draw,1,1,])
  values_con[draw,] <- sapply(1:12, function(x) inv_logit( s_extract$alpha[draw,1,1] + (s_extract$b_time_max[draw,1,1] * sum(deltas[1:x]) ) ) )
}

values_dist <- matrix(NA, length(s_extract$lp__), 12 )
for (draw in 1:length(s_extract$lp__)) {
  deltas <- c(0,s_extract$b_time[draw,1,2,])
  values_dist[draw,] <- sapply(1:12, function(x) inv_logit( s_extract$alpha[draw,1,2] + (s_extract$b_time_max[draw,1,2] * sum(deltas[1:x]) ) ) )
}

plot(values_con[1,],type = "n",ylim = c(0,0.45),yaxt="n", xaxt = "n", xlab = "", ylab = "", col = alpha(col.pal[2], alpha = 0.075))
axis(side = 2, at = c(0,0.2, 0.4))

for (draw in sample(1:length(s_extract$lp__),N_draws)) {
  lines(values_con[draw,], col = alpha(col.pal[2], alpha = 0.1))
  lines(values_dist[draw,], col = alpha(col.pal[3], alpha = 0.1))
}

lines(apply(values_con,2,mean), col = alpha(col.pal[2], alpha = 0.9), lwd=2)
lines(apply(values_dist,2,mean), col = alpha(col.pal[3], alpha = 0.9), lwd=2)
axis(side = 1,  seq(1,12,length.out = 12) , at = seq(1,12,length.out = 12), cex.axis = 0.9)

mtext(side = 2, "P(Successful)", line = 3, cex = 0.75)

draw = 1

values_con <- matrix(NA, length(s_extract$lp__), 12 )
for (draw in 1:length(s_extract$lp__)) {
  deltas <- c(0,s_extract$b_time[draw,2,1,])
  values_con[draw,] <- sapply(1:12, function(x) inv_logit( s_extract$alpha[draw,2,1] + (s_extract$b_time_max[draw,2,1] * sum(deltas[1:x]) ) ) )
}

values_dist <- matrix(NA, length(s_extract$lp__), 12 )
for (draw in 1:length(s_extract$lp__)) {
  deltas <- c(0,s_extract$b_time[draw,2,2,])
  values_dist[draw,] <- sapply(1:12, function(x) inv_logit( s_extract$alpha[draw,2,2] + (s_extract$b_time_max[draw,2,2] * sum(deltas[1:x]) ) ) )
}

plot(values_con[1,],type = "n",ylim = c(0,0.45),yaxt="n", xaxt = "n", xlab = "", ylab = "", col = alpha(col.pal[2], alpha = 0.075))

for (draw in sample(1:length(s_extract$lp__),N_draws)) {
  lines(values_con[draw,], col = alpha(col.pal[2], alpha = 0.1))
  lines(values_dist[draw,], col = alpha(col.pal[3], alpha = 0.1))
}

axis(side = 1,  seq(1,12,length.out = 12) , at = seq(1,12,length.out = 12), cex.axis = 0.9)

lines(apply(values_con,2,mean), col = alpha(col.pal[2], alpha = 0.9), lwd=2)
lines(apply(values_dist,2,mean), col = alpha(col.pal[3], alpha = 0.9), lwd=2)




#dev.off()

