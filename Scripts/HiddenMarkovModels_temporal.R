
#######
#####
###
# Temporal SOCIAL HIDDEN MARKOV DECISION and SUCCESS models
###
#####
#######




###
##
# Temporal SOCIAL HIDDEN MARKOV DECISION model
##
###

#Load data
load("Data/stan.data")

m_parallel <- cmdstan_model("Stan model code/m_SHMDM_temporal.stan", cpp_options = list(stan_threads = TRUE))
fit_parallel <- m_parallel$sample(stan.data, chains = 2, parallel_chains = 2, threads_per_chain = 50, refresh = 10, init = inits, iter_warmup = 1500 ,adapt_delta = 0.8, iter_sampling = 1000)
fit_temporal <- rstan::read_stan_csv(fit_parallel$output_files())
s_temporal <- extract.samples(fit_temporal)

##
# Script for Fig. 4: Temporal dynamics in state predictors. 
##


#graphics.off()
#pdf("TimeTrendPredictorsMono.pdf", height = 3.5, width = 9)

N_draws = 100
par(mfrow = c(2,4),
    mar = c(0,1.5, 0,0), 
    oma = c(3.5,2.75,1.75,0))

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
axis(side = 2, at = c(0.2,0.25,0.3),cex.axis = 0.8)

for (draw in sample(1:length(s_temporal$lp__),N_draws)) {
  lines(values_con[draw,], col = alpha(col.pal[2], alpha = 0.1))
  lines(values_dist[draw,], col = alpha(col.pal[3], alpha = 0.1))
}

lines(apply(values_con,2,mean), col = alpha(col.pal[2], alpha = 0.9), lwd=2)
lines(apply(values_dist,2,mean), col = alpha(col.pal[3], alpha = 0.9), lwd=2)

mtext(side = 2, "Group Incentives", line = 3, cex = 0.9)
mtext(side = 3, "Avg. P(I->S)", line = 0.5, cex = 0.9)
legend("topleft", title = "Environment", c("Concentrated", "Distributed"),col = c(alpha(col.pal[2],alpha = 0.9),alpha(col.pal[3],alpha = 0.9)), cex = 1, lwd = 8, lty = 1, bty = "n")


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
axis(side = 2, at = c(-0.6,-0.3,0),cex.axis = 0.8)

for (draw in sample(1:length(s_temporal$lp__),N_draws)) {
  lines(values_con[draw,], col = alpha(col.pal[2], alpha = 0.1))
  lines(values_dist[draw,], col = alpha(col.pal[3], alpha = 0.1))
}

lines(apply(values_con,2,mean), col = alpha(col.pal[2], alpha = 0.9), lwd=2)
lines(apply(values_dist,2,mean), col = alpha(col.pal[3], alpha = 0.9), lwd=2)

abline(h = 0, lty = 2, col = "grey")
mtext(side = 3, "Vis. Patch Dist.", line = 0.5, cex = 0.9)


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
axis(side = 2, at = c(-0.6,0,0.6), cex.axis = 0.8)

for (draw in sample(1:length(s_temporal$lp__),N_draws)) {
  lines(values_con[draw,], col = alpha(col.pal[2], alpha = 0.1))
  lines(values_dist[draw,], col = alpha(col.pal[3], alpha = 0.1))
}
abline(h = 0, lty = 2, col = "grey")

lines(apply(values_con,2,mean), col = alpha(col.pal[2], alpha = 0.9), lwd=2)
lines(apply(values_dist,2,mean), col = alpha(col.pal[3], alpha = 0.9), lwd=2)


mtext(side = 3, "Vis. Expl. Players", line = 0.5, cex = 0.9)


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
axis(side = 2, at = c(-0.6,0,0.6), cex.axis = 0.8)

for (draw in sample(1:length(s_temporal$lp__),N_draws)) {
  lines(values_con[draw,], col = alpha(col.pal[2], alpha = 0.1))
  lines(values_dist[draw,], col = alpha(col.pal[3], alpha = 0.1))
}
abline(h = 0, lty = 2, col = "grey")

lines(apply(values_con,2,mean), col = alpha(col.pal[2], alpha = 0.9), lwd=2)
lines(apply(values_dist,2,mean), col = alpha(col.pal[3], alpha = 0.9), lwd=2)

mtext(side = 3, "Time since Success", line = 0.5, cex = 0.9)



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
axis(side = 1,  seq(1,12,1) , at = seq(1,12,1), cex.axis = 0.8)


mtext(side = 2, "Individual Incentives", line = 3, cex = 0.9)
axis(side = 2, at = c(0.2,0.25,0.3),cex.axis = 0.8)





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
axis(side = 1,  seq(1,12,1) , at = seq(1,12,1), cex.axis = 0.8)
axis(side = 2, at = c(-0.6,-0.3,0),cex.axis = 0.8)


#Number

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

plot(values_con[1,],type = "n",ylim = c(-0.7,0.7),bty="n", yaxt = "n",xaxt = "n", xlab = "", ylab = "", col = alpha(col.pal[2], alpha = 0.075))

for (draw in sample(1:length(s_temporal$lp__),N_draws)) {
  lines(values_con[draw,], col = alpha(col.pal[2], alpha = 0.1))
  lines(values_dist[draw,], col = alpha(col.pal[3], alpha = 0.1))
}
abline(h = 0, lty = 2, col = "grey")

lines(apply(values_con,2,mean), col = alpha(col.pal[2], alpha = 0.9), lwd=2)
lines(apply(values_dist,2,mean), col = alpha(col.pal[3], alpha = 0.9), lwd=2)

axis(side = 1,  seq(1,12,1) , at = seq(1,12,1), cex.axis = 0.8)

axis(side = 2, at = c(-0.6,0,0.6), cex.axis = 0.8)



#Time

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

mtext(side = 1, "Time in round [min]", line = 2.5, cex = 0.9, outer = TRUE)

axis(side = 1,  seq(1,12,1) , at = seq(1,12,1), cex.axis = 0.8)
axis(side = 2, at = c(-0.6,0,0.6), cex.axis = 0.8)


#dev.off()


