
###
##
# Collective Visual-Spatial Dynamics Analyses using time-lagged Gaussian-process regressions
##
###


#Load relevant data files
load("Data/dat_players")

#Exclude tracks with missing data; there are two rounds (out of 160) where we have gaps in the time series due to technical error
dat_players <- dat_players[-which(dat_players$Group %in% c(2,21) & dat_players$Round == 3),]


####
###
##
# Compute average distances
# Compute visibility index 
##
###
####

dat_group <- unique(dat_players[c("t","Group","Round")])

dat_group$vis <- c()
dat_group$dist <- c()
dat_group$n_exploit <- c()

for (i in 1:nrow(dat_group)) {
  print(i)
  t <- dat_group$t[i]
  group <- dat_group$Group[i]
  round <- dat_group$Round[i]
  indices <- which(dat_players$t == t & dat_players$Group == group & dat_players$Round == round)
  
  ids <- unique(dat_players$id[indices])
  
  #Calculate distance of each player to each other player and take the mean
  dat_group$dist[i] <-mean(sapply(ids, function(each){
    sapply(ids[-which(ids==each)], function(alter){ 
      id_each <-   which(dat_players$id == each & dat_players$Round == round & dat_players$t == t)
      id_alter <- which(dat_players$id == alter & dat_players$Round == round & dat_players$t == t)
      Euclidian_distance(x1 = dat_players$PositionX[id_each],
                         x2 = dat_players$PositionX[id_alter],
                         y1 = dat_players$PositionZ[id_each],
                         y2 = dat_players$PositionZ[id_alter])
    }#alter
    )
  }))
  
  
  #Visibility
  dat_group$vis[i] <- sum(unlist(dat_players$VisibleOthers[indices])>0)
  
  dat_group$n_exploit[i] <- length(which(dat_players$IsExtracting[indices] == "True"))
  
}

#Add Environment and Payoff Information
dat_group$Environment <- c()
dat_group$Incentives <- c()

for (group in unique(dat_group$Group)) {
  for (round in 1:4) {
    dat_group$Environment[dat_group$Group== group & dat_group$Round == round] <- as.integer(ifelse(unique(dat_players$Env[dat_players$Group== group & dat_players$Round == round])=="C", 1, 2))
    dat_group$Incentives[dat_group$Group== group & dat_group$Round == round] <- as.integer(ifelse(unique(dat_players$Pay[dat_players$Group== group & dat_players$Round == round])=="Coop", 1, 2))
    
  }
}

stan.dataLagged <- list(N = nrow(dat_group),
                        t = dat_group$t,
                        Group = dat_group$Group, 
                        Round = dat_group$Round, 
                        dist = dat_group$dist, 
                        vis = dat_group$vis, 
                        n_exploit = dat_group$n_exploit, 
                        Environment = dat_group$Environment, 
                        Incentives = dat_group$Incentives,
                        intervals = 1,
                        MaxLag = 30)

stan.dataLagged$Exploit <- ifelse(stan.dataLagged$n_exploit > 0, 1, 0)


#Run stan models

#Full 3 minutes in 5s intervals

#DISTANCE
stan.dataLagged$MaxLag <- 36
stan.dataLagged$intervals <- 5
stan.dataLagged$predictor <- standardize(stan.dataLagged$dist)
m_parallel <- cmdstan_model("Stan model code/m_time_laggedGP.stan", cpp_options = list(stan_threads = TRUE))
fit_parallel <- m_parallel$sample(stan.dataLagged, chains = 2, parallel_chains = 2, threads_per_chain = 40, refresh = 1, iter_warmup = 500 ,adapt_delta = 0.99, max_treedepth = 14, iter_sampling = 100)
fit <- rstan::read_stan_csv(fit_parallel$output_files())
s_dist3min <- extract.samples(fit)

#VISIBILITY
stan.dataLagged$predictor <- standardize(stan.dataLagged$vis)
m_parallel <- cmdstan_model("Stan model code/m_time_laggedGP.stan", cpp_options = list(stan_threads = TRUE))
fit_parallel <- m_parallel$sample(stan.dataLagged, chains = 2, parallel_chains = 2, threads_per_chain = 40, refresh = 1, iter_warmup = 500 ,adapt_delta = 0.99, max_treedepth = 14, iter_sampling = 100)
fit <- rstan::read_stan_csv(fit_parallel$output_files())
s_vis3min <- extract.samples(fit)

#30 seconds in 1s intervals

#DISTANCE
stan.dataLagged$MaxLag <- 30
stan.dataLagged$intervals <- 1
stan.dataLagged$predictor <- standardize(stan.dataLagged$dist)
m_parallel <- cmdstan_model("Stan model code/m_time_laggedGP.stan", cpp_options = list(stan_threads = TRUE))
fit_parallel <- m_parallel$sample(stan.dataLagged, chains = 2, parallel_chains = 2, threads_per_chain = 40, refresh = 1, iter_warmup = 500 ,adapt_delta = 0.99, max_treedepth = 14, iter_sampling = 100)
fit <- rstan::read_stan_csv(fit_parallel$output_files())
s_dist30sec <- extract.samples(fit)

#VISIBILITY
stan.dataLagged$predictor <- standardize(stan.dataLagged$vis)
m_parallel <- cmdstan_model("Stan model code/m_time_laggedGP.stan", cpp_options = list(stan_threads = TRUE))
fit_parallel <- m_parallel$sample(stan.dataLagged, chains = 2, parallel_chains = 2, threads_per_chain = 40, refresh = 1, iter_warmup = 500 ,adapt_delta = 0.99, max_treedepth = 14, iter_sampling = 100)
fit <- rstan::read_stan_csv(fit_parallel$output_files())
s_vis30sec <- extract.samples(fit)



#graphics.off()
#pdf("LaggedSuccess.pdf", height = 6, width = 7)

par(mfrow = c(2,2), mar = c(1,1,1,0), oma = c(3.25,5,1,0))

stan.dataLagged$MaxLag <- 36

values_coop_con <- matrix(NA, length(s_dist3min$lp__), stan.dataLagged$MaxLag )
for (draw in 1:length(s_dist3min$lp__)) {
  values_coop_con[draw,] <- rev(s_dist3min$time_effects[draw,1,1,] )
}

values_coop_dist <- matrix(NA, length(s_dist3min$lp__),  stan.dataLagged$MaxLag )
for (draw in 1:length(s_dist3min$lp__)) {
  values_coop_dist[draw,] <- rev(s_dist3min$time_effects[draw,1,2,] ) 
}

values_comp_con <- matrix(NA, length(s_dist3min$lp__), stan.dataLagged$MaxLag )
for (draw in 1:length(s_dist3min$lp__)) {
  values_comp_con[draw,] <- rev(s_dist3min$time_effects[draw,2,1,] )
}

values_comp_dist <- matrix(NA, length(s_dist3min$lp__),  stan.dataLagged$MaxLag )
for (draw in 1:length(s_dist3min$lp__)) {
  values_comp_dist[draw,] <- rev(s_dist3min$time_effects[draw,2,2,] ) 
}


#Group Incentives
plot(values_coop_con[1,], type = "n", ylab = "", xlab = "", bty = "n", ylim = c(-0.4, 0.4), xaxt = "n")
mtext(side = 3, "Group Incentives", line = 0.5, cex = 1.2)

abline(h = 0, lty = 2, col = "grey")
# summarize the distribution of mu
mu.mean <- apply( values_coop_con , 2 , mean )
mu.PI <-  apply( values_coop_con , 2 , PI , prob=0.9 )

# plot a shaded region for 89% PI
shade( mu.PI , 1:stan.dataLagged$MaxLag , col =  alpha(col.pal[2], alpha = 0.4))

# plot the MAP line, aka the mean mu for each weight
lines( 1:stan.dataLagged$MaxLag , mu.mean, col = col.pal[2] )

# summarize the distribution of mu
mu.mean <- apply( values_coop_dist , 2 , mean )
mu.PI <-  apply( values_coop_dist , 2 , PI , prob=0.9 )

# plot a shaded region for 89% PI
shade( mu.PI , 1:stan.dataLagged$MaxLag , col =  alpha(col.pal[3], alpha = 0.4))

# plot the MAP line, aka the mean mu for each weight
lines( 1:stan.dataLagged$MaxLag , mu.mean, col = col.pal[3] )

mtext(side = 2, "Distance -> Collective Success", line = 5, font = 3, cex = 1 )
legend("topleft", title = "Environment", c("Concentrated", "Distributed"),col = c(alpha(col.pal[2],alpha = 0.9),alpha(col.pal[3],alpha = 0.9)), cex = 1, lwd = 8, lty = 1, bty = "n")


#Individual Incentives
plot(values_comp_con[1,], type = "n", ylab = "", xlab = "",bty = "n", ylim = c(-0.4, 0.4), xaxt = "n", yaxt = "n")
abline(h = 0, lty = 2, col = "grey")
mtext(side = 3, "Individual Incentives", line = 0.5, cex = 1.2)

# summarize the distribution of mu
mu.mean <- apply( values_comp_con , 2 , mean )
mu.PI <-  apply( values_comp_con , 2 , PI , prob=0.9 )

# plot a shaded region for 89% PI
shade( mu.PI , 1:stan.dataLagged$MaxLag , col =  alpha(col.pal[2], alpha = 0.4))

# plot the MAP line, aka the mean mu for each weight
lines( 1:stan.dataLagged$MaxLag , mu.mean, col = col.pal[2] )


# summarize the distribution of mu
mu.mean <- apply( values_comp_dist , 2 , mean )
mu.PI <-  apply( values_comp_dist , 2 , PI , prob=0.9 )

# plot a shaded region for 89% PI
shade( mu.PI , 1:stan.dataLagged$MaxLag , col =  alpha(col.pal[3], alpha = 0.4))

# plot the MAP line, aka the mean mu for each weight
lines( 1:stan.dataLagged$MaxLag , mu.mean, col = col.pal[3] )


stan.dataLagged$MaxLag <- 36

values_coop_con <- matrix(NA, length(s_vis3min$lp__), stan.dataLagged$MaxLag )
for (draw in 1:length(s_vis3min$lp__)) {
  values_coop_con[draw,] <- rev(s_vis3min$time_effects[draw,1,1,] )
}

values_coop_dist <- matrix(NA, length(s_vis3min$lp__),  stan.dataLagged$MaxLag )
for (draw in 1:length(s_vis3min$lp__)) {
  values_coop_dist[draw,] <- rev(s_vis3min$time_effects[draw,1,2,] ) 
}

values_comp_con <- matrix(NA, length(s_vis3min$lp__), stan.dataLagged$MaxLag )
for (draw in 1:length(s_vis3min$lp__)) {
  values_comp_con[draw,] <- rev(s_vis3min$time_effects[draw,2,1,] )
}

values_comp_dist <- matrix(NA, length(s_vis3min$lp__),  stan.dataLagged$MaxLag )
for (draw in 1:length(s_vis3min$lp__)) {
  values_comp_dist[draw,] <- rev(s_vis3min$time_effects[draw,2,2,] ) 
}


#Group Incentives
plot(values_coop_con[1,], type = "n", ylab = "", xlab = "", bty = "n", ylim = c(-0.7, 0.2), xaxt = "n")

axis(1, at = seq(0,36, 6), labels = seq(-180,0, 30))
abline(h = 0, lty = 2, col = "grey")
# summarize the distribution of mu
mu.mean <- apply( values_coop_con , 2 , mean )
mu.PI <-  apply( values_coop_con , 2 , PI , prob=0.9 )

# plot a shaded region for 89% PI
shade( mu.PI , 1:stan.dataLagged$MaxLag , col =  alpha(col.pal[2], alpha = 0.4))

# plot the MAP line, aka the mean mu for each weight
lines( 1:stan.dataLagged$MaxLag , mu.mean, col = col.pal[2] )


# summarize the distribution of mu
mu.mean <- apply( values_coop_dist , 2 , mean )
mu.PI <-  apply( values_coop_dist , 2 , PI , prob=0.9 )

# plot a shaded region for 89% PI
shade( mu.PI , 1:stan.dataLagged$MaxLag , col =  alpha(col.pal[3], alpha = 0.4))

# plot the MAP line, aka the mean mu for each weight
lines( 1:stan.dataLagged$MaxLag , mu.mean, col = col.pal[3] )

mtext(side = 2, "Visibility -> Collective Success", line = 5, font = 3, cex = 1 )

#Individual Incentives
plot(values_comp_con[1,], type = "n", ylab = "", xlab = "",bty = "n", ylim = c(-0.7, 0.2), xaxt = "n", yaxt = "n")
axis(1, at = seq(0,36, 6), labels = seq(-180,0, 30))
abline(h = 0, lty = 2, col = "grey")

# summarize the distribution of mu
mu.mean <- apply( values_comp_con , 2 , mean )
mu.PI <-  apply( values_comp_con , 2 , PI , prob=0.9 )

# plot a shaded region for 89% PI
shade( mu.PI , 1:stan.dataLagged$MaxLag , col =  alpha(col.pal[2], alpha = 0.4))

# plot the MAP line, aka the mean mu for each weight
lines( 1:stan.dataLagged$MaxLag , mu.mean, col = col.pal[2] )


# summarize the distribution of mu
mu.mean <- apply( values_comp_dist , 2 , mean )
mu.PI <-  apply( values_comp_dist , 2 , PI , prob=0.9 )

# plot a shaded region for 89% PI
shade( mu.PI , 1:stan.dataLagged$MaxLag , col =  alpha(col.pal[3], alpha = 0.4))

# plot the MAP line, aka the mean mu for each weight
lines( 1:stan.dataLagged$MaxLag , mu.mean, col = col.pal[3] )

mtext(side = 1, "Time lag [s]", line = 2, outer = TRUE)

mtext(side = 2, "Time-lagged regression weights", line = 1.75, outer = TRUE, cex = 0.9)

#Inlets
stan.dataLagged$MaxLag <- 30

values_coop_con <- matrix(NA, length(s_dist30sec$lp__), stan.dataLagged$MaxLag )
for (draw in 1:length(s_dist30sec$lp__)) {
  values_coop_con[draw,] <- rev(s_dist30sec$time_effects[draw,1,1,] )
}

values_coop_dist <- matrix(NA, length(s_dist30sec$lp__),  stan.dataLagged$MaxLag )
for (draw in 1:length(s_dist30sec$lp__)) {
  values_coop_dist[draw,] <- rev(s_dist30sec$time_effects[draw,1,2,] ) 
}

values_comp_con <- matrix(NA, length(s_dist30sec$lp__), stan.dataLagged$MaxLag )
for (draw in 1:length(s_dist30sec$lp__)) {
  values_comp_con[draw,] <- rev(s_dist30sec$time_effects[draw,2,1,] )
}

values_comp_dist <- matrix(NA, length(s_dist30sec$lp__),  stan.dataLagged$MaxLag )
for (draw in 1:length(s_dist30sec$lp__)) {
  values_comp_dist[draw,] <- rev(s_dist30sec$time_effects[draw,2,2,] ) 
}


par(fig = c(0.02,0.22, 0.55, 0.75), new = T)  

#Group Incentives
plot(values_coop_con[1,], type = "n", ylab = "", xlab = "",  ylim = c(-0.4, 0.4),yaxt = "n", xaxt = "n", cex.axis =0.6)
axis(1, at = seq(0,30, 10), labels = seq(-30,0, 10), cex.axis = 0.6)
abline(h = 0, lty = 2, col = "grey")
# summarize the distribution of mu
mu.mean <- apply( values_coop_con , 2 , mean )
mu.PI <-  apply( values_coop_con , 2 , PI , prob=0.9 )

# plot a shaded region for 89% PI
shade( mu.PI , 1:stan.dataLagged$MaxLag , col =  alpha(col.pal[2], alpha = 0.4))

# plot the MAP line, aka the mean mu for each weight
lines( 1:stan.dataLagged$MaxLag , mu.mean, col = col.pal[2] )


# summarize the distribution of mu
mu.mean <- apply( values_coop_dist , 2 , mean )
mu.PI <-  apply( values_coop_dist , 2 , PI , prob=0.9 )

# plot a shaded region for 89% PI
shade( mu.PI , 1:stan.dataLagged$MaxLag , col =  alpha(col.pal[3], alpha = 0.4))

# plot the MAP line, aka the mean mu for each weight
lines( 1:stan.dataLagged$MaxLag , mu.mean, col = col.pal[3] )


par(fig = c(0.52,0.72, 0.55, 0.75), new = T)  

#Individual Incentives
plot(values_comp_con[1,], type = "n", ylab = "", xlab = "",  ylim = c(-0.4, 0.4),yaxt = "n", xaxt = "n", cex.axis = 0.6)
axis(1, at = seq(0,30, 10), labels = seq(-30,0, 10), cex.axis = 0.6)
abline(h = 0, lty = 2, col = "grey")

# summarize the distribution of mu
mu.mean <- apply( values_comp_con , 2 , mean )
mu.PI <-  apply( values_comp_con , 2 , PI , prob=0.9 )

# plot a shaded region for 89% PI
shade( mu.PI , 1:stan.dataLagged$MaxLag , col =  alpha(col.pal[2], alpha = 0.4))

# plot the MAP line, aka the mean mu for each weight
lines( 1:stan.dataLagged$MaxLag , mu.mean, col = col.pal[2] )


# summarize the distribution of mu
mu.mean <- apply( values_comp_dist , 2 , mean )
mu.PI <-  apply( values_comp_dist , 2 , PI , prob=0.9 )

# plot a shaded region for 89% PI
shade( mu.PI , 1:stan.dataLagged$MaxLag , col =  alpha(col.pal[3], alpha = 0.4))

# plot the MAP line, aka the mean mu for each weight
lines( 1:stan.dataLagged$MaxLag , mu.mean, col = col.pal[3] )


#Inlets
stan.dataLagged$MaxLag <- 30

values_coop_con <- matrix(NA, length(s_vis30sec$lp__), stan.dataLagged$MaxLag )
for (draw in 1:length(s_vis30sec$lp__)) {
  values_coop_con[draw,] <- rev(s_vis30sec$time_effects[draw,1,1,] )
}

values_coop_dist <- matrix(NA, length(s_vis30sec$lp__),  stan.dataLagged$MaxLag )
for (draw in 1:length(s_vis30sec$lp__)) {
  values_coop_dist[draw,] <- rev(s_vis30sec$time_effects[draw,1,2,] ) 
}

values_comp_con <- matrix(NA, length(s_vis30sec$lp__), stan.dataLagged$MaxLag )
for (draw in 1:length(s_vis30sec$lp__)) {
  values_comp_con[draw,] <- rev(s_vis30sec$time_effects[draw,2,1,] )
}

values_comp_dist <- matrix(NA, length(s_vis30sec$lp__),  stan.dataLagged$MaxLag )
for (draw in 1:length(s_vis30sec$lp__)) {
  values_comp_dist[draw,] <- rev(s_vis30sec$time_effects[draw,2,2,] ) 
}


par(fig = c(0.02,0.22, 0.1, 0.3), new = T)  

#Group Incentives
plot(values_coop_con[1,], type = "n", ylab = "", xlab = "",  ylim = c(-0.8, 0.2),yaxt = "n", xaxt = "n", cex.axis =0.6)
axis(1, at = seq(0,30, 10), labels = seq(-30,0, 10), cex.axis = 0.6)
abline(h = 0, lty = 2, col = "grey")
# summarize the distribution of mu
mu.mean <- apply( values_coop_con , 2 , mean )
mu.PI <-  apply( values_coop_con , 2 , PI , prob=0.9 )

# plot a shaded region for 89% PI
shade( mu.PI , 1:stan.dataLagged$MaxLag , col =  alpha(col.pal[2], alpha = 0.4))

# plot the MAP line, aka the mean mu for each weight
lines( 1:stan.dataLagged$MaxLag , mu.mean, col = col.pal[2] )


# summarize the distribution of mu
mu.mean <- apply( values_coop_dist , 2 , mean )
mu.PI <-  apply( values_coop_dist , 2 , PI , prob=0.9 )

# plot a shaded region for 89% PI
shade( mu.PI , 1:stan.dataLagged$MaxLag , col =  alpha(col.pal[3], alpha = 0.4))

# plot the MAP line, aka the mean mu for each weight
lines( 1:stan.dataLagged$MaxLag , mu.mean, col = col.pal[3] )

par(fig = c(0.52,0.72, 0.1, 0.3), new = T)  

#Individual Incentives
plot(values_comp_con[1,], type = "n", ylab = "", xlab = "",  ylim = c(-0.8, 0.2),yaxt = "n", xaxt = "n", cex.axis = 0.6)
axis(1, at = seq(0,30, 10), labels = seq(-30,0, 10), cex.axis = 0.6)
abline(h = 0, lty = 2, col = "grey")

# summarize the distribution of mu
mu.mean <- apply( values_comp_con , 2 , mean )
mu.PI <-  apply( values_comp_con , 2 , PI , prob=0.9 )

# plot a shaded region for 89% PI
shade( mu.PI , 1:stan.dataLagged$MaxLag , col =  alpha(col.pal[2], alpha = 0.4))

# plot the MAP line, aka the mean mu for each weight
lines( 1:stan.dataLagged$MaxLag , mu.mean, col = col.pal[2] )


# summarize the distribution of mu
mu.mean <- apply( values_comp_dist , 2 , mean )
mu.PI <-  apply( values_comp_dist , 2 , PI , prob=0.9 )

# plot a shaded region for 89% PI
shade( mu.PI , 1:stan.dataLagged$MaxLag , col =  alpha(col.pal[3], alpha = 0.4))

# plot the MAP line, aka the mean mu for each weight
lines( 1:stan.dataLagged$MaxLag , mu.mean, col = col.pal[3] )


#dev.off()

