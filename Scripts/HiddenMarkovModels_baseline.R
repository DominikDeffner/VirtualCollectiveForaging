
#######
#####
###
# Baseline SOCIAL HIDDEN MARKOV DECISION MODEL
###
#####
#######


####
###
##
# Fit baseline model
##
###
####

load("Data/stan.data")


#Specify initial values for state-dependent variables
inits <- function () list(mu_closed = c(1, 0.5),mu_distance = c(0,-1), xangle=c(5,30), yangle=c(0,0), sigma_closed = c(0.5,0.5), sigma_distance = c(0.5,0.5))

m_parallel <- cmdstan_model("Stan model code/m_SHMDM.stan", cpp_options = list(stan_threads = TRUE))
fit_parallel <- m_parallel$sample(stan.data, chains = 2, parallel_chains = 2, threads_per_chain = 50, refresh = 10, init = inits, iter_warmup = 1500 ,adapt_delta = 0.8, iter_sampling = 1000)
fit <- rstan::read_stan_csv(fit_parallel$output_files())
s <- extract.samples(fit)


###
##
# Adaptive consequences of individual-level state predictors
##
###

#Calculate individual weights
baseline_weight <- data.frame(id = 1:160, Incentives = NA,  Concentrated = NA, Distributed = NA)
stay_weight <- data.frame(id = 1:160, Incentives = NA,  Concentrated = NA, Distributed = NA)
dist_weight <- data.frame(id = 1:160, Incentives = NA,  Concentrated = NA, Distributed = NA)
time_weight <- data.frame(id = 1:160, Incentives = NA,  Concentrated = NA, Distributed = NA)
numb_weight <- data.frame(id = 1:160, Incentives = NA,  Concentrated = NA, Distributed = NA)

#Loop over all individuals
for (id in 1:160) {
  print(id)
  incentives <- unique(stan.data$Incentives[stan.data$id==id])
  group <- unique(stan.data$group[stan.data$id==id])
  baseline_weight$Incentives[id] <- incentives
  time_weight$Incentives[id] <- incentives
  dist_weight$Incentives[id] <- incentives
  numb_weight$Incentives[id] <- incentives
  
  baseline_weight$Concentrated[id] <- mean(inv_logit((s$logit_p_I_S[,incentives,1] + s$offset_ID[,id,     ifelse(incentives==1, 1, 3) ] + s$offset_Group[,group,ifelse(incentives==1, 1, 3) ] ) +
                                                       (s$b_vis[,incentives,1] + s$offset_ID[,id, 4 + ifelse(incentives==1, 1, 3) ] + s$offset_Group[,group,4 +ifelse(incentives==1, 1, 3) ] )   ))
  
  baseline_weight$Distributed[id]  <- mean(inv_logit((s$logit_p_I_S[,incentives,2] + s$offset_ID[,id, ifelse(incentives==1, 2, 4) ]+ s$offset_Group[,group,ifelse(incentives==1, 2, 4) ] ) +
                                                       (s$b_vis[,incentives,2] + s$offset_ID[,id, 4 + ifelse(incentives==1, 2, 4) ] + s$offset_Group[,group,4 +ifelse(incentives==1, 2, 4) ])   ))
  
  stay_weight$Concentrated[id] <- mean(inv_logit(s$logit_p_S_S[,incentives,1] + s$offset_ID[,id, 20 + ifelse(incentives==1, 1, 3) ]+ s$offset_Group[,group,20 +ifelse(incentives==1, 1, 3) ] ) )
  stay_weight$Distributed[id]  <- mean(inv_logit(s$logit_p_S_S[,incentives,2] + s$offset_ID[,id, 20 + ifelse(incentives==1, 2, 4) ]+ s$offset_Group[,group,20 +ifelse(incentives==1, 2, 4) ]  ) )
  
  time_weight$Concentrated[id] <- mean(s$b_time[,incentives,1] + s$offset_ID[,id, 8 + ifelse(incentives==1, 1, 3) ] + s$offset_Group[,group,8 +ifelse(incentives==1, 1, 3) ]) 
  time_weight$Distributed[id]  <- mean(s$b_time[,incentives,2] + s$offset_ID[,id, 8 + ifelse(incentives==1, 2, 4) ] + s$offset_Group[,group,8 +ifelse(incentives==1, 2, 4) ]) 
  
  dist_weight$Concentrated[id] <- mean(s$b_dist[,incentives,1] + s$offset_ID[,id, 12 + ifelse(incentives==1, 1, 3) ] + s$offset_Group[,group,12 +ifelse(incentives==1, 1, 3) ] ) 
  dist_weight$Distributed[id]  <- mean(s$b_dist[,incentives,2] + s$offset_ID[,id, 12 + ifelse(incentives==1, 2, 4) ] + s$offset_Group[,group,12 +ifelse(incentives==1, 2, 4) ] ) 
  
  numb_weight$Concentrated[id] <- mean(s$b_numb[,incentives,1] + s$offset_ID[,id, 16 + ifelse(incentives==1, 1, 3) ]  + s$offset_Group[,group,16 +ifelse(incentives==1, 1, 3) ]) 
  numb_weight$Distributed[id]  <- mean(s$b_numb[,incentives,2] + s$offset_ID[,id, 16 + ifelse(incentives==1, 2, 4) ] + s$offset_Group[,group, 16 + ifelse(incentives==1, 2, 4) ] ) 
}


load("Data/d_rounds")
load("Data/d_group")


#Assign stan models to predict success based on latent decision weights
coins_predict <- "

data{

  int N; 
  int N_id;
  int N_group;
  real coins[N];
  real pred[N];
  int Environment[N];
  int Incentives[N];
  int group[N];
  int id[N];   

}

parameters{

  //Intercepts for environments
  vector[2] alpha; 
  
  //Offsets for incentives (coded as -1 and 1, so alpha represents mean across conditions)
  vector[2] beta_Incentive; 

  //Regression weights of predictor
  vector[2] weight; 
  real<lower = 0>sigma; 
 
  matrix[2, N_group] z_Group;     
  vector<lower = 0>[2] sigma_Group;
  cholesky_factor_corr[2] Rho_Group;

} 

transformed parameters {
 matrix[N_group, 2] offset_Group;
 
//Varying effects offsets
offset_Group = (diag_pre_multiply(sigma_Group, Rho_Group) * z_Group)';

} 

model{

 //Priors
 alpha ~ normal(5, 0.5);
 beta_Incentive ~ normal(0,1);
 weight ~ normal(0, 1); 
 sigma ~ exponential(3); 

  //Define prior distribution of varying group effects
  to_vector(z_Group) ~ normal(0, 1);
  sigma_Group ~ exponential(3);
  Rho_Group ~ lkj_corr_cholesky(4);
  
//Likelihoods  
for(i in 1:N){                                                        
      coins[i]  ~ lognormal( (alpha[Environment[i]]  + offset_Group[group[i],1]) +
                            beta_Incentive[Environment[i]]*Incentives[i]+
                            (weight[Environment[i]]  + offset_Group[group[i],2])*pred[i],  sigma  ); 
  }                                                                      
} 

"

coins_predict_group <- "

data{

  int N; 
  int N_group;
  real coins[N];
  real pred[N];
  int Environment[N];
  int Incentives[N];
  int group[N];
}

parameters{

  //Intercepts for environments
  vector[2] alpha; 
  
  //Offsets for incentives (coded as -1 and 1, so alpha represents mean across conditions)
  vector[2] beta_Incentive; 

  //Regression weights of predictor
  vector[2] weight; 
  real<lower = 0>sigma; 

} 

model{

 //Priors
 alpha ~ normal(5, 0.5);
 beta_Incentive ~ normal(0,1);
 weight ~ normal(0, 1); 
 sigma ~ exponential(3); 

//Likelihoods  
for(i in 1:N){                                                        
      coins[i]  ~ lognormal( alpha[Environment[i]] + beta_Incentive[Environment[i]]*Incentives[i]+
                            weight[Environment[i]] * pred[i],  sigma  ); 
  }                                                                      
} 

"

###
##
# Baseline
##
###

#Prepare data list
dat_baseline <- list(N = 320, 
                     N_id = length(unique(d$id))  , 
                     N_group = length(unique(d$Group))  , 
                     Environment =  c(rep(1, length(baseline_weight$Concentrated)),rep(2, length(baseline_weight$Distributed))), 
                     Incentives = rep(baseline_weight$Incentives, 2),
                     group = ceiling(rep(baseline_weight$id,2) /4),
                     id = rep(baseline_weight$id,2), 
                     pred = c(baseline_weight$Concentrated, baseline_weight$Distributed))


dat_baseline$coins <- 0
for (i in 1:dat_baseline$N) {
  dat_baseline$coins[i] <- mean(d$Coins[d$Env == ifelse(dat_baseline$Incentives[i]==1, "C", "D") & d$id == dat_baseline$id[i]] )
  
}

dat_baseline$Incentives <- ifelse(dat_baseline$Incentives==1, 1,-1)

m_baseline <- stan(model_code = coins_predict, data = dat_baseline, iter = 4000, cores = 1, chains = 1, refresh = 10)
s_baseline <- extract.samples(m_baseline)

###
##
# Distance
##
###

#Prepare data list
dat_dist <- list(N = 320, 
                 N_id = length(unique(d$id))  , 
                 N_group = length(unique(d$Group))  , 
                 Environment =  c(rep(1, length(dist_weight$Concentrated)),rep(2, length(dist_weight$Distributed))), 
                 Incentives = rep(dist_weight$Incentives, 2),
                 group = ceiling(rep(dist_weight$id,2) /4),
                 id = rep(dist_weight$id,2), 
                 pred = c(dist_weight$Concentrated, dist_weight$Distributed))

dat_dist$coins <- 0
for (i in 1:dat_dist$N) {
  dat_dist$coins[i] <- mean(d$Coins[d$Env == ifelse(dat_dist$Incentives[i]==1, "C", "D") & d$id == dat_dist$id[i]] )
  
}

dat_dist$Incentives <- ifelse(dat_dist$Incentives==1, 1,-1)
m_dist <- stan(model_code = coins_predict, data = dat_dist, iter = 4000, cores = 1, chains = 1, refresh = 10)
s_dist <- extract.samples(m_dist)


###
##
# Number
##
###


#Prepare data list
dat_numb <- list(N = 320, 
                 N_id = length(unique(d$id))  , 
                 N_group = length(unique(d$Group))  , 
                 Environment =  c(rep(1, length(numb_weight$Concentrated)),rep(2, length(numb_weight$Distributed))), 
                 Incentives = rep(numb_weight$Incentives, 2),
                 group = ceiling(rep(dist_weight$id,2) /4),
                 id = rep(numb_weight$id,2), 
                 pred = c(numb_weight$Concentrated, numb_weight$Distributed))

dat_numb$coins <- 0
for (i in 1:dat_numb$N) {
  dat_numb$coins[i] <- mean(d$Coins[d$Env == ifelse(dat_numb$Incentives[i]==1, "C", "D") & d$id == dat_numb$id[i]] )
  
}

dat_numb$Incentives <- ifelse(dat_numb$Incentives==1, 1,-1)

m_numb <- stan(model_code = coins_predict, data = dat_numb, iter = 4000, cores = 1, chains = 1, refresh = 10)
s_numb <- extract.samples(m_numb)


###
##
# Time
##
###


#Prepare data list
dat_time <- list(N = 320, 
                 N_id = length(unique(d$id))  , 
                 N_group = length(unique(d$Group))  , 
                 Environment =  c(rep(1, length(time_weight$Concentrated)),rep(2, length(time_weight$Distributed))), 
                 Incentives = rep(time_weight$Incentives, 2),
                 group = ceiling(rep(dist_weight$id,2) /4),
                 id = rep(time_weight$id,2), 
                 pred = c(time_weight$Concentrated, time_weight$Distributed))

dat_time$coins <- 0
for (i in 1:dat_time$N) {
  dat_time$coins[i] <- mean(d$Coins[d$Env == ifelse(dat_time$Incentives[i]==1, "C", "D") & d$id == dat_time$id[i]] )
  
}
dat_time$Incentives <- ifelse(dat_time$Incentives==1, 1,-1)

m_time <- stan(model_code = coins_predict, data = dat_time, iter = 4000, cores = 1, chains = 1, refresh = 10)

s_time <- extract.samples(m_time)

####
###
##
# Group-level analyses (including code for plot S9)
##
###
####

baseline_group <- data.frame(id = 1:40, Incentives = NA,  Concentrated = NA, Distributed = NA)
dist_group <- data.frame(id = 1:40, Incentives = NA,  Concentrated = NA, Distributed = NA)
time_group <- data.frame(id = 1:40, Incentives = NA,  Concentrated = NA, Distributed = NA)
numb_group <- data.frame(id = 1:40, Incentives = NA,  Concentrated = NA, Distributed = NA)


for (group in 1:40) {
  
  incentives <- unique(stan.data$Incentives[stan.data$group==group])
  
  baseline_group$Incentives[group] <- incentives
  time_group$Incentives[group] <- incentives
  dist_group$Incentives[group] <- incentives
  numb_group$Incentives[group] <- incentives
  
  ids <- unique(stan.data$id[stan.data$group==group])
  
  baseline_group$Concentrated[group] <- mean(baseline_weight$Concentrated[baseline_weight$id %in% ids])
  baseline_group$Distributed[group]  <- mean(baseline_weight$Distributed[baseline_weight$id %in% ids])
  
  dist_group$Concentrated[group] <- mean(dist_weight$Concentrated[dist_weight$id %in% ids])
  dist_group$Distributed[group]  <- mean(dist_weight$Distributed[dist_weight$id %in% ids])
  
  time_group$Concentrated[group] <- mean(time_weight$Concentrated[time_weight$id %in% ids])
  time_group$Distributed[group]  <- mean(time_weight$Distributed[time_weight$id %in% ids])
  
  numb_group$Concentrated[group] <- mean(numb_weight$Concentrated[numb_weight$id %in% ids])
  numb_group$Distributed[group]  <- mean(numb_weight$Distributed[numb_weight$id %in% ids])
  
}


###
##
# Baseline
##
###

#Prepare data list
dat_baseline_group <-list(N = 80, 
                          group = rep(1:40,2),
                          N_group = length(unique(d_group$id))  , 
                          Environment =  c(rep(1, length(baseline_group$Concentrated)),rep(2, length(baseline_group$Distributed))), 
                          Incentives = rep(baseline_group$Incentives, 2),
                          pred = c(baseline_group$Concentrated, baseline_group$Distributed))


dat_baseline_group$coins <- 0
for (i in 1:dat_baseline_group$N) {
  dat_baseline_group$coins[i] <- mean(d_group$Coins[d_group$Env == ifelse(dat_baseline_group$Incentives[i]==1, "C", "D") & d_group$id == dat_baseline_group$group[i]] )
  
}

dat_baseline_group$Incentives <- ifelse(dat_baseline_group$Incentives==1, 1,-1)

m_baseline_group <- stan(model_code = coins_predict_group, data = dat_baseline_group, iter = 4000, cores = 1, chains = 1, refresh = 10)
s_baseline_group <- extract.samples(m_baseline_group)

###
##
# Distance
##
###


#Prepare data list
dat_dist_group <-list(N = 80, 
                      group = rep(1:40,2),
                      N_group = length(unique(d_group$id))  , 
                      Environment =  c(rep(1, length(baseline_group$Concentrated)),rep(2, length(baseline_group$Distributed))), 
                      Incentives = rep(baseline_group$Incentives, 2),
                      pred = c(dist_group$Concentrated, dist_group$Distributed))


dat_dist_group$coins <- 0
for (i in 1:dat_dist_group$N) {
  dat_dist_group$coins[i] <- mean(d_group$Coins[d_group$Env == ifelse(dat_dist_group$Incentives[i]==1, "C", "D") & d_group$id == dat_dist_group$group[i]] )
  
}

dat_dist_group$Incentives <- ifelse(dat_dist_group$Incentives==1, 1,-1)

m_dist_group <- stan(model_code = coins_predict_group, data = dat_dist_group, iter = 4000, cores = 1, chains = 1, refresh = 10)
s_dist_group <- extract.samples(m_dist_group)


###
##
# Number
##
###


#Prepare data list
dat_numb_group <-list(N = 80, 
                      group = rep(1:40,2),
                      N_group = length(unique(d_group$id))  , 
                      Environment =  c(rep(1, length(baseline_group$Concentrated)),rep(2, length(baseline_group$Distributed))), 
                      Incentives = rep(baseline_group$Incentives, 2),
                      pred = c(numb_group$Concentrated, numb_group$Distributed))


dat_numb_group$coins <- 0
for (i in 1:dat_numb_group$N) {
  dat_numb_group$coins[i] <- mean(d_group$Coins[d_group$Env == ifelse(dat_numb_group$Incentives[i]==1, "C", "D") & d_group$id == dat_numb_group$group[i]] )
  
}

dat_numb_group$Incentives <- ifelse(dat_numb_group$Incentives==1, 1,-1)

m_numb_group <- stan(model_code = coins_predict_group, data = dat_numb_group, iter = 4000, cores = 1, chains = 1, refresh = 10)
s_numb_group <- extract.samples(m_numb_group)

###
##
# Time
##
###

#Prepare data list
dat_time_group <-list(N = 80, 
                      group = rep(1:40,2),
                      N_group = length(unique(d_group$id))  , 
                      Environment =  c(rep(1, length(baseline_group$Concentrated)),rep(2, length(baseline_group$Distributed))), 
                      Incentives = rep(baseline_group$Incentives, 2),
                      pred = c(time_group$Concentrated, time_group$Distributed))


dat_time_group$coins <- 0
for (i in 1:dat_time_group$N) {
  dat_time_group$coins[i] <- mean(d_group$Coins[d_group$Env == ifelse(dat_time_group$Incentives[i]==1, "C", "D") & d_group$id == dat_time_group$group[i]] )
  
}

dat_time_group$Incentives <- ifelse(dat_time_group$Incentives==1, 1,-1)

m_time_group <- stan(model_code = coins_predict_group, data = dat_time_group, iter = 4000, cores = 1, chains = 1, refresh = 10)
s_time_group <- extract.samples(m_time_group)



####
###
##
# Script for Fig. 3: Computational modelling results: state predictors. 
##
###
####

#graphics.off()
#pdf("Predictors.pdf", height = 7.5, width = 6)

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

#dev.off()


####
###
##
# Script for Fig. S9: Adaptive relevance of state predictors on the group level 
##
###
####

#graphics.off()
#pdf("PredictorsCoinsGroup.pdf", height = 6, width = 8)

par(mfrow = c(2,4), mar = c(0.5,0,0,1), oma = c(4.25,6,3,0))

#Concentrated
plot(dat_baseline_group$pred[dat_baseline_group$Environment==1], dat_baseline_group$coins[dat_baseline_group$Environment==1],  col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_baseline_group$Incentives[dat_baseline_group$Environment==1]==1, 1, 16), bty = "n", xlim = c(0.1,0.42), ylim = c(50,200) , xaxt = "n")
plot_regression_line(s_baseline_group, dat_baseline_group$pred, 1, color = col.pal[2])
mtext("Avg. Switching Prob. I->S", side = 3, line = 1.5)
mtext("Concentrated", side = 2, line = 3)

mean <- round(mean(s_baseline_group$weight[,1]),2)
lower <- round(PI(s_baseline_group$weight[,1], 0.9)[1],2)
upper <- round(PI(s_baseline_group$weight[,1], 0.9)[2],2)
text(0.25, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))


plot(dat_dist_group$pred[dat_dist_group$Environment==1], dat_dist_group$coins[dat_dist_group$Environment==1],  col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_baseline_group$Incentives[dat_baseline_group$Environment==1]==1, 1, 16)  , bty = "n", xlim = c(-0.65,0.2), ylim = c(50,200), yaxt = "n", xaxt = "n")
plot_regression_line(s_dist_group, dat_dist_group$pred, 1, color = col.pal[2])
mtext("Vis. Patch Dist.", side = 3, line = 1.5)
segments(0,50,0,200, lty = 2, col = "grey")

mean <- round(mean(s_dist_group$weight[,1]),2)
lower <- round(PI(s_dist_group$weight[,1], 0.9)[1],2)
upper <- round(PI(s_dist_group$weight[,1], 0.9)[2],2)
text(-0.3, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

plot(dat_numb_group$pred[dat_numb_group$Environment==1], dat_numb_group$coins[dat_numb_group$Environment==1],  col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_baseline_group$Incentives[dat_baseline_group$Environment==1]==1, 1, 16)  , bty = "n", xlim = c(-0.3,0.1), ylim = c(50,200), yaxt = "n", xaxt = "n")
plot_regression_line(s_numb_group, dat_numb_group$pred, 1, color = col.pal[2])
mtext("Vis. Expl. Players", side = 3, line = 1.5)
segments(0,50,0,200, lty = 2, col = "grey")

mean <- round(mean(s_numb_group$weight[,1]),2)
lower <- round(PI(s_numb_group$weight[,1], 0.9)[1],2)
upper <- round(PI(s_numb_group$weight[,1], 0.9)[2],2)
text(-0.1, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

plot(dat_time_group$pred[dat_time_group$Environment==1], dat_time_group$coins[dat_time_group$Environment==1],col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_baseline_group$Incentives[dat_baseline_group$Environment==1]==1, 1, 16)  , bty = "n", xlim = c(-0.1,0.2), ylim = c(50,200) , yaxt = "n", xaxt = "n")
plot_regression_line(s_time_group, dat_time_group$pred, 1, color = col.pal[2])
mtext("Time since Success", side = 3, line = 1.5)
legend("topright",title = "Incentives", c("Group", "Individual"), col = alpha("black", alpha = 0.6), pch = c(1,16), cex = 1.1, lwd = 1, lty = 1, bty = "n")
segments(0,50,0,200, lty = 2, col = "grey")

mean <- round(mean(s_time_group$weight[,1]),2)
lower <- round(PI(s_time_group$weight[,1], 0.9)[1],2)
upper <- round(PI(s_time_group$weight[,1], 0.9)[2],2)
text(-0.02, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))


#Distributed
plot(dat_baseline_group$pred[dat_baseline_group$Environment==2], dat_baseline_group$coins[dat_baseline_group$Environment==2], col = alpha(col.pal[3], alpha = 0.4), pch = ifelse(dat_baseline_group$Incentives[dat_baseline_group$Environment==2]==1, 1, 16)  , bty = "n", xlim = c(0.1,0.42), ylim = c(50,200) , xaxt = "n")
plot_regression_line(s_baseline_group, dat_baseline_group$pred, 2, color = col.pal[3])
mtext("Distributed", side = 2, line = 3)
axis(side = 1, at = seq(0.1,0.4,0.15), labels = c("0.1","0.25","0.4"))

mean <- round(mean(s_baseline_group$weight[,2]),2)
lower <- round(PI(s_baseline_group$weight[,2], 0.9)[1],2)
upper <- round(PI(s_baseline_group$weight[,2], 0.9)[2],2)
text(0.25, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

plot(dat_dist_group$pred[dat_dist_group$Environment==2], dat_dist_group$coins[dat_dist_group$Environment==2],  col = alpha(col.pal[3], alpha = 0.4), pch = ifelse(dat_baseline_group$Incentives[dat_baseline_group$Environment==2]==1, 1, 16)  , bty = "n", xlim = c(-0.65,0.2), ylim = c(50,200) , yaxt = "n", xaxt = "n")
plot_regression_line(s_dist_group, dat_dist_group$pred, 2, color = col.pal[3])
axis(side = 1, at = seq(-0.6,0.2,0.4))
segments(0,50,0,200, lty = 2, col = "grey")

mean <- round(mean(s_dist_group$weight[,2]),2)
lower <- round(PI(s_dist_group$weight[,2], 0.9)[1],2)
upper <- round(PI(s_dist_group$weight[,2], 0.9)[2],2)
text(-0.3, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

plot(dat_numb_group$pred[dat_numb_group$Environment==2], dat_numb_group$coins[dat_numb_group$Environment==2],col = alpha(col.pal[3], alpha = 0.4), pch = ifelse(dat_baseline_group$Incentives[dat_baseline_group$Environment==2]==1, 1, 16)  , bty = "n", xlim = c(-0.35,0.1), ylim = c(50,200) , yaxt = "n", xaxt = "n")
plot_regression_line(s_numb_group, dat_numb_group$pred, 2, color = col.pal[3])
axis(side = 1, at = seq(-0.3,0.1,0.2))
segments(0,50,0,200, lty = 2, col = "grey")

mean <- round(mean(s_numb_group$weight[,2]),2)
lower <- round(PI(s_numb_group$weight[,2], 0.9)[1],2)
upper <- round(PI(s_numb_group$weight[,2], 0.9)[2],2)
text(-0.1, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

plot(dat_time_group$pred[dat_time_group$Environment==2], dat_time_group$coins[dat_time_group$Environment==2], col = alpha(col.pal[3], alpha = 0.4), pch = ifelse(dat_baseline_group$Incentives[dat_baseline_group$Environment==2]==1, 1, 16)  , bty = "n", xlim = c(-0.1,0.2), ylim = c(50,200) , yaxt = "n", xaxt = "n")
plot_regression_line(s_time_group, dat_time_group$pred, 2, color = col.pal[3])
axis(side = 1, at = seq(-0.1,0.2,0.15), labels = c("-0.1","0.05","0.2"))
segments(0,50,0,200, lty = 2, col = "grey")

mean <- round(mean(s_time_group$weight[,2]),2)
lower <- round(PI(s_time_group$weight[,2], 0.9)[1],2)
upper <- round(PI(s_time_group$weight[,2], 0.9)[2],2)
text(0.05, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

mtext("Coins", side = 2, outer = TRUE, line = 4.5, cex = 1.3)
mtext("Average Decision Weights", side = 1, outer = TRUE, line = 3, cex = 1)

#dev.off()

