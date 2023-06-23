
#######
#####
###
# FIT MODELS IN STAN
###
#####
#######


#Specify initial values for state-dependent variables
inits <- function () list(mu_closed = c(1, 0.5),mu_distance = c(0,-1), xangle=c(5,30), yangle=c(0,0), sigma_closed = c(0.5,0.5), sigma_distance = c(0.5,0.5))

#Fit baseline model
m_parallel <- cmdstan_model("m_SHMDM.stan", cpp_options = list(stan_threads = TRUE))
fit_parallel <- m_parallel$sample(stan.data, chains = 2, parallel_chains = 2, threads_per_chain = 50, refresh = 10, init = inits, iter_warmup = 1500 ,adapt_delta = 0.8, iter_sampling = 1000)
fit <- rstan::read_stan_csv(fit_parallel$output_files())
save(fit, file = "fit_HMM")
s <- extract.samples(fit)

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

#save(baseline_weight, file = "mean_baseline_weight")
#save(dist_weight, file = "mean_dist_weight")
#save(numb_weight, file = "mean_numb_weight")
#save(time_weight, file = "mean_time_weight")
#save(stay_weight, file = "mean_stay_weight")


#Fit temporal model
m_parallel <- cmdstan_model("m_SHMDM_temporal.stan", cpp_options = list(stan_threads = TRUE))
fit_parallel <- m_parallel$sample(stan.data, chains = 2, parallel_chains = 2, threads_per_chain = 50, refresh = 10, init = inits, iter_warmup = 1500 ,adapt_delta = 0.8, iter_sampling = 1000)
fit_temporal <- rstan::read_stan_csv(fit_parallel$output_files())
save(fit_temporal, file = "fit_HMM_temporal")


