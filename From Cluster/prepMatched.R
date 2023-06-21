library(rethinking)
setwd("CoinScrounge")



library(cmdstanr)

#inits <- function () list(mu_closed = c(1, 0.5),mu_distance = c(0,-1), p_exploit_visible=c(0.5,0.99), xangle=c(3,12), yangle=c(0,0), sigma_closed = c(0.5,0.5), sigma_distance = c(0.5,0.5) )
inits <- function () list(mu_closed = c(1, 0.5),mu_distance = c(0,-1), xangle=c(5,30), yangle=c(0,0), sigma_closed = c(0.5,0.5), sigma_distance = c(0.5,0.5))




m_parallel <- cmdstan_model("HMM_CoinScroungeTime.stan", cpp_options = list(stan_threads = TRUE))
fit_parallel <- m_parallel$sample(stan.data, chains = 2, parallel_chains = 2, init = inits, threads_per_chain = 30, refresh = 1, iter_warmup = 1000 ,adapt_delta = 0.8, iter_sampling = 1000)
fit <- rstan::read_stan_csv(fit_parallel$output_files())
save(fit, file = "fit_HMMNTime_310323")

precis(fit, 3)

traceplot(fit)
s <- extract.samples(fit)

plot(precis(fit, pars = c("b_vis_time","b_dist_time", "b_numb_time", "b_time_time"), 3))


precis(fit, pars = c("sigma"), 3)



mod_gq <- cmdstan_model("HMM_CoinScroungeFullLuxuryViterbi.stan")
fit_gq <- mod_gq$generate_quantities(fit, data = stan.data, seed = 123)






baseline_weight <- data.frame(id = 1:160, Incentives = NA,  Concentrated = NA, Distributed = NA)
dist_weight <- data.frame(id = 1:160, Incentives = NA,  Concentrated = NA, Distributed = NA)
numb_weight <- data.frame(id = 1:160, Incentives = NA,  Concentrated = NA, Distributed = NA)
time_weight <- data.frame(id = 1:160, Incentives = NA,  Concentrated = NA, Distributed = NA)
stay_weight <- data.frame(id = 1:160, Incentives = NA,  Concentrated = NA, Distributed = NA)

for (id in 1:160) {
  print(id)
   
  incentives <- unique(stan.data$Incentives[stan.data$id==id])
  group <- unique(stan.data$group[stan.data$id==id])
  
  baseline_weight$Incentives[id] <- incentives
  vis_weight$Incentives[id] <- incentives
  time_weight$Incentives[id] <- incentives
  dist_weight$Incentives[id] <- incentives
  numb_weight$Incentives[id] <- incentives

  
  baseline_weight$Concentrated[id] <- mean(inv_logit((s$logit_p_I_S[,incentives,1] + s$offset_ID[,id, ifelse(incentives==1, 1, 3) ]  ) + (s$b_vis[,incentives,1] + s$offset_ID[,id, 2 + 1 ] ) ))
  baseline_weight$Distributed[id]  <- mean(inv_logit((s$logit_p_I_S[,incentives,2] + s$offset_ID[,id, 4+ifelse(incentives==1, 1, 3) ]  )+  (s$b_vis[,incentives,1] + s$offset_ID[,id, 2 + 2 ] ) ))
  
  stay_weight$Concentrated[id] <- mean(inv_logit(s$logit_p_S_S[,incentives,1] + s$offset_ID[,id, 10 + 1 ]  ))
  stay_weight$Distributed[id]  <- mean(inv_logit(s$logit_p_S_S[,incentives,2] + s$offset_ID[,id, 10 + 2 ]  ))
  
  time_weight$Concentrated[id] <- mean(inv_logit(s$b_time[,incentives,1] + s$offset_ID[,id, 4 + 1 ] ))
  time_weight$Distributed[id]  <- mean(inv_logit(s$b_time[,incentives,2] + s$offset_ID[,id, 4 + 2 ] ))
  
  dist_weight$Concentrated[id] <- mean(inv_logit(s$b_dist[,incentives,1] + s$offset_ID[,id, 6 + 1 ] ))
  dist_weight$Distributed[id]  <- mean(inv_logit(s$b_dist[,incentives,2] + s$offset_ID[,id, 6 + 2 ] ))
  
  numb_weight$Concentrated[id] <- mean(inv_logit(s$b_numb[,incentives,1] + s$offset_ID[,id, 8 + 1 ] ))
  numb_weight$Distributed[id]  <- mean(inv_logit(s$b_numb[,incentives,2] + s$offset_ID[,id, 8 + 2 ] ) )
  
  
}


save(baseline_weight, file = "baseline_weight") 
save(dist_weight, file = "dist_weight") 
save(numb_weight, file = "numb_weight")  
save(time_weight, file = "time_weight")  
save(stay_weight, file = "stay_weight") 


baseline <- array(NA, dim = c(2000,2,2))

for (i in 1:2) {
  for (j in 1:2) {
    baseline[, i,j] <- inv_logit(s$logit_p_I_S[,i,j] + s$b_vis[,i,j])
    
  }
}


par(mfrow = c(1,2))

dens(baseline[,1,1], xlim = c(0.1, 0.4))
dens(baseline[,1,2], xlim = c(0.1, 0.4), add = TRUE, lty = 2)

dens(baseline[,2,1], xlim = c(0.1, 0.4))
dens(baseline[,2,2], xlim = c(0.1, 0.4), add = TRUE, lty = 2)



HPDI(baseline[,1,]-baseline[,2,])
