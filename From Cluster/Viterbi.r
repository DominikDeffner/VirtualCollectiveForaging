
# Calculate Viterbi algorithm

s <- extract.samples(fit)

viterbi <- matrix(NA, nrow = stan.data$T, ncol = 10 )

N <- 2

#Loop over all samples in the posterior
for (i in 1:10 ) {
print(i)  
max_logp <- c()

back_ptr <- matrix(NA, nrow = stan.data$T, ncol =  N)
best_logp <- matrix(NA, nrow = stan.data$T, ncol =  N)

gamma <-array(NA, dim = c(stan.data$T, N, N))
log_gamma_tr <-array(NA, dim = c(stan.data$T, N, N))
epsilon <- 1e-6

#Loop over all datapoints
for (t in 1:stan.data$T) {
  
  p_I_S  <- c() 
  p_S_S <- c() 
  
  
  counter <- c() 
  
  #Create varying-effects counter varible to account for experimental condition
  
  if (stan.data$Incentives[t]==1 && stan.data$Environment[t]==1) counter = 1
  if (stan.data$Incentives[t]==1 && stan.data$Environment[t]==2) counter = 2
  if (stan.data$Incentives[t]==2 && stan.data$Environment[t]==1) counter = 3
  if (stan.data$Incentives[t]==2 && stan.data$Environment[t]==2) counter = 4
  
  # 1) Agents themselves just stopped exploiting or no other player was exploiting at time t-1 
# Agents will stay in (or switch to) exploration with very high probability
# Epsilon is a small number to avoid log(0), i.e., -Inf

if (stan.data$Exploit[t] == 1 || stan.data$No_Exploit[t] == 1){
  for ( n in 1:2 ){
    gamma[t,n,1] = 1-epsilon
    gamma[t,n,2] = 0+epsilon
  }
} else{
  
  # 2) Agents either explore or relocate towards successful group members

#Here we add predictors for transition probabilities
if (stan.data$Patch_Prox[t] >-10 ){
  p_I_S = inv_logit((s$logit_p_I_S[i,stan.data$Incentives[t],   stan.data$Environment[t] ] + s$offset_ID[i, stan.data$id[t],    counter ] + s$offset_Group[i, stan.data$group[t],   counter ])  +
                      (s$b_vis[i,stan.data$Incentives[t],   stan.data$Environment[t] ] + s$offset_ID[i, stan.data$id[t],4+  counter ] + s$offset_Group[i, stan.data$group[t],4+ counter ])  * stan.data$exploit_visible[t]  +
                      (s$b_time[i,stan.data$Incentives[t],   stan.data$Environment[t] ] + s$offset_ID[i, stan.data$id[t],8+  counter ] + s$offset_Group[i, stan.data$group[t],8+ counter ]) * stan.data$TimeSinceExploit[t] +
                      (s$b_dist[i,stan.data$Incentives[t],   stan.data$Environment[t] ] + s$offset_ID[i, stan.data$id[t],12+ counter ] + s$offset_Group[i, stan.data$group[t],12+ counter ]) * stan.data$exploit_visible[t]  * stan.data$Patch_Prox[t] +
                      (s$b_numb[i,stan.data$Incentives[t],   stan.data$Environment[t] ] + s$offset_ID[i, stan.data$id[t],16+ counter ] + s$offset_Group[i, stan.data$group[t],16+ counter ]) * stan.data$exploit_visible[t]  * stan.data$Patch_Number[t])
  
  
} else{
  p_I_S = inv_logit((s$logit_p_I_S[i,stan.data$Incentives[t],   stan.data$Environment[t] ] + s$offset_ID[i, stan.data$id[t],   counter ] + s$offset_Group[i, stan.data$group[t],   counter ])  +
                      (s$b_vis[i,stan.data$Incentives[t],   stan.data$Environment[t] ] + s$offset_ID[i, stan.data$id[t],4+ counter ] + s$offset_Group[i, stan.data$group[t],2+ counter ])  * stan.data$exploit_visible[t]  +
                      (s$b_time[i,stan.data$Incentives[t],   stan.data$Environment[t] ] + s$offset_ID[i, stan.data$id[t],8+ counter ] + s$offset_Group[i, stan.data$group[t],4+ counter ]) * stan.data$TimeSinceExploit[t])
  
  
  
}

p_S_S = inv_logit(s$logit_p_S_S[i,stan.data$Incentives[t],   stan.data$Environment[t] ] +
                    s$offset_ID[i, stan.data$id[t],   20 +  counter ] +
                    s$offset_Group[i, stan.data$group[t],20+counter ] )


#Complete transition probability matrix for time t
#P(stay individual)
gamma[t, 1, 1] = 1 - p_I_S

#P(switch to social)
gamma[t, 1, 2] = p_I_S

#P(switch to individual)
gamma[t, 2, 1] = 1 - p_S_S

#P(stay social)
gamma[t, 2, 2] = p_S_S
}

}#t

# Transpose transition probability matrix and take the log of each entry
for(t in 1:stan.data$T){
  for (n_from in 1:N){
    for (n in 1:N){
      log_gamma_tr[t, n, n_from] = log(gamma[t, n_from, n])
    }#n
  }#n_from
}#t



for (t in 1:stan.data$T) {
  if(t==1 || stan.data$track_id[t]!=stan.data$track_id[t-1]) {
    for(n in 1:N)
      best_logp[t, n] = log(dvonmises(stan.data$angles[t],  s$loc[i,n], s$kappa[i,n]))
  } else {
    for (n in 1:N) {
      best_logp[t, n] = -Inf
      
      for (j in 1:N) {
        logp <- c()
        logp = best_logp[t-1, j] + log_gamma_tr[t,j,n]
        
        if(stan.data$angles[t]>=(-pi) ){
          logp = logp + log(dvonmises(stan.data$angles[t],  s$loc[i,n], s$kappa[i,n]))
        }
        if(stan.data$deltadist[t]>-10){
          logp = logp + log(dnorm( stan.data$deltadist[t] , s$mu_distance[i,n], s$sigma_distance[i,n]))
        }
        if(stan.data$closed[t]>=0){
          logp = logp + log(dlnorm( stan.data$closed[t] , log(s$mu_closed[i,n]), s$sigma_closed[i,n]))
        }
        
        if (logp > best_logp[t, n]) {
          back_ptr[t, n] = j
          best_logp[t, n] = logp
        }#if
      }#j
    }#n
  }
}#t

for(t0 in 1:stan.data$T) {
  t = stan.data$T - t0 + 1
  if(t==stan.data$T || stan.data$track_id[t+1]!=stan.data$track_id[t]) {
    max_logp = max(best_logp[t,])
    for (n in 1:N)
      if (best_logp[t, n] == max_logp)
        viterbi[t,i] = n
  } else {
    viterbi[t,i] = back_ptr[t+1, viterbi[t+1,i]]
  }
}#t0



}
