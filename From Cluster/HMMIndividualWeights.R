
#Calculate individual weights from Hidden Markov Model

baseline_weight <- data.frame(id = 1:160, Incentives = NA,  Concentrated = NA, Distributed = NA)
stay_weight <- data.frame(id = 1:160, Incentives = NA,  Concentrated = NA, Distributed = NA)
dist_weight <- data.frame(id = 1:160, Incentives = NA,  Concentrated = NA, Distributed = NA)
time_weight <- data.frame(id = 1:160, Incentives = NA,  Concentrated = NA, Distributed = NA)
numb_weight <- data.frame(id = 1:160, Incentives = NA,  Concentrated = NA, Distributed = NA)

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

save(baseline_weight, file = "mean_baseline_weight")
save(dist_weight, file = "mean_dist_weight")
save(numb_weight, file = "mean_numb_weight")
save(time_weight, file = "mean_time_weight")
save(stay_weight, file = "mean_stay_weight")
