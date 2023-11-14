
###
##
# BEHAVIORAL ANALYSES ON INDIVIDUAL LEVEL
##
###

###
##
# The first part runs analyses for comparison between experimental conditions
# The second part analyzes relation of behavioral metrics to individual success
# The third part analyzes relationship between movement metrics and independent discoveries
##
###

#Load relevant data
load("Data/d_rounds")

####
###
##
# Coins (Success)
##
###
####

#Construct data list
dat <- list(N = length(d$joining), 
            N_id = length(unique(d$id))  , 
            N_group = length(unique(d$Group))  , 
            coins = d$Coins, 
            Environment = ifelse(d$Env == "C",1,2), 
            Incentives = ifelse(d$Pay == "Coop",1,2),
            group = d$Group, 
            id = d$id )


#Stan model code


#Multilevel Poisson regression to predict expected number of coins in each condition accounting for individual- and group-level variability

poisson_multilevel_coins <- "


data{

  int N; 
  int N_id;
  int N_group;
  int coins[N];
  int Environment[N];
  int Incentives[N];
  int group[N];
  int id[N];   

}

parameters{
  matrix[2,2] reward_rate; 


  matrix[2, N_id] z_ID;           
  vector<lower = 0>[2] sigma_ID;  
  cholesky_factor_corr[2] Rho_ID; 

  matrix[2, N_group] z_Group;     
  vector<lower = 0>[2] sigma_Group;
  cholesky_factor_corr[2] Rho_Group;

} 

transformed parameters {
 matrix[N_id, 2] offset_ID;
 matrix[N_group, 2] offset_Group;

 
//Varying effects offsets
offset_ID = (diag_pre_multiply(sigma_ID, Rho_ID) * z_ID)';
offset_Group = (diag_pre_multiply(sigma_Group, Rho_Group) * z_Group)';
}

model{

  //Priors
  to_vector(reward_rate) ~ normal(5, 0.5); 

  //Define prior distribution of varying individual and group effects
  to_vector(z_ID) ~ normal(0, 1);
  sigma_ID ~ exponential(3);
  Rho_ID ~ lkj_corr_cholesky(4);

  to_vector(z_Group) ~ normal(0, 1);
  sigma_Group ~ exponential(3);
  Rho_Group ~ lkj_corr_cholesky(4);
  
//Likelihoods  
for(i in 1:N){                                                        
      coins[i]  ~ poisson(exp(reward_rate[Incentives[i],     Environment[i]] + offset_ID[id[i], Environment[i]] + offset_Group[group[i], Environment[i]] )); 
  }                                                                      
} 

generated quantities{
  real coins_pred[2,2];

      for(i in 1:2){                 
      for(j in 1:2){ 
      
        //Compute expected coins on  outcome scale
        coins_pred[i,j] = exp(reward_rate[i,j]);

      }                            
    }  

}

"

#Run stan model and save the extracted samples
m_coins <- stan(model_code = poisson_multilevel_coins, data = dat, iter = 4000, cores = 4, chains = 4, refresh = 10, control = list(adapt_delta = 0.9, max_treedepth = 12))
s_coins <- extract.samples(m_coins)


#Success conditional on environment in previous round

#Get environment from previous round (3 means first round of experiment)
d$Environment_prev <- rep(3, nrow(d))
for (i in 1:nrow(d)) {
  if (d$round[i] > 1){
    d$Environment_prev[i] <- ifelse(d$Env[d$id == d$id[i] & d$round == (d$round[i]-1)] == "C",1,2)
  }  
}
dat$Environment_prev = d$Environment_prev

order_coins <- "

data{

  int N; 
  int N_id;
  int N_group;
  int coins[N];
  int Environment[N];
  int Environment_prev[N];
  int Incentives[N];
  int group[N];
  int id[N];   

}

parameters{
  real reward_rate[2,2,3]; 


  matrix[2, N_id] z_ID;           
  vector<lower = 0>[2] sigma_ID;  
  cholesky_factor_corr[2] Rho_ID; 

  matrix[2, N_group] z_Group;     
  vector<lower = 0>[2] sigma_Group;
  cholesky_factor_corr[2] Rho_Group;

} 

transformed parameters {
 matrix[N_id, 2] offset_ID;
 matrix[N_group, 2] offset_Group;

 
//Varying effects offsets
offset_ID = (diag_pre_multiply(sigma_ID, Rho_ID) * z_ID)';
offset_Group = (diag_pre_multiply(sigma_Group, Rho_Group) * z_Group)';
}

model{

  //Priors
  for (i in 1:2){
    for (j in 1:2){
      for (k in 1:3){
        reward_rate[i,j,k] ~ normal(5, 0.5); 
      }
    }
  }
  //Define prior distribution of varying individual and group effects
  to_vector(z_ID) ~ normal(0, 1);
  sigma_ID ~ exponential(3);
  Rho_ID ~ lkj_corr_cholesky(4);

  to_vector(z_Group) ~ normal(0, 1);
  sigma_Group ~ exponential(3);
  Rho_Group ~ lkj_corr_cholesky(4);
  
//Likelihoods  
for(i in 1:N){                                                        
      coins[i]  ~ poisson(exp(reward_rate[Incentives[i],Environment[i], Environment_prev[i] ] + offset_ID[id[i], Environment[i]] + offset_Group[group[i], Environment[i]] )); 
  }                                                                      
} 

generated quantities{
  real coins_pred[2,2,3];

      for(i in 1:2){                 
         for(j in 1:2){ 
            for(k in 1:3){ 

        //Compute expected coins on  outcome scale
        coins_pred[i,j,k] = exp(reward_rate[i,j,k]);
       }
      }                            
    }  
}

"


#Run stan model and save the extracted samples
m_order <- stan(model_code = order_coins, data = dat, iter = 4000, cores = 4, chains = 4, refresh = 10, control = list(adapt_delta = 0.99, max_treedepth = 12))
s_order <- extract.samples(m_order)


#Frequentist analysis
dat$Environment <- dat$Environment-1
dat$Incentives <- dat$Incentives-1
m_frequ_coins <- glm(coins ~ Environment + Incentives  + Environment*Incentives + (group/id), family = poisson(link = "log"), data = dat)



####
###
##
# Discoveries, Joinings and Scrounging
##
###
####


missing <- which(is.na(d$exploit_observed))
dat <- list(N = length(d$joining[-missing]), 
            N_id = length(unique(d$id[-missing]))  , 
            N_group = length(unique(d$Group[-missing]))  , 
            joining = d$joining[-missing], 
            discoveries = d$discoveries[-missing], 
            observed = d$exploit_observed[-missing],
            visibility = d$Visibility[-missing], 
            distance = d$MeanDistance[-missing],
            Environment = ifelse(d$Env[-missing] == "C",1,2), 
            Incentives = ifelse(d$Pay[-missing] == "Coop",1,2),
            group = d$Group[-missing], 
            id = d$id[-missing], 
            coins = d$Coins[-missing])


#Multilevel Poisson regression to predict expected number of discoveries/joinings in each condition accounting for individual- and group-level variability

poisson_multilevel_rates <- "


data{

  int N; 
  int N_id;
  int N_group;
  int joining[N];
  int discoveries[N];
  int observed[N];
  int Environment[N];
  int Incentives[N];
  int group[N];
  int id[N];   

}

parameters{
  matrix[2,2] discovery_rate; 
  matrix[2,2] joining_rate; 
  matrix[2,2] observation_rate; 


  matrix[6, N_id] z_ID;           
  vector<lower = 0>[6] sigma_ID;  
  cholesky_factor_corr[6] Rho_ID; 

  matrix[6, N_group] z_Group;     
  vector<lower = 0>[6] sigma_Group;
  cholesky_factor_corr[6] Rho_Group;

} 

transformed parameters {
 matrix[N_id, 6] offset_ID;
 matrix[N_group, 6] offset_Group;

 
//Varying effects offsets
offset_ID = (diag_pre_multiply(sigma_ID, Rho_ID) * z_ID)';
offset_Group = (diag_pre_multiply(sigma_Group, Rho_Group) * z_Group)';
}

model{

  //Priors
  to_vector(discovery_rate) ~ normal(2, 0.5); 
  to_vector(joining_rate) ~ normal(2, 0.5); 
  to_vector(observation_rate) ~ normal(2, 0.5); 

  //Define prior distribution of varying individual and group effects
  to_vector(z_ID) ~ normal(0, 1);
  sigma_ID ~ exponential(2);
  Rho_ID ~ lkj_corr_cholesky(4);

  to_vector(z_Group) ~ normal(0, 1);
  sigma_Group ~ exponential(2);
  Rho_Group ~ lkj_corr_cholesky(4);
  
//Likelihoods  
for(i in 1:N){                                                        
      joining[i]     ~ poisson(exp(joining_rate[Incentives[i],     Environment[i]] + offset_ID[id[i], 1 + 3*(Environment[i]-1)] + offset_Group[group[i], 1 + 3*(Environment[i]-1)] )); 
      discoveries[i] ~ poisson(exp(discovery_rate[Incentives[i],   Environment[i]] + offset_ID[id[i], 2 + 3*(Environment[i]-1)] + offset_Group[group[i], 2 + 3*(Environment[i]-1)] )); 
      observed[i]    ~ poisson(exp(observation_rate[Incentives[i], Environment[i]] + offset_ID[id[i], 3 + 3*(Environment[i]-1)] + offset_Group[group[i], 3 + 3*(Environment[i]-1)] )); 
  }                                                                      
} 

generated quantities{
  real joining_pred[2,2];
  real discoveries_pred[2,2];
  real observed_pred[2,2];

      for(i in 1:2){                 
      for(j in 1:2){ 
      
        //Compute joining, discoveries and observation rates on the outcome scale
        joining_pred[i,j] = exp(joining_rate[i,j]);
        discoveries_pred[i,j] = exp(discovery_rate[i,j]); 
        observed_pred[i,j] = exp(observation_rate[i,j]); 

      }                            
    }  

}

"

m_rates <- stan(model_code = poisson_multilevel_rates, data = dat, iter = 4000, cores = 4, chains = 4, refresh = 10, control = list(adapt_delta = 0.9, max_treedepth = 12))
s_rates <- extract.samples(m_rates)

#Frequentist analysis
dat$Environment <- dat$Environment-1
dat$Incentives <- dat$Incentives-1
m_frequ_join <- glm(joining ~ Environment + Incentives  + Environment*Incentives + (group/id), family = poisson(link = "log"), data = dat)
m_frequ_disc <- glm(discoveries ~ Environment + Incentives  + Environment*Incentives + (group/id), family = poisson(link = "log"), data = dat)


#Multilevel Binomial regression for scrounging rates in each condition accounting for individual- and group-level variability
#This computes the probability that players end up joining a patch where they've observed others exploiting

binomial_multilevel <- "

data{

  int N; 
  int N_id;
  int N_group;
  int joining[N];
  int observed[N];
  int Environment[N];
  int Incentives[N];
  int group[N];
  int id[N]; 
  int coins[N];
}

parameters{
  matrix[2,2] logit_scrounging_rate; 
  
  matrix[6, N_id] z_ID;           
  vector<lower = 0>[6] sigma_ID;  
  cholesky_factor_corr[6] Rho_ID; 

  matrix[6, N_group] z_Group;     
  vector<lower = 0>[6] sigma_Group;
  cholesky_factor_corr[6] Rho_Group;
  
    //Regression stuff
  //Intercepts for environments
  vector[2] alpha; 
  
  //Offsets for incentives (coded as -1 and 1, so alpha represents mean across conditions)
  vector[2] beta_Incentive; 

  //Regression weights of predictor
  vector[2] weight; 

} 

transformed parameters {
 matrix[N_id, 6] offset_ID;
 matrix[N_group, 6] offset_Group;

 
//Varying effects offsets
offset_ID = (diag_pre_multiply(sigma_ID, Rho_ID) * z_ID)';
offset_Group = (diag_pre_multiply(sigma_Group, Rho_Group) * z_Group)';
}

model{

  //Priors
  to_vector(logit_scrounging_rate) ~ normal(0, 1); 

  //Define prior distribution of varying individual and group effects
  to_vector(z_ID) ~ normal(0, 1);
  sigma_ID ~ exponential(3);
  Rho_ID ~ lkj_corr_cholesky(4);

  to_vector(z_Group) ~ normal(0, 1);
  sigma_Group ~ exponential(3);
  Rho_Group ~ lkj_corr_cholesky(4);
  
  //Priors
 alpha ~ normal(5, 0.5);
 beta_Incentive ~ normal(0,1);
 weight ~ normal(0, 1); 


  
//Likelihood  
for(i in 1:N){  
 real incent_pred;
 if (Incentives[i] == 1){
  incent_pred = 1;
 }else{
 incent_pred = -1;
 }
      joining[i] ~ binomial(observed[i], inv_logit(logit_scrounging_rate[Incentives[i],Environment[i]]  + offset_ID[id[i], Environment[i]] + offset_Group[group[i], Environment[i]]));

       coins[i]  ~ poisson(exp( (alpha[Environment[i]]  + offset_Group[group[i],2+Environment[i]]  + offset_ID[id[i],2+Environment[i]]) +
                            beta_Incentive[Environment[i]]*incent_pred+
                            (weight[Environment[i]]  + offset_Group[group[i],4 + Environment[i]] + offset_ID[id[i],4 + Environment[i]]) *
                            inv_logit(logit_scrounging_rate[Incentives[i],Environment[i]]  + offset_ID[id[i], Environment[i]] )  ) );
     

} 
} 

generated quantities{
  real scrounging_rate[2,2];
      for(i in 1:2){                 
      for(j in 1:2){ 
      
        //Compute Scrounging rate
        scrounging_rate[i,j] = inv_logit(logit_scrounging_rate[i,j]);

      }                            
    }  

}

"



#If there are more joining events than observed opportunities, set to maximum number; this can happen if 
#players "accidentally" join other patches without observing a player there

dat$joining <- ifelse(dat$joining > dat$observed, dat$observed, dat$joining)

m_scrounging <- stan(model_code = binomial_multilevel, data = dat, iter = 2000, cores = 4, chains = 4, refresh = 10, control = list(adapt_delta = 0.9, max_treedepth = 12))
s_scrounging <- extract.samples(m_scrounging)

#Frequentist analysis
m_frequ_scrounge <- glm(cbind(joining, observed-joining) ~ Environment + Incentives  + Environment*Incentives + (group/id), family = binomial, data = dat)


#Get individual scrounging rates for plotting
scrounging <- data_frame(id = 1:160, Concentrated = NA, Distributed = NA, Incentives = NA, CoinsCon = NA, CoinsDist = NA)
scrounging_con  <- matrix(NA, 160, length(s_scrounging$lp__))
scrounging_dist <- matrix(NA, 160, length(s_scrounging$lp__))


for (id in 1:160) {
  incent <- ifelse(unique(d$Pay[d$id == id])=="Coop", 1, 2)
  group <- unique(d$Group[d$id == id])
  scrounging$Concentrated[id] <- mean(inv_logit(s_scrounging$logit_scrounging_rate[,incent, 1] + s_scrounging$offset_ID[,id,1]   ))
  scrounging$Distributed[id]  <- mean(inv_logit(s_scrounging$logit_scrounging_rate[,incent, 2] + s_scrounging$offset_ID[,id,2]   ))
  
  scrounging_con[id,]  <- inv_logit(s_scrounging$logit_scrounging_rate[,incent, 1] + s_scrounging$offset_ID[,id,1]   )
  scrounging_dist[id,] <- inv_logit(s_scrounging$logit_scrounging_rate[,incent, 2] + s_scrounging$offset_ID[,id,2]   )
  
  scrounging$Incentives[id] <- incent
  
  scrounging$CoinsCon[id]  <- mean(d$Coins[d$id == id & d$Env == "C"])
  scrounging$CoinsDist[id] <- mean(d$Coins[d$id == id & d$Env == "D"])
  
}



####
###
##
# Distance
##
###
####



missing <- which(is.na(d$MeanDistance))
dat <- list(N = length(d$MeanDistance[-missing]), 
            N_id = length(unique(d$id[-missing]))  , 
            N_group = length(unique(d$Group[-missing]))  , 
            visibility = d$Visibility[-missing], 
            dist = d$MeanDistance[-missing], 
            Environment = ifelse(d$Env[-missing] == "C",1,2), 
            Incentives = ifelse(d$Pay[-missing] == "Coop",1,2),
            group = d$Group[-missing], 
            id = d$id[-missing] )


#Multilevel lognormal regression for average distance to other players in each condition accounting for individual- and group-level variability

lognormal_multilevel_distance <- "

data{

  int N; 
  int N_id;
  int N_group;
  real dist[N];
  int Environment[N];
  int Incentives[N];
  int group[N];
  int id[N];   

}

parameters{
  matrix[2,2] mean_distance; 
  matrix<lower=0>[2,2] sigma_distance; 

  matrix[2, N_id] z_ID;           
  vector<lower = 0>[2] sigma_ID;  
  cholesky_factor_corr[2] Rho_ID; 

  matrix[2, N_group] z_Group;     
  vector<lower = 0>[2] sigma_Group;
  cholesky_factor_corr[2] Rho_Group;

} 

transformed parameters {
 matrix[N_id, 2] offset_ID;
 matrix[N_group, 2] offset_Group;

 
//Varying effects offsets
offset_ID = (diag_pre_multiply(sigma_ID, Rho_ID) * z_ID)';
offset_Group = (diag_pre_multiply(sigma_Group, Rho_Group) * z_Group)';
}

model{

  //Priors
  to_vector(mean_distance) ~ normal(5, 0.5); 
  to_vector(sigma_distance) ~ exponential(2); 

  //Define prior distribution of varying individual and group effects
  to_vector(z_ID) ~ normal(0, 1);
  sigma_ID ~ exponential(2);
  Rho_ID ~ lkj_corr_cholesky(4);

  to_vector(z_Group) ~ normal(0, 1);
  sigma_Group ~ exponential(2);
  Rho_Group ~ lkj_corr_cholesky(4);
  
//Likelihoods  
for(i in 1:N){                                                        
      dist[i] ~ lognormal(mean_distance[Incentives[i],  Environment[i]] + offset_ID[id[i], 1] + offset_Group[group[i], 1] , sigma_distance[Incentives[i],  Environment[i]] ); 
  }                                                                      
} 

generated quantities{
  real avg_distance[2,2];

      for(i in 1:2){                 
      for(j in 1:2){ 
      
        //Compute Scrounging rate
        avg_distance[i,j] = exp(mean_distance[i,j]);

      }                            
    }  

}

"


m_distance <- stan(model_code = lognormal_multilevel_distance, data = dat, iter = 4000, cores = 4, chains = 4, refresh = 10, control = list(adapt_delta = 0.9, max_treedepth = 12))
s_distance <- extract.samples(m_distance)

#Frequentist analysis
dat$Environment <- dat$Environment-1
dat$Incentives <- dat$Incentives-1
m_frequ_dist <- glm(log(dist) ~ Environment + Incentives  + Environment*Incentives + (group/id), family = gaussian, data = dat)

####
###
##
# Visibility
##
###
####

missing <- which(is.na(d$Visibility))
dat <- list(N = length(d$Visibility[-missing]), 
            N_id = length(unique(d$id[-missing]))  , 
            N_group = length(unique(d$Group[-missing]))  , 
            visibility = d$Visibility[-missing], 
            dist = d$MeanDistance[-missing], 
            Environment = ifelse(d$Env[-missing] == "C",1,2), 
            Incentives = ifelse(d$Pay[-missing] == "Coop",1,2),
            group = d$Group[-missing], 
            id = d$id[-missing] )

#Multilevel Gaussian regression for visibility in each condition accounting for individual- and group-level variability

normal_multilevel_visibility <- "

data{

  int N; 
  int N_id;
  int N_group;
  real visibility[N];
  int Environment[N];
  int Incentives[N];
  int group[N];
  int id[N];   

}

parameters{
  matrix[2,2] mean_visibility; 
  matrix<lower=0>[2,2] sigma_visibility; 



  matrix[2, N_id] z_ID;           
  vector<lower = 0>[2] sigma_ID;  
  cholesky_factor_corr[2] Rho_ID; 

  matrix[2, N_group] z_Group;     
  vector<lower = 0>[2] sigma_Group;
  cholesky_factor_corr[2] Rho_Group;

} 

transformed parameters {
 matrix[N_id, 2] offset_ID;
 matrix[N_group, 2] offset_Group;

 
//Varying effects offsets
offset_ID = (diag_pre_multiply(sigma_ID, Rho_ID) * z_ID)';
offset_Group = (diag_pre_multiply(sigma_Group, Rho_Group) * z_Group)';
}

model{

  //Priors
  to_vector(mean_visibility) ~ normal(1, 0.5); 
  to_vector(sigma_visibility) ~ exponential(2); 


  //Define prior distribution of varying individual and group effects
  to_vector(z_ID) ~ normal(0, 1);
  sigma_ID ~ exponential(2);
  Rho_ID ~ lkj_corr_cholesky(4);

  to_vector(z_Group) ~ normal(0, 1);
  sigma_Group ~ exponential(2);
  Rho_Group ~ lkj_corr_cholesky(4);
  
//Likelihoods  
for(i in 1:N){                                                        
      visibility[i] ~ normal(mean_visibility[Incentives[i],  Environment[i]] + offset_ID[id[i], 1] + offset_Group[group[i], 1] , sigma_visibility[Incentives[i],  Environment[i]] ); 
  }                                                                      
} 


"


m_visibility <- stan(model_code = normal_multilevel_visibility, data = dat, iter = 4000, cores = 4, chains = 4, refresh = 10, control = list(adapt_delta = 0.9, max_treedepth = 12))
s_visibility <- extract.samples(m_visibility)

#Frequentist analysis
dat$Environment <- dat$Environment-1
dat$Incentives <- dat$Incentives-1
m_frequ_vis <- glm(visibility ~ Environment + Incentives  + Environment*Incentives + (group/id), family = gaussian, data = dat)


###
##
# Create supplementary behavioral plot S1 
##
###


#Define plotting function to be re-used later

behavioral_plotting_fct <- function(xseq = c(1.5, 2, 2.75, 3.25),
                                    
                                    ylim = c(30,230),
                                    
                                    ylab = "Coins",
                                    
                                    con_coop   =  dat$coins[dat$Environment==1 & dat$Incentives==1],
                                    con_coop_ids  =  dat$id[dat$Environment==1 & dat$Incentives==1],
                                    con_coop_mean =  mean(s_coins$coins_pred[,1,1]),
                                    con_coop_hpdi  =  HPDI(s_coins$coins_pred[,1,1]),
                                    
                                    dist_coop  =  dat$coins[dat$Environment==2 & dat$Incentives==1],
                                    dist_coop_ids   =  dat$id[dat$Environment==2 & dat$Incentives==1],
                                    dist_coop_mean  =  mean(s_coins$coins_pred[,1,2]),
                                    dist_coop_hpdi  =  HPDI(s_coins$coins_pred[,1,2]),
                                    
                                    con_comp  =  dat$coins[dat$Environment==1 & dat$Incentives==2],
                                    con_comp_ids   =  dat$id[dat$Environment==1 & dat$Incentives==2],
                                    con_comp_mean  =  mean(s_coins$coins_pred[,2,1]),
                                    con_comp_hpdi =  HPDI(s_coins$coins_pred[,2,1]),
                                    
                                    dist_comp  =  dat$coins[dat$Environment==2 & dat$Incentives==2],
                                    dist_comp_ids  =  dat$id[dat$Environment==2 & dat$Incentives==2],
                                    dist_comp_mean  =  mean(s_coins$coins_pred[,2,2]),
                                    dist_comp_hpdi  = HPDI(s_coins$coins_pred[,2,2])){
  
  
  plot(1:4,xlim = c(1.25,3.5), ylim = ylim, type = "n", xaxt = "n", xlab = "", ylab = "", bty = "n", las = 1)
  axis(side = 1, at = c(1.75, 3), labels = c("Group Incentives", "Individual Incentives"), col = "white", cex.axis = 1.1 )
  mtext(side = 2, ylab, line = 2.5, cex = 0.9)
  ##
  #Cooperative
  ##
  
  x_coords_con  <- rep(xseq[1],length(con_coop))+jitter(rep(0,length(con_coop)),9)
  x_coords_dist <- rep(xseq[2],length(dist_coop))+jitter(rep(0,length(dist_coop)),9)
  
  #Concentrated
  points(x=x_coords_con,y=con_coop, col = alpha(col.pal[2], alpha = 0.2))
  points(xseq[1], con_coop_mean, pch = 16, cex = 2, col = alpha(col.pal[2], alpha = 1))
  segments(xseq[1],con_coop_hpdi[1],xseq[1],con_coop_hpdi[2], col = alpha(col.pal[2], alpha = 1), lwd = 2)
  
  
  #Distributed
  points(x=x_coords_dist,y=dist_coop, col = alpha(col.pal[3], alpha = 0.2))
  points(xseq[2], dist_coop_mean, pch = 16, cex = 2, col = alpha(col.pal[3], alpha = 1))
  segments(xseq[2],dist_coop_hpdi[1],xseq[2],dist_coop_hpdi[2], col = alpha(col.pal[3], alpha = 1), lwd = 2)
  
  ##
  #Competitive
  ##
  x_coords_con  <- rep(xseq[3],length(con_comp))+jitter(rep(0,length(con_comp)),9)
  x_coords_dist <- rep(xseq[4],length(dist_comp))+jitter(rep(0,length(dist_comp)),9)
  
  #Concentrated
  points(x=x_coords_con,y=con_comp, col = alpha(col.pal[2], alpha = 0.2))
  points(xseq[3], con_comp_mean, pch = 16, cex = 2, col = alpha(col.pal[2], alpha = 1))
  segments(xseq[3],con_comp_hpdi[1],xseq[3],con_comp_hpdi[2], col = alpha(col.pal[2], alpha = 1), lwd = 2)
  
  #Distributed
  points(x=x_coords_dist,y=dist_comp, col = alpha(col.pal[3], alpha = 0.2))
  points(xseq[4], dist_comp_mean, pch = 16, cex = 2, col = alpha(col.pal[3], alpha = 1))
  segments(xseq[4],dist_comp_hpdi[1],xseq[4],dist_comp_hpdi[2], col = alpha(col.pal[3], alpha = 1), lwd = 2)
  
}




#graphics.off()
#pdf("S1Behavioral.pdf", height = 5.25, width = 7.25)

par(mfrow = c(2,2),
    mar = c(3,3, 0,0.5), 
    oma = c(0.5,0.6,1,0))


#Rates
missing <- which(is.na(d$exploit_observed))
dat <- list(N = length(d$joining[-missing]), 
            N_id = length(unique(d$id[-missing]))  , 
            N_group = length(unique(d$Group[-missing]))  , 
            joining = d$joining[-missing], 
            discoveries = d$discoveries[-missing], 
            observed = d$exploit_observed[-missing],
            visibility = d$Visibility[-missing], 
            distance = d$MeanDistance[-missing],
            Environment = ifelse(d$Env[-missing] == "C",1,2), 
            Incentives = ifelse(d$Pay[-missing] == "Coop",1,2),
            group = d$Group[-missing], 
            id = d$id[-missing] )

#Discoveries

behavioral_plotting_fct(            ylim = c(0,15),
                                    
                                    ylab = "Independent Discoveries",
                                    
                                    con_coop   =  dat$discoveries[dat$Environment==1 & dat$Incentives==1],
                                    con_coop_ids  =  dat$id[dat$Environment==1 & dat$Incentives==1],
                                    con_coop_mean =  mean(s_rates$discoveries_pred[,1,1]),
                                    con_coop_hpdi  =  HPDI(s_rates$discoveries_pred[,1,1]),
                                    
                                    dist_coop  =  dat$discoveries[dat$Environment==2 & dat$Incentives==1],
                                    dist_coop_ids   =  dat$id[dat$Environment==2 & dat$Incentives==1],
                                    dist_coop_mean  =  mean(s_rates$discoveries_pred[,1,2]),
                                    dist_coop_hpdi  =  HPDI(s_rates$discoveries_pred[,1,2]),
                                    
                                    con_comp  =  dat$discoveries[dat$Environment==1 & dat$Incentives==2],
                                    con_comp_ids   =  dat$id[dat$Environment==1 & dat$Incentives==2],
                                    con_comp_mean  =  mean(s_rates$discoveries_pred[,2,1]),
                                    con_comp_hpdi =  HPDI(s_rates$discoveries_pred[,2,1]),
                                    
                                    dist_comp  =  dat$discoveries[dat$Environment==2 & dat$Incentives==2],
                                    dist_comp_ids  =  dat$id[dat$Environment==2 & dat$Incentives==2],
                                    dist_comp_mean  =  mean(s_rates$discoveries_pred[,2,2]),
                                    dist_comp_hpdi  = HPDI(s_rates$discoveries_pred[,2,2]))


#Joinings
behavioral_plotting_fct(            ylim = c(0,15),
                                    
                                    ylab = "Patch Joinings",
                                    
                                    con_coop   =  dat$joining[dat$Environment==1 & dat$Incentives==1],
                                    con_coop_ids  =  dat$id[dat$Environment==1 & dat$Incentives==1],
                                    con_coop_mean =  mean(s_rates$joining_pred[,1,1]),
                                    con_coop_hpdi  =  HPDI(s_rates$joining_pred[,1,1]),
                                    
                                    dist_coop  =  dat$joining[dat$Environment==2 & dat$Incentives==1],
                                    dist_coop_ids   =  dat$id[dat$Environment==2 & dat$Incentives==1],
                                    dist_coop_mean  =  mean(s_rates$joining_pred[,1,2]),
                                    dist_coop_hpdi  =  HPDI(s_rates$joining_pred[,1,2]),
                                    
                                    con_comp  =  dat$joining[dat$Environment==1 & dat$Incentives==2],
                                    con_comp_ids   =  dat$id[dat$Environment==1 & dat$Incentives==2],
                                    con_comp_mean  =  mean(s_rates$joining_pred[,2,1]),
                                    con_comp_hpdi =  HPDI(s_rates$joining_pred[,2,1]),
                                    
                                    dist_comp  =  dat$joining[dat$Environment==2 & dat$Incentives==2],
                                    dist_comp_ids  =  dat$id[dat$Environment==2 & dat$Incentives==2],
                                    dist_comp_mean  =  mean(s_rates$joining_pred[,2,2]),
                                    dist_comp_hpdi  = HPDI(s_rates$joining_pred[,2,2]))

legend("topright",title = "Environment", c("Concentrated", "Distributed"), col = c(alpha(col.pal[2],alpha = 0.9),alpha(col.pal[3],alpha = 0.9)), cex = 0.9, lwd = 8, lty = 1, bty = "n")


# Distance

missing <- which(is.na(d$MeanDistance))
dat <- list(N = length(d$MeanDistance[-missing]), 
            N_id = length(unique(d$id[-missing]))  , 
            N_group = length(unique(d$Group[-missing]))  , 
            visibility = d$Visibility[-missing], 
            dist = d$MeanDistance[-missing], 
            Environment = ifelse(d$Env[-missing] == "C",1,2), 
            Incentives = ifelse(d$Pay[-missing] == "Coop",1,2),
            group = d$Group[-missing], 
            id = d$id[-missing] )

behavioral_plotting_fct(            ylim = c(5,50),
                                    
                                    ylab = "Distance",
                                    
                                    con_coop   =  dat$dist[dat$Environment==1 & dat$Incentives==1],
                                    con_coop_ids  =  dat$id[dat$Environment==1 & dat$Incentives==1],
                                    con_coop_mean =  mean(s_distance$avg_distance[,1,1]),
                                    con_coop_hpdi  =  HPDI(s_distance$avg_distance[,1,1]),
                                    
                                    dist_coop  =  dat$dist[dat$Environment==2 & dat$Incentives==1],
                                    dist_coop_ids   =  dat$id[dat$Environment==2 & dat$Incentives==1],
                                    dist_coop_mean  =  mean(s_distance$avg_distance[,1,2]),
                                    dist_coop_hpdi  =  HPDI(s_distance$avg_distance[,1,2]),
                                    
                                    con_comp  =  dat$dist[dat$Environment==1 & dat$Incentives==2],
                                    con_comp_ids   =  dat$id[dat$Environment==1 & dat$Incentives==2],
                                    con_comp_mean  =  mean(s_distance$avg_distance[,2,1]),
                                    con_comp_hpdi =  HPDI(s_distance$avg_distance[,2,1]),
                                    
                                    dist_comp  =  dat$dist[dat$Environment==2 & dat$Incentives==2],
                                    dist_comp_ids  =  dat$id[dat$Environment==2 & dat$Incentives==2],
                                    dist_comp_mean  =  mean(s_distance$avg_distance[,2,2]),
                                    dist_comp_hpdi  = HPDI(s_distance$avg_distance[,2,2]))




# Visibility

missing <- which(is.na(d$Visibility))
dat <- list(N = length(d$Visibility[-missing]), 
            N_id = length(unique(d$id[-missing]))  , 
            N_group = length(unique(d$Group[-missing]))  , 
            visibility = d$Visibility[-missing], 
            dist = d$MeanDistance[-missing], 
            Environment = ifelse(d$Env[-missing] == "C",1,2), 
            Incentives = ifelse(d$Pay[-missing] == "Coop",1,2),
            group = d$Group[-missing], 
            id = d$id[-missing] )


behavioral_plotting_fct(            ylim = c(0.2,2),
                                    
                                    ylab = "Visible Players",
                                    
                                    con_coop   =  dat$visibility[dat$Environment==1 & dat$Incentives==1],
                                    con_coop_ids  =  dat$id[dat$Environment==1 & dat$Incentives==1],
                                    con_coop_mean =  mean(s_visibility$mean_visibility[,1,1]),
                                    con_coop_hpdi  =  HPDI(s_visibility$mean_visibility[,1,1]),
                                    
                                    dist_coop  =  dat$visibility[dat$Environment==2 & dat$Incentives==1],
                                    dist_coop_ids   =  dat$id[dat$Environment==2 & dat$Incentives==1],
                                    dist_coop_mean  =  mean(s_visibility$mean_visibility[,1,2]),
                                    dist_coop_hpdi  =  HPDI(s_visibility$mean_visibility[,1,2]),
                                    
                                    con_comp  =  dat$visibility[dat$Environment==1 & dat$Incentives==2],
                                    con_comp_ids   =  dat$id[dat$Environment==1 & dat$Incentives==2],
                                    con_comp_mean  =  mean(s_visibility$mean_visibility[,2,1]),
                                    con_comp_hpdi =  HPDI(s_visibility$mean_visibility[,2,1]),
                                    
                                    dist_comp  =  dat$visibility[dat$Environment==2 & dat$Incentives==2],
                                    dist_comp_ids  =  dat$id[dat$Environment==2 & dat$Incentives==2],
                                    dist_comp_mean  =  mean(s_visibility$mean_visibility[,2,2]),
                                    dist_comp_hpdi  = HPDI(s_visibility$mean_visibility[,2,2]))




#dev.off()



#####
####
###
##
# BEHAVIOR AND SUCCESS
##
###
####
#####


#First save all stan models

coins_predict <- "

data{

  int N; 
  int N_id;
  int N_group;
  int coins[N];
  int pred[N];
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

  matrix[4, N_id] z_ID;     
  vector<lower = 0>[4] sigma_ID;
  cholesky_factor_corr[4] Rho_ID;
  
  matrix[4, N_group] z_Group;     
  vector<lower = 0>[4] sigma_Group;
  cholesky_factor_corr[4] Rho_Group;

} 

transformed parameters {
 matrix[N_group, 4] offset_Group;
 matrix[N_id, 4] offset_ID;

//Varying effects offsets
offset_Group = (diag_pre_multiply(sigma_Group, Rho_Group) * z_Group)';
offset_ID = (diag_pre_multiply(sigma_ID, Rho_ID) * z_ID)';


} 

model{

 //Priors
 alpha ~ normal(5, 0.5);
 beta_Incentive ~ normal(0,1);
 weight ~ normal(0, 1); 


  //Define prior distribution of varying  effects
  to_vector(z_Group) ~ normal(0, 1);
  sigma_Group ~ exponential(3);
  Rho_Group ~ lkj_corr_cholesky(4);
  
    to_vector(z_ID) ~ normal(0, 1);
  sigma_ID ~ exponential(3);
  Rho_ID ~ lkj_corr_cholesky(4);
  
//Likelihoods  
for(i in 1:N){
      coins[i]  ~ poisson(exp( (alpha[Environment[i]]  + offset_Group[group[i],Environment[i]]  + offset_ID[id[i],Environment[i]]) +
                            beta_Incentive[Environment[i]]*Incentives[i]+
                            (weight[Environment[i]]  + offset_Group[group[i],2 + Environment[i]] + offset_ID[id[i],2 + Environment[i]])*pred[i]) ); 
   
  }                                                                      
} 

"


coins_predict_dist <- "


data{

  int N; 
  int N_id;
  int N_group;
  int coins[N];
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

  matrix[4, N_id] z_ID;     
  vector<lower = 0>[4] sigma_ID;
  cholesky_factor_corr[4] Rho_ID;
  
  matrix[4, N_group] z_Group;     
  vector<lower = 0>[4] sigma_Group;
  cholesky_factor_corr[4] Rho_Group;

} 

transformed parameters {
 matrix[N_group, 4] offset_Group;
 matrix[N_id, 4] offset_ID;

//Varying effects offsets
offset_Group = (diag_pre_multiply(sigma_Group, Rho_Group) * z_Group)';
offset_ID = (diag_pre_multiply(sigma_ID, Rho_ID) * z_ID)';


} 

model{

 //Priors
 alpha ~ normal(5, 0.5);
 beta_Incentive ~ normal(0,1);
 weight ~ normal(0, 1); 


  //Define prior distribution of varying  effects
  to_vector(z_Group) ~ normal(0, 1);
  sigma_Group ~ exponential(3);
  Rho_Group ~ lkj_corr_cholesky(4);
  
    to_vector(z_ID) ~ normal(0, 1);
  sigma_ID ~ exponential(3);
  Rho_ID ~ lkj_corr_cholesky(4);
  
//Likelihoods  
for(i in 1:N){
   if (pred[i] > -10){
      coins[i]  ~ poisson(exp( (alpha[Environment[i]]  + offset_Group[group[i],Environment[i]]  + offset_ID[id[i],Environment[i]]) +
                            beta_Incentive[Environment[i]]*Incentives[i]+
                            (weight[Environment[i]]  + offset_Group[group[i],2 + Environment[i]] + offset_ID[id[i],2 + Environment[i]])*pred[i]) ); 
   
    }   
  }                                                                      
} 

"



discoveries_predict <- "


data{

  int N; 
  int N_id;
  int N_group;
  int discoveries[N];
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

 
  matrix[4, N_id] z_ID;     
  vector<lower = 0>[4] sigma_ID;
  cholesky_factor_corr[4] Rho_ID;
  
  matrix[4, N_group] z_Group;     
  vector<lower = 0>[4] sigma_Group;
  cholesky_factor_corr[4] Rho_Group;

} 

transformed parameters {
 matrix[N_group, 4] offset_Group;
 matrix[N_id, 4] offset_ID;

//Varying effects offsets
offset_Group = (diag_pre_multiply(sigma_Group, Rho_Group) * z_Group)';
offset_ID = (diag_pre_multiply(sigma_ID, Rho_ID) * z_ID)';


} 

model{

 //Priors
 alpha ~ normal(2, 0.5);
 beta_Incentive ~ normal(0,1);
 weight ~ normal(0, 1); 

  //Define prior distribution of varying  effects
  to_vector(z_Group) ~ normal(0, 1);
  sigma_Group ~ exponential(3);
  Rho_Group ~ lkj_corr_cholesky(4);
  
    to_vector(z_ID) ~ normal(0, 1);
  sigma_ID ~ exponential(3);
  Rho_ID ~ lkj_corr_cholesky(4);
  
//Likelihoods  
for(i in 1:N){     
   if (pred[i] > -10){
      discoveries[i]  ~ poisson(exp( (alpha[Environment[i]] + offset_ID[id[i],Environment[i]] + offset_Group[group[i],Environment[i]]) +
                            beta_Incentive[Environment[i]]*Incentives[i]+
                            (weight[Environment[i]]  + offset_ID[id[i],2 + Environment[i]] + offset_Group[group[i], 2 + Environment[i]])*pred[i] )  ); 
   }
  }                                                                      
} 

"


###
##
# Discoveries
##
###

#Prepare data list
dat_disc <-          list(N = nrow(d), 
                          N_id = length(unique(d$id))  , 
                          id = d$id,
                          N_group = length(unique(d$Group))  , 
                          Environment = ifelse(d$Env == "C",1,2), 
                          Incentives = ifelse(d$Pay == "Coop",1,2),
                          group =  d$Group,
                          pred = d$discoveries,
                          coins = d$Coins )


dat_disc$Incentives <- ifelse(dat_disc$Incentives==1, 1,-1)
m <- stan(model_code = coins_predict, data = dat_disc, iter = 2000, cores = 2, chains = 2, refresh = 10, control = list(adapt_delta=0.95, max_treedepth = 13))       
s_discoveries <- extract.samples(m)


###
##
# Distance
##
###

#Prepare data list
dat_distance <-             list(N = nrow(d), 
                                 N_id = length(unique(d$id))  , 
                                 id = d$id,
                                 N_group = length(unique(d$Group))  , 
                                 Environment = ifelse(d$Env == "C",1,2), 
                                 Incentives = ifelse(d$Pay == "Coop",1,2),
                                 group =  d$Group,
                                 pred = standardize(d$MeanDistance),
                                 coins = d$Coins )


dat_distance$pred[is.na(dat_distance$pred)] <- -10



dat_distance$Incentives <- ifelse(dat_distance$Incentives==1, 1,-1)


m <- stan(model_code = coins_predict_dist, data = dat_distance, iter = 2000, cores = 2, chains = 2, refresh = 10, control = list(adapt_delta=0.95, max_treedepth = 13))       
s_distance <- extract.samples(m)



#####
####
###
##
# MOVEMENT AND INDEPENDENT DISCOVERIES
##
###
####
#####



###
##
# Sinuosity
##
###


#Prepare data list
dat_sin <-             list(N = nrow(d), 
                            N_id = length(unique(d$id))  , 
                            id = d$id,
                            N_group = length(unique(d$Group))  , 
                            Environment = ifelse(d$Env == "C",1,2), 
                            Incentives = ifelse(d$Pay == "Coop",1,2),
                            group =  d$Group,
                            pred = d$Sinuosity,
                            discoveries = d$discoveries )

dat_sin$pred <- standardize(dat_sin$pred)
dat_sin$pred[is.na(dat_sin$pred)] <- -10
dat_sin$Incentives <- ifelse(dat_sin$Incentives==1, 1,-1)
m <- stan(model_code = discoveries_predict, data = dat_sin, iter = 2000, cores = 2, chains = 2, refresh = 10)
s_sin <- extract.samples(m)


###
##
# Directional change
##
###

#Prepare data list
dat_DC <-             list(N = nrow(d), 
                           N_id = length(unique(d$id))  , 
                           id = d$id,
                           N_group = length(unique(d$Group))  , 
                           Environment = ifelse(d$Env == "C",1,2), 
                           Incentives = ifelse(d$Pay == "Coop",1,2),
                           group =  d$Group,
                           pred = d$DC,
                           discoveries = d$discoveries )

dat_DC$pred <- standardize(dat_DC$pred)
dat_DC$pred[is.na(dat_DC$pred)] <- -10
dat_DC$Incentives <- ifelse(dat_DC$Incentives==1, 1,-1)
m <- stan(model_code = discoveries_predict, data = dat_DC, iter = 2000, cores = 2, chains = 2, refresh = 10)
s_DC <- extract.samples(m)

###
##
# SD Directional change
##
###

#Prepare data list
dat_SDDC <-             list(N = nrow(d), 
                             N_id = length(unique(d$id))  , 
                             id = d$id,
                             N_group = length(unique(d$Group))  , 
                             Environment = ifelse(d$Env == "C",1,2), 
                             Incentives = ifelse(d$Pay == "Coop",1,2),
                             group =  d$Group,
                             pred = d$SDDC,
                             discoveries = d$discoveries )

dat_SDDC$pred <- standardize(dat_SDDC$pred)
dat_SDDC$pred[is.na(dat_SDDC$pred)] <- -10
dat_SDDC$Incentives <- ifelse(dat_SDDC$Incentives==1, 1,-1)

m <- stan(model_code = discoveries_predict, data = dat_SDDC, iter = 2000, cores = 2, chains = 2, refresh = 10)
s_SDDC <- extract.samples(m)


###
##
# Convex Hull
##
###

#Prepare data list
dat_Hull <-             list(N = nrow(d), 
                             N_id = length(unique(d$id))  , 
                             id = d$id,
                             N_group = length(unique(d$Group))  , 
                             Environment = ifelse(d$Env == "C",1,2), 
                             Incentives = ifelse(d$Pay == "Coop",1,2),
                             group =  d$Group,
                             pred = d$ConvexHull,
                             discoveries = d$discoveries )

dat_Hull$pred <- standardize(dat_Hull$pred)
dat_Hull$pred[is.na(dat_Hull$pred)] <- -10
dat_Hull$Incentives <- ifelse(dat_Hull$Incentives==1, 1,-1)

m <- stan(model_code = discoveries_predict, data = dat_Hull, iter = 2000, cores = 2, chains = 2, refresh = 10)
s_Hull <- extract.samples(m)


#graphics.off()
#pdf("MovementPredictorsDiscoveries.pdf", height = 6, width = 9)

par(mfrow = c(2,4), mar = c(0.5,0,0,1), oma = c(4,7,1,0))

#Concentrated
plot(dat_sin$pred[dat_sin$Environment==1], dat_sin$discoveries[dat_sin$Environment==1],  col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_sin$Incentives[dat_sin$Environment==1]==1, 1, 16), bty = "n", xlim = c(-3,3), ylim = c(0,9) , xaxt = "n")
plot_regression_line(s_sin, dat_sin$pred, 1, color = col.pal[2])
#axis(side = 1, at = seq(0.1,0.4,0.15), labels = c("0.1","0.25","0.4"))
mtext("Concentrated", side = 2, line = 3)

mean <- round(mean(s_sin$weight[,1]),2)
lower <- round(PI(s_sin$weight[,1], 0.9)[1],2)
upper <- round(PI(s_sin$weight[,1], 0.9)[2],2)
text(0, 9, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

plot(dat_DC$pred[dat_DC$Environment==1], dat_DC$discoveries[dat_DC$Environment==1],  col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_DC$Incentives[dat_DC$Environment==1]==1, 1, 16)  , bty = "n", xlim = c(-3,3), ylim = c(0,9), yaxt = "n", xaxt = "n")
plot_regression_line(s_DC, dat_DC$pred, 1, color = col.pal[2])
#axis(side = 1, at = seq(-0.6,0.2,0.4))

mean <- round(mean(s_DC$weight[,1]),2)
lower <- round(PI(s_DC$weight[,1], 0.9)[1],2)
upper <- round(PI(s_DC$weight[,1], 0.9)[2],2)
text(0, 9, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

plot(dat_SDDC$pred[dat_SDDC$Environment==1], dat_SDDC$discoveries[dat_SDDC$Environment==1],  col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_SDDC$Incentives[dat_SDDC$Environment==1]==1, 1, 16)  , bty = "n", xlim = c(-3,3), ylim = c(0,9), yaxt = "n", xaxt = "n")
plot_regression_line(s_SDDC, dat_SDDC$pred, 1, color = col.pal[2])
#axis(side = 1, at = seq(-0.3,0.1,0.2))

mean <- round(mean(s_SDDC$weight[,1]),2)
lower <- round(PI(s_SDDC$weight[,1], 0.9)[1],3)
upper <- round(PI(s_SDDC$weight[,1], 0.9)[2],3)
text(0, 9, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

plot(dat_Hull$pred[dat_Hull$Environment==1], dat_Hull$discoveries[dat_Hull$Environment==1],col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_Hull$Incentives[dat_Hull$Environment==1]==1, 1, 16)  , bty = "n", xlim = c(-3,3), ylim = c(0,9) , yaxt = "n", xaxt = "n")
plot_regression_line(s_Hull, dat_Hull$pred, 1, color = col.pal[2])
#axis(side = 1, at = seq(-0.1,0.2,0.15), labels = c("-0.1","0.05","0.2"))
legend("topright",title = "Incentives", c("Group", "Individual"), col = alpha("black", alpha = 0.6), pch = c(1,16), cex = 1.1, lwd = 1, lty = 1, bty = "n")

mean <- round(mean(s_Hull$weight[,1]),2)
lower <- round(PI(s_Hull$weight[,1], 0.9)[1],2)
upper <- round(PI(s_Hull$weight[,1], 0.9)[2],2)
text(-1.5, 9, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))


#Distributed
plot(dat_sin$pred[dat_sin$Environment==2], dat_sin$discoveries[dat_sin$Environment==2], col = alpha(col.pal[3], alpha = 0.4), pch = ifelse(dat_sin$Incentives[dat_sin$Environment==2]==1, 1, 16)  , bty = "n", xlim = c(-3,3), ylim = c(0,16) , xaxt = "n")
plot_regression_line(s_sin, dat_sin$pred, 2, color = col.pal[3])
mtext("Distributed", side = 2, line = 3)
axis(side = 1, at = seq(-3,3,2))
mtext("Sinuosity", side = 1, line = 3)

mean <- round(mean(s_sin$weight[,2]),2)
lower <- round(PI(s_sin$weight[,2], 0.9)[1],2)
upper <- round(PI(s_sin$weight[,2], 0.9)[2],2)
text(0, 16, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))


plot(dat_DC$pred[dat_DC$Environment==2], dat_DC$discoveries[dat_DC$Environment==2],  col = alpha(col.pal[3], alpha = 0.4), pch = ifelse(dat_DC$Incentives[dat_DC$Environment==2]==1, 1, 16)  , bty = "n", xlim = c(-3,3), ylim = c(0,16) , yaxt = "n", xaxt = "n")
plot_regression_line(s_DC, dat_DC$pred, 2, color = col.pal[3])
axis(side = 1, at = seq(-3,3,2))
mtext("Mean Directional Change", side = 1, line = 3)

mean <- round(mean(s_DC$weight[,2]),2)
lower <- round(PI(s_DC$weight[,2], 0.9)[1],2)
upper <- round(PI(s_DC$weight[,2], 0.9)[2],2)
text(0, 16, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

plot(dat_SDDC$pred[dat_SDDC$Environment==2], dat_SDDC$discoveries[dat_SDDC$Environment==2],col = alpha(col.pal[3], alpha = 0.4), pch = ifelse(dat_SDDC$Incentives[dat_SDDC$Environment==2]==1, 1, 16)  , bty = "n", xlim = c(-3,3), ylim = c(0,16) , yaxt = "n", xaxt = "n")
plot_regression_line(s_SDDC, dat_SDDC$pred, 2, color = col.pal[3])
axis(side = 1, at = seq(-3,3,2))
mtext("SD Directional Change", side = 1, line = 3)

mean <- round(mean(s_SDDC$weight[,2]),2)
lower <- round(PI(s_SDDC$weight[,2], 0.9)[1],3)
upper <- round(PI(s_SDDC$weight[,2], 0.9)[2],3)
text(0, 16, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))


plot(dat_Hull$pred[dat_Hull$Environment==2], dat_Hull$discoveries[dat_Hull$Environment==2], col = alpha(col.pal[3], alpha = 0.4), pch = ifelse(dat_Hull$Incentives[dat_Hull$Environment==2]==1, 1, 16)  , bty = "n", xlim = c(-3,3), ylim = c(0,16) , yaxt = "n", xaxt = "n")
plot_regression_line(s_Hull, dat_Hull$pred, 2, color = col.pal[3])
axis(side = 1, at = seq(-3,3,2))
mtext("Convex Hull Area", side = 1, line = 3)

mean <- round(mean(s_Hull$weight[,2]),2)
lower <- round(PI(s_Hull$weight[,2], 0.9)[1],2)
upper <- round(PI(s_Hull$weight[,2], 0.9)[2],2)
text(0, 16, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))
mtext("Independent Discoveries", side = 2, outer = TRUE, line = 5.5, cex = 1.3)


#dev.off()

