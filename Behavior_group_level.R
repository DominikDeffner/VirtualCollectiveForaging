
###
##
# BEHAVIORAL ANALYSES ON GROUP LEVEL
##
###

load("data/d_group")

###
##
# The first part runs analyses for comparison between experimental conditions
# The second part analyzes relation of behavioral metrics to group success
##
###


####
###
##
# Coins
##
###
####

dat <- list(N = length(d_group$joining), 
            N_id = length(unique(d_group$id))  , 
            y = d_group$Coins, 
            Environment = ifelse(d_group$Env == "C",1,2), 
            Incentives = ifelse(d_group$Pay == "Coop",1,2),
            group = d_group$id, 
            N_group = length(unique(d_group$id)))



#Multilevel lognormal regression to predict average number of coins in each condition accounting for group-level variability

lognormal_group <- "


data{

  int N; 
  int N_group;
  real y[N];
  int group[N];
  int Environment[N];
  int Incentives[N];

}

parameters{
  matrix[2,2] reward_rate; 
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
  to_vector(reward_rate) ~ normal(5, 0.5); 
  sigma ~ exponential(3);
  
    to_vector(z_Group) ~ normal(0, 1);
  sigma_Group ~ exponential(3);
  Rho_Group ~ lkj_corr_cholesky(4);
  

//Likelihoods  
for(i in 1:N){                                                        
      y[i]  ~ lognormal(reward_rate[Incentives[i],  Environment[i]] + offset_Group[group[i], Environment[i]], sigma); 
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


m_coins <- stan(model_code = lognormal_group, data = dat, iter = 4000, cores = 4, chains = 4, refresh = 10, control = list(adapt_delta = 0.9, max_treedepth = 12))
s_coins <- extract.samples(m_coins)


####
###
##
# Discoveries, Joinings and Scrounging
##
###
####


missing <- which(is.na(d_group$observed))
dat <- list(N = length(d_group$joining[-missing]), 
            joining = d_group$joining[-missing], 
            discoveries = d_group$discoveries[-missing], 
            observed = d_group$observed[-missing],
            visibility = d_group$Visibility[-missing], 
            dist = d_group$Distance[-missing],
            Environment = ifelse(d_group$Env[-missing] == "C",1,2), 
            Incentives = ifelse(d_group$Pay[-missing] == "Coop",1,2),
            group = d_group$id[-missing], 
            N_group = length(unique(d_group$id[-missing])))



#Multilevel lognormal regression to predict average number of discoveries/joinings in each condition accounting for group-level variability

lognormal_rates <- "


data{

  int N; 
  real joining[N];
  real discoveries[N];
  real observed[N];
  int Environment[N];
  int Incentives[N];
  int N_group;
  int group[N];

}

parameters{
  matrix[2,2] discovery_rate; 
  matrix[2,2] joining_rate; 
  matrix[2,2] observation_rate; 

  real<lower = 0>sigma_joining; 
  real<lower = 0>sigma_discoveries; 
  real<lower = 0>sigma_observed; 

  matrix[6, N_group] z_Group;     
  vector<lower = 0>[6] sigma_Group;
  cholesky_factor_corr[6] Rho_Group;

} 

transformed parameters {
 matrix[N_group, 6] offset_Group;
 
//Varying effects offsets
offset_Group = (diag_pre_multiply(sigma_Group, Rho_Group) * z_Group)';
}


model{

  //Priors
  to_vector(discovery_rate) ~ normal(2, 0.5); 
  to_vector(joining_rate) ~ normal(2, 0.5); 
  to_vector(observation_rate) ~ normal(2, 0.5); 

  sigma_joining ~ exponential(3);
  sigma_discoveries ~ exponential(3);
  sigma_observed ~ exponential(3);

  to_vector(z_Group) ~ normal(0, 1);
  sigma_Group ~ exponential(3);
  Rho_Group ~ lkj_corr_cholesky(4);
  

//Likelihoods  
for(i in 1:N){                                                        
      joining[i]     ~ lognormal(joining_rate[Incentives[i],     Environment[i]] + offset_Group[group[i], Environment[i]], sigma_joining); 
      discoveries[i] ~ lognormal(discovery_rate[Incentives[i],   Environment[i]] + offset_Group[group[i],2+ Environment[i]], sigma_discoveries); 
      observed[i]    ~ lognormal(observation_rate[Incentives[i], Environment[i]] + offset_Group[group[i],4+ Environment[i]], sigma_observed); 
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
        observed_pred[i,j] = exp(joining_rate[i,j])/exp(observation_rate[i,j]); 

      }                            
    }  

}

"


m_rates <- stan(model_code = lognormal_rates, data = dat, iter = 4000, cores = 4, chains = 4, refresh = 10, control = list(adapt_delta = 0.9, max_treedepth = 12))
s_rates <- extract.samples(m_rates)




####
###
##
# Distance
##
###
####


#Multilevel lognormal regression to predict average distance among players in each condition accounting for group-level variability

lognormal_distance <- "

data{

  int N; 
  real dist[N];
  int Environment[N];
  int Incentives[N];
    int N_group;
  int group[N];

}

parameters{
  matrix[2,2] mean_distance; 
  real<lower=0>sigma_distance; 

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
  to_vector(mean_distance) ~ normal(5, 0.5); 
  sigma_distance ~ exponential(2); 

  to_vector(z_Group) ~ normal(0, 1);
  sigma_Group ~ exponential(3);
  Rho_Group ~ lkj_corr_cholesky(4);
  
//Likelihoods  
for(i in 1:N){                                                        
      dist[i] ~ lognormal(mean_distance[Incentives[i],  Environment[i]] + offset_Group[group[i], Environment[i]] , sigma_distance ); 
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

m_distance <- stan(model_code = lognormal_distance, data = dat, iter = 4000, cores = 4, chains = 4, refresh = 10, control = list(adapt_delta = 0.9, max_treedepth = 12))
s_distance <- extract.samples(m_distance)

####
###
##
# Visibility
##
###
####


#Multilevel Gaussian regression to predict average visibility among players in each condition accounting for group-level variability

normal_visibility <- "

data{

  int N; 
  real visibility[N];
  int Environment[N];
  int Incentives[N];
      int N_group;
  int group[N];


}

parameters{
  matrix[2,2] mean_visibility; 
  real<lower=0>sigma_visibility; 

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
  to_vector(mean_visibility) ~ normal(1, 0.5); 
  sigma_visibility ~ exponential(2); 

  to_vector(z_Group) ~ normal(0, 1);
  sigma_Group ~ exponential(3);
  Rho_Group ~ lkj_corr_cholesky(4);
  

//Likelihoods  
for(i in 1:N){                                                        
      visibility[i] ~ normal(mean_visibility[Incentives[i],  Environment[i]] + offset_Group[group[i], Environment[i]]  , sigma_visibility ); 
  }                                                                      
} 

"

m_visibility <- stan(model_code = normal_visibility, data = dat, iter = 4000, cores = 4, chains = 4, refresh = 10, control = list(adapt_delta = 0.9, max_treedepth = 12))
s_visibility <- extract.samples(m_visibility)


#####
###
##
# Density
##
###
####

dat <- list(N = length(d_group$density), 
            density = d_group$density, 
            Environment = ifelse(d_group$Env == "C",1,2), 
            Incentives = ifelse(d_group$Pay == "Coop",1,2),
            group = d_group$id, 
            N_group = length(unique(d_group$id)))


#Multilevel Gaussian regression to predict average density (i.e., number of players) at exploited patches in each condition accounting for group-level variability

normal_density <- "

data{

  int N; 
  real density[N];
  int Environment[N];
  int Incentives[N];
  int N_group;
  int group[N];


}

parameters{
  matrix[2,2] mean_dens; 
  real<lower=0>sigma_dens; 

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
  to_vector(mean_dens) ~ normal(1, 0.5); 
  sigma_dens ~ exponential(2); 

  to_vector(z_Group) ~ normal(0, 1);
  sigma_Group ~ exponential(3);
  Rho_Group ~ lkj_corr_cholesky(4);
  

//Likelihoods  
for(i in 1:N){                                                        
      density[i] ~ normal(mean_dens[Incentives[i],  Environment[i]] + offset_Group[group[i], Environment[i]]  , sigma_dens ); 
  }                                                                      
} 

"

m_density <- stan(model_code = normal_density, data = dat, iter = 4000, cores = 4, chains = 4, refresh = 10, control = list(adapt_delta = 0.9, max_treedepth = 12))
s_density <- extract.samples(m_density)



###
##
# Create behavioral plot (S3 in ESM)
##
###


behavioral_plotting_fct <- function(xseq = c(1.5, 2, 2.75, 3.25),
                                    
                                    ylim = c(60,170),
                                    
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
  
  
  plot(1:4,xlim = c(1.25,3.5), ylim = ylim, type = "n", xaxt = "n", xlab = "", ylab = "", bty = "n")
  axis(side = 1, at = c(1.75, 3), labels = c("Group Incentives", "Individual Incentives"), col = "white", cex = 1.1 )
  mtext(side = 2, ylab, line = 2.5, cex = 0.75)
  ##
  #Cooperative
  ##
  
  x_coords_con  <- rep(xseq[1],length(con_coop))+jitter(rep(0,length(con_coop)),9)
  x_coords_dist <- rep(xseq[2],length(dist_coop))+jitter(rep(0,length(dist_coop)),9)
  
  
  #Concentrated
  points(x=x_coords_con,y=con_coop, col = alpha(col.pal[2], alpha = 0.5))
  points(xseq[1], con_coop_mean, pch = 16, cex = 2, col = alpha(col.pal[2], alpha = 1))
  segments(xseq[1],con_coop_hpdi[1],xseq[1],con_coop_hpdi[2], col = alpha(col.pal[2], alpha = 1), lwd = 2)
  
  
  #Distributed
  points(x=x_coords_dist,y=dist_coop, col = alpha(col.pal[3], alpha = 0.5))
  points(xseq[2], dist_coop_mean, pch = 16, cex = 2, col = alpha(col.pal[3], alpha = 1))
  segments(xseq[2],dist_coop_hpdi[1],xseq[2],dist_coop_hpdi[2], col = alpha(col.pal[3], alpha = 1), lwd = 2)
  
  ##
  #Competitive
  ##
  x_coords_con  <- rep(xseq[3],length(con_comp))+jitter(rep(0,length(con_comp)),9)
  x_coords_dist <- rep(xseq[4],length(dist_comp))+jitter(rep(0,length(dist_comp)),9)
  
  #Concentrated
  points(x=x_coords_con,y=con_comp, col = alpha(col.pal[2], alpha = 0.5))
  points(xseq[3], con_comp_mean, pch = 16, cex = 2, col = alpha(col.pal[2], alpha = 1))
  segments(xseq[3],con_comp_hpdi[1],xseq[3],con_comp_hpdi[2], col = alpha(col.pal[2], alpha = 1), lwd = 2)
  
  #Distributed
  points(x=x_coords_dist,y=dist_comp, col = alpha(col.pal[3], alpha = 0.5))
  points(xseq[4], dist_comp_mean, pch = 16, cex = 2, col = alpha(col.pal[3], alpha = 1))
  segments(xseq[4],dist_comp_hpdi[1],xseq[4],dist_comp_hpdi[2], col = alpha(col.pal[3], alpha = 1), lwd = 2)
  
}


#graphics.off()
#pdf("BehavioralGroup.pdf", height = 5, width = 8)

par(mfrow = c(2,3),
    mar = c(3,3, 1,0.5), 
    oma = c(0,0.5,0,0))


#Coins
dat <- list(N = length(d_group$joining), 
            coins = d_group$Coins, 
            Environment = ifelse(d_group$Env == "C",1,2), 
            Incentives = ifelse(d_group$Pay == "Coop",1,2),
            id = d_group$id )

behavioral_plotting_fct()

#Rates
missing <- which(is.na(d_group$observed))
dat <- list(N = length(d_group$joining[-missing]), 
            joining = d_group$joining[-missing], 
            discoveries = d_group$discoveries[-missing], 
            observed = d_group$exploit_observed[-missing],
            visibility = d_group$Visibility[-missing], 
            distance = d_group$MeanDistance[-missing],
            Environment = ifelse(d_group$Env[-missing] == "C",1,2), 
            Incentives = ifelse(d_group$Pay[-missing] == "Coop",1,2),
            group = d_group$Group[-missing], 
            id = d_group$id[-missing] )

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




# Distance

missing <- which(is.na(d_group$Distance))
dat <- list(N = length(d_group$Distance[-missing]), 
            visibility = d_group$Visibility[-missing], 
            dist = d_group$Distance[-missing], 
            Environment = ifelse(d_group$Env[-missing] == "C",1,2), 
            Incentives = ifelse(d_group$Pay[-missing] == "Coop",1,2),
            group = d_group$Group[-missing], 
            id = d_group$id[-missing] )

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

missing <- which(is.na(d_group$Visibility))
dat <- list(N = length(d_group$Visibility[-missing]), 
            N_id = length(unique(d_group$id[-missing]))  , 
            N_group = length(unique(d_group$Group[-missing]))  , 
            visibility = d_group$Visibility[-missing], 
            dist = d_group$MeanDistance[-missing], 
            Environment = ifelse(d_group$Env[-missing] == "C",1,2), 
            Incentives = ifelse(d_group$Pay[-missing] == "Coop",1,2),
            group = d_group$Group[-missing], 
            id = d_group$id[-missing] )


behavioral_plotting_fct(            ylim = c(0.7,1.2),
                                    
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


# Density

dat <- list(N = length(d_group$density), 
            density = d_group$density, 
            Environment = ifelse(d_group$Env == "C",1,2), 
            Incentives = ifelse(d_group$Pay == "Coop",1,2),
            id = d_group$id)




behavioral_plotting_fct(            ylim = c(1,4),
                                    
                                    ylab = "Density",
                                    
                                    con_coop   =  dat$density[dat$Environment==1 & dat$Incentives==1],
                                    con_coop_ids  =  dat$id[dat$Environment==1 & dat$Incentives==1],
                                    con_coop_mean =  mean(s_density$mean_dens[,1,1]),
                                    con_coop_hpdi  =  HPDI(s_density$mean_dens[,1,1]),
                                    
                                    dist_coop  =  dat$density[dat$Environment==2 & dat$Incentives==1],
                                    dist_coop_ids   =  dat$id[dat$Environment==2 & dat$Incentives==1],
                                    dist_coop_mean  =  mean(s_density$mean_dens[,1,2]),
                                    dist_coop_hpdi  =  HPDI(s_density$mean_dens[,1,2]),
                                    
                                    con_comp  =  dat$density[dat$Environment==1 & dat$Incentives==2],
                                    con_comp_ids   =  dat$id[dat$Environment==1 & dat$Incentives==2],
                                    con_comp_mean  =  mean(s_density$mean_dens[,2,1]),
                                    con_comp_hpdi =  HPDI(s_density$mean_dens[,2,1]),
                                    
                                    dist_comp  =  dat$density[dat$Environment==2 & dat$Incentives==2],
                                    dist_comp_ids  =  dat$id[dat$Environment==2 & dat$Incentives==2],
                                    dist_comp_mean  =  mean(s_density$mean_dens[,2,2]),
                                    dist_comp_hpdi  = HPDI(s_density$mean_dens[,2,2]))


#dev.off()






####
###
##
# GROUP OUTCOMES
##
###
####



coins_predict_group <- "

data{

  int N; 
  real coins[N];
  real pred[N];
  int Environment[N];
  int Incentives[N];
  int group[N];
  int N_group;

}

parameters{

  //Intercepts for environments
  vector[2] alpha; 
  
  //Offsets for incentives (coded as -1 and 1, so alpha represents mean across conditions)
  vector[2] beta_Incentive; 

  //Regression weights of predictor
  vector[2] weight; 
  real<lower = 0>sigma; 

  matrix[3, N_group] z_Group;     
  vector<lower = 0>[3] sigma_Group;
  cholesky_factor_corr[3] Rho_Group;

} 

transformed parameters {
 matrix[N_group, 3] offset_Group;

//Varying effects offsets
offset_Group = (diag_pre_multiply(sigma_Group, Rho_Group) * z_Group)';


} 

model{

 //Priors
 alpha ~ normal(5, 0.5);
 beta_Incentive ~ normal(0,1);
 weight ~ normal(0, 1); 
 sigma ~ exponential(3); 
 
  //Define prior distribution of varying  effects
  to_vector(z_Group) ~ normal(0, 1);
  sigma_Group ~ exponential(1);
  Rho_Group ~ lkj_corr_cholesky(4);


//Likelihoods  
for(i in 1:N){
   if (pred[i] > -10){
      coins[i]  ~ lognormal( (alpha[Environment[i]]+ offset_Group[group[i],1]) + 
                     beta_Incentive[Environment[i]]*Incentives[i] +
                     (weight[Environment[i]] + offset_Group[group[i],2])*pred[i], sigma  ); 
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
dat_disc <-             list(N = nrow(d_group), 
                             N_group = length(unique(d_group$id))  , 
                             group = d_group$id,
                             Environment = ifelse(d_group$Env == "C",1,2), 
                             Incentives = ifelse(d_group$Pay == "Coop",1,2),
                             pred = d_group$discoveries,
                             coins = d_group$Coins )

dat_disc$Incentives <- ifelse(dat_disc$Incentives==1, 1,-1)
m <- stan(model_code = coins_predict_group, data = dat_disc, iter = 2000, cores = 2, chains = 2, refresh = 10)
s_discoveries <- extract.samples(m)

###
##
# Joining
##
###

#Prepare data list

dat_join <-             list(N = nrow(d_group), 
                             N_group = length(unique(d_group$id))  , 
                             group = d_group$id,
                             Environment = ifelse(d_group$Env == "C",1,2), 
                             Incentives = ifelse(d_group$Pay == "Coop",1,2),
                             pred = d_group$joining,
                             coins = d_group$Coins )

dat_join$Incentives <- ifelse(dat_join$Incentives==1, 1,-1)
m <- stan(model_code = coins_predict_group, data = dat_join, iter = 2000, cores = 2, chains = 2, refresh = 10)
s_joining <- extract.samples(m)

###
##
# Distance
##
###

#Prepare data list
dat_dist <-             list(N = nrow(d_group), 
                             N_group = length(unique(d_group$id))  , 
                             group = d_group$id,
                             Environment = ifelse(d_group$Env == "C",1,2), 
                             Incentives = ifelse(d_group$Pay == "Coop",1,2),
                             pred = d_group$Distance,
                             coins = d_group$Coins )

dat_dist$Incentives <- ifelse(dat_dist$Incentives==1, 1,-1)
dat_dist$pred[is.na(dat_dist$pred)] <- -10
m <- stan(model_code = coins_predict_group, data = dat_dist, iter = 2000, cores = 2, chains = 2, refresh = 10)
s_distance <- extract.samples(m)

###
##
# Visibility
##
###

#Prepare data list
dat_vis <-             list(N = nrow(d_group), 
                            N_group = length(unique(d_group$id))  , 
                            group = d_group$id,
                            Environment = ifelse(d_group$Env == "C",1,2), 
                            Incentives = ifelse(d_group$Pay == "Coop",1,2),
                            pred = d_group$Visibility,
                            coins = d_group$Coins )

dat_vis$Incentives <- ifelse(dat_vis$Incentives==1, 1,-1)
dat_vis$pred[is.na(dat_vis$pred)] <- -10
m <- stan(model_code = coins_predict_group, data = dat_vis, iter = 2000, cores = 2, chains =2, refresh = 10)
s_visibility <- extract.samples(m)

###
##
# Density
##
###

#Prepare data list
dat_dens <-             list(N = nrow(d_group), 
                             N_group = length(unique(d_group$id))  , 
                             group = d_group$id,
                             Environment = ifelse(d_group$Env == "C",1,2), 
                             Incentives = ifelse(d_group$Pay == "Coop",1,2),
                             pred = d_group$density,
                             coins = d_group$Coins )

dat_dens$Incentives <- ifelse(dat_dens$Incentives==1, 1,-1)
dat_dens$pred[is.na(dat_dens$pred)] <- -10
m <- stan(model_code = coins_predict_group, data = dat_dens, iter = 2000, cores = 2, chains = 2, refresh = 10)
s_dens <- extract.samples(m)


#graphics.off()
#pdf("BehavioralPredictorsCoinsGroup.pdf", height = 6, width = 10)

par(mfrow = c(2,5), mar = c(0.5,0,0,1), oma = c(4,6,1,0))

#Concentrated
plot(dat_disc$pred[dat_disc$Environment==1], dat_disc$coins[dat_disc$Environment==1],  col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_disc$Incentives[dat_disc$Environment==1]==1, 1, 16), bty = "n", xlim = c(0,12), ylim = c(50,200) , xaxt = "n")
plot_regression_line(s_discoveries, dat_disc$pred, 1, color = col.pal[2])
mtext("Concentrated", side = 2, line = 3)

mean <- round(mean(s_discoveries$weight[,1]),2)
lower <- round(PI(s_discoveries$weight[,1], 0.9)[1],2)
upper <- round(PI(s_discoveries$weight[,1], 0.9)[2],2)
text(5, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))


plot(dat_join$pred[dat_join$Environment==1], dat_join$coins[dat_join$Environment==1],  col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_join$Incentives[dat_join$Environment==1]==1, 1, 16)  , bty = "n", xlim = c(0,10), ylim = c(50,200), yaxt = "n", xaxt = "n")
plot_regression_line(s_joining, dat_join$pred, 1, color = col.pal[2])

mean <- round(mean(s_joining$weight[,1]),2)
lower <- round(PI(s_joining$weight[,1], 0.9)[1],4)
upper <- round(PI(s_joining$weight[,1], 0.9)[2],4)
text(5, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

plot(dat_dist$pred[dat_dist$Environment==1], dat_dist$coins[dat_dist$Environment==1],  col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_dist$Incentives[dat_dist$Environment==1]==1, 1, 16)  , bty = "n", xlim = c(10,50), ylim = c(50,200), yaxt = "n", xaxt = "n")
plot_regression_line(s_distance, dat_dist$pred, 1, color = col.pal[2])

mean <- round(mean(s_distance$weight[,1]),2)
lower <- round(PI(s_distance$weight[,1], 0.9)[1],3)
upper <- round(PI(s_distance$weight[,1], 0.9)[2],3)
text(25, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))


plot(dat_vis$pred[dat_vis$Environment==1], dat_vis$coins[dat_vis$Environment==1],col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_vis$Incentives[dat_vis$Environment==1]==1, 1, 16)  , bty = "n", xlim = c(0.8,1.2), ylim = c(50,200) , yaxt = "n", xaxt = "n")
plot_regression_line(s_visibility, dat_vis$pred, 1, color = col.pal[2])

mean <- round(mean(s_visibility$weight[,1]),2)
lower <- round(PI(s_visibility$weight[,1], 0.9)[1],2)
upper <- round(PI(s_visibility$weight[,1], 0.9)[2],2)
text(1, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

plot(dat_dens$pred[dat_dens$Environment==1], dat_dens$coins[dat_dens$Environment==1],col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_dens$Incentives[dat_dens$Environment==1]==1, 1, 16)  , bty = "n", xlim = c(1,4), ylim = c(50,200) , yaxt = "n", xaxt = "n")
plot_regression_line(s_dens, dat_dens$pred, 1, color = col.pal[2])
legend("topright",title = "Incentives", c("Group", "Individual"), col = alpha("black", alpha = 0.6), pch = c(1,16), cex = 1.1, lwd = 1, lty = 1, bty = "n")

mean <- round(mean(s_dens$weight[,1]),2)
lower <- round(PI(s_dens$weight[,1], 0.9)[1],2)
upper <- round(PI(s_dens$weight[,1], 0.9)[2],2)
text(1.75, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 0.9, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

#Distributed
plot(dat_disc$pred[dat_disc$Environment==2], dat_disc$coins[dat_disc$Environment==2], col = alpha(col.pal[3], alpha = 0.4), pch = ifelse(dat_disc$Incentives[dat_disc$Environment==2]==1, 1, 16)  , bty = "n", xlim = c(0,12), ylim = c(50,200) , xaxt = "n")
plot_regression_line(s_discoveries, dat_disc$pred, 2, color = col.pal[3])
mtext("Distributed", side = 2, line = 3)
axis(side = 1, at = seq(0,12,6))
mtext("Independent Discoveries", side = 1, line = 3)

mean <- round(mean(s_discoveries$weight[,2]),2)
lower <- round(PI(s_discoveries$weight[,2], 0.9)[1],2)
upper <- round(PI(s_discoveries$weight[,2], 0.9)[2],2)
text(5, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

plot(dat_join$pred[dat_join$Environment==2], dat_join$coins[dat_join$Environment==2],  col = alpha(col.pal[3], alpha = 0.4), pch = ifelse(dat_join$Incentives[dat_join$Environment==2]==1, 1, 16)  , bty = "n", xlim = c(0,10), ylim = c(50,200) , yaxt = "n", xaxt = "n")
plot_regression_line(s_joining, dat_join$pred, 2, color = col.pal[3])
axis(side = 1, at = seq(0,10,5))
mtext("Patch Joinings", side = 1, line = 3)

mean <- round(mean(s_joining$weight[,2]),2)
lower <- round(PI(s_joining$weight[,2], 0.9)[1],2)
upper <- round(PI(s_joining$weight[,2], 0.9)[2],2)
text(5, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

plot(dat_dist$pred[dat_dist$Environment==2], dat_dist$coins[dat_dist$Environment==2],col = alpha(col.pal[3], alpha = 0.4), pch = ifelse(dat_dist$Incentives[dat_dist$Environment==2]==1, 1, 16)  , bty = "n", xlim = c(10,50), ylim = c(50,200) , yaxt = "n", xaxt = "n")
plot_regression_line(s_distance, dat_dist$pred, 2, color = col.pal[3])
axis(side = 1, at = seq(10,50,20))
mtext("Average Distance", side = 1, line = 3)

mean <- round(mean(s_distance$weight[,2]),2)
lower <- round(PI(s_distance$weight[,2], 0.9)[1],3)
upper <- round(PI(s_distance$weight[,2], 0.9)[2],3)
text(25, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

plot(dat_vis$pred[dat_vis$Environment==2], dat_vis$coins[dat_vis$Environment==2], col = alpha(col.pal[3], alpha = 0.4), pch = ifelse(dat_vis$Incentives[dat_vis$Environment==2]==1, 1, 16)  , bty = "n", xlim = c(0.8,1.2), ylim = c(50,200) , yaxt = "n", xaxt = "n")
plot_regression_line(s_visibility, dat_vis$pred, 2, color = col.pal[3])
axis(side = 1, at = seq(0.8,1.2, 0.2))
mtext("Average Visibility", side = 1, line = 3)

mean <- round(mean(s_visibility$weight[,2]),2)
lower <- round(PI(s_visibility$weight[,2], 0.9)[1],2)
upper <- round(PI(s_visibility$weight[,2], 0.9)[2],2)
text(1, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

plot(dat_dens$pred[dat_dens$Environment==2], dat_dens$coins[dat_dens$Environment==2],col = alpha(col.pal[3], alpha = 0.4), pch = ifelse(dat_dens$Incentives[dat_dens$Environment==2]==1, 1, 16)  , bty = "n", xlim = c(1,4), ylim = c(50,200) , yaxt = "n", xaxt = "n")
plot_regression_line(s_dens, dat_dens$pred, 2, color = col.pal[3])
axis(side = 1, at = seq(1,4,length.out = 3), labels = c("1","2.5","4"))
mtext("Density", side = 1, line = 3)

mean <- round(mean(s_dens$weight[,2]),2)
lower <- round(PI(s_dens$weight[,2], 0.9)[1],2)
upper <- round(PI(s_dens$weight[,2], 0.9)[2],2)
text(2, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))
mtext("Coins", side = 2, outer = TRUE, line = 4.5, cex = 1.3)

#dev.off()
