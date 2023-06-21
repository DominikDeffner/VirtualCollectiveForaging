

load("data/d_rounds")



####
###
##
# INDIVIDUAL OUTCOMES
##
###
####




{
  
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



coins_predict_dist_vis <- "


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



multiple_coins_predict <- "


data{

  int N; 
  int N_id;
  int N_group;
  int coins[N];
  int pred1[N];
  int pred2[N];
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
  vector[2] weight1; 
  vector[2] weight2; 

  matrix[6, N_id] z_ID;     
  vector<lower = 0>[6] sigma_ID;
  cholesky_factor_corr[6] Rho_ID;
  
  matrix[6, N_group] z_Group;     
  vector<lower = 0>[6] sigma_Group;
  cholesky_factor_corr[6] Rho_Group;

} 

transformed parameters {
 matrix[N_group, 6] offset_Group;
 matrix[N_id, 6] offset_ID;

//Varying effects offsets
offset_Group = (diag_pre_multiply(sigma_Group, Rho_Group) * z_Group)';
offset_ID = (diag_pre_multiply(sigma_ID, Rho_ID) * z_ID)';


} 

model{

 //Priors
 alpha ~ normal(5, 0.5);
 beta_Incentive ~ normal(0,1);
 weight1 ~ normal(0, 1); 
 weight2 ~ normal(0, 1); 


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
                            (weight1[Environment[i]]  + offset_Group[group[i],2 + Environment[i]] + offset_ID[id[i],2 + Environment[i]])*pred1[i] + 
                            (weight2[Environment[i]]  + offset_Group[group[i],4 + Environment[i]] + offset_ID[id[i],4 + Environment[i]])*pred2[i]) );
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





}


###
##
# Discoveries
##
###

library(rstan)

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
save(s_discoveries, file = "s_discoveries")




###
##
# Joining
##
###


#Prepare data list
dat_join <- list(N = nrow(d), 
                 N_id = length(unique(d$id))  , 
                 id = d$id,
                 N_group = length(unique(d$Group))  , 
                 Environment = ifelse(d$Env == "C",1,2), 
                 Incentives = ifelse(d$Pay == "Coop",1,2),
                 group =  d$Group,
                 pred = d$joining,
                 coins = d$Coins )







dat_join$Incentives <- ifelse(dat_join$Incentives==1, 1,-1)


m <- stan(model_code = coins_predict, data = dat_join, iter = 2000, cores = 2, chains = 2, refresh = 10, control = list(adapt_delta=0.95, max_treedepth = 13))       
s_joining <- extract.samples(m)
save(s_joining, file = "s_joining")



###
##
# Discoveries and Joining Multiple Regression
##
###


#Prepare data list
dat_both <-             list(N = nrow(d), 
                                        N_id = length(unique(d$id))  , 
                                        id = d$id,
                                        N_group = length(unique(d$Group))  , 
                                        Environment = ifelse(d$Env == "C",1,2), 
                                        Incentives = ifelse(d$Pay == "Coop",1,2),
                                        group =  d$Group,
                                        pred1 = d$discoveries,
                                        pred2 = d$joining,
                                        coins = d$Coins )






dat_both$Incentives <- ifelse(dat_both$Incentives==1, 1,-1)


m <- stan(model_code = multiple_coins_predict, data = dat_both, iter = 2000, cores = 2, chains = 2, refresh = 10, control = list(adapt_delta=0.95, max_treedepth = 13))       
s_both <- extract.samples(m)
save(s_both, file = "s_both")



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


m <- stan(model_code = coins_predict_dist_vis, data = dat_distance, iter = 2000, cores = 2, chains = 2, refresh = 10, control = list(adapt_delta=0.95, max_treedepth = 13))       
s_distance <- extract.samples(m)
save(s_distance, file = "s_distance")


###
##
# Visibility
##
###

library(rstan)

dat_vis <-             list(N = nrow(d), 
                                 N_id = length(unique(d$id))  , 
                                 id = d$id,
                                 N_group = length(unique(d$Group))  , 
                                 Environment = ifelse(d$Env == "C",1,2), 
                                 Incentives = ifelse(d$Pay == "Coop",1,2),
                                 group =  d$Group,
                                 pred = d$Visibility,
                                 coins = d$Coins )


dat_vis$pred[is.na(dat_vis$pred)] <- -10



dat_vis$Incentives <- ifelse(dat_vis$Incentives==1, 1,-1)


m <- stan(model_code = coins_predict_dist_vis, data = dat_vis, iter = 2000, cores = 2, chains = 2, refresh = 10, control = list(adapt_delta=0.95, max_treedepth = 13))       


s_Visibility <- extract.samples(m)
save(s_Visibility, file = "s_Visibility")



load("s_discoveries")
load("s_joining")
load("s_distance")
load("s_Visibility")



graphics.off()
pdf("BehavioralPredictorsCoins.pdf", height = 6, width = 9)

par(mfrow = c(2,4), mar = c(0.5,0,0,1), oma = c(4,6,1,0))

#Concentrated
plot(dat_disc$pred[dat_disc$Environment==1], dat_disc$coins[dat_disc$Environment==1],  col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_disc$Incentives[dat_disc$Environment==1]==1, 1, 16), bty = "n", xlim = c(0,15), ylim = c(30,230) , xaxt = "n")
plot_regression_line(s_discoveries, dat_disc$pred, 1, color = col.pal[2])
#axis(side = 1, at = seq(0.1,0.4,0.15), labels = c("0.1","0.25","0.4"))
mtext("Concentrated", side = 2, line = 3)

mean <- round(mean(s_discoveries$weight[,1]),2)
lower <- round(PI(s_discoveries$weight[,1], 0.9)[1],2)
upper <- round(PI(s_discoveries$weight[,1], 0.9)[2],2)
text(5, 220, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))


plot(dat_join$pred[dat_join$Environment==1], dat_join$coins[dat_join$Environment==1],  col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_join$Incentives[dat_join$Environment==1]==1, 1, 16)  , bty = "n", xlim = c(0,15), ylim = c(30,230), yaxt = "n", xaxt = "n")
plot_regression_line(s_joining, dat_join$pred, 1, color = col.pal[2])
#axis(side = 1, at = seq(-0.6,0.2,0.4))

mean <- round(mean(s_joining$weight[,1]),2)
lower <- round(PI(s_joining$weight[,1], 0.9)[1],2)
upper <- round(PI(s_joining$weight[,1], 0.9)[2],2)
text(5, 220, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))



plot(dat_distance$pred[dat_distance$Environment==1], dat_distance$coins[dat_distance$Environment==1],  col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_distance$Incentives[dat_distance$Environment==1]==1, 1, 16)  , bty = "n", xlim = c(-3,3), ylim = c(30,230), yaxt = "n", xaxt = "n")
plot_regression_line(s_distance, dat_distance$pred[-which(dat_distance$pred==-10)], 1, color = col.pal[2])
#axis(side = 1, at = seq(-0.3,0.1,0.2))

mean <- round(mean(s_distance$weight[,1]),2)
lower <- round(PI(s_distance$weight[,1], 0.9)[1],2)
upper <- round(PI(s_distance$weight[,1], 0.9)[2],2)
text(0, 220, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))


plot(dat_vis$pred[dat_vis$Environment==1], dat_vis$coins[dat_vis$Environment==1],col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_vis$Incentives[dat_vis$Environment==1]==1, 1, 16)  , bty = "n", xlim = c(0.5,2), ylim = c(30,230) , yaxt = "n", xaxt = "n")
plot_regression_line(s_Visibility, dat_vis$pred[-which(dat_vis$pred==-10)], 1, color = col.pal[2])
#axis(side = 1, at = seq(-0.1,0.2,0.15), labels = c("-0.1","0.05","0.2"))
legend("topright",title = "Incentives", c("Group", "Individual"), col = alpha("black", alpha = 0.6), pch = c(1,16), cex = 1.1, lwd = 1, lty = 1, bty = "n")

mean <- round(mean(s_Visibility$weight[,1]),2)
lower <- round(PI(s_Visibility$weight[,1], 0.9)[1],2)
upper <- round(PI(s_Visibility$weight[,1], 0.9)[2],2)
text(0.9, 220, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))






#Distributed
plot(dat_disc$pred[dat_disc$Environment==2], dat_disc$coins[dat_disc$Environment==2], col = alpha(col.pal[3], alpha = 0.4), pch = ifelse(dat_disc$Incentives[dat_disc$Environment==2]==1, 1, 16)  , bty = "n", xlim = c(0,15), ylim = c(30,230) , xaxt = "n")
plot_regression_line(s_discoveries, dat_disc$pred, 2, color = col.pal[3])
mtext("Distributed", side = 2, line = 3)
axis(side = 1, at = seq(0,15,7.5), labels = c("0","7.5","15"))
mtext("Independent Discoveries", side = 1, line = 3)

mean <- round(mean(s_discoveries$weight[,2]),2)
lower <- round(PI(s_discoveries$weight[,2], 0.9)[1],2)
upper <- round(PI(s_discoveries$weight[,2], 0.9)[2],2)
text(5, 220, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))


plot(dat_join$pred[dat_join$Environment==2], dat_join$coins[dat_join$Environment==2],  col = alpha(col.pal[3], alpha = 0.4), pch = ifelse(dat_join$Incentives[dat_join$Environment==2]==1, 1, 16)  , bty = "n", xlim = c(0,15), ylim = c(30,230) , yaxt = "n", xaxt = "n")
plot_regression_line(s_joining, dat_join$pred, 2, color = col.pal[3])
axis(side = 1, at = seq(0,15,7.5), labels = c("0","7.5","15"))
mtext("Patch Joinings", side = 1, line = 3)

mean <- round(mean(s_joining$weight[,2]),2)
lower <- round(PI(s_joining$weight[,2], 0.9)[1],2)
upper <- round(PI(s_joining$weight[,2], 0.9)[2],2)
text(5, 220, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))



plot(dat_distance$pred[dat_distance$Environment==2], dat_distance$coins[dat_distance$Environment==2],col = alpha(col.pal[3], alpha = 0.4), pch = ifelse(dat_distance$Incentives[dat_distance$Environment==2]==1, 1, 16)  , bty = "n", xlim = c(-3,3), ylim = c(30,230) , yaxt = "n", xaxt = "n")
plot_regression_line(s_distance, dat_distance$pred[-which(dat_distance$pred==-10)], 2, color = col.pal[3])
axis(side = 1, at = seq(-3,3,3))
mtext("(Stand.) Distance", side = 1, line = 3)

mean <- round(mean(s_distance$weight[,2]),2)
lower <- round(PI(s_distance$weight[,2], 0.9)[1],4)
upper <- round(PI(s_distance$weight[,2], 0.9)[2],4)
text(0, 220, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))


plot(dat_vis$pred[dat_vis$Environment==2], dat_vis$coins[dat_vis$Environment==2], col = alpha(col.pal[3], alpha = 0.4), pch = ifelse(dat_vis$Incentives[dat_vis$Environment==2]==1, 1, 16)  , bty = "n", xlim = c(0.5,2), ylim = c(30,230) , yaxt = "n", xaxt = "n")
plot_regression_line(s_Visibility, dat_vis$pred[-which(dat_vis$pred==-10)], 2, color = col.pal[3])
axis(side = 1, at = seq(0.5,2,0.75), labels = c("0.5","1.25","2"))
mtext("Average Visibility", side = 1, line = 3)

mean <- round(mean(s_Visibility$weight[,2]),2)
lower <- round(PI(s_Visibility$weight[,2], 0.9)[1],2)
upper <- round(PI(s_Visibility$weight[,2], 0.9)[2],2)
text(1, 220, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))






mtext("Coins", side = 2, outer = TRUE, line = 4.5, cex = 1.3)


dev.off()



######
####
## Predicting number of independent discoveries
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
save(s_sin, file = "s_sin")





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
save(s_DC, file = "s_DC")






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
save(s_SDDC, file = "s_SDDC")




###
##
# Convex Hull
##
###

library(rstan)

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
save(s_Hull, file = "s_Hull")




graphics.off()
pdf("MovementPredictorsDiscoveries.pdf", height = 6, width = 9)

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


dev.off()







####
###
##
# GROUP OUTCOMES
##
###
####


load("data/d_group")


{
  
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



}



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






graphics.off()
pdf("BehavioralPredictorsCoinsGroup.pdf", height = 6, width = 10)

par(mfrow = c(2,5), mar = c(0.5,0,0,1), oma = c(4,6,1,0))

#Concentrated
plot(dat_disc$pred[dat_disc$Environment==1], dat_disc$coins[dat_disc$Environment==1],  col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_disc$Incentives[dat_disc$Environment==1]==1, 1, 16), bty = "n", xlim = c(0,12), ylim = c(50,200) , xaxt = "n")
plot_regression_line(s_discoveries, dat_disc$pred, 1, color = col.pal[2])
#axis(side = 1, at = seq(0.1,0.4,0.15), labels = c("0.1","0.25","0.4"))
mtext("Concentrated", side = 2, line = 3)

mean <- round(mean(s_discoveries$weight[,1]),2)
lower <- round(PI(s_discoveries$weight[,1], 0.9)[1],2)
upper <- round(PI(s_discoveries$weight[,1], 0.9)[2],2)
text(5, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))


plot(dat_join$pred[dat_join$Environment==1], dat_join$coins[dat_join$Environment==1],  col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_join$Incentives[dat_join$Environment==1]==1, 1, 16)  , bty = "n", xlim = c(0,10), ylim = c(50,200), yaxt = "n", xaxt = "n")
plot_regression_line(s_joining, dat_join$pred, 1, color = col.pal[2])
#axis(side = 1, at = seq(-0.6,0.2,0.4))

mean <- round(mean(s_joining$weight[,1]),2)
lower <- round(PI(s_joining$weight[,1], 0.9)[1],4)
upper <- round(PI(s_joining$weight[,1], 0.9)[2],4)
text(5, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))



plot(dat_dist$pred[dat_dist$Environment==1], dat_dist$coins[dat_dist$Environment==1],  col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_dist$Incentives[dat_dist$Environment==1]==1, 1, 16)  , bty = "n", xlim = c(10,50), ylim = c(50,200), yaxt = "n", xaxt = "n")
plot_regression_line(s_distance, dat_dist$pred, 1, color = col.pal[2])
#axis(side = 1, at = seq(-0.3,0.1,0.2))

mean <- round(mean(s_distance$weight[,1]),2)
lower <- round(PI(s_distance$weight[,1], 0.9)[1],3)
upper <- round(PI(s_distance$weight[,1], 0.9)[2],3)
text(25, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))


plot(dat_vis$pred[dat_vis$Environment==1], dat_vis$coins[dat_vis$Environment==1],col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_vis$Incentives[dat_vis$Environment==1]==1, 1, 16)  , bty = "n", xlim = c(0.8,1.2), ylim = c(50,200) , yaxt = "n", xaxt = "n")
plot_regression_line(s_visibility, dat_vis$pred, 1, color = col.pal[2])
#axis(side = 1, at = seq(-0.1,0.2,0.15), labels = c("-0.1","0.05","0.2"))

mean <- round(mean(s_visibility$weight[,1]),2)
lower <- round(PI(s_visibility$weight[,1], 0.9)[1],2)
upper <- round(PI(s_visibility$weight[,1], 0.9)[2],2)
text(1, 200, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))



plot(dat_dens$pred[dat_dens$Environment==1], dat_dens$coins[dat_dens$Environment==1],col = alpha(col.pal[2], alpha = 0.4), pch = ifelse(dat_dens$Incentives[dat_dens$Environment==1]==1, 1, 16)  , bty = "n", xlim = c(1,4), ylim = c(50,200) , yaxt = "n", xaxt = "n")
plot_regression_line(s_dens, dat_dens$pred, 1, color = col.pal[2])
#axis(side = 1, at = seq(-0.1,0.2,0.15), labels = c("-0.1","0.05","0.2"))
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


dev.off()









