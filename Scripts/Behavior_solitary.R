
###
##
# BEHAVIORAL ANALYSES FOR SOLO ROUNDS
##
###

#Load relevant data
load("Data/d_rounds_solo")

####
###
##
# Coins
##
###
####

dat <- list(N = length(d_solo$id), 
            N_id = length(unique(d_solo$id))  , 
            outcome = d_solo$Coins, 
            Environment = ifelse(d_solo$Env == "C",1,2), 
            id = d_solo$id )


#Model to predict number of Coins (Poisson)
poisson_multilevel <- "


data{

  int N; 
  int N_id;
  int outcome[N];
  int Environment[N];
  int id[N];   

}

parameters{
  vector[2] rate; 

  matrix[2, N_id] z_ID;     
  vector<lower = 0>[2] sigma_ID;
  cholesky_factor_corr[2] Rho_ID;
  
} 

transformed parameters {
 matrix[N_id, 2] offset_ID;

//Varying effects offsets
offset_ID = (diag_pre_multiply(sigma_ID, Rho_ID) * z_ID)';


} 


model{

  //Priors
  rate ~ normal(3, 1 ); 

  //Define prior distribution of varying  effects
  to_vector(z_ID) ~ normal(0, 1);
  sigma_ID ~ exponential(3);
  Rho_ID ~ lkj_corr_cholesky(4);
  

//Likelihoods  
for(i in 1:N){                                                        
      outcome[i]  ~ poisson(exp(rate[Environment[i]] + offset_ID[id[i], Environment[i]] )); 
  }                                                                      
} 

generated quantities{
  real outcome_pred[2];
      for(i in 1:2){                 
        //Compute expected coins on  outcome scale
        outcome_pred[i] = exp(rate[i]);
    }  
}

"


m_coins <- stan(model_code = poisson_multilevel, data = dat, iter = 4000, cores = 2, chains = 2, refresh = 10, control = list(adapt_delta = 0.8, max_treedepth = 12))
s_coins <- extract.samples(m_coins)


####
###
##
# Discoveries
##
###
####

dat$outcome <-  d_solo$discoveries

m_discoveries <- stan(model_code = poisson_multilevel, data = dat, iter = 4000, cores = 2, chains = 2, refresh = 10, control = list(adapt_delta = 0.8, max_treedepth = 12))
s_discoveries <- extract.samples(m_discoveries)

#Model to predict number of discoveries depending on movement metrics

discoveries_predict <- "


data{

  int N; 
  int N_id;
  int discoveries[N];
  real pred[N];
  int Environment[N];
  int id[N];   

}

parameters{

  //Intercepts for environments
  vector[2] alpha; 
  
  //Regression weights of predictor
  vector[2] weight; 

 
  matrix[4, N_id] z_ID;     
  vector<lower = 0>[4] sigma_ID;
  cholesky_factor_corr[4] Rho_ID;
  
} 

transformed parameters {
 matrix[N_id, 4] offset_ID;

//Varying effects offsets
offset_ID = (diag_pre_multiply(sigma_ID, Rho_ID) * z_ID)';


} 

model{

 //Priors
 alpha ~ normal(2, 0.5);
 weight ~ normal(0, 1); 

  //Define prior distribution of varying  effects
  to_vector(z_ID) ~ normal(0, 1);
  sigma_ID ~ exponential(3);
  Rho_ID ~ lkj_corr_cholesky(4);
  
//Likelihoods  
for(i in 1:N){     
      discoveries[i]  ~ poisson(exp( (alpha[Environment[i]] + offset_ID[id[i],Environment[i]]) +
                            (weight[Environment[i]]  + offset_ID[id[i],2 + Environment[i]])*pred[i] )  ); 
   
  }                                                                      
} 

"


###
##
# Sinuosity
##
###


#Prepare data list
dat$pred <- standardize(d_solo$Sinuosity)
dat$discoveries <- d_solo$discoveries

m <- stan(model_code = discoveries_predict, data = dat, iter = 2000, cores = 2, chains = 2, refresh = 10)
s_sin <- extract.samples(m)


###
##
# Directional Change
##
###


#Prepare data list
dat$pred <- standardize(d_solo$DC)
dat$discoveries <- d_solo$discoveries

m <- stan(model_code = discoveries_predict, data = dat, iter = 2000, cores = 2, chains = 2, refresh = 10)
s_DC <- extract.samples(m)


dat$pred <- standardize(d_solo$SDDC)
dat$discoveries <- d_solo$discoveries

m <- stan(model_code = discoveries_predict, data = dat, iter = 2000, cores = 2, chains = 2, refresh = 10)
s_SDDC <- extract.samples(m)

###
##
# Convex HUll Area
##
###


dat$pred <- standardize(d_solo$ConvexHull)
dat$discoveries <- d_solo$discoveries

m <- stan(model_code = discoveries_predict, data = dat, iter = 2000, cores = 2, chains = 2, refresh = 10)
s_ConvexHull <- extract.samples(m)


###
##
# Create plots
##
###


#Define plotting function to be re-used later

behavioral_plotting_fct <- function(xseq = c(1.5, 2),
                                    
                                    ylim = c(30,250),
                                    
                                    ylab = "Coins",
                                    
                                    con   =  dat$outcome[dat$Environment==1],
                                    con_ids  =  dat$id[dat$Environment==1],
                                    con_mean =  mean(s_coins$outcome_pred[,1]),
                                    con_hpdi  =  HPDI(s_coins$outcome_pred[,1]),
                                    
                                    dist  =  dat$outcome[dat$Environment==2],
                                    dist_ids   =  dat$id[dat$Environment==2],
                                    dist_mean  =  mean(s_coins$outcome_pred[,2]),
                                    dist_hpdi  =  HPDI(s_coins$outcome_pred[,2]))
{
                                    
 
  
  plot(1:2,xlim = c(1.3,2.1), ylim = ylim, type = "n", xaxt = "n", xlab = "", ylab = "", bty = "n")
  mtext(side = 2, ylab, line = 2.5, cex = 1.2)

  x_coords_con  <- rep(xseq[1],length(con))+jitter(rep(0,length(con)),5)
  x_coords_dist <- rep(xseq[2],length(dist))+jitter(rep(0,length(dist)),5)
  
  
  #Concentrated
  points(x=x_coords_con,y=con, col = alpha(col.pal[2], alpha = 0.4))
  points(xseq[1], con_mean, pch = 16, cex = 2, col = alpha(col.pal[2], alpha = 1))
  segments(xseq[1],con_hpdi[1],xseq[1],con_hpdi[2], col = alpha(col.pal[2], alpha = 1), lwd = 2)
  
  
  #Distributed
  points(x=x_coords_dist,y=dist, col = alpha(col.pal[3], alpha = 0.4))
  points(xseq[2], dist_mean, pch = 16, cex = 2, col = alpha(col.pal[3], alpha = 1))
  segments(xseq[2],dist_hpdi[1],xseq[2],dist_hpdi[2], col = alpha(col.pal[3], alpha = 1), lwd = 2)
  
}


#graphics.off()
#pdf("BehavioralSolo.pdf", height = 3.5, width = 5.5)

par(mfrow = c(1,2),
    mar = c(0,3.5,0.5,0.5), 
    oma = c(0,0,0,0))


#Coins
dat <- list(N = length(d_solo$id), 
            N_id = length(unique(d_solo$id))  , 
            outcome = d_solo$Coins, 
            Environment = ifelse(d_solo$Env == "C",1,2), 
            id = d_solo$id )


behavioral_plotting_fct()

#Discoveries

dat$outcome <-  d_solo$discoveries


behavioral_plotting_fct(         ylim = c(0,15),                                 
                                 ylab = "Discoveries",


                                 con   =  dat$outcome[dat$Environment==1],
                                 con_ids  =  dat$id[dat$Environment==1],
                                 con_mean =  mean(s_discoveries$outcome_pred[,1]),
                                 con_hpdi  =  HPDI(s_discoveries$outcome_pred[,1]),
                                 
                                 dist  =  dat$outcome[dat$Environment==2],
                                 dist_ids   =  dat$id[dat$Environment==2],
                                 dist_mean  =  mean(s_discoveries$outcome_pred[,2]),
                                 dist_hpdi  =  HPDI(s_discoveries$outcome_pred[,2]))
                                    
legend("topleft",title = "Environment", c("Concentrated", "Distributed"), col = c(alpha(col.pal[2],alpha = 0.9),alpha(col.pal[3],alpha = 0.9)), cex = 0.7, lwd = 8, lty = 1, bty = "n")

#dev.off()


#graphics.off()
#pdf("MovementPredictorsDiscoveriesSolo.pdf", height = 6, width = 9)

par(mfrow = c(2,4), mar = c(0.5,0,0,1), oma = c(4,7,1,0))

dat$pred <- standardize(d_solo$Sinuosity)
dat$discoveries <- d_solo$discoveries

#Concentrated
plot(dat$pred[dat$Environment==1], dat$discoveries[dat$Environment==1],  col = alpha(col.pal[2], alpha = 0.4), pch = 16, bty = "n", xlim = c(-3,3), ylim = c(0,7) , xaxt = "n")
plot_regression_line(s_sin, dat$pred, 1, color = col.pal[2])
mtext("Concentrated", side = 2, line = 3)

mean <- round(mean(s_sin$weight[,1]),2)
lower <- round(PI(s_sin$weight[,1], 0.9)[1],2)
upper <- round(PI(s_sin$weight[,1], 0.9)[2],2)
text(0, 7, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

dat$pred <- standardize(d_solo$DC)

plot(dat$pred[dat$Environment==1], dat$discoveries[dat$Environment==1],  col = alpha(col.pal[2], alpha = 0.4), pch = 16  , bty = "n", xlim = c(-3,3), ylim = c(0,7), yaxt = "n", xaxt = "n")
plot_regression_line(s_DC, dat$pred, 1, color = col.pal[2])

mean <- round(mean(s_DC$weight[,1]),2)
lower <- round(PI(s_DC$weight[,1], 0.9)[1],2)
upper <- round(PI(s_DC$weight[,1], 0.9)[2],2)
text(0, 7, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))


dat$pred <- standardize(d_solo$SDDC)

plot(dat$pred[dat$Environment==1], dat$discoveries[dat$Environment==1],  col = alpha(col.pal[2], alpha = 0.4), pch = 16  , bty = "n", xlim = c(-3,3), ylim = c(0,7), yaxt = "n", xaxt = "n")
plot_regression_line(s_SDDC, dat$pred, 1, color = col.pal[2])

mean <- round(mean(s_SDDC$weight[,1]),2)
lower <- round(PI(s_SDDC$weight[,1], 0.9)[1],3)
upper <- round(PI(s_SDDC$weight[,1], 0.9)[2],3)
text(0, 7, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

dat$pred <- standardize(d_solo$ConvexHull)

plot(dat$pred[dat$Environment==1], dat$discoveries[dat$Environment==1],col = alpha(col.pal[2], alpha = 0.4), pch = 16  , bty = "n", xlim = c(-3,3), ylim = c(0,7) , yaxt = "n", xaxt = "n")
plot_regression_line(s_ConvexHull, dat$pred, 1, color = col.pal[2])

mean <- round(mean(s_ConvexHull$weight[,1]),2)
lower <- round(PI(s_ConvexHull$weight[,1], 0.9)[1],2)
upper <- round(PI(s_ConvexHull$weight[,1], 0.9)[2],2)
text(0, 7, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))


#Distributed
dat$pred <- standardize(d_solo$Sinuosity)

plot(dat$pred[dat$Environment==2], dat$discoveries[dat$Environment==2], col = alpha(col.pal[3], alpha = 0.4), pch = 16  , bty = "n", xlim = c(-3,3), ylim = c(4,16) , xaxt = "n")
plot_regression_line(s_sin, dat$pred, 2, color = col.pal[3])
mtext("Distributed", side = 2, line = 3)
axis(side = 1, at = seq(-3,3,2))
mtext("Sinuosity", side = 1, line = 3)

mean <- round(mean(s_sin$weight[,2]),2)
lower <- round(PI(s_sin$weight[,2], 0.9)[1],2)
upper <- round(PI(s_sin$weight[,2], 0.9)[2],2)
text(0, 16, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

dat$pred <- standardize(d_solo$DC)

plot(dat$pred[dat$Environment==2], dat$discoveries[dat$Environment==2],  col = alpha(col.pal[3], alpha = 0.4), pch = 16  , bty = "n", xlim = c(-3,3), ylim = c(4,16) , yaxt = "n", xaxt = "n")
plot_regression_line(s_DC, dat$pred, 2, color = col.pal[3])
axis(side = 1, at = seq(-3,3,2))
mtext("Mean Directional Change", side = 1, line = 3)

mean <- round(mean(s_DC$weight[,2]),2)
lower <- round(PI(s_DC$weight[,2], 0.9)[1],2)
upper <- round(PI(s_DC$weight[,2], 0.9)[2],2)
text(0, 16, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

dat$pred <- standardize(d_solo$SDDC)

plot(dat$pred[dat$Environment==2], dat$discoveries[dat$Environment==2],col = alpha(col.pal[3], alpha = 0.4), pch = 16  , bty = "n", xlim = c(-3,3), ylim = c(4,16) , yaxt = "n", xaxt = "n")
plot_regression_line(s_SDDC, dat$pred, 2, color = col.pal[3])
axis(side = 1, at = seq(-3,3,2))
mtext("SD Directional Change", side = 1, line = 3)

mean <- round(mean(s_SDDC$weight[,2]),2)
lower <- round(PI(s_SDDC$weight[,2], 0.9)[1],3)
upper <- round(PI(s_SDDC$weight[,2], 0.9)[2],3)
text(0, 16, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))

dat$pred <- standardize(d_solo$ConvexHull)

plot(dat$pred[dat$Environment==2], dat$discoveries[dat$Environment==2], col = alpha(col.pal[3], alpha = 0.4), pch = 16  , bty = "n", xlim = c(-3,3), ylim = c(4,16) , yaxt = "n", xaxt = "n")
plot_regression_line(s_ConvexHull, dat$pred, 2, color = col.pal[3])
axis(side = 1, at = seq(-3,3,2))
mtext("Convex Hull Area", side = 1, line = 3)

mean <- round(mean(s_ConvexHull$weight[,2]),2)
lower <- round(PI(s_ConvexHull$weight[,2], 0.9)[1],2)
upper <- round(PI(s_ConvexHull$weight[,2], 0.9)[2],2)
text(0, 16, paste0(mean, " ","[",lower, ",", upper, "]"), cex = 1.1, col = alpha("black", alpha = ifelse(sign(lower) == sign(upper), 1, 0.4 ) ))
mtext("Independent Discoveries", side = 2, outer = TRUE, line = 5.5, cex = 1.3)

#dev.off()
