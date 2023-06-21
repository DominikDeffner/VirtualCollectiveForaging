// Parallelized stan code for SOCIAL HIDDEN MARKO DECISION MODEL (authored by Dominik Deffner: deffner@mpib-berlin.mpg.de)
// Temporal Trends in State predictors
// This model infers a time-series of latent behavioral states and their respectives state-dependent distributions.
// State 1 corresponds to individual exploration, state 2 to social relocation.
// We then use those inferred states to model how experimental conditions and situational factors influence players' decision making.

functions {

//Function to parallelize
real partial_sum(int[] track_slice,
                   int start,
                   int end,
                   int N,
                   int[] T_track,
                   int[] index,
                   int[] exploit_visible,
                   real[] angles,
                   real[] closed,
                   real[] deltadist,
                   vector loc,
                   vector kappa,
                   vector mu_distance,
                   vector sigma_distance,
                   vector mu_closed,
                   vector sigma_closed,
                   array[] matrix log_gamma_tr) {

      real logprop=0;
      vector[N] logp;
      vector[N] logptemp;
      int counter = 0;
      int t_temp;

for(i in start:end){

        counter += 1;

    // Likelihood computation using the forward algoritm to compute marginal likelood recursively
    // initialise forward variable if first obs of track
    logp = rep_vector(-log(N), N);

    for (t in 1:T_track[ track_slice[counter] ]){

         t_temp =  index[ track_slice[counter] ] + t ;

       for (n in 1:N) {

         //Likelihood of being in state n
         logptemp[n] = log_sum_exp(to_vector(log_gamma_tr[t_temp ,n]) + logp);

         //Likelihoods of variables conditional on being in state n

         //Turning angles
         if(angles[t_temp]>=(-pi()) ){
          logptemp[n] = logptemp[n] + von_mises_lpdf(angles[t_temp] | loc[n], kappa[n]);
         }

         //Change in distance
         if(deltadist[t_temp]>-10){
          logptemp[n] = logptemp[n] + normal_lpdf( deltadist[t_temp] | mu_distance[n], sigma_distance[n]);
         }

         //Closed angles
         if(closed[t_temp]>=0){
          logptemp[n] = logptemp[n] + lognormal_lpdf( closed[t_temp] | log(mu_closed[n]), sigma_closed[n]);
         }
  }//n

       logp = logptemp;

  }//t

       //Add log forward variable to target at the end of each track
       logprop += log_sum_exp(logp);
  }//id
   return logprop;
}

}


data {
 //
 int<lower=0> T; // length of the time series
 int N_id;       //number of unique individuals
 int id[T];     // unique participant id
 int N_group;     //number of unique groups
 int group[T];     // unique group id
 int track_id[T]; // track identifier
 int N_tracks; //Number of unique tracks
 int tracks[N_tracks]; // track ids
 int index[N_tracks];  //index where every track starts
 int T_track[N_tracks];  //lengths of single tracks
 int No_Exploit[T]; // indicator whether anyone was exploiting
 int Exploit[T]; // indicator whether player just stopped exploiting
 int TimeInRound[T];  //Time step in round

 //Data streams
 real angles[T]; // turning angles
 real closed[T]; // minimum closed angle
 real deltadist[T]; //minimum change in distance to exploiting players

 //Predictors for state transitions
 int exploit_visible[T]; // indicator whether exploiting player was in FOV
 real Patch_Prox[T]; // Proximity of (closest) observed exploited patch
 int Patch_Number[T]; // Number of others at (closest) observed exploited patch
 int TimeSinceExploit[T]; // Time since last success

 int Environment[T]; // Environment (concentrated = 1 vs. distributed = 2)
 int Incentives[T]; // Incentives (Cooperative = 1 vs competitive = 2)
 int<lower=1> N; // number of states
}

parameters {

//State-dependent variables
//Turning angles (von Mises); unconstrained angle parameters
  ordered[N] xangle;
  vector[N] yangle;

//Smallest closed angle (lognormal)
 vector<lower=0>[N] mu_closed;
 vector<lower=0>[N] sigma_closed;

 //Smallest change in distance (normal)
 vector[N] mu_distance;
 vector<lower=0>[N] sigma_distance;

 //Baseline transition probabilities
 matrix[2,2] logit_p_I_S;
 matrix[2,2] logit_p_S_S;

 //Transition predictors
 matrix[2,2] b_vis;
 matrix[2,2] b_dist;
 matrix[2,2] b_numb;
 matrix[2,2] b_time;

 //Monotonic Time effects
 matrix[2,2] b_vis_max;
 matrix[2,2] b_dist_max;
 matrix[2,2] b_numb_max;
 matrix[2,2] b_time_max;

 simplex[11] b_vis_time[2,2];
 simplex[11] b_dist_time[2,2];
 simplex[11] b_numb_time[2,2];
 simplex[11] b_time_time[2,2];


  matrix[4, N_id] z_ID;           //Matrix for our latent individual samples (z scores)
  vector<lower = 0>[4] sigma_ID;  //Standard deviation of transition parameters among individuals
  cholesky_factor_corr[4] Rho_ID; //Cholesky factor for covariance of transition parameters among individuals

  matrix[4, N_group] z_Group;           //Matrix for our latent individual samples (z scores)
  vector<lower = 0>[4] sigma_Group;  //Standard deviation of transition parameters among individuals
  cholesky_factor_corr[4] Rho_Group; //Cholesky factor for covariance of transition parameters among individuals


}

transformed parameters {
 matrix[N_id, 4] offset_ID;
 matrix[N_group, 4] offset_Group;

 //Turning angles (von Mises)
 vector<lower=-pi(),upper=pi()>[N] loc;
 vector<lower=0>[N] kappa;

  //Varying effects offsets
  offset_ID = (diag_pre_multiply(sigma_ID, Rho_ID) * z_ID)';
  offset_Group = (diag_pre_multiply(sigma_Group, Rho_Group) * z_Group)';

 //Transform parameters for each state
for(n in 1:N){

//Turning angles (von Mises)
loc[n] = atan2(yangle[n], xangle[n]);
kappa[n] = sqrt(xangle[n]*xangle[n] + yangle[n]*yangle[n]);

}

}
model {
 matrix[N,N] gamma[T];
 matrix[N,N] log_gamma_tr[T];
 real epsilon = 1e-6;

 //Monotonic Effects vectors
 vector[12] delta_vis[2,2];
 vector[12] delta_dist[2,2];
 vector[12] delta_numb[2,2];
 vector[12] delta_time[2,2];


// Priors
//Transition probabilities
to_vector(logit_p_I_S) ~ normal(0,1);
to_vector(logit_p_S_S) ~ normal(0,1);

//Transition predictors
for (i in 1:2){
to_vector(b_vis[i]) ~ normal(0,2);
to_vector(b_dist[i]) ~ normal(0,2);
to_vector(b_numb[i]) ~ normal(0,2);
to_vector(b_time[i]) ~ normal(0,2);
}

//State-dependent variables

//Turning angles (von Mises)
xangle[1] ~ normal(5, 5); // equiv to concentration when yangle = 0
xangle[2] ~ normal(50, 5);
yangle ~ normal(0, 0.01); // zero if mean angle is 0 or pi

//Smallest closed angle (lognormal)
mu_closed[1]    ~ normal( 1 , 0.5 )T[0, ];
mu_closed[2]    ~ normal( 0.5 , 0.5 )T[0, ];
sigma_closed[1] ~ exponential(2);
sigma_closed[2] ~ exponential(2);

//Minimum change in ditance
mu_distance[1]    ~ normal( 0 , 0.5 );
mu_distance[2]    ~ normal( -1.5 , 0.5 );
sigma_distance[1] ~ exponential(2);
sigma_distance[2] ~ exponential(2);

//Monotonic Effects

for(i in 1:2){
  for (j in 1:2){

   //Maximum time effect
   b_vis_max[i,j] ~ normal(0,1);
   b_dist_max[i,j] ~ normal(0,1);
   b_numb_max[i,j] ~ normal(0,1);
   b_time_max[i,j] ~ normal(0,1);

   //Offsets
   b_vis_time[i,j]  ~ dirichlet( rep_vector(2,11) );
   b_dist_time[i,j] ~ dirichlet( rep_vector(2,11) );
   b_numb_time[i,j] ~ dirichlet( rep_vector(2,11) );
   b_time_time[i,j] ~ dirichlet( rep_vector(2,11) );

   delta_vis[i,j]  = append_row( 0 , b_vis_time[i,j]);
   delta_dist[i,j] = append_row( 0 , b_dist_time[i,j]);
   delta_numb[i,j] = append_row( 0 , b_numb_time[i,j]);
   delta_time[i,j] = append_row( 0 , b_time_time[i,j]);

  }
}


//Define prior distribution of varying individual and group effects
to_vector(z_ID) ~ normal(0, 1);
sigma_ID ~ exponential(1);
Rho_ID ~ lkj_corr_cholesky(4);

to_vector(z_Group) ~ normal(0, 1);
sigma_Group ~ exponential(1);
Rho_Group ~ lkj_corr_cholesky(4);

//For each time step in the dataset, we derive 2x2 matrix of (log-)transition probabilities
for (t in 1:T) {

 real p_I_S;   //Probability to switch from individual search to social relocation
 real p_S_S;   //Probability to stay in social relocation


// 1) Agents themselves just stopped exploiting or no other player was exploiting at time t-1
// Agents will stay in (or switch to) exploration with very high probability
// Epsilon is a small number to avoid log(0), i.e., -Inf

if (Exploit[t] == 1 || No_Exploit[t] == 1){
  for ( i in 1:2 ){
    gamma[t,i,1] = 1-epsilon;
    gamma[t,i,2] = 0+epsilon;
  }
} else{

// 2) Agents either explore or relocate towards successful group members


//Here we add predictors for transition probabilities
//The effect of each predictor is time-dependent
if (Patch_Prox[t] >-10 ){
p_I_S = inv_logit(logit_p_I_S[Incentives[t],   Environment[t] ]  +
                        (b_vis[Incentives[t],   Environment[t] ] + offset_ID[ id[t],  1 ] + offset_Group[ group[t], 1 ] +
                     b_vis_max[Incentives[t],   Environment[t] ] * sum( delta_vis[Incentives[t], Environment[t] ][1:TimeInRound[t]]) )   * exploit_visible[t]  +
                       (b_time[Incentives[t],   Environment[t] ] + offset_ID[ id[t], 2 ] + offset_Group[ group[t],2  ] +
                    b_time_max[Incentives[t],   Environment[t] ] * sum( delta_time[Incentives[t], Environment[t] ][1:TimeInRound[t]])  ) * TimeSinceExploit[t] +
                       (b_dist[Incentives[t],   Environment[t] ] + offset_ID[ id[t], 3 ] + offset_Group[ group[t],3 ] +
                    b_dist_max[Incentives[t],   Environment[t] ] * sum( delta_dist[Incentives[t], Environment[t] ][1:TimeInRound[t]]) ) * exploit_visible[t]  * Patch_Prox[t] +
                       (b_numb[Incentives[t],   Environment[t] ] + offset_ID[ id[t],  4 ] + offset_Group[ group[t],4 ] +
                    b_numb_max[Incentives[t],   Environment[t] ] * sum( delta_numb[Incentives[t], Environment[t] ][1:TimeInRound[t]])  ) * exploit_visible[t]  * Patch_Number[t]);


} else{
p_I_S = inv_logit(logit_p_I_S[Incentives[t],   Environment[t] ] +
                     (b_vis[Incentives[t],   Environment[t] ] + offset_ID[ id[t],  1] + offset_Group[ group[t], 1] +
                  b_vis_max[Incentives[t],   Environment[t] ] * sum( delta_vis[Incentives[t], Environment[t] ][1:TimeInRound[t]])  )  * exploit_visible[t]  +
                    (b_time[Incentives[t],   Environment[t] ] + offset_ID[ id[t], 2  ] + offset_Group[ group[t],2 ] +
                 b_time_max[Incentives[t],   Environment[t] ] * sum( delta_time[Incentives[t], Environment[t] ][1:TimeInRound[t]]) ) * TimeSinceExploit[t]);



}

p_S_S = inv_logit(logit_p_S_S[Incentives[t],   Environment[t] ] );


//Complete transition probability matrix for time t
//P(stay individual)
gamma[t, 1, 1] = 1 - p_I_S;

//P(switch to social)
gamma[t, 1, 2] = p_I_S;

//P(switch to individual)
gamma[t, 2, 1] = 1 - p_S_S;

//P(stay social)
gamma[t, 2, 2] = p_S_S;
}

}//t

// Transpose transition probability matrix and take the log of each entry
for(t in 1:T){
  for (n_from in 1:N){
     for (n in 1:N){
          log_gamma_tr[t, n, n_from] = log(gamma[t, n_from, n]);
     }//n
  }//n_from
}//t


//Pass to reduce_sum to compute likelihood

target += reduce_sum(partial_sum, tracks , 10 , N, T_track, index, exploit_visible,
                    angles, closed, deltadist, loc, kappa, mu_distance, sigma_distance,
                    mu_closed, sigma_closed, log_gamma_tr);

}
