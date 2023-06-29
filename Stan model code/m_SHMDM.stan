
// Parallelized stan code for SOCIAL HIDDEN MARKO DECISION MODEL (authored by Dominik Deffner: deffner@mpib-berlin.mpg.de)

// This model infers a time-series of latent behavioral states and their respectives state-dependent distributions.
// State 1 corresponds to individual exploration, state 2 to social relocation.
// We then use those inferred states to model how experimental conditions and situational factors influence players' decision making.

functions {

// We define function to parallelize computation of the likelihood.
// We parallelize on the level of tracks, as those are the fundamental level of analysis for HMMs.
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
    // Initialise forward variable
    logp = rep_vector(-log(N), N);

    //Loop over all observations of track
    for (t in 1:T_track[ track_slice[counter] ]){
         t_temp =  index[ track_slice[counter] ] + t ;
       for (n in 1:N) {

         //Likelihood of being in state n
         logptemp[n] = log_sum_exp(to_vector(log_gamma_tr[t_temp ,n]) + logp);

         //Likelihoods of state-dependent variables conditional on being in state n
         //Turning angles
         if(angles[t_temp]>=(-pi()) ){
          logptemp[n] = logptemp[n] + von_mises_lpdf(angles[t_temp] | loc[n], kappa[n]);
         }

         //Change in distance
         if(deltadist[t_temp]>-10){
          logptemp[n] = logptemp[n] + normal_lpdf( deltadist[t_temp] | mu_distance[n], sigma_distance[n]);
         }

         //Relative Bearing (aka closed angle)
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
 int<lower=0> T; // length of entire dataset
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

 //Data streams
 real angles[T]; // turning angles
 real closed[T]; // minimum closed angle/relative bearing
 real deltadist[T]; //minimum change in distance to exploiting players

 //Predictors for state transitions
 int exploit_visible[T]; // indicator whether exploiting player was in FOV
 real Patch_Prox[T]; // Proximity of (closest) observed exploited patch
 int Patch_Number[T]; // Number of others at (closest) observed exploited patch
 real TimeSinceExploit[T]; // (Standardized) time since last success

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

 //Baseline transition probabilities on the logit scale
 matrix[2,2] logit_p_I_S;
 matrix[2,2] logit_p_S_S;

 //Transition predictors for different incentive conditions and environments
 matrix[2,2] b_vis;
 matrix[2,2] b_dist;
 matrix[2,2] b_numb;
 matrix[2,2] b_time;

  // Varying (aka random) effect terms (non-centered parameterization)
  matrix[24, N_id] z_ID;           //Matrix for our latent individual samples (z scores)
  vector<lower = 0>[24] sigma_ID;  //Standard deviation of transition parameters among individuals
  cholesky_factor_corr[24] Rho_ID; //Cholesky factor for covariance of transition parameters among individuals

  matrix[24, N_group] z_Group;        //Matrix for our latent individual samples (z scores)
  vector<lower = 0>[24] sigma_Group;  //Standard deviation of transition parameters among individuals
  cholesky_factor_corr[24] Rho_Group; //Cholesky factor for covariance of transition parameters among individuals

}

transformed parameters {
 matrix[N_id, 24] offset_ID;
 matrix[N_group, 24] offset_Group;

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

 //Create varying-effects counter varible to account for experimental condition
  int counter;
  if (Incentives[t]==1 && Environment[t]==1) counter = 1;
  if (Incentives[t]==1 && Environment[t]==2) counter = 2;
  if (Incentives[t]==2 && Environment[t]==1) counter = 3;
  if (Incentives[t]==2 && Environment[t]==2) counter = 4;


//There are two possible cases:

// 1) If agents just stopped exploiting or no other player was exploiting at time t-1
// (i.e., no social information was available), agents will stay in (or switch to) exploration
// with very high probability (epsilon is a small number to avoid log(0), i.e., -Inf)

if (Exploit[t] == 1 || No_Exploit[t] == 1){
  for ( i in 1:2 ){
    gamma[t,i,1] = 1-epsilon;
    gamma[t,i,2] = 0+epsilon;
  }
} else{

// 2) Otherwise players either explore independently or relocate towards successful group members

//Here we add predictors for transition probabilities
if (Patch_Prox[t] >-10 ){
p_I_S = inv_logit((logit_p_I_S[Incentives[t],   Environment[t] ] + offset_ID[ id[t],    counter ] + offset_Group[ group[t],   counter ])  +
                        (b_vis[Incentives[t],   Environment[t] ] + offset_ID[ id[t],4+  counter ] + offset_Group[ group[t],4+ counter ])  * exploit_visible[t]  +
                       (b_time[Incentives[t],   Environment[t] ] + offset_ID[ id[t],8+  counter ] + offset_Group[ group[t],8+ counter ]) * TimeSinceExploit[t] +
                       (b_dist[Incentives[t],   Environment[t] ] + offset_ID[ id[t],12+ counter ] + offset_Group[ group[t],12+ counter ]) * exploit_visible[t]  * Patch_Prox[t] +
                       (b_numb[Incentives[t],   Environment[t] ] + offset_ID[ id[t],16+ counter ] + offset_Group[ group[t],16+ counter ]) * exploit_visible[t]  * Patch_Number[t]);


} else{
p_I_S = inv_logit((logit_p_I_S[Incentives[t],   Environment[t] ] + offset_ID[ id[t],   counter ] + offset_Group[ group[t],   counter ])  +
                     (b_vis[Incentives[t],   Environment[t] ] + offset_ID[ id[t],4+ counter ] + offset_Group[ group[t],2+ counter ])  * exploit_visible[t]  +
                    (b_time[Incentives[t],   Environment[t] ] + offset_ID[ id[t],8+ counter ] + offset_Group[ group[t],4+ counter ]) * TimeSinceExploit[t]);
}

p_S_S = inv_logit(logit_p_S_S[Incentives[t],   Environment[t] ] +
                      offset_ID[ id[t],   20 +  counter ] +
                      offset_Group[ group[t],20+counter ] );


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
