
// Parallelized stan code for model of success probabilities over time (authored by Dominik Deffner: deffner@mpib-berlin.mpg.de)

functions {

real partial_sum(int[] y_slice,
                   int start,
                   int end,
                   matrix alpha,
                   matrix b_time_max,
                   vector[,] delta_time,
                   int[] TimeInRound,
                   int[] Incentives,
                   int[] Environment,
                   matrix offset_ID,
                   matrix offset_Group,
                   int[] id,
                   int[] group) {

             real logp=0;
             int counter=0;

              for(n in start:end){
              counter += 1;

                logp +=  bernoulli_logit_lpmf(y_slice[counter] | (alpha[Incentives[n], Environment[n] ] + offset_ID[ id[n], Environment[n] ] + offset_Group[ group[n], Environment[n] ]) +
                                                            (b_time_max[Incentives[n], Environment[n] ] + offset_ID[ id[n], 2+Environment[n] ] + offset_Group[ group[n], 2+ Environment[n] ])*
                                                        sum( delta_time[Incentives[n], Environment[n] ][1:TimeInRound[n]]));
              }

    return logp;
}

}

data{

  int N;
  int N_id;
  int N_group;
  int TimeInRound[N];
  int Environment[N];
  int Incentives[N];
  int group[N];
  int id[N];

  int extract[N];
}

parameters{

  //Intercepts for different conditions
  matrix[2,2] alpha;

 //Monotonic Time effects
 matrix[2,2] b_time_max;
 simplex[11] b_time[2,2];

  matrix[4, N_id] z_ID;
  vector<lower = 0>[4] sigma_ID;
  cholesky_factor_corr[4] Rho_ID;

  matrix[4, N_group] z_Group;
  vector<lower = 0>[4] sigma_Group;
  cholesky_factor_corr[4] Rho_Group;

}

transformed parameters {
  matrix[N_group, 4] offset_Group;
  matrix[N_id, 4]    offset_ID;

  //Varying effects offsets
  offset_Group = (diag_pre_multiply(sigma_Group, Rho_Group) * z_Group)';
  offset_ID = (diag_pre_multiply(sigma_ID, Rho_ID) * z_ID)';

}

model{
 vector[12] delta_time[2,2];

 //Priors
 to_vector(alpha) ~ normal(0, 1);

  //Define prior distribution of varying group effects
  to_vector(z_ID) ~ normal(0, 1);
  sigma_ID ~ exponential(1);
  Rho_ID ~ lkj_corr_cholesky(4);

  to_vector(z_Group) ~ normal(0, 1);
  sigma_Group ~ exponential(1);
  Rho_Group ~ lkj_corr_cholesky(4);
  
//Monotonic Effects
for(i in 1:2){
  for (j in 1:2){
   //Maximum time effect
   b_time_max[i,j] ~ normal(0,1);

   //Offsets
   b_time[i,j] ~ dirichlet( rep_vector(4,11) );
   delta_time[i,j] = append_row( 0 , b_time[i,j]);
  }
}

//Likelihood
target += reduce_sum(partial_sum, extract ,1, alpha, b_time_max, delta_time,TimeInRound, Incentives, Environment, offset_ID, offset_Group, id, group );

}
