
// Parallelized stan code for Time-lagged Gaussian-process regression model (authored by Dominik Deffner: deffner@mpib-berlin.mpg.de)
// This model describes the influence of interindividual distance/visibility on collective success across different time lags
// Gaussian processes are used to regularize estimates such that similar time lags are expected to have more similar weights

functions{
//Function for Gaussian Process kernel
  matrix GPL(int K, real C, real D, real S){
   matrix[K,K] Rho;
   real KR;
   KR = K;
   for(i in 1:(K-1)){
   for(j in (i+1):K){
    Rho[i,j] = C * exp(-D * ( (j-i)^2 / KR^2) );
    Rho[j,i] = Rho[i,j];
    }}
   for (i in 1:K){
    Rho[i,i] = 1;
    }
   return S*cholesky_decompose(Rho);
}

real partial_sum(int[] y_slice,
                   int start,
                   int end,
                   int MaxLag,
                   int intervals,
                   real[] predictor,
                   matrix alpha,
                   vector[,] time_effects,
                   int[] t,
                   int[] Incentives,
                   int[] Environment) {

             real logp=0;
             int counter=0;

            for(i in start:end){
              counter += 1;

            for( lag in 1:MaxLag ){
              //We start modeling the (lag*interval+1)th value in each round
                if ( t[i] >= (intervals*lag) + 1){
                       logp += binomial_lpmf(y_slice[counter] | 4, inv_logit(alpha[Incentives[i], Environment[i]]  +
                                              time_effects[Incentives[i], Environment[i]][lag] * predictor[i-(intervals*lag)]));
                 }
              }
}
    return logp;

}

}

// Define the observed variables we feed into the model as data
data {
  int N;
  int MaxLag;    //Maximum Time-lag
  int intervals; //Intervals between lags in seconds
  int t[N];
  int Group[N];
  int Round[N];
  real predictor[N];
  int vis[N];
  int n_exploit[N];
  int Environment[N]; // Environment (concentrated = 1 vs. distributed = 2)
  int Incentives[N]; // Incentives (Cooperative = 1 vs competitive = 2)  int outcome[N];
}

// Define the unobserved variables (parameters) that we estimate from the data
parameters {
  matrix[2,2] alpha;           //Condition intercepts
  matrix[2,2] b_time;           //Condition slopes
  vector[MaxLag] time_offsets[2,2];    //Matrix for Gaussian process time effects

  //Here we define the Control parameters for the Gaussian processes; they determine how covariance changes with increasing distance in time lags
  real<lower=0> eta[2,2];
  real<lower=0> sigma[2,2];
  real<lower=0, upper=1> rho[2,2];

}

transformed parameters {

 vector[MaxLag] time_effects[2,2];

  for (i in 1:2){
    for (j in 1:2){
     time_effects[i,j] = b_time[i,j] + time_offsets[i,j];
    }
  }

}

model {

  // Define priors for parameters
  to_vector(alpha) ~ normal(0, 1);
  to_vector(b_time) ~ normal(0, 1);

  //Define Gaussian-process priors for time-lag effects
  for (i in 1:2){
    for (j in 1:2){
       eta[i,j] ~ exponential(1);
       sigma[i,j] ~ exponential(1);
       rho[i,j] ~ beta(5, 1);

       time_offsets[i,j] ~ multi_normal_cholesky( rep_vector(0, MaxLag) , GPL(MaxLag, rho[i,j], eta[i,j], sigma[i,j]) );
    }
  }

//Likelihood
    target += reduce_sum(partial_sum, n_exploit ,1, MaxLag, intervals, predictor, alpha, time_effects,t, Incentives, Environment);
}
