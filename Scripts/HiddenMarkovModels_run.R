library(rethinking)
setwd("CoinScrounge")


#Construct smaller test dataset

stan.dataTest <- stan.data

stan.dataTest$tracks <- 1:100
stan.dataTest$N_tracks <- length(stan.data$tracks)

new_index <- which(stan.data$track_id %in% stan.dataTest$tracks)

stan.dataTest$track_id <- stan.data$track_id[new_index]

stan.dataTest$steps <- NULL
stan.dataTest$angles <- stan.data$angles[new_index]
stan.dataTest$steps <- stan.data$steps[new_index]+0.0001
stan.dataTest$closed <- stan.data$closed[new_index]
stan.dataTest$deltadist <- stan.data$deltadist[new_index]
stan.dataTest$exploit_visible <- stan.data$exploit_visible[new_index]
stan.dataTest$Environment <- stan.data$Environment[new_index]
stan.dataTest$Incentives <- stan.data$Incentives[new_index]
stan.dataTest$Exploit <- stan.data$Exploit[new_index]
stan.dataTest$T <- length(stan.dataTest$angles)

#Create variables needed for parallelized computation of likelihood
stan.dataTest$track_id <-   sapply(1: stan.dataTest$T, function(i) which(unique(stan.dataTest$track_id) == stan.dataTest$track_id[i] ))
stan.dataTest$tracks <- unique(stan.dataTest$track_id)
stan.dataTest$N_tracks <- length(stan.dataTest$tracks)
stan.dataTest$index  <- sapply(stan.dataTest$tracks, function(id) which(stan.dataTest$track_id == id)[1]-1)
stan.dataTest$T_track <- sapply(stan.dataTest$tracks, function(id) length(which(stan.dataTest$track_id == id)))






library(cmdstanr)

inits <- function () list(mu_closed = c(1, 0.5),mu_distance = c(0,-2),p_exploit_visible=c(0.5,0.99), xangle=c(3,12), yangle=c(0,0), sigma_closed = c(0.1,0.1), sigma_distance = c(0.1,0.1) )


m_parallel <- cmdstan_model("HMM_socialInfo_parallel_viterbi.stan", cpp_options = list(stan_threads = TRUE))
fit_parallel <- m_parallel$sample(stan.dataTest, chains = 2, parallel_chains = 2, threads_per_chain = 50, init = inits, refresh = 1, iter_warmup = 1500 ,adapt_delta = 0.8, iter_sampling = 1000)


fit <- rstan::read_stan_csv(fit_parallel$output_files())
save(fit, file = "fit_HMM_viterbi_200123")

s <- extract.samples(fit)
precis(fit, 3)

traceplot(fit)










