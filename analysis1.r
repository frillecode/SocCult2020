#library(brms)
#get_prior(model_f, data = data_sets[data_sets$data_set == 1,], family = gaussian())

do_analyses_1 <- function(max_distance) {
  print(">>>>>>>> Doing analyses")
  
  if (do_bglmm == TRUE) {
    print(">>>>>>>>>>>> Doing b-glmms")
    # prep columns
    # this function is in util.R
    save_analysis_results_1("bglmm")
    
    for (experiment in 1:n_experiments_per_repeat) {
      # for every experiment get the relevant data set
      this_data_set <- data_sets[data_sets$data_set == experiment,]
      
      # do the bglmm
      model_f <- bf(response ~ sex * condition + (1|participant_id))
      
      prior_m <- prior(student_t(3, 0, 10), class = b)
      
      model <- brm(
        formula = model_f,
        data = this_data_set,
        family = gaussian,
        prior = prior_m,
        sample_prior = T,
        chains = 2,
        cores = 2
      )
      
      # save the results of the glmm
      b_base_med <<- c(b_base_med, fixef(model)[,1][[1]])
      b_sex_med <<- c(b_sex_med, fixef(model)[,1][[2]])
      b_cond_med <<- c(b_cond_med, fixef(model)[,1][[3]])
      b_sex_cond_med <<- c(b_sex_cond_med, fixef(model)[,1][[4]])
      
      b_base_lower <<- c(b_base_lower, fixef(model)[,3][[1]])
      b_sex_lower <<- c(b_sex_lower, fixef(model)[,3][[2]])
      b_cond_lower <<- c(b_cond_lower, fixef(model)[,3][[3]])
      b_sex_cond_lower <<- c(b_sex_cond_lower, fixef(model)[,3][[4]])
      
      b_base_upper <<- c(b_base_upper, fixef(model)[,4][[1]])
      b_sex_upper <<- c(b_sex_upper, fixef(model)[,4][[2]])
      b_cond_upper <<- c(b_cond_upper, fixef(model)[,4][[3]])
      b_sex_cond_upper <<- c(b_sex_cond_upper, fixef(model)[,4][[4]])
      
      b_sex_p_value <<- c(b_sex_p_value, NaN)
      b_cond_p_value <<- c(b_cond_p_value, NaN)
      b_sex_cond_p_value <<- c(b_sex_cond_p_value, NaN)
    } # end of for each experiment loop
  }# end of do bglmm
  
  if (do_bskep == TRUE) {
    print(">>>>>>>>>>>> Doing b-skeps")
    # prep columns
    # this function is in util.R
    save_analysis_results_1("bskep") 
    
    for (experiment in 1:n_experiments_per_repeat) {
      # for every experiment get the relevant data set
      this_data_set <- data_sets[data_sets$data_set == experiment,]
      
      # do the bglmm
      model_f <- bf(response ~ sex * condition + (1|participant_id))
      
      prior_m <- c(
        prior(normal(0.5, 0.1), class = Intercept), 
        prior(normal(0, 0.1), class = b),
        prior(normal(0, 0.1), class = sd), 
        prior(normal(0.5, 0.1), class = sigma)
      )
      
      model <- brm(
        formula = model_f,
        data = this_data_set,
        family = gaussian,
        prior = prior_m,
        sample_prior = T,
        chains = 2,
        cores = 2
      )
      
      # save the results of the glmm
      b_base_med <<- c(b_base_med, fixef(model)[,1][[1]])
      b_sex_med <<- c(b_sex_med, fixef(model)[,1][[2]])
      b_cond_med <<- c(b_cond_med, fixef(model)[,1][[3]])
      b_sex_cond_med <<- c(b_sex_cond_med, fixef(model)[,1][[4]])
      
      b_base_lower <<- c(b_base_lower, fixef(model)[,3][[1]])
      b_sex_lower <<- c(b_sex_lower, fixef(model)[,3][[2]])
      b_cond_lower <<- c(b_cond_lower, fixef(model)[,3][[3]])
      b_sex_cond_lower <<- c(b_sex_cond_lower, fixef(model)[,3][[4]])
      
      b_base_upper <<- c(b_base_upper, fixef(model)[,4][[1]])
      b_sex_upper <<- c(b_sex_upper, fixef(model)[,4][[2]])
      b_cond_upper <<- c(b_cond_upper, fixef(model)[,4][[3]])
      b_sex_cond_upper <<- c(b_sex_cond_upper, fixef(model)[,4][[4]])
      
      b_sex_p_value <<- c(b_sex_p_value, NaN)
      b_cond_p_value <<- c(b_cond_p_value, NaN)
      b_sex_cond_p_value <<- c(b_sex_cond_p_value, NaN)
    } # end of for each experiment loop
  }# end of do bglmm
  
  if (do_pp == TRUE) {
    print(">>>>>>>>>>>> Doing posterior passing")
    # prep columns
    # this function is in util.R
    save_analysis_results_1("pp")
    
    #set initial prior for beta[4]
    pp_u <<- 0
    pp_prec <<- 10                              #REMEMBER TO change in saras code
    x <- paste("normal(", pp_u, ",", pp_sig, ")", sep = "")
    
    for (experiment in 1:n_experiments_per_repeat) {
      # for every experiment get the relevant data set
      model_f <- bf(response ~ sex * condition + (1|participant_id))
      
      prior_m <- c(
        prior(normal(x,x), class = Intercept), 
        prior_string(x, class = "b", coef = "sex:condition"),
        prior(normal(0,x), class = b, coef = "sex"),
        prior(normal(0,x), class = b, coef = "condition"),
        prior(normal(0,x), class = sd),
        prior(normal(), class = sigma)
      )
      
      model <- brm(
        formula = model_f,
        data = this_data_set,
        family = gaussian,
        prior = prior_m,
        sample_prior = T,
        chains = 2,
        cores = 2
      )
      
      # save the results of the glmm
      b_base_med <<- c(b_base_med, fixef(model)[,1][[1]])
      b_sex_med <<- c(b_sex_med, fixef(model)[,1][[2]])
      b_cond_med <<- c(b_cond_med, fixef(model)[,1][[3]])
      b_sex_cond_med <<- c(b_sex_cond_med, fixef(model)[,1][[4]])
      
      b_base_lower <<- c(b_base_lower, fixef(model)[,3][[1]])
      b_sex_lower <<- c(b_sex_lower, fixef(model)[,3][[2]])
      b_cond_lower <<- c(b_cond_lower, fixef(model)[,3][[3]])
      b_sex_cond_lower <<- c(b_sex_cond_lower, fixef(model)[,3][[4]])
      
      b_base_upper <<- c(b_base_upper, fixef(model)[,4][[1]])
      b_sex_upper <<- c(b_sex_upper, fixef(model)[,4][[2]])
      b_cond_upper <<- c(b_cond_upper, fixef(model)[,4][[3]])
      b_sex_cond_upper <<- c(b_sex_cond_upper, fixef(model)[,4][[4]])
      
      b_sex_p_value <<- c(b_sex_p_value, NaN)
      b_cond_p_value <<- c(b_cond_p_value, NaN)
      b_sex_cond_p_value <<- c(b_sex_cond_p_value, NaN)
      
      # update the priors for the next run
      if (publication_bias == TRUE){
        pb_prob <- ifelse(fixef(model)[,1][[4]] >= max_distance, 
                          1, 
                          fixef(model)[,1][[4]]/max_distance)
        pb <- rbinom(1, size = 1, prob = pb_prob)
        pp_u[1] <<- ifelse(pb == 1,
                           fixef(model)[,1][[4]],
                           pp_u[1])
        pp_sig[1] <<- ifelse(pb ==1,
                             fixef(model)[,2][[4]],
                             pp_sig[1])
      } else {
        pp_u[1] <<- fixef(model)[,1][[4]]
        pp_sig[1] <<- fixef(model)[,2][[4]] 
      }
    } # end of for each experiment loop
  }# end of do pp
}

