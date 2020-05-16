prepare_meta_vectors <- function() {
  total_meta_conditions <<- length(b_bases)*length(b_sexs)*length(b_conds)*length(b_sex_conds)*length(var_bases)*length(var_sexs)*length(var_conds)*length(var_sex_conds)
  meta_n_repeats <<- rep(n_repeats, total_meta_conditions)
  meta_n_experiments_per_repeat <<- rep(n_experiments_per_repeat, total_meta_conditions)
  meta_n_participants_per_experiment <<- rep(n_participants_per_experiment, total_meta_conditions)
  meta_n_trials_per_participant <<- rep(n_trials_per_participant, total_meta_conditions)
  meta_n_people <<- rep(n_people, total_meta_conditions)
  meta_true_base <<- vector()
  meta_true_sex <<- vector()
  meta_true_cond <<- vector()
  meta_true_sex_cond <<- vector()
  meta_var_base <<- vector()
  meta_var_sex <<- vector()
  meta_var_cond <<- vector()
  meta_var_sex_cond <<- vector()
  
  meta_base_estimate_bglmm <<- vector()
  meta_sex_estimate_bglmm <<- vector()
  meta_cond_estimate_bglmm <<- vector()
  meta_sex_cond_estimate_bglmm <<- vector()
  
  meta_base_estimate_bskep <<- vector()
  meta_sex_estimate_bskep <<- vector()
  meta_cond_estimate_bskep <<- vector()
  meta_sex_cond_estimate_bskep <<- vector()
  
  meta_base_estimate_pp <<- vector()
  meta_sex_estimate_pp <<- vector()
  meta_cond_estimate_pp <<- vector()
  meta_sex_cond_estimate_pp <<- vector()
  
  meta_base_estimate_upper_bglmm <<- vector()
  meta_sex_estimate_upper_bglmm <<- vector()
  meta_cond_estimate_upper_bglmm <<- vector()
  meta_sex_cond_estimate_upper_bglmm <<- vector()
  
  meta_base_estimate_upper_bskep <<- vector()
  meta_sex_estimate_upper_bskep <<- vector()
  meta_cond_estimate_upper_bskep <<- vector()
  meta_sex_cond_estimate_upper_bskep <<- vector()
  
  meta_base_estimate_upper_pp <<- vector()
  meta_sex_estimate_upper_pp <<- vector()
  meta_cond_estimate_upper_pp <<- vector()
  meta_sex_cond_estimate_upper_pp <<- vector()
  
  meta_base_estimate_lower_bglmm <<- vector()
  meta_sex_estimate_lower_bglmm <<- vector()
  meta_cond_estimate_lower_bglmm <<- vector()
  meta_sex_cond_estimate_lower_bglmm <<- vector()
  
  meta_base_estimate_lower_bskep <<- vector()
  meta_sex_estimate_lower_bskep <<- vector()
  meta_cond_estimate_lower_bskep <<- vector()
  meta_sex_cond_estimate_lower_bskep <<- vector()
  
  meta_base_estimate_lower_pp <<- vector()
  meta_sex_estimate_lower_pp <<- vector()
  meta_cond_estimate_lower_pp <<- vector()
  meta_sex_cond_estimate_lower_pp <<- vector()
  
  meta_sex_positive_rate_bglmm <<- vector()
  meta_cond_positive_rate_bglmm <<- vector()
  meta_sex_cond_positive_rate_bglmm <<- vector()
  
  meta_sex_positive_rate_bskep <<- vector()
  meta_cond_positive_rate_bskep <<- vector()
  meta_sex_cond_positive_rate_bskep <<- vector()
  
  meta_sex_positive_rate_pp <<- vector()
  meta_cond_positive_rate_pp <<- vector()
  meta_sex_cond_positive_rate_pp <<- vector()
  
  meta_base_uncertainty_bglmm <<- vector()
  meta_sex_uncertainty_bglmm <<- vector()
  meta_cond_uncertainty_bglmm <<- vector()
  meta_sex_cond_uncertainty_bglmm <<- vector()
  
  meta_base_uncertainty_bskep <<- vector()
  meta_sex_uncertainty_bskep <<- vector()
  meta_cond_uncertainty_bskep <<- vector()
  meta_sex_cond_uncertainty_bskep <<- vector()
  
  meta_base_uncertainty_pp <<- vector()
  meta_sex_uncertainty_pp <<- vector()
  meta_cond_uncertainty_pp <<- vector()
  meta_sex_cond_uncertainty_pp <<- vector()
}

prepare_data_vectors <- function() {
  repeat_id <<- vector()
  expt <<- vector()
  analysis_type <<- vector()
  true_base <<- vector()
  b_base_lower <<- vector()
  b_base_med <<- vector()
  b_base_upper <<- vector()
  b_base_error <<- vector()
  true_sex <<- vector()
  b_sex_p_value <<- vector()
  b_sex_lower <<- vector()
  b_sex_med <<- vector()
  b_sex_upper <<- vector()
  b_sex_error <<- vector()
  true_cond <<- vector()
  b_cond_p_value <<- vector()
  b_cond_lower <<- vector()
  b_cond_med <<- vector()
  b_cond_upper <<- vector()
  b_cond_error <<- vector()
  true_sex_cond <<- vector()
  b_sex_cond_p_value <<- vector()
  b_sex_cond_lower <<- vector()
  b_sex_cond_med <<- vector()
  b_sex_cond_upper <<- vector()
  b_sex_cond_error <<- vector()
  pp_true <<- vector()
}

save_analysis_results_1 <- function(type) {
  repeat_id <<- c(repeat_id, rep(rep, n_experiments_per_repeat))
  expt <<- c(expt, c(1:n_experiments_per_repeat))
  true_base <<- c(true_base, rep(b_base, n_experiments_per_repeat))
  true_sex <<- c(true_sex, rep(b_sex, n_experiments_per_repeat))
  true_cond <<- c(true_cond, rep(b_cond, n_experiments_per_repeat))
  true_sex_cond <<- c(true_sex_cond, rep(b_sex_cond, n_experiments_per_repeat))
  analysis_type <<- c(analysis_type, rep(type, n_experiments_per_repeat))
}

save_results_meta <- function() {
  print(">>>>>>>> Saving results")
  # combine results into a data frame
  results <- data.frame(repeat_id, expt, analysis_type,
                        true_base, b_base_lower, b_base_med, b_base_upper, b_base_error,
                        true_sex, b_sex_p_value, b_sex_lower, b_sex_med, b_sex_upper, b_sex_error,
                        true_cond, b_cond_p_value, b_cond_lower, b_cond_med, b_cond_upper, b_cond_error,
                        true_sex_cond, b_sex_cond_p_value, b_sex_cond_lower, b_sex_cond_med, b_sex_cond_upper, b_sex_cond_error, pp_true)
  saved_results <<- results
  rm(this_data_set, model,
     repeat_id, expt, analysis_type,
     true_base, b_base_lower, b_base_med, b_base_upper,
     true_sex, b_sex_p_value, b_sex_lower, b_sex_med, b_sex_upper,
     true_cond, b_cond_p_value, b_cond_lower, b_cond_med, b_cond_upper, 
     true_sex_cond, b_sex_cond_p_value, b_sex_cond_lower, b_sex_cond_med, b_sex_cond_upper,
     pos = ".GlobalEnv")
  
  for (rep in 1:n_repeats) {
    bglmm_results <- results[results$repeat_id == rep & results$analysis_type == "bglmm",]
    bskep_results <- results[results$repeat_id == rep & results$analysis_type == "bskep",] #ulla
    pp_results <- results[results$repeat_id == rep & results$analysis_type == "pp",]
    
    meta_base_estimate_bglmm <<- c(meta_base_estimate_bglmm, mean(bglmm_results$b_base_med))
    meta_sex_estimate_bglmm <<- c(meta_sex_estimate_bglmm, mean(bglmm_results$b_sex_med))
    meta_cond_estimate_bglmm <<- c(meta_cond_estimate_bglmm, mean(bglmm_results$b_cond_med))
    meta_sex_cond_estimate_bglmm <<- c(meta_sex_cond_estimate_bglmm, mean(bglmm_results$b_sex_cond_med))
    
    #ulla
    meta_base_estimate_bskep <<- c(meta_base_estimate_bskep, mean(bskep_results$b_base_med)) 
    meta_sex_estimate_bskep <<- c(meta_sex_estimate_bskep, mean(bskep_results$b_sex_med))
    meta_cond_estimate_bskep <<- c(meta_cond_estimate_bskep, mean(bskep_results$b_cond_med))
    meta_sex_cond_estimate_bskep <<- c(meta_sex_cond_estimate_bskep, mean(bskep_results$b_sex_cond_med))
    
    if (pp_final_expt_only == FALSE) {
      meta_base_estimate_pp <<- c(meta_base_estimate_pp, mean(pp_results$b_base_med))
      meta_sex_estimate_pp <<- c(meta_sex_estimate_pp, mean(pp_results$b_sex_med))
      meta_cond_estimate_pp <<- c(meta_cond_estimate_pp, mean(pp_results$b_cond_med))
      meta_sex_cond_estimate_pp <<- c(meta_sex_cond_estimate_pp, mean(pp_results$b_sex_cond_med))
    } else {
      meta_base_estimate_pp <<- c(meta_base_estimate_pp, mean(pp_results$b_base_med[pp_results$expt == n_experiments_per_repeat]))
      meta_sex_estimate_pp <<- c(meta_sex_estimate_pp, mean(pp_results$b_sex_med[pp_results$expt == n_experiments_per_repeat]))
      meta_cond_estimate_pp <<- c(meta_cond_estimate_pp, mean(pp_results$b_cond_med[pp_results$expt == n_experiments_per_repeat]))
      meta_sex_cond_estimate_pp <<- c(meta_sex_cond_estimate_pp, mean(pp_results$b_sex_cond_med[pp_results$expt == n_experiments_per_repeat]))
    }
  
    meta_base_estimate_upper_bglmm <<- c(meta_base_estimate_upper_bglmm, mean(bglmm_results$b_base_upper))
    meta_sex_estimate_upper_bglmm <<- c(meta_sex_estimate_upper_bglmm, mean(bglmm_results$b_sex_upper))
    meta_cond_estimate_upper_bglmm <<- c(meta_cond_estimate_upper_bglmm, mean(bglmm_results$b_cond_upper))
    meta_sex_cond_estimate_upper_bglmm <<- c(meta_sex_cond_estimate_upper_bglmm, mean(bglmm_results$b_sex_cond_upper))
    
    #ulla
    meta_base_estimate_upper_bskep <<- c(meta_base_estimate_upper_bskep, mean(bskep_results$b_base_upper))
    meta_sex_estimate_upper_bskep <<- c(meta_sex_estimate_upper_bskep, mean(bskep_results$b_sex_upper))
    meta_cond_estimate_upper_bskep <<- c(meta_cond_estimate_upper_bskep, mean(bskep_results$b_cond_upper))
    meta_sex_cond_estimate_upper_bskep <<- c(meta_sex_cond_estimate_upper_bskep, mean(bskep_results$b_sex_cond_upper))
    
    if (pp_final_expt_only == FALSE) {
      meta_base_estimate_upper_pp <<- c(meta_base_estimate_upper_pp, mean(pp_results$b_base_upper))
      meta_sex_estimate_upper_pp <<- c(meta_sex_estimate_upper_pp, mean(pp_results$b_sex_upper))
      meta_cond_estimate_upper_pp <<- c(meta_cond_estimate_upper_pp, mean(pp_results$b_cond_upper))
      meta_sex_cond_estimate_upper_pp <<- c(meta_sex_cond_estimate_upper_pp, mean(pp_results$b_sex_cond_upper))
    } else {
      meta_base_estimate_upper_pp <<- c(meta_base_estimate_upper_pp, mean(pp_results$b_base_upper[pp_results$expt == n_experiments_per_repeat]))
      meta_sex_estimate_upper_pp <<- c(meta_sex_estimate_upper_pp, mean(pp_results$b_sex_upper[pp_results$expt == n_experiments_per_repeat]))
      meta_cond_estimate_upper_pp <<- c(meta_cond_estimate_upper_pp, mean(pp_results$b_cond_upper[pp_results$expt == n_experiments_per_repeat]))
      meta_sex_cond_estimate_upper_pp <<- c(meta_sex_cond_estimate_upper_pp, mean(pp_results$b_sex_cond_upper[pp_results$expt == n_experiments_per_repeat]))
    }
    
    meta_base_estimate_lower_bglmm <<- c(meta_base_estimate_lower_bglmm, mean(bglmm_results$b_base_lower))
    meta_sex_estimate_lower_bglmm <<- c(meta_sex_estimate_lower_bglmm, mean(bglmm_results$b_sex_lower))
    meta_cond_estimate_lower_bglmm <<- c(meta_cond_estimate_lower_bglmm, mean(bglmm_results$b_cond_lower))
    meta_sex_cond_estimate_lower_bglmm <<- c(meta_sex_cond_estimate_lower_bglmm, mean(bglmm_results$b_sex_cond_lower))
    
    #ulla
    meta_base_estimate_lower_bskep <<- c(meta_base_estimate_lower_bskep, mean(bskep_results$b_base_lower))
    meta_sex_estimate_lower_bskep <<- c(meta_sex_estimate_lower_bskep, mean(bskep_results$b_sex_lower))
    meta_cond_estimate_lower_bskep <<- c(meta_cond_estimate_lower_bskep, mean(bskep_results$b_cond_lower))
    meta_sex_cond_estimate_lower_bskep <<- c(meta_sex_cond_estimate_lower_bskep, mean(bskep_results$b_sex_cond_lower))
    
    if (pp_final_expt_only == FALSE) {
      meta_base_estimate_lower_pp <<- c(meta_base_estimate_lower_pp, mean(pp_results$b_base_lower))
      meta_sex_estimate_lower_pp <<- c(meta_sex_estimate_lower_pp, mean(pp_results$b_sex_lower))
      meta_cond_estimate_lower_pp <<- c(meta_cond_estimate_lower_pp, mean(pp_results$b_cond_lower))
      meta_sex_cond_estimate_lower_pp <<- c(meta_sex_cond_estimate_lower_pp, mean(pp_results$b_sex_cond_lower))
    } else {
      meta_base_estimate_lower_pp <<- c(meta_base_estimate_lower_pp, mean(pp_results$b_base_lower[pp_results$expt == n_experiments_per_repeat]))
      meta_sex_estimate_lower_pp <<- c(meta_sex_estimate_lower_pp, mean(pp_results$b_sex_lower[pp_results$expt == n_experiments_per_repeat]))
      meta_cond_estimate_lower_pp <<- c(meta_cond_estimate_lower_pp, mean(pp_results$b_cond_lower[pp_results$expt == n_experiments_per_repeat]))
      meta_sex_cond_estimate_lower_pp <<- c(meta_sex_cond_estimate_lower_pp, mean(pp_results$b_sex_cond_lower[pp_results$expt == n_experiments_per_repeat]))
    }
    
    meta_sex_positive_rate_bglmm <<- c(meta_sex_positive_rate_bglmm, mean(bglmm_results$b_sex_lower > 0 | bglmm_results$b_sex_upper < 0))
    meta_cond_positive_rate_bglmm <<- c(meta_cond_positive_rate_bglmm, mean(bglmm_results$b_cond_lower > 0 | bglmm_results$b_cond_upper < 0))
    meta_sex_cond_positive_rate_bglmm <<- c(meta_sex_cond_positive_rate_bglmm, mean(bglmm_results$b_sex_cond_lower > 0 | bglmm_results$b_sex_cond_upper < 0))
    
    #ulla
    meta_sex_positive_rate_bskep <<- c(meta_sex_positive_rate_bskep, mean(bskep_results$b_sex_lower > 0 | bskep_results$b_sex_upper < 0))
    meta_cond_positive_rate_bskep <<- c(meta_cond_positive_rate_bglmm, mean(bskep_results$b_cond_lower > 0 | bskep_results$b_cond_upper < 0))
    meta_sex_cond_positive_rate_bskep <<- c(meta_sex_cond_positive_rate_bglmm, mean(bskep_results$b_sex_cond_lower > 0 | bskep_results$b_sex_cond_upper < 0))
    
    if (pp_final_expt_only == FALSE) {
      meta_sex_positive_rate_pp <<- c(meta_sex_positive_rate_pp, mean(pp_results$b_sex_lower > 0 | pp_results$b_sex_upper < 0))
      meta_cond_positive_rate_pp <<- c(meta_cond_positive_rate_pp, mean(pp_results$b_cond_lower > 0 | pp_results$b_cond_upper < 0))
      meta_sex_cond_positive_rate_pp <<- c(meta_sex_cond_positive_rate_pp, mean(pp_results$b_sex_cond_lower > 0 | pp_results$b_sex_cond_upper < 0))
    } else {
      meta_sex_positive_rate_pp <<- c(meta_sex_positive_rate_pp, 1*(mean(pp_results$b_sex_lower[pp_results$expt == n_experiments_per_repeat]) > 0 | mean(pp_results$b_sex_upper[pp_results$expt == n_experiments_per_repeat]) < 0))
      meta_cond_positive_rate_pp <<- c(meta_cond_positive_rate_pp, 1*(mean(pp_results$b_cond_lower[pp_results$expt == n_experiments_per_repeat]) > 0 | mean(pp_results$b_cond_upper[pp_results$expt == n_experiments_per_repeat]) < 0))
      meta_sex_cond_positive_rate_pp <<- c(meta_sex_cond_positive_rate_pp, 1*(mean(pp_results$b_sex_cond_lower[pp_results$expt == n_experiments_per_repeat]) > 0 | mean(pp_results$b_sex_cond_upper[pp_results$expt == n_experiments_per_repeat]) < 0))
    }
  
    meta_base_uncertainty_bglmm <<- c(meta_base_uncertainty_bglmm, mean(bglmm_results$b_base_upper - bglmm_results$b_base_lower))
    meta_sex_uncertainty_bglmm <<- c(meta_sex_uncertainty_bglmm, mean(bglmm_results$b_sex_upper - bglmm_results$b_sex_lower))
    meta_cond_uncertainty_bglmm <<- c(meta_cond_uncertainty_bglmm, mean(bglmm_results$b_cond_upper - bglmm_results$b_cond_lower))
    meta_sex_cond_uncertainty_bglmm <<- c(meta_sex_cond_uncertainty_bglmm, mean(bglmm_results$b_sex_cond_upper - bglmm_results$b_sex_cond_lower))
    
    #ulla
    meta_base_uncertainty_bskep <<- c(meta_base_uncertainty_bskep, mean(bskep_results$b_base_upper - bskep_results$b_base_lower))
    meta_sex_uncertainty_bskep <<- c(meta_sex_uncertainty_bglmm, mean(bskep_results$b_sex_upper - bskep_results$b_sex_lower))
    meta_cond_uncertainty_bskep <<- c(meta_cond_uncertainty_bglmm, mean(bskep_results$b_cond_upper - bskep_results$b_cond_lower))
    meta_sex_cond_uncertainty_bskep <<- c(meta_sex_cond_uncertainty_bglmm, mean(bskep_results$b_sex_cond_upper - bskep_results$b_sex_cond_lower))
    
    if (pp_final_expt_only == FALSE) {
      meta_base_uncertainty_pp <<- c(meta_base_uncertainty_pp, mean(pp_results$b_base_upper - pp_results$b_base_lower))
      meta_sex_uncertainty_pp <<- c(meta_sex_uncertainty_pp, mean(pp_results$b_sex_upper - pp_results$b_sex_lower))
      meta_cond_uncertainty_pp <<- c(meta_cond_uncertainty_pp, mean(pp_results$b_cond_upper - pp_results$b_cond_lower))
      meta_sex_cond_uncertainty_pp <<- c(meta_sex_cond_uncertainty_pp, mean(pp_results$b_sex_cond_upper - pp_results$b_sex_cond_lower))
    } else {
      meta_base_uncertainty_pp <<- c(meta_base_uncertainty_pp, mean(pp_results$b_base_upper[pp_results$expt == n_experiments_per_repeat] - pp_results$b_base_lower[pp_results$expt == n_experiments_per_repeat]))
      meta_sex_uncertainty_pp <<- c(meta_sex_uncertainty_pp, mean(pp_results$b_sex_upper[pp_results$expt == n_experiments_per_repeat] - pp_results$b_sex_lower[pp_results$expt == n_experiments_per_repeat]))
      meta_cond_uncertainty_pp <<- c(meta_cond_uncertainty_pp, mean(pp_results$b_cond_upper[pp_results$expt == n_experiments_per_repeat] - pp_results$b_cond_lower[pp_results$expt == n_experiments_per_repeat]))
      meta_sex_cond_uncertainty_pp <<- c(meta_sex_cond_uncertainty_pp, mean(pp_results$b_sex_cond_upper[pp_results$expt == n_experiments_per_repeat] - pp_results$b_sex_cond_lower[pp_results$expt == n_experiments_per_repeat]))
    }
    
  }
}

compile_meta_results <- function() {
  meta_base_estimate_bglmm_p <- exp(meta_base_estimate_bglmm)/(1+exp(meta_base_estimate_bglmm))
  meta_sex_estimate_bglmm_p <- exp(meta_sex_estimate_bglmm+meta_base_estimate_bglmm)/(1+exp(meta_sex_estimate_bglmm+meta_base_estimate_bglmm)) - meta_base_estimate_bglmm_p
  meta_cond_estimate_bglmm_p <- exp(meta_cond_estimate_bglmm+meta_base_estimate_bglmm)/(1+exp(meta_cond_estimate_bglmm+meta_base_estimate_bglmm)) - meta_base_estimate_bglmm_p
  meta_sex_cond_estimate_bglmm_p <- exp(meta_sex_cond_estimate_bglmm + meta_sex_estimate_bglmm + meta_cond_estimate_bglmm + meta_base_estimate_bglmm)/(1+exp(meta_sex_cond_estimate_bglmm + meta_sex_estimate_bglmm + meta_cond_estimate_bglmm + meta_base_estimate_bglmm)) - (meta_base_estimate_bglmm_p + meta_sex_estimate_bglmm_p + meta_cond_estimate_bglmm_p)
  
  #ulla
  meta_base_estimate_bskep_p <- exp(meta_base_estimate_bskep)/(1+exp(meta_base_estimate_bskep))
  meta_sex_estimate_bskep_p <- exp(meta_sex_estimate_bskep+meta_base_estimate_bskep)/(1+exp(meta_sex_estimate_bskep+meta_base_estimate_bskep)) - meta_base_estimate_bskep_p
  meta_cond_estimate_bskep_p <- exp(meta_cond_estimate_bskep+meta_base_estimate_bskep)/(1+exp(meta_cond_estimate_bskep+meta_base_estimate_bskep)) - meta_base_estimate_bskep_p
  meta_sex_cond_estimate_bskep_p <- exp(meta_sex_cond_estimate_bskep + meta_sex_estimate_bskep + meta_cond_estimate_bskep + meta_base_estimate_bskep)/(1+exp(meta_sex_cond_estimate_bskep + meta_sex_estimate_bskep + meta_cond_estimate_bskep + meta_base_estimate_bskep)) - (meta_base_estimate_bskep_p + meta_sex_estimate_bskep_p + meta_cond_estimate_bskep_p)
  
  meta_base_estimate_pp_p <- exp(meta_base_estimate_pp)/(1+exp(meta_base_estimate_pp))
  meta_sex_estimate_pp_p <- exp(meta_sex_estimate_pp+meta_base_estimate_pp)/(1+exp(meta_sex_estimate_pp+meta_base_estimate_pp)) - meta_base_estimate_pp_p
  meta_cond_estimate_pp_p <- exp(meta_cond_estimate_pp+meta_base_estimate_pp)/(1+exp(meta_cond_estimate_pp+meta_base_estimate_pp)) - meta_base_estimate_pp_p
  meta_sex_cond_estimate_pp_p <- exp(meta_sex_cond_estimate_pp + meta_sex_estimate_pp + meta_cond_estimate_pp + meta_base_estimate_pp)/(1+exp(meta_sex_cond_estimate_pp + meta_sex_estimate_pp + meta_cond_estimate_pp + meta_base_estimate_pp)) - (meta_base_estimate_pp_p + meta_sex_estimate_pp_p + meta_cond_estimate_pp_p)
  
  meta_base_uncertainty_bglmm_p <- exp(meta_base_estimate_upper_bglmm)/(1+exp(meta_base_estimate_upper_bglmm)) - exp(meta_base_estimate_lower_bglmm)/(1+exp(meta_base_estimate_lower_bglmm))
  meta_sex_uncertainty_bglmm_p <- exp(meta_sex_estimate_upper_bglmm+meta_base_estimate_bglmm)/(1+exp(meta_sex_estimate_upper_bglmm+meta_base_estimate_bglmm)) - exp(meta_sex_estimate_lower_bglmm+meta_base_estimate_bglmm)/(1+exp(meta_sex_estimate_lower_bglmm+meta_base_estimate_bglmm))
  meta_cond_uncertainty_bglmm_p <- exp(meta_cond_estimate_upper_bglmm+meta_base_estimate_bglmm)/(1+exp(meta_cond_estimate_upper_bglmm+meta_base_estimate_bglmm)) - exp(meta_cond_estimate_lower_bglmm+meta_base_estimate_bglmm)/(1+exp(meta_cond_estimate_lower_bglmm+meta_base_estimate_bglmm))
  meta_sex_cond_uncertainty_bglmm_p <- exp(meta_sex_cond_estimate_upper_bglmm + meta_sex_estimate_bglmm + meta_cond_estimate_bglmm +meta_base_estimate_bglmm)/(1+exp(meta_sex_cond_estimate_upper_bglmm + meta_sex_estimate_bglmm + meta_cond_estimate_bglmm +meta_base_estimate_bglmm)) - exp(meta_sex_cond_estimate_lower_bglmm + meta_sex_estimate_bglmm + meta_cond_estimate_bglmm +meta_base_estimate_bglmm)/(1+exp(meta_sex_cond_estimate_lower_bglmm + meta_sex_estimate_bglmm + meta_cond_estimate_bglmm +meta_base_estimate_bglmm))
  
  #ulla
  meta_base_uncertainty_bskep_p <- exp(meta_base_estimate_upper_bskep)/(1+exp(meta_base_estimate_upper_bskep)) - exp(meta_base_estimate_lower_bskep)/(1+exp(meta_base_estimate_lower_bskep))
  meta_sex_uncertainty_bskep_p <- exp(meta_sex_estimate_upper_bskep+meta_base_estimate_bskep)/(1+exp(meta_sex_estimate_upper_bskep+meta_base_estimate_bskep)) - exp(meta_sex_estimate_lower_bskep+meta_base_estimate_bskep)/(1+exp(meta_sex_estimate_lower_bskep+meta_base_estimate_bskep))
  meta_cond_uncertainty_bskep_p <- exp(meta_cond_estimate_upper_bskep+meta_base_estimate_bskep)/(1+exp(meta_cond_estimate_upper_bskep+meta_base_estimate_bskep)) - exp(meta_cond_estimate_lower_bskep+meta_base_estimate_bskep)/(1+exp(meta_cond_estimate_lower_bskep+meta_base_estimate_bskep))
  meta_sex_cond_uncertainty_bskep_p <- exp(meta_sex_cond_estimate_upper_bskep + meta_sex_estimate_bskep + meta_cond_estimate_bskep +meta_base_estimate_bskep)/(1+exp(meta_sex_cond_estimate_upper_bskep + meta_sex_estimate_bskep + meta_cond_estimate_bskep +meta_base_estimate_bskep)) - exp(meta_sex_cond_estimate_lower_bskep + meta_sex_estimate_bskep + meta_cond_estimate_bskep +meta_base_estimate_bskep)/(1+exp(meta_sex_cond_estimate_lower_bskep + meta_sex_estimate_bskep + meta_cond_estimate_bskep +meta_base_estimate_bskep))
  
  meta_base_uncertainty_pp_p <- exp(meta_base_estimate_upper_pp)/(1+exp(meta_base_estimate_upper_pp)) - exp(meta_base_estimate_lower_pp)/(1+exp(meta_base_estimate_lower_pp))
  meta_sex_uncertainty_pp_p <- exp(meta_sex_estimate_upper_pp+meta_base_estimate_pp)/(1+exp(meta_sex_estimate_upper_pp+meta_base_estimate_pp)) - exp(meta_sex_estimate_lower_pp+meta_base_estimate_pp)/(1+exp(meta_sex_estimate_lower_pp+meta_base_estimate_pp))
  meta_cond_uncertainty_pp_p <- exp(meta_cond_estimate_upper_pp+meta_base_estimate_pp)/(1+exp(meta_cond_estimate_upper_pp+meta_base_estimate_pp)) - exp(meta_cond_estimate_lower_pp+meta_base_estimate_pp)/(1+exp(meta_cond_estimate_lower_pp+meta_base_estimate_pp))
  meta_sex_cond_uncertainty_pp_p <- exp(meta_sex_cond_estimate_upper_pp + meta_sex_estimate_pp + meta_cond_estimate_pp +meta_base_estimate_pp)/(1+exp(meta_sex_cond_estimate_upper_pp + meta_sex_estimate_pp + meta_cond_estimate_pp +meta_base_estimate_pp)) - exp(meta_sex_cond_estimate_lower_pp + meta_sex_estimate_pp + meta_cond_estimate_pp +meta_base_estimate_pp)/(1+exp(meta_sex_cond_estimate_lower_pp + meta_sex_estimate_pp + meta_cond_estimate_pp +meta_base_estimate_pp))
  
  return(data.frame(meta_n_repeats,meta_n_experiments_per_repeat, meta_n_participants_per_experiment,
                    meta_n_trials_per_participant, meta_n_people,
                    meta_true_base, meta_true_sex, meta_true_cond, meta_true_sex_cond, 
                    meta_var_base, meta_var_sex, meta_var_cond, meta_var_sex_cond,
                    meta_base_estimate_bglmm, meta_sex_estimate_bglmm, meta_cond_estimate_bglmm, meta_sex_cond_estimate_bglmm,
                    meta_base_estimate_bglmm_p, meta_sex_estimate_bglmm_p, meta_cond_estimate_bglmm_p, meta_sex_cond_estimate_bglmm_p,
                    meta_base_estimate_bskep, meta_sex_estimate_bskep, meta_cond_estimate_bskep, meta_sex_cond_estimate_bskep,
                    meta_base_estimate_bskep_p, meta_sex_estimate_bskep_p, meta_cond_estimate_bskep_p, meta_sex_cond_estimate_bskep_p,
                    meta_base_estimate_pp, meta_sex_estimate_pp, meta_cond_estimate_pp, meta_sex_cond_estimate_pp,
                    meta_base_estimate_pp_p, meta_sex_estimate_pp_p, meta_cond_estimate_pp_p, meta_sex_cond_estimate_pp_p,
                    meta_base_estimate_lower_bglmm, meta_sex_estimate_lower_bglmm, meta_cond_estimate_lower_bglmm, meta_sex_cond_estimate_lower_bglmm,
                    meta_base_estimate_lower_bskep, meta_sex_estimate_lower_bskep, meta_cond_estimate_lower_bskep, meta_sex_cond_estimate_lower_bskep,
                    meta_base_estimate_lower_pp, meta_sex_estimate_lower_pp, meta_cond_estimate_lower_pp, meta_sex_cond_estimate_lower_pp,
                    meta_base_estimate_upper_bglmm, meta_sex_estimate_upper_bglmm, meta_cond_estimate_upper_bglmm, meta_sex_cond_estimate_upper_bglmm,
                    meta_base_estimate_upper_bskep, meta_sex_estimate_upper_bskep, meta_cond_estimate_upper_bskep, meta_sex_cond_estimate_upper_bskep,
                    meta_base_estimate_upper_pp, meta_sex_estimate_upper_pp, meta_cond_estimate_upper_pp, meta_sex_cond_estimate_upper_pp,
                    meta_sex_positive_rate_bglmm, meta_cond_positive_rate_bglmm, meta_sex_cond_positive_rate_bglmm,
                    meta_sex_positive_rate_bskep, meta_cond_positive_rate_bskep, meta_sex_cond_positive_rate_bskep,
                    meta_sex_positive_rate_pp, meta_cond_positive_rate_pp, meta_sex_cond_positive_rate_pp,
                    meta_base_uncertainty_bglmm, meta_sex_uncertainty_bglmm, meta_cond_uncertainty_bglmm, meta_sex_cond_uncertainty_bglmm,
                    meta_base_uncertainty_bglmm_p, meta_sex_uncertainty_bglmm_p, meta_cond_uncertainty_bglmm_p, meta_sex_cond_uncertainty_bglmm_p,
                    meta_base_uncertainty_bskep, meta_sex_uncertainty_bskep, meta_cond_uncertainty_bskep, meta_sex_cond_uncertainty_bskep,
                    meta_base_uncertainty_bskep_p, meta_sex_uncertainty_bskep_p, meta_cond_uncertainty_bskep_p, meta_sex_cond_uncertainty_bskep_p,
                    meta_base_uncertainty_pp, meta_sex_uncertainty_pp, meta_cond_uncertainty_pp, meta_sex_cond_uncertainty_pp,
                    meta_base_uncertainty_pp_p, meta_sex_uncertainty_pp_p, meta_cond_uncertainty_pp_p, meta_sex_cond_uncertainty_pp_p))
}

tidy_workspace <- function() {
  rm(meta_n_repeats,meta_n_experiments_per_repeat, meta_n_participants_per_experiment,
     meta_n_trials_per_participant, meta_n_people,
     meta_true_base, meta_true_sex, meta_true_cond, meta_true_sex_cond, 
     meta_var_base, meta_var_sex, meta_var_cond, meta_var_sex_cond,
     meta_base_estimate_bglmm, meta_sex_estimate_bglmm, meta_cond_estimate_bglmm, meta_sex_cond_estimate_bglmm,
     meta_base_estimate_bglmm_p, meta_sex_estimate_bglmm_p, meta_cond_estimate_bglmm_p, meta_sex_cond_estimate_bglmm_p,
     meta_base_estimate_bskep, meta_sex_estimate_bskep, meta_cond_estimate_bskep, meta_sex_cond_estimate_bskep,
     meta_base_estimate_bskep_p, meta_sex_estimate_bskep_p, meta_cond_estimate_bskep_p, meta_sex_cond_estimate_bskep_p,
     meta_base_estimate_pp, meta_sex_estimate_pp, meta_cond_estimate_pp, meta_sex_cond_estimate_pp,
     meta_base_estimate_pp_p, meta_sex_estimate_pp_p, meta_cond_estimate_pp_p, meta_sex_cond_estimate_pp_p,
     meta_base_estimate_lower_bglmm, meta_sex_estimate_lower_bglmm, meta_cond_estimate_lower_bglmm, meta_sex_cond_estimate_lower_bglmm,
     meta_base_estimate_lower_bskep, meta_sex_estimate_lower_bskep, meta_cond_estimate_lower_bskep, meta_sex_cond_estimate_lower_bskep,
     meta_base_estimate_lower_pp, meta_sex_estimate_lower_pp, meta_cond_estimate_lower_pp, meta_sex_cond_estimate_lower_pp,
     meta_base_estimate_upper_bskep, meta_sex_estimate_upper_bskep, meta_cond_estimate_upper_bskep, meta_sex_cond_estimate_upper_bskep,
     meta_base_estimate_upper_bglmm, meta_sex_estimate_upper_bglmm, meta_cond_estimate_upper_bglmm, meta_sex_cond_estimate_upper_bglmm,
     meta_base_estimate_upper_pp, meta_sex_estimate_upper_pp, meta_cond_estimate_upper_pp, meta_sex_cond_estimate_upper_pp,
     meta_sex_positive_rate_bglmm, meta_cond_positive_rate_bglmm, meta_sex_cond_positive_rate_bglmm,
     meta_sex_positive_rate_bskep, meta_cond_positive_rate_bskep, meta_sex_cond_positive_rate_bskep,
     meta_sex_positive_rate_pp, meta_cond_positive_rate_pp, meta_sex_cond_positive_rate_pp,
     meta_base_uncertainty_bglmm, meta_sex_uncertainty_bglmm, meta_cond_uncertainty_bglmm, meta_sex_cond_uncertainty_bglmm,
     meta_base_uncertainty_bglmm_p, meta_sex_uncertainty_bglmm_p, meta_cond_uncertainty_bglmm_p, meta_sex_cond_uncertainty_bglmm_p,
     meta_base_uncertainty_bskep, meta_sex_uncertainty_bskep, meta_cond_uncertainty_bskep, meta_sex_cond_uncertainty_bskep,
     meta_base_uncertainty_bskep_p, meta_sex_uncertainty_bskep_p, meta_cond_uncertainty_bskep_p, meta_sex_cond_uncertainty_bskep_p,
     meta_base_uncertainty_pp, meta_sex_uncertainty_pp, meta_cond_uncertainty_pp, meta_sex_cond_uncertainty_pp,
     meta_base_uncertainty_pp_p, meta_sex_uncertainty_pp_p, meta_cond_uncertainty_pp_p, meta_sex_cond_uncertainty_pp_p,
     b_base, b_bases, b_cond, b_conds, b_sex, b_sexs, b_sex_cond, b_sex_conds,
     var_base, var_bases, var_cond, var_conds, var_Sex, var_sexs, var_sex_cond, var_sex_conds,
     pos = ".GlobalEnv")
}

prepare_for_simulation <- function() {
  b_base <<- b_bases[i]
  b_sex <<- b_sexs[j]
  b_cond <<- b_conds[k]
  b_sex_cond <<- b_sex_conds[l]
  var_base <<- var_bases[m]
  var_sex <<- var_sexs[n]
  var_cond <<- var_conds[o]
  var_sex_cond <<- var_sex_conds[p]
  meta_true_base <<- c(meta_true_base, rep(b_base, n_repeats))
  meta_true_sex <<- c(meta_true_sex, rep(b_sex, n_repeats))
  meta_true_cond <<- c(meta_true_cond, rep(b_cond, n_repeats))
  meta_true_sex_cond <<- c(meta_true_sex_cond, rep(b_sex_cond, n_repeats))
  meta_var_base <<- c(meta_var_base, rep(var_base, n_repeats))
  meta_var_sex <<- c(meta_var_sex, rep(var_sex, n_repeats))
  meta_var_cond <<- c(meta_var_cond, rep(var_cond, n_repeats))
  meta_var_sex_cond <<- c(meta_var_sex_cond, rep(var_sex_cond, n_repeats))
  print(paste("running simulation with parameters: b_base: ", b_base,
              ", b_sex: ", b_sex,
              ", b_cond: ", b_cond,
              ", b_sex_cond: ", b_sex_cond,
              ", var_base: ", var_base,
              ", var_sex: ", var_sex,
              ", var_cond: ", var_cond,
              ", var_sex_cond: ", var_sex_cond,
              sep=" "))
}