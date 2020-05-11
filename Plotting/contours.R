plot_contours <- function() {

  library(reshape2)
  
  # un-comment this line to load in the published results
  meta_results <- read.delim("meta_results_18_09_16.txt")
  combined_results <- meta_results
  
  x <- combined_results$meta_true_sex_cond
  y <- combined_results$meta_var_base
  
  xvals <- round(1/(1+exp(-b_sex_conds)) - 0.5, 2)
  yvals <- var_bases
  
  do_plot <- function(z, m) {
    matrix <- data.frame(x=x, y=y, z=z)
    matrix2 <- acast(matrix, x~y, value.var="z", fun.aggregate=mean)
    filled.contour(matrix2,
                   x=xvals,
                   y=yvals,
                   xlim = c(min(xvals), max(xvals)), ylim = c(min(yvals), max(yvals)), zlim=c(-0.02, 0.5),
                   color.palette = colorRampPalette(c("blue","white","yellow")),
                   xlab="true effect", ylab="variance",
                   main=m)
  }
  
  if (do_anova) {
    do_plot(z=combined_results$meta_sex_cond_estimate_anova, m="anova estimate")
  }
  if (do_glmm) {
    do_plot(z=combined_results$meta_sex_cond_estimate_glmm_p, m="glmm estimate")
  }
  if (do_bglmm) {
    do_plot(z=combined_results$meta_sex_cond_estimate_bglmm_p, m="bglmm estimate")
  }
  if (do_pp) {
    do_plot(z=combined_results$meta_sex_cond_estimate_pp_p, m="pp estimate")
  }
  if (do_mega_bglmm) {
    do_plot(z=combined_results$meta_sex_cond_estimate_mega_bglmm_p, m="meta-bglmm estimate")
  }
  
  do_plot <- function(z, m) {
    matrix <- data.frame(x=x, y=y, z=z)
    matrix2 <- acast(matrix, x~y, value.var="z", fun.aggregate=mean)
    filled.contour(matrix2,
                   x=xvals,
                   y=yvals,
                   xlim = c(min(xvals), max(xvals)), ylim = c(min(yvals), max(yvals)), zlim=c(0, 1),
                   color.palette = colorRampPalette(c("blue","white","yellow")),
                   xlab="true effect", ylab="variance",
                   main=m)
  }
  
  if (do_anova) {
    do_plot(z=combined_results$meta_sex_cond_positive_rate_anova, m="anova positive rate")
  }
  if (do_glmm) {
    do_plot(z=combined_results$meta_sex_cond_positive_rate_glmm, m="glmm positive rate")
  }
  if (do_bglmm) {
    do_plot(z=combined_results$meta_sex_cond_positive_rate_bglmm, m="bglmm positive rate")
  }
  if (do_pp) {
    do_plot(z=combined_results$meta_sex_cond_positive_rate_pp, m="pp positive rate")
  }
  if (do_mega_bglmm) {
    do_plot(z=combined_results$meta_sex_cond_positive_rate_mega_bglmm, m="meta-bglmm positive rate")
  }
  
  do_plot <- function(z, m) {
    matrix <- data.frame(x=x, y=y, z=z)
    matrix2 <- acast(matrix, x~y, value.var="z", fun.aggregate=mean)
    filled.contour(matrix2,
                   x=xvals,
                   y=yvals,
                   xlim = c(min(xvals), max(xvals)), ylim = c(min(yvals), max(yvals)), zlim=c(0, 0.5),
                   color.palette = colorRampPalette(c("blue","white","yellow")),
                   xlab="true effect", ylab="variance",
                   main=m)
  }
  
  if (do_anova) {
    do_plot(z=combined_results$meta_sex_cond_uncertainty_anova, m="anova uncertainty")
  }
  if (do_glmm) {
    do_plot(z=combined_results$meta_sex_cond_uncertainty_glmm_p, m="glmm uncertainty")
  }
  if (do_bglmm) {
    do_plot(z=combined_results$meta_sex_cond_uncertainty_bglmm_p, m="bglmm uncertainty")
  }
  if (do_pp) {
    do_plot(z=combined_results$meta_sex_cond_uncertainty_pp_p, m="pp uncertainty")
  }
  if (do_mega_bglmm) {
    do_plot(z=combined_results$meta_sex_cond_uncertainty_mega_bglmm_p, m="meta-bglmm uncertainty")
  }
  
  do_plot <- function(z, m) {
    matrix <- data.frame(x=x, y=y, z=z)
    matrix2 <- acast(matrix, x~y, value.var="z", fun.aggregate=mean)
    filled.contour(matrix2,
                   x=xvals,
                   y=yvals,
                   xlim = c(min(xvals), max(xvals)), ylim = c(min(yvals), max(yvals)), zlim=c(-0.1, 0.1),
                   color.palette = colorRampPalette(c("blue","white","yellow")),
                   xlab="true effect", ylab="variance",
                   main=m)
  }
  
  if (do_anova) {
    do_plot(z=combined_results$meta_sex_cond_estimate_anova-(exp(x)/(1+exp(x))-0.5), m="anova error")
  }
  if (do_glmm) {
    do_plot(z=combined_results$meta_sex_cond_estimate_glmm_p-(exp(x)/(1+exp(x))-0.5), m="glmm error")
  }
  if (do_bglmm) {
    do_plot(z=combined_results$meta_sex_cond_estimate_bglmm_p-(exp(x)/(1+exp(x))-0.5), m="bglmm error")
  }
  if (do_pp) {
    do_plot(z=combined_results$meta_sex_cond_estimate_pp_p-(exp(x)/(1+exp(x))-0.5), m="pp error")
  }
  if (do_mega_bglmm) {
    do_plot(z=combined_results$meta_sex_cond_estimate_mega_bglmm_p-(exp(x)/(1+exp(x))-0.5), m="meta-bglmm error")
  }
}
  
  
