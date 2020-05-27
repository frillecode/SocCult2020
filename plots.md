### Load Packages and Data

``` r
library(pacman)
pacman::p_load(
  tidyverse,
  gridExtra,
  patchwork, formattable, knitr
)

### Load data ###

d000 <- read_csv("results/saved_results_000.csv")
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
d001 <- read_csv("results/saved_results_1.csv") %>% mutate(repeat_id = 2)
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
d002 <- read_csv("results/saved_results_2.csv") %>% mutate(repeat_id = 3) 
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
d003 <- read_csv("results/saved_results_3.csv") %>% mutate(repeat_id = 4) 
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
d004 <- read_csv("results/saved_results_4.csv") %>% mutate(repeat_id = 5)
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
d100 <- read_csv("results/saved_results_100.csv")  
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
d101 <- read_csv("results/saved_results_101.csv") %>% mutate(repeat_id = 2) 
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
d102 <- read_csv("results/saved_results_102.csv") %>% mutate(repeat_id = 3) 
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
d103 <- read_csv("results/saved_results_103.csv") %>% mutate(repeat_id = 4) 
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
d104 <- read_csv("results/saved_results_104.csv") %>% mutate(repeat_id = 5) 
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
d200 <- read_csv("results/saved_results_200.csv") 
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
d201 <- read_csv("results/saved_results_201.csv") %>% mutate(repeat_id = 2) 
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
d202 <- read_csv("results/saved_results_202.csv") %>% mutate(repeat_id = 3) 
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
d203 <- read_csv("results/saved_results_203.csv") %>% mutate(repeat_id = 4) 
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
d204 <- read_csv("results/saved_results_204.csv") %>% mutate(repeat_id = 5)  
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
d <- rbind(d000,d001,d002,d003,d004, 
           d100,d101,d102,d103,d104,
           d200,d201, d202,d203,d204) %>% 
     select(-c(X1))  

#meta-analysis
meta000 <- read_csv("results/meta_analysis_results_000.csv")
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
meta001 <- read_csv("results/meta_analysis_results_1.csv") %>% mutate(meta_repeat_id = 2) 
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
meta002 <- read_csv("results/meta_analysis_results_2.csv") %>% mutate(meta_repeat_id = 3)
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
meta003 <- read_csv("results/meta_analysis_results_3.csv") %>% mutate(meta_repeat_id = 4)
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
meta004 <- read_csv("results/meta_analysis_results_4.csv") %>% mutate(meta_repeat_id = 5) 
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
meta100 <- read_csv("results/meta_analysis_results_100.csv") 
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
meta101 <- read_csv("results/meta_analysis_results_101.csv") %>% mutate(meta_repeat_id = 2)
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
meta102 <- read_csv("results/meta_analysis_results_102.csv") %>% mutate(meta_repeat_id = 3)
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
meta103 <- read_csv("results/meta_analysis_results_103.csv") %>% mutate(meta_repeat_id = 4)
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
meta104 <- read_csv("results/meta_analysis_results_104.csv") %>% mutate(meta_repeat_id = 5)
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
meta200 <- read_csv("results/meta_analysis_results_200.csv") 
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
meta201 <- read_csv("results/meta_analysis_results_201.csv") %>% mutate(meta_repeat_id = 2)
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
meta202 <- read_csv("results/meta_analysis_results_202.csv") %>% mutate(meta_repeat_id = 3)
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
meta203 <- read_csv("results/meta_analysis_results_203.csv") %>% mutate(meta_repeat_id = 4)
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
meta204 <- read_csv("results/meta_analysis_results_204.csv") %>% mutate(meta_repeat_id = 5)
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
#merge data
meta <- rbind(meta000,meta001,meta002,meta003,meta004,
              meta100,meta101,meta102, meta103,meta104,
              meta200,meta201,meta202,meta203,meta204) %>% 
  mutate(analysis_type = "meta-analysis", true_base = 0) %>% 
  rename(
    repeat_id = meta_repeat_id,
    b_sex_cond = b_sex_cond_meta,
    b_sex_cond_lower = b_sex_cond_lower_meta,
    b_sex_cond_upper = b_sex_cond_upper_meta,
    b_sex_cond_error = b_sex_cond_error_meta,
    true_sex_cond = true_effect) %>% 
              select(-c(X1))  
```

### Summary tables

APPENDIX 2

``` r
# Summary of estimates from last pp
d_last_pp <- d %>%  group_by(true_sex_cond, pb_true) %>% 
  filter(expt == max(expt), analysis_type == "pp", repeat_id == 1) %>%  #change repeat here
  summarise(
    repeat_id = repeat_id,
    estimate = b_sex_cond_med,
    CI_lower = b_sex_cond_lower,
    CI_upper = b_sex_cond_upper, 
    analysis_type = "pp"
    )

# Summary of estimates from bglmm and bskep
d_mean <- d %>% filter(analysis_type != "pp", repeat_id == 1) %>%  #change repeat here
  group_by(true_sex_cond, analysis_type) %>% 
  summarize(
    repeat_id = mean(repeat_id),
    estimate = mean(b_sex_cond_med),
    CI_lower = mean(b_sex_cond_lower),
    CI_upper = mean(b_sex_cond_upper),
    pb_true = mean(pb_true)
  )

# Merging summaries
d_summarized <- rbind(d_last_pp, d_mean) %>% 
  mutate(dist = estimate - (boot::inv.logit(true_sex_cond) - 0.5),
    dist_lower = CI_lower - (boot::inv.logit(true_sex_cond) - 0.5),
    dist_upper = CI_upper - (boot::inv.logit(true_sex_cond) - 0.5))

# Fix order
d_summarized <- d_summarized[c(3, 2, 7, 1, 4, 5, 6, 8, 9, 10)]
```

APPENDIX 3

``` r
meta_pp <- meta %>% group_by(repeat_id) %>% rename(
    estimate = b_sex_cond,
    CI_lower = b_sex_cond_lower,
    CI_upper = b_sex_cond_upper
  ) %>% select(
    analysis_type, pb_true, estimate, CI_lower, CI_upper, true_sex_cond, n_exp, repeat_id
  )

d_last_pp <- d %>%  group_by(true_sex_cond, pb_true, repeat_id) %>% 
  mutate(n_exp = sum(pp_true))%>% 
  filter(expt == max(expt), analysis_type == "pp") %>% 
  summarise(
    estimate = b_sex_cond_med,
    CI_lower = b_sex_cond_lower,
    CI_upper = b_sex_cond_upper, 
    analysis_type = "pp",
    n_exp
    )

meta_pp <- rbind(d_last_pp, meta_pp) %>% 
  mutate(
    dist = estimate - (boot::inv.logit(true_sex_cond) - 0.5),
    dist_lower = CI_lower - (boot::inv.logit(true_sex_cond) - 0.5),
    dist_upper = CI_upper - (boot::inv.logit(true_sex_cond) - 0.5)
  )



mean_meta_pp <- meta_pp %>% 
  group_by(pb_true, analysis_type, true_sex_cond) %>% 
  summarize(
    mean_estimate = round(mean(estimate),3),
    sd_estimate = round(sd(estimate),3),
    mean_uncertainty = round(mean(CI_upper - CI_lower),3),
    sd_uncertainty = round(sd(CI_upper - CI_lower),3),
    mean_dist = round(mean(abs(dist)),3),
    sd_dist = round(sd(abs(dist)),3), 
    mean_exp = round(mean(n_exp),2)
  ) 
```

TABLE 3

``` r
mean_pb_meta_pp <-  meta_pp %>% 
  group_by(pb_true, analysis_type) %>% 
  summarize(
    mean_uncertainty = round(mean(CI_upper - CI_lower),3),
    sd_uncertainty = round(sd(CI_upper - CI_lower),3),
    mean_dist = round(mean(abs(dist)),3),
    sd_dist = round(sd(abs(dist)),3), 
    mean_exp = round(mean(n_exp),2)
  )
```

### Plots

FIGURE 2

``` r
# Remove data with publication bias
d_pb0 <- d[d$pb_true == 0,]
d_pb1 <- d[d$pb_true == 1,]

#### PLOTTING ####

# Plotting true effect 0
te0_pb0_plot <- ggplot(data = d_pb0[d_pb0$true_sex_cond == 0,]) + 
  geom_point(aes(x = expt, y = b_sex_cond_med, color = analysis_type), size = 1) +
  geom_errorbar(aes(x = expt, ymax = b_sex_cond_upper, ymin = b_sex_cond_lower, color = analysis_type, width = 1), alpha = 0.5, size = 0.8) +
  geom_line(aes(x = as.numeric(expt), y = (boot::inv.logit(true_sex_cond) - boot::inv.logit(true_base))), 
            color = 'black', size = 1) +
  scale_color_manual(values=c("goldenrod2", "seagreen4", "red"), labels = c("Naïve", "Skeptical", "PP")) +
  labs(x = "Experiment number", y = "Interaction Estimate", color = "Analysis Type")  + 
  ggtitle("A") + 
  theme_minimal()

# Plotting true effect 1
te1_pb0_plot <- ggplot(data = d_pb0[d_pb0$true_sex_cond == 1,]) + 
  geom_point(aes(x = expt, y = b_sex_cond_med, color = analysis_type), size = 1) +
  geom_errorbar(aes(x = expt, ymax = b_sex_cond_upper, ymin = b_sex_cond_lower, color = analysis_type, width = 1), alpha = 0.5, size = 0.8) +
  geom_line(aes(x = as.numeric(expt), y = (boot::inv.logit(true_sex_cond) - boot::inv.logit(true_base))), 
            color = 'black', size = 1) +
  scale_color_manual(values=c("goldenrod2", "seagreen4", "red"), labels = c("Naïve", "Skeptical", "PP")) +
  labs(x = "Experiment number", y = "Interaction Estimate", color = "Analysis Type") + 
  ggtitle("B") + 
  theme_minimal()

# Plotting true effect 2
te2_pb0_plot <- ggplot(data =  d_pb0[d_pb0$true_sex_cond == 2,]) + 
  geom_point(aes(x = expt, y = b_sex_cond_med, color = analysis_type), size = 1) +
  geom_errorbar(aes(x = expt, ymax = b_sex_cond_upper, ymin = b_sex_cond_lower, color = analysis_type, width = 1), alpha = 0.5, size = 0.8) +
  geom_line(aes(x = as.numeric(expt), y = (boot::inv.logit(true_sex_cond) - boot::inv.logit(true_base))), 
            color = 'black', size = 1) +
  scale_color_manual(values=c("goldenrod2", "seagreen4", "red"), labels = c("Naïve", "Skeptical", "PP")) +
  labs(x = "Experiment number", y = "Interaction Estimate", color = "Analysis Type") + 
  ggtitle("C") + 
  theme_minimal()


# Arranging plots
combined_te_pb0_plot <- te0_pb0_plot + te1_pb0_plot + te2_pb0_plot & theme(legend.position = "bottom")
combined_te_pb0_plot + plot_layout(guides = "collect")
```

![](plots_files/figure-markdown_github/figure_2-1.png)

FIGURE 3

``` r
no_pb <- ggplot(data = meta_pp[meta_pp$pb_true == 0,]) + 
  geom_point(aes(x = repeat_id, y = dist, color = analysis_type, shape = as.factor(true_sex_cond)), size = 2) +
  geom_errorbar(aes(x = repeat_id, ymax = dist_upper, ymin = dist_lower, color = analysis_type, width = 0.2), alpha = 0.5, size = 0.8) +
  geom_line(aes(x = as.numeric(repeat_id), y = 0), color = 'black', size = 1)+
  scale_color_manual(values=c("goldenrod2", "cyan4")) +
  scale_y_continuous(limit= c(-0.15, 0.05))+
  labs(title = "A",
       x = "Repeat Number", y = "Distance from True Effect", 
       color = "Analysis Type", shape = "True Effect") +
  theme_minimal()

pb <- ggplot(data = meta_pp[meta_pp$pb_true == 1,]) + 
  geom_point(aes(x = repeat_id, y = dist, color = analysis_type, shape = as.factor(true_sex_cond)), size = 2) +
  geom_errorbar(aes(x = repeat_id, ymax = dist_upper, ymin = dist_lower, color = analysis_type, width = 0.2), alpha = 0.5, size = 0.8) +
  geom_line(aes(x = as.numeric(repeat_id), y = 0), color = 'black', size = 1)+
  scale_color_manual(values=c("goldenrod2", "cyan4")) +
  scale_y_continuous(limit= c(-0.15, 0.05))+
  labs(title = "B",
       x = "Repeat Number", y = "Distance from True Effect", 
       color = "Analysis Type", shape = "True Effect") +
  theme_minimal()

pp_meta_plot <- no_pb + pb & theme(legend.position = "bottom")
pp_meta_plot + plot_layout(guides = "collect") 
```

![](plots_files/figure-markdown_github/figure_3-1.png)

FIGURE 4

``` r
# Isolating pp data
d_pp <- d[d$analysis_type == "pp",]

d_pp$abs_dist <- abs(d_pp$b_sex_cond_med - (boot::inv.logit(d_pp$true_sex_cond)-boot::inv.logit(d_pp$true_base))) 

#### PLOTTING ####

# pp distance from true effect (without publication bias)
pp_pb0_plot <- 
  ggplot(data = d_pp[d_pp$pb_true == 0,], aes(x = expt, y = abs_dist, color = as.factor(true_sex_cond)), size = 1) + 
  geom_point(alpha = 0.8, size = 0.5) + geom_line(size = 0.2, alpha = 0.7) +  
  scale_color_manual(values=c("dodgerblue3", "mediumseagreen", "goldenrod1")) +
  labs(x = "Experiment number", y = "Distance from true effect", color = "True Effect",
       title = "A")  + 
  #scale_y_continuous(limit= c(0, 0.11)) +
  theme_minimal() + theme(legend.position = "bottom") 

# pp distance from true effect (with publication bias)
pp_pb1_plot <- 
  ggplot(data = d_pp[d_pp$pb_true == 1,], aes(x = expt, y = abs_dist, color = as.factor(true_sex_cond)), size = 1) + 
  geom_point(alpha = 0.8, size = 0.5) + geom_line(size = 0.2, alpha = 0.7) + 
  scale_color_manual(values=c("dodgerblue3", "mediumseagreen", "goldenrod1")) +
  labs(x = "Experiment number", y = "Distance from true effect", color = "True Effect", 
       title = "B")  + 
  #scale_y_continuous(limit= c(0, 0.11)) +
  theme_minimal() + theme(legend.position = "bottom") 

# Arranging plots
pp_pb_plot <- pp_pb0_plot + pp_pb1_plot & theme(legend.position = "bottom")
pp_pb_plot + plot_layout(guides = "collect") 
```

![](plots_files/figure-markdown_github/figure_4-1.png)
