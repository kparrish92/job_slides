source(here::here("scripts", "00_libs.R"))


### Effect size function ####################################
### ----------------------------------------------------- ###
### This function calculates Cohen's d given two means and 
### two sds
### ----------------------------------------------------- ###

effect_size = function(mean1, mean2, sd1, sd2)
{
  mean_diff = mean1 - mean2
  pooled_sd = sqrt(((sd1^2 + sd2^2)/2))
  
  es = mean_diff/pooled_sd  
  return(es)
}  


### Power analysis function #################################
### ----------------------------------------------------- ###
###  This function runs a power analysis for one sample size
###  mean1/mean2 are the means of each group
###  sd1/sd2 are the sds of each group
###  i is the number of iterations (data simulations) 
###  run to test power (100+ recommended)
###  n1/n2 are the number of participants per group 
###  (or observations for within subjects)
### ----------------------------------------------------- ###

power_analysis_single = function(mean1, sd1, mean2, sd2, i, n1, n2)
{
  loop_df = matrix(nrow = i)
  
  for(thisRun in 1:i){
    sample_1 = rnorm(n1, m = mean1, sd = sd1)
    sample_2 = rnorm(n2, m = mean2, sd = sd2)
    t_test = t.test(sample_1,sample_2)
    loop_df[thisRun] = t_test$p.value
    
  }
  return(sum(loop_df < .05)/i)
}

### Plot power curve function ###############################
### ----------------------------------------------------- ###
### This function runs plots a power curve by running a 
### power analysis on multiple sample sizes for a given 
### effect. The sizes are a vector.
### ----------------------------------------------------- ###

plot_power_curve = function(mean1, sd1, mean2, sd2, sample_sizes)
{
  loop_df = matrix(nrow = i)
  sizes = sample_sizes
  container_df = matrix(nrow = length(sizes), ncol = 2)
  
  
  for(thisSize in 1:nrow(container_df)){
    
    for(thisRun in 1:i){
      sample_1 = rnorm(sizes[thisSize], m = mean1, sd = sd1)
      sample_2 = rnorm(sizes[thisSize], m = mean2, sd = sd2)
      t_test = t.test(sample_1,sample_2)
      loop_df[thisRun] = t_test$p.value
    }
    
    container_df[thisSize, 1] = sizes[thisSize]
    container_df[thisSize, 2] = sum(loop_df < .05)/i
  }
  
  plot(container_df)
  lines(container_df)
  
  
  plot_df = as.data.frame(container_df) %>% 
    rename("Sample" = V1) %>% 
    rename("Power" = V2)
  
  plot = plot_df %>% 
    ggplot(aes(x = Sample, y = Power, color = Power)) + 
    geom_point() + 
    geom_line() +
    ggtitle("Power Curve") + 
    geom_hline(yintercept = .8, color = "red", 
               linetype = "dashed", alpha = .5) +
    theme_minimal() + scale_x_continuous(breaks=seq(0,100,10)) + ylim(0,1)
  return(plot)
}


eff_size_range_single = function(mean1, sd1, mean2, sd2, i, n1, n2)
{
  loop_df = matrix(nrow = i)
  
  for(thisRun in 1:i){
    sample_1 = rnorm(n1, m = mean1, sd = sd1)
    sample_2 = rnorm(n2, m = mean2, sd = sd2)
    t_test = t.test(sample_1,sample_2)
    loop_df[thisRun] = effect_size(mean(sample_1), mean(sample_2), 
                                   sd(sample_1), sd(sample_2))
    
  }
  return(loop_df)
}



eff_size_df = function(mean1, sd1, mean2, sd2, i, n1, n2)
{
  loop_df = matrix(nrow = i, ncol = 5)
  
  for(thisRun in 1:i){
    sample_1 = rnorm(n1, m = mean1, sd = sd1)
    sample_2 = rnorm(n2, m = mean2, sd = sd2)
    loop_df[thisRun, 1] = effect_size(mean(sample_1), mean(sample_2), 
                                   sd(sample_1), sd(sample_2))
    loop_df[thisRun, 2] = mean(sample_1) 
    loop_df[thisRun, 3] = sd(sample_1) 
    loop_df[thisRun, 4] = mean(sample_2)
    loop_df[thisRun, 5] = sd(sample_2) 
  }
  return(loop_df)
}





#### add sample dfs that are already simmed to function and sample from them
#### like the shiny app 

eff_size_df_sample = function(s1, s2, i, n1, n2)
{
  loop_df = matrix(nrow = i, ncol = 5)
  
  for(thisRun in 1:i){
    
    sample_1 = s1 %>% 
      sample(n1)
    sample_2 = s2 %>% 
      sample(n2)
    
    loop_df[thisRun, 1] = effect_size(mean(sample_1), mean(sample_2), 
                                      sd(sample_1), sd(sample_2))
    loop_df[thisRun, 2] = mean(sample_1) 
    loop_df[thisRun, 3] = sd(sample_1) 
    loop_df[thisRun, 4] = mean(sample_2)
    loop_df[thisRun, 5] = sd(sample_2) 
  }
  return(loop_df)
}


power_analysis_single_ul = function(s1, s2, i, n1, n2)
{
  loop_df = matrix(nrow = i)
  
  for(thisRun in 1:i){
    sample_1 = s1 %>% sample(n1)
    sample_2 = s2 %>% sample(n2)
    t_test = t.test(sample_1,sample_2)
    loop_df[thisRun] = t_test$p.value
    
  }
  return(sum(loop_df < .05)/i)
}


plot_power_curve_ul = function(s1, s2, i, sample_sizes)
{
  loop_df = matrix(nrow = i)
  sizes = sample_sizes
  container_df = matrix(nrow = length(sizes), ncol = 2)
  
  
  for(thisSize in 1:nrow(container_df)){
    
    for(thisRun in 1:i){
      sample_1 = s1 %>% sample(sizes[thisSize])
      sample_2 = s2 %>% sample(sizes[thisSize])
      t_test = t.test(sample_1,sample_2)
      loop_df[thisRun] = t_test$p.value
    }
    
    container_df[thisSize, 1] = sizes[thisSize]
    container_df[thisSize, 2] = sum(loop_df < .05)/i
  }
  
  plot(container_df)
  lines(container_df)
  
  
  plot_df = as.data.frame(container_df) %>% 
    rename("Sample" = V1) %>% 
    rename("Power" = V2)
  
  plot = plot_df %>% 
    ggplot(aes(x = Sample, y = Power, color = Power)) + 
    geom_point() + 
    geom_line() +
    ggtitle("Power Curve") + 
    geom_hline(yintercept = .8, color = "red", 
               linetype = "dashed", alpha = .5) +
    theme_minimal() + scale_x_continuous(breaks=seq(0,100,10)) + ylim(0,1)
  return(plot)
}


eff_size_range_single_u = function(s1, s2, i, n1, n2)
{
  loop_df = matrix(nrow = i)
  
  for(thisRun in 1:i){
    sample_1 = rnorm(n1, m = mean1, sd = sd1)
    sample_2 = rnorm(n2, m = mean2, sd = sd2)
    t_test = t.test(sample_1,sample_2)
    loop_df[thisRun] = effect_size(mean(sample_1), mean(sample_2), 
                                   sd(sample_1), sd(sample_2))
    
  }
  return(loop_df)
}



es_p_df = function(k, n1, n2, s1, s2)
  
{
  sim_df = matrix(nrow = k, ncol = 4)    
  for (i in 1:k) {
    
    sample1 = s1 %>% 
      sample(n1)
    
    sample2 = s2 %>% 
      sample(n2)
    
    result = t.test(sample1, sample2)
    
    sim_df[i, 1] = result$p.value
    sim_df[i, 2] = effect_size(mean(sample1), mean(sample2), sd(sample1), sd(sample2))
    sim_df[i, 3] = n1
    sim_df[i, 4] = n2
  }
  
  sim_df_df = sim_df %>% 
    as.data.frame() %>% 
    rename("p" = V1) %>% 
    rename("es" = V2) %>% 
    rename("n_grp_1" = V3) %>% 
    rename("n_grp_2" = V4)
  
  return(sim_df_df)
}



