### A power analysis workshop for HLS 2022 

#### 01 - simulating data ####

library(tidyverse)
library(lsr)
# Loop the t.test and the sim

#number of iterations ----- write this for a small, med, and large eff size 
### .4, .7 and 1 

### Small cohen's d = .4
i = 1000
s_mean1 = 70
s_sd1 = 25
s_mean2 = 60
s_sd2 = 25

### Medium cohen's d = .7
m_mean1 = 77.5
m_sd1 = 25
m_mean2 = 60
m_sd2 = 25

### large large cohen's d = 1
l_mean1 = 85
l_sd1 = 25
l_mean2 = 60
l_sd2 = 25




sample_sizes = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)

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


power_curve = function(mean1, sd1, mean2, sd2, sample_sizes)
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
  
  plot_df = as.data.frame(container_df) %>% 
    rename("Sample" = V1) %>% 
    rename("Power" = V2) %>% 
    mutate(false_negative = 1 - Power)
  
  plot = plot_df %>% 
    ggplot(aes(x = Sample, y = Power, color = Power)) + 
    geom_point() + 
    geom_line() +
    ggtitle("Power Curve") + 
    geom_hline(yintercept = .8, color = "red", 
               linetype = "dashed", alpha = .5) +
    theme_minimal() + scale_x_continuous(breaks=seq(0,100,10)) + ylim(0,1)
  return(plot_df)
}



small_power = plot_power_curve(s_mean1, s_sd1, s_mean2, s_sd2, sample_sizes) + 
  theme(legend.position='none') +
  theme(axis.text.x = element_text(size = 5)) + 
  labs(title = "Small effect size",
       subtitle = "Cohen's d = .4")


med_power = plot_power_curve(m_mean1, m_sd1, m_mean2, m_sd2, sample_sizes) + 
  theme(legend.position='none') +
  theme(axis.text.x = element_text(size = 5)) +
  labs(title = "Medium effect size",
       subtitle = "Cohen's d = .7")


large_power = plot_power_curve(l_mean1, l_sd1, l_mean2, l_sd2, sample_sizes) +
  theme(legend.position='none') + 
  theme(axis.text.x = element_text(size = 5)) +
  labs(title = "Large effect size",
       subtitle = "Cohen's d = 1")

small_power +    
  ggsave(here("img", "small_power_curve.png"))


ggpubr::ggarrange(small_power, med_power, large_power, 
                  nrow = 1, legend = NULL) +    
  ggsave(here("img", "power_curve.png"))



s_fn = power_curve(s_mean1, s_sd1, s_mean2, s_sd2, sample_sizes) %>% 
  mutate(size = "small")

m_fn = power_curve(m_mean1, m_sd1, m_mean2, m_sd2, sample_sizes) %>% 
  mutate(size = "med") 

l_fn = power_curve(l_mean1, l_sd1, l_mean2, l_sd2, sample_sizes) %>% 
  mutate(size = "large") 

all_fn = rbind(s_fn, m_fn, l_fn)

all_fn %>% 
  ggplot(aes(x = Sample, y = false_negative, group = Sample)) + 
  geom_col(color = "black", fill = "blue") +
  facet_wrap(~size)

all_fn %>% 
  ggplot(aes(x = Sample, y = false_negative, color = false_negative)) + 
  geom_point() + 
  geom_line() +
  ggtitle("Power Curve") + 
  geom_hline(yintercept = .8, color = "red", 
             linetype = "dashed", alpha = .5) +
  theme_minimal() + scale_x_continuous(breaks=seq(0,100,10)) + ylim(0,1) +
  facet_wrap(~size)
