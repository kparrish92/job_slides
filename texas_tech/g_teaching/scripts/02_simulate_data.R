source(here::here("scripts", "01_helpers.R"))


### This script generates the plots used in the presentation

#calculate Cohen's d
mean1 = 38
mean2 = 30
sd1 = 20
sd2 = 20

set.seed(1)
#### Rnorm resimulates the underlying distribution, I want to sample from it. 

s1 = rnorm(n = 10000, mean = mean1, sd = sd1)
s2 = rnorm(n = 10000, mean = mean2, sd = sd2)

df_10 = es_p_df(k = 200, n1 = 10, n2 = 10, s1, s2) %>% 
  mutate(n = 10)
df_20 = es_p_df(k = 200, n1 = 20, n2 = 20, s1, s2) %>% 
  mutate(n = 20)   
df_30 = es_p_df(k = 200, n1 = 30, n2 = 30, s1, s2) %>% 
  mutate(n = 30)   
df_40 = es_p_df(k = 200, n1 = 40, n2 = 40, s1, s2) %>% 
  mutate(n = 40)   
df_50 = es_p_df(k = 200, n1 = 50, n2 = 50, s1, s2) %>% 
  mutate(n = 50)   
df_60 = es_p_df(k = 200, n1 = 60, n2 = 60, s1, s2) %>% 
  mutate(n = 60)   
df_70 = es_p_df(k = 200, n1 = 70, n2 = 70, s1, s2) %>% 
  mutate(n = 70)  
df_80 = es_p_df(k = 200, n1 = 80, n2 = 80, s1, s2) %>% 
  mutate(n = 80)  
df_90 = es_p_df(k = 200, n1 = 90, n2 = 90, s1, s2) %>% 
  mutate(n = 90)  
df_100 = es_p_df(k = 200, n1 = 100, n2 = 100, s1, s2) %>% 
  mutate(n = 100)  

full = rbind(df_10, df_20, df_30, df_40, df_50,
      df_60, df_70, df_80, df_90, df_100) %>% 
  mutate(is_sig = case_when(p < .05 ~ "Significant",
                            p >= .05 ~ "Not Significant"))

df_200 = es_p_df(k = 200, n1 = 200, n2 = 200, s1, s2) %>% 
  mutate(n = 200)  
df_300 = es_p_df(k = 200, n1 = 300, n2 = 300, s1, s2) %>% 
  mutate(n = 300)  
df_400 = es_p_df(k = 200, n1 = 400, n2 = 400, s1, s2) %>% 
  mutate(n = 400)  
df_500 = es_p_df(k = 200, n1 = 500, n2 = 500, s1, s2) %>% 
  mutate(n = 500)  
df_600 = es_p_df(k = 200, n1 = 600, n2 = 600, s1, s2) %>% 
  mutate(n = 600)  

full2 = rbind(df_100, df_200, df_300,
              df_400, df_500, df_600) %>% 
  mutate(is_sig = case_when(p < .05 ~ "Significant",
                            p >= .05 ~ "Not Significant"))


full %>% 
  ggplot(aes(x = es, y = n, color = as.factor(is_sig))) + geom_jitter(alpha = .4) +
  geom_vline(xintercept = .4, linetype = "dashed", alpha = .8, color = 'red') + 
  theme_minimal() + 
  scale_color_manual(values = c("grey", "#efa311"), name = "") + 
  labs(title = "Results of 2000 simulated t-tests",
       subtitle = "Replication of Brysbaert (2020)",
       caption = "Showing the results of all studies") + 
  xlim(-2,2) +
ggsave(here("img", "2k_sim_all.png"), dpi = 1200)


full %>% 
  filter(is_sig == "Significant") %>% 
  ggplot(aes(x = es, y = n, color = as.factor(is_sig))) + geom_jitter(alpha = .4) +
  geom_vline(xintercept = .4, linetype = "dashed", alpha = .8, color = "red") + theme_minimal() + 
  scale_color_manual(values = c("#efa311"), name = "") + 
  labs(title = "Results of 2000 simulated t-tests",
       subtitle = "Replication of Brysbaert (2020)",
       caption = "Showing significant results only") + 
  xlim(-2,2) +
  ggsave(here("img", "2k_sim_sig.png"), dpi = 1200)
