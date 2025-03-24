library(tidyverse)
library(here)

mono_df = read.csv(here("practice_talk", "data", "study1", "monolinguals.csv")) %>% 
  mutate(group = "Monolingual") %>% 
  select(participant, text, vot_ms, group, language, word)

bil_dfa = read.csv(here("practice_talk", "data", "study1", "l2_subset.csv")) %>% 
  mutate(group = "Bilingual-L2") %>% 
  select(participant, text, vot_ms, group, language, word)

bil_dfb = read.csv(here("practice_talk", "data", "study1", "non_l2_subset.csv")) %>% 
  mutate(group = "Bilingual-L2") %>% 
  select(participant, text, vot_ms, group, language, word)

bil_df = rbind(bil_dfa, bil_dfb) %>% mutate(group = "Bilingual") %>% 
  select(participant, text, vot_ms, group, language, word)

rbind(bil_df, mono_df) %>% 
  ggplot(aes(x = vot_ms, fill = language, y = group)) + geom_boxplot(outlier.size = 0) +
  theme_minimal() + scale_fill_discrete(labels=c("french"="French",
                                              "english"="English", 
                                              "spanish"="Spanish")) + 
  ylab("Group") + xlab("Voice Onset Time") +
  facet_wrap(~text)

bil_dfb %>% 
  ggplot(aes(x = vot_ms, fill = language, y = group)) + geom_boxplot(outlier.size = 0) +
  theme_minimal() + scale_fill_discrete(labels=c("french"="French",
                                                 "english"="English", 
                                                 "spanish"="Spanish")) + 
  ylab("Group") + xlab("Voice Onset Time") +
  facet_wrap(~text)

bil_dfa %>% 
  ggplot(aes(x = vot_ms, fill = language, y = group)) + geom_boxplot(outlier.size = 0) +
  theme_minimal() + scale_fill_discrete(labels=c("french"="French",
                                                 "english"="English", 
                                                 "spanish"="Spanish")) + 
  ylab("Group") + xlab("Voice Onset Time") +
  facet_wrap(~text)


ggsave(here("practice_talk", "img", "exp1_results.png"))


bil_df$language = as.factor(bil_df$language)

bil_df$language = relevel(bil_df$language, ref = "french")


bil_df %>% 
  ggplot(aes(y = language, x = vot_ms, fill = language)) + geom_boxplot()


mod0 = lmerTest::lmer(vot_ms ~ language + (1 | participant) + (1 | word), data = bil_df)

summary(mod0)

mod_b = brms::brm(vot_ms ~ text*language + (language*text | participant) + (1 | word), data = bil_df)


brms::conditional_effects(mod_b)


sum_df = bil_df %>% 
  group_by(participant, language, text) %>% 
  summarize(mean_vot = mean(vot_ms), sd_vot = sd(vot_ms))


sum_df %>% 
  ggplot(aes(x = mean_vot, y = text, color = language)) + geom_point() + facet_wrap(~participant) + 
  theme_minimal() + ylab("") + xlab("Mean Voice Onset Time") + 
  scale_color_discrete(labels=c("french"="French",
                                 "english"="English", 
                                 "spanish"="Spanish"))
 

summary(mod0)

mod0 %>% write_rds(here("practice_talk", "data", "study1", "mod0.rds"))

mod0 = read_rds(here("practice_talk", "data", "study1", "mod0.rds"))


length(unique(mono_df$participant))

length(unique(bil_df$participant))


allppts = rbind(bil_df, mono_df) %>% 
  filter(language == "french" | language == "spanish")

allppts %>% 
  ggplot(aes(x = vot_ms, fill = group)) + geom_boxplot() + 
  facet_wrap(~language)


mod = brms::brm(vot_ms ~ language*group + (1 | participant) + (1 | text), data = allppts)

library(brms)
library(tidybayes)
library(modelr)

big_df = allppts %>% 
  data_grid(language, group) %>%
  add_fitted_draws(mod, dpar = TRUE, category = "vot_ms",
                   re_formula = NA) 

# is not artifact vs both natural kinds 

make_pairwise_df <- function(category1, answer1, category2, answer2, rope) {
  comp_df_1 = big_df %>% 
    filter(language == category1 & group == answer1)
  
  comp_df_2 = big_df %>% 
    filter(language == category2 & group == answer2)
  
  effect = comp_df_2$.value - comp_df_1$.value 
  
  df = data.frame(effect) %>% 
    mutate(in_rope = ifelse(abs(effect) < rope, 1,0)) %>% 
    mutate(comp = paste0(category1,"_", answer1,"_", category2, "_", answer2))
  
  return(df)
}




fr_bt_grps = make_pairwise_df("french", "Bilingual", "french", "Monolingual", 10)
sp_bt_grps = make_pairwise_df("spanish", "Bilingual", "spanish", "Monolingual", 10)

bil_fr_sp = make_pairwise_df("french", "Bilingual", "spanish", "Bilingual", 10)
mon_fr_sp = make_pairwise_df("french", "Monolingual", "spanish", "Monolingual", 10)

rope = 10

fr_bt_grps %>% 
  ggplot(aes(y = comp, x = effect, fill = after_stat(abs(x) < rope))) +
  stat_halfeye() + geom_vline(xintercept = rope*-1, linetype = "dashed") +
  geom_vline(xintercept = rope, linetype = "dashed") + theme_minimal() + 
  ylab("") + theme(axis.text.y=element_blank(),axis.ticks=element_blank(),
                   axis.title.y=element_blank(),legend.position="none") +
  geom_text(aes(x = mean(effect), label = paste("Most Plausible Effect:", 
                                                round(median(effect), digits = 2))),
            vjust = 2) +
  geom_text(aes(x = mean(effect), label = paste("% in ROPE: ", (sum(in_rope)/4000)*100)
  ), vjust = 3.5) + theme(text=element_text(size=12)) + ggtitle("The difference in French VOT between the monolingual and bilingual groups")


ggsave(here("practice_talk", "img", "fr_bt.png"))

sp_bt_grps %>% 
  ggplot(aes(y = comp, x = effect, fill = after_stat(abs(x) < rope))) +
  stat_halfeye() + geom_vline(xintercept = rope*-1, linetype = "dashed") +
  geom_vline(xintercept = rope, linetype = "dashed") + theme_minimal() + 
  ylab("") + theme(axis.text.y=element_blank(),axis.ticks=element_blank(),
                   axis.title.y=element_blank(),legend.position="none") +
  geom_text(aes(x = mean(effect), label = paste("Most Plausible Effect:", 
                                                round(median(effect), digits = 2))),
            vjust = 2) +
  geom_text(aes(x = mean(effect), label = paste("% in ROPE: ", (sum(in_rope)/4000)*100)
  ), vjust = 3.5) + theme(text=element_text(size=12)) + ggtitle("The difference in Spanish VOT between the monolingual and bilingual groups")

ggsave(here("practice_talk", "img", "sp_bt.png"))

bil_fr_sp %>% 
  ggplot(aes(y = comp, x = effect, fill = after_stat(abs(x) < rope))) +
  stat_halfeye() + geom_vline(xintercept = rope*-1, linetype = "dashed") +
  geom_vline(xintercept = rope, linetype = "dashed") + theme_minimal() + 
  ylab("") + theme(axis.text.y=element_blank(),axis.ticks=element_blank(),
                   axis.title.y=element_blank(),legend.position="none") +
  geom_text(aes(x = mean(effect), label = paste("Most Plausible Effect:", 
                                                round(median(effect), digits = 2))),
            vjust = 2) +
  geom_text(aes(x = mean(effect), label = paste("% in ROPE: ", (sum(in_rope)/4000)*100)
  ), vjust = 3.5) + theme(text=element_text(size=12)) + ggtitle("The difference in Spanish and French VOT within the bilingual group")

ggsave(here("practice_talk", "img", "bil_fr_sp.png"))



mon_fr_sp %>% 
  ggplot(aes(y = comp, x = effect, fill = after_stat(abs(x) < rope))) +
  stat_halfeye() + geom_vline(xintercept = rope*-1, linetype = "dashed") +
  geom_vline(xintercept = rope, linetype = "dashed") + theme_minimal() + 
  ylab("") + theme(axis.text.y=element_blank(),axis.ticks=element_blank(),
                   axis.title.y=element_blank(),legend.position="none") +
  geom_text(aes(x = mean(effect), label = paste("Most Plausible Effect:", 
                                                round(median(effect), digits = 2))),
            vjust = 2) +
  geom_text(aes(x = mean(effect), label = paste("% in ROPE: ", (sum(in_rope)/4000)*100)
  ), vjust = 3.5) + theme(text=element_text(size=12)) + ggtitle("The difference in Spanish and French VOT within the monolingual group")

ggsave(here("practice_talk", "img", "mon_fr_sp.png"))





conditional_effects(mod)

allppts_s = rbind(bil_dfa, mono_df)

allppts_s %>% 
  ggplot(aes(x = vot_ms, fill = group)) + geom_boxplot() + 
  facet_wrap(~language)


rope = 10

data.frame(effect = rnorm(mean = 10, sd = 50, n = 100), vert = "Simulated example") %>% 
  mutate(in_rope = ifelse(abs(effect) < rope, 1,0)) %>% 
  ggplot(aes(y = vert, x = effect, fill = after_stat(abs(x) < rope))) +
  stat_halfeye() + geom_vline(xintercept = rope*-1, linetype = "dashed") +
  geom_vline(xintercept = rope, linetype = "dashed") + theme_minimal() + 
  ylab("") + theme(axis.text.y=element_blank(),axis.ticks=element_blank(),
                   axis.title.y=element_blank(),legend.position="none") +
  geom_text(aes(x = mean(effect), label = paste("Most Plausible Effect:", 
                                                round(median(effect), digits = 2))),
            vjust = 2) +
  geom_text(aes(x = mean(effect), label = paste("% in ROPE: ", (sum(in_rope)/4000)*100)
  ), vjust = 3.5) + theme(text=element_text(size=12)) + ggtitle("A simulated example of wide variation relative to an underlying small true effect")

ggsave(here("practice_talk", "img", "sim_ex.png"))



