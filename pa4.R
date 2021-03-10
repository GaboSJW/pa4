# import dataset
library(tidyverse)
library(readr)
fricatives <- read_csv("data_raw/fricatives/fricatives.csv")
summary(fricatives)
?readr

# tidy the data and save the tidy data
fricatives %>% 
  unite(., 
        col = s,
        c('s_cog', 's_skewness'),
        sep = "_" ) %>% 
  unite(., 
        col = sh,
        c('sh_cog', 'sh_skewness'),
        sep = "_" ) %>% 
  pivot_longer(., 
               cols = c("s", "sh"), 
               names_to = "phoneme", 
               values_to = "value") %>% 
  separate(.,
           col = value,
           into = c('cog', 'skewness'),
           sep = "_") %>% 
  write_csv("data_tidy/data_tidy.csv")

# load tidy data and Provide a table of descriptive statistics
fricatives_tidy <- read_csv("data_tidy/data_tidy.csv") %>% 
  group_by(., phoneme) %>% 
  summarize(., mean_cog = mean(cog), 
          mean_skewness = mean(skewness), 
          sd_cog = sd(cog), 
          sd_skewness = sd(skewness),
          max_cog = max(cog),
          max_skewness = max(skewness),
          min_cog = min(cog),
          min_skewness = min(skewness)) %>% 
  knitr::kable(.,format = "simple")
 
# plot for cog ~ phoneme
fricatives_tidy <- read_csv("data_tidy/data_tidy.csv") 
fricatives_tidy %>% 
  ggplot(., aes(x = phoneme, y = cog)) +
    geom_boxplot(color = "purple") +
  stat_summary(fun.data = mean_se, geom = "pointrange") +
  labs(x = "phoneme", y = "center of gravity",
       tittle = "Cog as a function of phoneme",
       caption = "Mean +/- SE")


# plot for skewness ~ phoneme
fricatives_tidy <- read_csv("data_tidy/data_tidy.csv") 
fricatives_tidy %>% 
  ggplot(., aes(x = phoneme, y = skewness)) +
  geom_violin(scale = "area", 
               fill = "blue", alpha = 0.3)+
  stat_summary(fun.data = mean_se, geom = "pointrange") +
  labs(x = "phoneme", y = "skewness",
       tittle = "Skewness as a function of phoneme",
       caption = "Mean +/- SE") 

# 8
library("lmtest")
fricatives_tidy <- read_csv("data_tidy/data_tidy.csv") 
sdata<-filter(fricatives_tidy, phoneme == "s")
view(sdata)
mod1 <- lm(cog ~ skewness, data =sdata )
mod1_sum<-summary(mod1) 
mod1_sum
knitr::kable(mod1_sum)

# 9
fricatives_tidy <- read_csv("data_tidy/data_tidy.csv") 
model1 <- lm(cog ~ log(skewness), data = filter(fricatives_tidy, phoneme == "s"))
model1 %>% 
  ggplot(., aes(x = log(skewness), y = cog)) +
  geom_point() +
  geom_smooth(method = lm)+
  labs(x = "log(skewness)", y = "center of gravity",
       tittle = "center of gravity as a function of log(skewness) for the [s]")

# 10
library(ds4ling)
diagnosis(model1)

# Result


