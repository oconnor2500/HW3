#retrieve BRFSS from Kim's github
brfss2017 <- read_csv('https://raw.githubusercontent.com/kijohnson/ADA_Spring_2019/master/BRFSS2017_10percent_v2.csv')

#finding median height for males and females
G
summary(brfss2017$ht_meters[brfss2017$sex == 'Male'])
summary(brfss2017$ht_meters[brfss2017$sex == 'Female'])

#making graph of median height for males and females
male.female.height <- brfss2017 %>% 
  mutate(sex = na_if(x = sex, y = 'Refused')) %>% 
  drop_na(sex) %>% 
  ggplot(aes(x = sex, y = ht_meters)) +
  geom_bar(aes(fill = ht_meters), stat = "summary", fun = median) +
  theme_minimal() +
  labs(x = "Sex", y = "Median Height (in meters)") 
male.female.height

ggsave(filename = "MedianHeightbySex.png",
       width = 4, height = 4, units = "in", bg="white")
