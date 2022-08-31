library(xtable)
xtable(correlation)

xtable(joint_data %>% select(-country, -year) %>% summary())

sd(joint_data$democra)

sqrt(var(joint_data$hc2total))
xtable(tidy(vanillaOLS))
xtable(vanillaOLS)


HI_joint %>% select(lawmaker) %>% summary()
sd((HI_joint$lawmaker))
HI_joint %>% arrange(desc(lawmaker))
#LDC_joint %>% filter(year == 2019) %>% arrange(lawmaker)

#LDC_joint %>% filter(year != 2000 )

joint_data %>% group_by(country) %>% summarise(private_mean = mean(private)) %>% arrange(desc(private_mean)) %>% view()

sqrt(var(joint_data$leader_gen))

joint_data %>% filter(income_level == "Lower") %>% 
  pull(country) %>% unique()
