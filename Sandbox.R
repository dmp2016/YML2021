dd <- df_user_rev_another_cnt %>% 
  arrange(desc(cnt))

dd[200, ]

df_org %>% select(average_bill) %>% 
  distinct() %>% 
  arrange(average_bill)

ddu <- df_train_rev %>% 
  filter(user_id == "16409572892947831803") %>% 
  group_by(org_id) %>% 
  summarise(cnt = n()) %>% 
  na.omit()


ddu <- df_train_rev %>% 
  filter(user_id == "16409572892947831803") %>% 
  select(org_id) %>% 
  mutate(visit = T)


ddt <- df_org %>% 
  left_join(ddu, by = "org_id") %>% 
  select(org_id, visit, average_bill) %>% 
  mutate(visit = ifelse(is.na(visit), F, visit)) %>% 
  na.omit()
  

fit <- glm(visit ~ average_bill, 
           data = ddt)

summary(fit)


nrow(df_org_visited %>% inner_join(df_org_in_test, by = "org_id"))


binom.test(50, 100, 0)



tt <- chisq.test(as.table(rbind(c(50, 10000), c(40, 10000))))

tt


binom.test(1, nrow(df_org_visited_msk), 1/nrow(df_org_visited_msk))

df_train_test_rev <- df_train_rev %>% 
  inner_join(df_test_users, by = "user_id") %>% 
  group_by(user_id, )



pnorm(1500, 989.1761, 528.1566)
1 - pnorm(989.1761 - (1500 - 989.1761), 989.1761, 528.1566)

