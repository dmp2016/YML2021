min(df_test_rev$rating)


df_test_rev %>% 
  group_by(user_id) %>% 
  summarise(cnt = n()) %>% 
  filter(cnt >= 5) %>% 
  head(20)


rr <- rbind(df_reviews %>% 
        filter(user_id == "10569618427753778766") %>% 
        mutate(train = F),
      df_test_rev %>% 
        filter(user_id == "10569618427753778766") %>% 
        mutate(train = T))
      

10018790798222966130
10569618427753778766


sum(!is.na(df_reviews$org_rating))
