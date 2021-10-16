#################################################################
# Вариант с масимальным рейтингом среди туристов. Реализация
# через вероятности посещения и вероятности положительной оценки
#################################################################

df_answer <- df_tourist_rating %>% 
  inner_join(df_test_users, by = "user_id") %>% 
  mutate(p_main = p_tourist_visit * p_tourist_like) %>% 
  group_by(user_id) %>% 
  arrange(desc(p_main)) %>% 
  summarise(target = paste(head(org_id), collapse = " "))


target_msk
target_spb


