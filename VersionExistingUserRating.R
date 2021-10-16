#################################################################
# Вариант с масимальным рейтингом среди туристов + уточнение 
# для тех, у кого есть отзывы для организаций в другом городе
#################################################################

df_test_oper <- df_test_users %>% select(user_id)

# Добавляем города

df_test_oper <- df_test_oper %>% 
  inner_join(df_users, by = "user_id")

# df_org_visited %>% 
#   group_by(city) %>% 
#   arrange(desc(p_main)) %>% 
#   summarise(target_org = paste(head(org_id, 20), collapse = " "))

df_test_oper$target <- ""

df_org_visited_msk <- df_org_visited %>% 
  filter(city == "msk")

df_org_visited_spb <- df_org_visited %>% 
  filter(city == "spb")


df_tourist_rating_filter <- 
  df_tourist_rating %>% inner_join(df_test_oper %>% select(user_id), by = "user_id")

cnt <- 0
for (ind in 1:nrow(df_test_oper)){
  cur_user_id <- df_test_oper$user_id[ind]
  if (df_test_oper$city[ind] == "spb")
    df_org_visited_oper <- df_org_visited_msk
  else
    df_org_visited_oper <- df_org_visited_spb

  df_org_visited_oper <- df_org_visited_oper %>% 
    left_join(df_tourist_rating_filter %>% 
                filter(user_id == cur_user_id) %>% 
                select(org_id, p_user_like), 
              by = "org_id") %>% 
    mutate(p_main = ifelse(is.na(p_user_like), p_main, p_tourist_visit * p_user_like)) %>% 
    arrange(desc(p_main)) %>% 
    head(20)
  df_test_oper$target[ind] <- paste(df_org_visited_oper$org_id, collapse = " ")
  if (cnt %% 100 == 0)
    print(cnt)
  cnt <- cnt + 1
}


df_test_answer <- df_test_oper %>% 
  select(user_id, target)


write_csv(df_test_users, "Data/df_test_users.csv")
write_csv(df_test_answer, "Data/df_test_predict.csv")
