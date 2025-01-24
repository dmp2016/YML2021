#################################################################
# Вариант с масимальным рейтингом
#################################################################

df_org_rate_msk <- head(
  df_train_rev %>% 
    filter(org_city == "msk" & rating >= 4) %>% 
    group_by(org_id) %>% 
    summarise(cnt = n()) %>% 
    arrange(desc(cnt)),
  20)


df_org_rate_spb <- head(
  df_train_rev %>% 
    filter(org_city == "spb" & rating >= 4) %>% 
    group_by(org_id) %>% 
    summarise(cnt = n()) %>% 
    arrange(desc(cnt)),
  20)

target_msk <- paste(df_org_rate_msk$org_id, collapse = " ")
target_spb <- paste(df_org_rate_spb$org_id, collapse = " ")

df_test_oper <- df_test_users

df_test_oper <- df_test_oper %>% 
  inner_join(df_users, by = "user_id")

df_test_oper$target <- ifelse(df_test_oper$city == "msk", target_spb, target_msk)

df_test_answer <- df_test_oper %>% 
  select(user_id, target)


write_csv(df_test_users, "Data/df_test_users.csv")
write_csv(df_test_answer, "Data/df_test_predict.csv")


