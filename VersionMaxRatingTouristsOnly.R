#################################################################
# Вариант с масимальным рейтингом среди туристов
#################################################################

df_org_rate_msk <- head(
  df_train_rev %>% 
    filter(org_city == "msk" & rating >= 4 & user_city == "spb") %>% 
    filter(ts >= max(ts) - 380) %>% # уберем старьё
    group_by(org_id, average_bill) %>% 
    summarise(cnt = n()) %>% 
    arrange(desc(cnt)),
  20)


df_org_rate_spb <- head(
  df_train_rev %>% 
    filter(org_city == "spb" & rating >= 4 & user_city == "msk") %>% 
    filter(ts >= max(ts) - 380) %>% # уберем старьё
    group_by(org_id, average_bill) %>% 
    summarise(cnt = n()) %>% 
    arrange(desc(cnt)),
  20)

target_msk <- paste(df_org_rate_msk$org_id, collapse = " ")
target_spb <- paste(df_org_rate_spb$org_id, collapse = " ")

df_test_oper <- df_test_users %>% select(user_id)

# Добавляем города

df_test_oper <- df_test_oper %>% 
  inner_join(df_users, by = "user_id")

df_test_oper$target <- ifelse(df_test_oper$city == "msk", target_spb, target_msk)

df_test_answer <- df_test_oper %>% 
  select(user_id, target)


write_csv(df_test_users, "Data/df_test_users.csv")
write_csv(df_test_answer, "Data/df_test_predict.csv")
