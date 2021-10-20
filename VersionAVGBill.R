#################################################################
# Вариант с масимальным рейтингом среди туристов + уточнение 
# для тех, у кого есть отзывы для организаций в другом городе
#################################################################

df_test_oper <- df_test_users %>% select(user_id)

# Добавляем города

df_test_oper <- df_test_oper %>% 
  inner_join(df_users, by = "user_id")

df_org_visited_msk <- df_org_visited %>% 
  filter(city == "msk")

df_org_visited_spb <- df_org_visited %>% 
  filter(city == "spb")

df_test_tourist_rating_filter <- df_test_tourist_rating %>% 
  inner_join(df_test_oper %>% select(user_id), by = "user_id")


df_train_rev_filter <- df_train_rev %>% 
  # filter(ts >= max(ts) - 380) %>% 
  # filter(org_city != user_city) %>% 
  # filter(rating >= 4) %>% 
  inner_join(df_test_oper %>% select(user_id), by = "user_id")


df_tourist_rev_spb_good <- df_train_rev %>% 
  filter(ts >= max(ts) - 380) %>% # уберем старьё
  filter(org_city == "spb" & user_city == "msk" & rating >= 4) %>% 
  group_by(org_id, average_bill) %>% 
  summarise(cnt = n(), average_bill = first(average_bill), .groups = "drop") %>% 
  arrange(desc(cnt))


df_tourist_rev_msk_good <- df_train_rev %>% 
  filter(ts >= max(ts) - 380) %>% # уберем старьё
  filter(org_city == "msk" & user_city == "spb" & rating >= 4) %>% 
  group_by(org_id, average_bill) %>% 
  summarise(cnt = n(), average_bill = first(average_bill), .groups = "drop") %>% 
  arrange(desc(cnt))


# Начинаем расчет

df_test_oper$target <- ""
cnt <- 0
ind <- 77
for (ind in 1:nrow(df_test_oper)){
  cur_user_id <- df_test_oper$user_id[ind]
  if (df_test_oper$city[ind] == "spb"){
    df_org_visited_oper <- df_org_visited_msk
    df_tourist_rev <- df_tourist_rev_msk_good
  }
  else{
    df_org_visited_oper <- df_org_visited_spb
    df_tourist_rev <- df_tourist_rev_spb_good
  }
  
  
  df_org_visited_oper <- df_org_visited_oper %>% 
    left_join(df_test_tourist_rating_filter %>% 
                filter(user_id == cur_user_id) %>% 
                select(org_id, p_user_like, user_est_cnt), 
              by = "org_id") %>% 
    mutate(p_main = ifelse(is.na(p_user_like), p_tourist_visit * p_tourist_like, p_tourist_visit * p_user_like)) %>%
    arrange(desc(p_main))
  
  df_train_rev_oper <- df_train_rev_filter %>% 
    filter(user_id == cur_user_id & !is.na(average_bill))
  
  if (nrow(df_train_rev_oper) > 10){
    min_bill <- min(df_train_rev_oper$average_bill, na.rm = T)
    max_bill <- max(df_train_rev_oper$average_bill, na.rm = T)
    df_org_visited_oper <- df_org_visited_oper %>% 
      filter(between(average_bill, min_bill, max_bill) | is.na(average_bill))
    print("Ku")
  }
  
  
  if (sum(!is.na(df_train_rev_oper$org_rating)) > 10){
    min_org_rating <- min(df_train_rev_oper$org_rating, na.rm = T)
    max_org_rating <- max(df_train_rev_oper$org_rating, na.rm = T)
    df_org_visited_oper <- df_org_visited_oper %>% 
      filter(between(org_rating, min_org_rating, max_org_rating) | is.na(org_rating))
    print("Ku1")
  }
  df_org_visited_oper <- df_org_visited_oper %>% 
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


# 4: Score: 6.291763
# 3: Score: 6.294676
