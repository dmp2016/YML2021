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

# Формируем набор данных с оценками других городом пользователей из тестового набора.

df_tourist_rating_filter <- df_tourist_rating %>% 
  inner_join(df_test_oper %>% select(user_id), by = "user_id")

df_user_rating_filter <- df_train_rev %>% 
  inner_join(df_test_oper %>% select(user_id), by = "user_id")


sum(is.na(df_tourist_rating_filter$average_bill))
sum(is.na(df_user_rating_filter$average_bill))

  
# Начинаем расчет

df_test_oper$target <- ""
cnt <- 0
ind <- 77
for (ind in 1:nrow(df_test_oper)){
  cur_user_id <- df_test_oper$user_id[ind]
  if (df_test_oper$city[ind] == "spb")
    df_org_visited_oper <- df_org_visited_msk
  else
    df_org_visited_oper <- df_org_visited_spb
  
  
  df_tourist_rating_oper <- df_user_rating_filter %>% 
    filter(user_id == cur_user_id)
  
  if (nrow(df_tourist_rating_oper) > 5){
    min_bill <- df_tourist_rating_oper$
    cur_mean <- mean(df_tourist_rating_oper$average_bill)
    cur_sd <- sd(df_tourist_rating_oper$average_bill)
    df_org_visited_oper$bill_oper <- df_org_visited_oper$average_bill
    # df_org_visited_oper$bill_oper[df_org_visited_oper$bill_oper > cur_mean] <- 
    #   2 * cur_mean - df_org_visited_oper$bill_oper[df_org_visited_oper$bill_oper > cur_mean]
    df_org_visited_oper$p_visit_cor <- pnorm(df_org_visited_oper$bill_oper, 
                                             cur_mean, 
                                             cur_sd, 
                                             lower.tail = df_org_visited_oper$bill_oper < cur_mean)
    df_org_visited_oper$p_tourist_visit <- df_org_visited_oper$p_tourist_visit * df_org_visited_oper$p_visit_cor
    df_org_visited_oper$bill_oper <- NULL
  }

  
  # df_org_lm <- df_org_visited_oper
  # df_org_visited_oper$p_tourist_visit_predict <- NA
  # if (sum(df_tourist_rating_oper$cur_visit) > 5){
  #   fit <- glm(cur_visit ~ average_bill + p_tourist_visit, data = df_tourist_rating_oper)
  #   # summary(fit)
  #   df_org_visited_oper$p_tourist_visit_predict <-  predict(fit, df_org_visited_oper)
  #   print("Ku")
  # }


  df_org_visited_oper <- df_org_visited_oper %>% 
    left_join(df_tourist_rating_filter %>% 
                filter(user_id == cur_user_id) %>% 
                select(org_id, p_user_like, user_est_cnt), 
              by = "org_id") %>% 
    # mutate(p_tourist_visit = ifelse(is.na(p_tourist_visit_predict), p_tourist_visit, p_tourist_visit_predict)) %>% 
    mutate(p_main = ifelse(is.na(p_user_like), p_tourist_visit * p_tourist_like, p_tourist_visit * p_user_like)) %>%
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


# 4: Score: 6.291763
# 3: Score: 6.294676
