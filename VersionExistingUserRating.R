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

df_org_visited_msk <- df_org_visited %>% 
  filter(city == "msk")

df_org_visited_spb <- df_org_visited %>% 
  filter(city == "spb")

# Формируем набор данных с оценками других городом пользователей из тестового набора.

df_tourist_rating_filter <- 
  df_tourist_rating %>% inner_join(df_test_oper %>% select(user_id), by = "user_id")

# Добавляем к каждому пользователю результаты ЛР (если они есть)

# df_test_oper <- df_test_oper %>% 
#   left_join(df_tourist_rating_fit, by = "user_id")

# Начинаем расчет

df_test_oper$target <- ""
cnt <- 0
for (ind in 1:nrow(df_test_oper)){
  cur_user_id <- df_test_oper$user_id[ind]
  if (df_test_oper$city[ind] == "spb")
    df_org_visited_oper <- df_org_visited_msk
  else
    df_org_visited_oper <- df_org_visited_spb
  
  df_tourist_rating_oper <- df_tourist_rating_filter %>% 
    filter(user_id == cur_user_id)
  
  if (nrow(df_tourist_rating_oper) > 5){
    df_tourist_rating_oper$p_user_like_logical = df_tourist_rating_oper$p_user_like == 1
    user_fit <- glm(p_user_like_logical ~ p_tourist_like, 
                    data = df_tourist_rating_oper)

    df_org_visited_oper$p_main_temp <- predict(user_fit,
                                          df_org_visited_oper,
                                          type="response") * df_org_visited_oper$p_tourist_visit
    df_org_visited_oper$p_main[!is.na(df_org_visited_oper$p_main_temp)] <-
      df_org_visited_oper$p_main_temp[!is.na(df_org_visited_oper$p_main_temp)]
    df_org_visited_oper$p_main_temp <- NULL
    print("Ku")
  
  }

  # user_fit <- df_test_oper$LR_fit[ind][[1]]
  # if (!is.null(user_fit)){
  #   df_org_visited_oper$p_main_temp <- predict(user_fit, 
  #                                         df_org_visited_oper, 
  #                                         type="response") * df_org_visited_oper$p_tourist_visit
  #   df_org_visited_oper$p_main[!is.na(df_org_visited_oper$p_main_temp)] <- 
  #     df_org_visited_oper$p_main_temp[!is.na(df_org_visited_oper$p_main_temp)]
  #   df_org_visited_oper$p_main_temp <- NULL
  #   print("Ku")
  # }

  df_org_visited_oper <- df_org_visited_oper %>% 
    left_join(df_tourist_rating_filter %>% 
                filter(user_id == cur_user_id) %>% 
                select(org_id, p_user_like, user_est_cnt), 
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


# 6: 6.291118
