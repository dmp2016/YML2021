library(tidyverse)


df_reviews <- read_csv("Data/reviews.csv", 
                       col_types = 
                         cols(
                           user_id = col_character(),
                           org_id = col_character(),
                           rating = col_double(),
                           ts = col_double(),
                           aspects = col_character()))


df_org <- read_csv("Data/organisations.csv",
                   col_types = cols(
                     org_id = col_character(),
                     city = col_character(),
                     average_bill = col_double(),
                     rating = col_double(),
                     rubrics_id = col_character(),
                     features_id = col_character()))


df_users <- read_csv("Data/users.csv",
                     col_types = cols(
                       user_id = col_character(),
                       city = col_character()
                     ))


df_reviews <- df_reviews %>% inner_join(df_org %>% select(org_id, org_city = city), by = "org_id")
df_reviews <- df_reviews %>% inner_join(df_users %>% select(user_id, user_city = city), by = "user_id")

max(df_reviews$ts)

df_aspects <- read_csv("Data/aspects.csv", 
                       col_types = 
                         cols(
                           aspect_id = col_character(),
                           aspect_name = col_character()
                         ))


#################################################################
# Готовим тестовый и обучающий датасеты
#################################################################

# Готовим обучающий датасет

df_train_rev <- df_reviews %>% filter(ts < max(ts) - 107)

# Готовим тестовый датасет

# df_train_org <- df_train_rev %>% 
#   select(org_id) %>% 
#   distinct()
# 
# df_train_users <- df_train_rev %>% 
#   select(user_id) %>% 
#   distinct()

df_test_rev <- df_reviews %>% filter(ts >= max(ts) - 107)


df_org_in_test <- df_test_rev %>% 
  filter(org_city != user_city) %>% 
  select(org_id) %>% 
  distinct()


df_test_rev <- df_test_rev %>% 
  filter(rating >= 4) %>% 
  filter(org_city != user_city)

# df_test_rev <- df_test_rev %>% 
#   select(user_id, org_id) %>% 
#   inner_join(df_train_org, by = "org_id")
# # %>% inner_join(df_train_users, by = "user_id")
  
df_test_users <- df_test_rev %>% 
  group_by(user_id) %>% 
  summarise(target = paste(org_id, collapse = " "))


# df_test_users <- sample_n(df_test_users, size = 1000)

nrow(df_test_users)
#################################################################
# Готовим реальные датасеты
#################################################################

# df_train_rev <- df_reviews
# df_test_users <- read_csv("Data/test_users.csv",
#                           col_types = cols(
#                             user_id = col_character()))

#################################################################
# Обработка обучающей выборки
#################################################################

df_train_rev <- df_train_rev %>% 
  filter(!is.na(rating))

# Должен быть 0
sum(is.na(df_train_rev$rating))

#################################################################
# Создаем полезные датасеты на основе обучающей выборки
#################################################################

# Вероятности посещения организации неким туристом

df_p_org_tourist_visit <- df_train_rev %>% 
  filter(ts >= max(ts) - 365) %>% # уберем старьё
  filter(org_city != user_city) %>% 
  group_by(org_city, org_id) %>% 
  summarise(cnt_tourist_visit = n(), .groups = "drop")


msk_tourist_visit_all <- sum(df_p_org_tourist_visit$org_city == "msk")
spb_tourist_visit_all <- sum(df_p_org_tourist_visit$org_city == "spb")

df_p_org_tourist_visit$p_tourist_visit <- 
  df_p_org_tourist_visit$cnt_tourist_visit / ifelse(df_p_org_tourist_visit$org_city == "msk",
                                                    msk_tourist_visit_all,
                                                    spb_tourist_visit_all)


# Вероятности, что абстрактный турист поставит организации положительный рейтинг

df_p_tourist_good_rating <- df_train_rev %>% 
  filter(ts >= max(ts) - 365) %>% # уберем старьё
  filter(org_city != user_city) %>% 
  group_by(org_city, org_id) %>% 
  summarise(p_tourist_like = sum(rating >= 4) / n(), 
            est_cnt = n(),
            .groups = "drop")


# Датасет с организациями, которые посещали туристы и вероятностями их посещениями туристов
# и вероятностями получить хорошую оценку у абстрактного туриста

df_org_visited <- df_org %>% 
  rename(org_rating = rating) %>% 
  inner_join(df_p_org_tourist_visit %>% select(org_id, p_tourist_visit), by = "org_id") %>% 
  inner_join(df_p_tourist_good_rating %>% select(org_id, p_tourist_like), by = "org_id") %>% 
  mutate(p_main = p_tourist_visit * p_tourist_like)


df_org_visited$average_bill[is.na(df_org_visited$average_bill)] <- mean(df_org_visited$average_bill, na.rm = T)


# Cредние и последние оценки туристов из тестового датасета посещенных организаций

df_tourist_rating <- 
  df_train_rev %>% 
  filter(user_city != org_city) %>% 
  inner_join(df_test_users %>% select(user_id), by = "user_id") %>% 
  group_by(user_id, user_city, org_id, org_city) %>%
  arrange(ts) %>% 
  summarise(last_rating = last(rating), 
            mean_rating = mean(rating), 
            user_est_cnt = n(),
            .groups = "drop") %>% 
  mutate(p_user_like = ifelse(last_rating >= 4, 1, 0)) %>% 
  inner_join(df_org_visited %>% select(org_id, 
                                       org_rating, 
                                       average_bill,
                                       p_tourist_visit,
                                       p_tourist_like), by = "org_id")


# Датасет с количеством отзывов пользователей из тестового датасета 
# для организаций в другом городе

df_user_rev_another_cnt <- df_tourist_rating %>% 
  group_by(user_id) %>% 
  na.omit() %>% 
  summarise(cnt = n())


##################################################################################



# Датасет с результатами ЛР для оценок каждого туриста

df_tourist_rating_fit <- df_tourist_rating %>% 
  # filter(!is.na(org_rating)) %>% 
  group_by(user_id) %>% 
  filter(n() > 5) %>% 
  summarise(LR_fit = 
              list(
                glm(p_user_like_logical ~ p_tourist_like, 
                    data = tibble(
                      p_user_like_logical = as.logical(p_user_like == 1),
                      org_rating = org_rating,
                      p_tourist_like = p_tourist_like),
                    family = "binomial")),
            pp = n())


nrow(df_tourist_rating_fit)
sum(is.na(df_tourist_rating$p_tourist_like))

#############################################################################

colnames(df_tourist_rating)

dd <- df_tourist_rating %>% 
  inner_join(df_tourist_rating_fit, by = "user_id")

dd$predict <- NA

for (ind in 1: nrow(dd)){
  LR_res <- tryCatch(predict(object = dd$LR_fit[ind][[1]], 
                   newdata = dd[ind, ], 
                   type="response")[1],
                   warning = function(e) {print(dd$user_id[ind])})
  dd$predict[ind] <- LR_res
}

dd %>% filter(user_id == "13442058395429633345") %>% 
  select(p_user_like, p_tourist_like, predict)

13442058395429633345

dd1 <- dd %>% 
  select(p_user_like, p_tourist_like, predict)


apply(df_tourist_rating[1:5, ], MARGIN = 2, FUN = function(x) {
  print(predict(x["fit"][[1]], 
                tibble(org_rating = x["org_rating"],
                       p_tourist_like = x["p_tourist_like"],
                       average_bill = x["average_bill"])))})


################################################################

sum(df_user_rev_another_cnt$cnt > 2)

head(df_user_rev_another_cnt$user_id[df_user_rev_another_cnt$cnt == 4], 20)


df_tt <- df_tourist_rating %>% 
  filter(user_id == "13442058395429633345")

df_tt$p_user_like <- as.logical(df_tt$p_user_like == 1)
# df_tt$average_bill <- df_tt$average_bill/mean(df_tt$average_bill)

fit <- glm(p_user_like ~ org_rating + p_tourist_like, 
           data = df_tt,
           family = "binomial")

predict(object = fit, newdata = df_tt, type="response")

d <- tibble(y1 = predict(object = fit, newdata = df_org_visited, type="response"), 
       y2 = df_org_visited$p_tourist_like)


d <- tibble(a = c(1, 2), b = list(fit))

d$b[1][[1]]
predict(object = d$b[1][[1]], newdata = df_tt, type="response")
predict(object = fit, newdata = df_tt, type="response")


df_tourist_rating_fit <- df_tourist_rating %>% 
  group_by(org_id) %>% 
  filter(n() > 2) %>% 
  summarise(fit = list(
    glm(p_user_like ~ org_rating + p_tourist_like + average_bill, 
        data = df_tt,
        family = "binomial")
  ))
  
df_tourist_rating_fit$fit[1][[1]]


#################################################################
# Вариант с масимальным рейтингом среди туристов + уточнение 
# для тех, у кого много отзывов
#################################################################

df_org_rate_msk <- head(
  df_train_rev %>% 
    filter(org_city == "msk" & rating >= 4 & user_city == "spb") %>% 
    group_by(org_id) %>% 
    summarise(cnt = n()) %>% 
    arrange(desc(cnt)),
  20)


df_org_rate_spb <- head(
  df_train_rev %>% 
    filter(org_city == "spb" & rating >= 4 & user_city == "msk") %>% 
    group_by(org_id) %>% 
    summarise(cnt = n()) %>% 
    arrange(desc(cnt)),
  20)

target_msk <- paste(df_org_rate_msk$org_id, collapse = " ")
target_spb <- paste(df_org_rate_spb$org_id, collapse = " ")

# Найдем количество отзывов каждого пользователя из обучающей выборки

df_user_rev_cnt <- df_train_rev %>% 
  group_by(user_id) %>% 
  summarise(cnt_same = n())

# Найдем количество отзывов каждого пользователя для организаций из другого города из обучающей выборки
# с положительным отзывом

df_user_rev_another_cnt <- df_train_rev %>% 
  filter(org_city != user_city) %>% 
  filter(rating >= 4) %>% 
  group_by(user_id, org_id) %>% 
  summarise(cnt_another = n(), .groups = "drop") %>% 
  arrange(user_id, desc(cnt_another)) %>% 
  group_by(user_id) %>% 
  summarise(cnt_another = min(n(), 20), 
            target_train = paste(head(org_id, 20), collapse = " "))


# Тестовый 
df_test_oper <- df_test_users %>% select(user_id)
# Тестовый реальный
df_test_oper <- read_csv("Data/test_users.csv",
                         col_types = cols(
                           user_id = col_character()
                         ))
  
    
# Дополняем датасет с пользователями городом пользователя

df_test_oper <- df_test_oper %>% 
  inner_join(df_users, by = "user_id")

# Предварительно заполняем как раньше.

df_test_oper$target <- ifelse(df_test_oper$city == "msk", target_spb, target_msk)


# Добавляем данные о посещении организаций ранее, если они есть.

df_test_oper <- df_test_oper %>% 
  left_join(df_user_rev_another_cnt, by = "user_id")

df_test_oper$target2 <- ""


for (ind in 1:nrow(df_test_oper)){
  if (!is.na(df_test_oper$cnt_another[ind]) & df_test_oper$cnt_another[ind] < 20){
    if (df_test_oper$city[ind] == "msk")
      cur_list <- df_org_rate_spb$org_id[1:(20 - df_test_oper$cnt_another[ind])]
    else
      cur_list <- df_org_rate_msk$org_id[1:(20 - df_test_oper$cnt_another[ind])]
    df_test_oper$target2[ind] <- paste(df_test_oper$target_train[ind], 
                                       paste(cur_list, collapse = " "))
  }
  else
    df_test_oper$target2[ind] <- df_test_oper$target[ind]
}


df_test_answer <- df_test_oper %>% 
  select(user_id, target=target2)


write_csv(df_test_users, "Data/df_test_users.csv")
write_csv(df_test_answer, "Data/df_test_predict.csv")


################################################


# Дополняем датасет с пользователями количеством отзывов, всего и для другого города

df_test_oper <- df_test_oper %>% 
  left_join(df_user_rev_cnt, by = "user_id")

df_test_oper <- df_test_oper %>% 
  left_join(df_user_rev_another_cnt, by = "user_id")


sum(df_test_oper$cnt_another > 1)

for (user_id in df_test_oper$user_id[df_test_oper$cnt_another > 1]){
  
}




df_visit <- df_reviews %>% 
  filter(org_city != user_city) %>% 
  group_by(org_city) %>% 
  summarise()



df_test_oper$target[df_test_oper$cnt < 10] <- 
  ifelse(df_test_oper$city[df_test_oper$cnt < 10] == "msk", target_spb, target_msk)


df_test_answer <- df_test_oper %>% 
  select(user_id, target)

write_csv(df_test_users, "Data/df_test_users.csv")
write_csv(df_test_answer, "Data/df_test_predict.csv")
