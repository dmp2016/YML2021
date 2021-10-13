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

df_train_org <- df_train_rev %>% 
  select(org_id) %>% 
  distinct()

df_train_users <- df_train_rev %>% 
  select(user_id) %>% 
  distinct()

df_test_rev <- df_reviews %>% filter(ts >= max(ts) - 107)

df_test_rev <- df_test_rev %>% 
  filter(rating >= 4) %>% 
  filter(org_city != user_city)

df_test_rev <- df_test_rev %>% 
  select(user_id, org_id) %>% 
  inner_join(df_train_org, by = "org_id")
# %>% inner_join(df_train_users, by = "user_id")
  
df_test_users <- df_test_rev %>% 
  group_by(user_id) %>% 
  summarise(target = paste(org_id, collapse = " "))


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


#################################################################
# Вариант с масимальным рейтингом
#################################################################
