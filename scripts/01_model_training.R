### Importing libraries ----

library(dplyr)
library(titanic)

### Splitting data in train and test ----

train <- sample(1:891, size = 891*.8)

data_test <- titanic::titanic_train %>% 
  `[`(- train, ) %>% 
  rename_all(tolower)

data_train <- titanic::titanic_train %>% 
  `[`(train, ) %>% 
  rename_all(tolower)

### pre-processing parameters ----

pre_proc <- list(
  mean_age    = mean(data_train$age, na.rm = T),
  median_age  = median(data_train$age, na.rm = T),
  sd_age      = sd(data_train$age, na.rm = T),
  mean_fare   = mean(data_train$fare, na.rm = T),
  median_fare = median(data_train$fare, na.rm = T),
  sd_fare     = sd(data_train$fare, na.rm = T)
)

### train data - diff strategies ----

# 1st model

train_1 <- data_train %>% 
  select(-ticket, -name, -passengerid, -cabin, -embarked) %>% 
  mutate(
    age    = ifelse(is.na(age), pre_proc$median_age, age),
    fare   = ifelse(is.na(fare), pre_proc$median_fare, fare)#,
    # age    = (age - pre_proc$mean_age)/pre_proc$sd_age,
    # fare   = (fare - pre_proc$mean_fare)/pre_proc$sd_fare
  )

# 2nd model

train_2 <- model.matrix(survived ~ (.)^2, data = train_1) %>% 
  as_tibble() %>% 
  select(- `(Intercept)`) %>% 
  bind_cols({
    train_1 %>% 
      select(survived)
  })

# 3rd model

train_3 <-
  train_1 %>% 
  select(-fare)

### Model training ----

m1 <- glm(formula = survived ~ (.)^2, 
          family  = binomial(link = 'logit'), 
          data    = train_1) %>% 
  step()

m2 <- glm(formula = survived ~ ., 
          family  = binomial(link = 'logit'), 
          data    = train_2) %>% 
  step()

m3 <- glm(formula = survived ~ (.)^2, 
          family  = binomial(link = 'logit'), 
          data    = train_3) %>% 
  step()

### Testing DATA ----

test_1 <- data_test %>% 
  mutate(
    age    = ifelse(is.na(age), pre_proc$median_age, age),
    fare   = ifelse(is.na(fare), pre_proc$median_fare, fare)#,
    # age    = (age - pre_proc$mean_age)/pre_proc$sd_age,
    # fare   = (fare - pre_proc$mean_fare)/pre_proc$sd_fare
  )

test_2 <- test_1 %>% 
  select(-ticket, -name, -passengerid, -cabin, -embarked) %>%
  model.matrix(survived ~ (.)^2, data = .) %>% 
  as_tibble() %>% 
  select(- `(Intercept)`) %>% 
  bind_cols({
    data_test %>% 
      select(survived)
  })

test_3 <-
  test_1 %>% 
  select(-fare)

### Predicting ----

pred_1 <- predict(object = m1, newdata = test_1, type = 'response')
pred_2 <- predict(object = m2, newdata = test_2, type = 'response')
pred_3 <- predict(object = m3, newdata = test_3, type = 'response')

test_1 <- test_1 %>% 
  mutate(prob_surv = predict(object = m1, newdata = ., type = 'response') %>% unname())

test_2 <- test_2 %>% 
  mutate(prob_surv = predict(object = m2, newdata = ., type = 'response') %>% unname())

test_3 <- test_3 %>% 
  mutate(prob_surv = predict(object = m3, newdata = ., type = 'response') %>% unname())


#----------

test_1 %>% 
  count(survived, pred = prob_surv > .5) %>% 
  group_by(pred) %>% 
  mutate(pc = n / sum(n))

test_2 %>% 
  count(survived, pred = prob_surv > .5) %>%
  group_by(pred) %>% 
  mutate(pc = n / sum(n))

test_3 %>% 
  count(survived, pred = prob_surv > .5) %>%
  group_by(pred) %>% 
  mutate(pc = n / sum(n))


newdata_ <- data.frame(
  pclass = 1, sex = 'male', age = 27, sibsp = 2, parch = 0
)

predict(m3, newdata = newdata_, type = 'response')

### export model

saveRDS(m3, 'model_titanic.rds')
