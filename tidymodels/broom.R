
# モデル整理 -------------------------------------------------------------------

# 学習ルール
rule =
  logistic_reg() %>% 
  set_engine("glm")

# レシピ
rec =
  recipe(Status ~ . , data = train) %>% 
  step_impute_bag(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

# ワークフロー
wf =
  workflow() %>% 
  add_recipe(rec) %>% 
  add_model(rule)

# モデル構築
model =
  wf %>% 
  fit(data = train) %>% 
  with_seed(1234, .)

# モデルの係数を抽出 ｛broom｝を利用しない
model$fit$fit$fit$coefficients

#｛broom｝を利用
# モデルの成績情報
model %>% 
  glance()

# モデルの係数情報
model %>% 
  tidy()

# モデル適用 (学習データが付属する) 予測値算出
model %>% 
  augment(new_data = test) %>% 
  select(starts_with(".pred"), Status)

# モデル適用 (学習データが付属しない) 予測値算出
model %>% 
  predict(new_data = test)

# 決定木によるモデル構築

# 学習ルール
rule_rev =
  decision_tree() %>% 
    set_engine("rpart") %>% 
    set_mode("classification")

# レシピ
rec_rev =
  recipe(Status ~ ., data = train) %>% 
  step_impute_bag(all_predictors())

# ワークフロー
wf_rev =
  workflow() %>% 
  add_recipe(rec_rev) %>% 
  add_model(rule_rev)

# モデル構築
model_rev =
  wf_rev %>% 
  fit(data = train) %>% 
  with_seed(1234, .)

# {broom} 利用
model_rev %>% 
  glance() # error

model_rev %>% 
  tidy() # error

model_rev %>% 
  augment(new_data = test) %>% 
  select(starts_with(".pred"), Status) # augmentのみ使える












