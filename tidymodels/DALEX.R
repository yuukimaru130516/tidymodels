# モデル解釈
library(DALEXtra)


# PFI 説明変数重要度 -------------------------------------------------------------

# 学習ルール
rule = 
  rand_forest() %>% 
  set_engine(engine = "ranger") %>% 
  set_mode("classification")

# レシピ
rec =
  recipe(Status ~ . , data = train) %>% 
  step_impute_bag(all_predictors()) %>% 
  step_mutate_at(
    all_nominal_predictors(), fn = ~as.integer(.)
  )

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

# {DALEXtra} 読み込み
library(DALEXtra)

# explainer作成
explainer = 
  model %>% 
  explain_tidymodels(
    data = test %>% select(-Status),
    y = test$Status %>% as_integer(),
    label = "rand_forest"
  )

# 特徴量重要度算出
pfi =
  explainer %>% 
  model_parts(
    type = "ratio"
  ) %>% 
  with_seed(1234, .)

# 特徴量重要度可視化
plot(pfi)
