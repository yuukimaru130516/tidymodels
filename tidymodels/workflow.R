# 学習ルール設定と特徴量エンジニアリングを同時に行う

# 学習ルール(ロジスティックモデル)
rule =
  logistic_reg() %>%
  set_engine('glm')

# レシピ
rec =
  recipe(Status ~ ., data = train) %>% 
    step_mutate(
      new = Income + Assets
    ) %>% 
    step_zv(all_predictors())


# 予測モデリング wfなし ------------------------------------------------------------

# モデル
model_nwf =
rule %>% 
  fit(Status ~ ., rec %>% prep() %>% bake(train))

# 予測値
pred_nwf =
  model_nwf %>% 
    predict(rec %>% prep() %>% bake(test))


# 予測モデリング wfあり ------------------------------------------------------------
# wfを通すことで記述が単純化される = 可読性が高くなる

# ワークフロー
wf =
  workflow() %>% 
    add_recipe(rec) %>% 
    add_model(rule)

# モデル
model_wf =
  wf %>% 
    fit(data = train)

#予測値
pred_wf =
  model_wf %>% 
    predict(test)

# wfなしとwfありの予測値の比較
bind_cols(
  pred_nwf %>% rename(nwf = .pred_class),
  pred_wf %>% rename(wf = .pred_class)
) %>% 
  print(n=100)


# 予測モデリング データ分割あり ---------------------------------------------------------
# ハイパラは固定

# stratified分割
stratified_splits =
  vfold_cv(train, v = 4, strata = "Status") %>% 
    with_seed(1234, .)

# 学習ルール(random forest)
rule =
  rand_forest(mtry = 10, 
              min_n = 5, 
              trees = 100) %>%
  set_engine('ranger') %>%
  set_mode('classification')

# レシピ
rec =
  recipe(Status ~ . , data = train) %>% 
    step_impute_bag(all_predictors())

# ワークフロー
wf =
  workflow() %>% 
    add_recipe(rec) %>% 
    add_model(rule)

# tune (各ワークフローの成績)
tune =
  wf %>% 
    fit_resamples(
      stratified_splits,
      metrics = metric_set(accuracy),
      control = 
        control_resamples(
          save_pred = TRUE
        )
    )

# 各assessmentの予測値
tune %>% 
  pluck(".predictions", 1)

# 各foldの評価指標
tune %>% 
  pluck(".metrics", 1)

# モデル
model =
  wf %>% 
    fit(data = train)

# 予測値
pred =
  model %>% 
  predict(test)









