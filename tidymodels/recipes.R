# 特徴量エンジニアリング
# データをアルゴリズムに合わせて変形すること

# 1. 数値変数の変換
recipe(Status ~ ., data = train) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% 
  prep() %>% 
  bake(new_data = train)

# ２．カテゴリ変数の変換
#one-hot encoding
recipe(Status ~ ., data = train) %>% 
  step_dummy(Records, one_hot = TRUE) %>% 
  prep() %>% 
  bake(new_data = train) %>% 
  select(starts_with("Records"))

# label encoding
recipe(Status~., data = train) %>% 
  step_mutate_at(Records, fn = ~as.integer(.)) %>% 
  prep() %>% 
  bake(new_data = train)

recipe(Status~., data = train) %>% 
  step_mutate_at(all_nominal_predictors(), fn = ~as.integer(.)) %>% 
  prep() %>% 
  bake(new_data = train)

# ３．欠損値の補完 ①代表値補間
# mean
recipe(Status ~ ., data = train) %>% 
  step_impute_mean(all_numeric_predictors()) %>% 
  prep() %>% 
  bake(new_data  = train)

# median
recipe(Status ~ ., data = train) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  prep() %>% 
  bake(new_data  = train)

# modeは文字列型の変数に使用可能
recipe(Status ~ ., data = train) %>% 
  step_impute_mode(all_nominal_predictors()) %>% 
  prep() %>% 
  bake(new_data  = train)

# double型に変換
train_db = 
  train %>% 
  mutate(
    across(where(is.numeric),
           ~ as.double(.x)
    )
  )

# 移動窓数と統計量に応じて補完
recipe(Status ~ ., data = train_db) %>% 
  step_impute_roll(
    all_numeric_predictors(), statistic = mean, window = 5
  ) %>% 
  prep() %>% 
  bake(new_data = train_db)

# ② 予測補間
# 線形回帰
recipe(Status ~ ., data = train) %>% 
  step_impute_linear(
    all_numeric_predictors(),
    impute_with = imp_vars(Seniority, Time:Records, Expenses)
  ) %>% 
  prep() %>% 
  bake(new_data = train)

# k近傍法 データ型はなんでもOK!
recipe(Status ~ ., data = train) %>% 
  step_impute_knn(
    all_predictors(),
    #impute_with = imp_vars(Seniority, Time:Records, Expenses)
  ) %>% 
  prep() %>% 
  bake(new_data = train)

# 決定木 バギング
recipe(Status ~ ., data = train) %>% 
  step_impute_bag(
    all_predictors(),
    #impute_with = imp_vars(Seniority, Time:Records, Expenses)
  ) %>% 
  prep() %>% 
  bake(new_data = train)

# ４．その他の便利関数
train_4 = 
train %>% 
  select(Status, Seniority:Age)

recipe(Status ~ ., data = train_4) %>% 
  step_mutate(new = Time + Age) %>% 
  prep() %>% 
  bake(new_data = train_4)

recipe(Status ~ ., data = train_4 %>% mutate(new = Time * 2)) %>% 
  step_corr(all_numeric_predictors()) %>% 
  prep() %>% 
  bake(new_data = train_4)

# スプライン近似のための基底変換列を作成
recipe(Status ~ ., data = train_4) %>% 
  step_bs(Time, degree = 3) %>% 
  prep() %>% 
  bake(new_data = train_4)

# 欠損値がある行を削除
recipe(Status ~ ., data = train_4) %>% 
  step_naomit(Home) %>% 
  prep() %>% 
  bake(new_data = train_4)
