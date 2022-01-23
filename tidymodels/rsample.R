# データ分割

# 1. K分割
kfold_splits =
rsample::vfold_cv(train, v = 4) %>% 
  with_seed(1234, .)

# splits列1行目のanalysis
kfold_splits %>% 
  pluck("splits", 1) %>% 
  analysis()

# splits列1行目のassessment
kfold_splits %>% 
  pluck("splits", 1) %>% 
  assessment()

# 各行のsplitに対するanalysisとassessment
kfold_splits %>% 
  rowwise() %>% 
  mutate(
    .analysis = list(splits %>%  analysis()),
    .assessment = list(splits %>% assessment())
  )

# 列名についているドットは名前の衝突を避けるため

# 2. hold-out分割 K=1に該当
hold_out_split =
validation_split(train, prop = 0.7) %>% 
  with_seed(1234, .)


# 3. leave-one-out分割 学習データ数に分割 K=学習データ
leave_one_out_splits = 
  loo_cv(train)

# 4. stratified分割 analysisとassessmentのカテゴリの割合が等しくなるように分割
stratified_splits = 
  vfold_cv(train, v = 4, strata = "Status") %>% 
    with_seed(1234, .)

# train の Status割合
train %>% 
  count(Status) %>% 
  mutate(
    prop = n / sum(n)
  )

# stratified分割による各foldのStatusのgoodの割合
stratified_splits %>% 
  rowwise() %>% 
  mutate(
    .analysis = list(splits %>% analysis()),
    .assessment = list(splits %>% assessment()),
    .analysis_prop_good = 
      .analysis %>% 
        count(Status) %>% 
        mutate(
          prop = n/sum(n)
        ) %>% 
      filter(Status == "good") %>% 
      pull(prop),
    .assessment_prop_good = 
      .assessment %>% 
      count(Status) %>% 
      mutate(
        prop = n/sum(n)
      ) %>% 
      filter(Status == "good") %>% 
      pull(prop),
  ) 

# K分割による各foldのStatusのgoodの割合
kfold_splits %>% 
  rowwise() %>% 
  mutate(
    .analysis = list(splits %>% analysis()),
    .assessment = list(splits %>% assessment()),
    .analysis_prop_good = 
      .analysis %>% 
      count(Status) %>% 
      mutate(
        prop = n/sum(n)
      ) %>% 
      filter(Status == "good") %>% 
      pull(prop),
    .assessment_prop_good = 
      .assessment %>% 
      count(Status) %>% 
      mutate(
        prop = n/sum(n)
      ) %>% 
      filter(Status == "good") %>% 
      pull(prop),
  ) 


# 5. 時系列分割 左
timeseries_splits = 
rolling_origin(
  train %>% rowid_to_column(),
  initial = 100,
  assess = 20,
  skip = 100,
  lag = 0,
  cumulative = FALSE
)

timeseries_splits %>% 
  pluck("splits", 1) %>% 
  analysis()

timeseries_splits %>% 
  pluck("splits", 1) %>% 
  assessment()

timeseries_splits %>% 
  pluck("splits", 2) %>% 
  analysis()

timeseries_splits %>% 
  pluck("splits", 2) %>% 
  assessment()

# 5. 時系列分割 右
timeseries_splits = 
  rolling_origin(
    train %>% rowid_to_column(),
    initial = 100,
    assess = 20,
    skip = 100,
    lag = 0,
    cumulative = TRUE
  )

timeseries_splits





