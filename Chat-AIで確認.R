library(parsnip)

# my_lm() 関数の定義
my_lm <- function(mode = "regression", engine = "lm") {
  if (mode != "regression") {
    rlang::abort("this mode is not a known mode for my_lm()")
  }
  parsnip::new_model_spec(
    "my_lm",
    args = list(),
    eng_args = NULL,
    mode = mode,
    method = NULL,   # methodを設定しない
    engine = engine  # エンジンを明示的に設定
  )
}

# モデル "my_lm" を登録
set_new_model("my_lm")

# モードを"regression"として設定
set_model_mode(model = "my_lm", mode = "regression")

# エンジン "lm" をモデル "my_lm" に登録
set_model_engine(model = "my_lm", mode = "regression", eng = "lm")

# 学習の際の設定を入力
set_fit(
  model = "my_lm",
  eng = "lm",
  mode = "regression",
  value = list(
    interface = "formula",               # フォーミュラを使う
    protect = c("formula", "data"),      # 必須引数
    func = c(pkg = "stats", fun = "lm"), # lm() 関数を使う
    defaults = list()                    # デフォルト設定
  )
)

# 予測の際の設定を入力
set_pred(
  model = "my_lm",
  eng = "lm",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(
        object = rlang::expr(object$fit),
        newdata = rlang::expr(new_data),
        type = "response"
      )
  )
)

# モデル定義の確認
show_model_info("my_lm")

# 自作の線形回帰モデルの定義
my_lm_mod <- my_lm() %>%
  set_engine("lm")

# 学習の実行
fit_ed <- my_lm_mod %>%
  fit(Sepal.Length ~ Sepal.Width, data = iris)
