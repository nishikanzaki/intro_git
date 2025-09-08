# 仮想的な母集団（全世帯の年収）を生成
# 対数正規分布に従う

# ライブラリ
library(ggplot2)


# 母集団の作成 ------------------------------------------------------------------

# 母集団データの作成（対数正規分布）
set.seed(34)
population_income <- rlnorm(100000, meanlog = 15.5, sdlog = 0.5)


# 平均と標準偏差の計算、ヒストグラムの描画 ----------------------------------------------------

# 平均と標準偏差を計算 ####
population_mean <- mean(population_income)
population_sd <- sd(population_income)

cat(sprintf("母集団の平均年収：%.2f\n", population_mean))
cat(sprintf("母集団の標準偏差：%.2f\n", population_sd))

# ヒストグラムを描画
ggplot(data.frame(income = population_income), aes(x = income)) +
  geom_histogram(
    bins = 20, 
    aes(y = after_stat(density)),  # 確率密度として表示
    fill = "skyblue", 
    color = "black",
    alpha = 0.7
  ) +
  geom_vline(
    xintercept = population_mean, 
    color = "red", 
    linetype = "dashed", 
    linewidth = 1,
    show.legend = TRUE
  ) +
  labs(
    title = "年収の分布(母集団)",
    x = "年収",
    y = "確率密度"
  ) +
  theme_minimal()



# サンプルを抽出する ---------------------------------------------------------------

# サンプルを抽出（100世帯）
set.seed(42)  # 再現性のため
sample_size <- 100
sample_income <- sample(population_income, size = sample_size, replace = FALSE)

# 平均と標準偏差（不偏分散, n-1 で割る）を計算
sample_mean <- mean(sample_income)
sample_std <- sd(sample_income)  # R の sd() はデフォルトで n-1 で割る

# 結果を表示（小数点以下2桁）
cat("サンプル平均年収：", format(round(sample_mean, 2), nsmall = 2), "\n")
cat("サンプル標準偏差：", format(round(sample_std, 2), nsmall = 2), "\n")


# 95％信頼区間の算出 --------------------------------------------------------------


#### SEの定義に従って区間推定
# サンプルサイズ
n <- length(sample_income)

# 標準誤差（SE）
sample_se <- sample_std / sqrt(n)

# 自由度
df <- n - 1

# t 値（両側 95% 信頼区間）
t_crit <- qt(0.975, df = df)  # 上側 2.5% の点

# 信頼区間の計算
lower <- sample_mean - t_crit * sample_se
upper <- sample_mean + t_crit * sample_se
confidence_interval <- c(lower, upper)

# 表示
cat("平均年収の95%信頼区間：", format(round(confidence_interval, 2), nsmall = 2), "\n")


#### 関数を使って区間推定
# t.test で信頼区間を計算
result <- t.test(sample_income, conf.level = 0.95)

# 信頼区間を表示
cat("平均年収の95%信頼区間：", 
    format(round(result$conf.int, 2), nsmall = 2), "\n")


# 最尤推定法 -------------------------------------------------------------------

n <- 10
k <- 7

p_candidates <- seq(0,1, by = 0.01)

# 二項分布の確率質量関数で尤度を計算
# Pythonのstats.binom.pmf(k, n, p) と同じ。
likelihoods <- dbinom(k, size = n, prob = p_candidates)

# 最大尤度を与える p を取得
# which.max():ベクトルの中で最大値を持つ要素の位置
# （インデックス番号）」 を返す関数
mle <- p_candidates[which.max(likelihoods)]

# sprintf():
# format 文字列("最尤推定法による推定値：%.2f\n")に従って、
# 数値や文字を埋め込んだ文字列を作る関数
# "%.2f" は 「小数点以下を2桁まで表示する浮動小数点数」 という指定
# \n は改行
cat(sprintf("最尤推定法による推定値：%.2f\n", mle))
df <- data.frame(p_candidates, likelihoods)

ggplot(df, aes(x = p_candidates, y = likelihoods)) +
  geom_line(aes(color = "尤度曲線"), linewidth = 1) +  # 黒線にラベル
  geom_vline(aes(xintercept = mle, color = "最尤推定値"), 
             linetype = "dashed", linewidth = 1) +  # 赤破線にラベル
  scale_color_manual(
    values = c("尤度曲線" = "darkblue", "最尤推定値" = "red")
  ) +
  labs(
    title = "二項分布の尤度関数",
    x = "表が出る確率",
    y = "尤度",
    color = "凡例"
  ) +
  theme_minimal()# 余白や装飾を極力省いたシンプルなテーマ



# 最小二乗法 -------------------------------------------------------------------

x_values = c(1,1,1,1,1,1,1,0,0,0)

# 誤差関数を定義
error_function <- function(p){
  sum((x_values - p)^2)
}

# 最小化（bounds = c(0,1))
result <- optimize(
  error_function,
  interval = c(0,1)
)  

# 最小二乗法による推定値
least_squares_p <- result$minimum
cat(sprintf("最小二乗法による推定値：%.2f\n",
            least_squares_p))


# MCMC法 -------------------------------------------------------------------

# RStanパッケージのインストールと読み込み
# install.packages("rstan")
library(rstan)

# データリストの作成
# Stanモデルに渡すデータをリスト形式で準備
stan_data <- list(
  n = 10, # 試行回数
  k = 7   # 表が出た回数
)

# Stanモデルのコードを文字列として定義
stan_model_code <- '
data {
  int<lower=0> n; // 試行回数
  int<lower=0> k; // 表が出た回数
}

parameters {
  real<lower=0, upper=1> p; // コインの表が出る確率
}

model {
  // 事前分布: p ~ Beta(1, 1)
  p ~ beta(1, 1);

  // 尤度: k ~ Binomial(n, p)
  k ~ binomial(n, p);
}
'

# Stanモデルのコンパイルとサンプリングの実行
fit <- stan(
  model_code = stan_model_code,
  data = stan_data,
  iter = 11000, # PyMCの tune + draws に相当 (10000 + 1000)
  warmup = 1000, # バーンイン
  chains = 2,
  seed = 42 # 乱数シード
)

# 事後分布の要約統計量を取得
print(fit, pars = c("p"))

# 事後分布のサンプルを取得
posterior_p <- as.array(fit, pars = "p")[, , 1]

# 事後分布の平均と標準偏差を計算
p_mean <- mean(posterior_p)
p_std <- sd(posterior_p)

# 結果の出力
cat(sprintf("事後分布の推定平均: %.2f\n", p_mean))
cat(sprintf("事後分布の推定標準偏差: %.3f\n", p_std))

# 事後分布のグラフを描く

# ggplot2 と dplyr パッケージを読み込む
# install.packages("ggplot2")
# install.packages("dplyr")
library(ggplot2)
library(dplyr)

# 事後分布のサンプルをデータフレームに変換
# rjags や rstan の出力形式に合わせて、この部分は適宜修正してください
posterior_data <- data.frame(p = as.numeric(posterior_p))

# 事後分布の平均値を計算
p_mean <- mean(posterior_p)

# グラフの作成
ggplot(posterior_data, aes(x = p)) +
  # ヒストグラムのレイヤーを追加
  geom_histogram(
    bins = 30,
    fill = "skyblue",
    color = "black"
  ) +
  # 密度曲線のレイヤーは削除
  
  # 平均値の垂直線のレイヤーを追加
  geom_vline(
    xintercept = p_mean,
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  # 平均値のテキストラベルを追加
  annotate(
    "text",
    x = p_mean,
    y = 500, # Y座標を適切な頻度値に調整
    label = paste("平均 =", round(p_mean, 2)),
    color = "red",
    vjust = -1.5,
    hjust = -0.1
  ) +
  # グラフのタイトルと軸ラベルを設定
  labs(
    title = "事後分布: 表が出る確率 p",
    x = "表が出る確率 p",
    y = "頻度 (サンプル数)"
  ) +
  # 全体的なテーマを調整
  theme_minimal() +
  
  # Y軸のラベルを200ごとに増加
  scale_y_continuous(breaks = seq(0, max(ggplot_build(last_plot())$data[[1]]$count), by = 200)) +
  
  # X軸のラベルを0.1ごとに増加
  scale_x_continuous(breaks = seq(0, 1, by = 0.1))


# 統計的仮説検定 -----------------------------------------------------------------

# ダイエット結果（体重変化量）のサンプルデータを生成
set.seed(42)  # 乱数シードを設定
wl <- rnorm(   # 正規分布
  50,         # サンプルサイズ
  2.1,        # 平均値
  1.0         # 標準偏差
)

# 帰無仮説「平均は2kg と等しい」を検定
# t.test 関数（1標本のt検定）を実行
# t.test(wl, mu = 2): Rで1標本のt検定を実行する標準関数です。
# 第1引数 x=wl には、検定対象となるデータベクトルを渡します。
# mu = 2 は、帰無仮説における母集団の平均値（比較対象となる値）を指定します。
t_test_result <- t.test(wl, mu = 2)

# 結果の表示
print(paste("サンプル平均:", round(mean(wl), 2), "kg"))
print(paste("t 統計量:", round(t_test_result$statistic, 4)))
print(paste("p 値:", round(t_test_result$p.value, 4)))

cat(sprintf("サンプル平均: %.2f", round(mean(wl),2)), "kg")
cat(sprintf("t 統計量: %.4f",  round(t_test_result$statistic, 4)))
cat(sprintf("p値: %.4f", round(t_test_result$p.value, 4)))


# 時系列データの成分分解 -------------------------------------------------------------

# 趨勢 ----------------------------------------------------------------------


# wbstatsとggplot2をインストール（初回のみ）
# install.packages("wbstats")
# install.packages("ggplot2")

# パッケージを読み込む
# Pythonのwbdataに相当する世界銀行のデータ取得ライブラリです。

library(wbstats)
library(ggplot2)
library(dplyr)
library(readr)

# 日本のGDPデータを世界銀行から取得
# wb_data()関数を使用して、世界銀行のデータにアクセスします。
# country = "JP": 取得する国を日本の国コード「JP」で指定します。
# indicator = "NY.GDP.MKTP.CD": 取得する指標をGDPのコードで指定します。
# return_wide = FALSE: データを「tidy」（整形された）形式で取得します。
# これはggplot2でのプロットに適しています。


# j_gdp <- wb_data(
#   country   = "JPN",
#   indicator = "NY.GDP.MKTP.CD",
#   return_wide = FALSE
# )

j_gdp <- read_csv("japan_gdp.csv") %>%
  filter(Year >= 1960)


library(ggplot2)
library(dplyr)

# GDP を兆ドルに変換
j_gdp <- j_gdp %>%
  mutate(GDP_trillion = GDP / 1e6)

# プロット
ggplot(j_gdp, aes(x = Year, y = GDP_trillion)) +
  geom_line(color = "steelblue", size = 1) +
  labs(
    title = "Japan GDP (World Bank, current USD, Trillion USD)",
    x = "Year",
    y = "GDP (Trillion USD)"
  ) +
  theme_minimal()




# 季節性 ---------------------------------------------------------------------

# パッケージのインストール（初回のみ）
# install.packages("tsibble")
# install.packages("fpp2")  # ワイン販売データが含まれるパッケージ
# install.packages("ggplot2")
# install.packages("lubridate")
# install.packages("dplyr")

# パッケージの読み込み（インストールがまだの場合は、install.packages()を実行）
library(tsibble)
library(fpp2)      # ワイン販売データが含まれるパッケージ
library(ggplot2)
library(lubridate)
library(dplyr)

# 1. データの準備
# fpp2パッケージのwineindデータセットを使用
# dateとwineindの列を持つtibbleを作成
# wineindデータはfpp2パッケージに元々含まれています。
# tibble()関数を使って、日付情報（date）と販売量（wineind）の2つの列
# を持つデータフレームを作成します。seq()とymd()を使い、1980年1月から
# 始まる月次の日付インデックスを生成しています。

wine_df <- tibble(
  date = seq(from = ymd("1980-01-01"), by = "month", length.out = length(wineind)),
  wineind = wineind
)

# tibbleをtsibbleに変換（時系列データを扱うための形式）
# as_tsibble()関数で、作成したデータフレームをtsibbleオブジェクトに
# 変換します。これにより、Rがデータを時系列データとして正しく認識し、
# 時系列分析やプロットが容易になります。

wine_ts <- as_tsibble(wine_df, index = date)

# 2. データの前処理
# 1991年以降のデータのみをフィルタリング
# dplyrのfilter()関数を使って、lubridateのyear()関数で抽出した年が
# 1991年以上の行だけを抽出します。これはPythonのloc['1991':]と同じ
# フィルタリング操作です。
wine_filtered <- wine_ts %>%
  filter(year(date) >= 1991)

# 3. データの可視化
# ggplot2を使用して折れ線グラフをプロット

ggplot(wine_filtered, aes(x = date, y = wineind)) +
  geom_line(color = "darkblue", linewidth = 1) +  # 濃い青の線を太く描画
  labs(
    title = "AUS ワインの月次販売量（1991年以降）",
    x = "年",
    y = "ワイン販売量"
  ) +
  theme_minimal()

autoplot(window(wineind, start = 1991))+
  geom_line(color = "blue", size = 1.5) +  
  ggtitle("AUS ワインの月次販売量（1991年以降）") +
  xlab("年") +
  ylab("ワイン販売量") +
  theme_minimal()


# 時系列データの成分分解 -------------------------------------------------------------

# 必要なパッケージを読み込みます
library(fpp2)      # AirPassengers データセットを含むパッケージ
library(ggplot2)
library(lubridate)
library(dplyr)

# 1. データの準備
# RのAirPassengersはすでに時系列オブジェクトです。
# まず、扱いやすいtibble（データフレーム）に変換します。
airpassengers_df <- tibble(
  date = seq(from = ymd("1949-01-01"), by = "month", length.out = length(AirPassengers)),
  passengers = as.vector(AirPassengers) # 値をベクトルに変換して代入
)
head(airpassengers_df)

# tibbleをtsibbleオブジェクトに変換します
airpassengers_ts <- as_tsibble(airpassengers_df, index = date)
head(airpassengers_ts)

# 2. データのプロット
# ggplot2を使用してプロットします
ggplot(airpassengers_ts, aes(x = date, y = passengers)) +
  geom_line(color ="darkblue", linewidth = 1) +
  labs(
    title = "国際航空旅客数",
    x = "年",
    y = "旅客数"
  ) +
  theme_minimal()

autoplot(AirPassengers)

class(AirPassengers)
class(airpassengers_df)
class(airpassengers_ts)

# STL分解 -------------------------------------------------------------------

# パッケージの読み込み
# feastsは、"Feature Extraction and STatistical Summary"
# 時系列データの特徴量抽出（feature extraction）や統計要約（statistical summary）
# に特化しています
# 時系列データから分析に役立つ特徴量を抽出し可視化するための関数を提供しています。
library(feasts)
library(fpp2)      # AirPassengers データセットを含む
library(tsibble)   # 時系列データを扱うための形式
library(ggplot2)
library(dplyr)
library(lubridate)

# 1. データの準備
# RのAirPassengersデータをtsibble形式に変換
airpassengers_ts <- AirPassengers %>%
  as_tsibble() %>%
  rename(passengers = value)

# 2. STL分解の実行と結果の取得
# model()関数でSTLモデルを指定し、components()で各成分を抽出
ap_decomp <- airpassengers_ts %>%
  model(STL(passengers ~ season(period = 12))) %>%
  components()

# 3. 結果のプロット
autoplot(ap_decomp, colour = "darkblue", size = 1) +
   labs(
    title = "国際航空旅客数のSTL分解",
    subtitle = "トレンド、季節性、および残差成分"
  )

# 左端にある棒グラフのようなものは、これらのグラフの変動幅（スケール）
# を可視化する役割を持っています。
# 棒グラフの高さ: これは、**季節性（Seasonal）**成分のY軸の範囲に対応
# しています。


# 各パネルのY軸: トレンド、季節性、残差の各パネルは、それぞれ異なる
# スケールで描画されますが、棒グラフと同じ高さで比較することができます。
# この棒グラフがあることで、データの変動全体（Observed）のうち、
# どの程度がトレンドによるものか、どの程度が季節性によるものか、
# どの程度が残差によるものかを相対的に比較することが容易になります。
# 例えば、この棒グラフがトレンドや残差のパネルの高さに対して非常に大
# きい場合、そのデータは強い季節性を持っていると判断できます。


# MSTL分解 ------------------------------------------------------------------

# 必要なパッケージを読み込みます
library(feasts)  # 時系列データの特徴量（features）を抽出し、
                 # 分解・可視化するための現代的なパッケージです。
library(tsibble) # データの形式を、時系列分析に最適な**tsibble**という
                 #データフレーム形式に変換・管理するためのパッケージです。
library(forecast) # 多くの時系列予測・分析手法を提供する伝統的なパッケージです。
                  # 今回のコードでは**msts()関数とmstl()**関数がこのパッケージに含まれています。
library(dplyr)

# データの準備
# 既存の`airpassengers_ts` tsibbleオブジェクトを使用

# MSTL分解の実行と結果の取得
# 1. 複数の周期を持つ時系列オブジェクト（msts）を作成
# このステップでは、元の時系列データairpassengers_ts$passengersを、
# 複数の季節性を持つ時系列オブジェクト（msts）に変換しています。
# seasonal.periods = c(6, 12)は、このデータに周期6（半年）と周期12
#（1年）の2つの季節性が含まれていることを明示的に指定しています。
# mstsオブジェクトは、この複数の季節性情報をmstl()関数に渡すために
# 不可欠な形式です。
airpassengers_msts <- msts(
  airpassengers_ts$passengers, 
  seasonal.periods = c(6, 12)
)

# 2. mstsオブジェクトをmstl()に渡す
# mstsオブジェクトであるairpassengers_mstsを**mstl()**関数に渡すこと
# で、**MSTL分解（Multiple Seasonal-Trend decomposition using Loess）
# **を実行しています。
ap_mstl <- mstl(airpassengers_msts)

# 3. 結果のプロット
# 最後に、feastsパッケージの**autoplot()**関数を使って、分解結果を
# 視覚化しています。autoplot()は、mstl()の出力のような特定のオブジェ
# クトの種類を自動的に認識し、最も適切なグラフを生成してくれます。
# この場合、トレンド、季節性（6ヶ月）、季節性（12ヶ月）、残差、そして元のデータ
# が、見やすく配置された複数のパネルグラフとして描画されます。
autoplot(ap_mstl)


# コレログラム分析 ----------------------------------------------------------------

# 必要なパッケージを読み込みます
library(forecast)
library(ggplot2)

# 2.15のコードで作成された 'airpassengers_ts' オブジェクトを使用

# プロットを2行1列に配置
# mar 引数で下、左、上、右の余白を調整
# デフォルト: c(5.1, 4.1, 4.1, 2.1)
# 変更後: 上側の余白を広げ、タイトルが表示されるようにする
par(mfrow = c(2, 1), mar = c(4, 4, 3, 2) + 0.1)

# 自己相関 (ACF) のプロット
# `main`引数でタイトルを明示的に指定
acf(airpassengers_ts$passengers, main = "Autocorrelation Function (ACF)", 
    xlab = "Lag", ylab = "ACF", lag.max = 40)

# 偏自己相関 (PACF) のプロット
# `main`引数でタイトルを明示的に指定
pacf(airpassengers_ts$passengers, main = "Partial Autocorrelation Function (PACF)", 
     xlab = "Lag", ylab = "PACF", lag.max = 40)

# プロット設定をリセット
# これを忘れると、その後のプロットもこのレイアウトになります
par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))

ggtsdisplay(airpassengers_df$passengers, main = "airpassengers")


# 定常性と非定常性 ----------------------------------------------------------------

# 必要なパッケージ
# install.packages("urca")
# install.packages("tseries")

library(urca)
library(tseries)

# ADF 検定を行う関数（修正版）####
adf_test <- function(series, type = "drift") {
  # type: "none" (定数なし), "drift" (定数あり), "trend" (定数+トレンド)
  
  result <- ur.df(na.omit(series), type = type, selectlags = "AIC")
  
  cat("=== ADF 検定結果 ===\n")
  cat(" 検定統計量 (ADF Statistic):", result@teststat[1], "\n")
  
  crit <- result@cval[1, ]
  cat(" 臨界値 (1%):", crit["1pct"], 
      " 5%:", crit["5pct"], 
      " 10%:", crit["10pct"], "\n")
  
  if (result@teststat[1] < crit["5pct"]) {
    cat(" 帰無仮説を棄却 → 定常\n\n")
  } else {
    cat(" 帰無仮説が棄却できず → 非定常かもしれない\n\n")
  }
}

adf_test(AirPassengers, type = "drift")  # 定数＋トレンド
# 呼び出し時に "trend" を指定すれば良い

# 通常のADF検定
summary(ur.df(AirPassengers, type = "drift"))

# typeの選択ができないので、使わない方がよい
adf.test(AirPassengers)


# KPSS 検定を行う関数####
kpss_test <- function(series, null = "Level") {
  # null: "Level" (定数のみ), "Trend" (定数+トレンド)
  
  result <- kpss.test(na.omit(series), null = null)
  
  cat("=== KPSS 検定結果 ===\n")
  cat(" 検定統計量 (KPSS Statistic):", result$statistic, "\n")
  cat(" p 値 (p-value):", result$p.value, "\n")
  
  if (result$p.value < 0.05) {
    cat(" 帰無仮説を棄却 → 非定常\n\n")
  } else {
    cat(" 帰無仮説が棄却できず → 定常かもしれない\n\n")
  }
}

# 例: AirPassengers に適用
kpss_test(AirPassengers, null = "Level")  # 定数+トレンド

# 通常のKPSS検定(帰無仮説：単位根なし）
summary(ur.kpss(AirPassengers, type = "mu"))
kpss.test(AirPassengers, null = "Level")



# 定常化の手法 ------------------------------------------------------------------

# トレンド調整の手法 ####

# 一階差分
df_lag1 <- diff(airpassengers_df$passengers)

# プロット
plot(df_lag1,
     type = "l",
     main = "一階差分を取った時系列データ",
     xlab = "時点",
     ylab = "差分値")


# 時間 t（1,2,3,...のインデックス）
t <- seq_along(airpassengers_df$passengers)

# 線形回帰モデルの学習
model <- lm(airpassengers_df$passengers ~ t)

# トレンドの予測
trend <- predict(model)

# トレンド除去
df_detrended <- airpassengers_df$passengers - trend

# プロット
plot(df_detrended,
     type = "l",
     main = "線形回帰モデルでトレンド除去した時系列データ",
     xlab = "時点",
     ylab = "トレンド除去後の値")


# 季節調整の手法 ####

class(AirPassengers)
df_des <- diff(AirPassengers, lag = 12)
plot(df_des,
     main = "季節差分を取った時系列データ")


# 分散の安定化の手法 ####

library(forecast)

# 対数変換
df_log <- log(AirPassengers)

# Box-Cox 変換（λを自動推定）
# Python → 尤度最大化法（デフォルト）
# R → Guerrero法（分散安定化）（デフォルト）
# Rでも method="loglik" を指定すれば、Pythonに近い λ が出るはずです。
lambda_param <- BoxCox.lambda(AirPassengers, method = "loglik")
df_bc <- BoxCox(AirPassengers, lambda_param)

# プロット
par(mfrow = c(2, 1))   # 2行1列に並べる

plot(df_log,
     type = "l",
     main = "対数変換後の時系列データ",
     xlab = "時間", ylab = "値")

plot(df_bc,
     type = "l",
     main = "Box-Cox 変換後の時系列データ",
     xlab = "時間", ylab = "値")
mtext(paste("Box-Cox 変換のλパラメータ:", round(lambda_param, 3)),
      side = 3, line = -1.5, adj = 0, cex = 0.9)

par(mfrow = c(1, 1))   # レイアウトを戻す



# 実践的な応用例 -----------------------------------------------------------------

# ==============================
# 必要なライブラリ
# ==============================
library(urca)     # ADF 検定
library(tseries)  # KPSS 検定

# ==============================
# ADF 検定ラッパー（判定コメント付き）
# ==============================
adf_test <- function(series, type = "drift") {
  adf <- ur.df(na.omit(series), type = type, selectlags = "AIC")
  
  # 検定統計量（ADF Statistic）
  stat <- adf@teststat[1]            
  # 5%臨界値
  crit <- adf@cval[1, "5pct"]       
  
  cat("=== ADF 検定結果 ===\n")
  cat("検定統計量 (ADF Statistic):", round(stat, 4), "\n")
  
  # 判定
  if(stat < crit) {
    cat("帰無仮説が棄却 → 定常の可能性あり\n")
  } else {
    cat("帰無仮説が棄却できず → 非定常かもしれない\n")
  }
}

# ==============================
# KPSS 検定ラッパー（判定コメント付き）
# ==============================
kpss_test <- function(series, null = "Level") {
  kpss <- kpss.test(na.omit(series), null = null)
  
  cat("\n=== KPSS 検定結果 ===\n")
  cat("検定統計量 (KPSS Statistic):", round(kpss$statistic, 4), "\n")
  cat("p 値 (p-value):", round(kpss$p.value, 4), "\n")
  
  if(kpss$p.value < 0.05) {
    cat("帰無仮説を棄却 → 非定常\n")
  } else {
    cat("帰無仮説を棄却できず → 定常かもしれない\n")
  }
}

# ==============================
# 可視化 + 検定まとめ関数
# ==============================
plot_and_stationarity_tests <- function(
    series,           # 時系列データ
    title,            # グラフタイトル
    adf_type = "drift",  # ADF 検定タイプ ("none", "drift", "trend")
    kpss_null = "Level"  # KPSS 帰無仮説 ("Level", "Trend")
) {
  par(mfrow = c(3, 1))
  
  # (a) 時系列プロット
  plot(series, type = "l", main = title, ylab = "値", xlab = "時間")
  
  # (b) ACF
  acf(na.omit(series), main = "ACF (自己相関)")
  
  # (c) PACF
  pacf(na.omit(series), main = "PACF (偏自己相関)")
  
  par(mfrow = c(1, 1))
  
  # ADF 検定
  adf_test(series, type = adf_type)
  
  # KPSS 検定
  kpss_test(series, null = kpss_null)
}

# ==============================
# データ準備 (AirPassengers をデータフレーム化)
# ==============================
df_ap <- data.frame(y = as.numeric(AirPassengers))

# ==============================
# 使用例
# ==============================
plot_and_stationarity_tests(
  df_ap$y,
  title = "元の時系列データ"
)
