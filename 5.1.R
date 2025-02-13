# 第5章 時系列データを活用したビジネス事例

# 5.1 モニタリング指標の異常検知によるキャンペーン評価（自動車ディーラー）

# 5.1.1 事例説明

# 従来のキャンペンを実施していた2017年12月までのデータを使って予測モデルを構築し、
# 2018年の販売台数を予測する。
# その予測値を「従来のキャンペーンを2018年1月も継続した場合」の販売台数と見なす。
# この予測値と2018年の実測値を比較し、異常な乖離があるかどうかを見ることで、
# キャンペーンの良し悪しを評価する。

# 5.1.2 データセットと分析概要

# 外れ値スコア（標準化残差の絶対値）= |(残差-残差の平均)/残差の標準偏差)|
# 
# 
# 外れ値スコアが「絶対値が1以上」「絶対値が2以上」「絶対値が3以上」かどうかを見て、
# 2σ以上の場合に異常値であると判断する。
# 
# 5.1.3 Rの実施例
# ざっくりした流れ
#  ステップ１：準備
#  ステップ２：異常検知で利用するモデルの構築
#  ステップ３：異常検知（評価対象データの期間）
# 


# ステップ１：準備 ----------------------------------------------------------------


# まず、必要なライブラリー（パッケージ）のモジュールを読み込む。

# code 5-1 -----------------------------------------------------------------

# 必要なパッケージの読み込み

library(tidyverse)
library(forecast)

# 予測モデルはARIMAで構築する。

# code 5-2 ----------------------------------------------------------------

# 必要なデータセットの読み込み

# library(tidyverse)

# 1.read_csv("chap5_1.csv")
#   "chap5_1.csv" をデータフレーム (tibble) として読み込む。
#   readr::read_csv() を使っており、data.frame ではなく tibble 形式になる。
# 2.mutate(yyyymm = as.Date(paste0(yyyymm, "-01"), format = "%Y-%m-%d"))
#   mutate() で yyyymm 列を変換。
#   paste0(yyyymm, "-01") で yyyymm 列に "-01" を追加 → "YYYY-MM" を "YYYY-MM-01" に変換。
#   as.Date(..., format = "%Y-%m-%d") で文字列を Date 型に変換。

df <- read_csv("chap5_1.csv") %>% 
  mutate(yyyymm = as.Date(paste0(yyyymm, "-01"), format = "%Y-%m-%d"))  # "YYYY-MM-01" 形式に変換

print(head(df))
print(tail(df))

# sales(販売台数）をグラフ化する
# code 5-3 ----------------------------------------------------------------

plot(df, type = "l")

# ggplot で時系列プロットを作成
ggplot(df, aes(x = yyyymm, y = sales)) +
  geom_line() +  # 折れ線グラフ
  geom_point() + # 各データ点を表示
  labs(title = "Monthly Sales Data", x = "Year-Month", y = "Sales") 


# 右肩上がりの増加傾向であることがわかる。
# また、季節性もありそう。
# 読み込んだデータを「モデル構築用データ」（2017年12月までの販売データ）と
# 「評価対象データ」（2018年1月以降の販売データ）に分割する。
# 「評価対象データ」は2018年1月から2018年6月までの直近6ヶ月間のデータである。
# code 5-4 ----------------------------------------------------------------

# 分割用のパラメータ
target_length <- 6  # 直近6ヶ月を評価対象データとする

# 訓練データと評価データに分割
# slice() は 行番号を指定してデータフレームの一部を抽出する dplyr の関数です。
# n() は dplyr パッケージで使える関数で、現在のデータフレームやティブル（tibble）
# の 行数 を返します。
df_train <- df %>% slice(1:(n() - target_length))  # 1:48 先頭〜(n-6)行を訓練データ
df_target <- df %>% slice((n() - target_length + 1):n())  # 49:54 直近6ヶ月を評価データ

# 確認
print(head(df_train))  # 訓練データの先頭
print(tail(df_target))  # 評価データの末尾

length(df_train$yyyymm)  
length(df_target$yyyymm)



# ステップ2：異常検知で利用するモデルの構築 ---------------------------------------------------

# 「モデル構築用データ」（2017年12月までの販売データ）を使い予測モデルを構築する。
# 季節性は「あり」（seasonal=True）にし、その季節性の周期（m）は12ヵ月にしている。

# cede 5-5 ----------------------------------------------------------------

library(forecast)

# 訓練データ（sales）の時系列オブジェクトを作成
ts_sales <- ts(df_train$sales, frequency = 12)  # 月次データで年周期（12ヶ月）

# auto_arima で最適なARIMAモデルを構築
arima_model <- auto.arima(ts_sales, seasonal = TRUE)

# モデルの概要を表示
summary(arima_model)

# モデルの診断
checkresiduals(arima_model)


# 予測モデルの構築は、これで終了。このモデルを使いモデル構築用データの販売台数を予測する。

# code 5-6 ----------------------------------------------------------------

# 訓練データの期間内で予測を実施
# モデルのフィッティング値（訓練データ内での予測値）
# 訓練データ内での予測はforecastではない
# RはbackcastingやKalmanフィルタで補完するので最初の12ヵ月が固定値にならない

train_pred <- fitted(arima_model) 
                                       

# 予測結果を表示
# train_pred には予測値、予測誤差、信頼区間などが含まれるので、その内容を確認できます。
print(train_pred)

# 予測と実測を折れ線グラフとしてプロットし、どのようになっているか視覚的に確認する

# code 5-7 ----------------------------------------------------------------

# 訓練データの予測結果
train_pred # 予測された値


# プロット（実測値と予測値をプロット）
ggplot(df_train, aes(x = yyyymm)) +
  geom_line(aes(y = sales, color = "Actual (Train dataset)"), na.rm = TRUE) +  # 実測値　
  　　　　　　　　　　　　　　　　　　　　　　# na.rm = TRUE は NA のデータがあってもプロットを進める設定。
  geom_line(aes(y = train_pred, color = "Predicted"), linetype = "dotted", linewidth = 1, na.rm = TRUE) +  # 予測値
    scale_color_manual(values = c("Actual (Train dataset)" = "red", "Predicted" = "blue")) +
  labs(title = "Actual vs Predicted (Train Dataset)",
       x = "Date", y = "Sales") +
  #theme_minimal()  +  # theme_minimal() を適用し、シンプルなデザインに変更。
  theme(legend.title = element_blank())  # 凡例のタイトル(colour)を削除し、シンプルな見た目にする。


# # 予測値から次の順番で外れ値スコアまで計算していく
# ① 残差（=実測値-予測値）
# ② 標準化残差
# ③ 外れ値スコア（標準化残差の絶対値）

# 計算して、res_dfに実測値、予測値の次に格納する。

# code 5-8、5-9、5-10 ----------------------------------------------------------------

# scale(res) を実行すると、結果が行列 (matrix) として返されるため、
# 列名に ["Series 1"] という情報が付加されるので、scale() の出力を 
# as.numeric() で明示的にベクトルに変換する。

res_df <- df_train %>%
  mutate(pred = train_pred, 
         residual = sales - pred, 
         std_res = as.numeric(scale(residual)),
         abs_std_res = abs(std_res))

# 確認
print(res_df)



# ステップ3：異常値検知（評価対象データの期間） -------------------------------------------------

# ステップ2で構築した予測モデルを使い、評価対象データの期間の販売台数を予測する。

# code 5-11 ---------------------------------------------------------------

target_pred <- forecast(arima_model, h = target_length)
target_pred

# 予測値と実績値をグラフ化してどのようになっているか見る。

# code 5-12 ---------------------------------------------------------------

# ggplot2 を使って予測値と実測値をプロット
# target_pred の予測値をデータフレームdf_targetに追加

df_target$pred <- target_pred$mean

# ggplot にデータを渡す
# scale_x_date() を追加し、x 軸を適切にフォーマット
# date_labels = "%Y-%m" を使い、年月の表示を調整
ggplot() +
  geom_line(data = df_target, aes(x = yyyymm, y = sales, color = "Actual")) +  # 実測値
  geom_line(data = df_target, aes(x = yyyymm, y = pred, color = "Predicted"), 
            linetype = "dotted", linewidth = 1) +  # 予測値
  scale_color_manual(values = c("Actual" = "red", "Predicted" = "blue")) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +  # X軸のラベル調整
  labs(title = "Actual vs Predicted (Evaluation Period)", x = "Date", y = "Sales") +
  #theme_minimal()
theme(legend.title = element_blank())  # 凡例のタイトル(colour)を削除し、シンプルな見た目にする。


# 実測値が予測値よりも下回っている。この時点で2018年1月からのキャンペーンがよくないことが想像できる。
# 予測値から残差、標準化残差、外れ値スコアを計算していく。
# まず残差を計算する。
# code 5-13、code 5-14、code 5-15 ---------------------------------------------------------------

# 残差（実測値 - 予測値）
#res <- df_target$sales - target_pred$mean
#res

target_res_df <- df_target %>% 
  mutate(residual = sales - pred,
         std_res = as.numeric(scale(residual)),
         abs_std_res = abs(std_res))
target_res_df



