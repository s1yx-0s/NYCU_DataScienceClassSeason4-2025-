# --- 1. 環境設定與套件載入 ---
# 若未安裝請先執行: install.packages(c("tidyquant", "PortfolioAnalytics", "ROI", "ROI.plugin.quadprog", "ROI.plugin.glpk"))
library(tidyquant)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(tidyverse)

# --- 2. 定義股票清單 (共 15 支) ---
# 包含你挑選的 4 支，以及補足的 4 支景氣型與 7 支防禦型
tickers <- c(
  "2330.TW", "2308.TW", "2317.TW", "2454.TW", "2382.TW", "3017.TW", # 景氣型 (6支)
  "2881.TW", "2412.TW", "2891.TW", "2887.TW", "5880.TW", "5876.TW", "1102.TW", "2207.TW", "1476.TW" # 防禦型 (9支)
)

# --- 3. 下載股價資料 (自 2016 年起) ---
stock_prices <- tq_get(tickers, from = "2016-01-01", get = "stock.prices")

# 計算月報酬率並轉換為寬表格 (優化矩陣需要格式)
stock_returns <- stock_prices %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "monthly", col_rename = "ret") %>%
  pivot_wider(names_from = symbol, values_from = ret) %>%
  column_to_rownames(var = "date") %>%
  as.xts()

# 移除含有 NA 的資料列 (處理部分股票較晚掛牌的情況)
stock_returns <- na.omit(stock_returns)

# --- 4. 建立投資組合規格 (Portfolio Specification) ---
pspec <- portfolio.spec(assets = colnames(stock_returns))

# 加入限制條件：權重總和為 1 (Full Investment) 且禁止放空 (Long Only)
pspec <- add.constraint(portfolio = pspec, type = "full_investment")
pspec <- add.constraint(portfolio = pspec, type = "long_only")

# --- 5. 定義並執行三種模型優化 ---

# 模型 A: 最大化報酬 (Max Return)
opt_max_ret <- add.objective(portfolio = pspec, type = "return", name = "mean") %>%
  optimize.portfolio(R = stock_returns, optimize_method = "ROI")

# 模型 B: 最小化風險 (Min Risk - Variance)
opt_min_risk <- add.objective(portfolio = pspec, type = "risk", name = "var") %>%
  optimize.portfolio(R = stock_returns, optimize_method = "ROI")

# 模型 C: 最大化報酬與風險之差 (Max Return - Risk)
# --- 修正後的模型 C: 最大化報酬與風險之差 (Quadratic Utility) ---
# 目標：Max(Mean - lambda * Var)
pspec_max_diff <- pspec
pspec_max_diff <- add.objective(portfolio = pspec_max_diff, type = "quadratic_utility", risk_aversion = 2) # lambda 設為 2 比較能看到平衡效果

opt_max_diff <- optimize.portfolio(R = stock_returns, portfolio = pspec_max_diff, optimize_method = "ROI")

# 重新執行後續的 weights_comparison 即可看到數值！

# --- 6. 績效比較與結果輸出 ---
weights_comparison <- data.frame(
  Max_Return = extractWeights(opt_max_ret),
  Min_Risk = extractWeights(opt_min_risk),
  Max_Diff = extractWeights(opt_max_diff)
)

print("--- 各模型權重分配比較 ---")
print(weights_comparison)

# 計算各組合的歷史績效
portfolio_rets <- cbind(
  Return_MaxRet = Return.portfolio(stock_returns, weights = extractWeights(opt_max_ret)),
  Return_MinRisk = Return.portfolio(stock_returns, weights = extractWeights(opt_min_risk)),
  Return_MaxDiff = Return.portfolio(stock_returns, weights = extractWeights(opt_max_diff))
)

print("--- 累積績效指標 ---")
table.AnnualizedReturns(portfolio_rets)