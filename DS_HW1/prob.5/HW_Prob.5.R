# install.packages("arules")
library(arules)
# 1. 讀取資料 (注意分隔符號為 ';') [cite: 55]
bank_data <- read.csv("bank.csv", sep = ";", stringsAsFactors = TRUE)

# 2. 移除指定欄位: job, contact, pdays, poutcome, month, day [cite: 69]
cols_to_remove <- c("job", "contact", "pdays", "poutcome", "month", "day")
bank_clean <- bank_data[, !(names(bank_data) %in% cols_to_remove)]

# 3. Discretize 'age' (年齡離散化) 
# young < 35, adult 35~55, old > 55
bank_clean$age <- cut(bank_clean$age, 
                      breaks = c(-Inf, 34, 55, Inf), 
                      labels = c("young", "adult", "old"))

# 4. 對剩餘的數值欄位使用 "中位數" 進行二分化 (low / high) 
# 找出目前的數值欄位 (balance, duration, campaign, previous)
num_cols <- sapply(bank_clean, is.numeric)
for (col in names(bank_clean)[num_cols]) {
  med_val <- median(bank_clean[[col]], na.rm = TRUE)
  bank_clean[[col]] <- ifelse(bank_clean[[col]] > med_val, "high", "low")
  bank_clean[[col]] <- as.factor(bank_clean[[col]])
}

# 將所有欄位轉換為交易型態所需的 factor (如果還不是的話)
bank_clean <- as.data.frame(lapply(bank_clean, as.factor))

# ---------------------------------------------------------
# 5. (a) 識別顯著規則 (Significant Rules) [cite: 72]
# 設定 RHS 分別為 "y=yes" 與 "y=no"
# ---------------------------------------------------------

# 挖掘與 y=yes 相關的規則
rules_yes <- apriori(bank_clean, 
                     parameter = list(supp = 0.01, conf = 0.2, minlen = 2),
                     appearance = list(rhs = "y=yes", default = "lhs"))

# 挖掘與 y=no 相關的規則
rules_no <- apriori(bank_clean, 
                    parameter = list(supp = 0.01, conf = 0.5, minlen = 2),
                    appearance = list(rhs = "y=no", default = "lhs"))

rules_yes <- rules_yes[!is.redundant(rules_yes)]
rules_no <- rules_no[!is.redundant(rules_no)]

# 依照 lift (增益值) 排序，找出最顯著的規則
inspect(head(sort(rules_yes, by = "lift"), 10))
inspect(head(sort(rules_no, by = "lift"), 10))

# ---------------------------------------------------------
# 6. (b) 統計 LHS 特徵頻率並排序 [cite: 72]
# ---------------------------------------------------------

get_lhs_freq <- function(rules_set) {
  # 提取 LHS 的所有項目
  lhs_items <- labels(lhs(rules_set))
  # 移除括號並分割項目
  lhs_items <- gsub("\\{", "", lhs_items)
  lhs_items <- gsub("\\}", "", lhs_items)
  all_features <- unlist(strsplit(lhs_items, ","))
  
  # 計算頻率
  freq_table <- table(all_features)
  return(sort(freq_table, decreasing = TRUE))
}

cat("\n--- 與 y=yes 相關的 LHS 特徵頻率排序 ---\n")
print(get_lhs_freq(rules_yes))

cat("\n--- 與 y=no 相關的 LHS 特徵頻率排序 ---\n")
print(get_lhs_freq(rules_no))
