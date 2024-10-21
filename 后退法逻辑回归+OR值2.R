#加入整合数据集的功能，方便导出

library(readxl)
library(dplyr)
library(broom)

# 读取Excel文件的第4个工作表
data <- read_excel("D:/R/Qatar/QataeWC_AFC4.xlsx", sheet = 4)

# 筛选数据集：只保留第一列为"AFC"的行
data <- data[data[[1]] == "AFC", ]

# 选择自变量和因变量
X <- data[, 7:ncol(data)] # 选择第7列到最后一列作为自变量
Y <- data$Outcome # 因变量

# 创建逻辑回归模型
model <- glm(Y ~ ., data = cbind(Y, X), family = binomial)

# 应用后退法进行变量选择
model_final <- step(model, direction = "backward")

# 使用broom包来整理模型的统计摘要
tidy_model <- broom::tidy(model_final) %>% 
  mutate(term = as.character(term))  # 确保term是字符类型，方便后面的合并

# 计算OR及其95%置信区间
exp_coef <- exp(coef(model_final))
conf_int <- confint(model_final)
exp_confint <- exp(conf_int)

# 创建包含OR及其置信区间的数据框
# 注意coef()和confint()得到的结果默认与模型的term顺序相同
or_df <- data.frame(
  term = rownames(exp_confint),
  OR = exp_coef,
  `Lower 95% CI` = exp_confint[, 1],
  `Upper 95% CI` = exp_confint[, 2]
)

# 按照变量名合并tidy_model和or_df，以确保顺序一致
final_data <- left_join(tidy_model, or_df, by = "term")

# 查看整合后的数据
print(final_data)

