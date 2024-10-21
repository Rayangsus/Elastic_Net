# 加载必要
library(glmnet)
library(readxl)
library(caret) # 用于数据预处理和分割

# 1. 数据导入
data <- read_excel("D:/R/Qatar/QataeWC_AFC4.xlsx", sheet = 3)

# 2. 数据清洗
# 处理缺失值：这里简单示例删除含有NA的行
data <- na.omit(data)

# 4. 数据变换
# 标准化自变量
X <- as.matrix(data[, 7:ncol(data)])
y <- data[["Outcome"]]
X_scaled <- scale(X) # 标准化特征

# 6. 数据集分割
set.seed(123) # 确保可重现性
trainIndex <- createDataPartition(y, p = .7, 
                                  list = FALSE, 
                                  times = 1)
X_train <- X_scaled[trainIndex, ]
y_train <- y[trainIndex]
X_test <- X_scaled[-trainIndex, ]
y_test <- y[-trainIndex]

# 7. 模型训练与交叉验证
cv_fit <- cv.glmnet(X_train, y_train, alpha = 1, family = "binomial")#通过调节Alpha值（0-1）来控制模型中关键变量地数量，例如从纯岭回归0，到纯Lasso回归1


# 查看最佳lambda值和相应的系数
best_lambda <- cv_fit$lambda.min
print(best_lambda)
print(coef(cv_fit, s = best_lambda))

# 8. 使用模型进行预测
predictions <- predict(cv_fit, newx = X_test, s = best_lambda, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# 评估模型性能
confusionMatrix(factor(predicted_classes), factor(y_test))

