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
cv_fit <- cv.glmnet(X_train, y_train, alpha = 1, family = "binomial")

# 提取最佳lambda值
best_lambda <- cv_fit$lambda.min
print(best_lambda) # 打印以确认

# 绘制LASSO系数路径图，并设置x轴名称
plot(cv_fit, xlab = "Log lambda")

# 为最佳lambda值添加一条垂直虚线
abline(v = log(best_lambda), col = "red", lwd = 2, lty = 2)

# 选择一个固定的y值，以确保文本显示在图形的顶部内部
# 注意：这里的y值需要你根据图形的实际显示进行调整
y_pos_fixed <- max(cv_fit$cvm) # 使用交叉验证误差的最大值作为参考

# 首先确定x和y的新位置
new_x_position <- log(best_lambda) # 例如，确切地在best_lambda的对数值上
new_y_position <- y_pos_fixed + 0.5 # 假设原来的位置向上调整了0.1单位

# 设置 x 轴的名称
xlabel <- "Log(lambda)"
# 设置 y 轴的名称
ylabel <- "Cross-validation Error"

# 使用这些新坐标在图形内部的顶部中间显示"Best lambda"的值
text(x = new_x_position, y = new_y_position, labels = paste("Best lambda =", round(best_lambda, 3)), col = "red", cex = 1)



