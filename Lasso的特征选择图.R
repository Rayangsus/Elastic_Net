library(readxl)
library(ggplot2)

# 从指定路径和工作表读取数据
data <- read_excel("D:/R/Qatar/QataeWC_AFC4.xlsx", sheet = 6)

# 假设x轴是第一列，y轴是第二列
x <- data[[2]] # 或者使用具体的列名，例如 data$columnName
y <- data[[1]] # 同上

# 将y列转换为因子，水平即为其在数据中出现的顺序
data$y <- factor(data[[1]], levels = unique(data[[1]]))

# 创建颜色映射变量，基于x值是0还是非零
data$color <- ifelse(data[[2]] == 0, "粉色", "天蓝色")

# 使用更新后的数据框来绘图，加入颜色映射
  ggplot(data, aes(x = x, y = y, color = color)) +
  geom_point(size = 3) + # 散点图，这里设置了点的大小
  geom_point() + # 散点图
  scale_color_manual(values = c("粉色" = "#FA8072", "天蓝色" = "#01bdcd")) + # 手动设置颜色
  theme_minimal() + # 简洁主题
  labs(x = "Value", y = "Coefficient")+ 
  theme(legend.position = "none")

