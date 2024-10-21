# 加载必要的库
library(readxl)
library(car)  # 用于Levene's test
library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)

# 指定Excel文件路径
excel_file_path <- "D:/R/violinForR/Violin.xlsx"

# 导入Excel文件中的指定Sheet
df <- read_excel(excel_file_path, sheet = 5)
df <- as.data.frame(df)

# 将Match_Type列转换为因子类型
df$Match_Type <- as.factor(df$Match_Type)

# 获取数据集中的数据列（从第6列开始）
data_columns <- df[, 6:ncol(df)]

# 初始化一个存储Levene's test结果的data frame
levene_results <- data.frame(Variable = character(), P_Value_Levene = numeric(), stringsAsFactors = FALSE)

# 对于每个数据列进行Levene's test
for (col in colnames(data_columns)) {
  # 为了避免列名中的特殊字符导致问题，使用反引号
  formula_text <- paste("`", col, "` ~ Match_Type", sep="")
  formula <- as.formula(formula_text)
  
  # 执行Levene's test
  levene_test_result <- leveneTest(formula, data = df)
  
  # 将变量名和对应的p值存储到结果data frame中
  levene_results <- rbind(levene_results, data.frame(Variable = col, P_Value_Levene = levene_test_result$`Pr(>F)`))
}

# 输出Levene's test的结果
levene_results

# 创建一个空的列表来存储所有的小提琴图
all_violin_plots <- list()

# 自定义 Match_Type 的顺序
custom_order <- c("NCS", "CS")

# 初始化一个存储t-test结果的data frame
t_test_results <- data.frame(Variable = character(), P_Value_TTest = numeric(), stringsAsFactors = FALSE)

# 对于每列数据进行处理
for (col in colnames(data_columns)) {
  # 判断方差是否齐性，如果满足方差齐性，则按照假定等方差的方式计算t检验
  if (levene_results$P_Value_Levene[levene_results$Variable == col] >= 0.05) {
    t_test_result <- t.test(df[df$Match_Type == "NCS", col], df[df$Match_Type == "CS", col],var.equal = TRUE)
  } else {
    # 如果方差不齐性，使用Welch's t-test
    t_test_result <- t.test(df[df$Match_Type == "NCS", col], df[df$Match_Type == "CS", col], var.equal = FALSE)
  }
  
  # 提取P值并将其格式化为三位小数
  p_value <- sprintf("%.3f", t_test_result$p.value)
  
  # 将计算的P值添加到 t_test_results data frame 中
  t_test_results <- rbind(t_test_results, data.frame(Variable = col, P_Value_TTest = as.numeric(p_value)))
  
  # 创建小提琴图，设置 x 轴顺序
  violin_plot <- ggplot(df, aes(x = factor(Match_Type, levels = custom_order), y = !!sym(col), fill = Match_Type)) +
    geom_violin() +
    ggtitle(col) +  # 将Y轴标签设置为图的标题
    labs(x = "", y = "", fill = "分组") +
    scale_fill_manual(values = c("NCS" = "#11bdcd", "CS" = "#ef1878")) +  # 更浅的红色和蓝色
    theme(legend.position = "") +
    stat_summary(fun.data = mean_sdl, 
                 geom = "errorbar", 
                 width = 0.2,
                 fun.args = list(mult = 1),
                 color = "white") +  # 将平均值和标准差的颜色设为白色
    stat_summary(fun.data = mean_sdl, 
                 geom = "point", 
                 shape = 20, 
                 size = 3, 
                 fun.args = list(mult = 1),
                 color = "white")  # 将平均值和标准差的颜色设为白色
  
  # 控制y轴可见范围，并在顶部添加一些额外的空白
  violin_plot <- violin_plot +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))  
  
  # 添加P值文本到图片的顶部下方（仅在P值小于0.05时标注）
  if (t_test_result$p.value < 0.05) {
    violin_plot <- violin_plot +
      annotate("text", x = 1.5, y = max(df[[col]]) * 1.1, label = paste("p =", p_value), size = 3)
  }
  
  # 设置图的标题大小
  violin_plot <- violin_plot +
    theme(plot.title = element_text(size = 12)) 
  
  # 将小提琴图添加到 all_violin_plots 列表
  all_violin_plots[[col]] <- violin_plot
}

# 输出 t-test 的结果
t_test_results

# 打印所有P值
cat("All P-values:", t_test_results$P_Value_TTest, "\n")

# 绘制多个小提琴图，以5列6行的格式展示
grid.arrange(grobs = all_violin_plots, ncol = 5)


