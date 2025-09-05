# 基础统计学与R语言：美丽数据可视化指南

# =========================================================
# 作者：Philo
# 日期：2025-2-15
# 版本：1.0
# =========================================================

# 本教程将介绍基础统计学概念，并通过R语言实现优雅的数据可视化
# 我们会使用多种R包创建专业级图表，同时提供清晰的数学解释

# 首先，安装必要的R包（如未安装）
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyr)) install.packages("tidyr")
if (!require(patchwork)) install.packages("patchwork")
if (!require(ggthemes)) install.packages("ggthemes")
if (!require(plotly)) install.packages("plotly")
if (!require(htmlwidgets)) install.packages("htmlwidgets")

# 加载所需的包
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(ggthemes)
library(plotly)
library(htmlwidgets)

# 设置中文字体支持（根据系统调整）
if (.Platform$OS.type == "windows") {
  windowsFonts("SimHei" = windowsFont("SimHei"))
  theme_set(theme_gray(base_family = "SimHei"))
} else {
  # Mac或Linux系统可能需要不同的字体设置
  theme_set(theme_gray())
}

# 设置全局主题，使图表更加美观
theme_custom <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "#555555"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      panel.grid = element_line(color = "#eeeeee"),
      plot.background = element_rect(fill = "#ffffff", color = NA)
    )
}

# =========================================================
# 第一部分：描述性统计
# =========================================================

cat("\n===== 第一部分：描述性统计 =====\n")

# 生成示例数据
set.seed(123)  # 设置随机数种子，保证结果可重现

# 1. 正态分布数据
normal_data <- data.frame(
  value = rnorm(1000, mean = 50, sd = 10),
  distribution = "正态分布"
)

# 2. 均匀分布数据
uniform_data <- data.frame(
  value = runif(1000, min = 20, max = 80),
  distribution = "均匀分布"
)

# 3. 指数分布数据
exponential_data <- data.frame(
  value = rexp(1000, rate = 0.05),
  distribution = "指数分布"
)

# 合并数据
distribution_data <- rbind(normal_data, uniform_data, exponential_data)

# 计算基本统计量
stats_summary <- distribution_data %>% 
  group_by(distribution) %>% 
  summarise(
    样本量 = n(),
    均值 = mean(value),
    中位数 = median(value),
    标准差 = sd(value),
    最小值 = min(value),
    最大值 = max(value),
    Q1 = quantile(value, 0.25),
    Q3 = quantile(value, 0.75)
  )

print(stats_summary)

# 创建分布对比图表 - 使用密度图
p1 <- ggplot(distribution_data, aes(x = value, fill = distribution)) +
  geom_density(alpha = 0.7, position = "identity") +
  facet_wrap(~distribution, ncol = 1, scales = "free_y") +
  labs(
    title = "不同概率分布的对比",
    subtitle = "三种常见分布的密度估计图",
    x = "数值",
    y = "密度"
  ) +
  scale_fill_manual(values = c("#FF6B6B", "#4ECDC4", "#45B7D1")) +
  theme_custom() +
  theme(legend.position = "none")

print(p1)

# 保存图表到output目录
if (!dir.exists("output")) dir.create("output")
ggsave("output/distribution_comparison.png", plot = p1, width = 10, height = 6, dpi = 300)

# =========================================================
# 第二部分：数据可视化的艺术
# =========================================================

cat("\n===== 第二部分：数据可视化的艺术 =====\n")

# 创建更复杂的数据集 - 模拟学生考试成绩
exam_data <- data.frame(
  学生ID = 1:200,
  数学 = round(rnorm(200, 70, 15)),
  语文 = round(rnorm(200, 75, 12)),
  英语 = round(rnorm(200, 68, 18)),
  理综 = round(rnorm(200, 72, 14)),
  文综 = round(rnorm(200, 74, 13)),
  性别 = sample(c("男", "女"), 200, replace = TRUE),
  班级 = sample(c("一班", "二班", "三班", "四班"), 200, replace = TRUE)
)

# 将长格式数据转为宽格式用于某些可视化
exam_long <- exam_data %>% 
  pivot_longer(cols = c(数学, 语文, 英语, 理综, 文综), 
               names_to = "科目", 
               values_to = "分数")

# 保存数据到input目录，以便用户可以加载和使用
if (!dir.exists("input")) dir.create("input")
write.csv(exam_data, "input/exam_scores.csv", row.names = FALSE)

# 1. 散点图 - 数学 vs 英语成绩，按性别着色
p2 <- ggplot(exam_data, aes(x = 数学, y = 英语, color = 性别)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "数学与英语成绩的相关性",
    subtitle = "不同性别的成绩分布与趋势线",
    x = "数学成绩",
    y = "英语成绩"
  ) +
  scale_color_manual(values = c("#45B7D1", "#FF6B6B")) +
  theme_custom()

print(p2)

# 计算相关性系数
correlation <- cor(exam_data$数学, exam_data$英语)
cat(sprintf("\n数学与英语成绩的相关系数: %.3f\n", correlation))

# 2. 箱线图 - 各科目成绩分布
p3 <- ggplot(exam_long, aes(x = 科目, y = 分数, fill = 科目)) +
  geom_boxplot(alpha = 0.7, notch = TRUE) +
  labs(
    title = "各科目考试成绩分布",
    subtitle = "中位数、四分位数与异常值展示",
    x = "科目",
    y = "分数"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_custom() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p3)

# 3. 小提琴图 - 按班级分组的数学成绩分布
p4 <- ggplot(exam_data, aes(x = 班级, y = 数学, fill = 班级)) +
  geom_violin(alpha = 0.7, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter(alpha = 0.3, size = 1) +
  labs(
    title = "各班数学成绩分布",
    subtitle = "小提琴图显示分布形状与四分位数",
    x = "班级",
    y = "数学成绩"
  ) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_custom()

print(p4)

# 4. 组合图表 - 使用patchwork包
combined_plot <- p2 + p3 + plot_layout(ncol = 2)
print(combined_plot)

ggsave("output/exam_analysis_plots.png", plot = combined_plot, width = 16, height = 8, dpi = 300)

# =========================================================
# 第三部分：统计推断基础
# =========================================================

cat("\n===== 第三部分：统计推断基础 =====\n")

# 1. 单样本t检验 - 检验数学成绩是否等于70分
math_ttest <- t.test(exam_data$数学, mu = 70)
print("单样本t检验结果（数学成绩是否等于70分）:")
print(math_ttest)

# 2. 独立样本t检验 - 比较男女生数学成绩差异
gender_ttest <- t.test(数学 ~ 性别, data = exam_data)
print("\n独立样本t检验结果（男女生数学成绩差异）:")
print(gender_ttest)

# 3. 方差分析 - 比较不同班级的数学成绩
anova_result <- aov(数学 ~ 班级, data = exam_data)
print("\n方差分析结果（不同班级数学成绩差异）:")
print(summary(anova_result))

# 创建显著性标记的条形图
class_means <- exam_data %>% 
  group_by(班级) %>% 
  summarise(平均成绩 = mean(数学), 标准误 = sd(数学)/sqrt(n()))

p5 <- ggplot(class_means, aes(x = 班级, y = 平均成绩, fill = 班级)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_errorbar(aes(ymin = 平均成绩 - 标准误*1.96, ymax = 平均成绩 + 标准误*1.96), 
                width = 0.2) +
  labs(
    title = "各班数学平均成绩与95%置信区间",
    subtitle = "误差线表示95%置信区间",
    x = "班级",
    y = "平均数学成绩"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_custom()

print(p5)

ggsave("output/class_comparison.png", plot = p5, width = 10, height = 6, dpi = 300)

# =========================================================
# 第四部分：交互式数据可视化
# =========================================================

cat("\n===== 第四部分：交互式数据可视化 =====\n")

# 使用plotly创建交互式散点图
interactive_plot <- plot_ly(
  data = exam_data,
  x = ~数学,
  y = ~英语,
  color = ~性别,
  symbol = ~班级,
  type = "scatter",
  mode = "markers",
  marker = list(size = 10, opacity = 0.7),
  text = ~paste("学生ID:", 学生ID, "\n",
                "数学:", 数学, "\n",
                "英语:", 英语, "\n",
                "班级:", 班级)
)

# 设置布局
interactive_plot <- interactive_plot %>% 
  layout(
    title = "交互式成绩分析图",
    xaxis = list(title = "数学成绩"),
    yaxis = list(title = "英语成绩"),
    legend = list(title = list(text = "性别与班级"))
  )

# 显示交互式图表
print("正在创建交互式图表...")
print(interactive_plot)

# 保存交互式图表为HTML文件
htmlwidgets::saveWidget(interactive_plot, "output/interactive_score_analysis.html")

# =========================================================
# 第五部分：高级数据可视化技巧
# =========================================================

cat("\n===== 第五部分：高级数据可视化技巧 =====\n")

# 1. 热图 - 科目间相关性
# 计算相关矩阵
cor_matrix <- cor(exam_data[, c("数学", "语文", "英语", "理综", "文综")])

# 转换为长格式
cor_long <- as.data.frame(cor_matrix) %>% 
  mutate(科目1 = rownames(.)) %>% 
  pivot_longer(cols = c(数学, 语文, 英语, 理综, 文综), 
               names_to = "科目2", 
               values_to = "相关系数")

p6 <- ggplot(cor_long, aes(x = 科目1, y = 科目2, fill = 相关系数)) +
  geom_tile() +
  geom_text(aes(label = round(相关系数, 2)), color = "white", size = 5) +
  labs(
    title = "各科目成绩间的相关性热图",
    subtitle = "相关系数越大表示关系越密切",
    x = "科目1",
    y = "科目2"
  ) +
  scale_fill_gradient2(low = "#2c7bb6", mid = "#ffffbf", high = "#d7191c",
                       midpoint = 0, limit = c(-1, 1)) +
  theme_custom() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p6)

ggsave("output/subject_correlation_heatmap.png", plot = p6, width = 10, height = 8, dpi = 300)

# 2. 雷达图 - 学生综合能力展示
# 计算前10名学生的各科成绩标准化分数
student_top10 <- exam_data %>% 
  mutate(总分 = 数学 + 语文 + 英语 + 理综 + 文综) %>% 
  arrange(desc(总分)) %>% 
  head(10) %>% 
  select(学生ID, 数学, 语文, 英语, 理综, 文综)

# 转换为适合雷达图的格式
student_radar <- student_top10 %>% 
  pivot_longer(cols = c(数学, 语文, 英语, 理综, 文综),
               names_to = "科目",
               values_to = "分数")

# 创建雷达图
p7 <- ggplot(student_radar, aes(x = 科目, y = 分数, group = factor(学生ID), color = factor(学生ID))) +
  geom_polygon(fill = NA, linewidth = 1.5, alpha = 0.3) +
  geom_line(linewidth = 1) +
  coord_polar() +
  labs(
    title = "优秀学生各科成绩雷达图",
    subtitle = "展示前10名学生的各科成绩分布",
    color = "学生ID"
  ) +
  scale_color_viridis_d() +
  theme_custom() +
  theme(legend.position = "right")

print(p7)

ggsave("output/top_students_radar.png", plot = p7, width = 12, height = 10, dpi = 300)

cat("\n===== 教程完成！所有图表已保存到output目录 =====\n")
cat("\n数据文件已保存到input目录\n")
cat("\n感谢使用本基础统计学与R语言可视化指南！\n")