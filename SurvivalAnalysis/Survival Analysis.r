# 生存分析教程：从数学原理到实践应用
# 作者：Wenxing Yi
# 日期：2025-4-2
# 版本：1.0

# ============================================================
# 第一部分：生存分析的数学基础
# ============================================================

# 1.1 生存分析的基本概念
# 生存分析是一种统计方法，用于分析事件发生的时间分布，特别适用于研究
# 从开始时间到感兴趣事件（如死亡、疾病复发、设备故障等）发生的时间间隔

# 1.2 基本数学定义
# - 生存函数S(t)：表示个体在时间t仍然存活的概率
# - 风险函数h(t)：表示在时间t已经存活的个体在接下来的极短时间内发生事件的概率
# - 累积风险函数H(t)：风险函数从0到t的积分
# - 概率密度函数f(t)：事件发生在时间t的概率密度

# S(t) = P(T > t) = 1 - F(t)
# h(t) = f(t)/S(t)
# H(t) = ∫₀^t h(u) du
# S(t) = exp(-H(t))

# 1.3 常用分布模型
# - 指数分布：假设风险恒定
h_exp <- function(t, lambda) lambda
S_exp <- function(t, lambda) exp(-lambda * t)

# - Weibull分布：允许风险随时间增加或减少
h_weibull <- function(t, lambda, k) k * lambda * (lambda * t)^(k-1)
S_weibull <- function(t, lambda, k) exp(-(lambda * t)^k)

# ============================================================
# 第二部分：R包安装与加载
# ============================================================

# 安装必要的R包
if (!require(survival)) install.packages("survival")  # 核心生存分析包
if (!require(survminer)) install.packages("survminer")  # 生存分析可视化
if (!require(ggplot2)) install.packages("ggplot2")    # 数据可视化
if (!require(dplyr)) install.packages("dplyr")        # 数据处理
if (!require(tidyr)) install.packages("tidyr")        # 数据整理

# 加载R包
suppressPackageStartupMessages(library(survival))
suppressPackageStartupMessages(library(survminer))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))

# 设置中文字体支持
if (.Platform$OS.type == "windows") {
  windowsFonts("SimHei" = windowsFont("SimHei"))
  theme_set(theme_grey(base_family = "SimHei"))
} else if (.Platform$OS.type == "unix") {
  theme_set(theme_grey(base_family = "WenQuanYi Micro Hei"))
}

# 创建自定义主题
theme_survival <- function() {
  theme_bw() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      strip.text = element_text(size = 12, face = "bold")
    )
}

# ============================================================
# 第三部分：数据准备
# ============================================================

# 使用R自带的生存分析数据集
# 数据集1：lung（肺癌患者生存数据）
print("数据集1：lung 预览")
head(lung)

# 数据集2：ovarian（卵巢癌患者生存数据）
print("数据集2：ovarian 预览")
head(ovarian)

# 数据集3：aml（急性髓细胞白血病患者生存数据）
print("数据集3：aml 预览")
head(aml)

# 数据预处理：将性别变量转换为因子
lung$sex <- factor(lung$sex, levels = c(1, 2), labels = c("男性", "女性"))

# ============================================================
# 第四部分：Kaplan-Meier生存曲线分析
# ============================================================

# 4.1 基础KM分析
# 创建生存对象
surv_object <- Surv(time = lung$time, event = lung$status)
print("生存对象前6行")
head(surv_object)

# 进行KM估计
km_fit <- survfit(surv_object ~ 1, data = lung)
print("KM估计结果摘要")
summary(km_fit)

# 4.2 分组KM分析（按性别）
km_fit_sex <- survfit(surv_object ~ sex, data = lung)
print("按性别分组的KM估计结果摘要")
summary(km_fit_sex)

# 4.3 绘制基础KM曲线
p1 <- ggsurvplot(
  km_fit, 
  data = lung, 
  risk.table = TRUE,         # 添加风险人数表
  pval = FALSE,              # 单组数据不需要p值检验
  conf.int = TRUE,           # 添加置信区间
  xlab = "生存时间（天）",    # x轴标签
  ylab = "生存概率",          # y轴标签
  title = "肺癌患者总体Kaplan-Meier生存曲线", # 标题
  legend.title = "分组",      # 图例标题
  legend.labs = c("总体"),    # 图例标签
  palette = "blue",          # 颜色
  ggtheme = theme_survival()  # 自定义主题
)
print("基础KM曲线图")
p1

# 4.4 绘制分组KM曲线
p2 <- ggsurvplot(
  km_fit_sex, 
  data = lung, 
  risk.table = TRUE,         # 添加风险人数表
  pval = TRUE,               # 添加Log-rank检验p值
  conf.int = TRUE,           # 添加置信区间
  xlab = "生存时间（天）",    # x轴标签
  ylab = "生存概率",          # y轴标签
  title = "不同性别肺癌患者的Kaplan-Meier生存曲线比较", # 标题
  legend.title = "性别",      # 图例标题
  legend.labs = c("男性", "女性"), # 图例标签
  palette = c("#E7B800", "#2E9FDF"), # 颜色
  ggtheme = theme_survival()  # 自定义主题
)
print("分组KM曲线图")
p2

# 4.5 精美的KM曲线定制
p3 <- ggsurvplot(
  km_fit_sex,
  data = lung,
  risk.table = TRUE,           # 添加风险人数表
  risk.table.col = "strata",   # 按分层着色
  pval = TRUE,                 # 添加Log-rank检验p值
  conf.int = TRUE,             # 添加置信区间
  xlab = "生存时间（天）",      # x轴标签
  ylab = "生存概率",            # y轴标签
  title = "不同性别肺癌患者的生存分析", # 标题
  subtitle = "基于Kaplan-Meier方法的生存曲线估计", # 副标题
  legend.title = "性别",        # 图例标题
  legend.labs = c("男性", "女性"), # 图例标签
  palette = c("#0073C2FF", "#EFC000FF"), # 颜色
  surv.median.line = "hv",     # 添加中位生存时间线
  break.time.by = 100,         # x轴刻度间隔
  ggtheme = theme_survival(),  # 自定义主题
  tables.theme = theme_survival() # 风险表主题
)
print("精美的KM曲线图")
p3

# 4.6 多组比较（使用aml数据集）
aml$group <- factor(aml$group, levels = c("Maintained", "Nonmaintained"), 
                    labels = c("维持治疗", "非维持治疗"))
surv_object_aml <- Surv(time = aml$time, event = aml$status)
km_fit_aml <- survfit(surv_object_aml ~ group, data = aml)

p4 <- ggsurvplot(
  km_fit_aml,
  data = aml,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = TRUE,
  xlab = "生存时间（周）",
  ylab = "生存概率",
  title = "AML患者不同治疗方案的生存曲线比较",
  legend.title = "治疗方案",
  legend.labs = c("维持治疗", "非维持治疗"),
  palette = c("#7E6148FF", "#4DBBD5FF"),
  ggtheme = theme_survival()
)
print("AML患者治疗方案比较KM曲线图")
p4

# ============================================================
# 第五部分：Cox比例风险模型分析
# ============================================================

# 5.1 基础Cox回归
# 创建Cox模型（仅包含性别变量）
cox1 <- coxph(Surv(time, status) ~ sex, data = lung)
print("基础Cox回归模型摘要")
summary(cox1)

# 5.2 多变量Cox回归
# 创建包含多个协变量的Cox模型
cox2 <- coxph(Surv(time, status) ~ sex + age + ph.ecog + wt.loss, data = lung)
print("多变量Cox回归模型摘要")
summary(cox2)

# 5.3 比例风险假设检验
# 使用Schoenfeld残差检验比例风险假设
test_ph <- cox.zph(cox2)
print("比例风险假设检验结果")
test_ph

# 可视化比例风险假设检验结果
p5 <- ggcoxzph(test_ph, se = TRUE)
print("比例风险假设检验可视化")
p5

# 5.4 森林图展示Cox回归结果
p6 <- ggforest(
  cox2, 
  data = lung,
  main = "多变量Cox回归结果森林图",
  cpositions = c(0.02, 0.22, 0.4),
  fontsize = 1.0,
  refLabel = "参考",
  noDigits = 2
)
print("Cox回归结果森林图")
p6

# 5.5 预测生存概率
# 基于Cox模型预测不同协变量组合的生存概率
surv_pred <- survfit(cox2, newdata = lung)

p7 <- ggsurvplot(
  surv_pred,
  data = lung,
  conf.int = FALSE,
  risk.table = FALSE,
  legend = FALSE,
  palette = "rainbow",
  title = "基于Cox模型的个体生存概率预测",
  xlab = "生存时间（天）",
  ylab = "生存概率",
  ggtheme = theme_survival()
)
print("基于Cox模型的生存概率预测图")
p7

# 5.6 分层Cox模型（使用ovarian数据集）
ovarian$rx <- factor(ovarian$rx, levels = c(1, 2), labels = c("治疗1", "治疗2"))
ovarian$resid.ds <- factor(ovarian$resid.ds, levels = c(1, 2), labels = c("无残留病灶", "有残留病灶"))

# 创建分层Cox模型
strata_cox <- coxph(Surv(futime, fustat) ~ rx + age + strata(resid.ds), data = ovarian)
print("分层Cox模型摘要")
summary(strata_cox)

# ============================================================
# 第六部分：高级生存分析技术
# ============================================================

# 6.1 时间依变协变量
# 创建时间依变协变量的Cox模型
# 这里使用模拟数据演示概念
set.seed(123)
n <- 100
time <- runif(n, 1, 100)
event <- rbinom(n, 1, 0.5)
covariate1 <- rnorm(n)
covariate2 <- rnorm(n)

# 创建时间依变协变量数据
data_temp <- data.frame(
  id = 1:n,
  time = time,
  event = event,
  covariate1 = covariate1,
  covariate2 = covariate2
)

# 演示时间依变协变量的模型设置
print("时间依变协变量的模型设置示例：")
print("model <- coxph(Surv(start, stop, event) ~ covariate1 + covariate2, data = time_dependent_data)")

# 6.2 竞争风险模型
# 演示竞争风险模型的概念
print("竞争风险模型的模型设置示例：")
print("cmprsk_model <- coxph(Surv(time, status) ~ covariate1 + covariate2, data = data, cluster = id)")

# ============================================================
# 第七部分：生存分析结果的可视化展示
# ============================================================

# 7.1 组合多个生存曲线图
p_list <- list(p2, p4)
combined_plots <- arrange_ggsurvplots(p_list, ncol = 2, nrow = 1)
print("组合生存曲线图")
combined_plots

# 7.2 风险函数可视化
p8 <- ggcoxfunctional(
  Surv(time, status) ~ log(age) + sqrt(ph.ecog) + ph.karno, 
  data = lung
)
print("协变量函数形式评估图")
p8

# ============================================================
# 第八部分：导出结果
# ============================================================

# 创建输出目录（如果不存在）
if (!dir.exists("output")) dir.create("output")

# 导出KM曲线为PDF
print("正在导出KM曲线为PDF文件...")
ggsave(
  filename = "output/KM_Curve_Gender.pdf",
  plot = p3,
  width = 12,
  height = 10,
  units = "in",
  dpi = 300
)

# 导出Cox回归森林图为PDF
print("正在导出Cox回归森林图为PDF文件...")
ggsave(
  filename = "output/Cox_Forest_Plot.pdf",
  plot = p6,
  width = 10,
  height = 8,
  units = "in",
  dpi = 300
)

# 导出组合图表为PDF
print("正在导出组合图表为PDF文件...")
ggsave(
  filename = "output/Combined_Survival_Plots.pdf",
  plot = combined_plots,
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)

# ============================================================
# 第九部分：总结与讨论
# ============================================================

print("生存分析教程总结：")
print("1. 我们介绍了生存分析的基本数学概念，包括生存函数、风险函数等")
print("2. 使用Kaplan-Meier方法估计了生存曲线，并进行了精美的可视化")
print("3. 建立了Cox比例风险模型，评估了多个协变量对生存时间的影响")
print("4. 检验了比例风险假设，并可视化展示了Cox回归结果")
print("5. 介绍了高级生存分析技术，如时间依变协变量和竞争风险模型")
print("6. 所有结果已导出到output目录中")

print("教程完成！")