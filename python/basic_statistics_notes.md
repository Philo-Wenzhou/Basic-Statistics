# 基础统计学笔记（Python 实现版）

本文档与 `python/basic_statistics_algorithms.py` 配套，目标是把“统计学概念 + 代码实现 + 结果解释”串起来。该实现仅依赖 Python 标准库，便于在离线或受限环境中运行。

## 1. 描述性统计（Descriptive Statistics）

给定样本 $x_1, x_2, \dots, x_n$：

- **均值**：$\bar{x}=\frac{1}{n}\sum_{i=1}^n x_i$
- **样本方差**：$s^2=\frac{1}{n-1}\sum_{i=1}^n (x_i-\bar{x})^2$
- **样本标准差**：$s=\sqrt{s^2}$
- **四分位数**：$Q_1,Q_2(\text{中位数}),Q_3$
- **四分位距**：$IQR=Q_3-Q_1$

在 Python 中对应函数：`descriptive_statistics(data, sample=True)`。

---

## 2. 常见分布模拟

统计学习中常见 3 类分布：

1. 正态分布（normal）
2. 均匀分布（uniform）
3. 指数分布（exponential）

在 Python 中对应函数：`simulate_distributions(...)`，默认会返回一个字典。

---

## 3. 相关分析（Pearson Correlation）

皮尔逊相关系数：

$$
r=\frac{\sum (x_i-\bar{x})(y_i-\bar{y})}{\sqrt{\sum(x_i-\bar{x})^2}\sqrt{\sum(y_i-\bar{y})^2}}
$$

- $r\in[-1,1]$
- 越接近 1：正相关越强
- 越接近 -1：负相关越强
- 接近 0：线性相关弱

在 Python 中对应函数：`pearson_correlation(x, y)`。

---

## 4. 统计推断

### 4.1 单样本 t 检验

检验总体均值是否等于给定值 $\mu_0$：

$$
t=\frac{\bar{x}-\mu_0}{s/\sqrt{n}},\quad df=n-1
$$

Python 函数：`one_sample_t_test(data, mu=...)`。  
返回 `p_value_approx`（正态近似双侧 p 值）。

### 4.2 两独立样本 Welch t 检验

用于两组均值比较，且不要求方差齐性。  
Python 函数：`welch_t_test(group1, group2)`。  
返回 `p_value_approx`（正态近似双侧 p 值）。

### 4.3 单因素方差分析（One-way ANOVA）

用于比较多个组的均值是否存在显著差异。  
Python 函数：`one_way_anova(group1, group2, group3, ...)`。  
返回 `p_value_approx`（教学近似值，严格场景建议 `scipy`）。

---

## 5. 快速运行

在仓库根目录执行：

```bash
python3 python/basic_statistics_algorithms.py
```

会输出：

- normal 分布的描述性统计
- 相关系数示例
- 单样本 t 检验结果
- Welch t 检验结果
- One-way ANOVA 结果

---

## 6. 学习建议

1. 先看公式，再看函数签名；
2. 修改随机种子、样本量、均值和方差，观察检验 p 值如何变化；
3. 将函数迁移到你自己的数据项目中，用 pandas 读入真实数据后复用。

> 注：本版本为了兼容无第三方依赖环境，推断统计的 p 值采用近似计算。若你在科研/生产中使用，请优先换用 `scipy.stats` 做精确检验。
