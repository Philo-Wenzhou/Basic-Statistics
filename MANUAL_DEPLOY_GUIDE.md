# GitHub Pages 手动部署指南

由于自动部署脚本遇到了网络连接问题，这里提供一个手动部署步骤指南，帮助你将统计学习项目内容部署到 GitHub Pages。

## 准备工作

确保你已经有以下环境和配置：
1. 安装了 Git
2. 已经配置了 GitHub 账号（有访问 Philo-Wenzhou.github.io 仓库的权限）
3. 本地有 Basic Statistics 项目的完整代码

## 手动部署步骤

### 1. 创建部署目录

首先创建一个专门用于部署的目录：

```bash
# 在任意位置创建一个新目录（不要在当前Git仓库内）
mkdir stats_deploy
cd stats_deploy
```

### 2. 初始化 Git 仓库

```bash
git init
# 设置分支为 main
git checkout -b main
```

### 3. 复制项目内容

从你的 Basic Statistics 项目中复制需要部署的文件：

```bash
# 复制生存分析相关文件
mkdir survival_analysis
copy "E:\Rprogram\Basic Statistics\SurvivalAnalysis\*.html" survival_analysis\
copy "E:\Rprogram\Basic Statistics\SurvivalAnalysis\styles.css" survival_analysis\

# 复制 README 文件
copy "E:\Rprogram\Basic Statistics\README.md" .
```

### 4. 创建主页文件

创建一个简单的 index.html 作为网站主页：

```bash
# 使用记事本创建 index.html
notepad index.html
```

在记事本中粘贴以下内容：

```html
<!DOCTYPE html>
<html lang="zh-CN">
<head>
    <meta charset="UTF-8">
    <title>统计学教程</title>
</head>
<body>
    <h1>统计学教程与数据分析笔记</h1>
    <p><a href="survival_analysis/生存分析教程与数学原理详解.html">生存分析教程与数学原理详解</a></p>
</body>
</html>
```

保存并关闭记事本。

### 5. 创建 .nojekyll 文件

创建一个空的 `.nojekyll` 文件，确保 GitHub Pages 正确处理以下划线开头的文件：

```bash
echo. > .nojekyll
```

### 6. 添加和提交文件

```bash
git add .
git commit -m "Deploy statistics tutorials to GitHub Pages"
```

### 7. 连接到 GitHub Pages 仓库

添加 GitHub Pages 仓库作为远程仓库：

```bash
git remote add origin https://github.com/Philo-Wenzhou/Philo-Wenzhou.github.io.git
```

### 8. 推送到 GitHub Pages

```bash
git push -f origin main
```

### 9. 验证部署

部署完成后，等待几分钟，然后访问 https://philo-wenzhou.github.io 查看部署效果。

## 故障排除

如果遇到连接问题：
- 检查网络连接是否正常
- 确认你有访问 Philo-Wenzhou.github.io 仓库的权限
- 尝试使用 SSH 而不是 HTTPS 连接 GitHub
- 检查是否需要配置代理

## 简化的快速部署方法

如果你已经设置了 SSH 密钥，也可以使用以下更简单的命令序列：

```bash
mkdir -p stats_deploy/survival_analysis
cp -r "E:\Rprogram\Basic Statistics\SurvivalAnalysis\*.html" stats_deploy/survival_analysis/
cp -r "E:\Rprogram\Basic Statistics\SurvivalAnalysis\styles.css" stats_deploy/survival_analysis/
cp "E:\Rprogram\Basic Statistics\README.md" stats_deploy/

# 创建 index.html 和 .nojekyll 文件（步骤同前）

cd stats_deploy
git init
git checkout -b main
git add .
git commit -m "Deploy"
git remote add origin git@github.com:Philo-Wenzhou/Philo-Wenzhou.github.io.git
git push -f origin main
```

## 自动化部署脚本（修复版）

如果你仍然希望使用脚本自动化部署过程，可以尝试在网络条件较好的情况下运行 `simple_deploy.bat` 脚本。脚本已经更新为使用完整的 GitHub Pages 仓库 URL。