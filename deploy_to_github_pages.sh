#!/bin/bash

# 部署脚本：将项目内容部署到GitHub Pages

# 设置中文显示
export LANG="zh_CN.UTF-8"

# 检查是否已添加GitHub Pages远程仓库
echo "检查GitHub Pages远程仓库连接..."
REMOTE_EXISTS=$(git remote | grep -c "github-pages")
if [ $REMOTE_EXISTS -eq 0 ]; then
    echo "错误：未找到github-pages远程仓库连接。请先执行："
    echo "git remote add github-pages https://github.com/Philo-Wenzhou/Philo-Wenzhou.github.io.git"
    exit 1
fi

# 创建临时部署目录
echo "创建临时部署目录..."
DEPLOY_DIR="./deploy_temp"
rm -rf $DEPLOY_DIR
mkdir -p $DEPLOY_DIR

# 复制HTML文件和必要资源
echo "准备部署内容..."

# 复制生存分析相关文件
if [ -d "./SurvivalAnalysis" ]; then
    mkdir -p $DEPLOY_DIR/survival_analysis
    cp -f ./SurvivalAnalysis/*.html $DEPLOY_DIR/survival_analysis/
    if [ -f "./SurvivalAnalysis/styles.css" ]; then
        cp -f ./SurvivalAnalysis/styles.css $DEPLOY_DIR/survival_analysis/
    fi
    echo "已复制生存分析相关文件"
fi

# 创建主页index.html
cat > $DEPLOY_DIR/index.html << 'EOF'
<!DOCTYPE html>
<html lang="zh-CN">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>统计学教程 - Philo的数据分析笔记</title>
    <style>
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            line-height: 1.6;
            color: #333;
            max-width: 1000px;
            margin: 0 auto;
            padding: 20px;
            background-color: #f5f5f5;
        }
        .container {
            background-color: white;
            padding: 40px;
            border-radius: 8px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
        h1 {
            color: #2c3e50;
            text-align: center;
            margin-bottom: 40px;
            border-bottom: 2px solid #3498db;
            padding-bottom: 15px;
        }
        h2 {
            color: #3498db;
            margin-top: 30px;
        }
        .course-card {
            background-color: #f8f9fa;
            padding: 20px;
            margin: 20px 0;
            border-radius: 6px;
            transition: transform 0.3s ease;
        }
        .course-card:hover {
            transform: translateY(-5px);
            box-shadow: 0 5px 15px rgba(0,0,0,0.1);
        }
        .course-card a {
            text-decoration: none;
            color: #3498db;
            font-weight: bold;
        }
        .course-card a:hover {
            text-decoration: underline;
        }
        footer {
            text-align: center;
            margin-top: 50px;
            color: #7f8c8d;
            font-size: 0.9em;
        }
    </style>
</head>
<body>
    <div class="container">
        <h1>统计学教程与数据分析笔记</h1>
        
        <p>欢迎来到我的统计学学习和数据分析笔记网站！这里整理了我在数据分析和统计学学习过程中的心得体会和实用教程。</p>
        
        <h2>教程列表</h2>
        
        <div class="course-card">
            <h3>生存分析教程与数学原理详解</h3>
            <p>本教程详细介绍了生存分析的基本概念、数学原理和R语言实现方法，包括Kaplan-Meier曲线、Cox比例风险模型等内容。</p>
            <a href="survival_analysis/生存分析教程与数学原理详解.html">查看教程</a>
        </div>
        
        <div class="course-card">
            <h3>基础统计学与数据可视化</h3>
            <p>介绍基础统计学概念和数据可视化技术，包括描述性统计、统计推断和各种图表绘制方法。</p>
            <p><em>内容即将上线...</em></p>
        </div>
        
        <footer>
            <p>&copy; 2025 Philo(Wenxing Yi) | <a href="https://github.com/Philo-Wenzhou">GitHub</a></p>
        </footer>
    </div>
</body>
</html>
EOF

# 添加.nojekyll文件以确保GitHub Pages正确处理以下划线开头的文件
touch $DEPLOY_DIR/.nojekyll

# 部署到GitHub Pages
cd $DEPLOY_DIR
echo "初始化部署仓库..."
git init
git add .
git commit -m "Deploy statistics tutorials to GitHub Pages"

echo "推送到GitHub Pages仓库..."
git push -f github-pages main

# 清理临时文件
cd ..
rm -rf $DEPLOY_DIR

echo "部署完成！"
echo "请访问 https://philo-wenzhou.github.io 查看部署效果。"
echo "注意：GitHub Pages可能需要几分钟时间来更新显示。"