@echo off
chcp 65001 >nul

:: 部署脚本：将项目内容部署到GitHub Pages（Windows版本）

:: 检查是否已添加GitHub Pages远程仓库
echo 检查GitHub Pages远程仓库连接...
for /f "tokens=1" %%i in ('git remote') do (
    if "%%i" == "github-pages" (
        set REMOTE_EXISTS=1
    )
)

if not defined REMOTE_EXISTS (
    echo 错误：未找到github-pages远程仓库连接。请先执行：
    echo git remote add github-pages https://github.com/Philo-Wenzhou/Philo-Wenzhou.github.io.git
    pause
    exit /b 1
)

:: 创建临时部署目录
echo 创建临时部署目录...
set DEPLOY_DIR=deploy_temp
if exist "%DEPLOY_DIR%" rmdir /s /q "%DEPLOY_DIR%"
mkdir "%DEPLOY_DIR%"

:: 复制HTML文件和必要资源
echo 准备部署内容...

:: 复制生存分析相关文件
if exist "SurvivalAnalysis" (
    mkdir "%DEPLOY_DIR%\survival_analysis"
    copy /y "SurvivalAnalysis\*.html" "%DEPLOY_DIR%\survival_analysis\"
    if exist "SurvivalAnalysis\styles.css" (
        copy /y "SurvivalAnalysis\styles.css" "%DEPLOY_DIR%\survival_analysis\"
    )
    echo 已复制生存分析相关文件
)

:: 创建主页index.html
echo 创建主页index.html...
( 
    echo ^<!DOCTYPE html^\>
    echo ^<html lang="zh-CN"^>
    echo ^<head^>
    echo     ^<meta charset="UTF-8"^>
    echo     ^<meta name="viewport" content="width=device-width, initial-scale=1.0"^>
    echo     ^<title^>统计学教程 - Philo的数据分析笔记^</title^>
    echo     ^<style^>
    echo         body {
    echo             font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
    echo             line-height: 1.6;
    echo             color: #333;
    echo             max-width: 1000px;
    echo             margin: 0 auto;
    echo             padding: 20px;
    echo             background-color: #f5f5f5;
    echo         }
    echo         .container {
    echo             background-color: white;
    echo             padding: 40px;
    echo             border-radius: 8px;
    echo             box-shadow: 0 2px 10px rgba(0,0,0,0.1);
    echo         }
    echo         h1 {
    echo             color: #2c3e50;
    echo             text-align: center;
    echo             margin-bottom: 40px;
    echo             border-bottom: 2px solid #3498db;
    echo             padding-bottom: 15px;
    echo         }
    echo         h2 {
    echo             color: #3498db;
    echo             margin-top: 30px;
    echo         }
    echo         .course-card {
    echo             background-color: #f8f9fa;
    echo             padding: 20px;
    echo             margin: 20px 0;
    echo             border-radius: 6px;
    echo             transition: transform 0.3s ease;
    echo         }
    echo         .course-card:hover {
    echo             transform: translateY(-5px);
    echo             box-shadow: 0 5px 15px rgba(0,0,0,0.1);
    echo         }
    echo         .course-card a {
    echo             text-decoration: none;
    echo             color: #3498db;
    echo             font-weight: bold;
    echo         }
    echo         .course-card a:hover {
    echo             text-decoration: underline;
    echo         }
    echo         footer {
    echo             text-align: center;
    echo             margin-top: 50px;
    echo             color: #7f8c8d;
    echo             font-size: 0.9em;
    echo         }
    echo     ^</style^>
    echo ^</head^>
    echo ^<body^>
    echo     ^<div class="container"^>
    echo         ^<h1^>统计学教程与数据分析笔记^</h1^>
    echo         
    echo         ^<p^>欢迎来到我的统计学学习和数据分析笔记网站！这里整理了我在数据分析和统计学学习过程中的心得体会和实用教程。^</p^>
    echo         
    echo         ^<h2^>教程列表^</h2^>
    echo         
    echo         ^<div class="course-card"^>
    echo             ^<h3^>生存分析教程与数学原理详解^</h3^>
    echo             ^<p^>本教程详细介绍了生存分析的基本概念、数学原理和R语言实现方法，包括Kaplan-Meier曲线、Cox比例风险模型等内容。^</p^>
    echo             ^<a href="survival_analysis/生存分析教程与数学原理详解.html"^>查看教程^</a^>
    echo         ^</div^>
    echo         
    echo         ^<div class="course-card"^>
    echo             ^<h3^>基础统计学与数据可视化^</h3^>
    echo             ^<p^>介绍基础统计学概念和数据可视化技术，包括描述性统计、统计推断和各种图表绘制方法。^</p^>
    echo             ^<p^>^<em^>内容即将上线...^</em^>^</p^>
    echo         ^</div^>
    echo         
    echo         ^<footer^>
    echo             ^<p^>^&copy; 2025 Philo(Wenxing Yi) ^| ^<a href="https://github.com/Philo-Wenzhou"^>GitHub^</a^>^</p^>
    echo         ^</footer^>
    echo     ^</div^>
    echo ^</body^>
    echo ^</html^>
) > "%DEPLOY_DIR%\index.html"

:: 添加.nojekyll文件以确保GitHub Pages正确处理以下划线开头的文件
echo. 2> "%DEPLOY_DIR%\.nojekyll"

:: 部署到GitHub Pages
echo 初始化部署仓库...
cd "%DEPLOY_DIR%"
git init
git add .
git commit -m "Deploy statistics tutorials to GitHub Pages"

echo 推送到GitHub Pages仓库...
git push -f github-pages main

:: 清理临时文件
cd ..
rmdir /s /q "%DEPLOY_DIR%"

echo 部署完成！
echo 请访问 https://philo-wenzhou.github.io 查看部署效果。
echo 注意：GitHub Pages可能需要几分钟时间来更新显示。
pause