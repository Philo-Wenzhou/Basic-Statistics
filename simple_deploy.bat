@echo off
chcp 65001 >nul

:: 简化版部署脚本：将项目内容直接复制并推送到GitHub Pages

:: 设置中文显示
rem 设置中文显示成功

:: 检查GitHub Pages远程仓库连接
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

:: 创建临时目录
echo 创建临时部署目录...
set DEPLOY_DIR=deploy_temp
del /q %DEPLOY_DIR% >nul 2>&1
rmdir /s /q %DEPLOY_DIR% >nul 2>&1
mkdir %DEPLOY_DIR%

:: 复制HTML文件
echo 复制HTML文件...
mkdir %DEPLOY_DIR%\survival_analysis >nul 2>&1
copy /y SurvivalAnalysis\*.html %DEPLOY_DIR%\survival_analysis\ >nul
if exist SurvivalAnalysis\styles.css (
    copy /y SurvivalAnalysis\styles.css %DEPLOY_DIR%\survival_analysis\ >nul
)

echo 复制README文件...
if exist README.md (
    copy /y README.md %DEPLOY_DIR% >nul
)

:: 创建简单的index.html主页
echo 创建主页...
echo ^<!DOCTYPE html^> > %DEPLOY_DIR%\index.html
echo ^<html lang="zh-CN"^> >> %DEPLOY_DIR%\index.html
echo ^<head^> >> %DEPLOY_DIR%\index.html
echo     ^<meta charset="UTF-8"^> >> %DEPLOY_DIR%\index.html
echo     ^<title^>统计学教程^</title^> >> %DEPLOY_DIR%\index.html
echo ^</head^> >> %DEPLOY_DIR%\index.html
echo ^<body^> >> %DEPLOY_DIR%\index.html
echo     ^<h1^>统计学教程与数据分析笔记^</h1^> >> %DEPLOY_DIR%\index.html
echo     ^<p^>^<a href="survival_analysis/生存分析教程与数学原理详解.html"^>生存分析教程与数学原理详解^</a^>^</p^> >> %DEPLOY_DIR%\index.html
echo ^</body^> >> %DEPLOY_DIR%\index.html
echo ^</html^> >> %DEPLOY_DIR%\index.html

:: 创建.nojekyll文件
echo. 2> %DEPLOY_DIR%\.nojekyll

:: 推送文件
cd %DEPLOY_DIR%
git init >nul
:: 设置用户名和邮箱（可选）
:: git config user.name "Your Name"
:: git config user.email "your.email@example.com"
git add . >nul
git commit -m "Deploy statistics content" >nul

:: 创建main分支
git branch -M main

:: 推送到GitHub Pages
rem 推送到GitHub Pages...
echo 正在推送...
git push -f https://github.com/Philo-Wenzhou/Philo-Wenzhou.github.io.git main

:: 清理
echo 清理临时文件...
cd ..
rmdir /s /q %DEPLOY_DIR% >nul

echo 部署完成！请访问 https://philo-wenzhou.github.io