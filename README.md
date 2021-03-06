# 上海疫情数据分析
上海的疫情现在看起来很严峻，网上很少看到有对此次疫情的分析。
因为自己本身住在上海，所以也对此次疫情的发展很关心。
## 1. 分析目标
- [x] 数据获取。通过爬虫从上海卫健委官方网站获取数据。
- [x] 确诊人数分析。主要是画图看确诊人数变化。
- [ ] 时间序列分析。
- [x] 按上海各区统计确诊人数。
- [ ] 通过地图来查看疫情变化。
- [x] **做一个dashboard更直观地表示。**
  - [x] 查看无症状和确诊的新增及累计人数
  - [x] 非管控区域病例比例
  - [x] 数据一览及下载

## 2. 执行手顺
```
# 爬取上海卫健委网站
source("web_crawler.r")
d.basic <- web_crawler()

# 提取确诊和无症状感染者信息
source("extract_info.r")
info <- extract_info()

# 在RStuido中进入shiny文件夹，Run App

```
## 3. dashboard截图
![地区病例趋势](screenshot/截屏1.png)
![非管控区域病例比例](screenshot/截屏2.png)
![详细数据](screenshot/截屏3.png)
![关于](screenshot/截屏4.png)

