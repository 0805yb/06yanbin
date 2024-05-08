#读取数据文件并将第一列设为行名
AData<-read.csv("D:/R语言在生物信息学的应用/ADdata/合并.csv",row.names=1)

#第1题
#将AData的dataframe类型转换为一维数据
AData2<-as.matrix(AData)
#查看
print(AData2)
#画出直方图
hist(AData2)



#第2题
# 计算距离矩阵  
AData_distmat <- dist(AData2, method = "euclidean") 
View(AData_distmat)
#进行层次聚类并绘图
AData_tree<-hclust(AData_distmat)
plot(AData_tree)


#第3题
# 加载ggplot2包  
library(ggplot2)
# 加载数据  
load("D:/R语言在生物信息学的应用/volcano.RData")  
# 绘制火山图  
ggplot(data = prostat, aes(x = FC, y = -log10(P)))  + 
  geom_point(aes(color = factor(ifelse(P< 0.05 & FC > log2(1.2), "Up",    
                                       ifelse(P< 0.05 & FC < log2(1/1.2), "Down", "NS")))))+   #添加数据点
  scale_color_manual(values = c("Up" = "red", "Down" = "green", "NS" = "grey"))  + #设置数据点的颜色
  labs(x = "Log2 Fold Change", y = "-Log10 P value", title = "Volcano Plot") +   #添加标签和标题
  theme_minimal() +  #设置主题
  theme(legend.title = element_blank(),  #隐藏图例
        plot.title = element_text(hjust = 0.5)) #标题居中显示
ggsave("实习4火山图.jpg", width = 8, height = 6, dpi = 300)  
