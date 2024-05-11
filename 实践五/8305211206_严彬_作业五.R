#第1题
#读取数据文件并将第一列设为行名
ADdata<-read.csv("D:/R语言在生物信息学的应用/ADdata/合并.csv",row.names=1)
#使用grep函数搜索列名中包含"asym"、"ad"和"ctl"的列，并返回这些列的索引。
a<-grep("asym",colnames(ADdata))
b<-grep("ad",colnames(ADdata))
c<-grep("ctl",colnames(ADdata))
#创建group向量，用于存储每列的组标签，并将其初始化为NA值
group<-NA
group[a]<-"asym"
group[b]<-"ad"
group[c]<-"ctl"
#创建一个数据框result，用于储存结果，先将pvalue列初始化为NA
result<-data.frame(ID=rownames(ADdata),pvalue=NA)

# 使用for循环遍历ADdata的前100行
for(i in 1:100){
# 提取第i行的数据，使用unlist函数将其转换为一个向量
prox<- unlist(ADdata[i,])
#统计这行三组的非零元素的数量
a0num<-sum(prox[a]!=0)
b0num<-sum(prox[b]!=0)
c0num<-sum(prox[c]!=0)
# 如果每组的至少有三个非零元素，则进行ANOVA检验：
if(a0num>=3 & b0num>=3 & c0num>=3)
{
  #将数据进行log2标准化
  prox<-log2(prox)
  #将数据转换后的无穷大值替换为NA
  prox[is.infinite(prox)]<-NA
  # 使用oneway.test函数对prox向量进行单因素方差分析
 test<- oneway.test(prox~group)
 # 从方差分析结果中提取p值，并将其存储在result数据框的pvalue列中对应的位置  
 result$pvalue[i]<-test$p.value
 }
}
write.table(result,file="8305211206-严彬-作业五第1题.csv",row.names = FALSE)


#第2题
load("volcano.RData")
#加载clusterProfiler包,用于富集分析
library(clusterProfiler)
#加载org.Hs.eg.db包，这是一个包含人类基因组注释信息的数据库包 
library(org.Hs.eg.db)
#GO富集分析
allID<-prostat$ID[prostat$P < 0.05] #提取P值小于0.05的基因ID
GO_enrich <- enrichGO(gene = allID, 
                      OrgDb = org.Hs.eg.db, #人类基因组数据库作为基因组注释数据库 
                     keyType = "SYMBOL", #本数据基因ID的类型是SYMBOL
                     ont = "BP",   #选择BP作为GO富集分析的本体
                     pvalueCutoff = 0.05,   #设置P值的截断值 
                     pAdjustMethod = 'none',  #P值矫正方法
                     readable = T   #将GO条目的描述简化为更易读的格式
                     ) 
# 使用dotplot函数绘制GO富集分析结果的点状图，展示前30个最显著的GO条目
dotplot(GO_enrich,title="GO Enrichment",showCategory=30,font.size=5)
