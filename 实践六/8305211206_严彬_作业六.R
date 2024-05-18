mRNA_exprSet<-read.csv(file="C:/Users/yb0410/Desktop/R实践课6/mRNA_exprSet.csv",row.names = 1)
# 提取列名（样本ID）  
sample_ids <- colnames(mRNA_exprSet)  
# 使用substr函数基于ID的最后两位将样本分为正常组和肿瘤组
groups <- substr(sample_ids, nchar(sample_ids) - 1, nchar(sample_ids))  
groups <- ifelse(groups == "01", "tumor", ifelse(groups =="11", "normal", NA))
# 提取表达矩阵  
countData <- as.matrix(mRNA_exprSet[, sample_ids, drop = FALSE])  
# 创建列数据（colData）  
colData <- data.frame(row.names = sample_ids, group=groups)

library(DESeq2)
# 创建DESeqDataSet对象  
dds <- DESeqDataSetFromMatrix(countData = countData,  
                              colData = colData,  
                              design = ~ group)  
#通过DESeq函数进行差异表达分析  
dds <- DESeq(dds)  
#使用deseq2包中的result（）函数提取deseq2差异分析结果 
res <- results(dds) 
#将结果转换为矩阵
DEG<-data.frame(res)

#给差异分析结果添加上下调标签,参考CSDNhttps://blog.csdn.net/weixin_49878699/article/details/136114233?ops_request_misc=&request_id=&biz_id=102&utm_term=deseq2&utm_medium=distribute.pc_search_result.none-task-blog-2~all~sobaiduweb~default-4-136114233.142^v100^pc_search_result_base3&spm=1018.2226.3001.4187
DEG$change <- as.factor(
  ifelse(DEG$pvalue < 0.05 & abs(DEG$log2FoldChange) >= 0.5,
         ifelse(DEG$log2FoldChange > 0.5,'Up','Down'),'Not'))
#计数各标签基因的个数
table(DEG$change)
#保存差异分析的结果
write.csv(DEG,file="8305211206_严彬_作业六差异分析结果.csv",row.names = TRUE)

