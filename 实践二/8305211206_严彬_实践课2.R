对象一="abc"
a=1
.b<-2
print(对象一)
print(a)
print(.b)

num1=c(27,44,10,54,38,23,30,22,78,40)
name=c('A','A','A','B','B','B','C','C','C','C')
age=c(23,22,14,35,67,45,58,50,37,53)
tapply(age,name,mean)

num3=sample(20:60,10,replace=TRUE)
num4=sample(20:60,10,replace=TRUE)
data.frame(num1, age, num3,num4) 

num=sample(1:50,10,replace = TRUE)
count=0
for (i in num) {
if(i>10) 
  {count=count+1}
}
print(count)

n = length(num)  
for (i in 1:(n - 1)) {
  for (j in 1:(n-i)) {
    if (num[j] < num[j + 1]) {  
      temp = num[j]  
      num[j] = num[j + 1]  
      num[j + 1] = temp  
    }  
  }
}
print(num)
