set.seed(1)
math.score <- sample(0:100, 80, replace = TRUE)
math.score

# (a) 座號1-30號同學之成積平均數: , 標準差:
mean(math.score[1:30]) # 平均數:56.16667
sd(math.score[1:30]) # 標準差:28.45212

# (b) 共有多少人及格? 31人, 及格同學的座號為何? 如第11行
sum(math.score >= 60) # 共有31人及格
which(math.score >= 60)# 1 5 8 11 12 15 17 18 19 21 22 25 27 29 31 32 39
#40 44 56 57 58 59 60 61 62 63 66 68 70 74

# (c)全班最高分與最低分為何? 對應的同學座號為何?
max(math.score) # 最高分:100
min(math.score) # 最低分:0
which(math.score == max(math.score)) # 最高分座號:22
which(math.score == min(math.score)) # 最低分座號:3

# (d)計算班上分數排行前十名(由高分至低分)之成積平均數、標準差
math.score.sort <- sort(math.score, decreasing = TRUE)
mean(math.score.sort[1:10]) # 平均:91.1
sd(math.score.sort[1:10]) # 標準差:5.3427

# (e)屏幕顯示80位同學分數資料的第一個四分位數
s <- summary(math.score)
s['1st Qu.'] # 第一四分位數: 27.75
