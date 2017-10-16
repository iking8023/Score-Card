# 1.数据导入
# -- library(data.table); library(dplyr); library(ggplot2)
df <- read.csv("C:/Users/XinshiYU/Desktop/GermanCredit.csv",  stringsAsFactors = F)

## 从txt导入
# ?read.table()

## 从数据库直接读取
# library(RJDBC)
# ?dbConnect()

## 数据拼接
# rbind()  行  cbind() 列
# library(sqldf) # !!! sqldf


# ---------------------------------------------------------------------------------
# 2. 数据探查
head(df)
str(df)
summary(df)
# 变量重赋值
df$credit_risk <- ifelse(df$credit_risk == 'bad', 1, 0) # credit_risk是否违约
data_bakup <- df

# 2.1检查缺失值
na_num <- apply(df, 2, function(x) sum(is.na(x)))
sort(na_num, decreasing = T) / nrow(df)
subset(df, is.na(job))
sqldf('select * from df where job is null ')

# library(VIM)  
# aggr(df)     
# library(mice) 
# md.pattern() 

# 2.2 缺失值处理
## 缺失值赋众数
df[is.na(df$job), 'job'] <- names(table(df$job)[which.max(table(df$job))])
sum(is.na(df$job))

## 缺失值赋均值
#df[which(is.na(df$age), 'age')] <- mean(df$age, na.rm=T)  # na.rm
## 缺失值赋特定值 
# for(i in 1:ncol(df)){
#   if(is.character(df[,i])){
#     df[is.na(df[ ,i]), i] <- "missing"
#   }
#   if(is.numeric(df[,i])){
#     df[is.na(df[ ,i]), i] <- -9999
#   }
# }

## 缺失值插补法
# library(DMwR)
# DMwR::knnImputation(data, k = 10, scale = T, meth = "weighAvg",  distData = NULL)
# library(mice)
# mice(data, m=5)

# 2.3 查看特征取值个数  -- unique(df[,'job']) %>% length()
val_num <- data.frame()
for (i in 1:ncol(df)){
  t1 <- length(unique(df[,i]))  # dplyr::n_distinct()
  t2 <- names(df)[i]
  val_num <- rbind(data.frame(variable = t2, num = t1, type = mode(df[,i]),
                              stringsAsFactors = F), val_num)
}
## apply(df, 2, function(x) length(unique(x)))
rm(i,t1,t2); gc()  # garbage collection

# 2.3.1 转换数据类型
convert_cols <- val_num[which(val_num$num < 5),'variable']
df[,convert_cols] <- sapply(df[,convert_cols], as.character)
str(df[, val_num[val_num$num < 5, 'variable']])

# 2.4 查看数据分布
# 2.4.1 数值型变量分位数  -- quantile(x, probs)
num_distribution <- c(); temp_name <- c()
for(i in names(df)){
  if(is.numeric(df[,i])){
    temp <- quantile(df[,i], probs=c(0,0.10,0.25,0.50,0.75,0.90,0.95,0.98,0.99,1), na.rm = T, names = T)
    temp_name <- c(temp_name, i)
    num_distribution <- rbind(num_distribution, temp)
  }
}
row.names(num_distribution) <- temp_name
num_distribution <- as.data.frame(num_distribution)
num_distribution$variable <- temp_name
rm(i, temp, temp_name)

## 异常值删除
age_0 <- subset(df, age==0); age_0
df <- df[- which(df$age==0), ]
rm(age_0)

# 2.3.2 查看类别取值分布   -- table(df$housing)
char_distribution <- data.frame(stringsAsFactors = F)
for(i in names(df)){
  if(!is.numeric(df[, i])){ 
    temp <- data.frame(Variable = i, table(df[, i]), stringsAsFactors = F)
    char_distribution <- rbind(char_distribution, temp)
  }
}
char_distribution$Per <- char_distribution$Freq / nrow(df)
rm(i,temp)

# 2.3.3 查看x-y分布  -- table(df$housing, df$credit_risk)
xy_distribution <- data.frame()
for(i in names(df)){
  if(!is.numeric(df[, i])){ 
    temp <- data.frame(variable = i, table(df[, i], df$credit_risk), stringsAsFactors = F)
    xy_distribution <- rbind(xy_distribution, temp)
  }
} 

xy_distribution <- transform(xy_distribution, Percent= xy_distribution$Freq / ifelse(xy_distribution$Var2 == 0, 699, 298))
rm(i,temp)

# 3.变量离散化 -- smbinning
# 3.1 字符转因子型 -- for smbinning.factor
for ( i in names(df)){
  if(i != 'credit_risk' & is.character(df[,i])) {
    df[, i] <- as.factor(df[, i])}
} 
str(df)

# 3.3 分箱 -- 连续变量分箱  smbinning
data_bak <- df
df$credit_risk <- as.numeric(df$credit_risk)  # 要求y值为数值型
bin_iv <- data.frame(); bin_var <- c()
library(smbinning)
var_name <- names(df)
for(i in var_name) {
  if(is.numeric(df[,i]) & i != 'credit_risk'){
    bin_tbl <- smbinning(df, y='credit_risk', x= i)
    bin_iv <- rbind(bin_iv, data.frame(bin_tbl$ivtable, variable=i))
    new_var <- paste('bin',i, sep='_')
    bin_var <- c(bin_var, new_var)
    df <- smbinning.gen(df, bin_tbl, new_var)    # 生成离散后的数据
  }
  
  if(is.factor(df[,i])){
    bin_tbl <- smbinning.factor(df, y='credit_risk', x= i)
    bin_iv <- rbind(bin_iv, data.frame(bin_tbl$ivtable, variable=i))
    new_var <-  paste('bin',i, sep='_')
    bin_var <- c(bin_var, new_var)
    df <- smbinning.factor.gen(df, bin_tbl, new_var)  # 生成离散后的数据
  }
} 
rm(i, new_var); 
# write.csv(bin_iv, file='C:/Users/XinshiYU/Desktop/bin_iv.csv')
# save(df, file='C:/Users/XinshiYU/Desktop/data_after_bin.rdata') 
df<- df[, c('credit_risk', bin_var)]
rm(bin_tbl, data_bak, var_name)

# 4. 计算WoE\IV
library(klaR)
# data split, training/test
# set.seed(24)
# train <- sample(nrow(df), 0.7*nrow(df), replace = F)
#   -- save(df, file='E:/YXS/app_download/new/data/before_woe.rdata')
woe_model <- woe(as.factor(df$credit_risk)~., data=df, zeroadj =0.5)
iv_table <- sort(woe_model$IV, decreasing = T) # woe_model$IV返回IV值，排序
iv_table

# 5.筛选特征变量
# 5.1 根据IV值筛选
iv_var <- names(iv_table[iv_table > 0.02])   
woe_model <- woe(as.factor(df$credit_risk)~., data = df[, c('credit_risk', iv_var)], zeroadj =0.5, appont =T)
traindata <- predict(woe_model, newdata=df[, c('credit_risk', iv_var)])
# 5.2 逐步回归筛选
library(leaps)
regfit <- regsubsets(credit_risk~., data = traindata, method = 'back', nvmax = 10) #'seqrep'
reg_summary <- summary(regfit)
plot(reg_summary$bic)
reg_summary
# 5.3 筛选入模变量
feature_in <- c('bin_status', 'bin_credit_history', 'bin_duration'
                ,'bin_savings','bin_purpose','bin_personal_status_sex',
                'bin_other_debtors', 'bin_installment_rate')
feature_in <- paste('woe', feature_in, sep='.')

# rm(woe_model, train_data,bin_iv, iv_var, reg_summary)

# 6. 逻辑回归训练
glmodel <- glm(credit_risk~., traindata[,c('credit_risk', feature_in)], family = binomial)
summary(glmodel)
# 6.1 相关性检验
corelation <- cor(traindata[,feature_in])
library(lattice)
levelplot(corelation)
rm(corelation)
# 6.2 VIF 共线性检验
library(car)
vif(glmodel, digits =3 )
# 6.3 模型评估
# 6.3.1 ROC / AUC
pred <- predict(glmodel, newdata = traindata,type = "response")
library(ROCR)
t <- prediction(pred, traindata[, 'credit_risk'])
t_roc <- performance(t, 'tpr', 'fpr')
plot(t_roc)
t_auc <- performance(t, 'auc')
t_auc@y.values
title(main = 'ROC Curve')
# 6.3.2 KS value
ks <- max(attr(t_roc, "y.values")[[1]] - (attr(t_roc, "x.values")[[1]])); print(ks)

# ---------------------------------------------------------------------------------
## 7. 制作评分卡 Scorecard
# 7.1 计算factor和offset
# 620 = offset + factor * log(15*2)
# 600 = offset + factor * log(15) # 按好坏比15为600分, 翻一番加20
factor <- 20/log(2)
offset <- 600-factor*log(15)
# 7.2提取所需 woe、逻辑回归系数、截距项、特征个数
glm_coef <- data.frame(coef(glmodel))
NamesWoE <- row.names(glm_coef)[-1] <- gsub('woe.', replacement = '', row.names(glm_coef)[-1])
a = glm_coef[1,1]   # 截距
Beta <- glm_coef$coef.glmodel.[-1]    # 系数
names(Beta) <- row.names(glm_coef)[-1]; Beta # 系数名
glm_coef$Variables  <-  row.names(glm_coef)
feature_num <- nrow(glm_coef) - 1 # 特征数目
Score_card <- data.frame()
# Score_card  <-  data.frame(WoE = c(NA),  Score = c(NA),  Variable = c(NA),  Beta = c(1), Band = c(NA))
# Score_card <- na.omit(Score_card) # delte na cases 

# 7.3 计算最终评分
for (i in NamesWoE) # 循环变量，计算每个变量取值下的分数
{
    WoEEE <- data.frame(woe_model$woe[i])
      # 评分公式
      Score <- data.frame(-(Beta[i]*WoEEE + a/(feature_num)) * factor + offset/(feature_num))
      Temp <- cbind(WoEEE,  Score)
      Temp$Variable <- i
      Temp$Beta <- Beta[i]
      Temp$Value <- row.names(Temp)
      
      names(Temp)[1] <- "WoE"
      names(Temp)[2] <- "Score"
      
      Score_card <- rbind(Temp,  Score_card)                   
}
rm(i,WoEEE, NamesWoE, feature_num, glm_coef, Temp, Score)
write.table(Score_card, file='C:/Users/XinshiYU/Desktop/Scorecard.csv', sep  =  ",  ", col.names  =  NA)

# 
df_score <- sqldf( "select  
  case when credit_risk='bad' then 1
                    else 0 end as credit_risk
                    
                    ,case when  installment_rate= '1' then 78.32
                    when  installment_rate= '2' then 74.41
                    when  installment_rate= '3' then 71.26
                    else  61.93 end as installment_rate_score
                    
                    ,case  when  other_debtors= 'co-applicant' then 46.97
                    when  other_debtors= 'guarantor' then 88.75
                    else 68.33 end as other_debtors_score
                    
                    ,case when  personal_status_sex= 'female : divorced/separated/married' then 60.81
                    when  personal_status_sex= 'male : divorced/separated' then 53.83
                    when  personal_status_sex= 'male : married/widowed' then 72.61
                    else 73.62 end as personal_status_sex_score
                    
                    ,case when  purpose= 'business' then 100.77
                    when  purpose= 'car (new)' then 58.65
                    when  purpose= 'car (used)' then 90.25
                    when  purpose= 'domestic appliances' then 78.89
                    when  purpose= 'education' then 60.54
                    when  purpose= 'furniture/equipment' then 54.63
                    when  purpose= 'others' then 62.84
                    when  purpose= 'radio/television' then 65.63
                    when  purpose= 'repairs' then 64.08
                    else 52.11 end as purpose_score
                    
                    , case when  savings= '... < 100 DM' then 62.35
                    when  savings= '... >= 1000 DM' then 92.59
                    when  savings= '100 <= ... < 500 DM' then 65.08
                    when  savings= '500 <= ... < 1000 DM' then 83.44
                    else 83.83 end as savings_score
                    
                    , case when  duration <= 11 then 93.35
                    when  duration < '33' then 69.07
                    else 46.43 end as duration_score
                    
                    , case when credit_history= 'all credits at this bank paid back duly' then 41.29
                    when  credit_history= 'critical account/other credits existing' then 85.55
                    when  credit_history= 'delay in paying off in the past' then 67.02
                    when  credit_history= 'existing credits paid back duly tillnow' then 66.15
                    else 36.01 end as credit_history_score
                    
                    , case when  status= '... < 100 DM' then 48.68
                    when  status= '... >= 200 DM / salary for at least 1year' then 77.92
                    when  status= '0 <= ... < 200 DM' then 58.75
                    else 96.37 end as status_score  from data_bakup")

# sql_score$tot_score <- apply(sql_score[,2:9], 1, sum)
# library(Hmisc)
# Ecdf(sql_score$tot_score, group=sql_score$credit_risk, lty=2, xlab="Score", 
# label.curves=list(keys=1:2), main="KS Curve")

## 图形探索
# library(ggthemr)
# library(gridExtra)
# ggthemr('dust')
# p1 <- ggplot(df, aes(x = duration, y = ..count..)) +
#   geom_histogram(fill = "blue", colour = "grey60", size = 0.2, alpha = 0.2,binwidth = 5)
# p2 <- ggplot(df, aes(x= status, y = ..count..)) + 
#   geom_bar(fill = "blue", size = 0.2, alpha = 0.2)
# ggplot(df, aes(x = age,y = ..count..)) + geom_histogram(fill = "blue", colour = "grey60", size = 0.2, alpha = 0.2,binwidth = 5)
# ggplot(df, aes(x =credit_risk,y = ..count..)) + geom_histogram(fill = "blue", colour = "grey60" , alpha = 0.2,binwidth = 0.5)
# grid.arrange(p1,p2)

