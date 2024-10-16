# 重新整理思路及代码
# Date::2022-09-06

rm(list = ls())

# 设置路径
setwd('/home/alo/Desktop/Myshare/RWorkspace/R/clinical_investigation_2022/outpatient_info/')
setwd('/home/alo/Rspace/clinical_investigation_2022/outpatient_info/') ## On server

# 导入程序

require(tidyverse)
require(readxl)
require(xlsx)
require(lubridate)

# 导入数据

## 门诊初治患者数据

### 导入门诊患者就诊记录(2015.3-2022.9)
load('../Output/outpatient_info_2015-2022.Rdata') ##共计144万患者


################################################################################
### 提取诊断为肝损原因待查、慢乙肝患者及初诊患者
### 所以按时间排序进行初诊患者鉴别

df_2 <- df %>% 
  select(门诊号, 诊次, 诊别, 姓名, 性别, 年龄, 临床诊断, 挂号时间)  %>% 
  mutate(CHB = ifelse(str_detect(临床诊断, '(慢性乙型肝炎)|
                                           (慢性乙型病毒性肝炎)|(慢性轻度乙型肝炎)|
                                           (慢性中度乙型肝炎)|(慢性重度乙型肝炎)|
                                           (HBsAg携带者)|(HBV携带者)|
                                           (慢乙肝)'), 
                      '是', '否')) %>%
  mutate(LFA = ifelse(str_detect(临床诊断, '(肝功)|(肝功能)'), '是', '否')) %>%   
  mutate(HCC = ifelse(str_detect(临床诊断, '(肝癌)|(肝恶性肿瘤)|(肝占位)|(肝脏肿瘤)'), '是', '否')) %>%
  mutate(LC = ifelse(str_detect(临床诊断, '(乙型肝炎肝硬化)|(乙型肝炎后肝硬化)'),'是', '否')) %>% 
  mutate('挂号时间' = as_date(挂号时间)) %>% 
  # filter(诊别 == '初诊' & (CHB == '是' | LFA == '是') & (HCC == '否' | LC == '否')) %>% 
  filter((CHB == '是' | LFA == '是') & (HCC == '否' | LC == '否')) %>% 
  # distinct(姓名, .keep_all = T) #已确保名字系唯一
  group_by(姓名) %>% 
  arrange(挂号时间) %>%
  slice(1) %>% 
  ungroup()

################################################################################
### 与门诊TAF和TDF的名单相交集
### 准备数据


setwd('/home/alo/sambashare/RWorkspace/R/clinical_investigation_2022/Data2/')
setwd('../Data2/')

fn <- dir()

df_3_1 <- read_excel(fn[1], sheet = 1) %>% 
  select(发药时间, 药品名称, 患者姓名, 门诊号) %>% 
  mutate('用药日期' = as_date(发药时间)) %>% 
  arrange(用药日期) %>% 
  mutate(药品名称 = "(韦瑞德)替诺福韦片")


df_3_2 <-  read_excel(fn[3], sheet = 1) %>% 
  select(发药时间, 药品名称, 患者姓名, 门诊号) %>% 
  mutate('用药日期' = as_date(发药时间)) %>% 
  arrange(用药日期) %>% 
  mutate(药品名称 = "丙酚替诺福韦片[韦立得]")


df_3 <- bind_rows(df_3_1, df_3_2) 




################################################################################
### 考虑如下情况，初诊为'肝功能异常原因待查'，后确诊为慢性乙型肝炎；或直接初诊为慢乙肝患者
### 通过与服用NAs (TAF+TDF)的患者相交，获得门诊初诊CHB患者的名单

fn <- df_3 %>% pull(患者姓名) ## 提取服用NAs(TAF+TDF)的患者名单

df_4 <-  df_2 %>% filter(姓名 %in% fn )

# save(df_4, file = '../Output/初诊患者名单更新(TAFTDFDATE)220914.Rdata')

################################################################################
### 发生肝癌的患者及发生日期

fn <- df_4 %>% pull(姓名) # 提取初诊CHB患者的名单

df_5 <- df %>%  # df为门诊全员数据
  select(门诊号, 诊次, 诊别, 姓名, 性别, 年龄, 临床诊断, 挂号时间)  %>% 
  mutate(CHB = ifelse(str_detect(临床诊断, '(慢性乙型肝炎)|
                                           (慢性乙型病毒性肝炎)|(慢性轻度乙型肝炎)|
                                           (慢性中度乙型肝炎)|(慢性重度乙型肝炎)|
                                           (HBsAg携带者)|(HBV携带者)|
                                           (慢乙肝)'), '是', '否')) %>%
  mutate(LFA = ifelse(str_detect(临床诊断, '(肝功)|(肝功能)'), '是', '否')) %>%   
  mutate(HCC = ifelse(str_detect(临床诊断, '(肝癌)|(肝恶性肿瘤)|(肝占位)|(肝脏肿瘤)'), '是', '否')) %>%
  mutate(LC = ifelse(str_detect(临床诊断, '(乙型肝炎肝硬化)|(乙型肝炎后肝硬化)'),'是', '否')) %>% 
  mutate('挂号时间' = as_date(挂号时间)) %>% 
  filter(诊别 == '复诊' & HCC == '是') %>% 
  filter(姓名 %in% fn) %>% 
  group_by(姓名) %>% 
  arrange(挂号时间) %>% 
  slice(1)

################################################################################
## 发生肝硬化的患者及发生日期

fn <- df_4 %>% pull(姓名) # 提取初诊CHB患者的名单

df_6 <- df %>% 
  select(门诊号, 诊次, 诊别, 姓名, 性别, 年龄, 临床诊断, 挂号时间)  %>% 
  mutate(CHB = ifelse(str_detect(临床诊断, '(慢性乙型肝炎)|
                                           (慢性乙型病毒性肝炎)|(慢性轻度乙型肝炎)|
                                           (慢性中度乙型肝炎)|(慢性重度乙型肝炎)|
                                           (HBsAg携带者)|(HBV携带者)|
                                           (慢乙肝)'), 
                      '是', '否')) %>%
  mutate(LFA = ifelse(str_detect(临床诊断, '(肝功)|(肝功能)'), '是', '否')) %>%   
  mutate(HCC = ifelse(str_detect(临床诊断, '(肝癌)|(肝恶性肿瘤)|(肝占位)|(肝脏肿瘤)'), '是', '否')) %>%
  mutate(LC = ifelse(str_detect(临床诊断, '(乙型肝炎肝硬化)|(乙型肝炎后肝硬化)'),'是', '否')) %>% 
  mutate('挂号时间' = as_date(挂号时间)) %>% 
  filter(诊别 == '复诊' & LC == '是' & HCC == '否') %>% 
  filter(姓名 %in% fn) %>% 
  group_by(姓名) %>% 
  arrange(挂号时间) %>% 
  slice(1) ## 根据时间进行排序后，取的第1列数据，即为第一次诊断HCC或LC的时间
 
 ############################################################################### 
 ### 整合数据

 ### 获得随访时间

setwd('/home/alo/sambashare/RWorkspace/R/clinical_investigation_2022/Data2/')

fn <- dir()

  df_7_1 <-read_excel(fn[1], sheet = 1) %>% 
    select(发药时间, 药品名称, 患者姓名, 门诊号) %>% 
    mutate('用药日期' = as_date(发药时间)) %>% 
    arrange(用药日期) %>% 
    rename(ID = 门诊号, 姓名 = 患者姓名) %>% select(-1) %>% 
    group_by(姓名) %>% 
    slice(c(1, n())) %>% 
    ungroup() %>% 
    mutate(药品名称 = "(韦瑞德)替诺福韦片") %>% 
    mutate('周期' = rep(c('D1', 'D2'), 5770)) %>%
    relocate(用药日期, .after = 周期) %>% 
    select(药品名称, 姓名, 周期, 用药日期) %>% 
    pivot_wider(names_from = 周期, values_from = 用药日期) %>% 
    mutate('疗程' = as.integer(difftime(D2,D1, unit = 'weeks'))) %>% 
    arrange(desc(疗程)) %>% 
    filter(疗程 >= 48)


  df_7_2 <-  read_excel(fn[3], sheet = 1) %>% 
    select(发药时间, 药品名称, 患者姓名, 门诊号) %>% 
    mutate('用药日期' = as_date(发药时间)) %>% 
    arrange(用药日期) %>% 
    rename(ID = 门诊号, 姓名 = 患者姓名) %>% select(-1) %>% 
    group_by(姓名) %>% 
    slice(c(1, n())) %>% 
    ungroup() %>% 
    mutate(药品名称 = "丙酚替诺福韦片[韦立得]") %>% 
    mutate('周期' = rep(c('D1', 'D2'), 2918)) %>%
    relocate(用药日期, .after = 周期) %>% 
    select(药品名称, 姓名, 周期, 用药日期) %>% 
    pivot_wider(names_from = 周期, values_from = 用药日期) %>% 
    mutate('疗程' = as.integer(difftime(D2,D1, unit = 'weeks'))) %>% 
    arrange(desc(疗程)) %>% 
    filter(疗程 >= 48)

df_7 <- bind_rows(df_7_1, df_7_2)
# write.csv(df_7, file = '../Output/门诊患者随访日期0906.csv')

## 提取重复的名单(这类患者通常为更换药物的患者)

df_7_du <- df_7 %>% group_by(姓名) %>% 
      filter(n()>1) %>% 
      ungroup() %>% 
      arrange(姓名)

# write.csv(df_7_du, file = "../Output/门诊患者换药名单(2015-2022).csv")

## 换药的患者分两部分分析，换药后的部分放到后续分析

df_7_sing <- df_7 %>% 
         group_by(姓名) %>% 
          arrange(D1) %>% 
         slice(1) %>% 
         ungroup() %>% 
         mutate(Trans = ifelse(姓名 %in% df_7_du$姓名,'是','否')) %>% 
         left_join(df_7_du %>% filter(药品名称 == '丙酚替诺福韦片[韦立得]'), by = '姓名')

table(df_7_sing$Trans)


################################################################################
### 门诊初诊慢乙肝患者，并在治疗期间发生了肝硬化及肝癌，及其发病时间

  
  data <- df_4 %>% 
          left_join(df_7_sing, by = "姓名") %>% arrange(挂号时间) %>% 
          group_by(姓名) %>% 
          slice(1) %>% 
          ungroup() %>% 
          filter(!is.na(D1.x)) %>% 
          left_join(df_6 %>% select(姓名:年龄, 挂号时间, HCC, LC), by = "姓名") %>% 
          left_join(df_5 %>% select(姓名:年龄, 挂号时间, HCC, LC), by = "姓名") %>% 
          mutate(LC.y = ifelse(abs(年龄.x - 年龄.y) > 5, NA, LC.y),
                HCC.y = ifelse(abs(年龄.x - 年龄.y) > 5, NA, HCC.y)) %>% ## 如果年龄相差超过5岁，认为不是同一个人
          mutate(HCC = ifelse(abs(年龄.x - 年龄) > 5, NA, HCC),
                 LC = ifelse(abs(年龄.x - 年龄) > 5, NA, LC)) %>% 
          mutate(性别.y = ifelse(LC.y == '是', 性别.y, NA),
                 年龄.y= ifelse(LC.y == '是', 年龄.y, NA),
                 挂号时间.y = ifelse(LC.y == '是', 挂号时间.y, NA)) %>% 
          mutate(性别 = ifelse(HCC == '是', 性别, NA),
                 年龄 = ifelse(HCC == '是', 年龄, NA),
                 挂号时间 = ifelse(HCC == '是', 挂号时间, NA)) %>% 
          mutate(folowup1 = ifelse(is.na(疗程.y), 疗程.x, 疗程.x + 疗程.y)) %>% # 注意这里的随访时间应该为换药前后相加
          mutate(挂号时间.y = as_date(挂号时间.y), 挂号时间 = as_date(挂号时间)) %>% 
          mutate(folowup1 = ifelse(is.na(LC.y),
                                   folowup1, as.integer(difftime(挂号时间.y, D1.x, unit = 'weeks')))) %>% 
          mutate(folowup2 = ifelse(!is.na(HCC),
                           as.integer(difftime(挂号时间, D1.x, unit = 'weeks')), 
                           ifelse(is.na(疗程.y), 疗程.x, 疗程.x + 疗程.y)
                           )) %>% # 发生肝癌的随访时间
          mutate(folowup3 = ifelse(folowup1 > folowup2,
                           folowup2, folowup1)) %>% # 发生肝硬化和肝癌的随访时间
         filter(folowup1 > 0 & folowup2 >0)  ## 说明有些患者在服用TAF或TDF前就已经是诊断LC或HCC了，这类患者应该要删除的
 
# write.csv(data, file = "../Output/原始数据表格0916.csv")
#  
# save(data, file = "../Output/TAF_and_TDF_raw_data_0916.Rdata")       

################################################################################
##  对数据进一步筛选

load("../Output/TAF_and_TDF_raw_data_0916.Rdata")

df <- data %>% mutate(grp = ifelse(Trans == '是', 'Trans',  
                                   ifelse(药品名称.x == '(韦瑞德)替诺福韦片', 'TDF', 'TAF'))) %>% 
  mutate(LC.y = ifelse(is.na(LC.y), '否', LC.y)) %>% # 不能有NA值
  mutate(HCC = ifelse(is.na(HCC), '否', HCC)) %>%
  group_by(HCC) %>% 
  mutate(HCC = ifelse(性别.x != 性别, NA, HCC)) %>% # 排除性别不同
  ungroup() %>% 
  mutate(HCC = ifelse(is.na(HCC), '否', HCC)) %>% 
  group_by(LC.y) %>% 
  mutate(LC.y= ifelse(性别.x != 性别.y, NA, LC.y)) %>% 
  ungroup() %>% 
  mutate(LC.y = ifelse(is.na(LC.y), '否', LC.y)) %>% 
  mutate(status.x = ifelse(LC.y == '是', 1, ifelse(HCC == '是', 1, 0) # LC&HCC, 0 = censored, 1 = event
                           )) %>% 
  mutate(status.y = ifelse(HCC == '是', 1, 0)) %>% # # HCC, 0 = censored, 1 = event
  filter(folowup1 <= 240 & folowup2 >= 48 & folowup3 >= 48) # 随访长度定义为5年; LC或肝硬化患者的随访时间需超过48周 
         
 table(df$status.x)
 table(df$status.y) 

 ############################################################################### 
 ## 导出肝癌或肝硬化事件患者进行再次确认
 
   hcc <- df %>% filter(status.y == 1) %>% 
   mutate(挂号时间.y = as_date(挂号时间.y),
          挂号时间 = as_date(挂号时间)) 
 
 
 tb <- read_excel('/home/alo/Desktop/HCC患者名单_0916.xlsx', sheet = 1) %>%
    select(姓名, 既往药物:说明)

  hcc <- left_join(hcc, tb, by = "姓名")

  write_csv(hcc, file = '../Output/HCC患者名单_0917.csv')
 
###############################################################################
## 导出肝硬化患者的名单进行再次确认
 
 lchcc <- df %>% filter(status.x == 1) %>% 
   mutate(挂号时间.y = as_date(挂号时间.y),
          挂号时间 = as_date(挂号时间))
 
 # write_csv(lchcc, file = '../Output/LCHCC患者名单_0917.csv') 
  
  lchcc <- lchcc %>% filter(!姓名 %in% hcc$姓名)
  # write_csv(lchcc, file = '../Output/LCHCC患者名单_0917_数据收集.csv')  
 
 ################################################################################
 ## 导出换药的患者
 trans <- df %>% filter(Trans == '是') %>% 
   mutate(挂号时间.y = as_date(挂号时间.y),
          挂号时间 = as_date(挂号时间)) %>% 
   mutate(D30 = D1.x + 30,
          D90 = D1.x + 90,
          D180 = D1.x + 180,
          D360 = D1.x + 360)
 
 # write_csv(trans, file = '../Output/换药患者名单_更新_0916.csv')
  
################################################################################
# 所有数据，去除随访时间较短的患者
  
  others <- df %>% filter(Trans == "否") %>% 
          filter(status.x == 0)
  
  # write.xlsx(others, file = '../Output/非肝癌肝硬化换药患者名单_0917.xlsx', sheetName = '1') 
 
 
 
 
 
