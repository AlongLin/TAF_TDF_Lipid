# 数据分析
## 2022年10月17日
## Desctibtion
## 在合并数据集内进行分析
rm(list = ls())
setwd('~/sambashare/RWorkspace/R/clinical_investigation_2022/Combination_data/')

# 加载程序包

if(!require(tidyverse))require(tidyverse) 

# 导入数据

load("liver_function.dta")
load("hbv.dta")
load("blood.dta")
load("ren_lip.dta")

# 分析肝功能

summary(liver)
summary(hbv)
summary(blood)
summary(ren_lip)

## 排除一下是否存在list的情况

# fn <- which(str_detect(liver$总胆红素, "c")) 
# 
# ## 采用零宽断言 匹配两个标志之间的内容
# ### 代码测试通过
# tb <- liver %>% slice(fn) %>% 
#        mutate(across(总胆红素:前白蛋白, ~str_extract(., "(?<=c\\().*(?=\\,\\s)")))
#        
# dt <- liver %>%  
#   slice(-fn) %>% 
#   bind_rows(tb) %>% 
#   mutate(日期 = as.Date(日期)) %>% 
#   mutate(across(总胆红素:前白蛋白, ~as.numeric(.)))
# liver <- dt

## 考虑全局替换

tb <- liver %>% 
     mutate(总胆红素 = ifelse(str_detect(总胆红素, "c"), str_extract(总胆红素, "(?<=c\\().{1,5}(?=\\,\\s)"), 总胆红素),
            直接胆红素 = ifelse(str_detect(直接胆红素, "c"), str_extract(直接胆红素, "(?<=c\\().{1,5}(?=\\,\\s)"), 直接胆红素),
            间接胆红素 = ifelse(str_detect(间接胆红素, "c"), str_extract(间接胆红素, "(?<=c\\().{1,5}(?=\\,\\s)"), 间接胆红素),
            丙氨酸氨基转移酶 = ifelse(str_detect(丙氨酸氨基转移酶, "c"), str_extract(丙氨酸氨基转移酶, "(?<=c\\().{1,5}(?=\\,\\s)"), 丙氨酸氨基转移酶),
            'γ-谷氨酰转肽酶' = ifelse(str_detect(`γ-谷氨酰转肽酶`, "c"), str_extract(`γ-谷氨酰转肽酶`, "(?<=c\\().{1,5}(?=\\,\\s)"), `γ-谷氨酰转肽酶`),
            碱性磷酸酶 = ifelse(str_detect(碱性磷酸酶, "c"), str_extract(碱性磷酸酶, "(?<=c\\().{1,5}(?=\\,\\s)"), 碱性磷酸酶),
            白蛋白 = ifelse(str_detect(白蛋白, "c"), str_extract(白蛋白, "(?<=c\\().{1,5}(?=\\,\\s)"), 白蛋白),
            球蛋白 = ifelse(str_detect(球蛋白, "c"), str_extract(球蛋白, "(?<=c\\().{1,5}(?=\\,\\s)"), 球蛋白),
            胆碱酯酶 = ifelse(str_detect(胆碱酯酶, "c"), str_extract(胆碱酯酶, "(?<=c\\().{1,5}(?=\\,\\s)"), 胆碱酯酶),
            前白蛋白 = ifelse(str_detect(前白蛋白, "c"), str_extract(前白蛋白, "(?<=c\\().{1,5}(?=\\,\\s)"), 前白蛋白) 
            ) %>% 
            mutate(across(总胆红素:前白蛋白, ~as.numeric(.)))


liver <- tb
summary(liver)

# dt %>% filter(丙氨酸氨基转移酶 == 7322.00)
# dt %>% filter(总胆红素 == 474.90)
# dt %>% filter(患者 == '高国栋')
# 
# summary(dt %>% filter(患者 != '邱绍本') %>% filter(患者 != '李伟') %>% filter(患者 != '王建平'))

hist(log(liver$总胆红素))
boxplot(liver$总胆红素)

# save(liver, file = "./clean_liver.dta")

## 血常规

tb <- blood %>% 
      mutate(白细胞计数 = ifelse(str_detect(白细胞计数, "c"), str_extract(白细胞计数, "(?<=c\\().{1,3}(?=\\,\\s)"), 白细胞计数),
             中性粒细胞计数 = ifelse(str_detect(中性粒细胞计数, "c"), str_extract(中性粒细胞计数, "(?<=c\\().{1,3}(?=\\,\\s)"), 中性粒细胞计数),
             红细胞计数 = ifelse(str_detect(红细胞计数, "c"), str_extract(红细胞计数, "(?<=c\\().{1,3}(?=\\,\\s)"), 红细胞计数),
             血红蛋白 = ifelse(str_detect(血红蛋白, "c"), str_extract(血红蛋白, "(?<=c\\().{1,3}(?=\\,\\s)"), 血红蛋白),
             血小板 = ifelse(str_detect(血小板, "c"), str_extract(血小板, "(?<=c\\().{1,3}(?=\\,\\s)"), 血小板)
             ) %>% 
            mutate(across(白细胞计数:血小板, ~as.numeric(.)))

blood <- tb
summary(blood)
# save(blood, file = "./clean_blood.dta")

## HBV

tb <- hbv %>% 
         mutate(乙型肝炎病毒表面抗原定量 = ifelse(str_detect(乙型肝炎病毒表面抗原定量, "c"), str_extract(乙型肝炎病毒表面抗原定量, "(?<=c\\().*(?=\\,\\s)"), 乙型肝炎病毒表面抗原定量),
         乙型肝炎病毒e抗原检测 = ifelse(str_detect(乙型肝炎病毒e抗原检测, "c"), str_extract(乙型肝炎病毒e抗原检测, "(?<=c\\().*(?=\\,\\s)"), 乙型肝炎病毒e抗原检测),
         乙型肝炎病毒e抗体检测 = ifelse(str_detect(乙型肝炎病毒e抗体检测, "c"), str_extract(乙型肝炎病毒e抗体检测, "(?<=c\\().*(?=\\,\\s)"), 乙型肝炎病毒e抗体检测),
         HBVDNA定量 = ifelse(str_detect(HBVDNA定量, "c"), str_extract(HBVDNA定量, "(?<=c\\().*(?=\\,\\s)"), HBVDNA定量)
          ) %>% 
        mutate(across(乙型肝炎病毒表面抗原定量:HBVDNA定量 , ~as.numeric(.)))


hbv <- tb
summary(hbv)
# save(hbv, file = "./clean_hbv.dta")


## ren_lip

tb <- ren_lip %>% 
  mutate(甘油三酯 = ifelse(str_detect(甘油三酯, "c"), str_extract(甘油三酯, "(?<=c\\().*(?=\\,\\s)"), 甘油三酯),
         总胆固醇 = ifelse(str_detect(总胆固醇, "c"), str_extract(总胆固醇, "(?<=c\\().*(?=\\,\\s)"), 总胆固醇),
         高密度脂蛋白 = ifelse(str_detect(高密度脂蛋白, "c"), str_extract(高密度脂蛋白, "(?<=c\\().*(?=\\,\\s)"), 高密度脂蛋白),
         低密度脂蛋白 = ifelse(str_detect(低密度脂蛋白, "c"), str_extract(低密度脂蛋白, "(?<=c\\().*(?=\\,\\s)"), 低密度脂蛋白),
         血肌酐 = ifelse(str_detect(血肌酐, "c"), str_extract(血肌酐, "(?<=c\\().*(?=\\,\\s)"), 血肌酐),
         尿素 = ifelse(str_detect(尿素, "c"), str_extract(尿素, "(?<=c\\().*(?=\\,\\s)"), 尿素)
  ) %>% 
  mutate(across(甘油三酯:尿素 , ~as.numeric(.)))


ren_lip <- tb
summary(ren_lip)
# save(ren_lip, file = "./clean_ren_lip.dta")


## 分析

## 提取时间点数据
## 提取名单：
## 初诊抗病毒治疗时间点，换药的时间，并按照上下7天的范围，来分别提取治疗后1个月、
## 治疗后3个月、治疗后6个月和治疗后12个月的患者时间范围（加减7天）

if(!require(tidyverse))require(tidyverse)
if(!require(xlsx))require(xlsx)
if(!require(readxl))require(readxl)

setwd('~/sambashare/RWorkspace/R/clinical_investigation_2022/Combination_data/Patient_info/')

fn <- dir()
fn


Hcc <- read_xlsx(fn[2], sheet = 1) %>% 
       filter(是否保留 == 'Y') %>% 
       select(1:37) %>% 
       mutate(across(.cols = everything(), ~as.character(.)))

LcHcc <- read_xlsx(fn[3], sheet = 1) %>% 
         filter(grp != 'Trans') %>% 
         mutate(across(.cols = everything(), ~as.character(.)))
  

noLcHcc <- read_xlsx(fn[5], sheet = 1) %>% 
       select(-1) %>% 
       mutate(across(.cols = everything(), ~as.character(.)))

df1 <- bind_rows(Hcc, LcHcc) %>% 
       bind_rows(noLcHcc) %>% 
       mutate(D1.x = as.Date(D1.x)) %>% 
       mutate(D30 = D1.x + 30,
             D90 = D1.x + 90,
             D180 = D1.x + 180,
             D360 = D1.x + 360) %>% 
       select(姓名, 性别.x, 年龄.x, D1.x, D30, D90,
                D180, D360,folowup1, folowup2, folowup3, grp, status.x, status.y) %>% 
       mutate(across(.cols = everything(), ~as.character(.)))

trans <- read_csv(fn[4]) %>%
  # filter(姓名!= '陈芯') %>%  # 患者：陈芯，发生了肝癌
  select(names(df1)) %>% 
# mutate(across(D1.y:D2.y, ~as.Date(.))) %>% 
mutate(across(.cols = everything(), ~as.character(.))) 

patient_info <- df1 %>% bind_rows(trans) 

head(patient_info, 10)

# save(patient_info, file = "./Combination_data.Rdata")






