## Extraction Update
##> 2023年12月02日
##> 1. 基线数据不能缺少血常规及凝血的指标
##> 2. 既然血脂是主要的研究指标，应该保证其数据的完整性和真实性。

rm(list = ls())
# 加载包
if(!require(tidyverse))require(tidyverse)

# 设置路径

# setwd("~/sambashare/RWorkspace/R/clinical_investigation_2022/")
setwd("~/Rspace/clinical_investigation_2022/")

# 加载数据

load("./Combination_data/patient_info_duration.dta")
load("./Combination_data/clean_blood.dta")
load("./Combination_data/clean_hbv.dta")
load("./Combination_data/clean_liver.dta")
load("./Combination_data/clean_ren_lip.dta")

################################################################################
# 提取血常规

d01 <- blood %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D01.x = ifelse(if_any(日期, ~str_detect(Duration01, .)), 'Y',
                        'N')) %>% 
  ungroup() %>% 
  filter(D01.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D01") %>% 
  select(-D01.x)


d30 <- blood %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D30.x = ifelse(if_any(日期, ~str_detect(Duration30, .)), 'Y',
                        'N')) %>% 
  ungroup() %>% 
  filter(D30.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup()%>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D30")%>% 
  select(-D30.x)

d90 <- blood %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D90.x = ifelse(if_any(日期, ~str_detect(Duration90, .)), 'Y',
                        'N')) %>% 
  ungroup() %>% 
  filter(D90.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup()%>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D90")%>% 
  select(-D90.x)

d180 <- blood %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D180.x = ifelse(if_any(日期, ~str_detect(Duration180, .)), 'Y',
                         'N')) %>% 
  ungroup() %>% 
  filter(D180.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup()%>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D180")%>% 
  select(-D180.x)

d360 <- blood %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D360.x = ifelse(if_any(日期, ~str_detect(Duration360, .)), 'Y',
                         'N')) %>% 
  ungroup() %>% 
  filter(D360.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D360")%>% 
  select(-D360.x)

d720 <- blood %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D720.x = ifelse(if_any(日期, ~str_detect(Duration720, .)), 'Y',
                         'N')) %>% 
  ungroup() %>% 
  filter(D720.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D720")%>% 
  select(-D720.x)

d1080 <- blood %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D1080.x = ifelse(if_any(日期, ~str_detect(Duration360X3, .)), 'Y',
                          'N')) %>% 
  ungroup() %>% 
  filter(D1080.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D1080")%>% 
  select(-D1080.x)

d1800 <- blood %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D1800.x = ifelse(if_any(日期, ~str_detect(Duration360X5, .)), 'Y',
                          'N')) %>% 
  ungroup() %>% 
  filter(D1800.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D1800")%>% 
  select(-D1800.x)

## 合并数据

final_blood <- d01 %>%
  bind_rows(d30) %>%
  bind_rows(d90) %>%
  bind_rows(d180) %>%
  bind_rows(d360) %>% 
  bind_rows(d720) %>%
  bind_rows(d1080) %>% 
  bind_rows(d1800)


### 第1天至第360天完整数据
blood_dt1 <- final_blood %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D720' & Time != 'D1080' & Time != 'D1800') %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 5) %>% 
  ungroup()

### 第1天、第30天、第180天和第360天完整数据   
blood_dt2 <- final_blood %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D90' & Time != 'D720' & Time != 'D1080' & Time != 'D1800') %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 4) %>% 
  ungroup()

### 第1天、第180天和第360天完整数据   
blood_dt3 <- final_blood %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' &Time != 'D90' & Time != 'D720' & Time != 'D1080' & Time != 'D1800') %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 3) %>% 
  ungroup()   

### 第1天和30天完整数据   
blood_dt4 <- final_blood %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D90' & Time != 'D180' & Time != 'D360' & Time != 'D720' & Time != 'D1080' & Time != 'D1800' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup()   

### 第1天和90天完整数据   
blood_dt5 <- final_blood %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' & Time != 'D180' & Time != 'D360' & Time != 'D720' & Time != 'D1080' & Time != 'D1800' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup()   

### 第1天和180天完整数据   
blood_dt6 <- final_blood %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' & Time != 'D90' & Time != 'D360' & Time != 'D720' & Time != 'D1080' & Time != 'D1800' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup()  

### 第1天和360天完整数据   
blood_dt7 <- final_blood %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' & Time != 'D90' & Time != 'D180' & Time != 'D720' & Time != 'D1080' & Time != 'D1800' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup() 

### 第1天和1080天完整数据   
blood_dt8 <- final_blood %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' & Time != 'D90' & Time != 'D180' & Time != 'D360' & Time != 'D720' & Time != 'D1800' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup() 

### 第1天和1800天完整数据   
blood_dt9 <- final_blood %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' & Time != 'D90' & Time != 'D180' & Time != 'D360' & Time != 'D720' & Time != 'D1080' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup()

### 第1天和720天完整数据   
blood_dt10 <- final_blood %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' & Time != 'D90' & Time != 'D180' & Time != 'D360' & Time != 'D1080' & Time != 'D1800' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup() 




################################################################################
# 提取肝功能

d01 <- liver %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D01.x = ifelse(if_any(日期, ~str_detect(Duration01, .)), 'Y',
                        'N')) %>% 
  ungroup() %>% 
  filter(D01.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D01") %>% 
  select(-D01.x)


d30 <- liver %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D30.x = ifelse(if_any(日期, ~str_detect(Duration30, .)), 'Y',
                        'N')) %>% 
  ungroup() %>% 
  filter(D30.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup()%>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D30")%>% 
  select(-D30.x)

d90 <- liver %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D90.x = ifelse(if_any(日期, ~str_detect(Duration90, .)), 'Y',
                        'N')) %>% 
  ungroup() %>% 
  filter(D90.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup()%>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D90")%>% 
  select(-D90.x)

d180 <- liver %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D180.x = ifelse(if_any(日期, ~str_detect(Duration180, .)), 'Y',
                         'N')) %>% 
  ungroup() %>% 
  filter(D180.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup()%>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D180")%>% 
  select(-D180.x)

d360 <- liver %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D360.x = ifelse(if_any(日期, ~str_detect(Duration360, .)), 'Y',
                         'N')) %>% 
  ungroup() %>% 
  filter(D360.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D360")%>% 
  select(-D360.x)

d720 <- liver %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D720.x = ifelse(if_any(日期, ~str_detect(Duration720, .)), 'Y',
                         'N')) %>% 
  ungroup() %>% 
  filter(D720.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D720")%>% 
  select(-D720.x)

d1080 <- liver %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D1080.x = ifelse(if_any(日期, ~str_detect(Duration360X3, .)), 'Y',
                          'N')) %>% 
  ungroup() %>% 
  filter(D1080.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D1080")%>% 
  select(-D1080.x)

d1800 <- liver %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D1800.x = ifelse(if_any(日期, ~str_detect(Duration360X5, .)), 'Y',
                          'N')) %>% 
  ungroup() %>% 
  filter(D1800.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D1800")%>% 
  select(-D1800.x)

## 合并数据

final_liver <- d01 %>%
  bind_rows(d30) %>%
  bind_rows(d90) %>%
  bind_rows(d180) %>%
  bind_rows(d360) %>% 
  bind_rows(d720) %>%
  bind_rows(d1080) %>% 
  bind_rows(d1800)


### 第1天至第360天完整数据
liver_dt1 <- final_liver %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D720' & Time != 'D1080' & Time != 'D1800') %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 5) %>% 
  ungroup()

### 第1天、第30天、第180天和第360天完整数据   
liver_dt2 <- final_liver %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D90' & Time != 'D720' & Time != 'D1080' & Time != 'D1800') %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 4) %>% 
  ungroup()

### 第1天、第180天和第360天完整数据   
liver_dt3 <- final_liver %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' &Time != 'D90' & Time != 'D720' & Time != 'D1080' & Time != 'D1800') %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 3) %>% 
  ungroup()   

### 第1天和30天完整数据   
liver_dt4 <- final_liver %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D90' & Time != 'D180' & Time != 'D360' & Time != 'D720' & Time != 'D1080' & Time != 'D1800' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup()   

### 第1天和90天完整数据   
liver_dt5 <- final_liver %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' & Time != 'D180' & Time != 'D360' & Time != 'D720' & Time != 'D1080' & Time != 'D1800' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup()   

### 第1天和180天完整数据   
liver_dt6 <- final_liver %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' & Time != 'D90' & Time != 'D360' & Time != 'D720' & Time != 'D1080' & Time != 'D1800' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup()  

### 第1天和360天完整数据   
liver_dt7 <- final_liver %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' & Time != 'D90' & Time != 'D180' & Time != 'D720' & Time != 'D1080' & Time != 'D1800' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup() 

### 第1天和1080天完整数据   
liver_dt8 <- final_liver %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' & Time != 'D90' & Time != 'D180' & Time != 'D360' & Time != 'D720' & Time != 'D1800' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup() 

### 第1天和1800天完整数据   
liver_dt9 <- final_liver %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' & Time != 'D90' & Time != 'D180' & Time != 'D360' & Time != 'D720' & Time != 'D1080' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup()

### 第1天和720天完整数据   
liver_dt10 <- final_liver %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' & Time != 'D90' & Time != 'D180' & Time != 'D360' & Time != 'D1080' & Time != 'D1800' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup() 


################################################################################   
# 提取HBV数据

d01 <- hbv %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D01.x = ifelse(if_any(日期, ~str_detect(Duration01, .)), 'Y',
                        'N')) %>% 
  ungroup() %>% 
  filter(D01.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D01") %>% 
  select(-D01.x)


d30 <- hbv %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D30.x = ifelse(if_any(日期, ~str_detect(Duration30, .)), 'Y',
                        'N')) %>% 
  ungroup() %>% 
  filter(D30.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup()%>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D30")%>% 
  select(-D30.x)

d90 <- hbv %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D90.x = ifelse(if_any(日期, ~str_detect(Duration90, .)), 'Y',
                        'N')) %>% 
  ungroup() %>% 
  filter(D90.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup()%>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D90")%>% 
  select(-D90.x)

d180 <- hbv %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D180.x = ifelse(if_any(日期, ~str_detect(Duration180, .)), 'Y',
                         'N')) %>% 
  ungroup() %>% 
  filter(D180.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup()%>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D180")%>% 
  select(-D180.x)

d360 <- hbv %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D360.x = ifelse(if_any(日期, ~str_detect(Duration360, .)), 'Y',
                         'N')) %>% 
  ungroup() %>% 
  filter(D360.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D360")%>% 
  select(-D360.x)

d720 <- hbv %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D720.x = ifelse(if_any(日期, ~str_detect(Duration720, .)), 'Y',
                         'N')) %>% 
  ungroup() %>% 
  filter(D720.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D720")%>% 
  select(-D720.x)

d1080 <- hbv %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D1080.x = ifelse(if_any(日期, ~str_detect(Duration360X3, .)), 'Y',
                          'N')) %>% 
  ungroup() %>% 
  filter(D1080.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D1080")%>% 
  select(-D1080.x)

d1800 <- hbv %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D1800.x = ifelse(if_any(日期, ~str_detect(Duration360X5, .)), 'Y',
                          'N')) %>% 
  ungroup() %>% 
  filter(D1800.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D1800")%>% 
  select(-D1800.x)

## 合并数据

final_hbv <- d01 %>%
  bind_rows(d30) %>%
  bind_rows(d90) %>%
  bind_rows(d180) %>%
  bind_rows(d360) %>% 
  bind_rows(d720) %>%
  bind_rows(d1080) %>% 
  bind_rows(d1800)

### 第1天至第360天完整数据
hbv_dt1 <- final_hbv %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D720' & Time != 'D1080' & Time != 'D1800') %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 5) %>% 
  ungroup()

### 第1天、第30天、第180天和第360天完整数据   
hbv_dt2 <- final_hbv %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D90' & Time != 'D720' & Time != 'D1080' & Time != 'D1800') %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 4) %>% 
  ungroup()

### 第1天、第180天和第360天完整数据   
hbv_dt3 <- final_hbv %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' &Time != 'D90' & Time != 'D720' & Time != 'D1080' & Time != 'D1800') %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 3) %>% 
  ungroup()   

### 第1天和30天完整数据   
hbv_dt4 <- final_hbv %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D90' & Time != 'D180' & Time != 'D360' & Time != 'D720' & Time != 'D1080' & Time != 'D1800' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup()   

### 第1天和90天完整数据   
hbv_dt5 <- final_hbv %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' & Time != 'D180' & Time != 'D360' & Time != 'D720' & Time != 'D1080' & Time != 'D1800' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup()   

### 第1天和180天完整数据   
hbv_dt6 <- final_hbv %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' & Time != 'D90' & Time != 'D360' & Time != 'D720' & Time != 'D1080' & Time != 'D1800' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup()  

### 第1天和360天完整数据   
hbv_dt7 <- final_hbv %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' & Time != 'D90' & Time != 'D180' & Time != 'D720' & Time != 'D1080' & Time != 'D1800' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup() 

### 第1天和1080天完整数据   
hbv_dt8 <- final_hbv %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' & Time != 'D90' & Time != 'D180' & Time != 'D360' & Time != 'D720' & Time != 'D1800' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup() 

### 第1天和1800天完整数据   
hbv_dt9 <- final_hbv %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' & Time != 'D90' & Time != 'D180' & Time != 'D360' & Time != 'D720' & Time != 'D1080' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup()

### 第1天和720天完整数据   
hbv_dt10 <- final_hbv %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' & Time != 'D90' & Time != 'D180' & Time != 'D360' & Time != 'D1080' & Time != 'D1800' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup() 


################################################################################   
# 提取ren_lip数据

d01 <- ren_lip %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D01.x = ifelse(if_any(日期, ~str_detect(Duration01, .)), 'Y',
                        'N')) %>% 
  ungroup() %>% 
  filter(D01.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D01") %>% 
  select(-D01.x)


d30 <- ren_lip %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D30.x = ifelse(if_any(日期, ~str_detect(Duration30, .)), 'Y',
                        'N')) %>% 
  ungroup() %>% 
  filter(D30.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup()%>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D30")%>% 
  select(-D30.x)

d90 <- ren_lip %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D90.x = ifelse(if_any(日期, ~str_detect(Duration90, .)), 'Y',
                        'N')) %>% 
  ungroup() %>% 
  filter(D90.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup()%>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D90")%>% 
  select(-D90.x)

d180 <- ren_lip %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D180.x = ifelse(if_any(日期, ~str_detect(Duration180, .)), 'Y',
                         'N')) %>% 
  ungroup() %>% 
  filter(D180.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup()%>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D180")%>% 
  select(-D180.x)

d360 <- ren_lip %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D360.x = ifelse(if_any(日期, ~str_detect(Duration360, .)), 'Y',
                         'N')) %>% 
  ungroup() %>% 
  filter(D360.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D360")%>% 
  select(-D360.x)

d720 <- ren_lip %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D720.x = ifelse(if_any(日期, ~str_detect(Duration720, .)), 'Y',
                         'N')) %>% 
  ungroup() %>% 
  filter(D720.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D720")%>% 
  select(-D720.x)

d1080 <- ren_lip %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D1080.x = ifelse(if_any(日期, ~str_detect(Duration360X3, .)), 'Y',
                          'N')) %>% 
  ungroup() %>% 
  filter(D1080.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D1080")%>% 
  select(-D1080.x)

d1800 <- ren_lip %>% 
  rename("姓名" = 患者) %>% 
  left_join(data, by = "姓名") %>% 
  mutate(日期 = as.Date(日期)) %>% 
  mutate(日期 = as.character(日期)) %>% 
  group_by(姓名) %>% 
  mutate(D1800.x = ifelse(if_any(日期, ~str_detect(Duration360X5, .)), 'Y',
                          'N')) %>% 
  ungroup() %>% 
  filter(D1800.x == "Y") %>% 
  group_by(姓名) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c(D1:Duration360X5)) %>% 
  mutate(Time = "D1800")%>% 
  select(-D1800.x)

## 合并数据

final_ren_lip <- d01 %>%
  bind_rows(d30) %>%
  bind_rows(d90) %>%
  bind_rows(d180) %>%
  bind_rows(d360) %>% 
  bind_rows(d720) %>% 
  bind_rows(d1080) %>% 
  bind_rows(d1800)

### 第1天至第360天完整数据
ren_lip_dt1 <- final_ren_lip %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D720' & Time != 'D1080' & Time != 'D1800') %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 5) %>% 
  ungroup()

### 第1天、第30天、第180天和第360天完整数据   
ren_lip_dt2 <- final_ren_lip %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D90' & Time != 'D720' & Time != 'D1080' & Time != 'D1800') %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 4) %>% 
  ungroup()

### 第1天、第180天和第360天完整数据   
ren_lip_dt3 <- final_ren_lip %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' &Time != 'D90' & Time != 'D720' & Time != 'D1080' & Time != 'D1800') %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 3) %>% 
  ungroup()   

### 第1天和30天完整数据   
ren_lip_dt4 <- final_ren_lip %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D90' & Time != 'D180' & Time != 'D360' & Time != 'D720' & Time != 'D1080' & Time != 'D1800' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup()   

### 第1天和90天完整数据   
ren_lip_dt5 <- final_ren_lip %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' & Time != 'D180' & Time != 'D360' & Time != 'D720' & Time != 'D1080' & Time != 'D1800' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup()   

### 第1天和180天完整数据   
ren_lip_dt6 <- final_ren_lip %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' & Time != 'D90' & Time != 'D360' & Time != 'D720' & Time != 'D1080' & Time != 'D1800' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup()  

### 第1天和360天完整数据   
ren_lip_dt7 <- final_ren_lip %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' & Time != 'D90' & Time != 'D180' & Time != 'D720' & Time != 'D1080' & Time != 'D1800' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup() 

### 第1天和1080天完整数据   
ren_lip_dt8 <- final_ren_lip %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' & Time != 'D90' & Time != 'D180' & Time != 'D360' & Time != 'D720' & Time != 'D1800' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup() 

### 第1天和1800天完整数据   
ren_lip_dt9 <- final_ren_lip %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' & Time != 'D90' & Time != 'D180' & Time != 'D360' & Time != 'D720' & Time != 'D1080' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup()

### 第1天和720天完整数据   
ren_lip_dt10 <- final_ren_lip %>% 
  mutate(Time = factor(Time, levels = c('D01', 'D30', 'D90', 'D180', 'D360', 'D720', 'D1080', 'D1800'))) %>% 
  filter(Time != 'D30' & Time != 'D90' & Time != 'D180' & Time != 'D360' & Time != 'D1080' & Time != 'D1800' ) %>% 
  arrange(姓名) %>% 
  group_by(姓名) %>% 
  filter(n() == 2) %>% 
  ungroup() 


###############################################################################
## 做流行病调查的数据是否不应该进行填补？
### 合并第1天和第360天完整数据

# tb <- hbv_dt7 %>% 
#   select(姓名, 性别.x:Time, 乙型肝炎病毒表面抗原定量:HBVDNA定量) %>% 
#   inner_join(liver_dt7 |> select(姓名, 性别.x:Time, 总胆红素:前白蛋白), 
#              by = c("姓名", '性别.x', '年龄.x', 'Time')) %>% 
#   inner_join(ren_lip_dt7 |> select(姓名, 性别.x:Time, 甘油三酯:尿素), 
#              by = c("姓名", '性别.x', '年龄.x', 'Time')) %>% 
#   # inner_join(blood_dt7 |> select(姓名, 性别.x:Time, 白细胞计数:血小板),
#   # by = c("姓名", '性别.x', '年龄.x', 'Time')) %>% 
#   rename(性别 = 性别.x, 
#          年龄 = 年龄.x) %>% 
#   mutate(性别 = factor(性别, levels = c("男", '女')),
#          年龄 = as.numeric(年龄),
#          乙型肝炎病毒e抗原检测 = ifelse(!is.na(乙型肝炎病毒表面抗原定量) & is.na(乙型肝炎病毒e抗原检测), 0, 乙型肝炎病毒e抗原检测), # 不可能只查HBsAg
#          乙型肝炎病毒e抗体检测 = ifelse(!is.na(乙型肝炎病毒表面抗原定量) & is.na(乙型肝炎病毒e抗体检测), 0, 乙型肝炎病毒e抗体检测))  
# %>% 
#          filter(!if_any(乙型肝炎病毒e抗原检测:乙型肝炎病毒e抗体检测, is.na))  # 删除无乙肝两对半数据患者


tb <- ren_lip_dt7 %>%
  select(姓名, 性别.x:Time, 甘油三酯:尿素) %>%
  left_join(hbv_dt7 |> select(姓名, 性别.x:Time, 乙型肝炎病毒表面抗原定量:HBVDNA定量),
             by = c("姓名", '性别.x', '年龄.x', 'Time')) %>%
  left_join(liver_dt7 |> select(姓名, 性别.x:Time, 总胆红素:前白蛋白),
             by = c("姓名", '性别.x', '年龄.x', 'Time')) %>%
  # left_join(blood_dt7 |> select(姓名, 性别.x:Time, 白细胞计数:血小板),
  # by = c("姓名", '性别.x', '年龄.x', 'Time')) %>%
  rename(性别 = 性别.x,
         年龄 = 年龄.x) %>%
  mutate(性别 = factor(性别, levels = c("男", '女')),
         年龄 = as.numeric(年龄),
         乙型肝炎病毒e抗原检测 = ifelse(!is.na(乙型肝炎病毒表面抗原定量) & is.na(乙型肝炎病毒e抗原检测), 0, 乙型肝炎病毒e抗原检测), # 不可能只查HBsAg
         乙型肝炎病毒e抗体检测 = ifelse(!is.na(乙型肝炎病毒表面抗原定量) & is.na(乙型肝炎病毒e抗体检测), 0, 乙型肝炎病毒e抗体检测))

summary(tb)

## 缺失数据分析

if(!require(naniar))require(naniar)

tb %>% 
  miss_var_summary()

## 缺失值主要集中在前白蛋白和胆碱酯酶，而其他的指标均为1~2个的缺失值

## 插补确实值

if(!require(mice))require(mice)

## 插补前将实验室检查数据与随访数据合并

df <- data %>% select(姓名,folowup1:status.y) %>% right_join(tb, by = "姓名") %>% 
  select(姓名, 性别:Time, folowup1:status.y, 甘油三酯:前白蛋白) %>% ## 调整列的位置
  mutate(across(folowup1:folowup3, as.numeric)) %>% 
  mutate(across(grp:status.y, ~as.factor(.))) %>% 
  rename(谷氨酰转肽酶 = `γ-谷氨酰转肽酶`) %>% 
  mutate(Time = fct_drop(Time))

## Multiple imputation
# see all the default settings for imputation                                                         
impu_default <- mice(df, maxit = 0)
summary(impu_default)

# see the predictor structure
pred <- quickpred(df, exclude = c('NAME'))
pred

# imputation method
meth <- impu_default$meth
meth    # imputation method can be changed manually
## 关于插补的方法，参考张敬信老师的教程
meth['碱性磷酸酶'] <- "rf"
meth['谷氨酰转肽酶'] <- "rf"
meth['丙氨酸氨基转移酶'] <- "rf"
meth['天门冬氨酸氨基转移酶'] <- "rf"  # Change the method 加权预测均值匹配
meth['HBVDNA定量'] <- "rf" 
meth['胆碱酯酶'] <- "rf"
meth['白蛋白'] <- "rf"
meth['球蛋白'] <- "rf"
meth['前白蛋白'] <- "rf"
meth['总胆固醇'] <- "rf"
meth['高密度脂蛋白'] <- "rf"
meth['乙型肝炎病毒表面抗原定量'] <- "rf"
meth['乙型肝炎病毒e抗原检测'] <- "rf"

K <- 100 # 100 times 

df_impu <- vector(K,mode="list") 


imputation_100 <- mice(df, maxit = 25, m = K, seed = 1234, pred = pred, meth = meth, print = TRUE)

for (i in 1:K) {
  df_impu[[i]] <- mice::complete(imputation_100, i)
}


## 补全后数据, 取第66次结果，用于补全数据     
dt  <- df_impu[[66]]

dt %>% 
  miss_var_summary()

save(df_impu, file = "./Output/df_impu_1_360_231202.dta")


###############################################################################
### 合并第1天和第720天完整数据

tb <- ren_lip_dt10 %>%
  select(姓名, 性别.x:Time, 甘油三酯:尿素) %>%
  left_join(hbv_dt10 |> select(姓名, 性别.x:Time, 乙型肝炎病毒表面抗原定量:HBVDNA定量),
            by = c("姓名", '性别.x', '年龄.x', 'Time')) %>%
  left_join(liver_dt10 |> select(姓名, 性别.x:Time, 总胆红素:前白蛋白),
            by = c("姓名", '性别.x', '年龄.x', 'Time')) %>%
  # left_join(blood_dt7 |> select(姓名, 性别.x:Time, 白细胞计数:血小板),
  # by = c("姓名", '性别.x', '年龄.x', 'Time')) %>%
  rename(性别 = 性别.x,
         年龄 = 年龄.x) %>%
  mutate(性别 = factor(性别, levels = c("男", '女')),
         年龄 = as.numeric(年龄),
         乙型肝炎病毒e抗原检测 = ifelse(!is.na(乙型肝炎病毒表面抗原定量) & is.na(乙型肝炎病毒e抗原检测), 0, 乙型肝炎病毒e抗原检测), # 不可能只查HBsAg
         乙型肝炎病毒e抗体检测 = ifelse(!is.na(乙型肝炎病毒表面抗原定量) & is.na(乙型肝炎病毒e抗体检测), 0, 乙型肝炎病毒e抗体检测))

summary(tb)

## 缺失数据分析

if(!require(naniar))require(naniar)

tb %>% 
  miss_var_summary()

## 缺失值主要集中在前白蛋白和胆碱酯酶，而其他的指标均为1~2个的缺失值

## 插补确实值

if(!require(mice))require(mice)

## 插补前将实验室检查数据与随访数据合并

df <- data %>% select(姓名,folowup1:status.y) %>% right_join(tb, by = "姓名") %>% 
  select(姓名, 性别:Time, folowup1:status.y, 甘油三酯:前白蛋白) %>% ## 调整列的位置
  mutate(across(folowup1:folowup3, as.numeric)) %>% 
  mutate(across(grp:status.y, ~as.factor(.))) %>% 
  rename(谷氨酰转肽酶 = `γ-谷氨酰转肽酶`) %>% 
  mutate(Time = fct_drop(Time))

## Multiple imputation
# see all the default settings for imputation                                                         
impu_default <- mice(df, maxit = 0)
summary(impu_default)

# see the predictor structure
pred <- quickpred(df, exclude = c('NAME', 'folowup3'))
pred

# imputation method
meth <- impu_default$meth
meth    # imputation method can be changed manually
# meth['HBVDNA定量'] <- "rf"  # Change the method from "pmm" to "cart"
# meth['胆碱酯酶'] <- "rf"
meth['前白蛋白'] <- "rf"
meth['白蛋白'] <- "rf"
meth['球蛋白'] <- "rf"
# meth['高密度脂蛋白'] <- "rf"
meth['乙型肝炎病毒表面抗原定量'] <- "rf"
meth['乙型肝炎病毒e抗原检测'] <- "rf"
meth['天门冬氨酸氨基转移酶'] <- "rf"
meth['丙氨酸氨基转移酶'] <- "rf"
# meth['乙型肝炎病毒e抗体检测'] <- "rf"
meth['总胆固醇'] <- "rf"

K <- 100 # 100 times 

df_impu <- vector(K,mode="list") 


imputation_100 <- mice(df, maxit = 25, m = K, seed = 1234, pred = pred, meth = meth, print = TRUE)

for (i in 1:K) {
  df_impu[[i]] <- mice::complete(imputation_100, i)
}


## 补全后数据, 取第66次结果，用于补全数据     
dt  <- df_impu[[66]]

dt %>% 
  miss_var_summary()

save(df_impu, file = "./Output/df_impu_1_720_231202.dta")
