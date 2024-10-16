# 时间区间数据提取
# 2022年10月18日

rm(list = ls())
# setwd("~/sambashare/RWorkspace/R/clinical_investigation_2022/Combination_data/")
setwd("~/Rspace/clinical_investigation_2022/Combination_data/") # On server

# 读入数据

load("./Patient_info/Combination_data.Rdata")

# 添加第3年和第5年的数据

summary(patient_info) 

patient_info <- patient_info %>% 
      mutate(D720 = as.character(as.Date(D1.x) + 720),
             D360X3 = as.character(as.Date(D1.x) + 1080),
             D360X5 = as.character(as.Date(D1.x) + 1800)) 

# 计算区间

################################################################################
## 使用tidyverse 体系无法完成工作
## 错误显示：longer object length is not a multiple of shorter object length

# data = patient_info %>%
#      mutate(Duration = paste0(as.character(as.Date(D1.x) + (-7:7)), collapse = ','))
# 
# patient_info$Duration <- paste0(as.character(as.Date(patient_info$D1.x) + (-7:7)), collapse = ',')


################################################################################
##采用tibble 做容器的问题是，在for循环之后，变量的顺序不知道为何发生了变化。

# fn = patient_info$D30 # 提取时间点，我相信这里的顺序应该没后发生改变的
#  
# Duration <- tibble()
# 
# for(i in 1:length(fn)) { # 这样的写法是否能够保证顺序没有发生变化？
#   
#   tb = as.character(as.Date(fn[i]) + (-7:7)) %>% 
#   paste0(collapse = ",") 
#   names(tb) <- "Duration"
#   Duration <- tb  %>% 
#   bind_rows(Duration)
#   print(paste(i,":",fn[i]))
#   
# }

################################################################################

patient_info <- patient_info %>% rename('D1' = D1.x)

fn = patient_info$D1 # 提取时间 d = 1

Duration01 = ls() # 以list做为字符串的容器

for(i in 1:length(fn)) {
  
  Duration01[i] = as.character(as.Date(fn[i]) + (-14:7)) %>% 
    paste0(collapse = ",")
  
  print(paste(i,":",fn[i]))
}

Duration01 = tibble(Duration01)

################################################################################

fn = patient_info$D30 # 提取时间 d = 30

Duration30 = ls() # 以list做为字符串的容器

for(i in 1:length(fn)) {
  
  Duration30[i] = as.character(as.Date(fn[i]) + (-7:7)) %>% 
    paste0(collapse = ",")
 
   print(paste(i,":",fn[i]))
}

Duration30 = tibble(Duration30)

################################################################################

fn = patient_info$D90 # 提取时间 d = 90

Duration90 = ls() # 以list做为字符串的容器

for(i in 1:length(fn)) {
  
  Duration90[i] = as.character(as.Date(fn[i]) + (-7:7)) %>% 
    paste0(collapse = ",")
  
  print(paste(i,":",fn[i]))
}

Duration90 = tibble(Duration90)

################################################################################

fn = patient_info$D180 # 提取时间 d = 180

Duration180 = ls() # 以list做为字符串的容器

for(i in 1:length(fn)) {
  
  Duration180[i] = as.character(as.Date(fn[i]) + (-30:30)) %>% 
    paste0(collapse = ",")
  
  print(paste(i,":",fn[i]))
}

Duration180 = tibble(Duration180)


################################################################################

fn = patient_info$D360 # 提取时间 d = 360

Duration360 = ls() # 以list做为字符串的容器

for(i in 1:length(fn)) {
  
  Duration360[i] = as.character(as.Date(fn[i]) + (-60:90)) %>% 
    paste0(collapse = ",")
  
  print(paste(i,":",fn[i]))
}

Duration360 = tibble(Duration360)

################################################################################

fn = patient_info$D720 # 提取时间 d = 720

Duration720 = ls() # 以list做为字符串的容器

for(i in 1:length(fn)) {
  
  Duration720[i] = as.character(as.Date(fn[i]) + (-90:90)) %>% 
    paste0(collapse = ",")
  
  print(paste(i,":",fn[i]))
}

Duration720 = tibble(Duration720)

################################################################################

fn = patient_info$D360X3 # 提取时间 d = 1080

Duration360X3 = ls() # 以list做为字符串的容器

for(i in 1:length(fn)) {
  
  Duration360X3[i] = as.character(as.Date(fn[i]) + (-120:120)) %>% 
    paste0(collapse = ",")
  
  print(paste(i,":",fn[i]))
}

Duration360X3 = tibble(Duration360X3)

################################################################################

fn = patient_info$D360X5 # 提取时间 d = 1800

Duration360X5 = ls() # 以list做为字符串的容器

for(i in 1:length(fn)) {
  
  Duration360X5[i] = as.character(as.Date(fn[i]) + (-120:120)) %>% 
    paste0(collapse = ",")
  
  print(paste(i,":",fn[i]))
}

Duration360X5 = tibble(Duration360X5)

#######此处代码块后面能用得上##################################################
# data <- patient_info %>%
#         bind_cols(Duration) %>%  slice(1:20) %>% 
#         mutate(D30.x = ifelse(if_any(D30, ~str_detect(Duration, .)), 'Y',
#                'N'))
# table(data$D30.x)
# data %>% filter(D30.x == 'Y')
###############################################################################

# 合并数据
data <- patient_info %>%
  bind_cols(Duration01) %>%
  bind_cols(Duration30) %>% 
  bind_cols(Duration90) %>% 
  bind_cols(Duration180) %>%
  bind_cols(Duration360) %>% 
  bind_cols(Duration720) %>%
  bind_cols(Duration360X3) %>% 
  bind_cols(Duration360X5)

save(data, file = "patient_info_duration.dta")



