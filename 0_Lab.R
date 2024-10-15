#  批量导入实验室检查数据
# Date:2022-10-10
# 步骤1，修改中文编码。采用vim批量修改，这是由于文件中windows转移到linux中，不可
# 避免会出现的问题；
# 步骤2，采用小批量数据进行函数调试，使用之前所采用的函数，坚持从项目入手进行数据
# 分析学习。
# 步骤3， 批量导入数据
# 步骤4, 数据分析，我觉得需要使用ungroup函数来进行问题的解决

# 设置路径
setwd("~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/TDF_TAF/额外/")

# 加载包

require(tidyverse)
require(xlsx)

# 更新函数，由于使用函数，无法追踪数据，所以只能拆开进行调试

extraction3 = function(fn1, fn2) {
  # fn1 is the patient.txt list, fn2 is the clinical index list.
  dt <- tibble()
  for(i in fn_1) { 
    ##根据日期拆分
    t1 <- readLines(i) %>% 
      iconv(from = "GB2312", to = "UTF-8") %>% ## No use this code in linux
      str_split("\\d{4}[-](0[1-9]|1[012])[-](0[1-9]|[12][0-9]|3[01])") 
    
    t1 <- data.frame(t1) %>% 
      slice(-1)
    names(t1) <- "项目"
    
    ##提取日期信息
    
    t2 <- readLines(i) %>% 
      str_extract_all("\\d{4}[-](0[1-9]|1[012])[-](0[1-9]|[12][0-9]|3[01])")
    
    t2 <- data.frame(t2)
    names(t2) <- "日期"
    
    ##合并日期和项目
    df <- bind_cols(t2,t1) 
    
    ##拆分单个检查项目，以","为标记
    df2 <- df %>% separate_rows("项目", sep = ",") 
    
    ##去除"*"，去除"："前的无效信息
    df2$'项目' <-df2$'项目' %>%  str_remove("\\*") %>% 
      str_remove('^.*\uff1a') 
    
    
    ##去除字符串前后空格
    df2$'项目' <- trimws(df2$'项目')      
    
    ##去除空格行
    df2 <- df2[-which(df2$"项目" == ""), ]
    
    ##以空格为标记，拆分数据
    df3 <- df2 %>% separate("项目", into = c("项目", "数值"), sep = " ") 
    
    ##长变宽前，要将数值项目设为numeric
    df3$'数值' <- as.numeric(df3$'数值') ##数值必须为numeric
    
    ## 判断是否包含某项目，可以最大限度保留数据
    fn <- fn_2[which(fn_2 %in% df3$"项目")] 
    
    df4 <- df3 %>%  
      filter(df3$"项目" %in% fn) %>% ##提取目标检查值
      pivot_wider(names_from = "项目", values_from = "数值") %>% 
      dplyr::select("日期", all_of(fn))%>% 
      arrange(日期) ##调整顺序
    df4 %>%   
      mutate('患者' = str_extract(i,'[\u4e00-\u9fa5]+')) %>% 
      relocate(患者, .before = 日期) %>% 
      fill(names(df4),.direction = 'downup') %>% 
      mutate(日期 = as.Date(日期)) %>% 
      mutate(across(everything(), as.character)) %>% 
      bind_rows(dt) -> dt ## 为啥放在前面“dt <- ”就不行
    print(i) # 加上这一步能够定位到错位位置
  }
  
  return(dt)
}

################################################################################
# 测试数据(额外)

# 肝功能

fn_1 <- dir()

fn_2 <- c('总胆红素', '直接胆红素','间接胆红素', '丙氨酸氨基转移酶' , 
          '天门冬氨酸氨基转移酶', 'γ-谷氨酰转肽酶', '碱性磷酸酶', '白蛋白' , 
          '球蛋白',  '胆碱酯酶', '前白蛋白' ) 

dt_liver <- extraction3(fn_1, fn_2)

# 血常规
fn_1 <- dir("./", "*.txt")
fn_2 <- c('白细胞计数', '中性粒细胞计数', '红细胞计数' , '血红蛋白' ,
          '血小板' ) 

dt_blood <- extraction3(fn_1, fn_2)  

# 乙肝病原学指标
setwd("../额外2/") #因之前的代码忘记将“未检测出”替换为“0”

fn_1 <- dir()
fn_2 <- c('乙型肝炎病毒表面抗原定量', '乙型肝炎病毒e抗原检测', 
          '乙型肝炎病毒e抗体检测','HBVDNA定量')
dt_hbv <- extraction3(fn_1, fn_2) 

# 肾功能+血脂
setwd("../额外2/") 
fn_1 <- dir()
fn_2 <- c('甘油三酯', '总胆固醇', '高密度脂蛋白', '低密度脂蛋白', '血肌酐', '尿素')
dt_ren_lip <- extraction3(fn_1, fn_2) 


# 保存数据

# save(dt_blood, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/ew_blood.dta")
# save(dt_liver, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/ew_liver.dta")
# save(dt_hbv, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/ew_hbv.dta")
# save(dt_ren_lip, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/ew_ren_lip.dta")
################################################################################
# 导入数据(换药患者22920)

# 肝功能
setwd("../换药患者22920/")

fn_1 <- dir()

fn_2 <- c('总胆红素', '直接胆红素','间接胆红素', '丙氨酸氨基转移酶' , 
          '天门冬氨酸氨基转移酶', 'γ-谷氨酰转肽酶', '碱性磷酸酶', '白蛋白' , 
          '球蛋白',  '胆碱酯酶', '前白蛋白' ) 

dt_liver <- extraction3(fn_1, fn_2)

# 血常规
fn_1 <- dir("./", "*.txt")
fn_2 <- c('白细胞计数', '中性粒细胞计数', '红细胞计数' , '血红蛋白' ,
          '血小板' ) 

dt_blood <- extraction3(fn_1, fn_2)  

# 乙肝病原学指标

fn_2 <- c('乙型肝炎病毒表面抗原定量', '乙型肝炎病毒e抗原检测', 
          '乙型肝炎病毒e抗体检测','HBVDNA定量')

dt_hbv <- extraction3(fn_1, fn_2) 

# 肾功能+血脂

fn_2 <- c('甘油三酯', '总胆固醇', '高密度脂蛋白', '低密度脂蛋白', '血肌酐', '尿素')

dt_ren_lip <- extraction3(fn_1, fn_2) 

# 保存检数据

save(dt_blood, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/22920_blood.dta")
save(dt_liver, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/22920_liver.dta")
save(dt_hbv, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/22920_hbv.dta")
save(dt_ren_lip, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/22920_ren_lip.dta")

################################################################################

# 导入数据(换药患者22917)

# 肝功能
setwd("../换药患者22917/")

fn_1 <- dir("./", "*.txt")

fn_2 <- c('总胆红素', '直接胆红素','间接胆红素', '丙氨酸氨基转移酶' , 
          '天门冬氨酸氨基转移酶', 'γ-谷氨酰转肽酶', '碱性磷酸酶', '白蛋白' , 
          '球蛋白',  '胆碱酯酶', '前白蛋白' ) 

dt_liver <- extraction3(fn_1, fn_2)

# 血常规
fn_1 <- dir("./", "*.txt")
fn_2 <- c('白细胞计数', '中性粒细胞计数', '红细胞计数' , '血红蛋白' ,
          '血小板' ) 

dt_blood <- extraction3(fn_1, fn_2)  

# 乙肝病原学指标

fn_2 <- c('乙型肝炎病毒表面抗原定量', '乙型肝炎病毒e抗原检测', 
          '乙型肝炎病毒e抗体检测','HBVDNA定量')

dt_hbv <- extraction3(fn_1, fn_2) 

# 肾功能+血脂

fn_2 <- c('甘油三酯', '总胆固醇', '高密度脂蛋白', '低密度脂蛋白', '血肌酐', '尿素')

dt_ren_lip <- extraction3(fn_1, fn_2) 

# 保存检数据

# save(dt_blood, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/22917_blood.dta")
# save(dt_liver, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/22917_liver.dta")
# save(dt_hbv, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/22917_hbv.dta")
# save(dt_ren_lip, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/22917_ren_lip.dta")

################################################################################
# 导入数据(换药患者220913)

# 肝功能
setwd("../换药患者220913/")

fn_1 <- dir()

fn_2 <- c('总胆红素', '直接胆红素','间接胆红素', '丙氨酸氨基转移酶' , 
          '天门冬氨酸氨基转移酶', 'γ-谷氨酰转肽酶', '碱性磷酸酶', '白蛋白' , 
          '球蛋白',  '胆碱酯酶', '前白蛋白' ) 

dt_liver <- extraction3(fn_1, fn_2)

# 血常规
fn_1 <- dir("./", "*.txt")
fn_2 <- c('白细胞计数', '中性粒细胞计数', '红细胞计数' , '血红蛋白' ,
          '血小板' ) 

dt_blood <- extraction3(fn_1, fn_2)  

# 乙肝病原学指标

fn_2 <- c('乙型肝炎病毒表面抗原定量', '乙型肝炎病毒e抗原检测', 
          '乙型肝炎病毒e抗体检测','HBVDNA定量')

dt_hbv <- extraction3(fn_1, fn_2) 

# 肾功能+血脂

fn_2 <- c('甘油三酯', '总胆固醇', '高密度脂蛋白', '低密度脂蛋白', '血肌酐', '尿素')

dt_ren_lip <- extraction3(fn_1, fn_2) 

# 保存检数据

# save(dt_blood, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/220913_blood.dta")
# save(dt_liver, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/220913_liver.dta")
# save(dt_hbv, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/220913_hbv.dta")
# save(dt_ren_lip, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/220913_ren_lip.dta")


################################################################################
# 导入数据(换药患者221008-09)

# 肝功能
setwd("../221008-09/")

fn_1 <- dir()

fn_2 <- c('总胆红素', '直接胆红素','间接胆红素', '丙氨酸氨基转移酶' , 
          '天门冬氨酸氨基转移酶', 'γ-谷氨酰转肽酶', '碱性磷酸酶', '白蛋白' , 
          '球蛋白',  '胆碱酯酶', '前白蛋白' ) 

dt_liver <- extraction3(fn_1, fn_2)

# 血常规
fn_1 <- dir("./", "*.txt")
fn_2 <- c('白细胞计数', '中性粒细胞计数', '红细胞计数' , '血红蛋白' ,
          '血小板' ) 

dt_blood <- extraction3(fn_1, fn_2)  

# 乙肝病原学指标

fn_2 <- c('乙型肝炎病毒表面抗原定量', '乙型肝炎病毒e抗原检测', 
          '乙型肝炎病毒e抗体检测','HBVDNA定量')

dt_hbv <- extraction3(fn_1, fn_2) 

# 肾功能+血脂

fn_2 <- c('甘油三酯', '总胆固醇', '高密度脂蛋白', '低密度脂蛋白', '血肌酐', '尿素')

dt_ren_lip <- extraction3(fn_1, fn_2) 

# 保存检数据

# save(dt_blood, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/221008-09_blood.dta")
# save(dt_liver, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/221008-09_liver.dta")
# save(dt_hbv, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/221008-09_hbv.dta")
# save(dt_ren_lip, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/221008-09_ren_lip.dta")

################################################################################
# 导入数据(换药患者221008)

# 肝功能
setwd("../221008/")

fn_1 <- dir()

fn_2 <- c('总胆红素', '直接胆红素','间接胆红素', '丙氨酸氨基转移酶' , 
          '天门冬氨酸氨基转移酶', 'γ-谷氨酰转肽酶', '碱性磷酸酶', '白蛋白' , 
          '球蛋白',  '胆碱酯酶', '前白蛋白' ) 

dt_liver <- extraction3(fn_1, fn_2)

# 血常规
fn_1 <- dir("./", "*.txt") # 这样写的好处就是可以避免错误读入Name_list.xlsx文件
fn_2 <- c('白细胞计数', '中性粒细胞计数', '红细胞计数' , '血红蛋白' ,
          '血小板' ) 

dt_blood <- extraction3(fn_1, fn_2)  

# 乙肝病原学指标

fn_2 <- c('乙型肝炎病毒表面抗原定量', '乙型肝炎病毒e抗原检测', 
          '乙型肝炎病毒e抗体检测','HBVDNA定量')

dt_hbv <- extraction3(fn_1, fn_2) 

# 肾功能+血脂

fn_2 <- c('甘油三酯', '总胆固醇', '高密度脂蛋白', '低密度脂蛋白', '血肌酐', '尿素')

dt_ren_lip <- extraction3(fn_1, fn_2)

# 保存检数据

# save(dt_blood, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/221008_blood.dta")
# save(dt_liver, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/221008_liver.dta")
# save(dt_hbv, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/221008_hbv.dta")
# save(dt_ren_lip, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/221008_ren_lip.dta")

################################################################################
# 导入数据(换药患者221004)

# 肝功能
setwd("../221004/")

fn_1 <- dir()

fn_2 <- c('总胆红素', '直接胆红素','间接胆红素', '丙氨酸氨基转移酶' , 
          '天门冬氨酸氨基转移酶', 'γ-谷氨酰转肽酶', '碱性磷酸酶', '白蛋白' , 
          '球蛋白',  '胆碱酯酶', '前白蛋白' ) 

dt_liver <- extraction3(fn_1, fn_2)

# 血常规
fn_2 <- c('白细胞计数', '中性粒细胞计数', '红细胞计数' , '血红蛋白' ,
          '血小板' ) 

dt_blood <- extraction3(fn_1, fn_2)  

# 乙肝病原学指标

fn_2 <- c('乙型肝炎病毒表面抗原定量', '乙型肝炎病毒e抗原检测', 
          '乙型肝炎病毒e抗体检测','HBVDNA定量')

dt_hbv <- extraction3(fn_1, fn_2) 

# 肾功能+血脂

fn_2 <- c('甘油三酯', '总胆固醇', '高密度脂蛋白', '低密度脂蛋白', '血肌酐', '尿素')

dt_ren_lip <- extraction3(fn_1, fn_2)

# 保存检数据

# save(dt_blood, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/221004_blood.dta")
# save(dt_liver, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/221004_liver.dta")
# save(dt_hbv, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/221004_hbv.dta")
# save(dt_ren_lip, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/221004_ren_lip.dta")

################################################################################
# 导入数据(换药患者220923/)

# 肝功能
setwd("../220923/")

fn_1 <- dir("./", "*.txt") 

fn_2 <- c('总胆红素', '直接胆红素','间接胆红素', '丙氨酸氨基转移酶' , 
          '天门冬氨酸氨基转移酶', 'γ-谷氨酰转肽酶', '碱性磷酸酶', '白蛋白' , 
          '球蛋白',  '胆碱酯酶', '前白蛋白' ) 

dt_liver <- extraction3(fn_1, fn_2)

# 血常规
fn_2 <- c('白细胞计数', '中性粒细胞计数', '红细胞计数' , '血红蛋白' ,
          '血小板' ) 

dt_blood <- extraction3(fn_1, fn_2)  

# 乙肝病原学指标

fn_2 <- c('乙型肝炎病毒表面抗原定量', '乙型肝炎病毒e抗原检测', 
          '乙型肝炎病毒e抗体检测','HBVDNA定量')

dt_hbv <- extraction3(fn_1, fn_2) 

# 肾功能+血脂

fn_2 <- c('甘油三酯', '总胆固醇', '高密度脂蛋白', '低密度脂蛋白', '血肌酐', '尿素')

dt_ren_lip <- extraction3(fn_1, fn_2)

# 保存检数据

# save(dt_blood, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/220923_blood.dta")
# save(dt_liver, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/220923_liver.dta")
# save(dt_hbv, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/220923_hbv.dta")
# save(dt_ren_lip, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/220923_ren_lip.dta")

###############################################################################
# 导入数据(换药患者220922/)

# 肝功能
setwd("../220922/")

fn_1 <- dir("./", "*.txt") 

fn_2 <- c('总胆红素', '直接胆红素','间接胆红素', '丙氨酸氨基转移酶' , 
          '天门冬氨酸氨基转移酶', 'γ-谷氨酰转肽酶', '碱性磷酸酶', '白蛋白' , 
          '球蛋白',  '胆碱酯酶', '前白蛋白' ) 

dt_liver <- extraction3(fn_1, fn_2)

# 血常规
fn_2 <- c('白细胞计数', '中性粒细胞计数', '红细胞计数' , '血红蛋白' ,
          '血小板' ) 

dt_blood <- extraction3(fn_1, fn_2)  

# 乙肝病原学指标

fn_2 <- c('乙型肝炎病毒表面抗原定量', '乙型肝炎病毒e抗原检测', 
          '乙型肝炎病毒e抗体检测','HBVDNA定量')

dt_hbv <- extraction3(fn_1, fn_2) 


# 肾功能+血脂


fn_2 <- c('甘油三酯', '总胆固醇', '高密度脂蛋白', '低密度脂蛋白', '血肌酐', '尿素')

dt_ren_lip <- extraction3(fn_1, fn_2)


# 保存检数据

# save(dt_blood, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/220922_blood.dta")
# save(dt_liver, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/220922_liver.dta")
# save(dt_hbv, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/220922_hbv.dta")
# save(dt_ren_lip, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/220922_ren_lip.dta")

###############################################################################
# 导入数据(换药患者20221003)

# 肝功能
setwd("../20221003/")

fn_1 <- dir("./", "*.txt") 

fn_2 <- c('总胆红素', '直接胆红素','间接胆红素', '丙氨酸氨基转移酶' , 
          '天门冬氨酸氨基转移酶', 'γ-谷氨酰转肽酶', '碱性磷酸酶', '白蛋白' , 
          '球蛋白',  '胆碱酯酶', '前白蛋白' ) 

dt_liver <- extraction3(fn_1, fn_2)

# 血常规
fn_2 <- c('白细胞计数', '中性粒细胞计数', '红细胞计数' , '血红蛋白' ,
          '血小板' ) 

dt_blood <- extraction3(fn_1, fn_2)  

# 乙肝病原学指标

fn_2 <- c('乙型肝炎病毒表面抗原定量', '乙型肝炎病毒e抗原检测', 
          '乙型肝炎病毒e抗体检测','HBVDNA定量')

dt_hbv <- extraction3(fn_1, fn_2) 

# 肾功能+血脂


fn_2 <- c('甘油三酯', '总胆固醇', '高密度脂蛋白', '低密度脂蛋白', '血肌酐', '尿素')

dt_ren_lip <- extraction3(fn_1, fn_2)

# 保存检数据

# save(dt_blood, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/20221003_blood.dta")
# save(dt_liver, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/20221003_liver.dta")
# save(dt_hbv, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/20221003_hbv.dta")
# save(dt_ren_lip, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/20221003_ren_lip.dta")

###############################################################################
# 导入数据(换药患者20220926)

# 肝功能
setwd("../20220926/")

fn_1 <- dir("./", "*.txt") 

fn_2 <- c('总胆红素', '直接胆红素','间接胆红素', '丙氨酸氨基转移酶' , 
          '天门冬氨酸氨基转移酶', 'γ-谷氨酰转肽酶', '碱性磷酸酶', '白蛋白' , 
          '球蛋白',  '胆碱酯酶', '前白蛋白' ) 

dt_liver <- extraction3(fn_1, fn_2)

# 血常规
fn_2 <- c('白细胞计数', '中性粒细胞计数', '红细胞计数' , '血红蛋白' ,
          '血小板' ) 

dt_blood <- extraction3(fn_1, fn_2)  

# 乙肝病原学指标

fn_2 <- c('乙型肝炎病毒表面抗原定量', '乙型肝炎病毒e抗原检测', 
          '乙型肝炎病毒e抗体检测','HBVDNA定量')

dt_hbv <- extraction3(fn_1, fn_2) 

# 肾功能+血脂

fn_2 <- c('甘油三酯', '总胆固醇', '高密度脂蛋白', '低密度脂蛋白', '血肌酐', '尿素')

dt_ren_lip <- extraction3(fn_1, fn_2)

# 保存检数据

# save(dt_blood, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/20220926_blood.dta")
# save(dt_liver, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/20220926_liver.dta")
# save(dt_hbv, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/20220926_hbv.dta")
# save(dt_ren_lip, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/20220926_ren_lip.dta")

###############################################################################
# 导入数据(补充数据1018)

# 肝功能
setwd("../补充数据1018/")

fn_1 <- dir("./", "*.txt") 

fn_2 <- c('总胆红素', '直接胆红素','间接胆红素', '丙氨酸氨基转移酶' , 
          '天门冬氨酸氨基转移酶', 'γ-谷氨酰转肽酶', '碱性磷酸酶', '白蛋白' , 
          '球蛋白',  '胆碱酯酶', '前白蛋白' ) 

dt_liver <- extraction3(fn_1, fn_2)

# 血常规
fn_2 <- c('白细胞计数', '中性粒细胞计数', '红细胞计数' , '血红蛋白' ,
          '血小板' ) 

dt_blood <- extraction3(fn_1, fn_2)  

# 乙肝病原学指标

fn_2 <- c('乙型肝炎病毒表面抗原定量', '乙型肝炎病毒e抗原检测', 
          '乙型肝炎病毒e抗体检测','HBVDNA定量')

dt_hbv <- extraction3(fn_1, fn_2) 

# 肾功能+血脂

fn_2 <- c('甘油三酯', '总胆固醇', '高密度脂蛋白', '低密度脂蛋白', '血肌酐', '尿素')

dt_ren_lip <- extraction3(fn_1, fn_2)

# 保存检数据

# save(dt_blood, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/1018_blood.dta")
# save(dt_liver, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/1018_liver.dta")
# save(dt_hbv, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/1018_hbv.dta")
# save(dt_ren_lip, file = "~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/1018_ren_lip.dta")


################################################################################
# 整合数据
# 练习盲打的目的是让自己的工作效率能够更快的获得提高，原来掌握盲打的秘诀还是在于记忆和练习

rm(list = ls())
setwd("~/sambashare/RWorkspace/R/clinical_investigation_2022/Lab_data/")

## 合并肝功能

# i = fn[1]
fn <- dir(pattern = "*_liver.dta")

dt <- tibble()

for(i in fn) {
   load(i) 
   dt <- dt_liver %>% 
   bind_rows(dt)
   print(i)
}

liver <- dt 

## 合并血常规

fn <- dir(pattern = "*_blood.dta")

dt <- tibble()

for(i in fn) {
  load(i) 
  dt <- dt_blood %>% 
    bind_rows(dt)
  print(i)
}

blood <- dt

## 合并病原学指标

fn <- dir(pattern = "*_hbv.dta")

dt <- tibble()

for(i in fn) {
  load(i) 
  dt <- dt_hbv %>% 
    bind_rows(dt)
  print(i)
}

hbv <- dt

## 合并肾功能及血脂指标

fn <- dir(pattern = "*_ren_lip.dta")

dt <- tibble()

for(i in fn) {
  load(i) 
  dt <- dt_ren_lip %>% 
    bind_rows(dt)
  print(i)
}

ren_lip <- dt
################################################################################
## 保存

# save(liver, file = '../Combination_data/liver_function.dta')
# save(blood, file = '../Combination_data/blood.dta')
# save(hbv, file = '../Combination_data/hbv.dta')
# save(ren_lip, file = '../Combination_data/ren_lip.dta')



