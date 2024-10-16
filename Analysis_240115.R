## 分析框架
##> 2024年1月15日
##> updated: 2024年5月16日
##> Step 1:
##> 第1年：整体分析只设一个条件age > 18
##> Step 2:
##> 治疗前后比较，采用配对T检验
##> Step 3:
##> 第1年：亚组分析HBeAg阳性CHB(定义：HBeAg阳性；HBV-DNA阳性)
##> Step 4:
##> 第1年：亚组分析HBeAg阴性CHB(定义：HBeAg阴性；HBV-DNA阳性)
##> Step 5:
##> 两组比较，PSM之后
##> At point
##> At changes
##> Step 6:
##> 第2年：整体分析
##> 
##> Additional 1
##> 补充基线血小板的数据，用于计算APRI or FIB-4

################################################################################

## 设置路径
setwd("/home/alo/Rspace/clinical_investigation_2022/Combination_data/Patient_info/")

## 加载分析包

#### 数据分析包
if(!require(tidyverse))require(tidyverse)
if(!require(readxl))require(readxl)
if(!require(writexl))require(writexl)
if(!require(xlsx))require(xlsx)
if(!require(survival))require(survival)
if(!require(survminer))require(survminer)
if(!require(lubridate))require(lubridate)
if(!require(CBCgrps))require(CBCgrps)
if(!require(MatchIt))require(MatchIt)
if(!require(naniar))require(naniar)

#### 作图包
if(!require(patchwork))require(patchwork)
if(!require(cowplot))require(cowplot)
if(!require(ggsci))require(ggsci)
if(!require(showtext))require(showtext)
if(!require(forcats))require(forcats)

## 加载原始数据

load("./Combination_data.Rdata")

table(patient_info$status.x)
table(patient_info$status.y) 

#> 61/(2283+61) 
#> [1] 0.02602389 肝硬化/肝癌5年发生率
#> 19/(2325+19)
#> [1] 0.008105802 肝癌5年发生率

################################################################################
## 48 weeks

#### 加载数据

load("../../Output/df_impu_1_360_231202.dta")  ## 以脂代谢指标为主的数据

#### 准备数据


##> To convert μmol/l to mg/dl, multiply by 0.0113. To convert mg/dl to μmol/l, multiply by 88.4 (血肌酐).
##> To convert Triglyceride values from mg/dL to mmol/L, divide the value in mg/dL by 88.57. (TG)
##> To convert from mmol/L to mg/dL, multiply the value in mmol/L by 88.57.(TG)
##> To convert Total cholesterol (TC), LDL and HDL from mg/dL to mmol/L, divide the value in mg/dL by 38.67. 
##> To convert from mmol/L to mg/dL, multiply the value in mmol/L by 38.67.
##> https://heartcare.sydney/cholesterol-unit-conversion/

lis = df_impu[[99]] %>% 
  # mutate(eGFR = ifelse(性别 == "女", 186*血肌酐^-1.154*年龄^-0.203*0.742, 186*血肌酐^-1.154*年龄^-0.203))%>% ## 由于未记录出处，已不知道该公式的出处
  mutate(血肌酐 = 血肌酐*0.0113) %>% # (mg/dl)
  mutate(eGFR = case_when(
    血肌酐 <= 0.7 & 性别 == "女" ~ 142*(血肌酐/0.7)^(-0.241)*0.9938^年龄*1.012, ## 采用2021 CKD-EPI公式
    血肌酐 > 0.7 & 性别 == "女" ~ 142*(血肌酐/0.7)^(-1.2)*0.9938^年龄*1.012,
    血肌酐 <= 0.9 & 性别 == "男" ~ 142*(血肌酐/0.9)^(-0.302)*0.9938^年龄*1,
    血肌酐 > 0.9 & 性别 == "男" ~ 142*(血肌酐/0.9)^(-1.2)*0.9938^年龄*1
  )) %>% 
  mutate(乙型肝炎病毒e抗原检测 = ifelse(乙型肝炎病毒e抗原检测 < 0.1, 'Negative', 'Positive')) %>% 
  mutate(乙型肝炎病毒e抗体检测 = ifelse(乙型肝炎病毒e抗体检测 > 0, 'Positive', 'Negative')) %>% 
  mutate(E抗原血清转化 = ifelse(乙型肝炎病毒e抗原检测 == 'Negative' & 乙型肝炎病毒e抗体检测 == 'Positive', "Yes", "No")) %>%
  mutate(HBVDNA定量 = ifelse(HBVDNA定量 <=500, "Negative", "Positive")) %>% 
  mutate(
    甘油三酯 = 甘油三酯*88.57, # (mg/dl)
    总胆固醇 = 总胆固醇*38.67, # (mg/dl)
    高密度脂蛋白 = 高密度脂蛋白*38.67, # (mg/dl)
    低密度脂蛋白 = 低密度脂蛋白*38.67) %>%
  mutate(甘油三酯 = ifelse(甘油三酯 > 1000, 137.9, 甘油三酯)) %>%  ## 删除一个离群值
  mutate(TC_HDL = 总胆固醇/高密度脂蛋白,
         LDL_HDL = 低密度脂蛋白/高密度脂蛋白) %>% 
  dplyr::filter(年龄 > 18) %>% 
  mutate(Time = factor(Time, levels=c("D01", 'D360'))) %>% 
  dplyr::filter(grp != "Trans") %>%
  mutate(grp = fct_drop(grp)) %>% 
  mutate(grp = factor(grp, levels = c("TDF","TAF")))

#### 数据探索
lis %>% dplyr::filter(Time == "D360") %>% 
  count(grp) ## 查看有完整数据的患者

lis %>% dplyr::filter(Time == "D01") %>% 
  count(HBVDNA定量) ## 查看DNA情况

lis$Time %>% table()

#### 补充基线血小板的数据

load("../clean_blood.dta")

fn = lis %>% 
  pull(姓名) %>% unique() 

plt = blood %>% 
  dplyr::select(患者, 日期, 血小板) %>% 
  mutate(日期 = as.Date(日期),
         '姓名' = 患者)%>% 
  dplyr::filter(患者 %in% fn) %>%
  group_by(患者) %>% 
  arrange(日期) %>%
  mutate(Time = 'D01') %>% 
  slice(1) %>% 
  ungroup() %>% 
  dplyr::select(姓名, Time, 血小板)

bas = left_join(lis, plt, by = c('姓名', 'Time')) %>% 
  dplyr::filter(Time == "D01") %>% 
  impute_mean_at(.vars = '血小板') %>%  ## 使用均值进行了填补
  mutate(FIB4 = (年龄*天门冬氨酸氨基转移酶)/(血小板*丙氨酸氨基转移酶^0.5)
  ) %>% 
  mutate(FIB4 = case_when(FIB4 < 1.45 ~ "Normal",
                          FIB4 >3.25 ~ "Fibrosis",
                          .default = "Median")) 
table(bas$FIB4)
###### 查看基线肝纤维化情况
bas %>% dplyr::select(年龄, 天门冬氨酸氨基转移酶, 丙氨酸氨基转移酶, 血小板, FIB4) %>% dplyr::filter(FIB4 == "Fibrosis")
bas %>% dplyr::filter(血小板 < 150) %>% count()


#### 1-两组的基线比较（未匹配）
bas %>% dplyr::filter(Time == "D01") %>% 
  twogrps(gvar = "grp", 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) ## 基线不匹配是PSM的前提条件

exc = bas %>% dplyr::filter(Time == "D01") %>% 
  twogrps(gvar = "grp", pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) %>% .[["Table"]]

#### 保存数据至新输出文件夹
write_xlsx(exc, "../../Output_for_publication/New20240115/Baseline_comparison_48weeks.xls")

#### 2-治疗前后比较(48 Weeks)

###### TDF
######## T-test
lis %>% dplyr::filter(grp == "TDF") %>% 
  twogrps(gvar = "Time", 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) ## 有部分变量不paired
exc = lis %>% dplyr::filter(grp == "TDF") %>% 
  twogrps(gvar = "Time", 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) %>% .[["Table"]] 

write_xlsx(exc, "../../Output_for_publication/New20240115/Pre_Post_T_TDF_48weeks.xls")

######## Paired T-test （需要加载修改后的函数）
lis %>% dplyr::filter(grp == "TDF") %>% 
  twoCom(gvar = "Time", 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) ## Paired的变量

exc = lis %>% dplyr::filter(grp == "TDF") %>% 
  twoCom(gvar = "Time", 
         pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
         ShowStatistic = T) %>% .[["Table"]]

write_xlsx(exc, "../../Output_for_publication/New20240115/Pre_Post_Paired_T_TDF_48weeks.xls")

###### TAF
######## T-test
lis %>% dplyr::filter(grp == "TAF") %>% 
  twogrps(gvar = "Time", 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) 

exc = lis %>% dplyr::filter(grp == "TAF") %>% 
  twogrps(gvar = "Time", 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) %>% .[["Table"]]

write_xlsx(exc, "../../Output_for_publication/New20240115/Pre_Post_T_TAF_48weeks.xls")

######## Paired T-test （需要加载修改后的函数）
lis %>% dplyr::filter(grp == "TAF") %>% 
  twoCom(gvar = "Time", 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) 

exc = lis %>% dplyr::filter(grp == "TAF") %>% 
  twoCom(gvar = "Time", 
         pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
         ShowStatistic = T) %>% .[["Table"]]

write_xlsx(exc, "../../Output_for_publication/New20240115/Pre_Post_Paired_T_TAF_48weeks.xls")

###### 血脂指标作图 (使用paired的结果)

dt1 = tibble(
  Groups = c("D1", "48W"),
  TC = c(168.07, 161.18),
  TG = c(105.29, 100.53),
  HDL = c(47.39, 46.24),
  LDL = c(89.91, 88.82),
  LDL_HDL = c(2.07, 2.04),
  TC_HDL =  c(3.81, 3.66),
  SD_TC = c(35.72, 31.72),
  SD_TG = c(63.81, 63.11),
  SD_LDL = c(24.85, 23.73),
  SD_HDL = c(14.07, 11.74),
  SD_RLDL = c(0.89, 0.77),
  SD_RTC = c(1.32, 1.06)
) %>% mutate(Groups = factor(Groups, levels=c("D1", "48W"))) %>% 
  pivot_longer(TC:TC_HDL, values_to = "Mean", names_to = "Index") %>% 
  rename('TC' = SD_TC,
         'TG' = SD_TG,
         'LDL' = SD_LDL,
         'HDL' = SD_HDL,
         'LDL_HDL' = SD_RLDL,
         'TC_HDL' = SD_RTC) %>% 
  pivot_longer(TC:TC_HDL, values_to = "SD", names_to = "Index2") %>% 
  dplyr::filter(Index == Index2) %>% 
  select(-Index2) %>% 
  mutate(Index = factor(Index, levels= c("TC", "TG","LDL", "HDL", "LDL_HDL", "TC_HDL")))

dt2 = tibble(
  Groups = c("D1", "48W"),
  TC = c(183.03 , 189.36 ),
  TG = c(114.13 , 129.97 ),
  HDL = c(49.51 , 47.02 ),
  LDL = c(107.13 , 121.88 ),
  LDL_HDL = c(2.27, 2.78),
  TC_HDL =  c(3.84, 4.32),
  SD_TC = c(40.89, 38.64),
  SD_TG = c(61.47, 71.16),
  SD_LDL = c(32.04, 34.33),
  SD_HDL = c(10.91, 11.84),
  SD_RLDL = c(0.84, 1.17),
  SD_RTC = c(1.13, 2.04)
) %>% mutate(Groups = factor(Groups, levels=c("D1", "48W"))) %>% 
  pivot_longer(TC:TC_HDL, values_to = "Mean", names_to = "Index") %>% 
  rename('TC' = SD_TC,
         'TG' = SD_TG,
         'LDL' = SD_LDL,
         'HDL' = SD_HDL,
         'LDL_HDL' = SD_RLDL,
         'TC_HDL' = SD_RTC) %>% 
  pivot_longer(TC:TC_HDL, values_to = "SD", names_to = "Index2") %>% 
  dplyr::filter(Index == Index2) %>% 
  select(-Index2) %>% 
  mutate(Index = factor(Index, levels= c("TC", "TG","LDL", "HDL", "LDL_HDL", "TC_HDL")))

p_dta = dt1 %>% 
  dplyr::filter(Index != "LDL_HDL" & Index != "TC_HDL" )

pre_pos_tdf_1 = ggplot(p_dta, aes(x = Index, y = Mean,  fill = Groups)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_y_continuous(limits = c(0, 250)) +
  labs(title="TDF", y = "mg/dL", x = "", fill = "Time") +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
                position=position_dodge(.9)) +
  geom_signif(annotations = "P < 0.001\n",fontface = "italic",
              xmin = 0.75, xmax = 1.20 ,y_position = 220,
              tip_length = 0.005, vjust=0.4,textsize=5) +
  theme_cowplot(font_size = 20)+
  scale_fill_npg()

p_dta = dt1 %>% 
  dplyr::filter(Index == "LDL_HDL" | Index == "TC_HDL" )

pre_pos_tdf_2 = ggplot(p_dta, aes(x = Index, y = Mean,  fill = Groups)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_y_continuous(limits = c(0, 8)) + 
  labs(title="TDF", y = "Ratio", x = "", fill = "Time") +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
                position=position_dodge(.9)) +
  geom_signif(annotations = "P = 0.022\n",fontface = "italic",
              xmin = 1.75, xmax = 2.20 ,y_position = 5.5,
              tip_length = 0.005, vjust=0.4,textsize=5) +
  theme_cowplot(font_size = 20) +
  scale_fill_npg()

p_dta = dt2 %>% 
  dplyr::filter(Index != "LDL_HDL" & Index != "TC_HDL" )

pre_pos_taf_1 = ggplot(p_dta, aes(x = Index, y = Mean,  fill = Groups)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_y_continuous(limits = c(0, 300)) +
  labs(title="TAF", y = "mg/dL", x = "", fill = "Time") +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
                position=position_dodge(.9)) + 
  geom_signif(annotations = c("P = 0.01\n", "P = 0.004\n" , "P < 0.001\n", "P = 0.001\n"),fontface = "italic",
              xmin = c(0.75, 1.75,2.75, 3.75), xmax = c(1.2, 2.20,3.20,4.2) ,y_position = c(250, 210,170, 70),
              tip_length = 0.005, vjust=0.4,textsize=5) +
  theme_cowplot(font_size = 20)+
  scale_fill_npg()

p_dta = dt2 %>% 
  dplyr::filter(Index == "LDL_HDL" | Index == "TC_HDL" )

pre_pos_taf_2 = ggplot(p_dta, aes(x = Index, y = Mean,  fill = Groups)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_y_continuous(limits = c(0,8)) +
  labs(title="TAF", y = "Ratio", x = "", fill = "Time") +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
                position=position_dodge(.9)) +
  geom_signif(annotations = c( "P < 0.001\n", "P = 0.001\n"),fontface = "italic",
              xmin = c(0.75,1.75), xmax = c(1.20,2.20) ,y_position = c(4.5, 7),
              tip_length = 0.005, vjust=0.4,textsize=5) +
  theme_cowplot(font_size = 20)+
  scale_fill_npg()

###### 拼图

(pre_pos_tdf_1 | pre_pos_tdf_2) / (pre_pos_taf_1 | pre_pos_taf_2) + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A")

ggsave("Pre_Post_48weeks_Bar.png", plot = last_plot(), device = "png", path = "../../Output_for_publication/New20240115/", width = 12,
height = 10, units = "in", dpi = 300)

figure1 = 
  (pre_pos_tdf_1 | pre_pos_tdf_2) / (pre_pos_taf_1 | pre_pos_taf_2) + 
    plot_layout(guides = "collect")


###### 随访周期
patient_info %>% 
  filter(姓名 %in% lis$姓名) %>% 
  mutate(across(D1.x:D360, as.Date)) %>% 
  summary()
#> 2017-05-09 to 2021-10-10

if(F){
## 添加的内容
##> 加入26例肝血管瘤患者的ctr比较

load("../../Output/control.Rdata") 

df_ctr = df_ctr %>% 
  select(性别,年龄,TG:LDLC) %>% 
  mutate(TC_HDL = CHOL/HDLC,
         LDL_HDL = LDLC/HDLC,
         grp = "CTR") %>% 
  rename(
    "甘油三酯" = TG,
    "总胆固醇" = CHOL,
    '高密度脂蛋白' = HDLC,
    "低密度脂蛋白" = LDLC
  ) %>% 
  mutate(
    甘油三酯 = 甘油三酯*88.57,
    总胆固醇 = 总胆固醇*38.67,
    高密度脂蛋白 = 高密度脂蛋白*38.67,
    低密度脂蛋白 = 低密度脂蛋白*38.67)

summary(df_ctr)


df_tdf = lis %>% dplyr::filter(grp == "TDF") %>% 
  filter(Time == "D360") %>% 
  select(grp, 性别,年龄, 甘油三酯, 总胆固醇, 高密度脂蛋白, 低密度脂蛋白, TC_HDL, LDL_HDL)

summary(df_tdf)

df_taf = lis %>% dplyr::filter(grp == "TAF") %>% 
  filter(Time == "D360") %>% 
  select(grp, 性别,年龄, 甘油三酯, 总胆固醇, 高密度脂蛋白, 低密度脂蛋白, TC_HDL, LDL_HDL)

summary(df_taf)

df_com_ctr = bind_rows(df_tdf, df_taf) %>% 
  bind_rows(df_ctr)

summary(df_com_ctr)

df_com_ctr$grp %>% table()

df_com_ctr %>% dplyr::filter(grp != "TDF") %>% 
  twogrps(gvar = "grp", pnormtest = 0,
          ShowStatistic = T) 

df_com_ctr %>% dplyr::filter(grp != "TAF") %>% 
  twogrps(gvar = "grp", pnormtest = 0,
          ShowStatistic = T) 

## ctr的样本量过小
## 基线1:2匹配

## TAF

data = df_com_ctr %>%  
      dplyr::filter(grp != "TDF") %>%  
      mutate(grp = factor(grp, levels=c("TAF","CTR")))

data$grp %>% levels()


m.out = matchit(grp ~ 年龄 + 性别, data = data,
                method = "nearest", replace = F, distance = "logit", 
                ratio = 2, caliper = 1.5) 

summary(m.out)

m.data = m.out %>% match.data() %>% 
  as.data.frame() %>% 
  select(-c(distance:subclass)) 

m.data$grp %>% table()

m.data %>% 
  twogrps(gvar = "grp", pnormtest = 0,
          ShowStatistic = T) 

exc = m.data %>% 
  twogrps(gvar = "grp", pnormtest = 0,
          ShowStatistic = T) %>% .[["Table"]]

# write_xlsx(exc, "../../Output/taf_ctr_360_240516.xlsx")

## Plot

data = m.data %>% 
  rename(
    TG = 甘油三酯,
    TC = 总胆固醇,
    HDL = 高密度脂蛋白,
    LDL = 低密度脂蛋白
  ) %>% 
  pivot_longer(TG:LDL_HDL, names_to = "Index", values_to = "Value")  %>% 
  group_by(grp, Index) %>% 
  mutate(Mean = mean(Value) |> round(2),
         SD = sd(Value)|> round(2) ) %>% 
  ungroup() %>% 
  mutate(Index = factor(Index, levels = c("TC","TG","LDL","HDL", "TC_HDL", "LDL_HDL")))


p1 = ggplot(data |> filter(Index != "LDL_HDL" & Index != "TC_HDL"), aes(x = Index, y = Mean,  fill = grp)) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_text(aes(label= Mean), vjust=1.6, color="black",
            position = position_dodge(0.9), size = 6) +
  labs(title="TAF", y = "mg/dL", x = "", fill = "")  +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
                position=position_dodge(.9))  +
  theme_cowplot()

p2 = ggplot(data |> filter(Index == "LDL_HDL" | Index == "TC_HDL"), aes(x = Index, y = Mean,  fill = grp)) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_text(aes(label= Mean), vjust=1.6, color="black",
            position = position_dodge(0.9), size = 6) +
  labs(title="TAF", y = "Ratio", x = "", fill = "")  +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
                position=position_dodge(.9))  +
  theme_cowplot()

## TDF
data = df_com_ctr %>%  
  dplyr::filter(grp != "TAF") %>%  
  mutate(grp = factor(grp, levels=c("TDF","CTR")))

data$grp %>% levels()
data$grp %>% table()

m.out = matchit(grp ~ 年龄 + 性别, data = data,
                method = "nearest", replace = F, distance = "logit", 
                ratio = 2, caliper = 2) 


m.data = m.out %>% match.data() %>% 
  as.data.frame() %>% 
  select(-c(distance:subclass)) 

m.data$grp %>% table()

m.data %>% 
  twogrps(gvar = "grp", pnormtest = 0,
          ShowStatistic = T) 

exc = m.data %>% 
  twogrps(gvar = "grp", pnormtest = 0,
          ShowStatistic = T) %>% .[["Table"]]

# write_xlsx(exc, "../../Output/tdf_ctr_360_240516.xlsx")

## Plot

data = m.data %>% 
  rename(
    TG = 甘油三酯,
    TC = 总胆固醇,
    HDL = 高密度脂蛋白,
    LDL = 低密度脂蛋白
  ) %>% 
  pivot_longer(TG:LDL_HDL, names_to = "Index", values_to = "Value")  %>% 
  group_by(grp, Index) %>% 
  mutate(Mean = mean(Value) |> round(2),
         SD = sd(Value)|> round(2) ) %>% 
  ungroup()  %>% 
  mutate(Index = factor(Index, levels = c("TC","TG","LDL","HDL", "TC_HDL", "LDL_HDL")))


p3 = ggplot(data |> filter(Index != "LDL_HDL" & Index != "TC_HDL"), aes(x = Index, y = Mean,  fill = grp)) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_text(aes(label= Mean), vjust=1.6, color="black",
            position = position_dodge(0.9), size = 6) +
  labs(title="TDF", y = "mg/dL", x = "", fill = "")  +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
                position=position_dodge(.9))  +
  geom_signif(annotations = c("p < 0.001", "P = 0.04","P < 0.001"), fontface = "italic",
              xmin = c(0.75,2.75,3.75), xmax = c(1.25,3.25, 4.25),y_position = c(280, 180, 80),
              tip_length = 0.025, vjust=0.4,textsize=4)+
  theme_cowplot()

p4 = ggplot(data |> filter(Index == "LDL_HDL" | Index == "TC_HDL"), aes(x = Index, y = Mean,  fill = grp)) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_text(aes(label= Mean), vjust=1.6, color="black",
            position = position_dodge(0.9), size = 6) +
  labs(title="TDF", y = "Ratio", x = "", fill = "")  +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
                position=position_dodge(.9))  +
  geom_signif(annotations = "P = 0.037", fontface = "italic",
              xmin = 1.75, xmax = 2.25, y_position = 3.8,
              tip_length = 0.025, vjust=0.4,textsize=4)+
  theme_cowplot()


(p3 + p4) / (p1 + p2) +
plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A")

ggsave("Com_Ctr_48weeks_Bar.png", plot = last_plot(), device = "png", path = "../../Output/", width = 12,
       height = 10, units = "in", dpi = 300)

}
################################################################################
#### 2-亚组分析（未匹配）

###### 2.1-HBeAg阳性CHB
lis1 = lis %>%
  dplyr::filter(Time == "D01" & 乙型肝炎病毒e抗原检测 == "Positive") 

lis1$grp %>% table() 
lis1$Time %>% table()

###### 基线比较
lis1 %>% twogrps(gvar = "grp", 
        pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
        ShowStatistic = T)

###### 合并数据
fn = lis1 %>% 
  pull(姓名)

dta = lis %>% 
  dplyr::filter(Time == "D360") %>% 
  dplyr::filter(姓名 %in% fn) %>% 
  bind_rows(lis1) %>% 
  arrange(姓名,Time)

dta$Time %>% table()

head(dta)

###### 治疗前后比较(亚组)
##### T-test (TDF)
dta %>%  
  dplyr::filter(grp == "TDF") %>%
  select(-姓名) %>% 
  twogrps(gvar = 'Time', 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"), 
          ShowStatistic = T) 

###### Paired T-test (TDF)

dta %>%  
  dplyr::filter(grp == "TDF") %>%
  select(-姓名) %>% 
  twoCom(gvar = 'Time', 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"), 
          ShowStatistic = T) 

exc = dta %>%  
  dplyr::filter(grp == "TDF") %>%
  select(-姓名) %>% 
  twoCom(gvar = 'Time', 
         pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"), 
         ShowStatistic = T) %>% .[["Table"]]

write_xlsx(exc, "../../Output_for_publication/New20240115/Positive_TDF_48weeks.xlsx")

##### T-test (TAF)
dta %>% 
  dplyr::filter(grp == "TAF") %>% 
  twogrps(gvar = 'Time', 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"), 
          ShowStatistic = T) 
###### Paired T-test (TAF)
dta %>% 
  dplyr::filter(grp == "TAF") %>% 
  twoCom(gvar = 'Time', 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"), 
          ShowStatistic = T) 

exc = dta %>% 
  dplyr::filter(grp == "TAF") %>% 
  twoCom(gvar = 'Time', 
         pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"), 
         ShowStatistic = T) %>% .[["Table"]]

write_xlsx(exc, "../../Output_for_publication/New20240115/Positive_TAF_48weeks.xlsx")

###### 2.2-HBeAg阴性CHB
lis1 = lis %>%
  dplyr::filter(Time == "D01" & 乙型肝炎病毒e抗原检测 == "Negative") 

lis1$grp %>% table() 
lis1$Time %>% table()

###### 基线比较
lis1 %>% twogrps(gvar = "grp", 
                 pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
                 ShowStatistic = T)

###### 合并数据
fn = lis1 %>% 
  pull(姓名)

dta = lis %>% 
  dplyr::filter(Time == "D360") %>% 
  dplyr::filter(姓名 %in% fn) %>% 
  bind_rows(lis1) %>% 
  arrange(姓名,Time)

dta$Time %>% table()

head(dta)

###### 治疗前后比较(亚组)
##### T-test (TDF)
dta %>%  
  dplyr::filter(grp == "TDF") %>%
  select(-姓名) %>% 
  twogrps(gvar = 'Time', 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"), 
          ShowStatistic = T) 

###### Paired T-test (TDF)

dta %>%  
  dplyr::filter(grp == "TDF") %>%
  select(-姓名) %>% 
  twoCom(gvar = 'Time', 
         pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"), 
         ShowStatistic = T) 

exc = dta %>%  
  dplyr::filter(grp == "TDF") %>%
  select(-姓名) %>% 
  twoCom(gvar = 'Time', 
         pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"), 
         ShowStatistic = T) %>% .[["Table"]]

write_xlsx(exc, "../../Output_for_publication/New20240115/Negative_TDF_48weeks.xlsx")


##### T-test (TAF)
dta %>% 
  dplyr::filter(grp == "TAF") %>% 
  twogrps(gvar = 'Time', 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"), 
          ShowStatistic = T) 
###### Paired T-test (TAF)
dta %>% 
  dplyr::filter(grp == "TAF") %>% 
  twoCom(gvar = 'Time', 
         pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"), 
         ShowStatistic = T) 

exc = dta %>% 
  dplyr::filter(grp == "TAF") %>% 
  twoCom(gvar = 'Time', 
         pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"), 
         ShowStatistic = T) %>% .[["Table"]]

write_xlsx(exc, "../../Output_for_publication/New20240115/Negative_TAF_48weeks.xlsx")

###### 2-3 做图

######## Positive
######## TAF
dt_Po_taf = tibble(
  Groups = c("D1", "48W"),
  TC = c(184.39, 188.64),
  TG = c(113.26, 121.9),
  HDL = c(48.7, 46.7),
  LDL = c(105.64, 123.21),
  LDL_HDL = c(2.27, 2.86),
  TC_HDL= c(3.93, 4.42),
  SD_TC = c(44.52,40.33),
  SD_TG = c(69.15, 67.06),
  SD_HDL = c(10.71, 12.12),
  SD_LDL = c(34.05, 34.8),
  SD_RLDL = c(0.88, 1.32),
  SD_RTC = c(1.28, 2.57)
) %>% mutate(Groups = factor(Groups, levels=c("D1", "48W"))) %>% 
  pivot_longer(TC:TC_HDL, values_to = "Mean", names_to = "Index") %>% 
  rename(
    "TC" = SD_TC,
    "TG" = SD_TG,
    'LDL' = SD_LDL,
    'HDL' = SD_HDL,
    'LDL_HDL' = SD_RLDL,
    'TC_HDL' = SD_RTC) %>% 
  pivot_longer(TC:TC_HDL, values_to = "SD", names_to = "Index2") %>% 
  dplyr::filter(Index == Index2) %>% 
  select(-Index2) %>% 
  mutate(Index = factor(Index, levels= c("TC","TG", "LDL","HDL", "LDL_HDL", "TC_HDL"))) 

###### Plotting
p1_po_taf = dt_Po_taf %>% dplyr::filter(Index != "LDL_HDL" & Index != "TC_HDL" ) %>%
  ggplot(aes(x = Index, y = Mean,  fill = Groups)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_y_continuous(limits = c(0, 250)) +
  labs(title="TAF", subtitle = "HBeAg Positive", y = "mg/dL", x = "", fill = "Time") +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
                position=position_dodge(.9)) +
  geom_signif(annotations = c("P < 0.001\n" , "P =0.046\n"),fontface = "italic",
              xmin = c(2.75, 3.75), xmax = c(3.20, 4.20) ,y_position = c(180, 70),
              tip_length = 0.005, vjust=0.4,textsize=5) +
  theme_cowplot(font_size = 20)+
  scale_fill_npg()

p2_po_taf = dt_Po_taf %>% dplyr::filter(Index == "LDL_HDL" | Index == "TC_HDL" ) %>%
  ggplot(aes(x = Index, y = Mean,  fill = Groups)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_y_continuous(limits = c(0, 8)) +
  labs(title="TAF", subtitle = "HBeAg Positive", y = "Ratio", x = "", fill = "Time") +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
                position=position_dodge(.9)) +
  geom_signif(annotations = c("P < 0.001\n"),fontface = "italic",
              xmin = c(0.75), xmax = c(1.20) ,y_position = c(4.5),
              tip_length = 0.005, vjust=0.4,textsize=5) +
  theme_cowplot(font_size = 20)+
  scale_fill_npg()

####### TDF

dt_Po_tdf = tibble(
  Groups = c("D1", "48W"),
  TC = c(168.12, 161.87),
  TG = c(108.42, 103.11),
  HDL = c(46.7, 46.22),
  LDL = c(88.99, 87.84),
  LDL_HDL = c(2.09, 2.04),
  TC_HDL= c(3.88, 3.7),
  SD_TC = c(36.36, 33.16),
  SD_TG = c(64.94, 66.81),
  SD_HDL = c(14.17, 12.21),
  SD_LDL = c(24.33, 23.52),
  SD_RLDL = c(0.93, 0.8),
  SD_RTC = c(1.39, 1.13)
) %>% mutate(Groups = factor(Groups, levels=c("D1", "48W"))) %>% 
  pivot_longer(TC:TC_HDL, values_to = "Mean", names_to = "Index") %>% 
  rename(
    "TC" = SD_TC,
    "TG" = SD_TG,
    'LDL' = SD_LDL,
    'HDL' = SD_HDL,
    'LDL_HDL' = SD_RLDL,
    'TC_HDL' = SD_RTC) %>% 
  pivot_longer(TC:TC_HDL, values_to = "SD", names_to = "Index2") %>% 
  dplyr::filter(Index == Index2) %>% 
  select(-Index2) %>% 
  mutate(Index = factor(Index, levels= c("TC","TG", "LDL","HDL", "LDL_HDL", "TC_HDL"))) 

###### Plotting
p1_po_tdf = dt_Po_tdf %>% dplyr::filter(Index != "LDL_HDL" & Index != "TC_HDL" ) %>%
  ggplot(aes(x = Index, y = Mean,  fill = Groups)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_y_continuous(limits = c(0, 250)) +
  labs(title="TDF", subtitle = "HBeAg Positive", y = "mg/dL", x = "", fill = "Time") +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
                position=position_dodge(.9)) +
  geom_signif(annotations = c("P = 0.002\n"),fontface = "italic",
              xmin = c(.75), xmax = c(1.20) ,y_position = c(220),
              tip_length = 0.005, vjust=0.4,textsize=5) +
  theme_cowplot(font_size = 20)+
  scale_fill_npg()

p2_po_tdf = dt_Po_tdf %>% dplyr::filter(Index == "LDL_HDL" | Index == "TC_HDL" ) %>%
  ggplot(aes(x = Index, y = Mean,  fill = Groups)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_y_continuous(limits = c(0, 8)) +
  labs(title=" TDF", subtitle = "HBeAg Positive", y = "Ratio", x = "", fill = "Time") +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
                position=position_dodge(.9)) +
  geom_signif(annotations = c("P = 0.013\n"),fontface = "italic",
              xmin = c(1.75), xmax = c(2.20) ,y_position = c(5.5),
              tip_length = 0.005, vjust=0.4,textsize=5) +
  theme_cowplot(font_size = 20)+
  scale_fill_npg()

######## Negative
####### TDF

dt_Ne_tdf = tibble(
  Groups = c("D1", "48W"),
  TC = c(167.84, 158.12),
  TG = c(91.52, 89.13),
  HDL = c(50.41, 46.33),
  LDL = c(93.98, 93.12),
  LDL_HDL = c(1.98, 2.08),
  TC_HDL= c(3.48, 3.51),
  SD_TC = c(33.12, 24.38),
  SD_TG = c(57.12, 41.83),
  SD_HDL = c(13.36, 9.52),
  SD_LDL = c(26.95,24.43),
  SD_RLDL = c(0.68, 0.64),
  SD_RTC = c(0.84, 0.69)
) %>% mutate(Groups = factor(Groups, levels=c("D1", "48W"))) %>% 
  pivot_longer(TC:TC_HDL, values_to = "Mean", names_to = "Index") %>% 
  rename(
    "TC" = SD_TC,
    "TG" = SD_TG,
    'LDL' = SD_LDL,
    'HDL' = SD_HDL,
    'LDL_HDL' = SD_RLDL,
    'TC_HDL' = SD_RTC) %>% 
  pivot_longer(TC:TC_HDL, values_to = "SD", names_to = "Index2") %>% 
  dplyr::filter(Index == Index2) %>% 
  select(-Index2) %>% 
  mutate(Index = factor(Index, levels= c("TC","TG", "LDL","HDL", "LDL_HDL", "TC_HDL"))) 

dt_Ne_taf = tibble(
  Groups = c("D1", "48W"),
  TC = c(181.49, 190.16),
  TG = c(115.1, 139.05),
  HDL = c(50.41, 47.38),
  LDL = c(108.8, 120.38),
  LDL_HDL = c(2.27, 2.69),
  TC_HDL= c(3.73, 4.21),
  SD_TC = c(36.62, 36.9),
  SD_TG = c(51.95, 74.94),
  SD_HDL = c(11.13, 11.6),
  SD_LDL = c(29.77, 33.98),
  SD_RLDL = c(0.79, 0.99),
  SD_RTC = c(0.94, 1.18)
) %>% mutate(Groups = factor(Groups, levels=c("D1", "48W"))) %>% 
  pivot_longer(TC:TC_HDL, values_to = "Mean", names_to = "Index") %>% 
  rename(
    "TC" = SD_TC,
    "TG" = SD_TG,
    'LDL' = SD_LDL,
    'HDL' = SD_HDL,
    'LDL_HDL' = SD_RLDL,
    'TC_HDL' = SD_RTC) %>% 
  pivot_longer(TC:TC_HDL, values_to = "SD", names_to = "Index2") %>% 
  dplyr::filter(Index == Index2) %>% 
  select(-Index2) %>% 
  mutate(Index = factor(Index, levels= c("TC","TG", "LDL","HDL", "LDL_HDL", "TC_HDL"))) 

p1_ne_taf = dt_Ne_taf %>% dplyr::filter(Index != "LDL_HDL" & Index != "TC_HDL" ) %>%
  ggplot(aes(x = Index, y = Mean,  fill = Groups)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_y_continuous(limits = c(0, 280)) +
  labs(title="TAF", subtitle = "HBeAg Negative", y = "mg/dL", x = "", fill = "Time") +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
                position=position_dodge(.9)) +
  geom_signif(annotations = c("P = 0.004\n", "P = 0.003\n", "P < 0.001\n", "P = 0.01\n"),fontface = "italic",
              xmin = c(0.75, 1.75, 2.75, 3.75), xmax = c(1.20, 2.20, 3.20, 4.20 ) ,y_position = c(240, 240, 180, 80),
              tip_length = 0.005, vjust=0.4,textsize=5) +
  theme_cowplot(font_size = 20)+
  scale_fill_npg()

p2_ne_taf = dt_Ne_taf %>% dplyr::filter(Index == "LDL_HDL" | Index == "TC_HDL" ) %>%
  ggplot(aes(x = Index, y = Mean,  fill = Groups)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_y_continuous(limits = c(0, 8)) +
  labs(title="TAF", subtitle = "HBeAg Negative", y = "Ratio", x = "", fill = "Time") +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
                position=position_dodge(.9)) +
  geom_signif(annotations = c("P < 0.001\n", "P < 0.001\n"),fontface = "italic",
              xmin = c(0.75,1.75), xmax = c(1.20, 2.20) ,y_position = c(4.5, 6.0),
              tip_length = 0.005, vjust=0.4,textsize=5) +
  theme_cowplot(font_size = 20)+
  scale_fill_npg()

p1_ne_tdf = dt_Ne_tdf %>% dplyr::filter(Index != "LDL_HDL" & Index != "TC_HDL" ) %>%
  ggplot(aes(x = Index, y = Mean,  fill = Groups)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_y_continuous(limits = c(0, 250)) +
  labs(title="TDF", subtitle = "HBeAg Negative", y = "mg/dL", x = "", fill = "Time") +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
                position=position_dodge(.9)) +
  geom_signif(annotations = c("P = 0.02\n"),fontface = "italic",
              xmin = c(3.75), xmax = c(4.20) ,y_position = c(80),
              tip_length = 0.005, vjust=0.4,textsize=5) +
  theme_cowplot(font_size = 20)+
  scale_fill_npg()

p2_ne_tdf = dt_Ne_tdf %>% dplyr::filter(Index == "LDL_HDL" | Index == "TC_HDL" ) %>%
  ggplot(aes(x = Index, y = Mean,  fill = Groups)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_y_continuous(limits = c(0, 8)) +
  labs(title=" TDF", subtitle = "HBeAg Negative", y = "Ratio", x = "", fill = "Time") +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
                position=position_dodge(.9)) +
  # geom_signif(annotations = c("P < 0.001\n"),fontface = "italic",
  #             xmin = c(0.75), xmax = c(1.20) ,y_position = c(4.5),
  #             tip_length = 0.005, vjust=0.4,textsize=6) +
  theme_cowplot(font_size = 20)+
  scale_fill_npg()

######## 合并图片
######### Positive
(p1_po_tdf + p2_po_tdf) / (p1_po_taf + p2_po_taf) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A")

ggsave("HBeAg_P_48weeks_Bar.png", plot = last_plot(), device = "png", path = "../../Output_for_publication/New20240115/", width = 12,
       height = 10, units = "in", dpi = 300)


(p1_ne_tdf + p2_ne_tdf) / (p1_ne_taf + p2_ne_taf) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A")

ggsave("HBeAg_N_48weeks_Bar.png", plot = last_plot(), device = "png", path = "../../Output_for_publication/New20240115/", width = 12,
       height = 10, units = "in", dpi = 300)

################################################################################
## 基线1:1匹配
m.out = matchit(grp ~ 性别+年龄+甘油三酯+总胆固醇+低密度脂蛋白+高密度脂蛋白, data = lis |> dplyr::filter(Time == "D01"),
                method = "nearest", replace = F, distance = "logit", 
                ratio = 1, caliper = 0.2) 

summary(m.out)

m.data = m.out %>% match.data() %>% 
  as.data.frame() %>% 
  select(-c(姓名, distance:subclass)) 

m.data$Time %>% table()

m.data %>% 
  twogrps(gvar = 'grp', 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) #PSM之后基线数据比较

## 查看匹配后FIB4的比较
fn = m.out %>% match.data() %>% 
  as.data.frame() %>% pull(姓名)

bas %>% dplyr::filter(姓名 %in% fn) %>% 
  twogrps(gvar = 'grp', 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) 

exc = bas %>% dplyr::filter(姓名 %in% fn) %>% 
  twogrps(gvar = 'grp', 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) %>% .[["Table"]]

write_xlsx(exc, "../../Output_for_publication/New20240115/PSM_baseline_48weeks.xls")

####### 合并数据

m.data = m.out %>% match.data() %>% 
  as.data.frame() 

fn = m.data %>% 
  pull(姓名) 

m.data = lis %>% 
  dplyr::filter(Time == "D360") %>% 
  dplyr::filter(姓名 %in% fn) %>% 
  bind_rows(m.data) %>% 
  select(-c(distance:subclass)) %>% 
  arrange(姓名,Time)

m.data$Time %>% table()

###### At 48 weeks comparison

m.data %>% 
  dplyr::filter(Time=="D360") %>% 
  dplyr::select(-姓名) %>% 
  twogrps(gvar = 'grp', 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) 

exc = m.data %>% 
  dplyr::filter(Time=="D360") %>% 
  dplyr::select(-姓名) %>% 
  twogrps(gvar = 'grp', 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) %>% .[["Table"]]

###### 保存数据
write_xlsx(exc, "../../Output_for_publication/New20240115/PSM_At_48weeks.xls")

###### TAF vs TDF delta At 48 Weeks
diff = function(x){
  fn = x - first(x)
  return(fn)
}

m.data %>% 
  group_by(姓名) %>%
  mutate(across(甘油三酯:尿素, ~ diff(.))) %>%
  mutate(across(总胆红素:eGFR, ~ diff(.))) %>% 
  mutate(across(TC_HDL:LDL_HDL, ~ diff(.))) %>% 
  slice(2) %>% 
  ungroup() %>% 
  select(-姓名) %>% 
  as.data.frame() %>% 
  twogrps(gvar = 'grp', 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) 

###### 保存数据

exc = m.data %>% 
  group_by(姓名) %>%
  mutate(across(甘油三酯:尿素, ~ diff(.))) %>%
  mutate(across(总胆红素:eGFR, ~ diff(.))) %>% 
  mutate(across(TC_HDL:LDL_HDL, ~ diff(.))) %>% 
  slice(2) %>% 
  ungroup() %>% 
  select(-姓名) %>% 
  as.data.frame() %>% 
  twogrps(gvar = 'grp', 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) %>% .[["Table"]]

# write_xlsx(exc, "../../Output_for_publication/New20240115/PSM_At_48weeks_changes.xls")

################################################################################
## 96 weeks
# load('../../Output/df_impu_1_720_221106.dta')
load('../../Output/df_impu_1_720_231202.dta')

set.seed(123)

lis = df_impu[[100]] %>% 
  # mutate(eGFR = ifelse(性别 == "女", 186*血肌酐^-1.154*年龄^-0.203*0.742, 186*血肌酐^-1.154*年龄^-0.203))%>% ## 由于未记录出处，已不知道该公式的出处
  mutate(血肌酐 = 血肌酐*0.0113) %>% 
  mutate(eGFR = case_when(
    血肌酐 <= 0.7 & 性别 == "女" ~ 142*(血肌酐/0.7)^(-0.241)*0.9938^年龄*1.012, ## 采用2021 CKD-EPI公式
    血肌酐 > 0.7 & 性别 == "女" ~ 142*(血肌酐/0.7)^(-1.2)*0.9938^年龄*1.012,
    血肌酐 <= 0.9 & 性别 == "男" ~ 142*(血肌酐/0.9)^(-0.302)*0.9938^年龄*1,
    血肌酐 > 0.9 & 性别 == "男" ~ 142*(血肌酐/0.9)^(-1.2)*0.9938^年龄*1
  )) %>% 
  mutate(乙型肝炎病毒e抗原检测 = ifelse(乙型肝炎病毒e抗原检测 < 0.1, 'Negative', 'Positive')) %>% 
  mutate(乙型肝炎病毒e抗体检测 = ifelse(乙型肝炎病毒e抗体检测 > 0, 'Positive', 'Negative')) %>% 
  mutate(E抗原血清转化 = ifelse(乙型肝炎病毒e抗原检测 == 'Negative' & 乙型肝炎病毒e抗体检测 == 'Positive', "Yes", "No")) %>%
  mutate(HBVDNA定量 = ifelse(HBVDNA定量 <=500, "Negative", "Positive")) %>% 
  mutate(
    甘油三酯 = 甘油三酯*88.57,
    总胆固醇 = 总胆固醇*38.67,
    高密度脂蛋白 = 高密度脂蛋白*38.67,
    低密度脂蛋白 = 低密度脂蛋白*38.67,
    TC_HDL = 总胆固醇/高密度脂蛋白,
    LDL_HDL = 低密度脂蛋白/高密度脂蛋白,
  ) %>% 
  dplyr::filter(年龄 > 18) %>% 
  mutate(Time = factor(Time, levels=c("D01", 'D720'))) %>% 
  dplyr::filter(grp != "Trans") %>%
  mutate(grp = fct_drop(grp)) %>% 
  mutate(grp = factor(grp, levels = c("TDF","TAF"))) 

lis %>% dplyr::filter(Time == "D720") %>% 
  count(grp) ## 查看有完整数据的患者

lis %>% dplyr::filter(Time == "D01") %>% 
  count(HBVDNA定量)

lis$Time %>% table()

###### 补充基线血小板的数据

load("../clean_blood.dta")

fn = lis %>% 
  pull(姓名) %>% unique() 

plt = blood %>% 
  dplyr::select(患者, 日期, 血小板) %>% 
  mutate(日期 = as.Date(日期),
         '姓名' = 患者)%>% 
  dplyr::filter(患者 %in% fn) %>%
  group_by(患者) %>% 
  arrange(日期) %>%
  mutate(Time = 'D01') %>% 
  slice(1) %>% 
  ungroup() %>% 
  dplyr::select(姓名, Time, 血小板)

bas = left_join(lis, plt, by = c('姓名', 'Time')) %>% 
  dplyr::filter(Time == "D01") %>% 
  impute_mean_at(.vars = '血小板') %>%  ## 使用均值进行了填补
  mutate(FIB4 = (年龄*天门冬氨酸氨基转移酶)/(血小板*丙氨酸氨基转移酶^0.5)
  ) %>% 
  mutate(FIB4 = case_when(FIB4 < 1.45 ~ "Normal",
                          FIB4 >3.25 ~ "Fibrosis",
                          .default = "Median")) 
table(bas$FIB4)
bas %>% dplyr::select(年龄, 天门冬氨酸氨基转移酶, 丙氨酸氨基转移酶, 血小板, FIB4) %>% dplyr::filter(FIB4 == "Fibrosis")

bas %>% dplyr::filter(血小板 < 150) %>% count()

bas %>% dplyr::filter(Time == "D01") %>% 
  twogrps(gvar = "grp", 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) ## 未匹配前基线比较

exc = bas %>% dplyr::filter(Time == "D01") %>% 
  twogrps(gvar = "grp", 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) %>% .[["Table"]]

#### 保存数据至新输出文件夹
write_xlsx(exc, "../../Output_for_publication/New20240115/Baseline_comparison_96weeks.xls")

#### 2-治疗前后比较(96 Weeks)

###### TDF
######## T-test
lis %>% dplyr::filter(grp == "TDF") %>% 
  twogrps(gvar = "Time", 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) ## 有部分变量不paired

exc = lis %>% dplyr::filter(grp == "TDF") %>% 
  twogrps(gvar = "Time", 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) %>% .[["Table"]] 

write_xlsx(exc, "../../Output_for_publication/New20240115/Pre_Post_T_TDF_96weeks.xls")

######## Paired T-test （需要加载修改后的函数）
lis %>% dplyr::filter(grp == "TDF") %>% 
  twoCom(gvar = "Time", 
         pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
         ShowStatistic = T) ## Paired的变量

exc = lis %>% dplyr::filter(grp == "TDF") %>% 
  twoCom(gvar = "Time", 
         pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
         ShowStatistic = T) %>% .[["Table"]]

write_xlsx(exc, "../../Output_for_publication/New20240115/Pre_Post_Paired_T_TDF_96weeks.xls")

###### TAF
######## T-test
lis %>% dplyr::filter(grp == "TAF") %>% 
  twogrps(gvar = "Time", 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) 

exc = lis %>% dplyr::filter(grp == "TAF") %>% 
  twogrps(gvar = "Time", 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) %>% .[["Table"]]

write_xlsx(exc, "../../Output_for_publication/New20240115/Pre_Post_T_TAF_96weeks.xls")

######## Paired T-test （需要加载修改后的函数）
lis %>% dplyr::filter(grp == "TAF") %>% 
  twoCom(gvar = "Time", 
         pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
         ShowStatistic = T) 

exc = lis %>% dplyr::filter(grp == "TAF") %>% 
  twoCom(gvar = "Time", 
         pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
         ShowStatistic = T) %>% .[["Table"]]

write_xlsx(exc, "../../Output_for_publication/New20240115/Pre_Post_Paired_T_TAF_96weeks.xls")

###### 血脂指标作图 

dt1 = tibble(
  Groups = c("D1", "96W"),
  TC = c( 168.86, 162.53),
  TG = c( 103.85,  104.29),
  HDL = c( 47.11, 46.52),
  LDL = c( 89.54, 92.98),
  LDL_HDL = c(2.07, 2.13),
  TC_HDL =  c(3.83, 3.67),
  SD_TC = c(35.1, 31.77),
  SD_TG = c(60.85, 62.85),
  SD_LDL = c(25.02, 24),
  SD_HDL = c(12.89, 12.12),
  SD_RLDL = c(0.91, 0.81),
  SD_RTC = c(1.31, 1.06)
) %>% mutate(Groups = factor(Groups, levels=c("D1", "96W"))) %>% 
  pivot_longer(TC:TC_HDL, values_to = "Mean", names_to = "Index") %>% 
  rename('TC' = SD_TC,
         'TG' = SD_TG,
         'LDL' = SD_LDL,
         'HDL' = SD_HDL,
         'LDL_HDL' = SD_RLDL,
         'TC_HDL' = SD_RTC) %>% 
  pivot_longer(TC:TC_HDL, values_to = "SD", names_to = "Index2") %>% 
  dplyr::filter(Index == Index2) %>% 
  select(-Index2) %>% 
  mutate(Index = factor(Index, levels= c("TC", "TG","LDL", "HDL", "LDL_HDL", "TC_HDL")))

dt2 = tibble(
  Groups = c("D1", "96W"),
  TC = c(179.06 , 183.7),
  TG = c( 118.16,  126.57),
  HDL = c( 47.03, 45.73),
  LDL = c( 98.26, 115.75),
  LDL_HDL = c(2.28, 2.69),
  TC_HDL =  c(4.11, 4.23),
  SD_TC = c(43.96, 32.98),
  SD_TG = c(80.02, 72),
  SD_LDL = c(31.03, 27.8),
  SD_HDL = c(12.88, 11.09),
  SD_RLDL = c(1.09, 0.9),
  SD_RTC = c(1.71, 1.15)
) %>% mutate(Groups = factor(Groups, levels=c("D1", "96W"))) %>% 
  pivot_longer(TC:TC_HDL, values_to = "Mean", names_to = "Index") %>% 
  rename('TC' = SD_TC,
         'TG' = SD_TG,
         'LDL' = SD_LDL,
         'HDL' = SD_HDL,
         'LDL_HDL' = SD_RLDL,
         'TC_HDL' = SD_RTC) %>% 
  pivot_longer(TC:TC_HDL, values_to = "SD", names_to = "Index2") %>% 
  dplyr::filter(Index == Index2) %>% 
  select(-Index2) %>% 
  mutate(Index = factor(Index, levels= c("TC", "TG","LDL", "HDL", "LDL_HDL", "TC_HDL")))

p_dta = dt1 %>% 
  dplyr::filter(Index != "LDL_HDL" & Index != "TC_HDL" )

pre_pos_tdf_1 = ggplot(p_dta, aes(x = Index, y = Mean,  fill = Groups)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_y_continuous(limits = c(0, 250)) +
  labs(title="TDF", y = "mg/dL", x = "", fill = "Time") +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
                position=position_dodge(.9)) +
  geom_signif(annotations = c("P = 0.002\n", "P = 0.008\n"),fontface = "italic",
              xmin = c(0.75, 2.75), xmax = c(1.20,3.2) ,y_position = c(220,135),
              tip_length = 0.005, vjust=0.4,textsize=5) +
  theme_cowplot(font_size = 20)+
  scale_fill_npg()

p_dta = dt1 %>% 
  dplyr::filter(Index == "LDL_HDL" | Index == "TC_HDL" )

pre_pos_tdf_2 = ggplot(p_dta, aes(x = Index, y = Mean,  fill = Groups)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_y_continuous(limits = c(0, 8)) + 
  labs(title="TDF", y = "Ratio", x = "", fill = "Time") +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
                position=position_dodge(.9)) +
  geom_signif(annotations = "P = 0.016\n",fontface = "italic",
              xmin = 1.75, xmax = 2.20 ,y_position = 5.5,
              tip_length = 0.005, vjust=0.4,textsize=5) +
  theme_cowplot(font_size = 20) +
  scale_fill_npg()

p_dta = dt2 %>% 
  dplyr::filter(Index != "LDL_HDL" & Index != "TC_HDL" )

pre_pos_taf_1 = ggplot(p_dta, aes(x = Index, y = Mean,  fill = Groups)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_y_continuous(limits = c(0, 250)) +
  labs(title="TAF", y = "mg/dL", x = "", fill = "Time") +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
                position=position_dodge(.9)) + 
  geom_signif(annotations = c("P < 0.001\n"),fontface = "italic",
              xmin = c(2.75), xmax = c(3.20) ,y_position = c(170),
              tip_length = 0.005, vjust=0.4,textsize=5) +
  theme_cowplot(font_size = 20)+
  scale_fill_npg()

p_dta = dt2 %>% 
  dplyr::filter(Index == "LDL_HDL" | Index == "TC_HDL" )

pre_pos_taf_2 = ggplot(p_dta, aes(x = Index, y = Mean,  fill = Groups)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_y_continuous(limits = c(0,8)) +
  labs(title="TAF", y = "Ratio", x = "", fill = "Time") +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
                position=position_dodge(.9)) +
  geom_signif(annotations = c( "P = 0.002\n"),fontface = "italic",
              xmin = c(0.75), xmax = c(1.20) ,y_position = c(4.5),
              tip_length = 0.005, vjust=0.4,textsize=5) +
  theme_cowplot(font_size = 20)+
  scale_fill_npg()

###### 拼图

(pre_pos_tdf_1 | pre_pos_tdf_2) / (pre_pos_taf_1 | pre_pos_taf_2) + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A")

ggsave("Pre_Post_96weeks_Bar.png", plot = last_plot(), device = "png", path = "../../Output_for_publication/New20240115/", width = 12,
       height = 10, units = "in", dpi = 300)

figure2 = (pre_pos_tdf_1 | pre_pos_tdf_2) / (pre_pos_taf_1 | pre_pos_taf_2) + 
  plot_layout(guides = "collect") 


(figure1 | figure2) + 
  plot_annotation(tag_levels = "A")

ggsave("Pre_Post_48_96weeks_Bar.png", plot = last_plot(), device = "png", path = "../../Output_for_publication/New20240115/", width = 24,
       height = 12, units = "in", dpi = 300)
################################################################################
###### 1:2匹配

m.out = matchit(grp ~ 性别+年龄+甘油三酯+总胆固醇+低密度脂蛋白+高密度脂蛋白, lis |> dplyr::filter(Time == "D01"),
                method = "nearest", replace = F, distance = "logit", 
                ratio = 2, caliper = 0.2) 
summary(m.out)
m.data = m.out %>% match.data() %>% 
  as.data.frame() %>% 
  select(-c(姓名, distance:subclass)) 

m.data$Time %>% table()

## PSM后基线
m.data %>% 
  twogrps(gvar = 'grp', 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) 

## 查看匹配后FIB4的比较
fn = m.out %>% match.data() %>% 
  as.data.frame() %>% pull(姓名)

bas %>% dplyr::filter(姓名 %in% fn) %>% 
  twogrps(gvar = 'grp', 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) 

exc = bas %>% dplyr::filter(姓名 %in% fn) %>% 
  twogrps(gvar = 'grp', 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) %>% .[["Table"]]

write_xlsx(exc, "../../Output_for_publication/New20240115/PSM_baseline_96weeks.xls")

####### 合并数据
m.data = m.out %>% match.data() %>% 
  as.data.frame() 

fn = m.data %>% 
  pull(姓名) 

m.data = lis %>% 
  dplyr::filter(Time == "D720") %>% 
  dplyr::filter(姓名 %in% fn) %>% 
  bind_rows(m.data) %>% 
  select(-c(distance:subclass)) %>% 
  arrange(姓名, Time)

m.data$Time %>% table()

###### TAF vs TDF

m.data %>% 
  dplyr::filter(Time=="D720") %>% 
  select(-姓名) %>% 
  twogrps(gvar = 'grp', 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) 

exc = m.data %>% 
  dplyr::filter(Time=="D720") %>% 
  select(-姓名) %>% 
  twogrps(gvar = 'grp', 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T)  %>% .[["Table"]]

write_xlsx(exc, "../../Output_for_publication/New20240115/PSM_96weeks.xls")

###### TAF vs TDF delta
diff = function(x){
  fn = x - first(x)
  return(fn)
}

m.data %>% 
  group_by(姓名) %>%
  mutate(across(甘油三酯:尿素, ~ diff(.))) %>%
  mutate(across(总胆红素:eGFR, ~ diff(.))) %>% 
  mutate(across(TC_HDL:LDL_HDL, ~ diff(.))) %>% 
  slice(2) %>% 
  ungroup() %>% 
  select(-姓名) %>% 
  as.data.frame() %>% 
  twogrps(gvar = 'grp', 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) 

exc = m.data %>% 
  group_by(姓名) %>%
  mutate(across(甘油三酯:尿素, ~ diff(.))) %>%
  mutate(across(总胆红素:eGFR, ~ diff(.))) %>% 
  mutate(across(TC_HDL:LDL_HDL, ~ diff(.))) %>% 
  slice(2) %>% 
  ungroup() %>% 
  select(-姓名) %>% 
  as.data.frame() %>% 
  twogrps(gvar = 'grp', 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T)  %>% .[["Table"]]

write_xlsx(exc, "../../Output_for_publication/New20240115/PSM_96weeks_changes.xls")

################################################################################
## 连续观察的患者

# rm(list = ls())

load("./Combination_data.Rdata")

#### 48 weeks data
load("../../Output/df_impu_1_360_231202.dta") 

lis_48weeks = df_impu[[99]] %>% 
  # mutate(eGFR = ifelse(性别 == "女", 186*血肌酐^-1.154*年龄^-0.203*0.742, 186*血肌酐^-1.154*年龄^-0.203))%>% ## 由于未记录出处，已不知道该公式的出处
  mutate(血肌酐 = 血肌酐*0.0113) %>% 
  mutate(eGFR = case_when(
    血肌酐 <= 0.7 & 性别 == "女" ~ 142*(血肌酐/0.7)^(-0.241)*0.9938^年龄*1.012, ## 采用2021 CKD-EPI公式
    血肌酐 > 0.7 & 性别 == "女" ~ 142*(血肌酐/0.7)^(-1.2)*0.9938^年龄*1.012,
    血肌酐 <= 0.9 & 性别 == "男" ~ 142*(血肌酐/0.9)^(-0.302)*0.9938^年龄*1,
    血肌酐 > 0.9 & 性别 == "男" ~ 142*(血肌酐/0.9)^(-1.2)*0.9938^年龄*1
  )) %>% 
  mutate(乙型肝炎病毒e抗原检测 = ifelse(乙型肝炎病毒e抗原检测 < 0.1, 'Negative', 'Positive')) %>% 
  mutate(乙型肝炎病毒e抗体检测 = ifelse(乙型肝炎病毒e抗体检测 > 0, 'Positive', 'Negative')) %>% 
  mutate(E抗原血清转化 = ifelse(乙型肝炎病毒e抗原检测 == 'Negative' & 乙型肝炎病毒e抗体检测 == 'Positive', "Yes", "No")) %>%
  mutate(HBVDNA定量 = ifelse(HBVDNA定量 <=500, "Negative", "Positive")) %>% 
  mutate(
    甘油三酯 = 甘油三酯*88.57,
    总胆固醇 = 总胆固醇*38.67,
    高密度脂蛋白 = 高密度脂蛋白*38.67,
    低密度脂蛋白 = 低密度脂蛋白*38.67) %>%
  mutate(甘油三酯 = ifelse(甘油三酯 > 1000, 137.9, 甘油三酯)) %>%  ## 删除一个离群值
  mutate(TC_HDL = 总胆固醇/高密度脂蛋白,
         LDL_HDL = 低密度脂蛋白/高密度脂蛋白) %>% 
  dplyr::filter(年龄 > 18) %>% 
  mutate(Time = factor(Time, levels=c("D01", 'D360'))) %>% 
  dplyr::filter(grp != "Trans") %>%
  mutate(grp = fct_drop(grp)) %>% 
  mutate(grp = factor(grp, levels = c("TDF","TAF")))

#### 96 Weeks

load('../../Output/df_impu_1_720_231202.dta')

lis_96weeks = df_impu[[100]] %>% 
  # mutate(eGFR = ifelse(性别 == "女", 186*血肌酐^-1.154*年龄^-0.203*0.742, 186*血肌酐^-1.154*年龄^-0.203))%>% ## 由于未记录出处，已不知道该公式的出处
  mutate(血肌酐 = 血肌酐*0.0113) %>% 
  mutate(eGFR = case_when(
    血肌酐 <= 0.7 & 性别 == "女" ~ 142*(血肌酐/0.7)^(-0.241)*0.9938^年龄*1.012, ## 采用2021 CKD-EPI公式
    血肌酐 > 0.7 & 性别 == "女" ~ 142*(血肌酐/0.7)^(-1.2)*0.9938^年龄*1.012,
    血肌酐 <= 0.9 & 性别 == "男" ~ 142*(血肌酐/0.9)^(-0.302)*0.9938^年龄*1,
    血肌酐 > 0.9 & 性别 == "男" ~ 142*(血肌酐/0.9)^(-1.2)*0.9938^年龄*1
  )) %>% 
  mutate(乙型肝炎病毒e抗原检测 = ifelse(乙型肝炎病毒e抗原检测 < 0.1, 'Negative', 'Positive')) %>% 
  mutate(乙型肝炎病毒e抗体检测 = ifelse(乙型肝炎病毒e抗体检测 > 0, 'Positive', 'Negative')) %>% 
  mutate(E抗原血清转化 = ifelse(乙型肝炎病毒e抗原检测 == 'Negative' & 乙型肝炎病毒e抗体检测 == 'Positive', "Yes", "No")) %>%
  mutate(HBVDNA定量 = ifelse(HBVDNA定量 <=500, "Negative", "Positive")) %>% 
  mutate(
    甘油三酯 = 甘油三酯*88.57,
    总胆固醇 = 总胆固醇*38.67,
    高密度脂蛋白 = 高密度脂蛋白*38.67,
    低密度脂蛋白 = 低密度脂蛋白*38.67,
    TC_HDL = 总胆固醇/高密度脂蛋白,
    LDL_HDL = 低密度脂蛋白/高密度脂蛋白,
  ) %>% 
  dplyr::filter(年龄 > 18) %>% 
  mutate(Time = factor(Time, levels=c("D01", 'D720'))) %>% 
  dplyr::filter(grp != "Trans") %>%
  mutate(grp = fct_drop(grp)) %>% 
  mutate(grp = factor(grp, levels = c("TDF","TAF"))) 

fn = lis_48weeks %>% distinct(姓名) %>%
  pull(姓名)
  
lis_add = lis_96weeks %>% dplyr::filter(姓名 %in%  fn) %>% 
  dplyr::filter(Time == "D720")

lis_cont = bind_rows(lis_48weeks, lis_add) %>% 
  dplyr::filter(.by = "姓名", n() == 3) %>% 
  arrange(姓名)

### 未匹配

load("../clean_blood.dta")

fn = lis_cont %>% 
  pull(姓名) %>% unique() 

plt = blood %>% 
  dplyr::select(患者, 日期, 血小板) %>% 
  mutate(日期 = as.Date(日期),
         '姓名' = 患者)%>% 
  dplyr::filter(患者 %in% fn) %>%
  group_by(患者) %>% 
  arrange(日期) %>%
  mutate(Time = 'D01') %>% 
  slice(1) %>% 
  ungroup() %>% 
  dplyr::select(姓名, Time, 血小板)

bas = left_join(lis_cont, plt, by = c('姓名', 'Time')) %>% 
  dplyr::filter(Time == "D01") %>% 
  impute_mean_at(.vars = '血小板') %>%  ## 使用均值进行了填补
  mutate(FIB4 = (年龄*天门冬氨酸氨基转移酶)/(血小板*丙氨酸氨基转移酶^0.5)
  ) %>% 
  mutate(FIB4 = case_when(FIB4 < 1.45 ~ "Normal",
                          FIB4 >3.25 ~ "Fibrosis",
                          .default = "Median")) 
table(bas$FIB4)
bas %>% dplyr::select(年龄, 天门冬氨酸氨基转移酶, 丙氨酸氨基转移酶, 血小板, FIB4) %>% dplyr::filter(FIB4 == "Fibrosis")

bas %>% dplyr::filter(血小板 < 150) %>% count()

bas %>% dplyr::filter(Time == "D01") %>% 
  twogrps(gvar = "grp", 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) ## 未匹配前基线比较

exc = bas %>% dplyr::filter(Time == "D01") %>% 
  twogrps(gvar = "grp", 
          pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
          ShowStatistic = T) %>% .[["Table"]]

#### 保存数据至新输出文件夹
write_xlsx(exc, "../../Output_for_publication/New20240115/Baseline_continous_96weeks.xls")


#### Draw a continuous plot

#### TDF

lis_cont %>% dplyr::filter(grp == "TDF") %>% 
  CBCgrps::multigrps(gvar = "Time",
               pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
               ShowStatistic = T)             



                               
#### 重点是后阶段的比较
lis_cont %>% dplyr::filter(Time != "D01" & grp == "TDF") %>% 
  mutate(Time = fct_drop(Time)) %>% 
  twoCom(gvar = "Time",
                     pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
                     ShowStatistic = T)       


exc = lis_cont %>% dplyr::filter(Time != "D01" & grp == "TDF") %>% 
  mutate(Time = fct_drop(Time)) %>% 
  twoCom(gvar = "Time",
         pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
         ShowStatistic = T)    %>% .[['Table']]

write_xlsx(exc, "../../Output_for_publication/New20240115/Comp_48_96_TDF.xlsx")

# Plot 
dta = tibble(
  Groups = c("D1", "48W", "96W"),
  TC = c( 168.3, 161.17, 160.31),
  TG = c( 105.66,  101.33, 104.41),
  HDL = c( 46.3, 45.43, 45.83),
  LDL = c( 89.47, 87.97, 91.54),
  LDL_HDL = c(2.12,2.07, 2.15),
  TC_HDL =  c(3.92,3.73 , 3.7),
  SD_TC = c(36.38, 32.66, 32.61),
  SD_TG = c(67.19, 64.3, 67.1),
  SD_LDL = c(25.48, 22.94, 24.41),
  SD_HDL = c(13.27, 11.57, 12.73),
  SD_RLDL = c(0.98, 0.81, 0.85),
  SD_RTC = c(1.44, 1.12, 1.13)
) %>% mutate(Groups = factor(Groups, levels=c("D1", "48W", "96W"))) %>% 
  pivot_longer(TC:TC_HDL, values_to = "Mean", names_to = "Index") %>% 
  rename('TC' = SD_TC,
         'TG' = SD_TG,
         'LDL' = SD_LDL,
         'HDL' = SD_HDL,
         'LDL_HDL' = SD_RLDL,
         'TC_HDL' = SD_RTC) %>% 
  pivot_longer(TC:TC_HDL, values_to = "SD", names_to = "Index2") %>% 
  dplyr::filter(Index == Index2) %>% 
  select(-Index2) %>% 
  mutate(Index = factor(Index, levels= c("TC", "TG","LDL", "HDL", "LDL_HDL", "TC_HDL")))

p1 = dta %>% 
  dplyr::filter(Index == "TC") %>% 
  ggplot(aes(x = Groups, y = Mean , group = Index)) + 
  geom_line() +
  geom_point(shape = 5, size = 5) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
  geom_segment(aes(x = 2, y = 170, xend = 3, yend = 170), 
               linetype = "dashed", color = "black") +
  annotate("text", x = 2.5, y = 175, label = "NS", color = "black", fontface = "italic", size = 5) +
  labs(title="TDF", subtitle = "TC", y = "mg/dL", x = "") +
  theme_cowplot(font_size = 20)
  
p2 = dta %>% 
  dplyr::filter(Index == "TG") %>% 
  ggplot(aes(x = Groups, y = Mean , group = Index)) + 
  geom_line() +
  geom_point(shape = 5, size = 5) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
  geom_segment(aes(x = 2, y = 120, xend = 3, yend = 120), 
               linetype = "dashed", color = "black") +
  annotate("text", x = 2.5, y = 128, label = "NS", color = "black", fontface = "italic", size = 5) +
  labs(title="TDF", subtitle = "TG", y = "mg/dL", x = "") +
  theme_cowplot(font_size = 20)

p3 = dta %>% 
  dplyr::filter(Index == "LDL") %>% 
  ggplot(aes(x = Groups, y = Mean , group = Index)) + 
  geom_line() +
  geom_point(shape = 5, size = 5) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
  geom_segment(aes(x = 2, y = 95, xend = 3, yend = 95), 
               linetype = "dashed", color = "black") +
  annotate("text", x = 2.5, y = 100, label = "P = 0.004", color = "black", fontface = "italic", size = 5) +
  labs(title="TDF", subtitle = "LDL", y = "mg/dL", x = "") +
  theme_cowplot(font_size = 20)

p4 = dta %>% 
  dplyr::filter(Index == "HDL") %>% 
  ggplot(aes(x = Groups, y = Mean , group = Index)) + 
  geom_line() +
  geom_point(shape = 5, size = 5) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
  geom_segment(aes(x = 2, y = 50, xend = 3, yend = 50), 
               linetype = "dashed", color = "black") +
  annotate("text", x = 2.5, y = 52, label = "NS", color = "black", fontface = "italic", size = 5) +
  labs(title="TDF", subtitle = "HDL", y = "mg/dL", x = "") +
  theme_cowplot(font_size = 20)

p5 = dta %>% 
  dplyr::filter(Index == "LDL_HDL") %>% 
  ggplot(aes(x = Groups, y = Mean , group = Index)) + 
  geom_line() +
  geom_point(shape = 5, size = 5) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
  geom_segment(aes(x = 2, y = 2.3, xend = 3, yend = 2.3), 
               linetype = "dashed", color = "black") +
  annotate("text", x = 2.5, y = 2.5, label = "P = 0.042", color = "black", fontface = "italic", size = 5) +
  labs(title="TDF", subtitle = "LDL_HDL", y = "ratio", x = "") +
  theme_cowplot(font_size = 20)

p6 = dta %>% 
  dplyr::filter(Index == "TC_HDL") %>% 
  ggplot(aes(x = Groups, y = Mean , group = Index)) + 
  geom_line() +
  geom_point(shape = 5, size = 5) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
  geom_segment(aes(x = 2, y = 4.1, xend = 3, yend = 4.1), 
               linetype = "dashed", color = "black") +
  annotate("text", x = 2.5, y = 4.3, label = "NS", color = "black", fontface = "italic", size = 5) +
  labs(title="TDF", subtitle = "TC_HDL", y = "Ratio", x = "") +
  theme_cowplot(font_size = 20)

#### 拼图

(p1 + p2)/(p3 + p4)/(p5 + p6) + plot_annotation(tag_levels = "A")

ggsave("Cont_TDF.png", plot = last_plot(), device = "png", path = "../../Output_for_publication/New20240115/", width = 12,
       height = 12, units = "in", dpi = 300)


#### TAF

lis_cont %>% dplyr::filter(grp == "TAF") %>% 
  CBCgrps::multigrps(gvar = "Time",
                     pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
                     ShowStatistic = T)                 

#### 重点是后阶段的比较
lis_cont %>% dplyr::filter(Time != "D01" & grp == "TAF") %>% 
  mutate(Time = fct_drop(Time)) %>% 
  twoCom(gvar = "Time",
         pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
         ShowStatistic = T)       


exc = lis_cont %>% dplyr::filter(Time != "D01" & grp == "TAF") %>% 
  mutate(Time = fct_drop(Time)) %>% 
  twoCom(gvar = "Time",
         pnormtest = 0, skewvar = c("乙型肝炎病毒表面抗原定量","丙氨酸氨基转移酶","天门冬氨酸氨基转移酶", "谷氨酰转肽酶"),
         ShowStatistic = T)  %>% .[["Table"]]

write_xlsx(exc, "../../Output_for_publication/New20240115/Comp_48_96_TAF.xlsx")

#### Plot

dta = tibble(
  Groups = c("D1", "48W", "96W"),
  TC = c( 180.87, 186.61, 185.79),
  TG = c( 117.88,  133.85, 131.84),
  HDL = c( 47.82, 48.67, 45.7),
  LDL = c( 97.91, 120.89, 117.31),
  LDL_HDL = c(2.16, 2.61, 2.71),
  TC_HDL =  c(3.98, 3.97, 4.27),
  SD_TC = c(45.91, 34.11, 33.82),
  SD_TG = c(78.47, 88.26, 76.2),
  SD_LDL = c(32.1, 31.39, 28.4),
  SD_HDL = c(11.61, 10.36, 10.95),
  SD_RLDL = c(0.88, 0.91, 0.88),
  SD_RTC = c(1.5, 0.99, 1.15)
) %>% mutate(Groups = factor(Groups, levels=c("D1", "48W", "96W"))) %>% 
  pivot_longer(TC:TC_HDL, values_to = "Mean", names_to = "Index") %>% 
  rename('TC' = SD_TC,
         'TG' = SD_TG,
         'LDL' = SD_LDL,
         'HDL' = SD_HDL,
         'LDL_HDL' = SD_RLDL,
         'TC_HDL' = SD_RTC) %>% 
  pivot_longer(TC:TC_HDL, values_to = "SD", names_to = "Index2") %>% 
  dplyr::filter(Index == Index2) %>% 
  select(-Index2) %>% 
  mutate(Index = factor(Index, levels= c("TC", "TG","LDL", "HDL", "LDL_HDL", "TC_HDL")))

p1 = dta %>% 
  dplyr::filter(Index == "TC") %>% 
  ggplot(aes(x = Groups, y = Mean , group = Index)) + 
  geom_line() +
  geom_point(shape = 5, size = 5) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
  geom_segment(aes(x = 2, y = 190, xend = 3, yend = 190), 
               linetype = "dashed", color = "black") +
  annotate("text", x = 2.5, y = 197, label = "NS", color = "black", fontface = "italic", size = 5) +
  labs(title="TAF", subtitle = "TC", y = "mg/dL", x = "") +
  theme_cowplot(font_size = 20)

p2 = dta %>% 
  dplyr::filter(Index == "TG") %>% 
  ggplot(aes(x = Groups, y = Mean , group = Index)) + 
  geom_line() +
  geom_point(shape = 5, size = 5) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
  geom_segment(aes(x = 2, y = 145, xend = 3, yend = 145), 
               linetype = "dashed", color = "black") +
  annotate("text", x = 2.5, y = 160, label = "NS", color = "black", fontface = "italic", size = 5) +
  labs(title="TAF", subtitle = "TG", y = "mg/dL", x = "") +
  theme_cowplot(font_size = 20)

p3 = dta %>% 
  dplyr::filter(Index == "LDL") %>% 
  ggplot(aes(x = Groups, y = Mean , group = Index)) + 
  geom_line() +
  geom_point(shape = 5, size = 5) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
  geom_segment(aes(x = 2, y = 125, xend = 3, yend = 125), 
               linetype = "dashed", color = "black") +
  annotate("text", x = 2.5, y = 135, label = "NS", color = "black", fontface = "italic", size = 5) +
  labs(title="TAF", subtitle = "LDL", y = "mg/dL", x = "") +
  theme_cowplot(font_size = 20)

p4 = dta %>% 
  dplyr::filter(Index == "HDL") %>% 
  ggplot(aes(x = Groups, y = Mean , group = Index)) + 
  geom_line() +
  geom_point(shape = 5, size = 5) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
  geom_segment(aes(x = 2, y = 50, xend = 3, yend = 50), 
               linetype = "dashed", color = "black") +
  annotate("text", x = 2.5, y = 53, label = "NS", color = "black", fontface = "italic", size = 5) +
  labs(title="TAF", subtitle = "HDL", y = "mg/dL", x = "") +
  theme_cowplot(font_size = 20)

p5 = dta %>% 
  dplyr::filter(Index == "LDL_HDL") %>% 
  ggplot(aes(x = Groups, y = Mean , group = Index)) + 
  geom_line() +
  geom_point(shape = 5, size = 5) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
  geom_segment(aes(x = 2, y = 2.80, xend = 3, yend = 2.80), 
               linetype = "dashed", color = "black") +
  annotate("text", x = 2.5, y = 2.95, label = "NS", color = "black", fontface = "italic", size = 5) +
  labs(title="TAF", subtitle = "LDL_HDL", y = "Ratio", x = "") +
  theme_cowplot(font_size = 20)

p6 = dta %>% 
  dplyr::filter(Index == "TC_HDL") %>% 
  ggplot(aes(x = Groups, y = Mean , group = Index)) + 
  geom_line() +
  geom_point(shape = 5, size = 5) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
  geom_segment(aes(x = 2, y = 4.35, xend = 3, yend = 4.35), 
               linetype = "dashed", color = "black") +
  annotate("text", x = 2.5, y = 4.55, label = "P = 0.022", color = "black", fontface = "italic", size = 5) +
  labs(title="TAF", subtitle = "TC_HDL", y = "Ratio", x = "") +
  theme_cowplot(font_size = 20)

#### 拼图

(p1 + p2)/(p3 + p4)/(p5 + p6) + plot_annotation(tag_levels = "A")

ggsave("Cont_TAF.png", plot = last_plot(), device = "png", path = "../../Output_for_publication/New20240115/", width = 12,
       height = 12, units = "in", dpi = 300)

