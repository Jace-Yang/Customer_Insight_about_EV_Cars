setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("FUNCS.R")
source("FUNCS_plotly.R")
source("FUNCS_ggplot2.R")

library(esquisse)
library(corrplot)       # used for making correlation plot.
library(ggplot2)        # used for plotting
library(psych)          # used for Factor Analysis
library(car)            # used for calculating VIF, to check multicollinearity
library(factoextra) # clustering algorithms & visualization
# library(caTools)        # used for partitioning the data

replace_na <- tidyr::replace_na

renew_data <- function(){
  read_excel("data/2021_04_29_02_11_56.xlsx") %>% slice(2:n()) %>% mutate_all(~ifelse(is.na(.x), "-",.x))}

#### 数据导入 ####

factor_to_int <- function(x) as.numeric(as.character(x))

# 知识
knowledge_raw = rev(c(
  "非常了解",
  "比较了解",
  "只了解些大概",
  "只是听过，不了解",
  "没怎么听过，不了解",
  "完全不了解也没听过"))
knowledge_value = 0:5

encode_knowledge <- function(x){
  factor_to_int(
  factor(x,
         levels = knowledge_raw,
         labels = knowledge_value))
}

decode_knowledge <- function(x){
  factor(x,
         levels = knowledge_value,
         labels = knowledge_raw)

}
# 买车类型
cartype_raw = c(
  "传统车（汽油车、柴油车及非充电的油电混合车等）",
  "新能源车：插电式混合动力车纯电动车",
  "新能源车：纯电动车",
  "其他新能源车")

cartype_value = c(
  "传统车",
  "混动车",
  "纯电车",
  "其他新能源车"
)

encode_cartype <- function(x){
  
    factor(x,
           levels = cartype_raw,
           labels = cartype_value)
}


# 买车间隔
buygap_raw = c(
  "1个月或1个月内",
  "1个月（不含）～3个月（含）",
  "3个月（不含）～6个月（含）",
  "6个月（不含）～1年（含）",
  "1年（不含）～2年（含）",
  "2年（不含）～3年（含）",
  "3年（不含）～5年（含）",
  "5年（不含）～7年（含）",
  "7年内都没有买车打算，原因是：")

buygap_value = c(
  0.5,
  2,
  4.5,
  9,
  12+6,
  12*2+6,
  12*4,
  6*12,
  7*12
)

buygap_string = c(
  "1个月或1个月内",
  "1~3个月",
  "6个月～1年",
  "1～2年",
  "2～3年",
  "3～5年",
  "5～7年",
  "7年内都没有买车打算")

encode_buygap <- function(x){
  factor_to_int(
    factor(x,
           levels = buygap_raw,
           labels = buygap_value))
}

decode_buygap <- function(x){
  factor(x,
         levels = buygap_value,
         labels = buygap_string)
}

# 买车预算
budget_raw = c(
  "-",
  "<10万",
  ">=10万且<15万",
  ">=15万且<20万",
  ">=20万且<30万",
  ">=30万且<40万",
  ">=40万且<50万",
  ">=50万且<60万",
  ">=60万")
  
budget_value = c(
  0,
  5,
  12.5,
  17.5,
  25,
  35,
  45,
  55,
  65
) * 10000

encode_budget <- function(x){
  factor_to_int(
    factor(x,
           levels = budget_raw,
           labels = budget_value))
}

decode_budget <- function(x){
  factor(x,
         levels = budget_value,
         labels = budget_raw)
}

# 同意程度
agree_raw = rev(c(
  "完全同意",
  "同意",
  "有些同意",
  "认为无差异",
  "有些不同意",
  "不同意",
  "完全不同意"))
  
agree_value = rev(c(
  3,
  2,
  1,
  0,
  -1,
  -2,
  -3
))

encode_agree <- function(x){
  factor_to_int(
    factor(x,
           levels = agree_raw,
           labels = agree_value))
}

decode_agree <- function(x){
  factor(x,
         levels = agree_value,
         labels = agree_raw)
}

# 资讯获取主动度

check_raw = rev(c(
  "会时不时地主动了解",
  "基本不会主动了解，但有资讯推送时会经常查看",
  "基本不会主动了解，但有资讯推送时会偶尔会点开看",
  "不会主动了解、收到推送也会忽略"))
  
check_value = 0:3

encode_check <- function(x){
  factor_to_int(
    factor(x,
           levels = check_raw,
           labels = check_value))
}

decode_check <- function(x){
  factor(x,
         levels = check_value,
         labels = check_raw)
}


