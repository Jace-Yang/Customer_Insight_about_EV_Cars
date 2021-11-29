#rm(list=ls())

if(!require("dplyr")) install.packages("dplyr")
if(!require("magrittr")) install.packages("magrittr")
if(!require("data.table")) install.packages("data.table")
if(!require("extrafont")) install.packages("extrafont")
if(!require("htmlwidgets")) install.packages("htmlwidgets")
if(!require("readxl")) install.packages("readxl")
if(!require("extrafont")) install.packages("extrafont")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("tidyr")) install.packages("tidyr")
if(!require("feather")) install.packages("feather")
if(!require("stringr")) install.packages("stringr")
if(!require("gtable")) install.packages("gtable")
if(!require("grid")) install.packages("grid")
if(!require("openxlsx")) install.packages("openxlsx")
if(!require("plotly")) install.packages("plotly")
if(!require("broom")) install.packages("broom")
if(!require("lubridate")) install.packages("lubridate")
if(!require("tibble")) install.packages("tibble")
if(!require("purrr")) install.packages("purrr")
if(!require("reticulate")) install.packages("reticulate")
if(!require("arules")) install.packages("arules")

savetemp <- function(df){
  df %>% writeEXCEL("./杂乱的数据/temp.xlsx")
}


#### 数据清洗 ####
fillna_at <- function(df, list, replace_to){
  df %>% mutate_at(list, ~tidyr::replace_na(.x, replace_to))
}

inside_para <- "\\（[^\\]]*\\）"
INSIDE_PARA <- inside_para



#### 德勤 ####

# 删空白
trimYearsPlus <- function(df){
  
  df %<>% mutate(年度区间 = as.numeric(年度区间))
  bind_rows(
    df %>% 
      filter(年度区间<100000) %>% mutate(年度区间 = as.Date(年度区间,origin = "1899-12-30")),
    df %>% 
      filter(年度区间>=100000) %>% mutate(年度区间 = as.Date(as.character(年度区间),format = "%Y%m%d"))) -> df
  return(df)
}

trimYearsPlus_rq <- function(df){
  
  df %<>% mutate(日期 = as.numeric(日期))
  bind_rows(
    df %>% 
      filter(日期<100000) %>% mutate(日期 = as.Date(日期,origin = "1899-12-30")),
    df %>% 
      filter(日期>=100000) %>% mutate(日期 = as.Date(as.character(日期),format = "%Y%m%d"))) -> df
  return(df)
}

remove_empty <- function(dat, which = c("rows", "cols"), quiet=TRUE) {
  if (missing(which) && !missing(dat)) {
    message("value for \"which\" not specified, defaulting to c(\"rows\", \"cols\")")
    which <- c("rows", "cols")
  }
  if ((sum(which %in% c("rows", "cols")) != length(which)) && !missing(dat)) {
    stop("\"which\" must be one of \"rows\", \"cols\", or c(\"rows\", \"cols\")")
  }
  if ("rows" %in% which) {
    mask_keep <- rowSums(is.na(dat)) != ncol(dat)
    if (!quiet) {
      remove_message(dat=dat, mask_keep=mask_keep, which="rows", reason="empty")
    }
    dat <- dat[mask_keep, , drop = FALSE]
  }
  if ("cols" %in% which) {
    mask_keep <- colSums(!is.na(dat)) > 0
    if (!quiet) {
      remove_message(dat=dat, mask_keep=mask_keep, which="columns", reason="empty")
    }
    dat <- dat[, mask_keep, drop = FALSE]
  }
  dat
}


#增加代码
add_CICS4321 <- function(df){
  for(i in 4:1){
    df[[paste0("CICS",i)]] <- ""
  }
  df
}


# Func data formatting
seecom <- function(df, code){
  df %>% filter(证券代码 == code) %>% View
}
seekm <- function(df){
  df %>% filter(证券代码 == "600518") %>% View
}

seecol <- function(df){
  df %>% colnames %>% as_tibble -> see_comp
  see_comp %>% View
  see_comp
}
addposition = function(df){
  if(any(grepl('产业链位置', colnames(df)))){
    df %>% rename(c('产业链位置'='位置'))
  }else if(any(grepl('位置', colnames(df))) ){
    if(any(is.na(df$位置))){
      print("Warning! 位置有缺失！！！！用NA 填补了")
      df$位置[is.na(df$位置)] <- NA
    }
  }else{
    print("没有位置这一列！！全部用中游了")
    df["位置"] <- "中游"}
  df}

# 加年份
addyear = function(df, year="2018-12-31"){
  if(any(grepl('年度区间', colnames(df))) | any(grepl('会计区间', colnames(df))) ){
    
  }else{
    df["年度区间"] <- year}
  df}

strtonum <- function(df){
  证券代码 <- df$证券代码
  df %>%
    select(-c("证券代码")) %>%
    mutate_all(type.convert) %>%
    mutate_if(is.factor, as.character) %>%
    cbind(证券代码,.) %>% as_tibble()
}

see <- function(df,n=20){
  if(n<0){
    View(tail(df,-n))
  }else{
    View(head(df,n))
  }}

writeFeather <- function(x,str){
  colnames(x)  <- enc2utf8(colnames(x))
  write_feather(x,str)
}


cleanse <- function(a, option='tbl'){
  if(option %in% 'tbl'){
    for (i in colnames(a)) {
      a <- a %>%
        mutate(!!sym(i) := gsub("\\+", "瘕", !!sym(i))) %>%
        mutate(
          !!sym(i) := gsub("[[:space:]]|和|及|与|[[:punct:]]", "", !!sym(i))) %>%
        mutate(!!sym(i) := gsub("瘕", "+", !!sym(i)))
    }
  }else {
    a = gsub("\\+", "瘕", a)
    a = gsub("[[:space:]]|和|及|与|[[:punct:]]","",a)
    a = gsub("瘕", "+", a)
  }
  return(a)
}

CR4 <- function(a) {
  a <- a[order(a, decreasing = TRUE)]
  a <- a[!duplicated(a)][1:4]
  a <- sum(a)
}


PIclassify <- function(database) {
  r <- nrow(database)
  divi <-
    ((sum(is.na(database$CICS4)) +
        sum(is.na(database$CICS3)) +
        sum(is.na(database$CICS2)) +
        sum(is.na(database$CICS1))) / r)
  cc4 <- (sum(is.na(database$CICS4)) / r) / divi
  cc3 <- (sum(is.na(database$CICS3)) / r) / divi
  cc2 <- (sum(is.na(database$CICS2)) / r) / divi
  cc1 <- (sum(is.na(database$CICS1)) / r) / divi
  database <- database %>%
    filter(!is.na(位置)) %>%
    group_by(证券代码, 分部标准) %>%
    summarise(
      count = n(),
      C4 = n_distinct(CICS4, na.rm = TRUE),
      C4n = sum(!is.na(CICS4)),
      C3 = n_distinct(CICS3, na.rm = TRUE),
      C3n = sum(!is.na(CICS3)),
      C2 = n_distinct(CICS2, na.rm = TRUE),
      C2n = sum(!is.na(CICS2)),
      C1 = n_distinct(CICS1, na.rm = TRUE),
      C1n = sum(!is.na(CICS1)),
      inx = (C4n * C4 * cc4 / count) + (C3n * C3 * cc3 / count) +
        (C2n * C2 * cc2 / count) + (C1n * C1 * cc1 / count)
    ) %>%
    mutate(score = ifelse(inx == max(inx), 1, 0)) %>%
    filter(score == max(score)) %>%
    mutate(score = ifelse((count == max(count)), score + 1, score)) %>%
    filter(score == max(score)) %>%
    mutate(score = ifelse((分部标准 == "分产品"), score + 0.6, score)) %>%
    filter(score == max(score))
  return(database)
}

# a2: returned by last function.  a1: database
procClassify <- function(a1, a2) {
  a2 <- a2 %>%
    select("证券代码", "分部标准", "score")
  colnames(a2)[2] <- "划分建议"
  a2 <- a1 %>%
    left_join(a2, by = "证券代码")
  a2 <- a2 %>%
    filter(分部标准 == 划分建议)
  return(a2)
}

parseToNum <- function(a, b, c) { # character to numerics
  for (i1 in b:c) {
    a[[i1]] <- as.numeric(a[[i1]])
  }
  return(a)
}

exclusive_join <- function(df1, df2){
  bind_rows(
    df1 %>% anti_join(df2),
    df2 %>% anti_join(df1)
  )
}

# x1 = tibble(a = 1:3,b=letters[1:3])
# x2 = tibble(a = 2:4,c=letters[4:6])
# x1 %>% full_join(x2)
# x1 %>% exclusive_join(x2)


#要改




# trimTails <- function(x){
#     if(grepl('证券代码',colnames(x))){
#     x %<>% mutate(证券代码=str_extract(证券代码,'[0-9]+'))
#     return(x)
#     }else {
#        print('没有找到[证券代码]')
#     }
# }

trimTails <- function(x){
  if(grepl('证券代码',colnames(x)[[1]])){
    x %<>% 
      mutate(证券代码 = str_extract(证券代码,'[0-9]+')) %>% 
      mutate(证券代码 = as.character(str_pad(证券代码, width = 6, pad = "0", side = "left")))
    return(x)
  }else {
    print('没有找到[证券代码]')
  }
}


# df contains 企业代码，(6)位企业代码
addTails <- function(df) {
  list(load("../数据/给数据/最新/2. RData格式全量数据/企业数据/企业基本信息表.RData"))
  df %<>% left_join(data_entity_info %>% select(企业代码, 证券代码) %>% relocate(证券代码, .after = '企业代码'))
  df %>% filter(is.na(证券代码)) %>% select(企业代码) %>% print()
  return(df)
}


trimTails_企业代码 <- function(x){
  if('企业代码' %in% colnames(x)){
    x %<>% 
      mutate(企业代码 = str_extract(企业代码,'[0-9]+')) %>% 
      mutate(企业代码 = as.character(str_pad(企业代码, width = 6, pad = "0", side = "left")))
    return(x)
  }else {
    print('没有找到[企业代码]')
  }
}


trimYears <- function(x,n=2){
  if(grepl('年度区间',colnames(x)[[n]])){
    x %<>% 
      mutate(年度区间 = as.Date(年度区间))
    return(x)
  }else {
    x
  }
}


changeNameofCol <- function(x) {
  a <- colnames(x)
  a1 <- grep("证券编码|股票代码|公司代码|企业编码|债券代码", a)
  a2 <- grep("披露业务|产品名称", a)
  a3 <- grep("位置", a)
  a4 <- grep("机构最新名称", a)
  a5 <- grep("截止日期", a)
  a6 <- grep("会计期间", a)
  if (length(a1) > 0) {
    colnames(x)[a1] <- "证券代码"
  }
  if (length(a2) > 0) {
    colnames(x)[a2] <- "分部项目"
  }
  if (length(a3) > 0) {
    colnames(x)[a3] <- "位置"
  }
  if (length(a4) > 0) {
    colnames(x)[a4] <- "企业名称"
  }
  if (length(a5) > 0) {
    colnames(x)[a5] <- "年度区间"
  }
  if (length(a6) > 0) {
    colnames(x)[a6] <- "年度区间"
  }
  colnames(x) <- gsub('[[:space:]]|[[:punct:]]','',colnames(x) )
  return(x)
}

# 改了数据清单之后的版本
changeNameofCol2 <- function(x) {
  a <- colnames(x)
  a1 <- grep("证券代码|证券编码|股票代码|公司代码|企业编码|债券代码", a)
  
  if (length(a1) > 0) {
    colnames(x)[a1] <- "企业代码"
  }
  
  # colnames(x) <- gsub('[[:space:]]|[[:punct:]]','',colnames(x) )
  return(x)
}



# Func 彤彤专利得分
ttScore <- function(x,y1=2007,y2=2018){
  x <- x %>% filter(报表类型==1 & 申请类型 %in% c('已获得','已授权','已申请'))
  x['ref']=rowSums(x[,8:10],na.rm = T)
  x <- x %>% mutate_all(funs(replace_na(.,0)))
  x <- x %>% mutate(
    统计截止日期= as.numeric(str_sub(统计截止日期,1,4)),
    实用新型=ifelse(!is.na(专利)&专利>ref, 专利-ref, 实用新型),
    发明专利=发明专利*1,
    实用新型=实用新型*0.5,
    外观设计=外观设计*0.2,
    T发明= 发明专利,
    T其他= 实用新型 +外观设计)
  y <- x %>% mutate(统计截止日期 = 统计截止日期+10) %>% 
    filter(!申请类型=='已申请') %>%rename('T其他过期'='T其他') %>% 
    select(证券代码,统计截止日期,地区,申请类型, T其他过期)
  z <- x %>% mutate(统计截止日期 = 统计截止日期+20) %>% 
    filter(!申请类型=='已申请') %>%rename('T发明过期'='T发明') %>% 
    select(证券代码,统计截止日期,地区,申请类型, T发明过期)
  x <- x %>% left_join(y) %>% left_join(z)
  x <- x %>% mutate_all(funs(replace_na(.,0)))
  x <- x %>% mutate(
    T其他过期= -1 * T其他过期,
    T发明过期= -1 * T发明过期
  )
  result <- tibble(统计截止日期=c(y1:y2))
  x <- x %>% group_by(证券代码,统计截止日期) %>% 
    transmute(
      T获取分=ifelse(!申请类型=='已申请',T发明+T其他+T发明过期+T其他过期,0),
      T申请分=ifelse(申请类型=='已申请',0.3*(T发明+T其他),0)
    )%>% summarise(T获取分=sum(T获取分),T申请分=sum(T申请分))
  result <- result %>% left_join(x)
  result <- result %>% group_by(证券代码) %>% arrange(证券代码,统计截止日期) %>% 
    mutate(ttScore= cumsum(T获取分)+T申请分)
}

writeEXCEL <- function(df, file = filename) {
  options("openxlsx.borderColour" = "#000000")
  hs <- createStyle(
    textDecoration = "BOLD",
    fontColour = "#FFFFFF",
    fontSize = 14,
    fontName = "Microsoft YaHei",
    fgFill = "#000000",
    halign = "Left"
  )
  
  write.xlsx(
    df,
    file = file,
    borders='all',
    firstRow = TRUE,
    asTable=TRUE,
    withFilter=TRUE,
    colWidths = 50, 
    headerStyle = hs
  )
}

writeEXCELV0 <- function(df, file = filename) {
  options("openxlsx.borderColour" = "#000000")
  hs <- createStyle(
    textDecoration = "BOLD",
    fontColour = "#FFFFFF",
    fontSize = 14,
    fontName = "Microsoft YaHei",
    fgFill = "#000000",
    halign = "center"
  )
  
  write.xlsx(
    df,
    file = file,
    borders='all',
    firstRow = TRUE,
    asTable=TRUE,
    withFilter=TRUE,
    colWidths = 25, 
    headerStyle = hs
  )
}

writeExcel <- function(df, file = filename) {
  options("openxlsx.borderColour" = "#000000")
  options("openxlsx.borderStyle" = "thick")
  hs <- createStyle(
    textDecoration = "BOLD",
    fontColour = "#FFFFFF",
    fontSize = 14,
    fontName = "Microsoft YaHei",
    fgFill = "#000000",
    halign = "center"
  )
  style1 <- createStyle(
    fontColour = "#CC3333",
    fontName = "Microsoft YaHei",
    halign = "Left",
    borderStyle = 'thick'
  )
  style2 <- createStyle(
    fontColour = "#CC3333",
    fontSize = 11,
    fontName = "Microsoft YaHei",
    fgFill = "#E6E6FA"
  )
  style3 <- createStyle(
    fontColour = "#000000",
    fontSize = 11,
    fontName = "Microsoft YaHei",
  )
  wb <- createWorkbook()
  addWorksheet(wb,"sheet1")
  writeData(wb,'sheet1',df,
            rowNames = FALSE,
            colNames = TRUE,
            startCol = 1, startRow = 1, xy = NULL,
            headerStyle = hs,
            borders = 'all',
            borderStyle = 'thick'
  )
  addStyle(wb,'sheet1',style1,cols=1:17,rows=2:nrow(df),gridExpand = TRUE)
  addStyle(wb,'sheet1',style2,cols=1:17,rows=which(mod(as.numeric(df$年份),2)==1)+1,gridExpand = TRUE,stack = TRUE)
  addStyle(wb,'sheet1',style3,cols=9,rows=which(is.na(df$CICS4))+1,gridExpand = TRUE,stack = TRUE)
  addStyle(wb,'sheet1',style3,cols=10,rows=which(is.na(df$CICS3))+1,gridExpand = TRUE,stack = TRUE)
  addStyle(wb,'sheet1',style3,cols=11,rows=which(is.na(df$CICS2))+1,gridExpand = TRUE,stack = TRUE)
  addStyle(wb,'sheet1',style3,cols=12,rows=which(is.na(df$CICS1))+1,gridExpand = TRUE,stack = TRUE)
  freezePane(wb,'sheet1',firstRow = TRUE,firstCol = FALSE)
  saveWorkbook(wb,file,overwrite = TRUE)
}

writeExcelV2 <- function(df, file = filename) {
  options("openxlsx.borderColour" = "#000000")
  options("openxlsx.borderStyle" = "thin")
  hs <- createStyle(
    textDecoration = "BOLD",
    fontColour = "#FFFFFF",
    fontSize = 14,
    fontName = "Microsoft YaHei",
    fgFill = "#000000",
    halign = "center"
  )
  style1 <- createStyle(
    fontColour = "#CC3333",
    fontName = "Microsoft YaHei",
    halign = "Left",
    borderStyle = 'thin'
  )
  style2 <- createStyle(
    fontColour = "#CC3333",
    fontSize = 11,
    fontName = "Microsoft YaHei",
    fgFill = "#E6E6FA"
  )
  style3 <- createStyle(
    fontColour = "#000000",
    fontSize = 11,
    fontName = "Microsoft YaHei",
  )
  style4 <- createStyle(
    fontColour = "#CC3333",
    fontSize = 11,
    fontName = "Microsoft YaHei",
    fgFill = "#ADD5A2"
  )
  style5 <- createStyle(
    fontColour = "#CC3333",
    fontSize = 11,
    fontName = "Microsoft YaHei",
    fgFill = "#FFB6C1"
  )
  wb <- createWorkbook()
  addWorksheet(wb,"sheet1")
  writeData(wb,'sheet1',df,
            rowNames = FALSE,
            colNames = TRUE,
            startCol = 1, startRow = 1, xy = NULL,
            headerStyle = hs,
            borders = 'all',
            borderStyle = 'thin'
  )
  addStyle(wb,'sheet1',style1,cols=1:18,rows=2:nrow(df),gridExpand = TRUE)
  addStyle(wb,'sheet1',style2,cols=1:18,rows=which(mod(as.numeric(df$年份),2)==1)+1,gridExpand = TRUE,stack = TRUE)
  #addStyle(wb,'sheet1',style4,cols=1:18,rows=which(abs(df$业务毛利率)>0.8 & !grepl('未披露业务', df$业务名称))+1,gridExpand = TRUE,stack = TRUE)
  addStyle(wb,'sheet1',style3,cols=9,rows=2:nrow(df),gridExpand = TRUE,stack = TRUE)
  addStyle(wb,'sheet1',style3,cols=10,rows=which(is.na(df$CICS4))+1,gridExpand = TRUE,stack = TRUE)
  addStyle(wb,'sheet1',style3,cols=11,rows=which(is.na(df$CICS3))+1,gridExpand = TRUE,stack = TRUE)
  addStyle(wb,'sheet1',style3,cols=12,rows=which(is.na(df$CICS2))+1,gridExpand = TRUE,stack = TRUE)
  addStyle(wb,'sheet1',style3,cols=13,rows=which(is.na(df$CICS1))+1,gridExpand = TRUE,stack = TRUE)
  addStyle(wb,'sheet1',style5,cols=1:18,rows=which(df$年份==2020)+1,gridExpand = TRUE,stack = TRUE )
  freezePane(wb,'sheet1',firstRow = TRUE,firstCol = FALSE)
  saveWorkbook(wb,file,overwrite = TRUE)
}

writeExcelyearNA <- function(df, file = filename) {
  options("openxlsx.borderColour" = "#000000")
  options("openxlsx.borderStyle" = "thin")
  hs <- createStyle(
    textDecoration = "BOLD",
    fontColour = "#FFFFFF",
    fontSize = 14,
    fontName = "Microsoft YaHei",
    fgFill = "#000000",
    halign = "center"
  )
  style1 <- createStyle(
    fontColour = "#000000",
    fontSize = 11,
    fontName = "Microsoft YaHei",
    fgFill = "#E6E6FA"
  )
  wb <- createWorkbook()
  addWorksheet(wb,"sheet1")
  writeData(wb,'sheet1',df,
            rowNames = FALSE,
            colNames = TRUE,
            startCol = 1, startRow = 1, xy = NULL,
            headerStyle = hs,
            borders = 'all',
            borderStyle = 'thin'
  )
  addStyle(wb,'sheet1',style1,cols=3,rows=which(!is.na(df$'2010'))+1,gridExpand = TRUE,stack=TRUE)
  addStyle(wb,'sheet1',style1,cols=4,rows=which(!is.na(df$'2011'))+1,gridExpand = TRUE,stack=TRUE)
  addStyle(wb,'sheet1',style1,cols=5,rows=which(!is.na(df$'2012'))+1,gridExpand = TRUE,stack=TRUE)
  addStyle(wb,'sheet1',style1,cols=6,rows=which(!is.na(df$'2013'))+1,gridExpand = TRUE,stack=TRUE)
  addStyle(wb,'sheet1',style1,cols=7,rows=which(!is.na(df$'2014'))+1,gridExpand = TRUE,stack=TRUE)
  addStyle(wb,'sheet1',style1,cols=8,rows=which(!is.na(df$'2015'))+1,gridExpand = TRUE,stack=TRUE)
  addStyle(wb,'sheet1',style1,cols=9,rows=which(!is.na(df$'2016'))+1,gridExpand = TRUE,stack=TRUE)
  addStyle(wb,'sheet1',style1,cols=10,rows=which(!is.na(df$'2017'))+1,gridExpand = TRUE,stack=TRUE)
  addStyle(wb,'sheet1',style1,cols=11,rows=which(!is.na(df$'2018'))+1,gridExpand = TRUE,stack=TRUE)
  addStyle(wb,'sheet1',style1,cols=12,rows=which(!is.na(df$'2019'))+1,gridExpand = TRUE,stack=TRUE)
  freezePane(wb,'sheet1',firstRow = TRUE,firstCol = FALSE)
  saveWorkbook(wb,file,overwrite = TRUE)
}

readEXCEL <- function(path){
  sheetnames <- openxlsx::getSheetNames(path)
  input <- list()
  for(sheet in sheetnames){
    input[[sheet]] <- read.xlsx(path,sheet = sheet)
  }
  input
}

sortPosition <- function(input) {
  input$位置 <- factor(input$位置, levels = c("上游", "中游", "下游"))
  input <- input %>% arrange(位置)
}


findIndicator <- function(x,a=a1){
  x <- as.character(x)
  return(agrep(x,a,value = T,max.distance = 1))}


clean2se <- function(df){
  df %<>% 
    mutate(CICS4 = cleanse(CICS4,option = "1")) %>% 
    mutate(分部项目 = cleanse(分部项目,option = "1"))
  df
}

clean1se <- function(df){
  df %<>% 
    mutate(分部项目 = cleanse(分部项目,option = "1"))
  df}

clean3se <- function(df){
  df %<>% 
    mutate(业务名称 = cleanse(业务名称,option = "1"))
  df}


filler <- function(db){
  #### 获得数据库
  #list(load(file="../数据/合并/三张表/发债.RData"))
  #list(load(file="../数据/合并/三张表/上市.RData"))
  list(load(file ="../数据/合并/收入成本毛利/期末.RData"))
  # bind_rows(
  #   reports_发债 %>% trimTails %>% select(证券代码,年度区间,营业总收入,营业总成本),
  #   reports_上市 %>% trimTails %>% select(证券代码,年度区间,营业总收入,营业总成本)) %>% 
  #   trimYears %>% unique -> 营业总收入总成本
  
  #### 去除之前打过的标签
  #db = db_已填爬虫
  db %<>% ungroup %>% 
    select(证券代码,年度区间,项目,分部标准,分部项目,
               分部收入,收入比例,分部成本,分部毛利,毛利率) %>%
    filter(分部项目!="合计"  & 分部项目!="总计" & !str_detect(分部项目,"fill") & 分部项目!="未披露业务")
  
  db %<>% mutate(if_del=0) # 剩下的不删除
  
  
  #### 抵消变成负数
  db %<>%
    mutate(
      分部收入 = ifelse(str_detect(分部项目, "抵消|抵减|抵销|关联交易"), -abs(分部收入), 分部收入),
      收入比例 = ifelse(str_detect(分部项目, "抵消|抵减|抵销|关联交易"), -abs(收入比例), 收入比例),
      分部成本 = ifelse(str_detect(分部项目, "抵消|抵减|抵销|关联交易"), -abs(分部成本), 分部成本))
  
  #### 其中去除
  db %<>%
    ungroup() %>% rowwise %>%
    mutate(
      belong=ifelse(str_detect(分部项目,'其中'),strsplit(分部项目, "其中")[[1]][1],分部项目),
      if_qz=grepl("其中",分部项目) & !str_detect(分部项目, "抵消|抵减|抵销|关联交易"))
  
  db %>%
    group_by(证券代码, 年度区间,分部标准, belong) %>%
    mutate(
      收入比例2 = max(收入比例,na.rm=T),
      分部成本2 = ifelse(all(is.na(分部成本)), NA, max(分部成本,na.rm=T)),
      分部收入2 = max(分部收入,na.rm=T)
    ) %>% group_by(if_qz) %>% group_split() -> tempor
  
  
  if(length(tempor) == 2){
    tempor[[2]] %<>%
      group_by(证券代码,年度区间,分部标准,belong) %>%
      arrange(分部项目) %>%
      do(add_row(.)) %>%
      ungroup() %>%
      fill(c(证券代码, 年度区间, 分部标准, belong,if_qz, 收入比例2, 分部成本2,分部收入2), .direction = "down") %>%
      mutate(分部项目 = ifelse(is.na(分部项目),paste0(belong,"其中fill"),分部项目)) %>% 
      group_by(证券代码,年度区间,分部标准,belong) %>%
      group_modify(~{
        .x %>%
          group_by(if_qz) %>% 
          mutate(
            收入比例=ifelse(str_detect(分部项目,"其中fill"), 收入比例2-sum(收入比例,na.rm=T), 收入比例),
            分部成本=ifelse(str_detect(分部项目,"其中fill"), 分部成本2-sum(分部成本,na.rm=T), 分部成本),
            分部收入=ifelse(str_detect(分部项目,"其中fill"), 分部收入2-sum(分部收入,na.rm=T), 分部收入),
            项目=ifelse(str_detect(分部项目,"其中fill"), 3, 项目)) 
      }
      )
    db <- do.call(rbind,tempor) %>% arrange(证券代码,年度区间, 分部标准)
  }else
    db <- tempor[[1]] %>% arrange(证券代码,年度区间, 分部标准)
  
  
  db %<>% mutate(if_del = ifelse(is.na(if_del),0,if_del))
  
  # 老刘改，因为可能fill=0，有两个最大
  db %<>%
    group_by(证券代码,年度区间,分部标准,belong) %>%
    mutate(if_qz = as.integer(if_qz)) %>%
    # 删掉推算了的、总数
    mutate(if_del = ifelse(sum(if_qz)>0 & if_qz == 0, 1, 0))
  #db %<>%
  #  group_by(证券代码,年度区间,分部标准,belong) %>%
  #  mutate(if_qz = as.integer(if_qz)) %>%
  #  mutate(if_qz = ifelse(is.na(收入比例) | 收入比例 != 收入比例2, if_qz+1, if_qz)) %>%
  #  mutate(if_del = ifelse(if_qz == max(if_qz), if_del, T))
  
  #### 加未披露业务标签
  ##### 把一样的去掉
  
  db %<>% group_by(证券代码,年度区间,分部标准,分部收入,分部成本) %>%
    mutate(ref = ifelse(str_detect(分部项目, "收入|成本"),1,0)) %>% filter(ref == min(ref)) %>% select(-ref)
  
  db %<>% group_by(证券代码,年度区间,分部标准,分部收入,分部成本) %>%
    mutate(ref = 1:n()) %>% filter(ref == max(ref))  %>% select(-ref)
  
  
  
  
  db %>% ungroup %>%
    filter(if_del!=1) %>% 
    left_join(期末) %>%ungroup %>% 
    group_by(证券代码,年度区间,分部标准) %>%
    #mutate(收入比例 = ifelse(is.na(收入比例), 100*分部收入/营业总收入,收入比例)) %>% # seecom("000159")
    summarise(项目 = 3,
                分部项目= "未披露业务", 
                分部收入 = 期末营业收入 - sum(分部收入,na.rm=T),
                分部成本 = 期末营业成本 - sum(分部成本,na.rm=T),
                收入比例 = 100*分部收入/期末营业收入) %>% unique  -> db_补未披露 
  
  #### 缺失的重算
  bind_rows(db %>% ungroup%>% unique, db_补未披露%>% ungroup %>% unique) %>%
    left_join(期末) %>%
    arrange(证券代码,年度区间,分部标准) %>% ungroup %>%
    mutate(分部毛利 = 分部收入-分部成本,
               毛利率 = 100*分部毛利/分部收入,
               毛利率 = ifelse(分部收入==0,NA,毛利率),
               收入比例 = 100*分部收入/期末营业收入) %>%
    #收入比例 = ifelse(is.na(收入比例),100*分部收入/期末营业收入,收入比例)) %>%
    mutate(if_del = ifelse(is.na(if_del), 0,if_del)) %>%
    
    ##### 去掉标签
    filter(if_del != 1)  %>%
    select(证券代码,年度区间,项目,分部标准,分部项目,分部收入,收入比例,分部成本,分部毛利,毛利率) -> db_好了
  
  db_好了
}

get_check_list <- function(db_fixed){
  list(load("../数据/合并/企业名称/企业名单2019.RData")) #名单
  
  # 出名单
  db_fixed %>%
    
    # 统计人工看的次数
    group_by(证券代码,年度区间,分部标准) %>%
    summarise(人工看次数 = sum(需要人看)) %>% 
    filter(人工看次数>=1) %>%
    
    # 筛年份: 从2009开始做数据
    filter(year(年度区间)>=2009) %>%
    
    # 补充名单
    ## 可以对上的部分
    left_join(comp_list_暂用) %>%
    arrange(证券代码,年度区间) -> db_fixed_named
  db_fixed_named
}

changecics1 <- function(df){
  df %>%
    mutate(CICS1 = ifelse(CICS1 %in% "采掘","采掘业",CICS1)) %>%
    mutate(CICS1 = ifelse(CICS1 %in% "电气设备","电力系统",CICS1)) %>%
    mutate(CICS1 = ifelse(CICS1 %in% "建材装饰","建材家居",CICS1)) %>%
    mutate(CICS1 = ifelse(CICS1 %in% "软件信息技术服务","软件及信息技术服务",CICS1)) %>%
    mutate(CICS1 = ifelse(CICS1 %in% "酒店餐馆休闲","综合类行业",CICS1))}

# 这个只是打标签，不改变db
get_labeled_db <- function(db){
  db %>%
    group_by(证券代码,年度区间,分部标准) %>%
    mutate(
      # 收入成本
      收入缺或严重的成本缺 = ifelse( is.na(分部收入) | is.na(分部成本), 1, 0 ),
      收入缺或严重的成本缺 = ifelse( 收入缺或严重的成本缺==1 & is.na(分部成本) & !is.na(分部收入) & 收入比例<10, 0, 收入缺或严重的成本缺),
      
      # 没披露
      未披露业务过半 = ifelse(分部项目== "未披露业务" & 收入比例>50,1,0),
      出现业务冗余 = ifelse(分部项目== "未披露业务" & 收入比例+5<=0,1,0),
      其中项超过百分之三十 = ifelse(str_detect(分部项目,"其中fill") & 收入比例>30,1,0),
      
      # 成本错
      成本负 = ifelse(!grepl('抵|减', 分部项目) & ifelse(is.na(分部成本), 0, 分部成本) < -0.1, 1, 0),
      
      # 生成标签
      需要人看 = ifelse(收入缺或严重的成本缺==1 | 未披露业务过半 == 1 | 出现业务冗余 ==1 | 其中项超过百分之三十==1 | 成本负==1,1,0))  
  
  
  
}
# 打了标签之后 统计标签
get_check_list_new <- function(db_fixed,n){
  # 其中fill太大
  db_fixed %>% get_labeled_db %>% unique %>%
    group_by(证券代码,年度区间,分部标准) %>%
    summarise(人工看次数 = sum(需要人看)) %>% 
    filter(人工看次数>=1) %>%
    filter(year(年度区间)>=n) %>%
    left_join(comp_list_暂用) %>%
    arrange(证券代码,年度区间) -> db_fixed_named
  db_fixed_named
}

# 统计了标签之后，把错的拉出来
get_check_item <- function(db_fixed){
  db_fixed %>% get_check_list_new %>% left_join(
    db_fixed %>% get_labeled_db)
}
add_期末 <- function(a,b,c,d){
  list(load("../数据/合并/收入成本毛利/期末.RData"))
  temp = as_tibble( t(c(a,b,c,d)))
  colnames(temp) = c('证券代码','年度区间','期末营业收入','期末营业成本')
  temp %<>% mutate(年度区间 = as.Date(年度区间),
                       期末营业收入 = as.double(期末营业收入),
                       期末营业成本 = as.double(期末营业成本))
  期末 %<>% bind_rows(temp) %>% unique()
  save(期末, file ="../数据/合并/收入成本毛利/期末.RData")
  
}

change_期末 <- function(a,b,c,d){
  list(load("../数据/合并/收入成本毛利/期末.RData"))
  期末[which(期末$证券代码== a & 期末$年度区间==as.Date(b)),'期末营业收入'] = c
  期末[which(期末$证券代码== a & 期末$年度区间==as.Date(b)),'期末营业成本'] = d
  save(期末, file ="../数据/合并/收入成本毛利/期末.RData")
}

get_check_list_new_1 <- function(db_fixed,n,m){
  # 其中fill太大
  db_fixed %>% get_labeled_db %>% unique %>%
    group_by(证券代码,年度区间,分部标准) %>%
    summarise(人工看次数 = sum(需要人看)) %>% 
    filter(人工看次数>=1) %>%
    filter(year(年度区间)>=n & year(年度区间) <= m) %>%
    left_join(comp_list_暂用) %>%
    arrange(证券代码,年度区间) -> db_fixed_named
  db_fixed_named
}

统计人工工作量 <- function(idsty){
  list(load(paste0("../数据/产业链/",idsty,"/",idsty,"_死亡补救","/",idsty,"_打了标签_爬虫填补完成.RData")))
  db_已填爬虫 %>% filler %>% get_check_list_new_1(2015,2017) %>% filter(企业类型 == '上市') %>%  nrow() -> 上市
  db_已填爬虫 %>% filler %>% get_check_list_new_1(2015,2017) %>% filter(企业类型 == '发债') %>%  nrow() -> 发债
  answer = list(idsty,上市,发债)
  return(answer)
}

发债断层问题统计 <- function(idsty){
  list(load(paste0("../数据/产业链/",idsty,"/",idsty,"_死亡补救/",idsty,"_死亡补救_上市+发债_合并.RData")))
  db_security_loan_wind %>% trimTails %>% left_join(comp_list_暂用) %>% 
    filter(企业类型 == '发债') %>% 
    group_by(证券代码) %>% 
    summarise(new = max(year)) %>% 
    filter(year(new) < 2019) %>% nrow() -> 发债
  answer = list(idsty,发债)
  return(answer)
}



uncleanseCICS <- function(df){
  知识图谱 <- read_excel(paste0("../数据/合并/知识图谱/知识图谱.xlsx")) 
  
  df %<>%
    mutate(CICS4_1=ifelse(is.na(CICS4),0,CICS4)) %>% select(-CICS4) %>%
    mutate(CICS4_1=cleanse(CICS4_1, option='1')) %>%
    left_join(知识图谱 %>% mutate(CICS4_1=cleanse(CICS4,option='1')) %>%
                    select(CICS4,CICS4_1), by="CICS4_1") %>% unique %>%
    mutate(CICS4=ifelse(is.na(CICS4),CICS4_1,CICS4)) %>% select(-CICS4_1) %>%
    mutate(CICS4=ifelse(CICS4==0,NA,CICS4)) %>% relocate(CICS4,.before=CICS3)
  
  
  df %<>%
    mutate(CICS3_1=ifelse(is.na(CICS3),0,CICS3)) %>% select(-CICS3) %>%
    mutate(CICS3_1=cleanse(CICS3_1, option='1')) %>%
    left_join(知识图谱 %>% mutate(CICS3_1=cleanse(CICS3,option='1')) %>%
                    select(CICS3,CICS3_1), by="CICS3_1") %>% unique %>%
    mutate(CICS3=ifelse(is.na(CICS3),CICS3_1,CICS3)) %>% select(-CICS3_1) %>%
    mutate(CICS3=ifelse(CICS3==0,NA,CICS3)) %>% relocate(CICS3,.before=CICS2)
  
  df %<>%
    mutate(CICS2_1=ifelse(is.na(CICS2),0,CICS2)) %>% select(-CICS2) %>%
    mutate(CICS2_1=cleanse(CICS2_1, option='1')) %>%
    left_join(知识图谱 %>% mutate(CICS2_1=cleanse(CICS2,option='1')) %>%
                    select(CICS2,CICS2_1), by="CICS2_1") %>% unique %>%
    mutate(CICS2=ifelse(is.na(CICS2),CICS2_1,CICS2)) %>% select(-CICS2_1) %>%
    mutate(CICS2=ifelse(CICS2==0,NA,CICS2)) %>% relocate(CICS2,.before=CICS1)
  
  return(df)
}


conflict_fixer <- function(old){
  
  知识图谱 <- read_excel(paste0("../数据/合并/知识图谱/知识图谱.xlsx")) %>% 
    select(CICS4,CICS3,CICS2,CICS1,位置)
  
  old %<>%
    # 优先选2018人工匹
    group_by(企业代码,业务名称) %>% 
    filter(优先 == max(优先)) %>%
    select(-优先) %>%
    
    # 优先选在知识图谱
    left_join(知识图谱) %>% 
    mutate(是否在知识图谱=ifelse(!is.na(位置),1,0)) %>%
    group_by(企业代码,业务名称) %>% 
    filter(是否在知识图谱==max(是否在知识图谱)) %>% select(-位置) %>%
    
    # 优先选缺的少的
    mutate(缺1 = ifelse(is.na(CICS1),1,0),
            缺2 = ifelse(is.na(CICS2),1,0),
            缺3 = ifelse(is.na(CICS3),1,0),
            缺4 = ifelse(is.na(CICS4),1,0),
            缺 = 缺1+缺2+缺3+缺4) %>%
    filter(缺 == min(缺))  %>% select(-c(缺1:缺)) %>% ungroup
  return(old)
}

uncleanse_name <- function(df,idsty){
  load(file=paste0("../数据/产业链/",idsty,"/",idsty,"_修复数据.RData"))
  mutate(db_修复完成, 分部标准 = as.character(分部标准)) -> db
  df %<>% ungroup %>% mutate(年度区间=as.Date(paste0(年份,"-12-31"))) %>%
    mutate(分部标准=case_when(分部标准=="分行业" ~ '1',
                              分部标准=="分产品" ~ '3',
                              TRUE ~ 分部标准)) %>% 
    mutate(分部标准 = as.character(分部标准)) %>%
    left_join(db %>% mutate(业务名称=cleanse(分部项目,option='1')) %>%
                select(企业代码=证券代码,年度区间,分部标准,业务名称,分部项目), 
              by = c("企业代码", "年度区间","分部标准", "业务名称")) %>% unique %>% 
    mutate(业务显示名称=分部项目) %>% select(-分部项目,-年度区间) %>% relocate(业务显示名称,.after=业务名称) %>%
    mutate(分部标准=case_when(分部标准=="1" ~ '分行业',
                              分部标准=="3" ~ '分产品'))
  return(df)
}

### 整合知识图谱
loadnode <- function(path){
  filenames <- dir(path)
  knowledge <-read_excel(paste0(path,filenames[1])) %>% 
    select("CICS4","CICS3","CICS2","CICS1","位置")
  
  for (i in filenames[-1]) {
    # i=filenames[6]
    small <- read_excel(paste0(path,i)) %>% 
      select("CICS4","CICS3","CICS2","CICS1","位置")
    knowledge <- bind_rows(knowledge, small)
  }
  knowledge %<>% unique()
  writeEXCEL(knowledge, "../数据/合并/知识图谱/知识图谱.xlsx")
}

# 带解释和代码
loadnode_all <- function(path){
  filenames <- dir(path)
  knowledge <-read_excel(paste0(path,filenames[1]), col_types = 'text')
  
  for (i in filenames[-1]) {
    # i=filenames[6]
    small <- read_excel(paste0(path,i), col_types = 'text')
    knowledge <- bind_rows(knowledge, small)
  }
  knowledge %<>% unique()
  return(knowledge)
}

# 整合知识图谱转换
loadnode_trans <- function(path){
  # 批量合并
  dat_match <- map_dfr(list.files(path), ~read.xlsx(
    paste0(path, .x)) %>% filter(!is.na(CICS1), !is.na(CICS1新)) %>%
      select(starts_with('CICS')
      )) %>% unique()
  # 筛选映射后，在图谱里的
  knowledge <- read.xlsx("../数据/合并/知识图谱/知识图谱.xlsx") %>%
    select(starts_with('CICS')) %>%
    rename_all(~paste0(.x, '新'))
  dat_match %<>%
    semi_join(knowledge)
  # 删掉自己映射自己
  dat_match %<>% filter(!(CICS1 == CICS1新 & CICS2 == CICS2新 & CICS3 == CICS3新 & CICS4 == CICS4新))
  # 检查主键是否唯一
  if((dat_match %>% distinct(CICS4, CICS3, CICS2, CICS1) %>% nrow()) != nrow(dat_match)){
    dat_match %>%group_by(CICS4, CICS3, CICS2, CICS1) %>% filter(n() > 1) %>% view()
    print('出事了！主键不唯一！')
  }
  else
    writeEXCELV0(dat_match, '../数据/合并/知识图谱/知识图谱转换规则汇总.xlsx')
}

addNA <- function(df){
  df %<>% mutate(CICS1=na_if(CICS1,'-'),CICS2=na_if(CICS2,'-'),CICS3=na_if(CICS3,'-'),CICS4=na_if(CICS4,'-'))
  return(df)
}


# 分行业分产品冲突
full_subsets <- function(df){
  #生成4倍长的cics
  df_big = rbind(df, mutate(df, CICS4 = NA))
  df_big = rbind(df_big, mutate(df, CICS3 = NA, CICS4 = NA))
  df_big = rbind(df_big, mutate(df, CICS2 = NA, CICS3 = NA, CICS4 = NA)) %>%
    unique()
  return(df_big)
}

conflict_行业产品 <- function(df){
  df <- df %>% mutate_at(all_of(paste0('CICS', 1:4)), ~ifelse(.=='-', NA, .))
  df_分行业 <- df %>% filter(分部标准 == '分行业', !is.na(CICS1)) %>%
    select(-c(分部标准)) %>% unique()
  df_分产品 <- df %>% filter(分部标准 == '分产品', !is.na(CICS1)) %>% 
    select(-c(分部标准,业务名称)) %>% unique()
  if(nrow(df_分产品) * nrow(df_分行业) == 0)
    return(data.frame())
  else{       
    # 冲突：分行业的cics 不是分产品的子集
    df_分产品_big = full_subsets(df_分产品)
    conflict <- df_分行业 %>% anti_join(df_分产品_big)
    if(nrow(conflict) == 0)
      return(data.frame())
    else{
      # 排除：分产品的cics 是conflict的子集
      conflict$index <- 1:nrow(conflict)
      conflict_big <- full_subsets(conflict)
      conflict_排除 <- unique((df_分产品 %>% inner_join(conflict_big))$index)
      if(length(conflict_排除) == 0)
        return(conflict[, -6])
      else
        return(conflict[-conflict_排除, -6])
    }
  }
}

add_number_clean <- function(df){
  df %>%
    filter(!is.na(证券代码)) %>% unique() %>%
    mutate(分部标准 = as.character(分部标准),
               分部标准 = case_when(分部标准 == "分行业" ~ "1", 分部标准 == "分产品" ~ "3", TRUE ~ 分部标准),
               年度区间 = str_replace_all(年度区间, '/', '-'),
               年度区间备份 = as.numeric(ifelse(grepl('-', 年度区间), NA, 年度区间)), 
               年度区间备份 = ifelse(年度区间备份 > 40000, as.character(as.Date('1899-12-30') + 年度区间备份), 
                                     ifelse(年度区间备份 < 2050, paste0(年度区间备份, '-12-31'), '???')),
               年度区间 = ifelse(is.na(年度区间备份), as.character(年度区间), 年度区间备份),
               年度区间 = as.Date(年度区间)
    ) %>%
    select(-年度区间备份)
}

# 列名是日期、不用filter
add_number_clean_rq <- function(df){
  df %>%
    rename(年度区间 = 日期) %>%
    mutate(    年度区间 = str_replace_all(年度区间, '/', '-'),
           年度区间备份 = as.numeric(ifelse(grepl('-', 年度区间), NA, 年度区间)), 
           年度区间备份 = ifelse(年度区间备份 > 40000, as.character(as.Date('1899-12-30') + 年度区间备份), 
                                 ifelse(年度区间备份 < 2050, paste0(年度区间备份, '-12-31'), '???')),
           年度区间 = ifelse(is.na(年度区间备份), as.character(年度区间), 年度区间备份),
           年度区间 = as.Date(年度区间)
    ) %>%
    rename(日期 = 年度区间) %>%
    select(-年度区间备份)
}

RatioOfNA <- function(x) {
  return(sum(is.na(x)) / length(x))
}

replace_empty <- function(a) {
  a[a==""] <- NA
}

#读取python的文件
#source_python("FUNCS.py")
# read_pickle_file: 读取pickle文件

report_na <- function(df){
  print(paste("共有",nrow(df),"行，每行缺失了X个统计:"))
  print(table(apply(df, 1, function(x){sum(is.na(x))})))
  print(paste("共有",ncol(df),"列，每列缺失了X个统计:"))
  apply(df, 2, function(x){sum(is.na(x))}) %>% as.data.frame
}

checklist_all = function(path,path1) {
  tryCatch({
    #loadnode("../数据/合并/知识图谱/分行业知识图谱/")
    knowledge = read.xlsx("../数据/合并/知识图谱/知识图谱.xlsx")
    #knowledge = read_excel("../数据/产业链/电力系统/电力(1).xlsx")
    Layout = read.xlsx(path) %>% 
      select(企业代码, 企业名称, 企业类型, 年份, 分部标准, 是否做图, 业务名称,
                 业务显示名称, 业务附注, CICS4, CICS3, CICS2, CICS1, 业务营收占比, 业务营收,
                 业务成本, 业务毛利, 业务毛利率)
    Layout %>% filter(企业类型=='上市') %>% ungroup %>%
      filter(is.na(CICS1) & is.na(CICS2) & is.na(CICS3) & is.na(CICS4)) -> CICS_NA
    
    Layout %>% filter(业务名称=='未披露业务') %>% ungroup %>%
      filter(!(CICS1=='-' & CICS2=='-' & CICS3=='-' & CICS4=='-')) -> CICS_未披露
    
    Layout %>% filter(企业类型=='上市') %>% 
      filter(!(is.na(CICS1) & is.na(CICS2) & is.na(CICS3) & is.na(CICS4))) %>%
      filter(!(CICS1=='-' & CICS2=='-' & CICS3=='-' & CICS4=='-')) %>% ungroup %>% addNA -> Layout_no_na
    
    #生成4倍长的知识图谱
    knowledge3 = rbind(knowledge, mutate(knowledge, CICS4 = NA))
    knowledge3 = rbind(knowledge3, mutate(knowledge, CICS3 = NA, CICS4 = NA))
    knowledge3 = rbind(knowledge3, mutate(knowledge, CICS2 = NA, CICS3 = NA, CICS4 = NA))
    
    bind_rows(CICS_NA, CICS_未披露, anti_join(Layout_no_na, knowledge3, by = c("CICS4", "CICS3", 'CICS2', 'CICS1'))) %>% unique() %>% 
      mutate(是否错误=1) -> Wrong_List
    
    Layout %>% 
      left_join(Wrong_List %>% select(企业代码,年份,分部标准,业务名称,是否错误)) -> Layout_wrong
    output=list()
    output[['产业布局']]=data.frame(Layout_wrong)
    output[['知识图谱']]=knowledge
    writeEXCEL(output, path1)
    return(Wrong_List %>% select(企业代码,年份,分部标准,业务名称, CICS4, CICS3, CICS2, CICS1))
  } , error = function(err){
    print('Cannot find the excel')
  })
}

# 返回一个上市的企业名单
get_complist = function(type = "上市"){
  
  bind_rows(
    readEXCEL("../数据/合并/企业名单/上市+发债企业名单.xlsx")$已做企业名单 %>%
      mutate(做不做 = "做"),
    readEXCEL("../数据/合并/企业名单/上市+发债企业名单.xlsx")$未做企业名单 %>%
      mutate(做不做 = "不做")) %>%
    select(企业代码, 企业名称, 企业类型, 上市日期, 做不做, idsty, 备注) -> comp_list
  
  if(type == "上市"){
    comp_list %<>% filter(企业类型=="上市")}
  
  return(comp_list)
}

get_complist_view = function(type = "上市"){
  
  bind_rows(
    readEXCEL("../数据/合并/企业名单/上市+发债企业名单.xlsx")$已做企业名单 %>%
      mutate(做不做 = "做"),
    readEXCEL("../数据/合并/企业名单/上市+发债企业名单.xlsx")$未做企业名单 %>%
      mutate(做不做 = "不做")) %>%
    select(企业代码, 企业名称, 企业类型, 上市日期, 做不做, idsty, 备注) -> comp_list
  
  if(type == "上市"){
    comp_list %<>% filter(企业类型=="上市")}
  
  View(comp_list)
}

# 检查冲突
## 一个键———— 别用这个了，以后用checkct_at
checkct <- function(df, keys, output = F){
  if(nrow(df) == nrow(df%>% distinct(!!sym(keys))))
    print("没冲突")
  else{
    print("冲突了!!!!!!!!!!!!!!!!!!!!!")
    if(output == T)
      df %>% 
      group_by(!!sym(keys)) %>% filter(n() > 1) %>% 
      arrange(!!sym(keys)) %>% return()
  }
}
## 两个键———— 别用这个了，以后用checkct_at
checkcts <- function(df, output = F, ...){
  if(nrow(df) == nrow(df%>% distinct(...))){
    print("没冲突")
    return(df)
  }else{
    print("冲突了!!!!!!!!!!!!!!!!!!!!!")
    if(output == T)
      df %>% 
      group_by(...) %>% filter(n() > 1) %>% 
      arrange(...) %>% return()
  }
}

checkct_at <- function(df, vars){
  # vars: 需要检查冲突的分组列表，如c("企业代码", "年份")
  
  df %>% group_by_at(vars) %>% filter(n() > 1) -> conflict
  
  if(nrow(conflict) == 0){
    print("没冲突, 返回原表")
    df %>% ungroup %>% return()
    
  }else{
    print("冲突了!!!!!!!!!!!!!!!!!!")
    return(conflict) %>% arrange_at(vars) %>% return()
  }
}

## 

# 暴力解决冲突
fixct <- function(df, ...){
  if(nrow(df) == nrow(df%>% distinct(...)))
    print("没冲突")
  else{
    print("冲突了!!!!暴力解决中!!!!!!!!!!!!!!!!!")
    df %>% 
      group_by(...) %>% filter(n() == 1) %>% ungroup %>% return()
  }
}

# 检查CICS1在不在产业链里
get_wrongcics1 <- function(df){
  cics1_list = c(dir("../数据/产业链"), "海外布局", "农林牧渔","无法匹配")
  return(
    df %>% 
      mutate(ok = ifelse(CICS1%in%cics1_list,1,0 )) %>%
      filter(ok == 0) %>% select(-ok))
}

# 检查CICS1是否覆盖了各种行业
check_competecics1 <- function(df){
  cics1_list = tibble(CICS1 = c(dir("../数据/产业链"), "海外布局", "农林牧渔","无法匹配"))
  return(cics1_list %>% anti_join(df %>% distinct(CICS1)))
}

# 检查匹配的里面 错误的CICS1
check_wrongcics1 <- function(df){
  cics1_list = tibble(CICS1 = c(dir("../数据/产业链"), "海外布局", "农林牧渔","无法匹配"))
  return(df %>% distinct(CICS1)) %>% anti_join(cics1_list)
}

# 检查地址是否覆盖了各种行业
check_competeadd <- function(df){
  read_excel("./小公司/省市县.xlsx") %>% distinct(province) %>% rename(注册地=province) %>%
    bind_rows(tibble(注册地=c("海外布局"))) -> address_list
  return(address_list %>% anti_join(df %>% distinct(注册地)))
}

# 检查匹配的里面 错误的地址
check_wrongadd <- function(df){
  read_excel("./小公司/省市县.xlsx") %>% distinct(province) %>% rename(注册地=province) %>%
    bind_rows(tibble(注册地=c("海外布局"))) -> address_list
  return(df %>% distinct(注册地)) %>% anti_join(address_list)
}






# 把一个文件夹里面所有的excel拼起来
rbinder <- function(pre_path, 来源=F){
  tryCatch({
    
    files = paste0(pre_path, dir(pre_path)) # 应该可以直接df = map_dfr(files, ~read.xlsx(.x)) %>% unique #这么牛逼！
    dfs = list()
    for(file in files){
      if(来源){
        read_excel(file) %>% mutate(来源=file) -> dfs[[file]]
      }else{
        read_excel(file) -> dfs[[file]]
      }
    }
    do.call(rbind, dfs) %>% unique -> df
    return(df)
    
  }, error = function(err){
    
    print('rbind不行，换成bind_rows')
    
    files = paste0(pre_path, dir(pre_path)) # 应该可以直接df = map_dfr(files, ~read.xlsx(.x)) %>% unique #这么牛逼！
    dfs = list()
    for(file in files){
      if(来源){
        read_excel(file) %>% mutate(来源=file) -> dfs[[file]]
      }else{
        read_excel(file) -> dfs[[file]]
      }
    }
    do.call(bind_rows, dfs) %>% unique -> df
    return(df)
    
  })
}

# 把冲突横过来，然后按冲突程度排序
transpose_ct <- function(df, key, value){
  
  df %>%
    group_by(!!sym(key)) %>%
    mutate(NO = 1:n(), feq = max(NO)) %>%  #NO 第几个值，feq第几个值
    
    spread(key=NO, value = !!sym(value)) %>% 
    rename_at(vars(-1, -2), ~paste0("NO.",.x)) %>%
    arrange(-feq, !!sym(key)) %>% select(-feq) %>% return()
}

# 把distinct的东西整理成一个list
distinct_list <- function(df, col){
  df %<>% distinct(!!sym(col)) %>% as.data.frame()
  return(df[,1])
  
}


# 数据分列拉长————废了 直接separate_rows就可以了
sep_pivot_longer <- function(df, col,sep = "~"){
  df %>% ungroup %>% distinct(!!sym(col)) %>% nrow -> range
  df %>%
    separate(!!sym(col),sep = sep,into=as.character(1:range)) %>%
    pivot_longer(cols = 2:(range+1), 
                 names_to = "temp",
                 values_to = col) %>%
    filter(!is.na(!!sym(col)))  %>%
    select(-temp) %>% return()
}

# 类似python的tolist
to_list <- function(df){
  df %<>% as.data.frame()
  return(df[,1])
}

# 把target_col里面 能识别出海外布局规则的,output_col干成海外布局(CICS1/地址啥的)
cics1_match_foreign <- function(df, target_col="小公司", output_col="CICS1", safematch=F){
  
  readEXCEL("./小公司/国家中英文对照表.xlsx")$用-> foreign
  foreign_1 =  foreign %>% distinct_list("国家中文名")
  foreign_2 =  foreign %>% distinct_list("大洲")
  foreign_3 = readEXCEL("./小公司/国家中英文对照表.xlsx")$海外mark %>% distinct_list("mark")
  
  
  foreign_reg = paste0(c(foreign_1, foreign_2,foreign_3),collapse = "|")
  chinese_reg <- "[\u4e00-\u9fa5]+"
  
  ## 一个中文都找不到、或者找到了海外关键词，直接匹海外布局
  df %>% ungroup %>%
    mutate(!!sym(output_col) := ifelse(!grepl(chinese_reg, !!sym(target_col)) | grepl(foreign_reg, !!sym(target_col)), "海外布局", !!sym(output_col))) %>%
    unique %>%
    return()
}

# 换汤不换药
address_match_foreign <- function(df, target_col="小公司", output_col="注册地", safematch=T){
  return(cics1_match_foreign(df, target_col=target_col, output_col=output_col, safematch))
}

# 打好优先这个tag之后，去解决key列的冲突
fixct_byyx <- function(df, ..., fill = F){
  
  if(is.numeric(fill)){ df %<>% ungroup %>%  mutate_at("优先", funs(replace_na(.,fill))) }
  
  df %>% group_by(...) %>% filter(优先==max(优先)) %>% select(-优先) %>% ungroup %>% return()}

# CICS1带了优先词的优先 ...表示key
cics1_keephigher <- function(df, ..., 优先词){
  df %>% ungroup %>%
    mutate(优先 = ifelse(CICS1 == 优先词, 1, 0)) %>%
    fixct_byyx(...)  %>% return()
}

# CICS1带了落后词的落后 ...表示key
cics1_droplower <- function(df, ..., 落后词){
  df %>% ungroup %>%
    mutate(优先 = ifelse(CICS1 == 落后词, -1, 0)) %>%
    fixct_byyx(...)  %>% return()
}

# 某个列带了落后词的落后 ...表示key
address_keephigher <- function(df, ..., 优先词){
  df %>% ungroup %>%
    mutate(优先 = ifelse(注册地 == 优先词, 1, 0)) %>%
    fixct_byyx(...)  %>% return()
}

address_droplower <- function(df, ..., 落后词){
  df %>% ungroup %>%
    mutate(优先 = ifelse(注册地 == 落后词, -1, 0)) %>%
    fixct_byyx(...)  %>% return()
}

# 无视一个列去做unique
unique_ignore <- function(df, ignore_col){
  df %>% select(-!!sym(ignore_col)) %>% unique %>% return()
}

# 批量正则匹配关键词，留下匹配成功的
keyword_mapper<- function(df, fullword_col, mapper, key_word_col, drop_yx=T){
  # df = scomp_cics1p_maybect %>% rename(CICS1_old = CICS1) 
  # fullword_col = "小公司"
  # mapper = nlmy_mapper
  # key_word_col = "小公司关键词"
  
  # 制作大矩阵
  df %>% distinct_list(col = fullword_col) -> companylist
  companylist %>% length() -> company_num
  mapper %>% nrow -> mapper_num
  
  mapper[rep(seq_len(nrow(mapper)), company_num),] -> full_mapper
  rep(companylist, each = mapper_num) -> full_companylist
  
  # 批量计算
  full_mapper %<>%
    # 提取关键词
    add_column(!!sym(fullword_col) := full_companylist,.before = key_word_col) %>%
    rowwise() %>%
    filter(grepl(!!sym(key_word_col), !!sym(fullword_col))) %>% ungroup %>% unique
  if(drop_yx){
    # 使用优先规则去冲突
    full_mapper %>% fixct_byyx(!!sym(fullword_col)) %>% return()
  }else{
    full_mapper %>% return()
  }
}



# 统计每一列都有哪些值（类似Excel的筛选）
sumcol <- function(df){
  df %>% colnames -> cols
  output = list()
  
  for(col in cols){
    df %>% select(!!sym(col)) %>% table(exclude = "None") %>% as_tibble %>%
      arrange(-n) -> temp
    colnames(temp) <- c(col, "出现频率")
    temp -> output[[col]]
  }
  
  return(output)
}

# 获得今天的日期
get_today <- function(){
  today = Sys.time()
  date = str_split(today, " ")[[1]][1]
  return(gsub("-", "", date))}
