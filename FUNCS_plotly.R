if(!require("scales")) install.packages("scales")
if(!require("scales")) install.packages("scales")
if(!require("rjson")) install.packages("rjson")
if(!require("gridExtra")) install.packages("gridExtra")
if(!require("DT")) install.packages("DT")
if(!require("ggthemes")) install.packages("ggthemes")
if(!require("sf")) install.packages("sf")
if(!require("rmapshaper")) install.packages("rmapshaper")
if(!require("RColorBrewer")) install.packages("RColorBrewer")
if(!require("networkD3")) install.packages("networkD3")
if(!require("RCurl")) install.packages("RCurl")
if(!require("ggrepel")) install.packages("ggrepel")
# if(!require("ggthemr")) install.packages("ggthemr")
if(!require("ggpubr")) install.packages("ggpubr") # ggarrange
if(!require("ggtext")) install.packages("ggtext") # 控制text的好看 https://wilkelab.org/ggtext/


#### 配色 ####
my_pick  <- c("#E69F00", "#56B4E9", "#009E73","#d9a99e",
              "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
              "#F4EDCA",  "#9a3820",  "#a7a97c")

my_pick2 <- c("#FF5A5F", "#FFB400", "#007A87", 
                   "#8CE071", "#7B0051", "#00D1C1", "#FFAA91", "#B4A76C", 
                   "#9CA299", "#565A5C", "#00A04B", "#E54C20")
fill_my_pick2 <- scale_fill_manual(values = my_pick2)

all_blues = brewer.pal(9,"Blues")[3:9]
all_spectral = brewer.pal(10,"Spectral")[c(1:5,7:10)]
blues = all_blues

# flat颜色
#ggthemr_reset()
# flat_color = as.vector(ggthemr("flat",set_theme = FALSE)$palette$swatch)
flat_color <- c(
  '#34495e',
  '#3498db', '#2ecc71', 
  '#f1c40f', '#e74c3c', 
  '#9b59b6', '#1abc9c',
  '#f39c12', '#d35400') # From https://github.com/Mikata-Project/ggthemr/blob/master/R/palettes.R
fill_flat <- scale_fill_manual(values = flat_color)
color_flat <- scale_color_manual(values = flat_color)

get_blues = function(n){
  colorRampPalette(all_blues)(n)}

get_blues_ =function(df, ...){
  
  df %>% ungroup %>%
    distinct(...) %>% nrow -> n
  colorRampPalette(all_blues)(n)}

get_spectral_ =function(df, ...){
  
  df %>% ungroup %>%
    distinct(...) %>% nrow -> n
  colorRampPalette(all_spectral)(n)}

get_blues_plus =function(df, ...){
  all_blues = brewer.pal(9,"Blues")[2:9]
  df %>% ungroup %>%
    distinct(...) %>% nrow -> n
  colorRampPalette(all_blues)(n)}

get_blues_less =function(df, ...){
  all_blues = brewer.pal(9,"Blues")[4:9]
  df %>% ungroup %>%
    distinct(...) %>% nrow -> n
  colorRampPalette(all_blues)(n)}

all_greens = brewer.pal(9,"Greens")[2:9]

get_greens = function(n){
  color = colorRampPalette(all_greens)(n)
  #show_col(color)
  return(color)}





# 返回一个更浅的颜色
get_lighter = function(color, n = 15){
  fun_color_range <- colorRampPalette(c("#FFFFFF", color))  
  my_colors <- fun_color_range(n) 
  show_col(my_colors)
  return(my_colors)
}

# 返回一个更深的颜色
get_stronger = function(color, n = 15){
  fun_color_range <- colorRampPalette(c(color,"#000000"))  
  my_colors <- fun_color_range(n) 
  show_col(my_colors)
  return(my_colors)
}

get_darker <- get_stronger

# 获得透明的背景
do_transparant <- function(pig){
  pig %>%  
    layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
           paper_bgcolor = "rgba(0, 0, 0, 0)")}
#fig_bgcolor   = "rgba(0, 0, 0, 0)"



select_list = function(df, ...){
  df %<>% select(...) %>% as.data.frame()
  return(df[,1])
  
}


data_glimpse = function(data, file=F, na.omit = T, head_num = 4){
  # head_num: 离散型的 需要展示几个
  
  get_range <- function(var, data){
    
    target = data %>% distinct(!!sym(var)) %>% filter(!is.na(!!sym(var))) 
    
    if(is.numeric(target %>% pull(1)) | is.integer(target %>% pull(1))){
      
      str1 = paste(round(range(target %>% pull(1)),2), collapse=", ")
      return(paste0("[",str1,"]"))} 
    else{
      str1 = paste0(target %>% head(head_num) %>% pull(1), collapse = "、")
      str2 = paste0("等",nrow(target),"类取值")
      return(paste0(str1,str2))}
  }
  
  get_type <- function(var, data){
    target = data %>% distinct(!!sym(var)) %>% pull(1)
    if(is.numeric(target) | is.integer(target)){return("连续")} 
    else{return("离散")}
  }
  
  temp = tibble("指标" = data %>% colnames)
  
  if(file==F){
    
    temp %<>%
      rowwise() %>%
      transmute(指标,
                  类型 = get_type(指标, data),
                  取值范围 = get_range(指标,data)) %>% ungroup
    return(temp)
  }else{
    temp = tibble("指标" = data %>% colnames)
    temp %<>%
      rowwise() %>%
      transmute(变量分类=NA,
                    变量名称 = NA,
                    变量符号,
                    类型 = get_type(指标),
                    变量含义=NA,
                    取值范围 = get_range(指标)) %>% ungroup
    write.xlsx(temp,file)
    return(temp)
  }
}



#### 数据清洗 ####
as.numeric_at <- function(df, col_list){
  df %>%
    mutate_at(col_list, ~as.double(.x))
}

as.date_at <- function(df, col_list){
  df %>%
    mutate_at(col_list, ~as.Date(.x))
}
not_all_na <- function(x) any(!is.na(x))

not_any_na <- function(x) all(!is.na(x))

drop_allna_col <- function(df){
  df %>% select(where(not_all_na))
}



deal_zero <- function(v, fill_na = T){
  v <- ifelse(v==0, '0.00', as.character(v))
  v <- ifelse(v=='0%', '0.00%', v)
  if(fill_na == T)
    v <- ifelse(is.na(v), '无数据', v)
  return(v)
}
#### 数据处理常用 ####
distinct_list <- function(df, col){
  df %<>% ungroup %>% distinct(!!sym(col)) %>% as.data.frame()
  return(df[,1])
  
}

paste_percent <- function(x){
  return(ifelse(is.na(x), NA, paste0(x, '%')))
}

clean_number <- function(df, x){
  vari <- sym(x)
  if(df_match$单位[df_match$字段名 == x] == '%')
    df %<>% mutate(!!vari := paste_percent(round(!!vari*100, 2)))
  else{
    if(df_match$单位[df_match$字段名 == x] == '万元')
      df %<>% mutate(!!vari := round(!!vari/10000, 2))
    else
      df %<>% mutate(!!vari := round(!!vari, 2))
  }
  return(df)
}

factor_at <- function(df, y_col, rev = F){
  if(!rev) levels = distinct_list(df,y_col)
  if(rev) levels = rev(distinct_list(df,y_col))
  
  df %>%
    ungroup %>%
    mutate(!!sym(y_col) := factor(!!sym(y_col), levels)) %>%
    return
}

as.factor_at <- factor_at



conut_column <- function(df, ...){
  df %>% ungroup %>%
    distinct(省份) %>% nrow
}

set_col <- function(df, colnames){
  df %>% `colnames<-`(colnames)
}



#### 前端组件 ####
get_DT <-function(df, rownames = F, default_show = 10){
  df %>%
    
    DT::datatable(
      extensions = 'Buttons', 
      class = 'display',
      rownames = rownames,
      
      editable = "cell",
      filter = 'top',
      options = list(autoWidth = T,
                     digits=2,
                     pageLength = default_show,
                     dom = 'Blfrtip',
                     
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#ffffff', 'color': '#000000'});",
                       "}"),
                     
                     # language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Chinese.json'),
                     
                     search = list(regex = TRUE, caseInsensitive = FALSE),
                     
                     buttons = list(
                       list(
                         extend = 'collection',
                         buttons = c('csv', 'excel', 'pdf'),
                         text = '下载'),
                       
                       list(
                         extend = 'copy',
                         buttons = c('copy'),
                         title = NULL, # 复制的时候不会有标题
                         text = '复制'),
                       
                       list(
                         extend = 'print',
                         buttons = c('print'),
                         text = '打印')),
                     
                     lengthMenu = c(10, 25, 50, 200, 500)))  -> DTtable
  return(DTtable)
}


get_DT <-function(df, rownames = F, default_show = 10){
  df %>%
    
    DT::datatable(
      extensions = 'Buttons', 
      class = 'display',
      rownames = rownames,
      editable = "cell",
      filter = 'top',
      options = list(autoWidth = T,
                     digits=2,
                     pageLength = default_show,
                     dom = 'Blfrtip',
                     columnDefs = 
                       list(list(className = 'dt-center', 
                                 targets = "_all")),
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#ffffff', 'color': '#000000'});",
                       "}"),
                     
                     # language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Chinese.json'),
                     
                     search = list(regex = TRUE, caseInsensitive = FALSE),
                     
                     buttons = list(
                       list(
                         extend = 'collection',
                         buttons = c('csv', 'excel', 'pdf'),
                         text = '下载'),
                       
                       list(
                         extend = 'copy',
                         buttons = c('copy'),
                         title = NULL, # 复制的时候不会有标题
                         text = '复制'),
                       
                       list(
                         extend = 'print',
                         buttons = c('print'),
                         text = '打印')),
                     
                     lengthMenu = c(10, 25, 50, 200, 500)))  -> DTtable
  return(DTtable)
}



get_empty_note <- function(p_ori, note){
  
  plot_ly() %>%
    
    layout(
      yaxis = list(showgrid = FALSE,color = 'white'),
      xaxis = list(showgrid = FALSE,color = 'white'),
      annotations = list(
        text = note,
        font = list(color = "black", 
                    size = 15,
                    family= 'Microsoft YaHei'),
        yanchor = "bottom",
        xanchor = "center",
        align = "center",
        x = 0.5,
        y = 0.5,
        showarrow = FALSE)) ->p
  
  subplot(p_ori, p, 
          nrows = 2,
          heights = c(0.8, 0.05), 
          margin = 0.09) %>%
    return
}

get_PLOT <- function(data, 
                     vairble_num,
                     measure_num,
                     title_size = 5,
                     showlegend = T){
  title = variables[vairble_num]
  measure = measures[measure_num]
  
  data %>%
    filter(Variable == title,
           Measure == measure) %>%
    select(国家 = Country, 
             年份 = Year, 
             值 = Value) %>%
    unique %>%
    
    plot_ly(
      x = ~年份,
      y = ~值,
      color =  ~国家,
      legendgroup =  ~国家,
      colors = "Paired",
      type = 'scatter',
      mode='line'
    )  %>% 
    layout( 
      annotations = list(
        text = paste0(
          vairble_num,"-",title,
          '<br>',
          '<sup>',
          measure_num,"-", measure,
          '</sup>'),
        font = list(color = "black", 
                    size = title_size,
                    family= 'Microsoft YaHei'),
        xref = "paper",
        yref = "paper",
        yanchor = "bottom",
        xanchor = "center",
        align = "center",
        x = 0.5,
        y = 1,
        showarrow = FALSE),
      margin = list(t = 100, b = 0)) -> pig
  
  if(!showlegend) pig %<>% layout(showlegend = F)
  
  return(pig %>% svg_button)
}

get_all_PLOT <- function(data, nrows = 10,  title_size = 5, judge_empty = F){
  fig_list = list()
  for(i in 1:length(variables)){
    for(j in 1:length(measures)){
      data %>% 
        filter(Variable == variables[i],
               Measure == measures[j]) %>% 
        nrow == 0 -> is_empty
      if(judge_empty & is_empty){
        x3 = 1
      }else if(i == 1 & j == 1){
        data %>% 
          get_PLOT(i, j, title_size = title_size, showlegend = T) -> fig_list[[paste0(i,"-",j)]]
      }else{
        data %>% 
          get_PLOT(i, j, title_size = title_size, showlegend = T) -> fig_list[[paste0(i,"-",j)]]
      }
      
    }
  }
  fig_list %>% subplot(nrows = nrows, shareX = T)
}

read_data <- function(filename){
  file_path = paste0("./data/", filename)
  return(read.csv(file_path) %>% as_tibble)
}

country_filter <- function(df) {
  temp = colnames(df)[1]
  df %>% 
    rename_at(1, ~"temp") %>%
    right_join(
      country_filter_df %>%
        distinct(country), by = c("temp"  = "country")) %>%
    rename_at(1, ~temp) %>%
    
    return
  
}



#### 基础plotly图 ####

get_basic_line_1 <- function(data, index_name = NA, mycolors = NA){
  
  if(is.na(mycolors)){
    n = data %>% distinct(指标类型) %>% nrow
    if(n ==1){
      colors = all_blues[6]
    }else{
      colors = colorRampPalette(all_blues)(n)  
    }
  }
  
  data %>%
    plot_ly(
      x = ~日期,
      y = ~指标值,
      color = ~指标类型,
      colors = colors,
      type = 'scatter',
      mode = 'lines+markers') %>%
    my_layout %>%
    layout(yaxis = list(title = index_name,showgrid=T,zeroline=F),
           xaxis = list(title = NA)) -> p 
  
  if(data %>% ungroup %>% distinct(指标值) %>% max(na.rm=T) <=1){
    return(p %>% layout(yaxis = list(tickformat = "%")))
  }else{
    return(p %>% layout(yaxis = list(tickformat = ".0f"))) 
  }
  
  
}

get_basic_line_2 <- function(data, index_name = NA, mycolors = NA){
  
  if(is.na(mycolors)){
    n = data %>% distinct(指标类型) %>% nrow
    if(n == 1){
      mycolors = all_blues[6]
    }else{
      mycolors = colorRampPalette(all_blues)(n)  
    }
  }
  
  data %>%
    plot_ly(
      x = ~日期,
      y = ~指标值,
      color = ~指标类型,
      colors = mycolors,
      type = 'scatter',
      mode = 'lines') %>%
    my_layout %>%
    layout(yaxis = list(title = index_name,showgrid=T,zeroline=F),
           xaxis = list(title = NA)) -> p 
  
  if(data %>% ungroup %>% distinct(指标值) %>% max(na.rm=T) <=1){
    return(p %>% layout(yaxis = list(tickformat = "%")))
  }else{
    return(p %>% layout(yaxis = list(tickformat = ".0f"))) 
  }
  
}

get_basic_line_3 <- function(data, y, index_name = NA){
  
  data %>%
    plot_ly(
      x = ~年份,
      y = y,
      type = 'scatter',
      mode = 'lines+markers') %>%
    my_layout %>%
    layout(yaxis = list(title = gsub("节点","行业",index_name)),
           xaxis = xaxis_1)  -> p 
  
  if(data %>% ungroup %>% distinct(!!sym(index_name)) %>% max(na.rm=T) <=1){
    return(p %>% layout(yaxis = list(tickformat = "%")))
  }else{
    return(p) 
  }
  
}

get_doubley_layout <- function(fig, title_left = NA, title_right = NA){
  fig %>%

    layout(barmode = 'stack',
           yaxis2 = list(
             title = title_right,
             showgrid = F,
             zeroline = F,
             automargin = T,
             overlaying = "y", 
             tickformat = "%",
             side = "right"),
           hovermode = 'compare',
           yaxis = list(spikedash="dash",
                        title = title_left,
                        zeroline = F,
                        showgrid = F),
           xaxis = list(spikedash="dash",
                        dtick = 1,
                        tickangle = -90,
                        title = NA,
                        zeroline = TRUE,
                        showgrid = F)) %>%
    #margin = list(t = 90, b = 40)) %>%
    center_legend %>% 
    svg_button %>% 
    do_transparant }
  
  get_basic_stack <- function(data, index_name = NA, mycolors = NA){
    if(is.na(mycolors)){
      mycolors = colorRampPalette(all_blues)(data %>% distinct(指标类型) %>% nrow)}
    data %>%
      plot_ly(
        x = ~日期, 
        y = ~指标值, 
        name = ~指标类型, 
        type = 'bar',
        color = ~指标类型,
        colors = mycolors,
        textposition = 'auto') %>%
      
      layout(
        xaxis = list(title = NA),
        yaxis = list(title = index_name),
        barmode = 'stack') %>% 
      my_layout-> p 
    
    if(data %>% ungroup %>% distinct(指标值) %>% max(na.rm=T) <=1){
      return(p %>% layout(yaxis = list(tickformat = "%")))
    }else{
      return(p %>% layout(yaxis = list(tickformat = ".0f"))) 
    }
    
  }
  
  
get_basic_stack_h <- function(data, index_name = NA, mycolors = NA){
    if(is.na(mycolors)){
      mycolors = colorRampPalette(all_blues)(data %>% distinct(指标类型) %>% nrow)}
    data %>%
      plot_ly(
        x = ~指标值, 
        y = ~日期, 
        orientation = 'h',
        name = ~指标类型, 
        type = 'bar',
        color = ~指标类型,
        colors = mycolors,
        textposition = 'auto') %>%
      
      layout(
        yaxis = list(title = NA),
        xaxis = list(title = index_name),
        barmode = 'stack') %>% 
      my_layout-> p 
    
    if(data %>% ungroup %>% distinct(指标值) %>% max(na.rm=T) <=1){
      return(p %>% layout(xaxis = list(tickformat = "%")))
    }else{
      return(p %>% layout(xaxis = list(tickformat = ".0f"))) 
    }
    
  }
  
  
get_basic_group <- function(data, index_name = NA, mycolors = NA){
  if(is.na(mycolors)){
    mycolors = colorRampPalette(all_blues)(data %>% distinct(指标类型) %>% nrow)}
  
  data %>%
    plot_ly(
      x = ~日期, 
      y = ~指标值, 
      name = ~指标类型, 
      type = 'bar',
      color = ~指标类型,
      colors = mycolors,
      textposition = 'auto') %>%
    
    layout(
      xaxis = list(title = NA),
      yaxis = list(title = index_name),
      barmode = 'group') %>% 
    my_layout-> p 
  
  if(data %>% ungroup %>% distinct(指标值) %>% max(na.rm=T) <=1){
    return(p %>% layout(yaxis = list(tickformat = "%")))
  }else{
    return(p %>% layout(yaxis = list(tickformat = ".0f"))) 
  }
}

get_basic_pie <- function(pie_data){
  pie_data %>%
    plot_ly(labels = ~指标类型,
            #color = ~指标类型,
            #colors = "Blues", 
            marker = list(colors = get_blues_plus(pie_data, 指标类型)),
            values = ~指标值) %>%
    add_pie(hole = 0.5)  %>%
    my_layout()
}


# 堆积果冻图
get_gd1 <- function(data, x, y, color, y_title, colors="viridis" ){
  data %>%
    plot_ly(
      type='scatter',
      x = x,
      y = y,
      color = color,
      legendgroup = color,
      colors=colors, #改颜色板
      mode='lines',
      showlegend = T,
      stackgroup='one',
      
      line = list(shape='line', width = 2)) %>%
    #line=list(shape='spline', width = 2)) %>%
    layout(
      legend=list(font = list(color = "black"),
                  y = 0.5,
                  size = 10),
      # title = list(text = paste0(com_name,"——主营业务节点占比变化"),
      #              font = list(color = "white", size = 24, family= 'Microsoft YaHei')),
      xaxis = list(title = NA,
                   spikedash="dash",
                   color = "black",
                   autorange = T,
                   zeroline = FALSE,
                   family='SimHei',
                   showgrid = F,
                   dtick = 1),
      yaxis = list(title = y_title,
                   spikedash="dash",
                   color = "black", 
                   autorange = TRUE, 
                   zeroline = FALSE,
                   showgrid = F,
                   family='SimHei',
                   tickformat="%"),
      #paper_bgcolor = "black",
      #plot_bgcolor = "black",
      uirevision = TRUE,
      margin = list(t = 40, b = 40)) %>%
    basic_layout}

# 展开果冻图
get_gd2 <- function(data, x, y, color, y_title, colors="viridis" ){
  data %>%
    plot_ly(
      type='scatter',
      x = x,
      y = y,
      color = color,
      legendgroup = color,
      colors=colors, #改颜色板
      mode='lines',
      showlegend = T,
      stackgroup='one',
      #line=list(shape='line', width = 2),
      line=list(shape='spline', width = 2)) %>%
    layout(
      legend=list(font = list(color = "black"),
                  y = 0.5,
                  size = 10),
      # title = list(text = paste0(com_name,"——主营业务节点占比变化"),
      #              font = list(color = "white", size = 24, family= 'Microsoft YaHei')),
      xaxis = list(title = NA,
                   color = "black",
                   autorange = T,
                   zeroline = FALSE,
                   family='SimHei',
                   dtick = 1),
      yaxis = list(title = y_title,
                   color = "black", 
                   autorange = TRUE, 
                   zeroline = FALSE,
                   family='SimHei'),
      #paper_bgcolor = "black",
      #plot_bgcolor = "black",
      uirevision = TRUE
    ) %>%
    layout(hovermode = 'compare',
           yaxis = list(spikedash="dash",
                        zeroline = TRUE,
                        showgrid = F),
           xaxis = list(spikedash="dash",
                        zeroline = TRUE,
                        showgrid = F)) %>%
    center_legend %>% 
    svg_button2 %>% 
    do_transparant %>% return()
}


####  调整plotly ####
my_font1 = list(color = 'black',
                family = 'sans serif',
                size = 14)

get_bar_label = function(fig, x_col, label_data, font = my_font1){
  fig %>%
    add_annotations(
      text = ifelse(
        label_data > 0.05,
        #paste0("<b>", round(label_data * 100, 0), "%","</b>"),
        paste0(round(label_data * 100, 0), "%"),
        ""),
      x = x_col,
      font = font,
      y = unlist(tapply(label_data, 
                        x_col, FUN=cumsum))-label_data/2 , 
      showarrow = F)
}

svg_button <- function(fig){
  fig %>% 
    config(toImageButtonOptions = list(
      format = "svg",
      filename = "plot",
      width = 605, # for png
      height = 300
      #width = 935, # for png
      #height = 442
    )) %>%
    return()
}

svg_button2 <- function(fig){
  fig %>% 
    config(toImageButtonOptions = list(
      format = "svg",
      filename = "plot"
    )) %>%
    return()
}


title_font = list(color = "black", 
                  size = 20,
                  family= 'Microsoft YaHei')

center_legend <- function(pig){
  pig %>%
    layout(legend = list(x = 1, y = 0.5)) %>%
    return()
}



xaxis_1 = list(
  title = "",
  tickmode='linear',
  autorange = TRUE,
  showgrid = F,
  ticks = "outside",
  tick0 = 0,
  dtick = 1,
  ticklen = 5,
  tickwidth = 2,
  tickcolor = toRGB("black"),
  #rangemode = "tozero",
  tickangle = -90)

get_date_x = function(dtick = 1){
  temp_axis = xaxis_1
  temp_axis$dtick = dtick
  temp_axis$zeroline = F
  return(temp_axis)
}

set_date_x = function(fig, dtick = 1){
  fig %>% layout(xaxis = get_date_x(dtick)) %>% no0x
}

set_date_y = function(fig, dtick = 1){
  fig %>% layout(yaxis = get_date_x(dtick)) %>% no0x
}

# 百分比
yaxis_1 = list(
  tick0 = 0,
  showgrid = F,
  tickformat = "%",
  rangemode = "tozero"
)

get_yaxis_2 = function(title){
  list(
    title = title,
    tick0 = 0,
    showgrid = F,
    #tickformat = "%",
    rangemode = "tozero"
  ) }

# 美元
htem_1 = ~ paste(
  "%{xaxis.title.text}: %{x:}年",
  "\n%{yaxis.title.text}: %{y:.2f}",
  "\n国家个数:",国家个数)

htem_2 = ~ paste(
  "%{xaxis.title.text}: %{x:}年",
  "\n%{yaxis.title.text}: %{y:.2f}")

meanline <- function(y, df, group = "年份"){
  df <- df %>% #ungroup %>% 
    group_by(!!sym(group)) %>%
    summarise(均值 = mean(!!sym(y), na.rm=T))
  return(df)
}

mline_style <- list(
  color = "black",
  width = 4, 
  dash = 'dash',
  opacity = 0.3)

get_mline <- function(color){
  temp = mline_style
  temp$color = color
  return(temp)
}

get_mean_trace <- function(fig, colname){
  fig %>%
    add_trace(
      type = "scatter",
      data = meanline(colname, who_data_small),
      x = ~年份,
      name = paste0("全球历年平均", colname),
      inherit = FALSE,
      mode = "lines",
      y = ~均值,
      line = mline_style
      
    )
}

basic_layout <- function(fig){
  fig %>%
    center_legend %>% 
    svg_button %>% 
    do_transparant 
}

no0x <- function(fig){
  fig %>% layout(
    yaxis = list(zeroline = F)  
  )
}

yesygrid <- function(fig){
  fig %>% layout(
    yaxis = list(showgrid = T)  
  )
}

my_layout <- function(fig){
  fig %>%
    layout(hovermode = 'compare',
           yaxis = list(spikedash="dash",
                        zeroline = TRUE,
                        showgrid = F),
           xaxis = list(spikedash="dash",
                        zeroline = TRUE,
                        showgrid = F)) %>%
    center_legend %>% 
    svg_button %>% 
    do_transparant %>% return()
}

my_layout2 <- function(fig){
  fig %>%
    layout(hovermode = 'compare',
           yaxis = list(spikedash="dash",
                        zeroline = TRUE,
                        showgrid = F),
           xaxis = list(spikedash="dash",
                        zeroline = TRUE,
                        showgrid = F)) %>%
    center_legend %>% 
    svg_button2 %>% 
    do_transparant %>% return()
}

showygrid <- function(fig){
  fig %>%layout(yaxis = list(showgrid = T))
}

#### 基础ggplot2图 ####
Plot_industry <- function(df, x, add_table = T){
  vari <- sym(x)
  # 文字平移距离
  (df %>% ungroup() %>% summarise(distance = (max(!!vari, na.rm =T) - min(!!vari, na.rm =T)) / 15))$distance -> dis
  
  if(df_match$单位[df_match$字段名 == x] == '%'){
    p <- df %>%
      mutate(l = deal_zero(paste_percent(round(!!vari*100, 2)), fill_na = F)) %>%
      ggplot(aes(x = 年份, y = !!vari)) +
      geom_line(color = '#046A38', size = 1) +
      geom_point(color = '#046A38', size = 1.5) +
      geom_text(aes(label = l), nudge_y = dis, size = 2.5) + 
      scale_x_continuous(breaks = 2010:2019, expand = c(0,1)) +
      scale_y_continuous(n.breaks = 6, labels = scales::percent_format(accuracy = 1)) +
      labs(x = '', y = '', title = paste0(x, '(', df_match$单位[df_match$字段名 == x], ')')) +
      theme_clean(base_family = 'STXihei') +
      theme(plot.background = element_rect(colour = "transparent", fill = "#fcfcfc"))
    
    # 统计表格
    if((add_table == T) & (df %>% filter(!is.na(!!vari)) %>% nrow() > 0)){
      df %>% 
        summarise(
          平均值 = round(mean(!!vari, na.rm = TRUE), digits = 4), 
          标准差 = round(sd(!!vari, na.rm = TRUE), digits = 4), 
          最大值 = round(max(!!vari, na.rm = TRUE), digits = 4), 
          最小值 = round(min(!!vari, na.rm = TRUE), digits = 4)
        ) %>%
        select(平均值, 标准差, 最大值, 最小值) -> table_temp
      
      mytable <- data.frame(统计量 = names(table_temp), 值 = table_temp[1, ] %>% as.numeric() ) %>%
        mutate(值 = paste_percent(值*100)) %>%
        mutate(值 = deal_zero(值, fill_na = F)) %>%
        `colnames<-`(c('统计量', paste0('值', '(', df_match$单位[df_match$字段名 == x], ')')))
      
      table_theme <- ttheme_default(core = list(bg_params = list(fill=rep(c("grey95", "grey90"), length.out=4), "steelblue2", "pink2"), alpha = rep(c(1,0.5), each=4)), base_family = 'STXihei', base_size = 10)
      p <- p + 
        theme(plot.margin = unit(c(0,5,0,0), 'cm')) +
        annotation_custom(tableGrob(mytable, rows=NULL, theme = table_theme), xmin=2020, xmax=2022, ymin = table_temp$最小值, ymax = table_temp$最大值)
    }
    
  }
  
  else{
    if(df_match$单位[df_match$字段名 == x] == '万元'){
      df %<>% mutate(!!vari := round(!!vari/10000, 2))
      p <- df %>%
        ggplot(aes(x = 年份, y = !!vari)) +
        geom_line(color = '#046A38', size = 1) +
        geom_point(color = '#046A38', size = 1.5) +
        geom_text(aes(label = deal_zero(!!vari, fill_na = F)), nudge_y = dis/10000, size = 2.5) + 
        scale_x_continuous(breaks = 2010:2019, expand = c(0,1)) +
        scale_y_continuous(n.breaks = 6, labels = scales::comma_format()) +
        labs(x = '', y = '', title = paste0(x, '(', df_match$单位[df_match$字段名 == x], ')')) +
        theme_clean(base_family = 'STXihei') +
        theme(plot.background = element_rect(colour = "transparent", fill = "#fcfcfc"))
      
      # 统计表格
      if((add_table == T) & (df %>% filter(!is.na(!!vari)) %>% nrow() > 0)){
        df %>% 
          summarise(
            平均值 = round(mean(!!vari, na.rm = TRUE), digits = 2), 
            标准差 = round(sd(!!vari, na.rm = TRUE), digits = 2),
            最大值 = round(max(!!vari, na.rm = TRUE), digits = 2),
            最小值 = round(min(!!vari, na.rm = TRUE), digits = 2)
          ) %>%
          select(最大值, 平均值, 最小值, 标准差) -> table_temp
        mytable <- data.frame(统计量 = names(table_temp), 值 = table_temp[1, ] %>% as.numeric() ) %>%
          mutate(值 = deal_zero(值, fill_na = F)) %>%
          `colnames<-`(c('统计量', paste0('值', '(', df_match$单位[df_match$字段名 == x], ')')) )
        
        table_theme <- ttheme_default(core = list(bg_params = list(fill=rep(c("grey95", "grey90"), length.out=4), "steelblue2", "pink2"), alpha = rep(c(1,0.5), each=4)), base_family = 'STXihei', base_size = 10)
        p <- p + 
          theme(plot.margin = unit(c(0,5,0,0), 'cm')) +
          annotation_custom(tableGrob(mytable, rows=NULL, theme = table_theme), xmin=2020, xmax=2022, ymin = table_temp$最小值, ymax = table_temp$最大值)
      }
      
    }else{
      p <- df %>%
        mutate(!!vari := round(!!vari, 2)) %>%
        ggplot(aes(x = 年份, y = !!vari)) +
        geom_line(color = '#046A38', size = 1) +
        geom_point(color = '#046A38', size = 1.5) +
        geom_text(aes(label = deal_zero(!!vari, fill_na = F)), nudge_y = dis, size = 2.5) + 
        scale_x_continuous(breaks = 2010:2019, expand = c(0,1)) +
        scale_y_continuous(n.breaks = 6, labels = scales::comma_format()) +
        labs(x = '', y = '', title = paste0(x, '(', df_match$单位[df_match$字段名 == x], ')')) +
        theme_clean(base_family = 'STXihei') +
        theme(plot.background = element_rect(colour = "transparent", fill = "#fcfcfc"))
      
      # 统计表格
      if((add_table == T) & (df %>% filter(!is.na(!!vari)) %>% nrow() > 0)){
        df %>% 
          summarise(
            平均值 = round(mean(!!vari, na.rm = TRUE), digits = 2), 
            标准差 = round(sd(!!vari, na.rm = TRUE), digits = 2),
            最大值 = round(max(!!vari, na.rm = TRUE), digits = 2),
            最小值 = round(min(!!vari, na.rm = TRUE), digits = 2)
          ) %>%
          select(最大值, 平均值, 最小值, 标准差) -> table_temp
        mytable <- data.frame(统计量 = names(table_temp), 值 = table_temp[1, ] %>% as.numeric() )%>%
          mutate(值 = deal_zero(值, fill_na = F)) %>%
          `colnames<-`(c('统计量', paste0('值', '(', df_match$单位[df_match$字段名 == x], ')'))) 
        
        table_theme <- ttheme_default(core = list(bg_params = list(fill=rep(c("grey95", "grey90"), length.out=4), "steelblue2", "pink2"), alpha = rep(c(1,0.5), each=4)), base_family = 'STXihei', base_size = 10)
        p <- p + 
          theme(plot.margin = unit(c(0,5,0,0), 'cm')) +
          annotation_custom(tableGrob(mytable, rows=NULL, theme = table_theme), xmin=2020, xmax=2022, ymin = table_temp$最小值, ymax = table_temp$最大值)
      }
      
    }
  }
  p
}

addUnits_cn <- function(n) {
  labels <- ifelse(n < 1e4, n,  # less than thousands
                   ifelse(n < 1e8, paste0(round(n/1e4), '万'),
                          ifelse(n < 1e12, paste0(round(n/1e8), '亿'),
                                 #ifelse(n < 1e14, paste0(round(n/1e12), '百亿'),
                                 ifelse(n < 1e16, paste0(round(n/1e12), '万亿'),
                                        ifelse(n < 1e20, paste0(round(n/1e16), '亿亿'), n)))))
  return(labels)
}


addUnits_en <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12), 'T'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}

# 利润池图


#### 调整ggplot2 ####
text_theme = theme(
  text = element_text(family = 'Microsoft YaHei UI'),
  plot.title = element_text(face="bold",
                            hjust = 0.5,
                            size = 14),
  plot.subtitle = element_text(face="bold", 
                               hjust = 0.5,
                               size = 4,
                               margin = c(t = 13, r = 0, b = 17, l = 0)),
  axis.title = element_text(size=12),
  
  legend.title = element_text(size = 12,
                              family = 'Microsoft YaHei UI'),
  legend.text = element_text(size = 11,
                             family = 'Microsoft YaHei UI'))

mytheme <- function(p){
  p + 
    text_theme + xlab("") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
} 

# 获得一个好看的字体！
theme_bw() -> themebw_help 
theme_clean() + text_theme +
  theme(
    panel.background = themebw_help$legend.background,
    panel.border = themebw_help$panel.border,
    legend.background = themebw_help$legend.background,
    plot.background = element_rect(color = "white")
  ) -> my_theme_clean#$panel.border
my_theme_clean$panel.border$colour = "white"

my_theme_clean$panel.grid.major.y$linetype = "dashed"


blue_title = theme(plot.title = element_text(color = "limegreen"))


#### 把CICS行业翻译回本企业 ####
get_one_gd2 <- function(code, detail = T, more_color = T){
  data_entity_industry_csum_full %>%
    filter(企业代码 == code) %>%
    distinct(企业代码, CICS层级, CICS节点, 披露业务汇总) %>%
    separate_rows(披露业务汇总,sep = "、") %>% 
    unique %>%
    group_by(企业代码, CICS层级, CICS节点) %>%
    summarise(披露业务汇总 = paste(披露业务汇总, collapse = "、"))  -> company_biz_info
  
  #### 节点披露问题 ####
  data_entity_industry_full %>%
    mutate_at((ncol(.)-3):ncol(.), ~ifelse(.x =="-",NA, .x)) %>%
    filter(企业代码 == code) %>%
    mutate(CICS1 = ifelse(is.na(CICS1),
                          '其他',CICS1)) %>% 
    mutate(CICS2 = ifelse(is.na(CICS2),
                          paste0('CICS1:',CICS1),CICS2)) %>% 
    mutate(CICS3 = ifelse(is.na(CICS3),
                          ifelse(str_detect(CICS2, "CICS"), CICS2, paste0('CICS2:',CICS2)),CICS3)) %>%
    mutate(CICS4 = ifelse(is.na(CICS4),
                          ifelse(str_detect(CICS3, "CICS"), CICS3, paste0('CICS3:',CICS3)), paste0('CICS4:', CICS4))) %>%
    select(企业代码,年份,业务名称 = 业务显示名称,业务营收,业务毛利,CICS节点=CICS4) %>%
    separate(CICS节点,sep = ":",into=c("CICS层级","CICS节点")) %>%
    group_by(企业代码,年份,CICS层级,CICS节点) %>%
    mutate(企业节点营收规模 = sum(业务营收,na.rm = TRUE)) %>%
    ungroup %>% select(1,2,6,7,8)  %>% unique %>%
    group_by(企业代码,年份) %>% 
    mutate(企业节点营收规模占比 = 企业节点营收规模/sum(企业节点营收规模, na.rm=TRUE)) %>%
    ungroup %>%
    distinct(企业代码,年份, CICS层级,CICS节点, 企业节点营收规模, 企业节点营收规模占比) %>%
    left_join(company_biz_info) -> temp
  
    compname = data_entity_info %>% filter(企业代码 == code) %>% distinct_list("证券简称")
  
if(detail){
  temp %<>% mutate(CICS节点 = paste0(披露业务汇总," (",CICS节点,"行业)"))
}
  temp %>% 
    mutate_at("CICS节点", ~ifelse(.x =="NA (其他行业)" | .x == "其他",  "其他行业或未披露", .x)) -> data
  
  if(more_color){colors = get_blues_plus(data, CICS节点)
  }else{colors = get_blues_less(data, CICS节点)}

  data %>%
    filter(企业代码 == code) %>%
    arrange(企业节点营收规模)  %>%
    get_gd2(
      x = ~年份,
      y = ~企业节点营收规模,
      color = ~CICS节点,
      colors = colors,
      y_title = "业务规模") %>%
    my_layout() %>%
    layout(yaxis = list(title = paste0(compname, "历年业务规模")))
}