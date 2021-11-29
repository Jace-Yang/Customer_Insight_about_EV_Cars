setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(ggshadow) # 给线阴影
if(!require("GGally")) install.packages("GGally") # ggpairs


#### 导出ggplot2 ####
library(ggplot2)
library(export)

saveppt <- function(img,
                    path,
                    append=T,
                    
                    width = 6.5,
                    height = 6.2/2){
  
  graph2ppt(
    x = img,
    file = path,
    width = width,
    height = height,
    append = append,
    paper = 'A4',
    #center = T,
    # offx = 0,
    # offy = 2,
    vector.graphic = TRUE,
    margins = c(top = 0, right = 0, bottom = 0, left = 0))
}

#### ppt字体 ####
geom.text.size = 9 * 5 / 14


ppt_text = theme(
  text = element_text(family = 'Microsoft YaHei',
                      color = "black"),
  plot.title = element_text(face="bold",
                            hjust = 0.5,
                            family = 'Microsoft YaHei',
                            colour = "#0070C0",
                       #     margin = c(t = 0, r = 0, b = 5, l = 0),
                            size = 14),
  axis.text = element_text(family = 'Microsoft YaHei',
                           color = "black"),
  plot.subtitle = element_text(face="bold", 
                               family = 'Microsoft YaHei',
                               hjust = 0.5,
                               size = 4,
                               margin = c(t = 13, r = 0, b = 17, l = 0)),
  axis.title = element_text(size=11),
  legend.title = element_text(size = 10,
                              face = "bold",
                              color = "black",
                              family = 'Microsoft YaHei'),
  legend.text = element_text(size = 9,
                             color = "black",
                             family = 'Microsoft YaHei'))

ppt_text2 = theme(
  text = element_text(family = 'STSongti-SC-Regular'),
  plot.title = element_text(face="bold",
                            hjust = 0.5,
                            colour = "#0070C0",
                            #     margin = c(t = 0, r = 0, b = 5, l = 0),
                            size = 14),
  plot.subtitle = element_text(face="bold", 
                               hjust = 0.5,
                               size = 4,
                               margin = c(t = 13, r = 0, b = 17, l = 0)),
  axis.title = element_text(size=11),
  legend.title = element_text(size = 11,
                              face = "bold",
                              family = 'STSongti-SC-Regular'),
  legend.text = element_text(size = 9,
                             family = 'STSongti-SC-Regular'))



theme_bw() -> themebw_help
ppt_others = theme(
  panel.background = themebw_help$legend.background,
  panel.border = themebw_help$panel.border,
  legend.background = themebw_help$legend.background,
  plot.background = element_rect(color = "white"))
#,
  #rect = element_rect(fill = "transparent") 

theme(
  panel.background = element_rect(fill = "transparent",colour = NA), # bg of the panel
  plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
  panel.grid.major = element_blank(), # get rid of major grid
  panel.grid.minor = element_blank(), # get rid of minor grid
  panel.border = element_blank(),
  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
  legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
) -> empty_theme

theme_legend = theme(
  legend.box.margin = margin(6, 6, 6, 6),
  legend.background = element_rect(color = NA),
  legend.box.background = element_blank()
)

my_theme = theme(
  panel.grid.major.y = element_line(linetype = "dashed",color = "gray")
)

only_y = theme(
  panel.grid.major.y = element_line(linetype = "dashed"),
  panel.grid.major.x = element_blank()
)

only_x = theme(
  panel.grid.major.x = element_line(linetype = "dashed", color = "lightgray"),
  panel.grid.major.y = element_blank()
)

both_dashed = theme(
  panel.grid.major.y = element_line(linetype = "dashed", color = "lightgray"),
  panel.grid.major.x = element_line(linetype = "dashed", color = "lightgray")
)

dotted_line = theme(
  panel.grid.major.y = element_line(linetype = "dotted",color = "gray")
  #panel.grid.minor.y = element_line(linetype = "dotted",color = "gray")
  #panel.grid.minor.y = elemnel.grid.linetype .y"dashed", color = "gray")
)

my_theme_rev = theme(
  panel.grid.major.x = element_line(linetype = "dashed", color = "gray"),
  panel.grid.major.y = element_blank()
)

theme_clean() + ppt_text + ppt_others + empty_theme + theme_legend + my_theme-> ppt_theme


theme_clean() + ppt_text + theme_legend + my_theme -> ppt_theme_bg





#### ggplot2 其他主题
small_legend = theme(legend.key.size = unit(0.85,"line"))
#### ggplot2 坐标轴调整 ####
show_all_x <- function(data, p, step = 1, len = NA, col = "日期"){
  data %>% distinct_list(col) -> date_list
  if(is.na(len)){
    p + scale_x_continuous(breaks = seq(min(date_list), max(date_list), by = step))
  }else{
    p + scale_x_continuous(breaks = seq(min(date_list), max(date_list), length.out = len))
  }
  
}

# 百分比
percenty <- scale_y_continuous(expand = c(0, 0), 
                               labels = scales::percent,
                               breaks = scales::breaks_pretty(9))



get_percenty <- function(n){
  scale_y_continuous(
    expand = c(0, 0), 
    labels = scales::percent,
    breaks = scales::breaks_pretty(n))}

get_percentx <- function(n){
  scale_x_continuous(
    expand = c(0, 0), 
    labels = scales::percent,
    breaks = scales::breaks_pretty(n))}

# 大数字
bigy <- scale_y_continuous(expand = c(0, 0), labels = addUnits_cn)

get_bigy <- function(limits)(
  scale_y_continuous(expand = c(0, 0), 
                     limits = limits,
                     labels = addUnits_cn)
)

# 自己加后缀
unity <- function(unit){
  return(scale_y_continuous(expand = c(0, 0), labels = function(x) paste0(x,unit)))
}


#### ggplot 案例 ####

get_pool <- function(temp, my_xmax = NA, my_ymax = NA){
  temp %<>%
    mutate(累加份额 = cumsum(temp$节点营收规模),
               start = 累加份额 - 节点营收规模 * 0.5)
  
  
  
  color_temp = colorRampPalette(all_blues)(temp %>% ungroup %>% distinct(CICS节点) %>% nrow + 2)[-(1:2)]
  temp %<>% arrange(节点营收规模)
  
  ggplot(temp, mapping = aes(x = start,
                             y = 节点毛利率,
                             fill = CICS节点)) +  
    geom_bar(width = temp$节点营收规模,
             stat = "identity",
             position = "dodge") +
    scale_fill_manual(values = color_temp)  + 
    my_theme_clean +
    geom_label(
      aes(label = paste(CICS节点,
                        "\n",paste0(round(节点毛利率* 100, 1), "%"),
                        "\n",addUnits_cn(节点营收规模)),
          y = 节点毛利率),
      show.legend = FALSE,
      #position = position_dodge(width = 0.1),
      size = 2,
      color = "black",
      fill = colorRampPalette(all_blues)(temp %>% ungroup %>% distinct(CICS节点) %>% nrow + 2)[1],
      #nudge_x = 0.5,
      #hjust = 0.5,
      #vjust = 0.5,
      #box.padding = 0.2,
      #arrow = NULL,
      family = "Microsoft YaHei") +
    
    labs(x = "行业市场规模", 
         y = "行业加权平均毛利率") -> p
  
  ymax <- max(temp$节点毛利率)
  xmax <- max(temp$累加份额) 
  
  if(is.na(my_xmax)){
    p + 
      scale_x_continuous(
        expand = c(0, 0), 
        limit = c(xmax * -0.02, xmax * 1.05),
        breaks = scales::breaks_pretty(9),
        labels = addUnits_cn) -> p
  }else{
    p + 
      scale_x_continuous(
        expand = c(0, 0), 
        limit = c(xmax * -0.02, my_xmax),
        breaks = scales::breaks_pretty(9),
        labels = addUnits_cn) -> p
  }
  
  if(is.na(my_ymax)){
    p + 
      scale_y_continuous(expand = c(0, 0), 
                         limit=c(0, ymax * 1.2),
                         labels = scales::percent) -> p 
  }else{
    p + 
      scale_y_continuous(expand = c(0, 0), 
                         limit=c(0, my_ymax),
                         labels = scales::percent) -> p 
  }
  return(p)
}

gg_lineplot <- function(data, 
                        y, 
                        color, 
                        x= "日期", 
                        title="",
                        yaxis="",
                        show_all_x = T,
                        color_scale = scale_color_manual(values = get_blues_(data, !!sym(color)))){
  
  data %>%
    ggplot(aes_string(
      x = x,
      y = y,
      color = color
    )) + ppt_theme +
    geom_line(size = 1) + 
    geom_point(size = 1.4) + 
    color_scale +
    xlab("") + 
    ggtitle(title)  -> p
  if(show_all_x) return(data %>% show_all_x(p, col = x))
  return(p)
}

## 展示CSMAR拉出来的所有指标！
show_all_index <- function(
  data,
  x = "日期",
  y = "指标值",
  color = "证券简称",
  index_type = "指标类型",
  report_type = "报表类型"){
  data %>%
    my_pivot_longer(names = "指标类型", values = "指标值", cols = 4:ncol(.)) -> temp

  temp %>%
    #filter_at(x, ~year(.x)>2012) %>%
    ggplot(aes(
      x = !!sym(x),
      y = !!sym(y),
      color = !!sym(color)
    )) +
    geom_line() +
    ppt_theme + xlab("") + ylab("") +
    facet_wrap(eval(expr(!!ensym(index_type) ~ !!ensym(report_type))), 
               ncol =2,
               scales = "free_y" ) -> p
  ggplotly(p) %>%
    layout(hovermode = 'compare',
           yaxis = list(spikedash="dash",
                        zeroline = TRUE,
                        showgrid = F),
           xaxis = list(spikedash="dash",
                        zeroline = TRUE,
                        showgrid = F)) %>%
    center_legend %>% 
    svg_button2 %>% 
    do_transparant 
  
}


show_y_index <- function(temp_,
                         y = NA,
                         title = ""){
  if(is.na(y)){y = colnames(temp_)[3]}
  y = sym(y)
  bind_rows(
    temp_ %>%
      filter(证券简称 %in% c("牧原股份","温氏股份")) %>% mutate(type = "标的公司"),
    
    temp_ %>%
      group_by(日期) %>%
      right_join(com_list) %>%
      summarise(!!y := mean(!!y, na.rm=T)) %>%
      mutate(证券简称 = "行业平均",
                 type = "行业平均"),
    
    temp_ %>%
      filter(!证券简称 %in% c("牧原股份","温氏股份")) %>% mutate(type = "行业其他")) %>%
    mutate(type = factor(type, 
                         levels = c("标的公司","行业平均","行业其他"))) -> data_
  
  data_ %>% 
    group_by(证券简称) %>%
    filter(日期 == max(日期)) %>%
    arrange(-!!y) %>%
    filter(!证券简称 %in% c("牧原股份","温氏股份","行业平均")) %>%
    distinct_list("证券简称") -> temp_level
  
  data_ %<>% 
    mutate(证券简称 = factor(证券简称, 
                             levels = c("牧原股份","温氏股份","行业平均",temp_level)))
  

  # 选择颜色
  tibble(证券简称 = levels(data_$证券简称)) -> temp_id
  bind_rows(
    temp_id %>%
      filter(证券简称 %in% c("牧原股份","温氏股份","行业平均")) %>%
      mutate(color = c(my_pick[2], my_pick[3], "black"),
             size = c(1,1,0.8),
             alpha = c(1,1,0.9),
             line_type =  c("solid", "solid","solid")),
    
    temp_id %>%
      filter(!证券简称 %in% c("牧原股份","温氏股份","行业平均")) %>%
      mutate(color = get_spectral_(.,证券简称),
             size = 0.5,
             alpha = 0.7,
             line_type =  "dashed")) -> temp_scale
  
  
  data_ %>%  
    ggplot(aes_string(
      x = "日期",
      y = y,
    
      group = "证券简称",
      color = "证券简称",
      linetype = "证券简称",
      alpha = "证券简称",
      size = "证券简称"
    )) + ppt_theme + dotted_line +
    geom_line() +
    #geom_point(aes(size = ))+ 
    scale_alpha_manual(values = temp_scale$alpha) +
    scale_linetype_manual(values = temp_scale$line_type) +
    scale_color_manual(values = temp_scale$color) +
    scale_size_manual(values = temp_scale$size) +
    xlab("") + ylab(y) +
    bigy +
    ggtitle(title) +
    scale_x_date(labels = date_format("%y"),
                 breaks = date_breaks("1 year")) +
    small_legend
}

