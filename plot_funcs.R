demo_plot <- function(plot_palette, download) {
  set.seed(123)
  
  n_colors <- length(plot_palette)
  plot_df <- data.frame(
    x = sample(1:100, n_colors),
    y = sample(1:100, n_colors),
    c = plot_palette
  )
  
  p <- ggplot(
    plot_df,
    aes(x = x, y = y)
  ) +
    geom_point(aes(color = c), size = 6) +
    scale_colour_manual(values = plot_palette) +
    labs(title = "Example Plot", x = "X", y = "Y") +
    theme(
      text= element_text(size = 15),
      plot.background = element_rect(fill='transparent'),
      legend.position = "none"
    )
  
  p <- ggplotly(p) |>
    config(
      modeBarButtons = list(list("toImage", "resetScale2d")),
      toImageButtonOptions = list(
        format = download$format,
        width = download$width,
        height = download$height,
        scale = download$scale
      )
    )
  
  return(p)
}

collection_progress_plot <- function(df, plot_palette, download, 
                                     select_factor,
                                     plot_title,
                                     title_size,subtitle_size,
                                     axisx_size,axisy_size,legend_text_size,
                                     axisx_text_size,axisy_text_size,
                                     font_family,
                                     font_face_title,font_face_axis) {
  plot_df <- df |>
    filter(!is.na(FACTOR)) |>
    group_by(FACTOR, WEEK) |>
    summarize(
      recruit = n(),
      seed = sum(SEED, na.rm = TRUE),
      coupon = sum(CT_T_CP_USED, na.rm = TRUE)
    ) |>
    mutate(
      recruit = cumsum(recruit),
      seed = cumsum(seed),
      coupon = cumsum(coupon)
    )

  plot_df <- plot_df |> pivot_longer(-c(WEEK, FACTOR), names_to = "type", values_to = "count")
  
  # 检查分类变量 'group' 有多少个不同的值
  number_of_levels <- length(unique(plot_df$FACTOR))
  
  # 设置labeller函数
  # 如果只有一个级别的 'group'，则不显示变量名，否则使用 label_both
  custom_labeller <- function(variable_values){
    setNames(paste(select_factor, variable_values, sep = ": "), variable_values)
  }
  if (number_of_levels > 1) {
    # When there are multiple levels, use the custom labeller
    labeller_function <- as_labeller(custom_labeller)
  } else {
    # When there is one or no level, do not show facets
    labeller_function <- label_value
  }
    #if(number_of_levels > 1) label_both else label_value
  plot_title_final <- if(plot_title == '') select_factor else plot_title
  p <- plot_df |>
    ggplot(
      aes(x = WEEK, y = count)
    ) +
    geom_line(
      aes(color = type),
      linewidth = 0.8
    ) +
    facet_wrap(
      vars(FACTOR),labeller = labeller_function#,labeller = label_both#,
      #labeller = labeller(FACTOR = label_bquote(.(unique(FACTOR)) == .(value)))
    ) +
    scale_color_manual(
      values = plot_palette
    ) +
    theme_bw() +
    labs(
      title = "",
      x = "Week in production", y = "Count"
    ) +
    theme(
      panel.spacing.y = unit(2, "lines"),
      plot.background = element_rect(fill='transparent'),
      legend.position = "top",
      legend.justification = "left",
      strip.text = element_text(size = subtitle_size, family = font_family),
      plot.title = element_text(hjust = 0.5, size = title_size,family = font_family, face = font_face_title,margin = margin(b = 100, unit = "lines")),  # 自定义标题样式
      # 下面设置axis标题和legend的字体和大小
      axis.title.x = element_text(size = axisx_size, family = font_family, face = font_face_axis),
      axis.title.y = element_text(size = axisy_size, family = font_family, face = font_face_axis),
      legend.text = element_text(size = legend_text_size, family = font_family),
      axis.text.x = element_text(size = axisx_text_size, family = font_family, face = "bold"),
      axis.text.y = element_text(size = axisy_text_size, family = font_family, face =  "italic")
    )+labs(title = plot_title_final)
  
  p <- ggplotly(p) |>
    layout(
      #新添加
      title = list(
        text = plot_title_final,
        x = 0.5,
        xanchor = "center",
        y = 0.98,  # 调节这个参数来改变标题和图的距离
        yanchor = "top",
        pad = list(b = 100)  # padd函数增加额外的空间
      ),
      legend = list(
        x = 0, y = 1,
        xanchor = "left", yanchor = "top",
        orientation = "h",
        bgcolor = "white",
        bordercolor ="black",
        borderwidth = 1,
        orientation ="v",
        traceorder = "original",
        title = ""
      )
    ) |>
    config(
      modeBarButtons = list(list("toImage", "resetScale2d")),
      toImageButtonOptions = list(
        format = download$format,
        width = download$width,
        height = download$height,
        scale = download$scale
      )
    )
  
  return(p)
}

# respondent_plot <- function(df, spec) {
#   # df <- df |> filter(!is.na(FACTOR))
#   
#   by_site <- df |>
#     group_by(LOCATION, FACTOR) |>
#     summarize(count = n()) |>
#     mutate(
#       prop = count / sum(count),
#       total_count = sum(count)
#     )
#   
#   total <- df |>
#     group_by(FACTOR) |>
#     summarize(count = n()) |>
#     mutate(
#       prop = count / sum(count),
#       total_count = sum(count),
#       LOCATION = "TOTAL"
#     )
#   
#   plot_df <- bind_rows(by_site, total)
#   
#   p <- plot_df |>
#     ggplot(
#       aes(
#         x = LOCATION,
#         y = prop,
#         fill = FACTOR
#       )
#     ) +
#     geom_bar(
#       stat = "identity",
#       position = "stack"
#     ) +
#     geom_text(
#       aes(label = sprintf("%1.1f%%", prop * 100)),
#       position = position_stack(vjust = 0.5),
#       size = 7,
#       hjust = 0.5
#     ) +
#     labs(title = "", x = "Location", y = "") +
#     scale_fill_manual(
#       values = spec$colors,
#       labels = spec$labels,
#       name = spec$title,
#       na.value = spec$na_color
#     ) +
#     theme_bw() +
#     theme(text = element_text(size = 18))
#   
#   return(p)
# }

respondent_plot <- function(df, plot_palette, download,
                            select_factor,
                            plot_title,bar_width,
                            title_size,legend_text_size,
                            axisx_text_size,axisy_text_size,
                            font_family,font_face_title) {
  total <- nrow(df)
  plot_df <- df |>
    mutate(FACTOR = as.character(FACTOR)) %>%
    replace(is.na(.), "NA") |>
    mutate(FACTOR = factor(FACTOR, levels = c(setdiff(sort(unique(FACTOR)), "NA"), "NA"))) |>
    group_by(FACTOR) |>
    summarize(perc = n() / total)
  #添加默认title
  plot_title_final <- if(plot_title == '') select_factor else plot_title
  p <- ggplot(
    data = plot_df,
    aes(x = FACTOR, y = perc, fill = FACTOR)
  ) +
    geom_bar(
      stat = "identity",
      position = "dodge",
      width=bar_width,
    ) +
    
    scale_fill_manual(
      values = plot_palette
      )  +
 
    scale_y_continuous(
      labels = scales::percent
    ) +
    labs(x = "", y = "") +
    theme(
      text = element_text(size = 15),
      plot.background = element_rect(fill='transparent'),
      plot.title = element_text(hjust = 0.5, size = title_size,family = font_family, face = font_face_title),
      axis.text.x = element_text(family = font_family, size = axisx_text_size),
      axis.text.y = element_text(family = font_family, size = axisy_text_size),
      legend.title = element_text(size = legend_text_size, family=font_family),
      legend.text = element_text(size = legend_text_size, family=font_family)
    )+
    labs(title = plot_title_final, fill = select_factor)
  
  p <- ggplotly(p) |>
    config(
      modeBarButtons = list(list("toImage", "resetScale2d")),
      toImageButtonOptions = list(
        format = download$format,
        width = download$width,
        height = download$height,
        scale = download$scale
      )
    )
  p <- p %>% layout(
    legend = list(x = 1, y = 1, xanchor = 'right', yanchor = 'top'),
    font = list(family = font_family)
  )
  return(p)
}

#加标题，线重叠问题，可下载
recruitment_chain_graph <- function(df, plot_palette,select_factor,
                                    plot_title,
                                    layout_method,
                                    title_size,font_family) {
  #layout_function<-get(layout_method)
  set.seed(123)
  na_color <- "grey"
  na_count <- sum(is.na(df$FACTOR))
  levels <- df$FACTOR[!is.na(df$FACTOR)] |> unique() |> sort()
  if(na_count!=0){
    df$FACTOR[is.na(df$FACTOR)] <- "NA"
    levels <- c(levels,"NA")
    colors <-c(plot_palette[1:length(levels)-1],na_color)
  }else{
    colors <-c(plot_palette[1:length(levels)])
  }
  
  #添加默认title
  plot_title_final <- if(plot_title == '') select_factor else plot_title
  
  plot_df <- df |>
    mutate(
      from = R_ID,
      to = ID,
      id = ID,
      seed = SEED,
      factor = FACTOR
    ) |>
    select(from, to, id, seed, factor)

  opar <- par(no.readonly = TRUE)  # 保存当前参数设置
  par(family=font_family,cex.main=title_size)   # 设置新的参数
                         # 恢复原参数设置
  
  p <- graph_from_data_frame(
    d = plot_df |> select(from, to) |> na.omit(),
    vertices = plot_df |> select(id, seed, factor),
    directed = FALSE
  ) %>%
    plot(.,main=plot_title_final,
      layout = get(layout_method),
      vertex.label = NA,
      vertex.size = ifelse(V(.)$seed == 1, 5, 3),
      vertex.color = factor(
        V(.)$factor,
        levels = levels,
        labels = colors
      ) |> as.character(),
      edge.color = "black" ,
      edge.width = 1
    )

  legend(
    x = -0.85, y = -0.95,
    xjust = 0.5, yjust = 0.5,
    legend = if(length(levels) == 1) c("Seed", "Not seed") else levels,
    pch = 21,
    col = "black",
    pt.bg = colors,
    pt.cex = if(length(levels) == 1) c(2.5, 1.5) else 1.5,
    cex = 1.5,
    bty = "n"
  )
  par(opar)
}

