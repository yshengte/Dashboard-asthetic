respondent_plot1 <- function(df, download,
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
    aes(x = FACTOR, y = perc)
  ) +
    geom_bar(
      stat = "identity",
      position = "dodge",
      width = bar_width
    ) +
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

respondent_plot2 <- function(df, plot_palete, download,
                             select_factor,
                             select_factor_aux,
                             plot_title,bar_width,
                             title_size,legend_text_size,
                             axisx_text_size,axisy_text_size,
                             font_family,font_face_title) {
  df <- df |> mutate(
    MAIN = as.character(MAIN),
    AUX = as.character(AUX)
  ) %>%
    replace(is.na(.), "NA")
  
  by_both <- df |>
    group_by(AUX, MAIN) |>
    summarize(count = n()) |>
    mutate(
      prop = count / sum(count),
      total_count = sum(count)
    )
  
  by_main <- df |>
    group_by(MAIN) |>
    summarize(count = n()) |>
    mutate(
      prop = count / sum(count),
      total_count = sum(count),
      AUX = "Total"
    )
  
  by_aux <- df |>
    group_by(AUX) |>
    summarize(count = n())
  
  plot_df <- rbind(by_both, by_main) |>
    mutate(
      MAIN = factor(MAIN, levels = c(setdiff(sort(unique(MAIN)), "NA"), "NA")),
      AUX  = factor(AUX, levels = c("Total", setdiff(sort(unique(AUX)), c("Total", "NA")), "NA"))
    )
  
  p <- ggplot(
    plot_df,
    aes(x = AUX, y = prop, fill = MAIN)
  ) +
    geom_bar(stat = "identity", position = "stack", width = 0.6) +
    geom_text(
      aes(label = sprintf("%1.1f%%", prop * 100)),
      position = position_stack(vjust = 0.5),
      size = 5,
      hjust = 0.5
    ) +
    scale_y_continuous(
      labels = scales::percent
    ) +
    scale_fill_manual(
      values = plot_palete,
      name = NULL
    ) +
    labs(x = "", y = "") +
    theme(
      text = element_text(size = 15),
      plot.background = element_rect(fill='transparent'),
      legend.background = element_rect(fill = "transparent")
    )
  
  for(i in 1:nrow(by_aux)) {
    p <- p + annotate(
      "text",
      x = by_aux$AUX[i],
      y = 1,
      label = paste0("n=", by_aux$count[i]),
      vjust = -0.5,
      size = 5
    )
  }
  
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
  
  plot_count(plot_count() + 1) 
}
