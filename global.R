library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)

library(DT)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(igraph)
library(leaflet)
library(leaflet.minicharts)
library(leaflet.extras)


source("data_funcs.R")
source("plot_funcs.R")
source("ui_funcs.R")


create_ids <- function(items, prefix) {
   id_list <- paste0(prefix, 1:length(items)) |> as.list()
   names(id_list) <- items
   
   return(id_list)
}


IDs <- list(
  Select = create_ids(c("rcoupon", "respID", "plot", "waves", "seeds",
                        "prog", "resp_main","resp_aux", "chain",
                        "color", "demo_format",
                        #后为新添加
                        "cpt_font_family",
                        "cpt_font_face_title","cpt_font_face_axis",
                        "pc_font_family","pc_font_face_title",
                        "rc_layout_method","rc_font_family"),
                      "select"),
  Keyboard = create_ids(c("max_coupon",
                          "prog", "resp", "chain",
                          "demo_height", "demo_width", "demo_scale"),
                        "keyboard"),
  File = create_ids(c("input"), "file"),
  Table = create_ids(c("input"), "table"),
  Plot = create_ids(c("prog", "resp", "chain", "map", "demo"), "plot"),
  Button = create_ids(c("toggle_left_panel", "toggle_right_panel",
                        "toggle_step1", "toggle_step2", "toggle_step3",
                        "add_tab", "prog_add", "resp_add", "chain_add",
                        "select_all_waves", "deselect_all_waves",
                        "select_all_seeds", "deselect_all_seeds",
                        "add_color", "set_palette", "clear_palette", "default_palette",
                        #新加入
                        "cpt_title_options_hidden_bottom",
                        "cpt_plot_detail_options_hidden_bottom",
                        "cpt_font_options_hidden_bottom",
                        "pc_title_options_hidden_bottom",
                        "pc_plot_detail_options_hidden_bottom",
                        "pc_font_options_hidden_bottom",
                        "rc_title_options_hidden_bottom",
                        "rc_plot_detail_options_hidden_bottom",
                        "rc_font_options_hidden_bottom",
                        "cpt_download_buttom",
                        "pc_download_buttom",
                        "rc_download_buttom"
                        ),
                      "button"),
  UI = create_ids(c("panel_toggle", "middle_panel", "right_panel",
                    "var_select_main1","var_select_main2", "var_select_other",
                    "palette"),
                  "ui"),
  Tabset = create_ids(c("main", "prog", "resp", "chain"),
                      "tabset"),
  Tab = create_ids(c("data"),
                   "tab"),
  Other = create_ids(c("left_panel_div", "right_panel_div","download",
                       "cpt_title_options_hidden_id",
                       "cpt_plot_detail_options_hidden_id",
                       "cpt_font_options_hidden_id",
                       "pc_title_options_hidden_id",
                       "pc_plot_detail_options_hidden_id",
                       "pc_font_options_hidden_id",
                       "rc_title_options_hidden_id",
                       "rc_plot_detail_options_hidden_id",
                       "rc_font_options_hidden_id"
                       ),
                     "other"),
  Slider = create_ids(c("cpt_t_size","cpt_subt_size","cpt_x_t_size","cpt_y_t_size","cpt_legend_text_size",
                        "cpt_x_text_size","cpt_y_text_size",
                        "pc_bar_width","pc_title_size","pc_legend_text_size",
                        "pc_x_axis_text_size","pc_y_axis_text_size",
                        "rc_title_size"),
                    "slider")
)

plot2vars <- list(
  `Data Collection Progress` = list(
    MAIN_DATE = "Date of appointment"
  ),
  `Participant Characteristics` = list(),
  `Recruitment Chains` = list(),
  Geolocation = list(
    LAT = "Latitude",
    LNG = "Longtitude"
  )
)

color_settings <- list(
  min_colors = 2,
  max_colors = 8,
  default = RColorBrewer::brewer.pal(8, "Set1")
)
