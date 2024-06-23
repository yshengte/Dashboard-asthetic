show_alert <- function(message, session) {
  showModal(
    modalDialog(
      title = tagList(icon("triangle-exclamation", "fa"), "Warning"),
      message
    ),
    session = session
  )
}

show_notif <- function(message, session) {
  showModal(
    modalDialog(
      title = tagList(icon("bell", "fa"), "Notification"),
      message
    ),
    session = session
  )
}

create_progress_tab <- function(
    select_id,
    textin_id,
    button_id,
    tabset_id,
    plot_id,
    choices,
    #后面为新添加
    title_options_hidden_id,
    title_options_hidden_bottom,
    title_size_id,
    subtitle_size_id,
    plot_detail_options_hidden_id,
    plot_detail_options_hidden_bottom,
    x_axis_size_id,
    y_axis_size_id,
    legend_text_size_id,
    x_axis_text_size_id,
    y_axis_text_size_id,
    font_options_hidden_id,
    font_options_hidden_bottom,
    font_family_id,
    font_title_face_id,
    font_axis_face_id
) {
  tabPanel("Data Collection Progress",
    tags$div(class = "pad_top",
      fluidRow(
        fluidRow(
          column(width = 3,
            selectInput(
              inputId = select_id,
              label = "Select factor",
              choices = choices,
              width = "100%"
            )
          ),
          column(width = 3,
            actionButton(
              inputId = button_id,
              label = "ADD",
              style = "position: relative; top: 23px;"
            )
          )
          
        ),
        fluidRow(      
          tabsetPanel(id = tabset_id,
            tabPanel("Overall",
              tags$div(class = "pad_top",
                plotlyOutput(plot_id, height = 750)
              )
            )
          )
          ),
      fluidRow(
        column(width = 3,
               actionButton(
                 inputId = title_options_hidden_bottom,
                 label = "Toggle Title Options",
               )
        )
      ),
      hidden(
        div(id = title_options_hidden_id,
          fluidRow(
            column(width = 3,
              textInput(
                inputId = textin_id,
                label = "Title in plot",
                width = "100%"
              )
            ),
            column(width = 3,
                   sliderInput(
                     inputId = title_size_id,  # 您需要确保此ID在您的应用中是唯一的
                     label = "Title Size:",
                     min = 8,
                     max = 30,
                     value = 12  # 初始值设置为12
                   )
            ),
            column(width = 3,
                   sliderInput(
                     inputId = subtitle_size_id,  # 您需要确保此ID在您的应用中是唯一的
                     label = "Subtitle Size:",
                     min = 8,
                     max = 30,
                     value = 12  # 初始值设置为12
                   )
            )
          )
        )
      ),
      fluidRow(
        column(width = 3,
               actionButton(
                 inputId = plot_detail_options_hidden_bottom,
                 label = "Toggle Plot Detail Options",
               )
        )
      ),
      hidden(
        div(id = plot_detail_options_hidden_id,
          fluidRow(
            column(width = 3,
                   sliderInput(
                     inputId = x_axis_size_id,
                     label = "X Axis Size:",
                     min = 8,
                     max = 30,
                     value = 12  # 初始值设置为12
                   )
            ),
            column(width = 3,
                   sliderInput(
                     inputId = y_axis_size_id,
                     label = "Y Axis Size:",
                     min = 8,
                     max = 30,
                     value = 12  # 初始值设置为12
                   )
            ),
            column(width = 3,
                   sliderInput(
                     inputId = x_axis_text_size_id,
                     label = "X Axis Text Size:",
                     min = 8,
                     max = 30,
                     value = 12  # 初始值设置为12
                   )
            ),
            column(width = 3,
                   sliderInput(
                     inputId = y_axis_text_size_id,
                     label = "Y Axis Text Size:",
                     min = 8,
                     max = 30,
                     value = 12  # 初始值设置为12
                   )
            ),
            column(width = 3,
                   sliderInput(
                     inputId = legend_text_size_id,
                     label = "Legend Text Size:",
                     min = 8,
                     max = 30,
                     value = 12  # 初始值设置为12
                   )
            )
          )
        )
      ),
      fluidRow(
        column(width = 3,
               actionButton(
                 inputId = font_options_hidden_bottom,
                 label = "Toggle Font Options",
               )
        )
      ),
     hidden(
       div(id = font_options_hidden_id, 
            fluidRow(
              column(3,
                     selectInput(
                       inputId = font_family_id,
                       label = "Font Family:",
                       choices = c("Arial", "Times New Roman", "Courier"),
                       width = "100%"
                     )
              ),
              column(3,
                     selectInput(
                       inputId = font_title_face_id,
                       label = "Title Font Face:",
                       choices = c("plain", "italic", "bold", "bold.italic"),
                       width = "100%"
                     )
              ),
              column(3,
                     selectInput(
                       inputId = font_axis_face_id,
                       label = "Axis Font Face:",
                       choices = c("plain", "italic", "bold", "bold.italic"),
                       width = "100%"
                     )
              )
            )
        )
     ) 
        
      
    )))
}

create_respondent_tab <- function(
    select_id,
    textin_id,
    button_id,
    tabset_id,
    choices,
    #以下为新添加
    title_options_hidden_id,
    title_options_hidden_bottom,
    title_size_id,
    plot_detail_options_hidden_id,
    plot_detail_options_hidden_bottom,
    bar_width_id,
    legend_text_size_id,
    x_axis_text_size_id,
    y_axis_text_size_id,
    font_options_hidden_id,
    font_options_hidden_bottom,
    font_family_id,
    font_title_face_id
) {
  tabPanel("Participant Characteristics",
    tags$div(class = "pad_top",
      fluidRow(
        fluidRow(
        column(width = 3,
          selectInput(
            inputId = select_id,
            label = "Select factor",
            choices = choices,
            width = "100%"
          )
        ),
        column(width = 3,
          actionButton(
            inputId = button_id,
            label = "ADD",
            style = "position: relative; top: 23px;"
          )
        )),
      fluidRow(
      tabsetPanel(id = tabset_id)),
      fluidRow(
        column(width = 3,
               actionButton(
                 inputId = title_options_hidden_bottom,
                 label = "Toggle Title Options",
               )
        )
      ),
      hidden(
        div(id = title_options_hidden_id,  
          fluidRow(
            column(width = 3,
              textInput(
                inputId = textin_id,
                label = "Title in plot",
                width = "100%"
              )
            ),
            column(width = 3,
                   sliderInput(
                     inputId = title_size_id,  # 您需要确保此ID在您的应用中是唯一的
                     label = "Title Size:",
                     min = 8,
                     max = 30,
                     value = 12  # 初始值设置为12
                   )
            )
          )
        )
      ),
      fluidRow(
        column(width = 3,
               actionButton(
                 inputId = plot_detail_options_hidden_bottom,
                 label = "Toggle Plot Detail Options",
               )
        )
      ),
      hidden(
        div(id = plot_detail_options_hidden_id,
          fluidRow(
            column(width = 3,
                     sliderInput(
                       inputId = bar_width_id,
                       label = "Bar Width:",
                       min = 0,
                       max = 1,
                       value = 0.5  # 初始值设置为12
                     )
            ),
            column(width = 3,
                   sliderInput(
                     inputId = legend_text_size_id,
                     label = "Legend Text Size:",
                     min = 8,
                     max = 30,
                     value = 12  # 初始值设置为12
                   )
            ),
            column(width = 3,
                   sliderInput(
                     inputId = x_axis_text_size_id,
                     label = "X Axis Text Size:",
                     min = 8,
                     max = 30,
                     value = 12  # 初始值设置为12
                   )
            ),
            column(width = 3,
                   sliderInput(
                     inputId = y_axis_text_size_id,
                     label = "Y Axis Text Size:",
                     min = 8,
                     max = 30,
                     value = 12  # 初始值设置为12
                   )
            )
          )
        )
      ),
      fluidRow(
        column(width = 3,
               actionButton(
                 inputId = font_options_hidden_bottom,
                 label = "Toggle Font Options",
               )
        )
      ),
      hidden(
        div(id = font_options_hidden_id, 
          fluidRow(
            column(width = 3,
                   selectInput(
                     inputId = font_family_id,
                     label = "Font Family:",
                     choices = c("Arial","Times New Roman","Courier"),
                     width = "100%"
                   )
            ),
            column(width = 3,
                   selectInput(
                     inputId = font_title_face_id,
                     label = "Title Font Face:",
                     choices = c("plain","italic","bold","bold.italic"),
                     width = "100%"
                   )
            )
          )
        )
      )
    )
  )
)}

create_rchain_tab <- function(
    select_id,
    textin_id,
    button_id,
    tabset_id,
    plot_id,
    choices,
    #以下为新添加
    title_options_hidden_id,
    title_options_hidden_bottom,    
    title_size_id,
    plot_detail_options_hidden_id,
    plot_detail_options_hidden_bottom,    
    layout_method_id,
    font_options_hidden_id,
    font_options_hidden_bottom,
    font_family_id,
    #downloadable
    download_buttom
) {
  tabPanel("Recruitment Chains",
    tags$div(class = "pad_top",
      fluidRow(
        column(width = 3,
          selectInput(
            inputId = select_id,
            label = "Select factor",
            choices = choices,
            width = "100%"
          )
        ),
        column(width = 3,
          actionButton(
            inputId = button_id,
            label = "ADD",
            style = "position: relative; top: 23px;"
          )
        ),
        column(width = 3,
          downloadButton(download_buttom, "Download Image")
        )
        
      ),
      tabsetPanel(id = tabset_id,
        tabPanel("Seed",
          plotOutput(plot_id, height = 750)
        )
        #downloadButton(outputId="down",label = "Download the last added plot")
      ),
      fluidRow(
        column(width = 3,
               actionButton(
                 inputId = title_options_hidden_bottom,
                 label = "Toggle Title Options",
               )
        )
      ),
      hidden(
        div(id = title_options_hidden_id, 
          fluidRow(
            column(width = 3,
              textInput(
                inputId = textin_id,
                label = "Title in plot",
                width = "100%"
              )
            ),
            column(width = 3,
                   sliderInput(
                     inputId = title_size_id,  # 您需要确保此ID在您的应用中是唯一的
                     label = "Title Size:",
                     min = 0.5,
                     max = 5,
                     value = 1  # 初始值设置为12
                   )
            )
          )
        )
      ),
      fluidRow(
        column(width = 3,
               actionButton(
                 inputId = plot_detail_options_hidden_bottom,
                 label = "Toggle Plot Detail Options",
               )
        )
      ),
      hidden(
        div(id = plot_detail_options_hidden_id,
          fluidRow(
            column(width = 3,
                   selectInput(
                     inputId = layout_method_id,
                     label = "Layout Method:",
                     choices = c("layout_with_fr","layout_with_kk",
                                 "layout_as_tree","layout_with_dh",
                                 "layout_with_lgl","layout_with_sugiyama",
                                 "layout_with_gem","layout_on_grid",
                                 "layout_on_sphere","layout_randomly"),
                     width = "100%"
                   )
            )
          )
        )
      ),
      fluidRow(
        column(width = 3,
               actionButton(
                 inputId = font_options_hidden_bottom,
                 label = "Toggle Font Options",
               )
        )
      ),
      hidden(
        div(id = font_options_hidden_id, 
          fluidRow(
            column(width = 3,
                   selectInput(
                     inputId = font_family_id,
                     label = "Font Family:",
                     choices = c("Arial","Times New Roman","Courier"),
                     width = "100%"
                   )
            )
          )
        )
      )
    )
  )
}

create_map_tab <- function(waves, seeds) {
  tabPanel("Geolocation",
    tags$div(class = "pad_top",
      fluidRow(
        column(width = 4,
          shinyWidgets::pickerInput(
            inputId = IDs$Select$waves,
            label = "Filter by wave number",
            choices = waves,
            selected = waves,
            multiple = TRUE,
            width = "100%",
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `selected-text-format` = "values"
            )
          )
        ),
        column(width = 8,
          shinyWidgets::pickerInput(
            inputId = IDs$Select$seeds,
            label = "Filter by seed ID",
            choices = seeds,
            selected = seeds,
            multiple = TRUE,
            width = "100%",
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `selected-text-format` = "count > 10"
            )
          )
        )
      ),
      leafletOutput(outputId = IDs$Plot$map, height = 800)
    )
  )
}

