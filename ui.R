ui <- tagList(
  useShinyjs(),
  includeCSS("main.css"),
  navbarPage("RDS Dashboard",
    theme = shinytheme("simplex"),
    #Dashboard的UI
    tabPanel("Dashboard",
      icon = icon("chart-line", "fa"),
      sidebarLayout(
        sidebarPanel(width = 3,
          tags$p(style="color: #000000;",class = "step_desc",
            tags$u("Step 1"),
            ": Upload data (CSV or Excel file)", 
            tags$div(style = "display: inline-block",
              checkboxGroupButtons(
                inputId = IDs$Button$toggle_step1,
                label = NULL,
                choices = "toggle",
                selected = "toggle",
                size = "xs"
              )
            )
          ),
          conditionalPanel(condition = paste0("input.", IDs$Button$toggle_step1, "== 'toggle'"),
            fileInput(
              inputId = IDs$File$input,
              label = NULL,
              accept = c(".csv", ".xlsx")
            )
          ),
          tags$p(style="color: #000000;",class = "step_desc",
            tags$u("Step 2"),
            ": Select required variables",
            tags$div(style = "display: inline-block",
              checkboxGroupButtons(
                inputId = IDs$Button$toggle_step2,
                label = NULL,
                choices = "toggle",
                size = "xs"
              )
            )
          ),
          conditionalPanel(condition = paste0("input.", IDs$Button$toggle_step2, "== 'toggle'"),
            tags$div(class = "panel panel-primary",
              tags$div(class = "panel-heading", "Recruitment Information"),
              
              tags$div(style="color: #000000;",class = "panel-body",
                uiOutput(outputId = IDs$UI$var_select_main1),
                numericInput(
                  inputId = IDs$Keyboard$max_coupon,
                  label = "Max coupon / participant",
                  value = 1,
                  min = 1, max = 10, step = 1
                ),
                uiOutput(outputId = IDs$UI$var_select_main2)
              )
            )
          ),
          tags$p(style="color: #000000;",class = "step_desc",
            tags$u("Step 3"),
            ": Select plot type",
            tags$div(style = "display: inline-block",
              checkboxGroupButtons(
                inputId = IDs$Button$toggle_step3,
                label = NULL,
                choices = "toggle",
                size = "xs"
              )
            )
          ),
          conditionalPanel(condition = paste0("input.", IDs$Button$toggle_step3, " == 'toggle'"),
                          
            selectInput(
              inputId = IDs$Select$plot,
              label = NULL,
              choices = names(plot2vars)
            ),
            tags$div(class = "panel panel-primary",
              tags$div(class = "panel-heading", "Additional Information"),
              tags$div(style="color: #000000;",class = "panel-body",
                uiOutput(outputId = IDs$UI$var_select_other) 
              )
            ),
            actionButton(
              inputId = IDs$Button$add_tab,
              label = "Generate plots"
            )
          )
        ),
        mainPanel(width = 9,
          tabsetPanel(id = IDs$Tabset$main,
            tabPanel("Data",
              tags$div(class = "pad_top",
                DT::dataTableOutput(outputId = IDs$Table$input)
              )
            )
          )
        )
      )
    ),
    #颜色的目前不用看
    tabPanel("Settings",
      icon = icon("gear", "fa"),
      fluidRow(
        column(width = 4,
          wellPanel(
            tags$h2("Color Palette"),
            fluidRow(
              column(width = 10,
                colorPickr(
                  inputId = IDs$Select$color,
                  label = NULL,
                  update = "change",
                  interaction = list(save = FALSE, clear = FALSE, hex = FALSE, rgba = FALSE)
                )
              ),
              column(width = 2,
                actionButton(
                  inputId = IDs$Button$add_color,
                  label = "Add",
                  width = "100%"
                )
              )
            ),
            uiOutput(outputId = IDs$UI$palette),
            tags$div(id = "palette_control",
              actionButton(
                inputId = IDs$Button$set_palette,
                label = "Set"
              ),
              actionButton(
                inputId = IDs$Button$clear_palette,
                label = "Clear"
              ),
              actionButton(
                inputId = IDs$Button$default_palette,
                label = "Default"
              )
            ),
            tags$h2("Plot Download", style = "margin-top: 50px"),
            selectInput(inputId = IDs$Select$demo_format, label = tags$label("Format", style = "color: #000000;"), choices = c("png", "jpeg", "svg")),
            fluidRow(
              column(width = 4,
                numericInput(IDs$Keyboard$demo_height, label = tags$label("Height (pixels)", style = "color: #000000;"), min = 100, max = 2000, step = 100, value = 400)
              ),
              column(width = 4,
                numericInput(IDs$Keyboard$demo_width, label = tags$label("Width (in pixels)", style = "color: #000000;"), min = 100, max = 2000, step = 100, value = 600)
              ),
              column(width = 4,
                numericInput(IDs$Keyboard$demo_scale,label = tags$label("Scale", style = "color: #000000;") , min = 1, max = 5, step = 0.1, value = 1)
              )
            ),
            
          )
        ),
        column(width = 8,
          plotlyOutput(outputId = IDs$Plot$demo, height = 800)
        )
      )
    ),
    tabPanel("Info",
      icon = icon("circle-question", "fa")
    )
  )
)
  

