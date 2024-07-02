server <- function(input, output, session) {
  
  ### REACTIVES
  cleaned_data <- reactiveVal()
  data <- reactiveVal()
  rchains <- reactiveVal()
  tablist <- reactiveVal(c())
  var2col <- reactiveVal()
  plot_count <- reactiveVal(0)
  added_colors <- reactiveVal(color_settings$default)
  plot_palette <- reactiveVal(color_settings$default)
  #
  cpt_tab_values <- reactiveValues(current_tab = NULL)
  cpt_tabs_data<-reactiveValues()
  pc_tab_values <- reactiveValues(current_tab = NULL)
  pc_tabs_data<-reactiveValues()
  rc_tab_values <- reactiveValues(current_tab = NULL)
  rc_tabs_data<-reactiveValues()
  
  observeEvent(input[[IDs$File$input]], {
    var2col(c())
  })
  
  rawdata <- reactive({
    file <- input[[IDs$File$input]]
    req(file)
    path <- file$datapath

    if(str_ends(path, "csv")) {
      read_csv(path, show_col_types = FALSE)
    } else if (str_ends(path, "(xlsx|xls)")) {
      read_excel(path)
    }
  })
  
  
  
  ### SIDE PANEL
  output[[IDs$UI$var_select_main1]] <- renderUI({
    tagList(
      selectizeInput(
        inputId = IDs$Select$respID,
        label = "Respondent ID",
        choices = names(rawdata())
      ),
      selectizeInput(
        inputId = IDs$Select$rcoupon,
        label = "Redeemed coupon code",
        choices = names(rawdata())
      )
    ) 
  })
  output[[IDs$UI$var_select_main2]] <- renderUI({
    tagList(
      map(1:input[[IDs$Keyboard$max_coupon]],
          ~ selectizeInput(
            inputId = paste0("tcoupon", .x),
            label = paste0("Coupon ", .x, " code"),
            choices = names(rawdata())
          )
      )
    )
  })
  output[[IDs$UI$var_select_other]] <- renderUI({
    var2label <- plot2vars[[input[[IDs$Select$plot]]]]
    
    tagList(
      map(names(var2label),
          ~ selectizeInput(
            inputId = paste0("select_", .x),
            label = var2label[[.x]],
            choices = names(rawdata())
          )
      )  
    )
  })
  output[[IDs$Table$input]] <- DT::renderDataTable({
    rawdata() |>
      DT::datatable(
        options = list(
          scrollX = TRUE
        )
      )
  })
  observeEvent(input[[IDs$Button$add_tab]], {

    if(!(input[[IDs$Select$plot]] %in% tablist())) {
      tcoupon_colnames <- paste0("tcoupon", 1:input[[IDs$Keyboard$max_coupon]]) |>
        map(function(id) input[[id]]) |>
        unlist()
      
      clean_data(rawdata(), input[[IDs$Select$respID]]) |> cleaned_data()
      
      errors <- try({
        # get columns with recruitment info
        df <- recruitment_info(
          cleaned_data(),
          input[[IDs$Select$respID]],
          input[[IDs$Select$rcoupon]],
          tcoupon_colnames
        )
        
        # get other columns
        var2label <- plot2vars[[input[[IDs$Select$plot]]]]
        varnames <- names(var2label)
        colnames <- paste0("select_", varnames) |>
          map(function(id) input[[id]]) |>
          unlist()
        
        names(colnames) <- varnames
        var2col_temp <- c(var2col(), colnames)
        
        df_other <- cleaned_data() |> select(all_of(var2col_temp))
        names(df_other) <- names(var2col_temp)
        df <- df |> cbind(df_other)
        
        if("MAIN_DATE" %in% names(df)) {
          df <- df |> mutate(WEEK = get_week(df))
        }
        
        check_data(df)

      }, silent = TRUE)

      if ("try-error" %in% class(errors)) {
        show_alert("There was a problem processing the provided information. Please check whether the selected columns contain the correct data.", session)
      } else if(length(errors) > 0) {
        show_alert(
          tagList(
            tags$ul(
              purrr::map(unlist(errors), ~ tags$li(.x))
            )
          ),
          session
        )
      } else {
        if(input[[IDs$Select$plot]] == "Data Collection Progress") {
          tab <- create_progress_tab(
            IDs$Select$prog,
            IDs$Keyboard$prog,
            IDs$Button$prog_add,
            IDs$Tabset$prog,
            IDs$Plot$prog,
            names(cleaned_data()),
            #后面为新添加
            #title selection
            IDs$Other$cpt_title_options_hidden_id,
            IDs$Button$cpt_title_options_hidden_bottom,
            IDs$Slider$cpt_t_size,
            IDs$Slider$cpt_subt_size,
            #plot detail selection
            IDs$Other$cpt_plot_detail_options_hidden_id,
            IDs$Button$cpt_plot_detail_options_hidden_bottom,
            IDs$Slider$cpt_x_t_size,
            IDs$Slider$cpt_y_t_size,
            IDs$Slider$cpt_legend_text_size,
            IDs$Slider$cpt_x_text_size,
            IDs$Slider$cpt_y_text_size,
            #Font selection
            IDs$Other$cpt_font_options_hidden_id,
            IDs$Button$cpt_font_options_hidden_bottom,
            IDs$Select$cpt_font_family,
            IDs$Select$cpt_font_face_title,
            IDs$Select$cpt_font_face_axis,
            #
            IDs$Button$cpt_download_buttom
          )
        } else if(input[[IDs$Select$plot]] == "Participant Characteristics") {
          tab <- create_respondent_tab(
            IDs$Select$resp_main,
            IDs$Select$resp_aux,
            IDs$Keyboard$resp,
            IDs$Button$resp_add,
            IDs$Tabset$resp,
            names(cleaned_data()),
            #后面为新添加
            IDs$Other$pc_title_options_hidden_id,
            IDs$Button$pc_title_options_hidden_bottom,
            IDs$Slider$pc_title_size,
            #
            IDs$Other$pc_plot_detail_options_hidden_id,
            IDs$Button$pc_plot_detail_options_hidden_bottom,
            IDs$Slider$pc_bar_width,
            IDs$Slider$pc_legend_text_size,
            IDs$Slider$pc_x_axis_text_size,
            IDs$Slider$pc_y_axis_text_size,
            #
            IDs$Other$pc_font_options_hidden_id,
            IDs$Button$pc_font_options_hidden_bottom,
            IDs$Select$pc_font_family,
            IDs$Select$pc_font_face_title,
            #
            IDs$Button$pc_download_buttom
          )
        } else if(input[[IDs$Select$plot]] == "Recruitment Chains") {
          tab <- create_rchain_tab(
            IDs$Select$chain,
            IDs$Keyboard$chain,
            IDs$Button$chain_add,
            IDs$Tabset$chain,
            IDs$Plot$chain,
            names(cleaned_data()),
            #后面为新添加
            #
            IDs$Other$rc_title_options_hidden_id,
            IDs$Button$rc_title_options_hidden_bottom,
            IDs$Slider$rc_title_size,
            #
            IDs$Other$rc_plot_detail_options_hidden_id,
            IDs$Button$rc_plot_detail_options_hidden_bottom,
            IDs$Select$rc_layout_method,
            #
            IDs$Other$rc_font_options_hidden_id,
            IDs$Button$rc_font_options_hidden_bottom,
            IDs$Select$rc_font_family,
            #
            IDs$Button$rc_download_buttom
          )
        } else if(input[[IDs$Select$plot]] == "Geolocation") {
          rchains(get_rchains(df))
          waves <- df$WAVE |> unique() |> sort()
          seeds <- df$S_ID |> unique() |> sort()
  
          tab <- create_map_tab(waves, seeds)
        }
        
        appendTab(
          inputId = IDs$Tabset$main,
          select = TRUE,
          tab
        )
        
        data(df)
        tablist(c(tablist(), input[[IDs$Select$plot]]))
        var2col(var2col_temp)
      }
    }
  })
  
  
  # Data Collection Progress
  observeEvent(input[[IDs$Button$prog_add]], {
    plot_id <-  paste0("prog_plot_", isolate(plot_count()))
    kb <- isolate(input[[IDs$Keyboard$prog]])
    sl <- isolate(input[[IDs$Select$prog]])
    tab_title <- if(kb == '') sl else kb
    
    #储存tab_title和对应sl
    cpt_tabs_data[[tab_title]] <- sl
    
    df <- isolate(data())
    df_cleaned <- isolate(cleaned_data())
    
    if(n_distinct(df_cleaned[[sl]]) > length(plot_palette())) {
      show_alert("The number of unique values exceeds the number of colors in the selected palette.", session)
    } else {
      appendTab(
        inputId = IDs$Tabset$prog,
        select = TRUE,
        tabPanel(tab_title,
          tags$div(class = "pad_top",
            plotlyOutput(outputId = plot_id, height = 750)
          )
        )
      )
      
      output[[plot_id]] <- renderPlotly({
        df |>
          mutate(FACTOR = df_cleaned[[sl]]) |>
          collection_progress_plot(plot_palette(), list(
            format = input[[IDs$Select$demo_format]],
            height = input[[IDs$Keyboard$demo_height]],
            width = input[[IDs$Keyboard$demo_width]],
            scale = input[[IDs$Keyboard$demo_scale]]
          ),
          sl,
          input[[IDs$Keyboard$prog]],
          #后面为新添加
          input[[IDs$Slider$cpt_t_size]],
          input[[IDs$Slider$cpt_subt_size]],
          input[[IDs$Slider$cpt_x_t_size]],
          input[[IDs$Slider$cpt_y_t_size]],
          input[[IDs$Slider$cpt_legend_text_size]],
          input[[IDs$Slider$cpt_x_text_size]],
          input[[IDs$Slider$cpt_y_text_size]],
          input[[IDs$Select$cpt_font_family]],
          input[[IDs$Select$cpt_font_face_title]],
          input[[IDs$Select$cpt_font_face_axis]]
          )
      })
      plot_count(plot_count() + 1) 
    }
  })
  
  output[[IDs$Plot$prog]] <- renderPlotly({
    isolate(data()) |>
      mutate(FACTOR = "") |>
      collection_progress_plot(plot_palette(), 
      list(
        format = input[[IDs$Select$demo_format]],
        height = input[[IDs$Keyboard$demo_height]],
        width = input[[IDs$Keyboard$demo_width]],
        scale = input[[IDs$Keyboard$demo_scale]]
      ),
      "Overall",
      input[[IDs$Keyboard$prog]],
      #后面为新添加
      input[[IDs$Slider$cpt_t_size]],
      input[[IDs$Slider$cpt_subt_size]],
      input[[IDs$Slider$cpt_x_t_size]],
      input[[IDs$Slider$cpt_y_t_size]],
      input[[IDs$Slider$cpt_legend_text_size]],
      input[[IDs$Slider$cpt_x_text_size]],
      input[[IDs$Slider$cpt_y_text_size]],
      input[[IDs$Select$cpt_font_family]],
      input[[IDs$Select$cpt_font_face_title]],
      input[[IDs$Select$cpt_font_face_axis]]
      
      )
      
  })
  # 可以在控制台输出当前选中的tab
  observe({
    cpt_tab_values$current_tab <- input[[IDs$Tabset$prog]]  # 获取当前选中的tab名字
  })   
  
  #设置下载按钮的输出
  output[[IDs$Button$cpt_download_buttom]]<-downloadHandler(
    #设置文件名字
    filename=paste(paste("Data_Collection_Progress",input[[IDs$Keyboard$prog]],sep="_"),"png",sep=".") ,
    #设置文件内容      
    content = function(file){
      current_tab <- cpt_tab_values$current_tab
      print(current_tab)
      #sl_factor<-current_tab
      if(current_tab=='Overall'){
        isolated_data <- isolate(data())
        prepared_data <-   isolated_data |> mutate(FACTOR = "")
        
        p<-collection_progress_plot_down(
            prepared_data,
            plot_palette(),
            "Overall",
            input[[IDs$Keyboard$prog]],
            #后面为新添加
            input[[IDs$Slider$cpt_t_size]],
            input[[IDs$Slider$cpt_subt_size]],
            input[[IDs$Slider$cpt_x_t_size]],
            input[[IDs$Slider$cpt_y_t_size]],
            input[[IDs$Slider$cpt_legend_text_size]],
            input[[IDs$Slider$cpt_x_text_size]],
            input[[IDs$Slider$cpt_y_text_size]],
            input[[IDs$Select$cpt_font_family]],
            input[[IDs$Select$cpt_font_face_title]],
            input[[IDs$Select$cpt_font_face_axis]]
          )
        plot_width_ui <- 9.6 # 假定UI的宽度为10英寸
        plot_height_ui <- 750 / 96 # 假定UI的高度设置为750像素, 96是转换因子
        
        # 使用ggsave保存图形，尺寸应该接近UI中的显示区域大小
        ggsave(file, plot = p, width = plot_width_ui, height = plot_height_ui, units = "in", dpi = 96,bg = "white")
      }else{
        df <- isolate(data())
        df_cleaned <- isolate(cleaned_data())
        #通过tab_title获得factor
        sl_factor <- cpt_tabs_data[[current_tab]]
        print(sl_factor)
        
      }
    }
  )
  observeEvent(input[[IDs$Button$cpt_title_options_hidden_bottom]], {
    shinyjs::toggle(id = IDs$Other$cpt_title_options_hidden_id)  # Toggle the visibility of the font options
  })
  observeEvent(input[[IDs$Button$cpt_plot_detail_options_hidden_bottom]], {
    shinyjs::toggle(id = IDs$Other$cpt_plot_detail_options_hidden_id)  # Toggle the visibility of the font options
  })
  observeEvent(input[[IDs$Button$cpt_font_options_hidden_bottom]], {
      shinyjs::toggle(id = IDs$Other$cpt_font_options_hidden_id)  # Toggle the visibility of the font options
  })
  
  
  # Participant Characteristics
  observeEvent(input[[IDs$Button$resp_add]], {
    plot_id <-  paste0("resp_plot_", isolate(plot_count()))
    kb <- isolate(input[[IDs$Keyboard$resp]])
    sl1 <- isolate(input[[IDs$Select$resp_main]])
    sl2 <- isolate(input[[IDs$Select$resp_aux]])
    tab_title <- if(kb == '') sl1 else kb
    #储存tab_title和对应sl
    pc_tabs_data[[tab_title]] <- sl1
    
    df <- isolate(data())
    df_cleaned <- isolate(cleaned_data())
    max_level_count <- 10
    
    if(sl2 == "") {
      if(n_distinct(df_cleaned[[sl1]]) > max_level_count) {
        show_alert("The selected columns contain too many unique values.", session)
      } else {
        appendTab(
          inputId = IDs$Tabset$resp,
          select = TRUE,
          tabPanel(tab_title,
                   tags$div(class = "pad_top",
                            plotlyOutput(outputId = plot_id, height = 750)
                   )
          )
        )
        
        output[[plot_id]] <- renderPlotly({
          df |>
            mutate(FACTOR = df_cleaned[[sl1]]) |>
            respondent_plot1(list(
              format = input[[IDs$Select$demo_format]],
              height = input[[IDs$Keyboard$demo_height]],
              width = input[[IDs$Keyboard$demo_width]],
              scale = input[[IDs$Keyboard$demo_scale]]
            ),sl1,
            input[[IDs$Keyboard$resp]],
            #以下为新添加
            input[[IDs$Slider$pc_bar_width]],
            input[[IDs$Slider$pc_title_size]],
            input[[IDs$Slider$pc_legend_text_size]],
            input[[IDs$Slider$pc_x_axis_text_size]],
            input[[IDs$Slider$pc_y_axis_text_size]],
            input[[IDs$Select$pc_font_family]],
            input[[IDs$Select$pc_font_face_title]]
            )
        })
        
        plot_count(plot_count() + 1) 
      }
    } else {
      if(n_distinct(df_cleaned[[sl2]]) > max_level_count | n_distinct(df_cleaned[[sl1]]) > length(plot_palette())) {
        show_alert("The selected columns contain too many unique values.", session)
      } else {
        appendTab(
          inputId = IDs$Tabset$resp,
          select = TRUE,
          tabPanel(tab_title,
                   tags$div(class = "pad_top",
                            plotlyOutput(outputId = plot_id, height = 750)
                   )
          )
        )
        
        output[[plot_id]] <- renderPlotly({
          df |>
            mutate(
              MAIN = df_cleaned[[sl1]],
              AUX = df_cleaned[[sl2]]
            ) |>
            respondent_plot2(plot_palette(), list(
              format = input[[IDs$Select$demo_format]],
              height = input[[IDs$Keyboard$demo_height]],
              width = input[[IDs$Keyboard$demo_width]],
              scale = input[[IDs$Keyboard$demo_scale]]
            ),sl1,sl2,
            input[[IDs$Keyboard$resp]],
            #以下为新添加
            input[[IDs$Slider$pc_bar_width]],
            input[[IDs$Slider$pc_title_size]],
            input[[IDs$Slider$pc_legend_text_size]],
            input[[IDs$Slider$pc_x_axis_text_size]],
            input[[IDs$Slider$pc_y_axis_text_size]],
            input[[IDs$Select$pc_font_family]],
            input[[IDs$Select$pc_font_face_title]])
        })
        
        plot_count(plot_count() + 1) 
      }
    }
  })
  
  # 可以在控制台输出当前选中的tab
  observe({
    pc_tab_values$current_tab <- input[[IDs$Tabset$resp]]  # 获取当前选中的tab名字
  })   
  
  #设置下载按钮的输出
  output[[IDs$Button$pc_download_buttom]]<-downloadHandler(
    #设置文件名字
    filename=paste(paste("Participant_Characteristics",input[[IDs$Keyboard$resp]],sep="_"),"png",sep=".") ,
    #设置文件内容      
    content = function(file){
      current_tab <- pc_tab_values$current_tab
        
        df <- isolate(data())
        df_cleaned <- isolate(cleaned_data())
        #通过tab_title获得factor
        sl_factor <- pc_tabs_data[[current_tab]]
        
        #png(file)
        p<-df |>
          mutate(FACTOR = df_cleaned[[sl_factor]]) |>
          respondent_plot_down(plot_palette(),
          sl_factor,
          input[[IDs$Keyboard$resp]],
          #以下为新添加
          input[[IDs$Slider$pc_bar_width]],
          input[[IDs$Slider$pc_title_size]],
          input[[IDs$Slider$pc_legend_text_size]],
          input[[IDs$Slider$pc_x_axis_text_size]],
          input[[IDs$Slider$pc_y_axis_text_size]],
          input[[IDs$Select$pc_font_family]],
          input[[IDs$Select$pc_font_face_title]]
          )
        #dev.off()
        #ggsave(file, plot = p, device = "png")
        #plotly::to_image(p, file = file, format = "png")
        # 获取UI显示的宽度值，这可能需要您手动调整或从UI设计中获取
        plot_width_ui <- 9.6 # 假定UI的宽度为10英寸
        plot_height_ui <- 750 / 96 # 假定UI的高度设置为750像素, 96是转换因子
        
        # 使用ggsave保存图形，尺寸应该接近UI中的显示区域大小
        ggsave(file, plot = p, width = plot_width_ui, height = plot_height_ui, units = "in", dpi = 96,bg = "white")
    }
  )
  
    observeEvent(input[[IDs$Button$pc_title_options_hidden_bottom]], {
      shinyjs::toggle(id = IDs$Other$pc_title_options_hidden_id)  # Toggle the visibility of the font options
    })
    observeEvent(input[[IDs$Button$pc_plot_detail_options_hidden_bottom]], {
      shinyjs::toggle(id = IDs$Other$pc_plot_detail_options_hidden_id)  # Toggle the visibility of the font options
    })
    observeEvent(input[[IDs$Button$pc_font_options_hidden_bottom]], {
      shinyjs::toggle(id = IDs$Other$pc_font_options_hidden_id)  # Toggle the visibility of the font options
    })  
   

    
   
#Recruitment chain
  output[[IDs$Plot$chain]] <- renderPlot({
    isolate(data()) |>
      mutate(FACTOR = "") |>
      recruitment_chain_graph(plot_palette(),"Seed",
                              input[[IDs$Keyboard$chain]],
                              input[[IDs$Select$rc_layout_method]],
                              input[[IDs$Slider$rc_title_size]],
                              input[[IDs$Select$rc_font_family]])
  })
  
  
  observeEvent(input[[IDs$Button$chain_add]], {
    plot_id <-  paste0("chain_plot_", isolate(plot_count())) 
    kb <- isolate(input[[IDs$Keyboard$chain]])
    sl <- isolate(input[[IDs$Select$chain]])
    tab_title <- if(kb == '') sl else kb
    #储存tab_title和对应sl
    rc_tabs_data[[tab_title]] <- sl
    
    #print(tabs_data[[tab_title]]$factor)
    df <- isolate(data())
    df_cleaned <- isolate(cleaned_data())
    
    if(n_distinct(df_cleaned[[sl]]) > length(plot_palette())) {
      show_alert("The number of unique values exceeds the number of colors in the selected palette.", session)
    } else {
      appendTab(
        inputId = IDs$Tabset$chain,
        select = TRUE,
        tabPanel(tab_title,
                 div(plotOutput(outputId = plot_id, height = 750),
                     #添加隐藏的plot_id显示
                     hidden(textInput(inputId = plot_id, label = NULL, value = plot_id)))
                
        )
      )
      
      output[[plot_id]] <- renderPlot({
        df |>
          mutate(FACTOR = df_cleaned[[sl]]) |>
          recruitment_chain_graph(plot_palette(),sl,
                                  input[[IDs$Keyboard$chain]],
                                  input[[IDs$Select$rc_layout_method]],
                                  input[[IDs$Slider$rc_title_size]],
                                  input[[IDs$Select$rc_font_family]])
      })
      
      
      plot_count(plot_count() + 1)
    }
    
  })

# 可以在控制台输出当前选中的tab
    observe({
      rc_tab_values$current_tab <- input[[IDs$Tabset$chain]]  # 获取当前选中的tab名字
      print(input[[IDs$Tabset$chain]])

    })   

#设置下载按钮的输出
  output[[IDs$Button$rc_download_buttom]]<-downloadHandler(
    #设置文件名字
    filename=paste(paste("recruitment_chain",input[[IDs$Keyboard$chain]],sep="_"),"png",sep=".") ,
    #设置文件内容      
    content = function(file){
      current_tab <- rc_tab_values$current_tab
      #sl_factor<-current_tab
      if(current_tab=='Seed'){
        png(file)
        isolate(data()) |>
        mutate(FACTOR = "") |>
        recruitment_chain_graph(plot_palette(),"Seed",
                                input[[IDs$Keyboard$chain]],
                                input[[IDs$Select$rc_layout_method]],
                                input[[IDs$Slider$rc_title_size]],
                                input[[IDs$Select$rc_font_family]])

        dev.off()
      }else{

        df <- isolate(data())
        df_cleaned <- isolate(cleaned_data())
       #通过tab_title获得factor
        sl_factor <- rc_tabs_data[[current_tab]]
        
        
        png(file)
        df|>
          mutate(FACTOR = df_cleaned[[sl_factor]]) |>
          recruitment_chain_graph(plot_palette(),sl_factor,
                                  input[[IDs$Keyboard$chain]],
                                  input[[IDs$Select$rc_layout_method]],
                                  input[[IDs$Slider$rc_title_size]],
                                  input[[IDs$Select$rc_font_family]])
        dev.off()
      }
    }
    )
      
  observeEvent(input[[IDs$Button$rc_title_options_hidden_bottom]], {
    shinyjs::toggle(id = IDs$Other$rc_title_options_hidden_id)  # Toggle the visibility of the font options
  })
  observeEvent(input[[IDs$Button$rc_plot_detail_options_hidden_bottom]], {
    shinyjs::toggle(id = IDs$Other$rc_plot_detail_options_hidden_id)  # Toggle the visibility of the font options
  })
  observeEvent(input[[IDs$Button$rc_font_options_hidden_bottom]], {
    shinyjs::toggle(id = IDs$Other$rc_font_options_hidden_id)  # Toggle the visibility of the font options
  })    
  
  # Map
  output[[IDs$Plot$map]] <- renderLeaflet({
    df <- data() |> filter(
      S_ID %in% input[[IDs$Select$seeds]],
      WAVE %in% input[[IDs$Select$waves]]
    )

    flow <- rchains() |> filter(
      S_ID %in% input[[IDs$Select$seeds]],
      WAVE %in% input[[IDs$Select$waves]]
    )

    map <- NULL
    if(nrow(df) > 0) {
      map <- leaflet() |>
        addProviderTiles("CartoDB.Positron") |>
        addCircleMarkers(
          lat = df$LAT[df$SEED == 1],
          lng = df$LNG[df$SEED == 1],
          popup = df$ID[df$SEED == 1],
          radius = 4,
          weight = 1,
          opacity = 0.9,
          color = "#CD0000"
        ) |>
        addCircleMarkers(
          lat = df$LAT[df$SEED == 0],
          lng = df$LNG[df$SEED == 0],
          popup = df$ID[df$SEED == 0],
          radius = 4,
          weight = 1,
          opacity = 0.9,
          color = "#63B8FF"
        ) |>
        addLegend(
          position = "topright",
          labels = c("Seed", "Not seed"),
          colors = c("#CD0000", "#63B8FF")
        ) 

        if(nrow(flow) > 0) {
          map <- map |> addFlows(
            lat0 = flow$LAT0, lng0 = flow$LNG0,
            lat1 = flow$LAT1, lng1 = flow$LNG1,
            dir = 1,
            color = "black", opacity = 0.5,
            # popup = flow$WAVE,
            minThickness = 0.5, maxThickness = 1.5
          )
        }
    }

    return(map)
  })
  
  #以下为添加颜色
  observeEvent(input[[IDs$Button$add_color]], {
    if(length(added_colors()) < color_settings$max_color) {
      added_colors(c(added_colors(), input[[IDs$Select$color]]))
    } else {
      show_alert("Maximum number of colors has been reached.", session)
    }
    
  })
  
  observeEvent(input[[IDs$Button$clear_palette]], {
    added_colors(c())
  })

  observeEvent(input[[IDs$Button$default_palette]], {
    added_colors(color_settings$default)
    plot_palette(color_settings$default)
  })

  observeEvent(input[[IDs$Button$set_palette]], {
    if(length(added_colors()) <= color_settings$max_colors
       & length(added_colors()) >= color_settings$min_colors) {
      plot_palette(added_colors())
    } else {
      show_alert(sprintf("Palette must consist of between %d and %d colors.", color_settings$min_colors, color_settings$max_colors), session)
    }
  })

  output[[IDs$UI$palette]] <- renderUI({
    tags$div(id = "palette",
      map(added_colors(), ~ tags$div(style = sprintf("display: inline-block; height: 36px; width: 12.5%%; background-color: %s", .x)))
    )
  })

  output[[IDs$Plot$demo]] <- renderPlotly({
    demo_plot(plot_palette(), list(
      format = input[[IDs$Select$demo_format]],
      height = input[[IDs$Keyboard$demo_height]],
      width = input[[IDs$Keyboard$demo_width]],
      scale = input[[IDs$Keyboard$demo_scale]]
    ))
  })
  
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
  
}
