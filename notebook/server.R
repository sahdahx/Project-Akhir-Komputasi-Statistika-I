# ============================================================================
# DASHBOARD RANDOM NUMBER GENERATOR - R SHINY
# ============================================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)
library(readr)
library(readxl)
library(tidyr)
library(dplyr)

# ============================================================================
# SERVER SECTION
# ============================================================================

server <- function(input, output, session) {
  
  # -------------------------
  # Uji 1 Populasi
  # -------------------------
  
  data_1popul <- reactive({
    
    # 1. Check if a file is present
    req(input$file_1popul)
    
    # Get necessary info
    file_path <- input$file_1popul$datapath
    file_name <- input$file_1popul$name
    
    # 2. Extract file extension for conditional reading
    # Get everything after the last dot in the filename (e.g., "csv" from "data.csv")
    ext <- tools::file_ext(file_name) 
    
    data <- switch(ext,
                   # Case 1: CSV files
                   "csv" = read.csv(file_path, header = input$header_1popul, stringsAsFactors = FALSE),
                   
                   # Case 2: Text files (assuming space or tab delimited)
                   "txt" = read.delim(file_path, header = input$header_1popul, stringsAsFactors = FALSE),
                   
                   # Case 3: Excel files (using readxl package)
                   "xlsx" = readxl::read_excel(file_path, col_names = input$header_1popul),
                   
                   # Default case (if the file type is not supported)
                   stop(paste("Tipe file tidak didukung:", ext))
    )
    
    return(data)
  })
  
  observeEvent(data_1popul(), {
    
    # Get the names of the columns from the uploaded data frame
    data_columns <- names(data_1popul())
    
    # Update the selectInput widget
    updateSelectInput(
      session = session,
      inputId = "column_1popul",
      label = "Pilih Kolom Uji:",
      choices = data_columns,
      selected = data_columns[1] # Automatically select the first column
    )
  })
  
  observeEvent(input$column_1popul,{
    selected_column_data_1popul <- reactive({
      # Ensure a column is selected before trying to subset the data
      req(input$column_1popul) 
      
      # Select the column using the name provided by the input
      data_frame <- data_1popul()
      
      # Using double brackets [[]] extracts the column as a vector
      column_vector <- data_frame[[input$column_1popul]] 
      
      return(column_vector)
    })
    
    output$table_1popul <- renderTable({
      
      # 1. Ensure the required inputs are available
      req(input$column_1popul)
      
      # 2. Get the vector data
      data_vector <- selected_column_data_1popul()
      
      # 3. Create a single-column data frame for display
      # Use the name the user selected as the column header
      data_to_display <- data.frame(data_vector)
      names(data_to_display) <- input$column_1popul # Set the column name
      
      # 4. Return the data frame (can limit to the first 100 rows if needed)
      head(data_to_display, 10) 
      
    },
    bordered = TRUE, # Adds borders to the cells
    striped = TRUE,  # Alternating row colors
    hover = TRUE,    # Highlight rows on mouse hover
    rownames = TRUE, # Keeps the row numbers (indices)
    width = "auto")   # Let the table size itself naturally)
    
    
  })
  
  #33
  observeEvent(input$test_1popul,{
    data_1_populasi <- data_1popul()
    data_1_populasi <- data.frame(values = data_1_populasi[[input$column_1popul]])
    
    mean_1popul <- mean(as.numeric(data_1_populasi$values), na.rm = TRUE)
    sd_1popul <- sd(as.numeric(data_1_populasi$values), na.rm = TRUE)
    
    curve_1popul <- data.frame(
      x = seq(min(data_1_populasi$values), max(data_1_populasi$values), length.out = 200)
    )
    curve_1popul$y <- dnorm(curve_1popul$x, mean = mean_1popul, sd = sd_1popul)
    
    if(input$normal_1popul == TRUE){
      
      output$hist_norm_1popul <- renderPlotly({
        plot1 <- ggplot(data_1_populasi, aes(x = values)) +
          geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#3c8dbc", color = "white") + 
          geom_line(data = curve_1popul, aes(x = x, y = y), color = "red", linewidth = 1.2) +
          theme_minimal() +
          labs(x = "Nilai", y = "Frekuensi")
        
        ggplotly(plot1)
      })
      
      hasil_norm <- round(shapiro.test(data_1_populasi$values)$p.value,3)
      tolakan <- "ditolak"
      kesimpulan <- "data tidak berdistribusi normal"
      banding <- "kurang dari"
      
      if(hasil_norm > 0.05){
        tolakan <- "tidak ditolak"
        kesimpulan <- "data berdistribusi normal"
        banding <- "lebih dari"
      }
      
      
      
      output$norm_inter_1popul <- renderUI({
        
        # Create an HTML <div> tag
        tags$div(
          class = "variable-display", # Apply the CSS class defined above, yang tadi
          paste("Diberikan data", var1," kolom ", var2,
                ".Ingin diuji apakah data berdistribusi normal atau tidak.
                Dengan hipotesis nol, yaitu data berdistribusi normal, dan
                hipotesis alternatif, yaitu data tidak berdistribusi normal.
                Dengan daerah kritik berupa P-value kurang dari alpha = 0.05,
                ditemukan nilai P-value = ",hasil_norm,",maka hipotesis nol",tolakan,".
                Dengan demikian, ",kesimpulan,".")     # Inject the R variable content
        )
      })
      
      hasil_norm = round(hasil_norm,3)
      kesimpulan_norm_1popul = paste("Karena nilai P-value =",hasil_norm, banding,"α = 0.05
                , maka H0",tolakan,".Dengan demikian, ",kesimpulan,".")
    }
    
    
    var1 <- input$file_1popul$name
    var2 <- input$column_1popul
    
    if(input$normal_1popul == FALSE){
      output$hist_norm_1popul <- renderPlotly({
        plot1 <- ggplot(data_1_populasi, aes(x = values)) +
          geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#3c8dbc", color = "white") + 
          theme_minimal() +
          labs(x = "Nilai", y = "Frekuensi")
        
        ggplotly(plot1)
      })
      
      output$norm_inter_1popul <- renderUI({
        
        # Create an HTML <div> tag
        tags$div(
          class = "variable-display", # Apply the CSS class defined above, yang tadi
          paste("Data diasumsikan berdistribusi normal")     # Inject the R variable content
        )
      })
      
      hasil_norm = "(diasumsikan lebih dari alpha)"
      kesimpulan_norm_1popul = "Diasumsikan data berditribusi normal"
    }
    
    #SERVER hipotesis normal 1popul======
    output$hipo1_norm_1popul_1 <- renderUI({
      tags$div(
        class = "variable-display", 
        paste("H0: Data berdistribusi normal")    
      )
    })
    output$hipo1_norm_1popul_2 <- renderUI({
      tags$div(
        class = "variable-display", 
        paste("H1: Data tidak berdistribusi normal")    
      )
    })
    output$hipo2_norm_1popul <- renderUI({
      tags$div(
        class = "variable-display", 
        paste("α = 0,05")    
      )
    })
    output$hipo3_norm_1popul <- renderUI({
      tags$div(
        class = "variable-display", 
        paste("P-value = ",hasil_norm)    
      )
    })
    output$hipo4_norm_1popul <- renderUI({
      tags$div(
        class = "variable-display", 
        paste("H0 ditolak jika P-value < α")    
      )
    })
    output$hipo5_norm_1popul <- renderUI({
      tags$div(
        class = "variable-display", 
        paste(kesimpulan_norm_1popul)    
      )
    })
    
    
    output$hist_uji_1popul <- renderPlotly({
      plot2 <- ggplot(data_1_populasi, aes(x = values)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#3c8dbc", color = "white") + 
        geom_vline(xintercept = mean_1popul, color = "red",linewidth = 1.2, linetype = "solid") +
        geom_vline(xintercept = input$mean_1popul, color = "green",linewidth = 1.2, linetype = "solid") +
        theme_minimal() +
        labs(x = "Nilai", y = "Frekuensi")
      
      ggplotly(plot2)
    })
    
    if(input$testing_1popul == "test1_1popul"){
      
      test_t<-t.test(data_1_populasi$values, alternative = "two.sided", mu=input$mean_1popul,
                     conf.level = 0.95)
      
      tolakan = "tidak ditolak"
      kesimpulan = "rata-rata populasi sama dengan nilai mean hipotesis"
      banding <- "lebih dari"
      
      if(test_t$p.value < 0.05){
        tolakan = "ditolak"
        kesimpulan = "rata-rata populasi tidak sama dengan nilai mean hipotesis"
        banding <- "kurang dari"
      }
      
      hipotesis0 = "H0: μ = μ0"
      hipotesis1 = "H1: μ ≠ μ0"
      P_uji_1popul = round(test_t$p.value,3)
      kesimpulan_uji_1popul = paste("Karena nilai P-value =",P_uji_1popul, banding,"α = 0.05
                , maka H0",tolakan,".Dengan demikian, ",kesimpulan,".")
      interpretasi_uji_1popul = paste(
        "Diberikan data", var1," kolom ", var2,
        ".Ingin diuji apakah rata-rata populasi sama dengan nilai mean hipotesis.
                Dengan hipotesis nol, yaitu rata-rata populasi sama dengan nilai mean hipotesis, dan
                hipotesis alternatif, yaitu rata-rata populasi tidak sama dengan nilai mean hipotesis.
                Dengan daerah kritik berupa P-value kurang dari alpha = 0.05,
                ditemukan nilai P-value = ",P_uji_1popul,",maka hipotesis nol",tolakan,".
                Dengan demikian, ",kesimpulan,".")
    }
    
    if(input$testing_1popul == "test2_1popul"){
      
      test_t<-t.test(data_1_populasi$values, alternative = "greater", mu=input$mean_1popul,
                     conf.level = 0.95)
      
      tolakan = "tidak ditolak"
      kesimpulan = "rata-rata populasi lebih kecil dari nilai mean hipotesis"
      banding <- "lebih dari"
      
      if(test_t$p.value < 0.05){
        tolakan = "ditolak"
        kesimpulan = "rata-rata populasi lebih besar dari nilai mean hipotesis"
        banding <- "kurang dari"
      }
      
      hipotesis0 = "H0: μ ≤ μ0"
      hipotesis1 = "H1: μ > μ0"
      P_uji_1popul = round(test_t$p.value,3)
      kesimpulan_uji_1popul = paste("Karena nilai P-value =",P_uji_1popul, banding,"α = 0.05
                , maka H0",tolakan,".Dengan demikian, ",kesimpulan,".")
      interpretasi_uji_1popul = paste(
        "Diberikan data", var1," kolom ", var2,
        ".Ingin diuji apakah rata-rata populasi lebih kecil dari nilai mean hipotesis.
                Dengan hipotesis nol, yaitu rata-rata populasi lebih kecil dari nilai mean hipotesis, dan
                hipotesis alternatif, rata-rata populasi lebih besar dari nilai mean hipotesis.
                Dengan daerah kritik berupa P-value kurang dari alpha = 0.05,
                ditemukan nilai P-value = ",P_uji_1popul,",maka hipotesis nol",tolakan,".
                Dengan demikian, ",kesimpulan,".")
    }#============================================================
    
    if(input$testing_1popul == "test3_1popul"){
      test_t<-t.test(data_1_populasi$values, alternative = "less", mu=input$mean_1popul,
                     conf.level = 0.95)
      tolakan = "tidak ditolak"
      kesimpulan = "rata-rata populasi lebih besar dari nilai mean hipotesis"
      
      if(test_t$p.value < 0.05){
        tolakan = "ditolak"
        kesimpulan = "rata-rata populasi lebih kecil dari nilai mean hipotesis"
      }
      
      hipotesis0 = "H0: μ ≥ μ0"
      hipotesis1 = "H1: μ < μ0"
      P_uji_1popul = round(test_t$p.value,3)
      kesimpulan_uji_1popul = paste("Karena nilai P-value =",P_uji_1popul, banding,"α = 0.05
                , maka H0",tolakan,".Dengan demikian, ",kesimpulan,".")
      interpretasi_uji_1popul = paste(
        "Diberikan data", var1," kolom ", var2,
        ".Ingin diuji apakah rata-rata populasi lebih besar dari nilai mean hipotesis.
                Dengan hipotesis nol, yaitu rata-rata populasi lebih besar dari nilai mean hipotesis, dan
                hipotesis alternatif, rata-rata populasi lebih kecil dari nilai mean hipotesis.
                Dengan daerah kritik berupa P-value kurang dari alpha = 0.05,
                ditemukan nilai P-value = ",P_uji_1popul,",maka hipotesis nol",tolakan,".
                Dengan demikian, ",kesimpulan,".")
    }
    
    #SERVER hipotesis uji 1popul======
    output$hipo1_uji_1popul_1 <- renderUI({
      tags$div(
        class = "variable-display", 
        paste(hipotesis0)    
      )
    })
    output$hipo1_uji_1popul_2 <- renderUI({
      tags$div(
        class = "variable-display", 
        paste(hipotesis1)    
      )
    })
    output$hipo2_uji_1popul <- renderUI({
      tags$div(
        class = "variable-display", 
        paste("α = 0,05")    
      )
    })
    output$hipo3_uji_1popul <- renderUI({
      tags$div(
        class = "variable-display", 
        paste("P-value = ",P_uji_1popul)    
      )
    })
    output$hipo4_uji_1popul <- renderUI({
      tags$div(
        class = "variable-display", 
        paste("H0 ditolak jika P-value < α")    
      )
    })
    output$hipo5_uji_1popul <- renderUI({
      tags$div(
        class = "variable-display", 
        paste(kesimpulan_uji_1popul)    
      )
    })
    
    #Server Interpretasi Uji 1 popul=====
    output$uji_inter_1popul <- renderUI({
      tags$div(
        class = "variable-display", 
        paste(interpretasi_uji_1popul)     
      )
    })
    
    
    bagian_server_1popul_akhir <- function(tagged){}
    
  }) #tags observe event
  
  
  
  # -------------------------
  # Uji 2 Populasi Independen
  # -------------------------
  
  data_2indepen <- reactive({
    
    # 1. Check if a file is present
    req(input$file_2indepen)
    
    # Get necessary info
    file_path <- input$file_2indepen$datapath
    file_name <- input$file_2indepen$name
    
    # 2. Extract file extension for conditional reading
    # Get everything after the last dot in the filename (e.g., "csv" from "data.csv")
    ext <- tools::file_ext(file_name) 
    
    data <- switch(ext,
                   # Case 1: CSV files
                   "csv" = read.csv(file_path, header = input$header_2indepen, stringsAsFactors = FALSE),
                   
                   # Case 2: Text files (assuming space or tab delimited)
                   "txt" = read.delim(file_path, header = input$header_2indepen, stringsAsFactors = FALSE),
                   
                   # Case 3: Excel files (using readxl package)
                   "xlsx" = readxl::read_excel(file_path, col_names = input$header_2indepen),
                   
                   # Default case (if the file type is not supported)
                   stop(paste("Tipe file tidak didukung:", ext))
    )
    
    return(data)
  })
  
  observeEvent(data_2indepen(), {
    
    # Get the names of the columns from the uploaded data frame
    data_columns <- names(data_2indepen())
    
    # Update the selectInput widget
    updateSelectInput(
      session = session,
      inputId = "column1_2indepen",
      label = "Pilih Kolom:",
      choices = data_columns,
      selected = data_columns[1] # Automatically select the first column
    )
  })
  
  observeEvent(input$column1_2indepen,{
    selected_column1_data_2indepen <- reactive({
      # Ensure a column is selected before trying to subset the data
      req(input$column1_2indepen) 
      
      # Select the column using the name provided by the input
      data_frame <- data_2indepen()
      
      # Using double brackets [[]] extracts the column as a vector
      column1_vector <- data_frame[[input$column1_2indepen]] 
      
      return(column1_vector)
    })
    
    output$table1_2indepen <- renderTable({
      
      # 1. Ensure the required inputs are available
      req(input$column1_2indepen)
      
      # 2. Get the vector data
      data_vector1 <- selected_column1_data_2indepen()
      
      # 3. Create a single-column data frame for display
      # Use the name the user selected as the column header
      data_to_display1 <- data.frame(data_vector1)
      names(data_to_display1) <- input$column1_2indepen # Set the column name
      
      # 4. Return the data frame (can limit to the first 100 rows if needed)
      head(data_to_display1, 10) 
      
    },
    bordered = TRUE, # Adds borders to the cells
    striped = TRUE,  # Alternating row colors
    hover = TRUE,    # Highlight rows on mouse hover
    rownames = TRUE) # Keeps the row numbers (indices)
    
    
  })
  #==============================================
  
  observeEvent(data_2indepen(), {
    
    # Get the names of the columns from the uploaded data frame
    data_columns <- names(data_2indepen())
    
    # Update the selectInput widget
    updateSelectInput(
      session = session,
      inputId = "column2_2indepen",
      label = "Pilih Kolom:",
      choices = data_columns,
      selected = data_columns[1] # Automatically select the first column
    )
  })
  
  observeEvent(input$column2_2indepen,{
    selected_column2_data_2indepen <- reactive({
      # Ensure a column is selected before trying to subset the data
      req(input$column2_2indepen) 
      
      # Select the column using the name provided by the input
      data_frame <- data_2indepen()
      
      # Using double brackets [[]] extracts the column as a vector
      column2_vector <- data_frame[[input$column2_2indepen]] 
      
      return(column2_vector)
    })
    
    output$table2_2indepen <- renderTable({
      
      # 1. Ensure the required inputs are available
      req(input$column2_2indepen)
      
      # 2. Get the vector data
      data_vector2 <- selected_column2_data_2indepen()
      
      # 3. Create a single-column data frame for display
      # Use the name the user selected as the column header
      data_to_display2 <- data.frame(data_vector2)
      names(data_to_display2) <- input$column2_2indepen # Set the column name
      
      # 4. Return the data frame (can limit to the first 100 rows if needed)
      head(data_to_display2, 10) 
      
    },
    bordered = TRUE, # Adds borders to the cells
    striped = TRUE,  # Alternating row colors
    hover = TRUE,    # Highlight rows on mouse hover
    rownames = TRUE) # Keeps the row numbers (indices)
    
    
  })
  
  #33===================================
  observeEvent(input$test_2indepen,{
    data_2_independen <- data.frame(data_2indepen())
    data_2_independen <- data.frame(value1 = data_2_independen[[input$column1_2indepen]],
                                    value2 = data_2_independen[[input$column2_2indepen]])
    
    jumlah_kolom1 = ncol(data_2_independen)-1
    jumlah_kolom2 = ncol(data_2_independen) 
    
    long_2_independen <- data_2_independen %>%
      pivot_longer(
        cols = jumlah_kolom1:jumlah_kolom2,
        names_to = "tipe",
        values_to = "value"
      )
    
    #garis normal 1====
    
    mean1_2indepen <- mean(as.numeric(data_2_independen$value1), na.rm = TRUE)
    sd1_2indepen <- sd(as.numeric(data_2_independen$value1), na.rm = TRUE)
    
    curve1_2indepen <- data.frame(
      x = seq(min(data_2_independen$value1), max(data_2_independen$value1), length.out = 200)
    )
    curve1_2indepen$y <- dnorm(curve1_2indepen$x, mean = mean1_2indepen, sd = sd1_2indepen)
    
    #garis normal 2====
    
    mean2_2indepen <- mean(as.numeric(data_2_independen$value2), na.rm = TRUE)
    sd2_2indepen <- sd(as.numeric(data_2_independen$value2), na.rm = TRUE)
    
    print(mean2_2indepen)
    print(mean1_2indepen)
    
    curve2_2indepen <- data.frame(
      x = seq(min(data_2_independen$value2), max(data_2_independen$value2), length.out = 200)
    )
    curve2_2indepen$y <- dnorm(curve2_2indepen$x, mean = mean2_2indepen, sd = sd2_2indepen)
    
    if(input$normal_2indepen == TRUE){
      
      output$hist_norm_2indepen <- renderPlotly({
        plot2 <- ggplot(long_2_independen, aes(x = value, fill = tipe)) +
          geom_histogram(aes(y = after_stat(density)), alpha = 0.5, position = "identity", bins = 30) + 
          geom_line(data = curve1_2indepen, aes(x = x, y = y), color = "red", linewidth = 1.2, inherit.aes = FALSE) +
          geom_line(data = curve2_2indepen, aes(x = x, y = y), color = "blue", linewidth = 1.2, inherit.aes = FALSE) +
          coord_cartesian(ylim = c(0, 1)) +
          theme_minimal() +
          labs(x = "Nilai", y = "Frekuensi")
        
        ggplotly(plot2)
      })
      
      #hasil norm 1====
      
      hasil_norm1 <- round(shapiro.test(data_2_independen$value1)$p.value,3)
      tolakan1 <- "ditolak"
      kesimpulan1 <- "data tidak berdistribusi normal"
      banding1 = "kurang dari"
      
      if(hasil_norm1 > 0.05){
        tolakan1 <- "tidak ditolak"
        kesimpulan1 <- "data berdistribusi normal"
        banding1 = "lebih dari"
      }
      
      #hasil norm 2====
      
      hasil_norm2 <- round(shapiro.test(data_2_independen$value2)$p.value,3)
      tolakan2 <- "ditolak"
      kesimpulan2 <- "data tidak berdistribusi normal"
      banding2 = "kurang dari"
      
      if(hasil_norm2 > 0.05){
        tolakan2 <- "tidak ditolak"
        kesimpulan2 <- "data berdistribusi normal"
        banding2 = "lebih dari"
      }
      
      
      var1 <- input$file_2indepen$name
      var2_1 <- input$column1_2indepen
      var2_2 <- input$column2_2indepen
      
      #interpretasi normalitas====
      
      output$norm_inter_2indepen <- renderUI({
        
        # Create an HTML <div> tag
        tags$div(
          class = "variable-display", # Apply the CSS class defined above, yang tadi
          paste("Diberikan data", var1,"yang berisi 2 populasi independen. 
                Populasi pertama terdapat pada kolom ", var2_1,"dan 
                Populasi kedua terdapat pada kolom", var2_2,
                ".Ingin diuji apakah kedua populasi berdistribusi normal atau tidak.
                Dengan hipotesis nol, yaitu data berdistribusi normal, dan
                hipotesis alternatif, yaitu data tidak berdistribusi normal.
                Ditentukan juga daerah kritik berupa P-value kurang dari alpha = 0.05.
                Untuk populasi pertama, ditemukan nilai P-value = ",hasil_norm1,
                ",maka hipotesis nol",tolakan1,".Dengan demikian, ",kesimpulan1,
                ".Untuk populasi kedua, ditemukan nilai P-value = ",hasil_norm2,
                ",maka hipotesis nol",tolakan2,".Dengan demikian, ",kesimpulan2,".")     
        )
      })
      
      #untuk uji hipotesis
      hasil_norm1 = round(hasil_norm1,3)
      hasil_norm2 = round(hasil_norm2,3)
      kesimpulan_norm_2indepen = paste("Untuk sampel pertama, karena nilai P-value =",hasil_norm1,
                                       banding1,"α = 0.05, maka H0",tolakan1,".Dengan demikian, ",kesimpulan1,".Untuk sampel 
      kedua, karena nilai P-value =",hasil_norm2,banding2,"α = 0.05, maka H0",tolakan2,".Dengan 
      demikian, ",kesimpulan2,".")
    }
    
    if(input$normal_2indepen == FALSE){
      output$hist_norm_2indepen <- renderPlotly({
        plot2 <- ggplot(long_2_independen, aes(x = value, fill = tipe)) +
          geom_histogram(aes(y = after_stat(density)), alpha = 0.5, position = "identity", bins = 30) + 
          theme_minimal() +
          labs(x = "Nilai", y = "Frekuensi")
        
        ggplotly(plot2)
      })
      
      output$norm_inter_2indepen <- renderUI({
        
        # Create an HTML <div> tag
        tags$div(
          class = "variable-display", # Apply the CSS class defined above, yang tadi
          paste("Data kedua populasi diasumsikan berdistribusi normal")     # Inject the R variable content
        )
      })
      
      #untuk uji hipotesis
      hasil_norm1 = "(diasumsikan lebih dari alpha)"
      hasil_norm2 = "(diasumsikan lebih dari alpha)"
      kesimpulan_norm_2indepen = paste("Diasumsikan kedua data berdistribusi normal")
      
    }
    
    #Uji Hipotesis norm 2indepen=====
    output$hipo1_norm_2indepen_1 <- renderUI({
      tags$div(
        class = "variable-display", 
        paste("H0: Data berdistribusi normal")    
      )
    })
    output$hipo1_norm_2indepen_2 <- renderUI({
      tags$div(
        class = "variable-display", 
        paste("H1: Data tidak berdistribusi normal")    
      )
    })
    output$hipo2_norm_2indepen <- renderUI({
      tags$div(
        class = "variable-display", 
        paste("α = 0,05")    
      )
    })
    output$hipo3_norm_2indepen_1 <- renderUI({
      tags$div(
        class = "variable-display", 
        paste("P-value Data 1 =",hasil_norm1)  
      )
    })
    output$hipo3_norm_2indepen_2 <- renderUI({
      tags$div(
        class = "variable-display", 
        paste("P-value Data 2 =",hasil_norm2)  
      )
    })
    output$hipo4_norm_2indepen <- renderUI({
      tags$div(
        class = "variable-display", 
        paste("H0 ditolak jika P-value < α")    
      )
    })
    output$hipo5_norm_2indepen <- renderUI({
      tags$div(
        class = "variable-display", 
        paste(kesimpulan_norm_2indepen)    
      )
    })
    
    #uji kesamaan variansi=====
    if(input$var_2indepen == TRUE){
      hasil_var <- round(var.test(data_2_independen$value1, data_2_independen$value2, ratio = 1, conf.level = 0.95)$p.value,3)
      tolakan_var <- "ditolak"
      kesimpulan_var <- "kedua populasi memiliki variansi yang berbeda"
      banding_var <- "kurang dari"
      var_state <- FALSE
      
      if(hasil_var > 0.05){
        tolakan_var <- "tidak ditolak"
        kesimpulan_var <- "kedua populasi memiliki variansi yang sama"
        banding_var <- "lebih dari"
        var_state <- TRUE
      }
      
      
      var1 <- input$file_2indepen$name
      var2_1 <- input$column1_2indepen
      var2_2 <- input$column2_2indepen
      
      output$var_inter_2indepen <- renderUI({
        
        tags$div(
          class = "variable-display",
          paste("Diberikan data", var1,"yang berisi 2 populasi independen. 
                Populasi pertama terdapat pada kolom ", var2_1,"dan 
                Populasi kedua terdapat pada kolom", var2_2,
                ".Ingin diuji apakah kedua populasi memiliki variansi yang sama atau tidak.
                Dengan hipotesis nol, yaitu kedua populasi memiliki variansi yang sama, dan
                hipotesis alternatif, yaitu kedua populasi memiliki variansi yang berbeda
                Ditentukan juga daerah kritik berupa P-value kurang dari alpha = 0.05.
                Ditemukan nilai P-value = ",hasil_var,
                ",maka hipotesis nol",tolakan_var,".Dengan demikian, ",kesimpulan_var)     
        )
      })
      
      #untuk uji hipotesis
      hasil_var = hasil_var
      kesimpulan_var_2indepen = paste("Karena nilai P-value =",hasil_var,
                                      banding_var,"α = 0.05, maka H0",tolakan_var,
                                      ".Dengan demikian, ",kesimpulan_var,".")
    }
    
    if(input$var_2indepen == FALSE){
      var_state <- TRUE
      
      output$var_inter_2indepen <- renderUI({
        
        tags$div(
          class = "variable-display", 
          paste("Data kedua populasi diasumsikan memiliki variansi yang sama")     
        )
      })
      
      #untuk uji hipotesis
      hasil_var = "(diasumsikan lebih dari alpha)"
      kesimpulan_var_2indepen = paste("Diasumsikan kedua data memiliki variansi yang sama")
    }
    
    output$hipo1_var_2indepen_1 <- renderUI({
      tags$div(
        class = "variable-display", 
        paste("H0: Kedua data memiliki variansi yang sama")    
      )
    })
    output$hipo1_var_2indepen_2 <- renderUI({
      tags$div(
        class = "variable-display", 
        paste("H1: Kedua data memiliki variansi yang tidak sama")    
      )
    })
    output$hipo2_var_2indepen <- renderUI({
      tags$div(
        class = "variable-display", 
        paste("α = 0,05")    
      )
    })
    output$hipo3_var_2indepen <- renderUI({
      tags$div(
        class = "variable-display", 
        paste("P-value =",hasil_var)  
      )
    })
    output$hipo4_var_2indepen <- renderUI({
      tags$div(
        class = "variable-display", 
        paste("H0 ditolak jika P-value < α")    
      )
    })
    output$hipo5_var_2indepen <- renderUI({
      tags$div(
        class = "variable-display", 
        paste(kesimpulan_var_2indepen)    
      )
    })
    
    
    #uji hipotesis dua independen ======
    var1 <- input$file_2indepen$name
    var2_1 <- input$column1_2indepen
    var2_2 <- input$column2_2indepen
    
    output$hist_uji_2indepen <- renderPlotly({
      plot2 <- ggplot(long_2_independen, aes(x = value, fill = tipe)) +
        geom_histogram(aes(y = after_stat(density)), alpha = 0.5, position = "identity", bins = 30) + 
        geom_vline(xintercept = mean1_2indepen, color = "red",linewidth = 1.2, linetype = "solid") +
        geom_vline(xintercept = mean2_2indepen, color = "blue",linewidth = 1.2, linetype = "solid") +
        theme_minimal() +
        labs(x = "Nilai", y = "Frekuensi")
      
      ggplotly(plot2)
    })
    
    
    if(input$testing_2indepen == "test1_2indepen"){
      
      test_t<-round(t.test(data_2_independen$value1, data_2_independen$value2,
                           alternative="two.sided", mu=input$mean_2indepen, var.equal=var_state)$p.value,3)
      
      tolakan = "tidak ditolak"
      kesimpulan = "rata-rata kedua populasi sama"
      banding_2indepen = "lebih dari"
      
      if(test_t < 0.05){
        tolakan = "ditolak"
        kesimpulan = "rata-rata kedua populasi berbeda"
        banding_2indepen = "kurang dari"
      }
      output$uji_inter_2indepen <- renderUI({
        
        tags$div(
          class = "variable-display", 
          paste("Diberikan data", var1,"yang berisi 2 populasi independen. 
                Populasi pertama terdapat pada kolom ", var2_1,"dan 
                Populasi kedua terdapat pada kolom", var2_2,
                ".Ingin diuji apakah kedua populasi memiliki rata-rata yang sama atau tidak.
                Dengan hipotesis nol, yaitu rata-rata kedua populasi sama, dan
                hipotesis alternatif, yaitu rata-rata kedua populasi berbeda.
                Ditentukan juga daerah kritik berupa P-value kurang dari alpha = 0.05.
                Untuk populasi pertama, ditemukan nilai P-value = ",test_t,
                ",maka hipotesis nol",tolakan,".Dengan demikian, ",kesimpulan)     
        )
      })
      
      #untuk uji hipotesis
      hipotesis0_2indepen = "H0: μ1 = μ2"
      hipotesis1_2indepen = "H1: μ1 ≠ μ2"
      hasil_uji = test_t
      kesimpulan_uji_2indepen = paste("Karena nilai P-value =",hasil_uji,
                                      banding_2indepen,"α = 0.05, maka H0",tolakan,
                                      ".Dengan demikian, ",kesimpulan,".")
      
    }
    
    if(input$testing_2indepen == "test2_2indepen"){
      
      test_t<-round(t.test(data_2_independen$value1, data_2_independen$value2,
                           alternative="greater", mu=input$mean_2indepen, var.equal=var_state)$p.value,3)
      
      tolakan = "tidak ditolak"
      kesimpulan = "rata-rata populasi pertama lebih kecil dari rata-rata populasi kedua"
      banding_2indepen = "lebih dari"
      
      if(test_t < 0.05){
        tolakan = "ditolak"
        kesimpulan = "rata-rata populasi pertama lebih besar dari rata-rata populasi kedua"
        banding_2indepen = "kurang dari"
      }
      output$uji_inter_2indepen <- renderUI({
        
        tags$div(
          class = "variable-display", 
          paste("Diberikan data", var1,"yang berisi 2 populasi independen. 
                Populasi pertama terdapat pada kolom ", var2_1,"dan 
                Populasi kedua terdapat pada kolom", var2_2,
                ".Ingin diuji apakah populasi pertama memiliki rata-rata yang lebih besar
                dari populasi kedua atau tidak.
                Dengan hipotesis nol, yaitu rata-rata populasi pertama lebih kecil dari 
                rata-rata populasi kedua, dan
                hipotesis alternatif, yaitu rata-rata populasi pertama lebih besar dari 
                rata-rata populasi kedua.
                Ditentukan juga daerah kritik berupa P-value kurang dari alpha = 0.05.
                Untuk populasi pertama, ditemukan nilai P-value = ",test_t,
                ",maka hipotesis nol",tolakan,".Dengan demikian, ",kesimpulan)     
        )
      })
      
      #untuk uji hipotesis
      hipotesis0_2indepen = "H0: μ1 ≤ μ2"
      hipotesis1_2indepen = "H1: μ1 > μ2."
      hasil_uji = test_t
      kesimpulan_uji_2indepen = paste("Karena nilai P-value =",hasil_uji,
                                      banding_2indepen,"α = 0.05, maka H0",tolakan,
                                      ".Dengan demikian, ",kesimpulan,".")
    }
    
    if(input$testing_2indepen == "test3_2indepen"){
      test_t<-round(t.test(data_2_independen$value1, data_2_independen$value2,
                           alternative="less", mu=input$mean_2indepen, var.equal=var_state)$p.value,3)
      
      tolakan = "tidak ditolak"
      kesimpulan = "rata-rata populasi pertama lebih besar dari rata-rata populasi kedua"
      banding_2indepen = "lebih dari"
      
      if(test_t < 0.05){
        tolakan = "ditolak"
        kesimpulan = "rata-rata populasi pertama lebih kecil dari rata-rata populasi kedua"
        banding_2indepen = "kurang dari"
      }
      output$uji_inter_2indepen <- renderUI({
        
        tags$div(
          class = "variable-display", 
          paste("Diberikan data", var1,"yang berisi 2 populasi independen. 
                Populasi pertama terdapat pada kolom ", var2_1,"dan 
                Populasi kedua terdapat pada kolom", var2_2,
                ".Ingin diuji apakah populasi pertama memiliki rata-rata yang lebih besar
                dari populasi kedua atau tidak.
                Dengan hipotesis nol, yaitu rata-rata populasi pertama lebih besar dari 
                rata-rata populasi kedua, dan
                hipotesis alternatif, yaitu rata-rata populasi pertama lebih kecil dari 
                rata-rata populasi kedua.
                Ditentukan juga daerah kritik berupa P-value kurang dari alpha = 0.05.
                Untuk populasi pertama, ditemukan nilai P-value = ",test_t,
                ",maka hipotesis nol",tolakan,".Dengan demikian, ",kesimpulan)     
        )
      })
      
      #untuk uji hipotesis
      hipotesis0_2indepen = "H0: μ1 ≥ μ2"
      hipotesis1_2indepen = "H1: μ1 < μ2."
      hasil_uji = test_t
      kesimpulan_uji_2indepen = paste("Karena nilai P-value =",hasil_uji,
                                      banding_2indepen,"α = 0.05, maka H0",tolakan,
                                      ".Dengan demikian, ",kesimpulan,".")
    }
    
    output$hipo1_uji_2indepen_1 <- renderUI({
      tags$div(
        class = "variable-display", 
        paste(hipotesis0_2indepen)    
      )
    })
    output$hipo1_uji_2indepen_2 <- renderUI({
      tags$div(
        class = "variable-display", 
        paste(hipotesis1_2indepen)    
      )
    })
    output$hipo2_uji_2indepen <- renderUI({
      tags$div(
        class = "variable-display", 
        paste("α = 0,05")    
      )
    })
    output$hipo3_uji_2indepen <- renderUI({
      tags$div(
        class = "variable-display", 
        paste("P-value =",hasil_uji) 
      )
    })
    output$hipo4_uji_2indepen <- renderUI({
      tags$div(
        class = "variable-display", 
        paste("H0 ditolak jika P-value < α")    
      )
    })
    output$hipo5_uji_2indepen <- renderUI({
      tags$div(
        class = "variable-display", 
        paste(kesimpulan_uji_2indepen)    
      )
    })
    
    
    bagian_server_2indepen_akhir <- function(tagged){}
    
  })
  
  # -------------------------
  # Uji 2 Populasi Dependen
  # -------------------------
  
  # ======================================================
  # 1. Baca file (CSV/TXT/XLSX)
  # ======================================================
  data_file_2dependen <- reactive({
    req(input$file_2dependen)
    
    file_path <- input$file_2dependen$datapath
    file_name <- input$file_2dependen$name
    ext <- tools::file_ext(file_name)
    
    # CSV / TXT
    if (ext %in% c("csv", "txt")) {
      df <- if (ext == "csv") {
        read.csv(file_path, header = input$header_2dependen)
      } else {
        read.delim(file_path, header = input$header_2dependen)
      }
      
      return(list(
        sheets = "Data",
        data = list("Data" = df)
      ))
    }
    
    # XLSX
    if (ext == "xlsx") {
      sheets <- readxl::excel_sheets(file_path)
      
      data_list <- lapply(sheets, function(s) {
        readxl::read_excel(
          file_path,
          sheet = s,
          col_names = input$header_2dependen
        )
      })
      
      names(data_list) <- sheets
      
      return(list(
        sheets = sheets,
        data = data_list
      ))
    }
    
    stop("Tipe file tidak didukung.")
  })
  
  # ======================================================
  # 2. Update dropdown SHEET (hanya 1)
  # ======================================================
  observeEvent(data_file_2dependen(), {
    updateSelectInput(
      session,
      "sheet_2dependen",
      choices = data_file_2dependen()$sheets
    )
  })
  
  # ======================================================
  # 3. Reactive data berdasarkan sheet yang sama
  # ======================================================
  active_sheet_data <- reactive({
    req(input$sheet_2dependen)
    data_file_2dependen()$data[[input$sheet_2dependen]]
  })
  
  # ======================================================
  # 4. Update dropdown kolom Data 1 & Data 2
  # ======================================================
  observeEvent(active_sheet_data(), {
    cols <- names(active_sheet_data())
    
    updateSelectInput(
      session,
      "column1_2dependen",
      choices = cols
    )
    
    updateSelectInput(
      session,
      "column2_2dependen",
      choices = cols
    )
  })
  
  # ======================================================
  # 5. Tabel Overview 1
  # ======================================================
  output$table1_2dependen <- renderTable({
    req(input$column1_2dependen)
    df <- active_sheet_data()
    data.frame(df[[input$column1_2dependen]]) |> head(10)
  })
  
  # ======================================================
  # 6. Tabel Overview 2
  # ======================================================
  output$table2_2dependen <- renderTable({
    req(input$column2_2dependen)
    df <- active_sheet_data()
    data.frame(df[[input$column2_2dependen]]) |> head(10)
  })
  
  # ======================================================
  # 7. UJI HIPOTESIS 2 POPULASI DEPENDEN
  # ======================================================
  observeEvent(input$test_2dependen, {
    
    # ------------------------------
    # Ambil data
    # ------------------------------
    df <- active_sheet_data()
    data_2_dependen <- data.frame(
      value1 = df[[input$column1_2dependen]],
      value2 = df[[input$column2_2dependen]]
    )
    
    # Nama file & kolom untuk interpretasi
    var1 <- input$file_2dependen$name
    var2_1 <- input$column1_2dependen
    var2_2 <- input$column2_2dependen
    
    # =============================
    # Selisih untuk uji normalitas & paired t-test
    # =============================
    d <- na.omit(data_2_dependen$value1 - data_2_dependen$value2)
    n_d <- length(d)
    
    # =====================================================
    # UJI NORMALITAS OTOMATIS
    # =====================================================
    if (input$normal_2dependen == TRUE) {
      
      # Pilih uji normalitas
      if (n_d < 50) {
        hasil_norm <- shapiro.test(d)
        metode_norm <- "Shapiro-Wilk"
      } else {
        hasil_norm <- ks.test(d, "pnorm", mean = mean(d), sd = sd(d))
        metode_norm <- "Kolmogorov-Smirnov"
      }
      
      p_value_norm <- round(hasil_norm$p.value, 3)
      keputusan_norm <- ifelse(p_value_norm > 0.05, "tidak ditolak", "ditolak")
      kesimpulan_norm <- ifelse(p_value_norm > 0.05,
                                "data berdistribusi normal",
                                "data tidak berdistribusi normal")
      
      # Plot histogram selisih
      output$hist_norm_2dependen <- renderPlotly({
        plot2 <- ggplot(data.frame(d = d), aes(x = d)) +
          geom_histogram(aes(y = after_stat(density)), alpha = 0.5, fill = "steelblue", bins = 30) +
          geom_density(alpha = 0.4, color = "red") +
          theme_minimal() +
          labs(x = "Selisih d = value1 - value2", y = "Densitas")
        ggplotly(plot2)
      })
      
      # Interpretasi uji normalitas
      output$norm_inter_2dependen <- renderUI({
        tags$div(
          class = "variable-display",
          paste(
            "Diberikan data", var1, "yang berisi 2 populasi dependen.",
            "Uji normalitas dilakukan pada selisih d = value1 - value2.",
            "Dengan metode", metode_norm, ", didapat P-value =", p_value_norm, ",",
            "maka hipotesis nol", keputusan_norm, ".",
            "Kesimpulan:", kesimpulan_norm, "."
          )
        )
      })
    }
    
    # =====================================================
    # UJI HIPOTESIS PAIRED T-TEST
    # =====================================================
    
    # ------------------------------
    # Two-sided
    # ------------------------------
    if (input$testing_2dependen == "test1_2dependen") {
      
      test_t <- round(t.test(data_2_dependen$value1, data_2_dependen$value2,
                             alternative = "two.sided",
                             mu = input$mean_2dependen,
                             paired = TRUE)$p.value, 3)
      
      tolakan <- ifelse(test_t < 0.05, "ditolak", "tidak ditolak")
      kesimpulan <- ifelse(test_t < 0.05,
                           "rata-rata selisih antara populasi pertama dan kedua berbeda dari mu0",
                           "tidak cukup bukti bahwa rata-rata selisih berbeda dari mu0")
      
      output$uji_inter_2dependen <- renderUI({
        tags$div(
          class = "variable-display",
          paste(
            "Diberikan data", var1,"yang berisi 2 populasi dependen.",
            "Populasi pertama terdapat pada kolom", var2_1,"dan",
            "Populasi kedua terdapat pada kolom", var2_2, ".",
            "Ingin diuji apakah rata-rata selisih antara populasi pertama dan kedua sama dengan mu0 atau tidak.",
            "Dengan hipotesis nol, yaitu rata-rata selisih = mu0, dan",
            "hipotesis alternatif, yaitu rata-rata selisih ≠ mu0.",
            "Ditentukan juga daerah kritik berupa P-value kurang dari alpha = 0.05.",
            "Untuk selisih antara populasi pertama dan kedua, ditemukan nilai P-value =", test_t, ",",
            "maka hipotesis nol", tolakan, ".",
            "Dengan demikian,", kesimpulan
          )
        )
      })
    }
    
    # ------------------------------
    # Greater
    # ------------------------------
    if (input$testing_2dependen == "test2_2dependen") {
      
      test_t <- round(t.test(data_2_dependen$value1, data_2_dependen$value2,
                             alternative = "greater",
                             mu = input$mean_2dependen,
                             paired = TRUE)$p.value, 3)
      
      tolakan <- ifelse(test_t < 0.05, "ditolak", "tidak ditolak")
      kesimpulan <- ifelse(test_t < 0.05,
                           "rata-rata selisih (value1 - value2) lebih besar dari mu0",
                           "tidak cukup bukti bahwa rata-rata selisih lebih besar dari mu0")
      
      output$uji_inter_2dependen <- renderUI({
        tags$div(
          class = "variable-display",
          paste(
            "Diberikan data", var1,"yang berisi 2 populasi dependen.",
            "Populasi pertama terdapat pada kolom", var2_1,"dan",
            "Populasi kedua terdapat pada kolom", var2_2, ".",
            "Ingin diuji apakah rata-rata selisih (value1 - value2) lebih besar dari mu0 atau tidak.",
            "Dengan hipotesis nol, yaitu rata-rata selisih ≤ mu0, dan",
            "hipotesis alternatif, yaitu rata-rata selisih > mu0.",
            "Ditentukan juga daerah kritik berupa P-value kurang dari alpha = 0.05.",
            "Untuk selisih antara populasi pertama dan kedua, ditemukan nilai P-value =", test_t, ",",
            "maka hipotesis nol", tolakan, ".",
            "Dengan demikian,", kesimpulan
          )
        )
      })
    }
    
    # ------------------------------
    # Less
    # ------------------------------
    if (input$testing_2dependen == "test3_2dependen") {
      
      test_t <- round(t.test(data_2_dependen$value1, data_2_dependen$value2,
                             alternative = "less",
                             mu = input$mean_2dependen,
                             paired = TRUE)$p.value, 3)
      
      tolakan <- ifelse(test_t < 0.05, "ditolak", "tidak ditolak")
      kesimpulan <- ifelse(test_t < 0.05,
                           "rata-rata selisih (value1 - value2) lebih kecil dari mu0",
                           "tidak cukup bukti bahwa rata-rata selisih lebih kecil dari mu0")
      
      output$uji_inter_2dependen <- renderUI({
        tags$div(
          class = "variable-display",
          paste(
            "Diberikan data", var1,"yang berisi 2 populasi dependen.",
            "Populasi pertama terdapat pada kolom", var2_1,"dan",
            "Populasi kedua terdapat pada kolom", var2_2, ".",
            "Ingin diuji apakah rata-rata selisih (value1 - value2) lebih kecil dari mu0 atau tidak.",
            "Dengan hipotesis nol, yaitu rata-rata selisih ≥ mu0, dan",
            "hipotesis alternatif, yaitu rata-rata selisih < mu0.",
            "Ditentukan juga daerah kritik berupa P-value kurang dari alpha = 0.05.",
            "Untuk selisih antara populasi pertama dan kedua, ditemukan nilai P-value =", test_t, ",",
            "maka hipotesis nol", tolakan, ".",
            "Dengan demikian,", kesimpulan
          )
        )
      })
    }
    
    # =====================================================
    # Histogram uji paired
    # =====================================================
    output$hist_uji_2dependen <- renderPlotly({
      plot2 <- ggplot(data.frame(d = d), aes(x = d)) +
        geom_histogram(aes(y = after_stat(density)), alpha = 0.5, fill = "steelblue", bins = 30) +
        geom_vline(xintercept = mean(d), color = "red", linewidth = 1.2) +
        theme_minimal() +
        labs(x = "Selisih d = value1 - value2", y = "Densitas")
      ggplotly(plot2)
    })
    
  })
  
  # -------------------------
  # Uji ANOVA (Visualisasi & Uji Asumsi)
  # -------------------------
  
  # Data reaktif untuk ANOVA
  data_anova <- reactive({
    req(input$file_anova)
    
    file_path <- input$file_anova$datapath
    file_name <- input$file_anova$name
    ext <- tools::file_ext(file_name)
    
    data <- switch(
      ext,
      "csv" = read.csv(file_path, header = input$header_anova, stringsAsFactors = FALSE),
      "txt" = read.delim(file_path, header = input$header_anova, stringsAsFactors = FALSE),
      "xlsx" = readxl::read_excel(file_path, col_names = input$header_anova),
      stop(paste("Tipe file tidak didukung:", ext))
    )
    
    return(data)
  })
  
  # Update pilihan variabel respon & faktor ketika data ANOVA di-upload
  observeEvent(data_anova(), {
    df <- data_anova()
    cols <- names(df)
    
    # Respon: idealnya numerik
    numeric_cols <- cols[sapply(df, is.numeric)]
    if (length(numeric_cols) == 0) numeric_cols <- cols
    
    updateSelectInput(
      session,
      "response_anova",
      choices = numeric_cols,
      selected = numeric_cols[1]
    )
    
    # Faktor: bisa factor/character, tapi kalau nggak ada ya semua kolom
    factor_cols <- cols[sapply(df, function(x) is.factor(x) || is.character(x))]
    if (length(factor_cols) == 0) factor_cols <- cols
    
    updateSelectInput(
      session,
      "factor_anova",
      choices = factor_cols,
      selected = factor_cols[1]
    )
  })
  
  # Tempat menyimpan model ANOVA terakhir
  anova_model <- reactiveVal(NULL)
  
  # Jalankan ANOVA + semua plot & uji asumsi saat tombol diklik
  observeEvent(input$run_anova, {
    req(data_anova(), input$response_anova, input$factor_anova)
    
    df_raw <- data_anova()
    
    # Ambil hanya kolom yang dipakai
    df_model <- df_raw[, c(input$response_anova, input$factor_anova)]
    names(df_model) <- c("y", "group")  # biar gampang di-ggplot
    
    # Konversi tipe data
    df_model$y <- as.numeric(df_model$y)
    df_model$group <- as.factor(df_model$group)
    
    # Hapus NA
    df_model <- na.omit(df_model)
    
    # Fit model ANOVA satu arah
    model <- aov(y ~ group, data = df_model)
    anova_model(model)  # simpan
    
    # --------------------------
    # 1. Tabel ANOVA
    # --------------------------
    output$anova_table <- renderPrint({
      summary(model)
    })
    
    # --------------------------
    # 2. Boxplot respon per kelompok
    # --------------------------
    output$anova_boxplot <- renderPlot({
      ggplot(df_model, aes(x = group, y = y)) +
        geom_boxplot(fill = "#3c8dbc", alpha = 0.7, outlier.colour = "red") +
        geom_jitter(width = 0.1, alpha = 0.6) +
        theme_minimal() +
        labs(
          x = input$factor_anova,
          y = input$response_anova,
          title = "Boxplot Respon per Kelompok Faktor"
        )
    })
    
    # --------------------------
    # 3. Residual & fitted
    # --------------------------
    res <- residuals(model)
    fit <- fitted(model)
    df_res <- data.frame(
      residual = res,
      fitted = fit
    )
    
    # Histogram residual
    output$anova_resid_hist <- renderPlot({
      ggplot(df_res, aes(x = residual)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30,
                       fill = "#3c8dbc", color = "white") +
        geom_density(linewidth = 1) +
        theme_minimal() +
        labs(
          x = "Residual",
          y = "Densitas",
          title = "Histogram Residual ANOVA"
        )
    })
    
    # QQ-plot residual
    output$anova_resid_qq <- renderPlot({
      ggplot(df_res, aes(sample = residual)) +
        stat_qq() +
        stat_qq_line() +
        theme_minimal() +
        labs(
          title = "QQ-Plot Residual ANOVA",
          x = "Kuantil Teoretis",
          y = "Kuantil Sampel"
        )
    })
    
    # Residual vs Fitted
    output$anova_resid_fitted <- renderPlot({
      ggplot(df_res, aes(x = fitted, y = residual)) +
        geom_point(alpha = 0.7) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        theme_minimal() +
        labs(
          x = "Fitted Value",
          y = "Residual",
          title = "Plot Residual vs Fitted"
        )
    })
    
    # --------------------------
    # 4. Uji Asumsi: Shapiro-Wilk & Bartlett
    # --------------------------
    hasil_shapiro <- NULL
    hasil_bartlett <- NULL
    alpha <- 0.05
    
    if (input$normal_anova) {
      # Shapiro-Wilk untuk residual
      hasil_shapiro <- shapiro.test(res)
    }
    
    if (input$homo_anova) {
      # Bartlett untuk homogenitas varians
      hasil_bartlett <- bartlett.test(y ~ group, data = df_model)
    }
    
    # Tampilkan objek uji mentah (angka + statistik) di verbatimTextOutput
    output$anova_assump_test <- renderPrint({
      out <- list()
      if (!is.null(hasil_shapiro)) {
        out$`Uji Normalitas (Shapiro-Wilk)` <- hasil_shapiro
      }
      if (!is.null(hasil_bartlett)) {
        out$`Uji Homogenitas Varians (Bartlett)` <- hasil_bartlett
      }
      out
    })
    
    # Interpretasi uji asumsi dalam kalimat
    output$anova_assump_interpret <- renderUI({
      teks <- c()
      
      if (!is.null(hasil_shapiro)) {
        p_norm <- hasil_shapiro$p.value
        p_norm_round <- round(p_norm, 3)
        keputusan_norm <- ifelse(p_norm > alpha, "tidak ditolak", "ditolak")
        kesimpulan_norm <- ifelse(
          p_norm > alpha,
          "residual dapat dianggap berdistribusi normal.",
          "residual tidak berdistribusi normal."
        )
        
        teks <- c(
          teks,
          paste0(
            "Uji normalitas Shapiro-Wilk memberikan p-value = ",
            p_norm_round,
            ". Pada taraf signifikansi α = 0,05, H0 (residual berdistribusi normal) ",
            keputusan_norm,
            ". Dengan demikian, ",
            kesimpulan_norm
          )
        )
      } else {
        teks <- c(teks, "Uji normalitas residual tidak dijalankan (diasumsikan normal).")
      }
      
      if (!is.null(hasil_bartlett)) {
        p_bart <- hasil_bartlett$p.value
        p_bart_round <- round(p_bart, 3)
        keputusan_bart <- ifelse(p_bart > alpha, "tidak ditolak", "ditolak")
        kesimpulan_bart <- ifelse(
          p_bart > alpha,
          "variansi antar kelompok dapat dianggap homogen.",
          "variansi antar kelompok tidak homogen."
        )
        
        teks <- c(
          teks,
          paste0(
            "Uji homogenitas varians Bartlett memberikan p-value = ",
            p_bart_round,
            ". Pada taraf signifikansi α = 0,05, H0 (variansi antar kelompok sama) ",
            keputusan_bart,
            ". Dengan demikian, ",
            kesimpulan_bart
          )
        )
      } else {
        teks <- c(teks, "Uji homogenitas varians tidak dijalankan (diasumsikan homogen).")
      }
      
      tags$div(
        class = "variable-display",
        HTML(paste(teks, collapse = "<br><br>"))
      )
    })
    
    # --------------------------
    # 5. Interpretasi model ANOVA (p-value F-test utama)
    # --------------------------
    output$anova_interpret <- renderUI({
      anova_tab <- summary(model)[[1]]
      p_val <- anova_tab[["Pr(>F)"]][1]
      p_round <- round(p_val, 3)
      
      keputusan <- ifelse(p_val < alpha, "ditolak", "tidak ditolak")
      kesimpulan <- ifelse(
        p_val < alpha,
        "terdapat perbedaan rata-rata respon yang signifikan antar kelompok faktor.",
        "tidak terdapat bukti yang cukup untuk menyatakan adanya perbedaan rata-rata respon antar kelompok faktor."
      )
      
      tags$div(
        class = "variable-display",
        paste0(
          "Dari hasil ANOVA satu arah diperoleh p-value = ",
          p_round,
          ". Pada taraf signifikansi α = 0,05, H0 (rata-rata semua kelompok sama) ",
          keputusan,
          ". Dengan demikian, ",
          kesimpulan
        )
      )
    })
    
  })  # akhir observeEvent(input$run_anova)
  
  
} #jangan dihapus, kurung kurawa fungsi server


# ============================================================================
# RUN APLIKASI
# ============================================================================

shinyApp(ui, server)