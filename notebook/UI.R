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
# USER INTERFACE
# ============================================================================

ui <- dashboardPage(
  title = "Dashboard Random Number Generator",
  skin = "green",
  
  # Header
  dashboardHeader(title = NULL, titleWidth = 0,
                  tags$li(
                    a(
                      href = "#", 
                      class = "logo-title",
                      # 1st Word: White (using tags$span)
                      tags$span(
                        style = "font-weight: bold; font-size: 20px; color: white;", 
                        "Dashboard Inferensi Statistika" 
                      ),
                      
                      # 2nd Word: Red (using tags$span)
                      tags$span(
                        style = "font-weight: bold; font-size: 20px; color: #00a65a;", 
                        "__________________________________" 
                      )
                    ),
                    class = "dropdown"
                  )),
  
  #title width = seberapa lebar header yang ditampilkan
  #cek icon: https://fontawesome.com/search?c=business&ic=free-collection
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Uji Rata-Rata Satu Populasi", tabName = "1popul", icon = icon("book")),
      menuItem("Uji Rata-Rata Dua Populasi Independen", tabName = "2indepen", icon = icon("book")), 
      menuItem("Uji Rata-Rata Dua Populasi Dependen", tabName = "2dependen", icon = icon("book")),
      menuItem("Uji ANOVA", tabName = "anova", icon = icon("chart-bar"),
               menuSubItem("Teori", tabName = "teori_anova", icon = icon("bezier-curve")),
               menuSubItem("Uji", tabName = "uji_anova", icon = icon("bezier-curve"))
      )
    )
  ),
  
  # Body
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .main-header .logo {text-align: left;}
        .content-wrapper { background-color: #ecf0f5; }
        .box { border-radius: 5px; }
        .info-box { min-height: 90px; border-radius: 5px; }
        .small-box { border-radius: 5px; }
        h2 { color: #3c8dbc; font-weight: bold; }
        .box-header h3 {
  color: #ffffff !important;
        }
        .theory-section { 
          background: white; 
          padding: 20px; 
          border-radius: 5px; 
          margin-bottom: 20px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12);
        }
        .variable-display {
          font-family: 'Arial', sans-serif;
          font-size: 16px;
          color: #ff0000;      /* Set the color to RED */
          font-weight: bold;   /* Set the format/weight */
          margin-top: 10px;
          padding: 5px;
          border: 1px solid #ff0000;
          background-color: #ffe0e0;
        }
      "))
    ),
    
    tabItems(
      # ========================================================================
      # TAB 1: Uji Populasi Satu Angkatan
      # ========================================================================
      tabItem(tabName = "1popul",
              fluidRow(
                box(
                  width = 4,
                  title = "Pengaturan Parameter",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  fileInput(
                    inputId = "file_1popul",        # Unique ID for the server
                    label = "Masukkan Data di Sini:", # Label for the user
                    placeholder = "Tidak ada file terpilih", # Text displayed when no file is chosen
                    multiple = FALSE,             # Allows only one file upload
                    accept = c(".csv", ".txt",".xlsx")    # Restrict file types (e.g., to CSV and text)
                  ),
                  
                  selectInput(
                    inputId = "column_1popul", # ID for the server side
                    label = "Pilih Kolom:",     # Label for the user
                    choices = NULL              # Start with NULL; choices will be populated by the server
                  ),
                  
                  checkboxInput(
                    inputId = "header_1popul", # Unique ID for the server side
                    label = "Header",  # Label next to the checkbox (e.g., Calculate Automatically)
                    value = TRUE                # Initial state (TRUE means checked by default)
                  ),
                  
                  h4("Uji Asumsi"),
                  checkboxInput(
                    inputId = "normal_1popul", # Unique ID for the server side
                    label = "Asumsi Normalitas",  # Label next to the checkbox (e.g., Calculate Automatically)
                    value = TRUE                # Initial state (TRUE means checked by default)
                  ),
                  
                  numericInput("mean_1popul", "Mean Hipotesis:", value = 0),
                  
                  selectInput(
                    inputId = "testing_1popul", # ID for the server side
                    label = "Pilih Uji Hipotesis:",     # Label for the user
                    choices = c(
                      "H0: μ = μ0" = "test1_1popul",
                      "H0: μ ≤ μ0" = "test2_1popul",
                      "H0: μ ≥ μ0" = "test3_1popul"
                    ),
                    selected = "test1_1popul"
                  ),
                  
                  actionButton("test_1popul", "Test", 
                               class = "btn-primary btn-block",  style = "color: white;"),
                  hr()
                ),
                box(
                  width = 8,
                  title = "Tabel Overview",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  tableOutput("table_1popul")
                ),
                box(
                  width = 8,
                  title = "Uji Hipotesis dan Asumsi",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  h2("Uji Normalitas"),
                  
                  plotlyOutput("hist_norm_1popul"),
                  
                  h3("Uji Hipotesis:"),
                  h4("1. Hipotesis"),
                  uiOutput("hipo1_norm_1popul_1"),
                  uiOutput("hipo1_norm_1popul_2"),
                  h4("2. Tingkat Signifikansi"),
                  uiOutput("hipo2_norm_1popul"),
                  h4("3. Statistik Uji"),
                  uiOutput("hipo3_norm_1popul"),
                  h4("4. Daerah Kritik"),
                  uiOutput("hipo4_norm_1popul"),
                  h4("5. Kesimpulan"),
                  uiOutput("hipo5_norm_1popul"),
                  
                  
                  h3("Interpretasi:"),
                  uiOutput("norm_inter_1popul"),
                  
                  h2("Uji Hipotesis 1 Populasi"),
                  
                  plotlyOutput("hist_uji_1popul", height = "400px"),
                  
                  h3("Uji Hipotesis:"),
                  h4("1. Hipotesis"),
                  uiOutput("hipo1_uji_1popul_1"),
                  uiOutput("hipo1_uji_1popul_2"),
                  h4("2. Tingkat Signifikansi"),
                  uiOutput("hipo2_uji_1popul"),
                  h4("3. Statistik Uji"),
                  uiOutput("hipo3_uji_1popul"),
                  h4("4. Daerah Kritik"),
                  uiOutput("hipo4_uji_1popul"),
                  h4("5. Kesimpulan"),
                  uiOutput("hipo5_uji_1popul"),
                  
                  h3("Interpretasi:"),
                  uiOutput("uji_inter_1popul")
                )
              )
      ),
      
      # ========================================================================
      # TAB 2: Uji Populasi Dua Angkatan Independen
      # ========================================================================
      tabItem(tabName = "2indepen",
              fluidRow(
                box(
                  width = 4,
                  title = "Pengaturan Parameter",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  #file 
                  
                  fileInput(
                    inputId = "file_2indepen",        # Unique ID for the server
                    label = "Masukkan Data di Sini:", # Label for the user
                    placeholder = "Tidak ada file terpilih", # Text displayed when no file is chosen
                    multiple = FALSE,             # Allows only one file upload
                    accept = c(".csv", ".txt",".xlsx")    # Restrict file types (e.g., to CSV and text)
                  ),
                  
                  checkboxInput(
                    inputId = "header_2indepen", # Unique ID for the server side
                    label = "Header",  # Label next to the checkbox (e.g., Calculate Automatically)
                    value = TRUE                # Initial state (TRUE means checked by default)
                  ),
                  
                  #kolom 1
                  h4("Data 1"),
                  selectInput(
                    inputId = "column1_2indepen", # ID for the server side
                    label = "Pilih Kolom:",     # Label for the user
                    choices = NULL              # Start with NULL; choices will be populated by the server
                  ),
                  
                  
                  #kolom 2
                  h4("Data 2"),
                  selectInput(
                    inputId = "column2_2indepen", # ID for the server side
                    label = "Pilih Kolom:",     # Label for the user
                    choices = NULL              # Start with NULL; choices will be populated by the server
                  ),
                  
                  
                  #bagian uji asumsi
                  
                  h4("Uji Asumsi"),
                  checkboxInput(
                    inputId = "normal_2indepen", # Unique ID for the server side
                    label = "Asumsi Normalitas",  # Label next to the checkbox (e.g., Calculate Automatically)
                    value = TRUE                # Initial state (TRUE means checked by default)
                  ),
                  checkboxInput(
                    inputId = "var_2indepen", # Unique ID for the server side
                    label = "Asumsi Kesamaan Variansi",  # Label next to the checkbox (e.g., Calculate Automatically)
                    value = TRUE                # Initial state (TRUE means checked by default)
                  ),
                  
                  numericInput("mean_2indepen", "Hipotesis perbedaan mean:", value = 0),
                  
                  selectInput(
                    inputId = "testing_2indepen", # ID for the server side
                    label = "Pilih Uji Hipotesis:",     # Label for the user
                    choices = c(
                      "H0: μ1 = μ2" = "test1_2indepen",
                      "H0: μ1 ≤ μ2" = "test2_2indepen",
                      "H0: μ1 ≥ μ2" = "test3_2indepen"
                    ),
                    selected = "test1_2indepen"
                  ),
                  
                  actionButton("test_2indepen", "Test", 
                               class = "btn-primary btn-block",  style = "color: white;"),
                  hr()
                ),
                
                #tabel 1
                
                box(
                  width = 4,
                  title = "Tabel Overview 1",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  tableOutput("table1_2indepen")
                ),
                
                #tabel 2
                
                box(
                  width = 4,
                  title = "Tabel Overview 2",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  tableOutput("table2_2indepen")
                ),
                
                box(
                  width = 8,
                  title = "Uji Hipotesis dan Asumsi",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  h2("Uji Normalitas"),
                  
                  plotlyOutput("hist_norm_2indepen"), #ui histogram norm 2 indenpen====
                  
                  #########Hipotesis norm 2indepen======
                  h3("Uji Hipotesis:"),
                  h4("1. Hipotesis"),
                  uiOutput("hipo1_norm_2indepen_1"),
                  uiOutput("hipo1_norm_2indepen_2"),
                  h4("2. Tingkat Signifikansi"),
                  uiOutput("hipo2_norm_2indepen"),
                  h4("3. Statistik Uji"),
                  uiOutput("hipo3_norm_2indepen_1"),
                  uiOutput("hipo3_norm_2indepen_2"),
                  h4("4. Daerah Kritik"),
                  uiOutput("hipo4_norm_2indepen"),
                  h4("5. Kesimpulan"),
                  uiOutput("hipo5_norm_2indepen"),
                  
                  h3("Interpretasi:"),
                  uiOutput("norm_inter_2indepen"), #interpretasi normalitas====
                  
                  h2("Uji Kesamaan Variansi"),
                  
                  h4("1. Hipotesis"),
                  uiOutput("hipo1_var_2indepen_1"),
                  uiOutput("hipo1_var_2indepen_2"),
                  h4("2. Tingkat Signifikansi"),
                  uiOutput("hipo2_var_2indepen"),
                  h4("3. Statistik Uji"),
                  uiOutput("hipo3_var_2indepen"),
                  h4("4. Daerah Kritik"),
                  uiOutput("hipo4_var_2indepen"),
                  h4("5. Kesimpulan"),
                  uiOutput("hipo5_var_2indepen"),
                  
                  h3("Interpretasi:"),
                  uiOutput("var_inter_2indepen"),
                  
                  h2("Uji Hipotesis 2 Populasi Independen"),
                  
                  plotlyOutput("hist_uji_2indepen", height = "400px"),
                  
                  h4("1. Hipotesis"),
                  uiOutput("hipo1_uji_2indepen_1"),
                  uiOutput("hipo1_uji_2indepen_2"),
                  h4("2. Tingkat Signifikansi"),
                  uiOutput("hipo2_uji_2indepen"),
                  h4("3. Statistik Uji"),
                  uiOutput("hipo3_uji_2indepen"),
                  h4("4. Daerah Kritik"),
                  uiOutput("hipo4_uji_2indepen"),
                  h4("5. Kesimpulan"),
                  uiOutput("hipo5_uji_2indepen"),
                  
                  h3("Interpretasi:"),
                  uiOutput("uji_inter_2indepen")
                )
              )
      ),
      # ========================================================================
      # TAB 2: Uji Populasi Dua Angkatan Dependen
      # ========================================================================
      tabItem(
        tabName = "2dependen",
        fluidRow(
          # ------------------------------
          # Panel Parameter
          # ------------------------------
          box(
            width = 4,
            title = "Pengaturan Parameter",
            status = "primary",
            solidHeader = TRUE,
            fileInput(
              "file_2dependen",
              "Masukkan Data di Sini:",
              placeholder = "Tidak ada file terpilih",
              multiple = FALSE,
              accept = c(".csv", ".txt", ".xlsx")
            ),
            checkboxInput("header_2dependen", "Header", value = TRUE),
            selectInput("sheet_2dependen", "Pilih Sheet:", choices = NULL),
            h4("Data 1"),
            selectInput("column1_2dependen", "Pilih Kolom:", choices = NULL),
            h4("Data 2"),
            selectInput("column2_2dependen", "Pilih Kolom:", choices = NULL),
            h4("Uji Asumsi"),
            checkboxInput(
              "normal_2dependen",
              "Lakukan Uji Normalitas pada selisih (d = value1 - value2)",
              value = TRUE
            ),
            numericInput(
              "mean_2dependen",
              "Hipotesis perbedaan mean (μ0) pada selisih:",
              value = 0
            ),
            selectInput(
              "testing_2dependen",
              "Pilih Uji Hipotesis:",
              choices = c(
                "H0: μ = μ0" = "test1_2dependen",
                "H0: μ ≤ μ0" = "test2_2dependen",
                "H0: μ ≥ μ0" = "test3_2dependen"
              )
            ),
            actionButton(
              "test_2dependen",
              "Test",
              class = "btn-primary btn-block",
              style = "color: white;"
            ),
            hr(),
            uiOutput("summary_info_2dependen")
          ),
          
          # ------------------------------
          # Tabel Overview 1 & 2
          # ------------------------------
          box(
            width = 4,
            title = "Tabel Overview 1",
            status = "primary",
            solidHeader = TRUE,
            tableOutput("table1_2dependen")
          ),
          box(
            width = 4,
            title = "Tabel Overview 2",
            status = "primary",
            solidHeader = TRUE,
            tableOutput("table2_2dependen")
          ),
          
          # ------------------------------
          # Hasil Uji
          # ------------------------------
          box(
            width = 8,
            title = "Uji Hipotesis dan Asumsi",
            status = "primary",
            solidHeader = TRUE,
            h2("Uji Normalitas"),
            plotlyOutput("hist_norm_2dependen"),
            h3("Interpretasi:"),
            uiOutput("norm_inter_2dependen"),
            h2("Uji Hipotesis 2 Populasi Dependen"),
            plotlyOutput("hist_uji_2dependen", height = "400px"),
            h3("Interpretasi:"),
            uiOutput("uji_inter_2dependen")
          )
        )
      ),
      # ============================================================================
      # TAB 4: ANOVA 
      # ============================================================================
      tabItem(
        tabName = "teori_anova",
        box(
          width = 12,
          title = "Teori ANOVA",
          status = "primary",
          solidHeader = TRUE,
          h2("Konsep Dasar ANOVA"),
          p("ANOVA (Analysis of Variance) digunakan untuk menguji apakah terdapat perbedaan rata-rata antara tiga atau lebih kelompok."),
          tags$ul(
            tags$li("H0: Semua rata-rata populasi sama (μ₁ = μ₂ = ... = μk)"),
            tags$li("H1: Setidaknya ada dua rata-rata populasi yang berbeda")
          ),
          p("Statistik uji menggunakan Rasio F:"),
          withMathJax("$$F = \\frac{MS_{Between}}{MS_{Within}}$$"),
          p("Jika p-value < 0.05 maka H0 ditolak → terdapat perbedaan rata-rata antar kelompok.")
        )
      ), 
      tabItem(tabName = "uji_anova",
              
              fluidRow(
                box(
                  width = 4,
                  title = "Pengaturan Parameter ANOVA",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  fileInput("file_anova", "Masukkan Data Di Sini:",
                            placeholder = "Tidak ada file terpilih",
                            accept = c(".csv", ".txt", ".xlsx")),
                  
                  checkboxInput("header_anova", "Header", value = TRUE),
                  
                  selectInput("response_anova", "Pilih Variabel Respon:", choices = NULL),
                  
                  selectInput("factor_anova", "Pilih Variabel Faktor:", choices = NULL),
                  
                  h4("Uji Asumsi"),
                  checkboxInput("normal_anova", "Uji Normalitas Residual", value = TRUE),
                  checkboxInput("homo_anova", "Uji Homogenitas Varians", value = TRUE),
                  
                  actionButton("run_anova", "Jalankan ANOVA",
                               class = "btn-primary btn-block", style = "color: white;")
                ),
                
                box(
                  width = 8,
                  title = "Hasil ANOVA",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  h3("Tabel ANOVA"),
                  verbatimTextOutput("anova_table"),
                  
                  h3("Boxplot Respon per Kelompok"),
                  plotOutput("anova_boxplot", height = "300px"),
                  
                  h3("Plot Diagnostik Residual"),
                  fluidRow(
                    column(
                      width = 6,
                      h4("Histogram Residual"),
                      plotOutput("anova_resid_hist", height = "250px")
                    ),
                    column(
                      width = 6,
                      h4("QQ-Plot Residual"),
                      plotOutput("anova_resid_qq", height = "250px")
                    )
                  ),
                  
                  h4("Residual vs Fitted"),
                  plotOutput("anova_resid_fitted", height = "250px"),
                  
                  h3("Uji Asumsi ANOVA"),
                  verbatimTextOutput("anova_assump_test"),
                  
                  h3("Interpretasi Uji Asumsi"),
                  uiOutput("anova_assump_interpret"),
                  
                  h3("Interpretasi Model"),
                  uiOutput("anova_interpret")
                )
                
              )
      )      
    )
  )
)
