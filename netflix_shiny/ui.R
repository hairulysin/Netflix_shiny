dashboardPage(skin = "black",
              # ------ HEADER -------
              dashboardHeader(title = span(tagList(icon = icon("film"), tags$b("NETFLIX"),
                                                   titlePanel("Netflix Dashboard")))),
              
              # ------ SIDE BAR -------
              dashboardSidebar(
                tags$head(
                  tags$style(HTML("
                          /* Memperbesar ukuran teks sidebar */
                          .sidebar .treeview-menu > li > a {
                            font-size: 16px; /* Ubah ukuran teks sesuai kebutuhan */
                          }
                        "))
                ),
                sidebarMenu(
                  menuItem(
                    text = "Ringkasan",
                    tabName = "page1",
                    icon = icon("clipboard")
                  ),
                  menuItem(
                    text = "Populer",
                    tabName = "page2",
                    icon = icon("chart-simple")
                  ),
                  menuItem(
                    text = "Perkembangan",
                    tabName = "page3",
                    icon = icon("arrow-trend-up")
                  ),
                  menuItem(
                    text = "Tabel Data",
                    tabName = "page4",
                    icon = icon("table"),
                    badgeLabel = "Lihat",
                    badgeColor = "maroon"
                  )
                )
              ),
              
              # ------ BODY -------
              dashboardBody(
                tabItems(
                  tabItem(
                    #--------------- PAGE 1 (OVERVIEW) ---------------
                    tabName = "page1",
                    h2(tags$b("Gambaran Ringkas")),
                    br(),
                    fluidPage(
                      valueBox(nrow(netflix),
                               "Total Konten",
                               icon = icon("tv"),
                               color = "red"),
                      valueBox(sum(netflix_new$type == "Movie"),
                               "Jumlah Film",
                               icon = icon("film"),
                               color = "blue"),
                      valueBox(length(unique(netflix$director)),
                               "Sutradara Teratas",
                               icon = icon("user-large"),
                               color = "maroon")
                    ),
                    br(),br(),
                    
                    fluidRow(
                      box(width = 6, plotlyOutput(outputId = "plot_sutradara", height = 843)),
                      box(width = 6, plotlyOutput(outputId = "plot_type", height = 400)),
                      box(width = 6, plotlyOutput(outputId = "plot_rating", height = 400))
                    ),
                    fluidPage(
                      h3(tags$b("Peta Persebaran Kontribusi Negara")),br(),
                      box(width = 12,
                          solidHeader = TRUE,
                          leafletOutput("leaflet", height = 530))
                    )
                  ),
                  #--------------- PAGE 2 (TREND) ---------------
                  tabItem(
                    tabName = "page2",
                    h2(tags$b("Popularitas Genre")),
                    br(),
                    fluidRow(
                      box(width = 6, height = 200, solidHeader = TRUE, status = "warning", background = "blue",
                          sliderInput("category_range",
                                      h4(tags$b("Pilih Rentang Jumlah Kategori Populer (Maks 15)  : ")),
                                      min = 1, max = length(unique(netflix$listed_in)), value = c(1,15), step = 1)),
                      box(width = 6, height = 200, solidHeader = TRUE, status = "warning", background = "blue",
                          checkboxGroupInput(
                            inputId = "negara",
                            label = tags$b("Pilih Negara :"),
                            choices = c("Thailand", "Indonesia", "Singapore", "Vietnam", "Philippines",  "Japan"),
                            selected = character(0)
                          )),
                      box(width = 6, plotlyOutput(outputId = "plot_genre", height = 883)),
                      box(width = 6, plotlyOutput(outputId = "plot_banding", height = 400)),
                      tabBox(width = 6,
                             title = tags$b("Distribusi Durasi Konten"),
                             id = "tabset1",
                             side = "right",
                             tabPanel(tags$b("Durasi Rating"),
                                      plotlyOutput("plot_dist2")),
                             tabPanel(tags$b("Durasi Film"),
                                      plotlyOutput("plot_dist"))
                      )
                    ),
                    
                  ),
                  #--------------- PAGE 3 ---------------
                  tabItem(
                    tabName = "page3",
                    h2(tags$b("Perkembangan Konten")),
                    br(),
                    fluidPage(
                      sidebarLayout(
                         box(width = 4, height = 370, background = "red",
                            selectInput(
                            inputId = "negara_asean",
                            label = h4(tags$b("Pilih Negara untuk Dibandingkan :")),
                            choices = head(unique(netflix$country[netflix$release_year > 2000]), 40),
                            selected = c("Thailand")
                          )),
                        mainPanel(
                          box(width = 7.5, plotlyOutput(outputId = "plot_trendIndo", height = 350))
                        )
                      ),
                      fluidRow(
                        tabBox(width = 6,
                               title = tags$b("Tren Konten tiap Tahun"),
                               id = "tabset1",
                               side = "right",
                               tabPanel(tags$b("Penambahan Konten"),
                                        plotlyOutput("plot_kontenTahun")),
                               tabPanel(tags$b("Film & Serial TV"),
                                        plotlyOutput("plot_trendType"))
                               ),
                        # box(width = 6, plotlyOutput(outputId = "plot_trendType", height = 350)),
                        # box(width = 6, plotlyOutput(outputId = "plot_kontenTahun", height = 350)),
                        box(width = 6, plotlyOutput(outputId = "plot_asean", height = 440)),
                        br(),br()
                      )
                    ),
                  ),
      
                  #--------------- PAGE 4 ---------------
                  tabItem(
                    tabName = "page4",
                    h2(tags$b("Tabel Data")),
                    br(),
                    fluidPage(box(width = 12, DT::dataTableOutput("data3")))
                  )
                )
              )
)
