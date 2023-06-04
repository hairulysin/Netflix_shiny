function(input, output) {
  
  #======================= TAB 1 =================
  output$leaflet <-  renderLeaflet({
    m <- leaflet(shape) %>% 
      addProviderTiles("Esri.NatGeoWorldMap") %>%
      setView(lat = 10, lng = 0, zoom = 2) %>%
      addPolygons(
        fillColor = ~mypalette(shape@data$contributors),
        color = "red",
        dashArray = "3",
        fillOpacity = 0.6,
        weight = 1,
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        ),
        popup = popup_shape
      ) %>%
      addLegend(
        pal = mypalette,
        values = shape@data$contributors,
        opacity = 0.9,
        title = "Jumlah Kontribusi",
        position = "bottomleft",
        labels = formatC(seq(1, max(netflix2$contributors), by = 1000), digits = 0)
      )
    
    #tampilkan map
    m
    
  })
  
  output$plot_type <- renderPlotly({
    palet_warna <- c("#db0000", "black")
    
    # Membuat grafik bar chart
    plot_bar <- ggplot(netflix1,
                       aes(x = type, 
                           y = count, 
                           fill = type,
                           text = glue("Jenis Konten : {type}
                                   Jumlah: {count}
                                   Distribusi : {round(count/sum(count)*100,2)} %"))) +
      geom_bar(stat = "identity", color = "black") +
      labs(x = "Jenis Konten", 
           y = "Jumlah", 
           fill = "Jenis Konten") +
      ggtitle("Jenis Konten di Netflix") +
      scale_fill_manual(values = palet_warna) +
      theme(plot.title = element_text(hjust = 0.5))+
      theme_algoritma+
      guides(fill = FALSE)
    
    ggplotly(plot_bar, tooltip = "text")
  })
  
  output$plot_rating <- renderPlotly({
    netflix4 <- select(netflix, rating)
    
    # Menghitung frekuensi rating
    rating_counts <- table(netflix4$rating)
    rating_data <- data.frame(rating = names(rating_counts), 
                              count = as.numeric(rating_counts))
    
    # Membuat grafik bar plot 
    plot_rating <- ggplot(rating_data, 
                          aes(x = reorder(rating, +count), 
                              y = count,
                              text = glue("Rating: {rating}
                                      Jumlah: {count}"))) +
      geom_bar(stat = "identity", aes(fill = count)) +
      labs(title = "Distribusi Rating Konten di Netflix",
           x = "Rating",
           y = "Jumlah",
           fill = " Jumlah") +
      # coord_flip() +
      scale_fill_gradient(low = "black", high = "#db0000") +  # Gradasi warna merah
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position = "none") +
      theme_algoritma
    
    ggplotly(plot_rating, tooltip = "text")
  })
  
  # Menghitung frekuensi kategori konten
  netflix_6 <- table(netflix$listed_in)
  
  output$plot_genre <- renderPlotly({
    # Mengambil rentang jumlah kategori berdasarkan input slider
    category_range <- input$category_range
    
    # Mengurutkan frekuensi secara menurun
    sorted_categories <- sort(netflix_6, decreasing = TRUE)
    
    # Memilih kategori yang sesuai dengan rentang jumlah
    selected_categories <- sorted_categories[category_range[1]:category_range[2]]
    
    # Mengambil 12 kategori teratas
    top_categories <- selected_categories[1:12]
    
    # Membuat data frame dari hasil penghitungan
    category_data <- data.frame(category = names(top_categories),
                                count = as.numeric(top_categories))
    
    # Membuat plot bar untuk visualisasi
    plot_categories <- ggplot(category_data, aes(x = reorder(category, +count), 
                                                 y = count, fill = count,
                                                 text = glue("Kategori : {category}\nJumlah : {count}"))) +
      geom_bar(stat = "identity", 
               width = 0.9, 
               position = "dodge") +
      labs(title = "Kategori Genre Paling Umum di Netflix",
           x = NULL,
           y = "Jumlah",
           fill = "Jumlah") +
      coord_flip() +
      scale_fill_gradient(low = "#db0000", high = "black") +
      scale_x_discrete(labels = wrap_format(width = 25)) +
      theme_algoritma +
      theme(legend.position = "none") +
      theme(plot.title = element_text(hjust = 0.7),
            panel.grid.major.x = element_line(color = "darkgrey", linetype = 2),
            panel.grid.major.y = element_blank())
    
    ggplotly(plot_categories, tooltip = "text")
  })
  #======================= TAB 2 =================
  
  output$plot_dist <- renderPlotly({
    # Filter data hanya untuk tipe 'Movie'
    netflix_5 <- netflix %>%
      filter(type == 'Movie')
    
    # Membuat kolom 'duration_numeric' yang berisi durasi dalam format numerik
    netflix_5 <- netflix_5 %>%
      mutate(Durasi = as.numeric(gsub("[^0-9]", "", duration)))
    
    mean_duration <- mean(netflix_5$Durasi)
    
    # Visualisasi dengan histogram
    plot_movie_duration <- ggplot(netflix_5, aes(x = Durasi)) +
      geom_histogram(binwidth = 10, fill = "#831010", color = "black") +
      geom_vline(xintercept = mean_duration, color = "black", linetype = "dashed", size = 1) +
      labs(title = "Distribusi Durasi Film di Netflix",
           x = "Durasi (Menit)",
           y = "Jumlah Film (count) ") +
      geom_text(x = mean_duration, y = 140, label = "Rata-rata", color = "white", vjust = -2, hjust = -0.2) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme_algoritma +
      scale_y_continuous(name = "Jumlah Film")
    
    ggplotly(plot_movie_duration)
  })
  
  
  output$plot_banding <- renderPlotly({
    
    # Membaca input checkbox
    selected_countries <- input$negara
    
    perbandingan_rating <- netflix_new %>%
      group_by(country, rating_type) %>%
      summarize(jumlah_film = n()) %>%
      filter(rating_type %in% c("Remaja", "Dewasa")) %>%
      filter(country != "Not Given") %>%
      mutate(proporsi = round((jumlah_film / sum(jumlah_film)) * 100))
    
    # Menentukan negara yang akan ditampilkan dalam plot
    predefined_countries <- c("United States", "United Kingdom", "France", "Spain", "Mexico", "South Korea")
    
    selected_countries <- union(selected_countries, predefined_countries)
    
    top_countries <- perbandingan_rating %>%
      filter(country %in% selected_countries)
    
    plot_composition <- top_countries %>% 
      ggplot(mapping = aes(x = proporsi,
                           y = country,
                           fill = factor(rating_type, levels = c("Dewasa", "Remaja")),
                           text = glue("Rating: {rating_type}\nPersentase: {proporsi}%"))) +
      geom_col(position = position_stack(reverse = TRUE)) +
      geom_vline(xintercept = 50, lty = 2, lwd = 1.5, col = "white") +
      scale_fill_manual(values = c("Dewasa" = "#db0000", "Remaja" = "#474747"), drop = FALSE) +
      scale_x_continuous(labels = scales::number_format(suffix = "%")) +
      labs(title = "Rating Dewasa vs Remaja di Negara Teratas",
           x = NULL,
           y = NULL) +
      theme_algoritma +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major.x = element_line(color = "darkgrey", linetype = 2),
            panel.grid.major.y = element_blank()) +
      theme(axis.text.y = element_text(hjust = 0.5, size = 10)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position = "none")
    
    ggplotly(plot_composition, tooltip = c("text"))
    
  })
  
  
  
  output$plot_sutradara <- renderPlotly({
    # Menghitung jumlah konten berdasarkan sutradara
    director_counts <- netflix %>%
      count(director, sort = TRUE)
    top_directors <- head(director_counts, n = 15)
    
    # Menghilangkan sutradara dengan nilai "Not Given"
    top_directors <- top_directors %>%
      filter(director != "Not Given")
    
    # Menghitung persentase jumlah konten
    top_directors <- top_directors %>%
      mutate(percentage = n / sum(n) * 100)
    
    #  sutradara berdasarkan persentase tertinggi
    top_directors <- top_directors %>%
      arrange(desc(percentage))
    
    # Membuat plot bar chart 
    plot_sutradara <- ggplot(top_directors, aes(x = percentage, 
                                   y = reorder(director, percentage),
                                   text = glue("Sutradara: {director}
                                           Persentase: {round(percentage, 2)} %",
                                           fill = percentage))) +
      geom_col(aes(fill = percentage)) +
      labs(title = "Sutradara Terbanyak",
           x = "Jumlah Persentase", y = "", fill = "Persentase") +
      theme_algoritma +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major.x = element_line(color = "darkgrey", linetype = 2),
            panel.grid.major.y = element_blank()) +
      theme(legend.position = "none") +
      scale_y_discrete(labels = wrap_format(width = 15))+
      scale_x_continuous(labels = scales::number_format(suffix = "%")) +
      scale_fill_gradient(low = "#474747", high = "#c40815")
    
    ggplotly(plot_sutradara, tooltip = "text")
  })
  output$plot_dist2 <- renderPlotly({
    netflix_new <- netflix_new[netflix_new$rating_type != "Lainnya" & !is.na(netflix_new$durasi_menit), ]
    
    # Menghitung rata-rata durasi
    rata_durasi <- mean(netflix_new$durasi_menit, na.rm = TRUE)
    
    # Membuat plot histogram dengan garis putus-putus rata-rata durasi
    durasi_rating_plot <- ggplot(netflix_new, aes(x = durasi_menit, fill = rating_type, 
                                                  text = paste("Durasi:", durasi_menit, "menit",
                                                               "\nRating :", rating_type))) +
      geom_histogram(binwidth = 10, position = "identity", alpha = 0.7) +
      geom_vline(xintercept = rata_durasi, linetype = "dashed", color = "white") +  # Menambahkan garis putus-putus rata-rata
      geom_text(x = rata_durasi, y = 10, label = "Rata-rata", vjust = -1, color = "white", size = 3, angle = 90) +  # Menambahkan teks "Rata-rata"
      labs(title = "Perbedaan Durasi antara Rating Dewasa & Remaja",
           x = "Durasi (menit)", y = "Jumlah Film") +
      scale_fill_manual(values = c("Dewasa" = "#db0000", "Remaja" = "black"), 
                        labels = c("Dewasa", "Remaja"),
                        breaks = c("Dewasa", "Remaja"),
                        drop = FALSE) +
      theme(legend.position = "none") +
      theme_algoritma +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major.x = element_line(color = "darkgrey", linetype = 2))
    
    durasi_rating_plot <- ggplotly(durasi_rating_plot, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.35, y = -0.2)) 
    
    durasi_rating_plot
  })
  
  #======================= TAB 3 =================
  
  output$plot_kontenTahun <- renderPlotly({
    netflix3 <- netflix %>% 
      group_by(release_year) %>% 
      summarise(count = n()) %>% 
      arrange(release_year) %>% 
      filter(release_year >= 1980)
    
    plot_trend <- ggplot(netflix3, 
                         aes(x = release_year, 
                             y = count,
                             text = glue("Tahun Rilis : {release_year}
                                     Jumlah konten : {count}"))) +
      geom_col(fill = "#564d4d", width = 0.8) +
      geom_col(data = netflix3 %>% 
                 filter(release_year == 2018), fill = "#db0000", width = 1) +
      labs(title = "Tren Penambahan Konten ke Netflix Tiap Tahun",
           x = NULL,
           y = "Jumlah Konten") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme_algoritma 
    
    
    ggplotly(plot_trend, tooltip = "text")
  })
  
  output$plot_trendType <- renderPlotly({
    # Filter data
    netflix_20th_century <- netflix %>%
      filter(release_year >= 2000)
    
    # Menghitung jumlah film dan serial TV berdasarkan tahun dan tipe
    trend_counts <- netflix_20th_century %>%
      group_by(release_year, type) %>%
      summarise(Jumlah = n()) %>%
      ungroup()
    
    # Menghitung proporsi
    trend_counts <- trend_counts %>%
      group_by(release_year) %>%
      mutate(proportion = Jumlah / sum(Jumlah))
    
    # Visualisasi tren jumlah film dan serial TV
    plot_trend <- trend_counts %>%
      ggplot(mapping = aes(x = release_year,
                           y = Jumlah,
                           fill = type,
                           text = paste("Tahun Rilis: ", release_year, "<br>Tipe: ", type, "<br>Jumlah: ", Jumlah))) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = "Tren Jumlah Film dan Serial TV di Netflix",
           x = "Tahun Rilis",
           y = "Jumlah",
           fill = "Tipe") +
      scale_fill_manual(values = c("#831010", "black")) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position = "none") +
      theme_algoritma
    
     ggplotly(plot_trend, tooltip = "text")
 
  })
  output$plot_asean <- renderPlotly({
    asia_countries <- c("Indonesia", "Malaysia", "Thailand", "Singapore", "Philippines", "Vietnam", "Laos", "Myanmar", "Cambodia")
    
    asia_plot <- netflix %>%
      filter(country %in% asia_countries) %>%
      group_by(country, type) %>%
      summarise(count = n()) %>%
      ggplot(aes(x = reorder(country, -count), y = count, fill = type, text = glue("Negara: {country}\nJumlah: {count}\nTipe: {type}"))) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = "Perbedaan Jumlah Film dan TV Show di ASEAN",
           x = "", y = "Jumlah", fill = "Tipe :") +
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
      scale_fill_manual(values = c("Movie" = "#971400", "TV Show" = "black")) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme_algoritma
    
    asia_plot_interaktif <- ggplotly(asia_plot, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.35, y = -0.1)) 
    asia_plot_interaktif
  })
  
  filtered_data <- reactive({
    # Filter data berdasarkan negara yang dipilih dan tahun 2000 ke atas
    netflix[netflix$country == input$country & netflix$release_year >= 2000, ]
  })
  
  yearly_counts <- reactive({
    # Menghitung jumlah film yang dirilis setiap tahun
    table(filtered_data()$release_year)
  })
  
  output$plot_trendIndo <- renderPlotly({
    # Filter data untuk film yang dirilis di Indonesia sejak tahun 2000
    filtered_data_indonesia <- netflix[netflix$country == "Indonesia" & netflix$release_year >= 2000, ]
    
    # Filter data untuk film yang dirilis di negara ASEAN lainnya sejak tahun 2000
    filtered_data_asean <- netflix[netflix$country %in% input$negara_asean & netflix$release_year >= 2000, ]
    
    # Menghitung jumlah film yang dirilis setiap tahun di Indonesia
    yearly_counts_indonesia <- table(filtered_data_indonesia$release_year)
    
    # Menghitung jumlah film yang dirilis setiap tahun di negara ASEAN lainnya
    yearly_counts_asean <- table(filtered_data_asean$release_year)
    
    df_indonesia <- data.frame(tahun = as.numeric(names(yearly_counts_indonesia)), jumlah = as.numeric(yearly_counts_indonesia))
    df_asean <- data.frame(tahun = as.numeric(names(yearly_counts_asean)), jumlah = as.numeric(yearly_counts_asean))
    
    # Membuat plot 
    line_plot <- ggplot() +
      geom_line(data = df_indonesia, aes(x = tahun, 
                                         y = jumlah, 
                                         color = "Indonesia"), size = 0.9) +
      geom_line(data = df_asean, aes(x = tahun, 
                                     y = jumlah, 
                                     color = "Lain"), size = 0.9) +
      labs(title = "Perbandingan Trend Rilis Film Indonesia",
           x = NULL, y = "Jumlah Film",
           color = "Negara") +
      scale_color_manual(values = c("Indonesia" = "#db0000", "Lain" = "#564d4d")) +
      theme_algoritma +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major.x = element_line(color = "darkgrey", linetype = 2))
    
    # Konversi plot menjadi plot interaktif menggunakan plotly
    interactive_plot <- ggplotly(line_plot, tooltip = c("x", "y", "group")) %>%
      layout(hoverlabel = list(namelength = -1),
             hovertemplate = "Tahun Rilis: %{x}<br>Total: %{y}<br>Negara: %{group}") %>% 
      layout(legend = list(orientation = "h", x = 0.35, y = -0.1)) 
    
    # Menampilkan plot interaktif
    interactive_plot
  })
  
  # Data Table
  output$data3 <- DT::renderDataTable(netflix_new, options = list(scrollX = T))
}
