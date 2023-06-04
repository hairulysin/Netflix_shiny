# Import library
library(shiny)
library(shinydashboard)
library(dplyr) # untuk transformasi data
library(plotly) # untuk membuat plot menjadi interaktif
library(glue) # untuk custom informasi saat plot interaktif
library(scales) # untuk custom keterangan axis atau lainnya
library(tidyr) # untuk custom keterangan axis atau lainnya
library(DT) # untuk menampilkan dataset
library(stringr)
library(maps)
library(leaflet)
library(shinythemes)
library(maps)
library(rgdal)
library(leaflet)
library(rgeos)
library(RColorBrewer)


fluidPage(theme = shinytheme("united"),
          # Import dataset yang sudah clean
          netflix <- read.csv("data_IP/netflix.csv"),
          # Data Cleansing
          netflix1 <-  netflix %>% 
            group_by(type) %>% 
            summarize(count = n()) %>% 
            mutate(type = factor(type)),
          
          # Membuat kolom durasi_jam dan durasi_menit
          netflix_new <- netflix %>%
            mutate(durasi_menit = as.numeric(sub(" min", "", duration)),
                   durasi_season = as.numeric(sub(" Season", "", duration))) %>% 
            mutate(rating_type = case_when(
              rating %in% c("TV-MA", "R") ~ "Dewasa",
              rating %in% c("PG-13", "TV-14") ~ "Remaja",
              TRUE ~ "Lainnya"
            )),
          
          
          
          # Import dataset yang sudah clean
          theme_algoritma <- theme(legend.key = element_rect(fill="black"),
                                   legend.background = element_rect(color="white", fill="#263238"),
                                   plot.subtitle = element_text(size=6, color="white"),
                                   panel.background = element_rect(fill="#dddddd"),
                                   panel.border = element_rect(fill=NA),
                                   panel.grid.minor.x = element_blank(),
                                   panel.grid.major.x = element_blank(),
                                   panel.grid.major.y = element_line(color="darkgrey", linetype=2), 
                                   panel.grid.minor.y = element_blank(),
                                   plot.background = element_rect(fill="#263238"),
                                   text = element_text(color="white"),
                                   axis.text = element_text(color="white")
          ),
          
          world_map <- map_data("world"),
          
          # Prepare Netflix data
          netflix2 <- netflix %>% 
            group_by(country) %>% 
            summarise(contributors = n()) %>% 
            arrange(desc(contributors)),
          
          # Shape file
          shape <- raster::shapefile("TM_WORLD_BORDERS_SIMPL-0.3.shp"),
          
          # Combine tabular data into shape data
          shape@data <- shape@data %>% dplyr::left_join(netflix2, by = c("NAME" = "country")),
          
          # Prepare leaflet map data
          leaflet_data <- world_map %>% 
            left_join(netflix2, by = c("region" = "country"), suffix = c(".map", ".netflix")),
          
          mypalette <- colorNumeric(
            palette = "YlOrRd", 
            domain = c(0, max(netflix2$contributors))
          ),
        
          mytext <- paste(shape@data$NAME) %>%
            lapply(htmltools::HTML),
          
          popup_shape <- paste("<h3><b>", shape@data$NAME, "</b></h3>",
                               "Jumlah Film : ", shape@data$contributors, "<br>",
                               sep=""),
        
          shape@data[shape@data$NAME == "United States", "NAME"] <- "USA"
)

# SliderInput

## Menghitung jumlah kategori yang unik
num_categories <- length(unique(netflix$listed_in))

## Menentukan nilai minimum pada slider berdasarkan jumlah kategori yang unik
min_value <- max(num_categories - 11, 1)
