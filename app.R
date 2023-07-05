#  remove scientific notation
options("scipen"=100, "digits"=4) 
# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))

# install.packages(c("shiny", "knitr", "shinyWidgets", "shinythemes", "ggplot2", "plotly", "dplyr", "leaflet", "RColorBrewer", "htmltools", "kableExtra", "png", "rsconnect"))
library(shiny)
library(knitr)
library(shinyWidgets)
library(shinythemes)
library(ggplot2)
library(plotly)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(htmltools)
library(kableExtra)
library(png)
library(rsconnect)

# DATABASE: https://public.opendatasoft.com/explore/dataset/significant-earthquake-database/table/?sort=deaths


data <- read.csv("https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/significant-earthquake-database/exports/csv?lang=en&timezone=Europe%2FHelsinki&use_labels=true&delimiter=%3B", sep = ";")
data <- data[,-c(1,8:14,16,18:30,32:34, 36:42)]
continents <- read.csv("continents-according-to-our-world-in-data.csv")
continents$Entity <- toupper(continents$Entity) 
continents <- continents[, c(1,4)]
colnames(continents)[1] <- "Country"
continents[nrow(continents) + 1,] <- c("USA", "North America")
data <- data %>% 
  filter(!is.na(Total.Effects...Deaths) & !is.na(EQ.Primary) & !is.na(Focal.Depth) & !is.na(Month) & !is.na(Day) 
         & Year >= 1901 & Coordinates != "")
data <- data %>%
  left_join(x = data, y = continents, by = "Country") %>%
  filter(!is.na(Continent)) %>% 
  mutate(Continent_midpoint = ifelse(Continent == "Asia", "34.0479, 100.6197",
                                     ifelse(Continent == "Africa", "5.740284, 22.025688",
                                            ifelse(Continent == "Europe", "54.5260, 15.2551",
                                                   ifelse(Continent == "Oceania", "-12.295243, 147.638966",
                                                          ifelse(Continent == "Antarctica", "82.8628, 135.0000",
                                                                 ifelse(Continent == "North America", "34.524334, -94.928129", "-16.720766, -56.080475")))))),
         Period = ifelse(Year <= 1925, "1901-1925",
                         ifelse(Year > 1925 & Year <= 1950, "1926-1950",
                                ifelse(Year > 1950 & Year <= 1975, "1951-1975",
                                       ifelse(Year > 1975 & Year <= 2000, "1976-2000", "2001-2020")))),
         Month = ifelse(Month %in% c(1:9), paste("0", Month, sep = ""), Month),
         Day = ifelse(Day %in% c(1:9), paste("0", Day, sep = ""), Day),
         Flag.Tsunami = ifelse(Flag.Tsunami == "Tsunami", "jah", "ei"), 
         Total.Effects...Injuries = ifelse(is.na(Total.Effects...Injuries), "teadmata", as.character(Total.Effects...Injuries)))

#-----------------------------------------
# FUNKTIONS

# map \page 1
add_cord <- function(period, continent, layer) {
  kaart <- leaflet() %>%
    addTiles() 
  
  if (period == "kogu aeg" & continent == "kõik kontinendid"){
    period_data = data
  }
  else if (period == "kogu aeg" & continent != "kõik kontinendid") {
    period_data = data %>% 
      filter(Continent == continent)
  }
  else if (period != "kogu aeg" & continent == "kõik kontinendid") {
    period_data = data %>% 
      filter(Period == period)
  } else{
    period_data = data %>% 
      filter(Period == period & Continent == continent)
  }
  
  pal <- colorNumeric("PuBuGn", period_data$EQ.Primary)
  
  for (i in 1:nrow(period_data)) {
    maavarin <- period_data[i,]
    kuupaev <- paste(maavarin$Day, maavarin$Month, maavarin$Year, sep = ".")
    labels <- sprintf("<strong>%s</strong><br/>Kuupäev: %s<br/>Sügavus: %g km<br/>Magnituud: %g<br/>Surmad: %g<br/>Vigastatud: %s",
                      maavarin$Location.name, kuupaev, maavarin$Focal.Depth, maavarin$EQ.Primary, maavarin$Total.Effects...Deaths, maavarin$Total.Effects...Injuries) %>% 
      lapply(htmltools::HTML)
    
    kaart = kaart %>% 
      addCircleMarkers(lat = as.numeric(strsplit(maavarin$Coordinates, ", ")[[1]][1]),
                       lng = as.numeric(strsplit(maavarin$Coordinates, ", ")[[1]][2]),
                       radius = sqrt(maavarin$Total.Effects...Deaths)/5, weight = 1, fillColor=pal(maavarin$EQ.Primary),
                       fillOpacity = 0.7, color = "grey",
                       label = labels)
  }
  if (continent == "kõik kontinendid"){
    continent_lat = 36.022958
    continent_lng = 7.315462
    zoom = 2
  } else{
    continent_lat = as.numeric(strsplit(period_data[1,]$Continent_midpoint, ", ")[[1]][1])
    continent_lng = as.numeric(strsplit(period_data[1,]$Continent_midpoint, ", ")[[1]][2])
    zoom = 3
  }
  
  kaart = kaart %>% 
    addLegend(position = "bottomright", pal = pal, values = period_data$EQ.Primary, title = paste("Magnituud", "<br/>")) %>% 
    setView(lng = continent_lng, lat = continent_lat, zoom = zoom) %>% 
    addProviderTiles(layer)
  return(kaart)
}

add_cord("kogu aeg", "kõik kontinendid", providers$CartoDB.Positron)

# sort and show data \page 2
topn = function(data, s, n1){
  data2 <- data %>%
    arrange(desc(s))
  t = data.frame(data2[c(1:n1), c(8, 2, 6, 5, 9, 10)])
  colnames(t) <- c("Asukoht", "Aasta", "Magnituud", "Sügavus (km)", "Surmad", "Vigastatud")
  return(t)
}

# tsunami status \page 3 second graph
tsunami <- function(data, period, continent) {
  
  if (period == "kogu aeg" & continent == "kõik kontinendid"){
    period_data = data
  }
  else if (period == "kogu aeg" & continent != "kõik kontinendid") {
    period_data = data %>% 
      filter(Continent == continent)
  }
  else if (period != "kogu aeg" & continent == "kõik kontinendid") {
    period_data = data %>% 
      filter(Period == period)
  } else{
    period_data = data %>% 
      filter(Period == period & Continent == continent)
  }
  
  p <- period_data %>% 
    group_by(EQ.Primary) %>% 
    mutate(count = n()) %>% 
    mutate(text = paste("\nMagnituud: ", EQ.Primary, "\nKogus: ", count, sep=""))
  
  p <- ggplot(p, aes(x = EQ.Primary, fill = Flag.Tsunami, text = text)) +
    geom_bar(alpha=0.7) +
    scale_fill_manual(values = c("#52854C", "#35978f")) +
    xlab("Magnituud") +
    ylab(paste("Kogus", " (kokku ", nrow(period_data), ")", sep = "")) + 
    labs(fill="Tsunami staatus") + theme_minimal()  + 
    ggtitle("Maavärinate arv magnituudi järgi") +
    theme(plot.title = element_text(hjust = 0.5))
  ggplotly(p, tooltip="text") %>% 
    layout(legend = list(x = 100, y = 0.5))
}

tsunami(data, "kogu aeg", "kõik kontinendid")

# bubble function \page 2 second graph
bubble <- function(data, n, s) {
  p <- data %>% 
    arrange(desc(s))
  p <- p[c(1:n),] 
  p <- p %>% 
    mutate(text = paste("Riik: ", Country, "\nSurmad: ", Total.Effects...Deaths, "\nVigastatud: ", Total.Effects...Injuries, "\nMagnituud: ", EQ.Primary, "\nSügavus: ", Focal.Depth, " km", sep=""))

  p <- ggplot(p, aes(y=Focal.Depth, x=EQ.Primary, size = Total.Effects...Deaths, color = Continent, text=text)) +
    geom_point(alpha=0.7) +
    scale_radius(range = c(1.5, 25), name="") +
    scale_color_manual(values = c("#543005", "#dfc27d", 
                       "#D16103", "#C3D7A4", "#4E84C4", "#52854C")) +
    theme_minimal() + labs(y = "Sügavus", x = "Magnituud")+ 
    labs(color="Kontinent")
  
  ggplotly(p, tooltip="text") %>% 
    layout(legend = list(x = 100, y = 0.5))
} 

bubble(data, 50, data$EQ.Primary)

# \page 3 first graph
proportion <- function(data, continent, period) {
  if (period == "kogu aeg" & continent == "kõik kontinendid"){
    period_data = data
  }
  else if (period == "kogu aeg" & continent != "kõik kontinendid") {
    period_data = data %>% 
      filter(Continent == continent)
  }
  else if (period != "kogu aeg" & continent == "kõik kontinendid") {
    period_data = data %>% 
      filter(Period == period)
  } else{
    period_data = data %>% 
      filter(Period == period & Continent == continent)
  }
  
  data_tihedus <- period_data %>%
    group_by(Country) %>%
    summarise(density = n() / nrow(period_data), earthquake_count = n())
  
  data_filter <- left_join(x = data_tihedus, y = continents, by = "Country")
  
  if (continent != "kõik kontinendid") {
    data_filter = data_filter %>%
      filter(Continent == continent)
  } else {data_filter = data_filter}
  
  p <- ggplot(data_filter, aes(x = Country, y = density,
                               text = paste(Continent, "\nRiik:", Country, "<br>Tihedus:", round(density * 100, 2), "%", "<br>Maavärinate arv:", earthquake_count))) +
    labs(x = "Riik", y = "Tihedus")  + theme_minimal() +
    ggtitle("Maavärinate tihedus riigiti") +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
  
  if (continent == "Asia") {
    p <- p + geom_col(alpha=0.8, fill = "#dfc27d")
  } else if (continent == "Africa"){
    p <- p + geom_col(alpha=0.8, fill = "#543005")
  } else if (continent == "Europe"){
    p <- p + geom_col(alpha=0.8, fill = "#D16103")
  } else if (continent == "North America"){
    p <- p + geom_col(alpha=0.8, fill = "#C3D7A4")
  } else if (continent == "South America"){
    p <- p + geom_col(alpha=0.8, fill = "#52854C")
  } else if (continent == "Oceania"){
    p <- p + geom_col(alpha=0.8, fill = "#4E84C4")
  } else{
    p <- p + geom_col(alpha=0.8, aes(fill = Continent)) + 
      scale_fill_manual(values = c("#543005", "#dfc27d", "#D16103", "#C3D7A4", "#4E84C4", "#52854C"))
  }
  ggplotly(p, tooltip = "text")
}

proportion(data, "Europe", "kogu aeg")

# \page 2 3rd graph
density <- function(data, n, s) {
  p <- data %>% 
    arrange(desc(s))
  p <- p[c(1:n),] %>% 
    mutate(text = paste("Kontinent: ", Continent,"\nMax magnituud: ", max(EQ.Primary),"\nKesk magnituud: ", round(mean(EQ.Primary), 1),
                        "\nMin magnituud: ", min(EQ.Primary), "", sep=""))
    
  p <- ggplot(p, aes(x = EQ.Primary, fill = Continent, text = text)) +
    geom_density(alpha = 0.5, color = "grey") +
    labs(x = "Magnituud", y = "Tihedus") +
    theme_minimal() +
    labs(fill="Kontinent") +
    scale_fill_manual(values = c("#543005", "#dfc27d", 
                                  "#D16103", "#C3D7A4", "#4E84C4", "#52854C"))
  ggplotly(p, tooltip = "text") %>% 
    layout(legend = list(x = 100, y = 0.5))
}

density(data, 1000, data$EQ.Primary)

#-------------------------------
# APP

ui <- bootstrapPage(
  navbarPage(theme = shinytheme("yeti"), collapsible = T,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">20-21 SAJANDI MAAVÄRINAD</a>'),
             windowTitle = "Projekt",
             tabPanel("Kaart", 
                      div(class="outer",
                        tags$head(includeCSS("styles.css")),
                        leafletOutput("map1", width="100%", height = "100%"), # это не работает хотя если запускать фгнкцию то все ок
                        absolutePanel(id = "controls", top = 30, left = 30, width = 250,
                          selectInput("period", 
                                      label = h4("Vali periood:"),
                                      choices = c("kogu aeg", c("2001-2020","1976-2000","1951-1975","1926-1950","1901-1925")),
                                      selected = "kogu aeg"),
                          selectInput("continent",
                                      label = h4("Vali kontinent:"),
                                      choices = c("kõik kontinendid", sort(unique(data$Continent), decreasing = T)),
                                      selected = "Europe"),
                          radioButtons("layer", "Kaardi tüüp:",
                                             choices = c("vaikimisi", "satelliit"), selected = "vaikimisi"
                          )))),
             
             tabPanel("Üldine statistika",
                      sidebarPanel(id="controls", width = 3,
                                   sliderInput("range", 
                                              label = h4("Maavärinate arv visualiseerimiseks:"),
                                              min = 10, max = nrow(data), value = 10),
                                    selectInput("sort1", 
                                                label = h4("Sorteerimine:"),
                                                choices = c("Magnituud", "Surmad", "Sügavus"),
                                                selected = "Magnituud")),
                      mainPanel(tabsetPanel(type = "tabs",
                                            tabPanel("data", tableOutput("topn")),
                                            tabPanel("Mulldiagramm", plotlyOutput("bubble")),
                                            tabPanel("Histogramm", plotlyOutput("density"))))),
             tabPanel("Kontinendi statistika", 
                      sidebarPanel(id="controls", width = 3,
                                   selectizeInput("graph_period", 
                                        label = h4("Periood:"),
                                        choices = c("kogu aeg", c("2001-2020","1976-2000","1951-1975","1926-1950","1901-1925")),
                                        selected = "kogu aeg"),
                                   selectInput("continent2", 
                                        label = h4("Kontinent:"),
                                        choices = c("kõik kontinendid", unique(data$Continent)),
                                        selected = "Europe")),
               mainPanel(wellPanel(plotlyOutput("proportion"),
                         plotlyOutput("tsunami")))),
             tabPanel("Info",
                      fluidRow(
                        column(width = 6,
                               div(style = "margins:10px;font-size: 35px;text-align: justify;padding-top:25px;padding-left:30px",
                                   " Maavärinad on üks hävitavamaid ja ohtlikumaid loodusnähtusi, mis võivad põhjustada tohutut kahju nii materiaalselt kui ka inimlikult. Need võivad kaasa tuua hoonete, infrastruktuuri ja looduskeskkonna hävimise. Kahjuks kaasnevad maavärinatega sageli ka inimohvrid ja märkimisväärsed kaotused."
                                   
                               ),
                               div(style = "padding-left:30px",
                                   p(tags$a(href="https://www.worldvision.org/disaster-relief-news-stories/2010-haiti-earthquake-facts", "Fotol"),
                                 " on suurim maavärin Haitil 2010. aastal, milles hukkus üle 300 tuhande inimese, sai vigustada ka üle 300 000 ja 1,5 miljonit inimest jättis koduta."))
                        )
                        ,
                        column(width = 6,div(style = "padding-top:20px;padding-right:30px;height:300"
                                             ,
                                             plotOutput("png")
                                             ))
                      )
                      ,
                      div(style = "margin: 20px;",
                          h4("Selle projekti eesmärk on uurida maavärinaid ja visuaalselt esitada nende loodusnähtustega seotud andmeid. Meie eesmärk on paremini mõista maavärinate põhiomadusi, nende levikut riikide ja kontinendite vahel ning tuvastada võimalikke seoseid ja mustreid erinevate tegurite vahel, nagu tugevus, sügavus ja tsunami teke."),
                          p("Selle projekti raames on välja töötatud erinevad graafikud ja tabel, mis aitavad visualiseerida maavärinate andmeid. Projekti põhikomponendid hõlmavad:"),
                          tags$ul(
                            tags$li("Maavärinate kaart: rakenduse leht sisaldab interaktiivset kaarti, mis näitab erinevate ajaperioodide ja kontinentide maavärinaid. Kasutajad saavad andmeid uurida, vahetades erinevate perioodide ja kontinendite vahel."),
                            tags$li("Andmevaade: rakendus võimaldab juurdepääsu töös kasutatud andmetele."),
                            tags$li("Maavärina sügavuse ja tugevuse seoste diagrammid: rakendus esitab maavärina sügavuse ja tugevuse vahelise seose graafilise esituse. See võimaldab teil visuaalselt uurida nende tegurite mõju maavärinate omadustele."),
                            tags$li("Tihedusgraafik: see graafik võimaldab teil hinnata maavärinate suhtelist arvu igas riigis. See näitab maavärinate tihedust erinevates riikides ja aitab tuvastada enim mõjutatud kontinente."),
                            tags$li("Riikidevahelise seose graafikud: rakendus esitab graafikud, mis näitavad riikide vahelist seost maavärinate arvu järgi."),
                            tags$li("Maavärinate arvu histogramm magnituudi ja tsunamide järgi: rakendus esitab ka histogrammi, mis kuvab maavärinate arvu sõltuvalt nende tugevusest. Lisaks näidatakse, kas nende maavärinatega kaasnesid tsunamid.")
                          )
                      ),
                      div(style = "margin: 20px;",
                          p("Antud rakendus töötati välja Tartu Ülikooli õppeaine Statistiline andmeteadus ja visualiseerimine raames (kevad 2023).",
                            p("Töö autorid: Oksana Lihhatsova ja Darja Rohi"),
                            p("Kasutatud andmebaas: ", tags$a(href="https://public.opendatasoft.com/explore/dataset/significant-earthquake-database/table/?sort=deaths", "https://public.opendatasoft.com/explore/dataset/significant-earthquake-database/table/?sort=deaths"))
                          )
                      )))
  
)

server <- function(input, output) {
  output$map1 <- renderLeaflet({
    add_cord(input$period, input$continent, 
                   ifelse(input$layer == "vaikimisi", providers$CartoDB.Positron, "Esri.WorldImagery"))
  })
  output$topn <- function(){
    sort_column <- switch(input$sort1,
                          "Magnituud" = data$EQ.Primary,
                          "Surmad" = data$Total.Effects...Deaths,
                          "Sügavus" = data$Focal.Depth)
    
    tabel1 <- topn(data, sort_column, input$range)
     kable(tabel1) %>%  
       kable_styling(bootstrap_options = c("striped", "hover"))
  }
  output$tsunami <- renderPlotly({
    tsunami(data, input$graph_period, input$continent2)
  })
  output$proportion <- renderPlotly({
    proportion(data, input$continent2, input$graph_period)
  })
  output$bubble <- renderPlotly({
    bubble(data, input$range, switch(input$sort1,
                                       "Magnituud" = data$EQ.Primary,
                                       "Surmad" = data$Total.Effects...Deaths,
                                       "Sügavus" = data$Focal.Depth))
  })
  output$density <- renderPlotly({
    density(data, input$range, switch(input$sort1,
                                        "Magnituud" = data$EQ.Primary,
                                        "Surmad" = data$Total.Effects...Deaths,
                                        "Sügavus" = data$Focal.Depth))
  })
  output$png <- renderPlot({
    pic = readPNG('1.png')
    plot.new()
    grid::grid.raster(pic)

  })
} 

shinyApp(ui = ui, server = server)
