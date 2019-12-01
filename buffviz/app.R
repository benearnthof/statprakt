library(raster)
library(sf)
library(ggmap)
library(shiny)

ui <- fluidPage(

    titlePanel("buffviz"),

    sidebarLayout(
        sidebarPanel(
            numericInput("radius", "Radius", value = 1000, min = 100, max = 15000, step = 1),
            selectInput("dataset", "Daten", choices = c("tabula", "gem_vor", 
                                                       "sla_vor", "gem_lat",
                                                       "sla_lat")),
            checkboxInput("round", "Runden", value = TRUE)
        ),

        mainPanel(
            plotOutput("map", height = "650px", width = "1000px")
          # tableOutput("debug")
        )
    )
)

server <- function(input, output) {
    distinct_inschriften <- readRDS("distinct_inschriften.RDS")
    coordinates(distinct_inschriften) <- ~ lng + lat
    get_coords <- function(x) {
        tmp <- as.character(x)
        tmp <- sub("POINT", "", tmp)
        tmp <- sub("\\(", "", tmp)
        tmp <- sub("\\)", "", tmp)
        list <- stringr::str_split(tmp, " ")
        lng <- as.numeric(sapply(list, `[[`, 1))
        lat <- as.numeric(sapply(list, `[[`, 2))
        list(lng = lng, lat = lat)
    }
    # preprocessing tabula
    tabula <- read.csv("tabula.csv", encoding = "UTF-8", sep = ",")
    x <- tabula$Geodaten
    dta <- x[-length(x)]
    coords_tabula <- get_coords(dta)
    tabula <- tabula[-(490), ]
    tabula$lat = coords_tabula$lat
    tabula$lng = coords_tabula$lng
    coordinates(tabula) <- ~ lng + lat
    
    bufferzone_counter <- function(distance = 100, centerpoints = tabula, points = distinct_inschriften) {
        # crs damit umwandeln zu spatial points funktioniert
        crs <-  CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
        crs(centerpoints) <- crs
        # umwandeln zu spatial points damit sp::over funktioniert
        sp_center <- sp::SpatialPoints(coords = centerpoints@coords, proj4string = crs)
        utm_center <- spTransform(sp_center, CRS("+proj=utm +zone=32 ellps=WGS84"))
        sf_center <- st_as_sf(utm_center)
        # konstruieren von Bufferzonen mit gewünschtem Radius um die Punkte
        center_buff <- st_buffer(sf_center, dist = distance, nQuadSegs = 50)
        sp_polygons_buffer <- sf::as_Spatial(center_buff$geometry)
        crs(points) <- crs
        sp_points <- sp::SpatialPoints(coords = points@coords, proj4string = crs)
        # letzte conversion damit sp::over funktioniert
        sp_points_utm <- spTransform(sp_points, CRS("+proj=utm +zone=32 ellps=WGS84"))
        crs(sp_polygons_buffer) <- CRS("+proj=utm +zone=32 ellps=WGS84")
        # check welche der punkte in welches polygon fallen
        check <- sp::over(sp_points_utm, sp_polygons_buffer)
        check <- check[!is.na(check)]
        # tabelle die angibt welche zeilennummer aus tabula wie viele inschriftenfundorte
        # im festgelegten Radius enthält.
        tbl <- table(check)
        tbl
    }
    
    augment_tabula <- function(buffertable, tab = tabula) {
        tmp <- as.data.frame(buffertable)
        tab@data$Freq <- 0
        tmp$check <- as.numeric(levels(tmp$check))[tmp$check]
        tab@data$Freq[tmp$check] <- tmp$Freq
        tab
    }
    
    plt_tabulamap <- function(augment_res, canvas = insch, title = "Testtitle", round = TRUE) {
        data <- cbind(augment_res@data, augment_res@coords)
        if (round) {
            data$Freq <- round(data$Freq, digits = -1)
        }
        map <- canvas + geom_point(data = data, aes(x = lng, y = lat, size = Freq),
                                   color = "red", alpha = 0.75) + 
            ggtitle(title) +
            labs(size = "test") +
            scale_size(range = c(3, 7))
        
        map
    }
    gem_vor <- readRDS("gem_vor.RDS")
    sla_vor <- readRDS("sla_vor.RDS")
    gem_lat <- readRDS("gem_lat.RDS")
    sla_lat <- readRDS("sla_lat.RDS")
    wrapper <- function(obj) {
        crds <- get_coords(obj$Georeferenz)
        obj$lat = crds$lat
        obj$lng = crds$lng
        coordinates(obj) <- ~ lng + lat
        obj
    }
    
    gem_vor <- wrapper(gem_vor)
    sla_vor <- wrapper(sla_vor)
    gem_lat <- wrapper(gem_lat)
    sla_lat <- wrapper(sla_lat)
    
    insch <- readRDS("canvas.RDS")
    rapper <- function(dist, center, rnd) {
    buffertable <- bufferzone_counter(distance = dist, centerpoints = center)
    res <- augment_tabula(buffertable, tab = center)
    plt_tabulamap(res, round = rnd)
    } 
    output$map <- renderPlot({
        plt <- switch(input$dataset, 
                      "tabula" = {rapper(input$radius, tabula, input$round)}, 
                      "gem_vor" = {rapper(input$radius, gem_vor, input$round)}, 
                      "sla_vor" = {rapper(input$radius, sla_vor, input$round)},
                      "gem_lat" = {rapper(input$radius, gem_lat, input$round)},
                      "sla_lat" = {rapper(input$radius, sla_lat, input$round)},
                      print(NULL)
                      )
        return(plt)
    })
    
    # output$debug <- renderTable({
    #     head(distinct_inschriften@coords, n = 10)
    # })
}

shinyApp(ui = ui, server = server)
