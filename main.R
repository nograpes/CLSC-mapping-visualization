# load packages
library(shiny)
library(tmap)
library(rgdal)
library(leaflet)
library(dplyr)

# load data
mapping_table<-read.csv("mapping_table_cleaned.csv", stringsAsFactors = FALSE)
cd16.shp <- readOGR(dsn = "data/cd", layer = "cd_qc_16", encoding = "UTF-8", stringsAsFactors = FALSE)
csd16.shp <- readOGR(dsn = "data/csd", layer = "csd_qc_16", encoding = "UTF-8", stringsAsFactors = FALSE)
ct16.shp <- readOGR(dsn = "data/ct", layer = "ct_qc_16", encoding = "UTF-8", stringsAsFactors = FALSE)
da16.shp <- readOGR(dsn = "data/da", layer = "da_qc_16", encoding = "UTF-8", stringsAsFactors = FALSE)
clsc.shp <-readOGR(dsn = "data/clsc", layer="Territoires_CLSC_2017", encoding = "UTF-8", stringsAsFactors = FALSE)
clsc.shp<-spTransform(clsc.shp, CRS(proj4string(csd16.shp)))

# define UI
ui <- fluidPage(titlePanel("CLSC Mapping Visualization"), 
              # select clsc to map
              wellPanel(
                  selectInput(inputId = "type", 
                              label = "select by code or name",
                              choices = c("CLSC_code", "CLSC_nom"),
                              selected = "CLSC_code"),
                  uiOutput("option1")),
              
              mainPanel(
                  fluidRow(
                      column ("view in map", width = 10,leafletOutput(outputId = "plot")),
                      column ("View in table", width = 2,tableOutput("table"))
                  )
              )
             
 )

# define the server code
server <- function(input, output){
    output$option1 <- renderUI({
        selectInput(inputId = "option",
                    label = paste("Select", input$type),
                    choices = mapping_table[[input$type]][order(mapping_table[[input$type]])])
    })
    output$plot <- renderLeaflet({
        # find the matching census region
        cd <- as.character(mapping_table$census_id[mapping_table[[input$type]] == input$option & mapping_table$census_type == "CD"])
        csd <- as.character(mapping_table$census_id[mapping_table[[input$type]] == input$option & mapping_table$census_type == "CSD"])
        ct <- as.character(mapping_table$census_id[mapping_table[[input$type]] == input$option & mapping_table$census_type == "CT"])
        da <- as.character(mapping_table$census_id[mapping_table[[input$type]] == input$option & mapping_table$census_type == "DA"])
        
        if (length(cd) >=1) {
            plot <- tm_shape(clsc.shp) +
                tm_polygons(border.col = "black", alpha = 0.1, lwd=1) +
                tm_shape(cd16.shp[cd16.shp$CDUID %in% c(cd),]) +
                tm_polygons(border.col = "red", alpha = 0.2, lwd=2)
        }
        if (length(csd) >=1){
            plot <- tm_shape(clsc.shp) +
                tm_polygons(border.col = "black", alpha = 0.1, lwd=1) +
                tm_shape(csd16.shp[csd16.shp$CSDUID %in% c(csd),])+
                tm_polygons(border.col = "red", alpha = 0.2, lwd=2) 
        }
        if (length(da) >=1){
            plot <- tm_shape(clsc.shp) +
                tm_polygons(border.col = "black", alpha = 0.1, lwd=1) +
                tm_shape(da16.shp[da16.shp$DAUID %in% c(da),])+
                tm_polygons(border.col = "red", alpha = 0.2, lwd=2) 
        }
        if (length(ct) >=1){
            plot <- tm_shape(clsc.shp) +
                tm_polygons(border.col = "black", alpha = 0.1, lwd=1) +
                tm_shape(ct16.shp[ct16.shp$CTUID %in% c(ct),])+
                tm_polygons(border.col = "red", alpha = 0.2, lwd=2) 
        }
    
        tmap_leaflet(plot)
    })
    output$table <- renderTable({mapping_table[mapping_table[[input$type]] == input$option,2:ncol(mapping_table)]})
}

#return a shiny app object
shinyApp (ui = ui, server = server)