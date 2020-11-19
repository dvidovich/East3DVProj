library(shiny)
library(leaflet)
library(tidyverse)
library(sf)


##########load data############
#parks
parks <-read.csv("Parks_Locations_and_Features.csv")
#parks <- parks %>% st_as_sf(coords = c("Lon","Lat")) %>% st_set_crs(value = 4326)

#cases
cases <- read.csv("Code_Enforcement_Cases.csv", stringsAsFactors = F)
#cases <- as_tibble(readRDS("~/Data-Viz-2020-Fall/FinalProject/DatVizProj/cases.RData"))

#abandoned props
ab <- st_read('Abandoned_Property_Parcels/Abandoned_Property_Parcels.shp', stringsAsFactors = FALSE)
ab <- ab %>% rename(Outcome = Outcome_St)
ab <- ab[!is.na(ab$Outcome),]

#census
census <- st_read("2010_CensusData/2010_CensusData.shp", stringsAsFactors = FALSE)  
names(census)[names(census)=="SE_T008_00"] <- "Total Population of Census Tracts"
names(census)[names(census)=="SE_T008_01"] <- "Ages under 5yrs"
names(census)[names(census)=="SE_T008_02"] <- "Ages 5 to 9yrs"
names(census)[names(census)=="SE_T008_03"] <- "Ages 10 to 14yrs"
names(census)[names(census)=="SE_T008_04"] <- "Ages 15 to 17yrs"
names(census)[names(census)=="SE_T008_05"] <- "Ages 18 to 34yrs"
names(census)[names(census)=="SE_T008_06"] <- "Ages 25 to 34yrs"
names(census)[names(census)=="SE_T008_07"] <- "Ages 35 to 44yrs"
names(census)[names(census)=="SE_T008_08"] <- "Ages 45 to 54yrs"
names(census)[names(census)=="SE_T008_09"] <- "Ages 55 to 64yrs"
names(census)[names(census)=="SE_T008_10"] <- "Ages 65 to 74yrs"
names(census)[names(census)=="SE_T008_11"] <- "Ages 75 to 84yrs"
names(census)[names(census)=="SE_T008_12"] <- "Ages 85yrs and over"


#make colors 
pal <- colorFactor(palette = 'Set1', domain =parks$Park_Type)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("South Bend"),
    
    tabsetPanel(type = "tabs",
                tabPanel("Parks",
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("types", "Park Types:",
                                            unique(parks$Park_Type), multiple = TRUE),
                                uiOutput("names"),
                                h4("Amenities"),
                                div(style = 'overflow-x: scroll', tableOutput('stats'))
                                ),
                            # Show a plot of the generated distribution
                            mainPanel(
                                leafletOutput(outputId = "map"),
                                column(width = 12,
                                           div(style = 'overflow-x: scroll', tableOutput('table'))
                                )
                            )
                        )
                ),#end of tab
                tabPanel("Census Data",
                         plotOutput(outputId = "plot2"), 
                         hr(),
                         fluidRow(
                             selectInput(inputId = "var", label = "Population Group:",
                                         choices = c("Total Population of Census Tracts",
                                                     "Ages under 5yrs","Ages 5 to 9yrs",
                                                     "Ages 10 to 14yrs","Ages 18 to 34yrs",
                                                     "Ages 18 to 34yrs","Ages 25 to 34yrs",
                                                     "Ages 35 to 44yrs","Ages 45 to 54yrs",
                                                     "Ages 55 to 64yrs","Ages 65 to 74yrs",
                                                     "Ages 75 to 84yrs","Ages 85yrs and over")), 
                             selectInput(inputId = "var2", label = "Select a specific Census Tract",
                                         choices = sort(census$NAMELSAD))
                         )
                         ),
                tabPanel("Abandoned Properties",
                         sidebarLayout(
                        sidebarPanel(
                         selectInput(inputId = 'variable', label = 'Outcome',
                                     choices = c('Deconstructed', 'Demolished',
                                                 'Occupied & Not Repaired', 'Repaired', 'Repaired & Occupied')),
                        ),
                         ##### Show a plot of the generated distribution
                         mainPanel(
                             leafletOutput('map_ab')
                         )
                         )
                ),#end tab
                tabPanel("Code Enforcement Cases",
                         sidebarLayout(
                             sidebarPanel(
                                 selectInput("cases", "Case Type:",
                                             unique(cases$Case_Type_Code_Description), multiple = TRUE)
                             ),
                             mainPanel(
                                 leafletOutput(outputId = "mapCases"),
                                 plotOutput("distPlot")
                             )
                         )
                         ) #end tab 
    
    )
)


server <- function(input, output) {
    #######START PARKS ###########
    selectedTypes <- reactive({
        parks %>% filter(Park_Type %in% input$types)
    })
    
    names <- reactive({
        names <- as.character(selectedTypes()$Park_Name)
    })
    output$names <- renderUI({
        
        selectInput(inputId = "names", label = "Names", choices = names(), multiple = TRUE)
    })
    
    parksData<-reactive(if(length(input$names)>0){
        #get table for selected parks
        ps <- selectedTypes() %>% filter(selectedTypes()$Park_Name %in% input$names) %>% select_if(~sum(!is.na(.)) > 0)
        output$table <- renderTable(ps)
        #count amenities
        output$stats <- renderTable(ps %>% dplyr::select(-Lat, -Lon, -Zip_Code) %>% select_if(is.numeric) %>% replace(is.na(.), 0) %>% summarise_all(funs(sum))  %>% pivot_longer(everything()))
        #get selected parks to map
        return(selectedTypes() %>% filter(selectedTypes()$Park_Name %in% input$names))
    }else{
        #if no park names are picked show all of selected types
        return(selectedTypes())
    })

 
    output$map <-  renderLeaflet({
        leaflet(data= selectedTypes()) %>% addTiles() %>%
            addCircleMarkers(data = parksData(), 
                             popup = paste(parksData()$Park_Type, "<br>", parksData()$Park_Name), color=~pal(parksData()$Park_Type), 
                             fillOpacity = .9, radius=5) 
        
    })
    ######START CENCUS ######
    output$plot2 <-  renderPlot({
        census %>%
            mutate(highlight_flag = ifelse(NAMELSAD == input$var2, T, F)) %>%
            ggplot(aes(x=NAMELSAD, y=.data[[input$var]])) + geom_col(aes(fill = highlight_flag))+
            ggtitle(input$var)+xlab("Name of Census Tract") +ylab("Number of Citizens")+
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,), legend.position="none")+
            scale_fill_manual(values = c('#595959', 'blue'))
        
    })

    ####START CASES###########
    selectedCases <- reactive({
        if (is.null(input$types)){
            rtrn <- cases
        }else{
            rtrn <- cases %>% filter(Case_Type_Code_Description %in% input$cases)
        }
        return(rtrn)
    })
    output$mapCases <-  renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addCircleMarkers(data = selectedCases(), 
                             popup = paste(selectedCases()$Case_Type_Code_Description,
                                           "<br>",
                                           selectedCases()$Case_Status_Code_Description), 
                             fillOpacity = .9, radius=5)
    })
    output$distPlot <- renderPlot({
        hist(x = selectedCases()$Case_Year, col = 'white', border = 'blue')
    })
    
    ####START ABANDONED PROPERTY######
    #### Output Map
    output$map_ab <-  renderLeaflet({
        leaflet(data= ab %>% filter(Outcome == input$variable)) %>%
            addTiles() %>%
            addPolygons()
    })
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
