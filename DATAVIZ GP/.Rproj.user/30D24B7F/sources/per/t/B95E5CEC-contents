library(shiny)
library(ggplot2)
library(dplyr)
library(ggmap)


hotelscatter <- read.csv("TripAdvisorHotelClean.csv")
hotels <- read.csv("hotels.csv")
hotels <- na.omit(hotels)
restaurants <- read.csv("restaurants.csv")
restaurants <- na.omit(restaurants)
metro <- read.csv("metro.csv")
stops <- read.csv("stops.csv")
df <- data.frame()
df$colname <- hotels$colname
###
artwork <- read.csv("artwork.csv")
attraction <- read.csv("attraction.csv")
monument <- read.csv("monument.csv")
museum <- read.csv("museum.csv")
###
clothes <- read.csv("clothes.csv")
shoes <- read.csv("shoes.csv")
supermarket <- read.csv("supermarket.csv")

clusterize <- function(data, botlat, toplat, leftlong, rightlong, k) {
    newdat <- data %>% filter(latitude <= toplat & latitude >= botlat,
                              longitude <= rightlong & longitude >= leftlong)
    pos <- cbind(newdat$latitude, newdat$longitude)
    cluster <- kmeans(pos, k)
    return(cluster$centers)
}
    
    
ui <- navbarPage("Dashboard",
                            tabPanel("Descriptive",
                                     sidebarLayout(
                                         sidebarPanel(h4("Scatterplot Parameters"),
                                             selectInput("xvar",
                                                         label = "x axis variable",
                                                         choices = list("Stars"="Stars.out.of.5", "Price"="Price", 
                                                                        "Rating"="Rating", "Nearby Attractions"="Nearby.Attractions",
                                                                        "Reviews" = "Rating.Count", "Nearby Restaurants"="Nearby.Resaurants")),
                                             selectInput("yvar",
                                                         label = "y axis variable",
                                                         choices = list("Stars"="Stars.out.of.5", "Price"="Price", 
                                                                        "Rating"="Rating", "Nearby Attractions"="Nearby.Attractions",
                                                                        "Reviews" = "Rating.Count", "Nearby Restaurants"="Nearby.Resaurants")),
                                             sliderInput("alpha", label = "Transparency",min = 0, max = 1, value = 1, step = 0.05),
                                             checkboxInput("jitter",
                                                           label = "Add Jittering"),
                                             h4("Smoothing Parameters"),
                                             checkboxInput("smooth",
                                                           label = "Smooth line"),
                                             checkboxInput("confint", "Display Confidence Interval"),
                                             sliderInput("span", label = "Span",min = 0.3, max = 1, value = 1, step = 0.05),
                                             selectInput("method",
                                                         "Method",
                                                         choices = list("Auto"="auto", "Levenberg-Marquardt" = "lm",
                                                                        "Generalized Linear Model" = "glm",
                                                                        "Generalized Additive Model" = "gam",
                                                                        "LOESS" = "loess")),
                                             actionButton("scatter", "Plot")
                                         ),
                                         mainPanel(
                                             plotOutput("scatplot"),
                                             plotOutput("histogram"),
                                             sliderInput("bins", "Number of Bins for Histogram", min = 1, max = 300, value = 30)
                                         )
                                     )),
                 tabPanel("Maps",
                                  sidebarLayout(
                                      sidebarPanel(width = 3,
                                                   
                                                   
                                                   fluidPage(h3("Filters"),
                                                             checkboxInput(inputId = "maphotel", label = "Hotels",value = FALSE),
                                                             checkboxInput(inputId = "maprest", label = "Restaurants",value = FALSE),
                                                             checkboxInput(inputId = "mapmetro", label = "Metros",value = FALSE),
                                                             checkboxInput(inputId = "mapmonument", label = "Monuments",value = FALSE),
                                                             checkboxInput(inputId = "mapsuper", label = "Supermarkets",value = FALSE),
                                                             checkboxInput(inputId = "mapmuseum", label = "Musemums",value = FALSE),
                                                             checkboxInput(inputId = "mapattraction", label = "Attractions",value = FALSE),
                                                             checkboxInput(inputId = "mapstops", label = "Stops",value = FALSE),
                                                             checkboxInput(inputId = "mapclothes", label = "Clothing Stores",value = FALSE),
                                                             checkboxInput(inputId = "mapshoes", label = "Shoe Shops",value = FALSE),
                                                             checkboxInput(inputId = "heatmap", label = "Heatmap",value = FALSE)
                                                             
                                                   )),
                                      mainPanel(width = 9,plotOutput(outputId = "Maps", width = 900, height =700 )))
                                  
                 ),
                 tabPanel("Clustering",
                          fluidRow(
                              column(3,
                                  sliderInput("lat",
                                              label = "Latitude",
                                              min = 40.088,
                                              max = 40.25,
                                              value = c(40.168, 40.194),
                                              step = 0.000001)),
                              column(3,
                                  sliderInput("lon",
                                              label = "Longitude",
                                              min = 44.383,
                                              max = 44.642,
                                              value = c(44.487,44.539),
                                              step = 0.000001)),
                              column(3, 
                                     sliderInput("k",
                                                 label = "Number of Clusters",
                                                 min = 1,
                                                 max = 10,
                                                 value = 5)),
                              column(3, 
                                     selectInput("dataset", 
                                                  label = "Dataset",
                                                  choices = list("Hotels" = 1, "Restaurants" = 2, "Both" = 3),
                                                 selected = "Hotels"))
                              ),
                          plotOutput(outputId = "clustermap"),
                          fluidRow(
                              column(4,
                                     h3("Additional Layers")
                              )
                          ),
                          fluidRow(
                              column(3,
                                     checkboxGroupInput(inputId = "transport", label = "Public Transport", 
                                                        choiceNames = c("Metro", "Bus Stops"),
                                                        choiceValues = c("metro", "stops"))
                                     ),
                              column(3,
                                     checkboxGroupInput(inputId = "tourism", label = "Tourism", 
                                                        choiceNames = c("Attractions", "Artwork", "Monuments", "Museums"),
                                                        choiceValues = c("attraction", "artwork", "monument", "museum"))
                                     ),
                              column(3,
                                     checkboxGroupInput(inputId = "shops", label = "Shopping", 
                                                        choiceNames = list("Shoes", "Clothes", "Supermarkets"),
                                                        choiceValues = list("shoes", "clothes", "supermarket"))
                              ),
                              column(3,
                                     checkboxInput(inputId = "all", label = "Combine All"),
                                     helpText("For better visual quality uncheck all other additional layers")
                              )
                          ),
                          fluidRow(
                              column(3,
                                     numericInput("ktrans", h4("Number of Clusters"),
                                                  min = 1,
                                                  value = 3)
                                     ),
                              column(3,
                                     numericInput("ktour", h4("Number of Clusters"),
                                                  min = 1,
                                                  value = 3)
                                     ),
                              column(3,
                                     numericInput("kshops", h4("Number of Clusters"),
                                                  min = 1,
                                                  value = 3)
                              ),
                              column(3,
                                     numericInput("kall", h4("Number of Clusters"),
                                                  min = 1,
                                                  value = 3)
                              )
                          )
                 ))




server <- function(input, output) {
    
    output$Maps <- renderPlot({
        longlats <- data.frame(x = c(44.478145,44.550805),y = c(40.161538, 40.200461))
        plot2 <-qmplot( x = x,  y =  y , data =longlats  ,source="osm", geom = "blank") +
            {if(input$mapmetro) {geom_point(data = metro, mapping = aes(x=longitude, y=latitude),color = 'darkred', shape = 77, size = 3, stroke = 3)}}+
            {if(input$mapsuper) {geom_point(data = supermarket, mapping = aes(x=longitude, y=latitude),color = 'darkblue', overlaping = T)}}+
            {if(input$mapmonument) {geom_point(data = monument, mapping = aes(x=longitude, y=latitude),color = 'darkgreen')}}+
            {if(input$maphotel) {geom_point(data = hotels, mapping = aes(x=longitude, y=latitude),color = 'black', size = 3)}}+
            {if(input$maprest) {geom_point(data = restaurants[1:50,], mapping = aes(x=longitude, y=latitude),color = 'coral')}}+
            {if(input$mapmuseum) {geom_point(data = museum, mapping = aes(x=longitude, y=latitude),color = 'black')}}+
            {if(input$mapstops) {geom_point(data = stops, mapping = aes(x=longitude, y=latitude),color = 'red')}}+
            {if(input$mapshoes) {geom_point(data = shoes, mapping = aes(x=longitude, y=latitude),color = 'coral4')}}+
            {if(input$mapclothes) {geom_point(data = clothes, mapping = aes(x=longitude, y=latitude),color = 'darkorange')}}+
            {if(input$mapattraction) {geom_point(data = attraction, mapping = aes(x=longitude, y=latitude),color = 'darkorange')}}+
            {if(input$heatmap){geom_density2d_filled(data = rbind({if(input$mapmetro){metro[,3:4]}},
                                                   {if(input$mapsuper){supermarket[,3:4]}},
                                                   {if(input$mapmonument){monument[,3:4]}},
                                                   {if(input$maphotel){hotels[,2:3]}},
                                                   {if(input$maprest){restaurants[,2:3]}},
                                                   {if(input$mapmuseum){museum[,3:4]}},
                                                   {if(input$mapstops){stops[,3:4]}},
                                                   {if(input$mapshoes){shoes[,3:4]}},
                                                   {if(input$mapclothes){clothes[,3:4]}},
                                                   {if(input$mapattraction){attraction[,3:4]}}), mapping = aes(x=longitude, y=latitude), 
                                      alpha = 0.3)
            }}
        
        plot2
        
    })
    
    scatter <- observeEvent(input$scatter, {
        output$scatplot <- renderPlot({
            set.seed(1)
            ggplot(data = hotelscatter)+labs(title = "Scatterplot", x="Variable X", y="Variable Y")+
                geom_point(mapping=aes(x=eval(parse(text=input$xvar)), y = eval(parse(text=input$yvar))), 
                           position = {ifelse(input$jitter, "jitter", "identity")},
                           alpha = input$alpha)+
                {if(input$smooth){
                    geom_smooth(mapping=aes(x=eval(parse(text=input$xvar)), y = eval(parse(text=input$yvar))),
                                span = input$span, se = input$confint, method = input$method)
                }}
        })
        output$histogram <- renderPlot({
            {if(input$xvar %in% list("Price", "Nearby.Attractions", "Nearby.Resaurants", "Rating.Count")){
                    ggplot(data = hotelscatter)+labs(title = "Histogram", x="Variable X", y="Count")+
                geom_histogram(mapping = aes(x=eval(parse(text=input$xvar))), bins = input$bins)} else{
                    ggplot(data = hotelscatter)+labs(title = "Count Plot", x="Variable X", y="Count")+
                        stat_count(mapping = aes(x=eval(parse(text=input$xvar))), bins = input$bins)  
                }}
            
        }) 
    })
    
    
    
    output$clustermap <- renderPlot({
        #Main Cluster####
        df <- hotels
        if(input$dataset == ""){ 
            return(NULL)
        }else{
            if(input$dataset == 1) df <- hotels
            if(input$dataset == 2) df <- restaurants
            if(input$dataset == 3) df <- rbind(hotels, restaurants)
            
            df <- df %>% filter(longitude<=input$lon[2] & longitude>=input$lon[1], latitude<=input$lat[2] & latitude>=input$lat[1])
            longlat <- cbind(df$longitude, df$latitude)
            set.seed(1)
            clusters <- kmeans(longlat, centers = input$k)
            }
        

        
        #Additional Clusters####
        if("metro" %in% input$transport || 'stops' %in% input$transport) {
            trans <- bind_rows(lapply(input$transport, function(x) eval(parse(text=x))))
            transclust <- clusterize(trans, input$lat[1], input$lat[2], input$lon[1], input$lon[2], input$ktrans)
        }
        
        if(length(input$tourism) != 0) {
            tour <- bind_rows(lapply(input$tourism, function(x) eval(parse(text=x))))
            tourclust <- clusterize(tour, input$lat[1], input$lat[2], input$lon[1], input$lon[2], input$ktour)
        }
        
        if(length(input$shops) != 0) {
            shop <- bind_rows(lapply(input$shops, function(x) eval(parse(text=x))))
            shopsclust <- clusterize(shop, input$lat[1], input$lat[2], input$lon[1], input$lon[2], input$kshops)
        }
        
        if(input$all) {
            all <- rbind(metro, stops, attraction, artwork, museum, monument, clothes, shoes, supermarket)
            allclust <- clusterize(all, input$lat[1], input$lat[2], input$lon[1], input$lon[2], input$kall)
        }
        
        plot1 <- qmplot(x=longitude, y=latitude, data=df, source="osm")+
            geom_point(mapping=aes(color=as.factor(clusters$cluster)), size = 2)+
            geom_point(data = as.data.frame(clusters$centers), 
                       mapping=aes(x=clusters$centers[,1], 
                                   y=clusters$centers[,2]), shape = 8, size = 7)+
            geom_text(data = as.data.frame(clusters$centers), 
                      mapping=aes(x=clusters$centers[,1], 
                                  y=clusters$centers[,2]), label = "Dataset", vjust = -1)+
            #Transport####
            {
                if(length(input$transport) != 0) {
                    geom_point(data = as.data.frame(transclust),
                               mapping = aes(x=transclust[,2],
                                             y=transclust[,1]),
                               size = 7, shape = 13,color = "darkblue")}}+
            {
                if(length(input$transport) != 0) {
                    geom_text(data = as.data.frame(transclust),
                              mapping = aes(x=transclust[,2],
                                            y=transclust[,1], label = "Transport"), vjust=-1)}
            }+
            #Tourism####
            {
                if(length(input$tourism) != 0) {
                    geom_point(data = as.data.frame(tourclust),
                               mapping = aes(x=tourclust[,2],
                                             y=tourclust[,1]),
                               size = 7, shape = 23,color = "darkred")
                }
            }+
            {
                if(length(input$tourism) != 0) {
                    geom_text(data = as.data.frame(tourclust),
                              mapping = aes(x=tourclust[,2],
                                            y=tourclust[,1], label = "Tourism"), vjust=-1)}
            }+
            #Shops####
            {
                if(length(input$shops) != 0) {
                    geom_point(data = as.data.frame(shopsclust),
                               mapping = aes(x=shopsclust[,2],
                                             y=shopsclust[,1]),
                               size = 7, shape = 11,color = "darkgreen")
                }
            }+
            {
                if(length(input$shops) != 0) {
                    geom_text(data = as.data.frame(shopsclust),
                              mapping = aes(x=shopsclust[,2],
                                            y=shopsclust[,1], label = "Shops"), vjust=-1)}
            }+
            #All Layers####
            {
                if(input$all) {
                    geom_point(data = as.data.frame(allclust),
                               mapping = aes(x=allclust[,2],
                                             y=allclust[,1]),
                               size = 7, shape = 7 ,color = "brown")
                }
            }+
            {
                if(input$all) {
                    geom_text(data = as.data.frame(allclust),
                              mapping = aes(x=allclust[,2],
                                            y=allclust[,1], label = "Combined"), vjust=-1)}
            }+
            theme(legend.position = "none")
        
        plot1
    })
    
    
    #output$test <- renderPrint({input$data})
}




shinyApp(ui = ui, server = server)

