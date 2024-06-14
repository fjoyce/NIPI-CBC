library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(cowplot)

#library(devtools) #only need for installing tanagR or calecopal

#devtools::install_github("an-bui/calecopal")
#library(calecopal)

#devtools::install_github("cdanielcadena/tanagR")
#library(tanagR)

min_max <- function(vector){
    min_max <- c(min(vector), max(vector))
    return(min_max)
}

# read in cleaned CBC data
NIPI <- read.csv("NIPI-CBC-2023-cleaned.csv")

## redo with species
species_list <- NIPI %>%
    distinct(species_latin) %>%
    rename(Especies = species_latin) #%>%
    #arrange(Especies) don't sort alphabetically


years_list <- NIPI %>%
    distinct(year) %>%
    rename(Year = year) %>%
    arrange(-Year)

year_min_max <- min_max(years_list)


#navbarPage is a shiny page layout with a nav bar on top. Title for overall app.
ui <- navbarPage("Conteo Navideño de Aves- Paso del Istmo 2015-2023",
                 
                 tabPanel(
                     
                     # First Tab title ----
                     titlePanel(tags$h4("Tendencias por especie")),
                     
                     # Sidebar layout with input and output definitions ----
                     sidebarLayout(
                         
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                             
                            #once I figure out how to toggle betwen scientific and common names this is the control
                             #radioButtons("radio", label = h3("Especies"),
                                        #choices = list("cientÃ­ficos" = "species_latin", "inglÃ©s" = "species"), 
                                        #selected = 1),
                           
                             tags$h3("Especies"),
                             
                             # Input: which species ----
                             selectizeInput("species_picked",
                                            multiple = TRUE,
                                            selected = c("Eumomota superciliosa", "Amazona auropalliata", "Brotogeris jugularis", "Eupsittula canicularis", "Amazona albifrons", "Amazona autumnalis"),
                                            label = "Puede seleccionar los nombres científicos de la lista o escribirlos (máximo 6):",
                                            choices = species_list,
                                            options = list(maxItems = 6)),
                             
                             
                             #Input: Slider for the number of years ----
                             sliderInput("years_picked",
                                         label = "Años para visualizar:",
                                         step = 1,
                                         sep = "",
                                         min = year_min_max[1],
                                         max = year_min_max[2],
                                         value = c(2015, year_min_max[2])),
                             
                             helpText(tags$ul(
                                 tags$li("Tenga en cuenta que los ejes verticales tienen escalas diferentes"),
                                 
                                 #tags$li("Los datos no incluyen aves avistados Ãºnicamente durante los tres dÃ­as antes o despuÃ©s del conteo (semana de conteo)"),
                                 
                                 tags$li("El conteo se ha realizado durante la primara semana de enero"),
                                 
                                 #tags$li("No incluye aves no identificadas a nivel de especie"),
                                 
                                 tags$li("Por cambios en taxonomía y nomenclatura algunas especies aparecen bajo nombres científicos desactualizados"),
                                 tags$li("Los años en esta página corresponden a la fecha el conteo, no al año de conteo utilizado por Audubon")
                             
                    
                             )
                             ),
                             
                             tags$a(href="https://netapp.audubon.org/CBCObservation/Historical/ResultsByCount.aspx", "Los datos se pueden descargar de la base de datos de Audubon", target="_blank")
                         ),
                         
                         # Main panel for displaying outputs ----
                         mainPanel(
                             
                             # Output: Line graph ----
                             plotOutput(outputId = "time_series_plot", height = "800px")
                             
                         )
                     )
                 ),
                 
                 #Second tab for species ranked by detections by year
                 tabPanel(
                     
                     titlePanel(tags$h4("Especies más detectadas por año")),
                     
                     # Sidebar layout with input and output definitions ----
                     sidebarLayout(
                         
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                             
                             # Input: which year ----
                             selectInput("individual_year_picked",
                                         label = "¿Para cuál año quiere visualizar los resultados del Conteo Navideño de Aves Paso del Istmo?",
                                         choices = years_list),
                             
                             
                             
                         ),
                         
                         # Main panel for displaying outputs ----
                         mainPanel(
                             
                             # Output: Data table ---- 
                             # can also do dataTableOutput
                             tableOutput(outputId = "count_table")
                             
                         )
                     )
                     
                     
                     
                 ),
                 
                 #Third panel for circle map
                 tabPanel(
                     
                     titlePanel(tags$h4("Mapa del círculo")),
                     
                     #in theory no sidebar required?
                     # Main panel for displaying outputs ----
                     mainPanel(
                         
                         leafletOutput("mapa"),
                         p()
                         
                     )
                 )
                     
                     
                     
                 )

# Define server logic required to draw a plot and table ----
server <- function(input, output) {
    
    # First navbar output ----
    data_input <- reactive({
        
        #update with NIPI variables
        NIPI %>% 
            filter(year >= req(input$years_picked[1]),
                   year <= req(input$years_picked[2]),
                   species_latin %in% req(input$species_picked))
        
    })
    
    
    output$time_series_plot <- renderPlot({
        
        #can change y axis to how_many_counted in ggplot aesthetic
        plotting_function <- function(input_for_plot){
            
            input_for_plot %>%
                ggplot(aes(x = year, y = how_many_counted_by_hour, color = species_latin)) +
                geom_line(size = 1) +
                geom_point(size = 2) +
                xlab("Año") +
                ylab("Inividuos por hora") + #can change to "nÃºmero/hora"
                theme_minimal_hgrid()+
                theme(text = element_text(size = 18),
                      legend.position = "none",
                      plot.margin = margin(2, 20, 2, 2)) +
                facet_wrap(vars(species_latin),
                           scales = "free",
                           dir = "v") +
                theme(strip.text = element_text(face = "bold.italic")) + #make facet labels/titles italics             
                #scale_colour_paletteer_c("tanagr::tangara_chilensis") +
                #scale_color_manual(values = cal_palette("sierra1")) +
                scale_x_continuous(breaks = seq(2015, 2023, by = 1)) +
                expand_limits(y=0) +
                scale_y_continuous(labels = comma)#+
                #scale_color_tanagr(palette_name = "tangara_chilensis")
            
            
        }
        
        plotting_function(data_input())
        
    })
    
    
    # Second navbar output ----
    
    output$count_table <- renderTable({
        
        ##update with circle code
        NIPI %>%
            filter(year == input$individual_year_picked) %>%
            count(species_latin, how_many_counted) %>%
            select(-n) %>%
            arrange(-how_many_counted) %>%
            filter(how_many_counted > 0) %>%
            mutate(how_many_counted = how_many_counted %>%
                       scales::number(big.mark = ",")) %>%
            rename(Especie = species_latin, `Individuos contados` = how_many_counted)
        
    },
    
    # Third navbar output ----
    output$mapa <- renderLeaflet({
        leaflet() %>% 
            addProviderTiles("Esri.WorldImagery") %>% 
            addCircles(lng = -85.73,
                       lat = 11.21,
                       weight = 4,
                       radius = 12070,
                       color = "green",
                       fillOpacity = 0,
                       popup = "circulo de conteo") %>% 
            setView(lat = 11.21,
                    lng = -85.73,
                    zoom = 11)
    }),
    
    align = "lr")
}

shinyApp(ui = ui, server = server)
