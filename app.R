library(shiny)
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

# read in cleaned CRMO CBC data
CRMO <- read_csv("CRMO-CBC-2019-cleaned.csv")

## redo with CRMO species
species_list <- CRMO %>%
    distinct(species_latin) %>%
    rename(Especies = species_latin) #%>%
    #arrange(Especies) don't sort alphabetically

##redo with CRMO data
years_list <- CRMO %>%
    distinct(year) %>%
    rename(Year = year) %>%
    arrange(-Year)

year_min_max <- min_max(years_list)


ui <- navbarPage("Conteo Navideño de Aves Monteverde 1994-2019",
                 
                 tabPanel(
                     
                     # App title ----
                     titlePanel(tags$h4("Tendencias por especie")),
                     
                     # Sidebar layout with input and output definitions ----
                     sidebarLayout(
                         
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                             
                            #once I figure out how to toggle betwen scientific and common names this is the control
                             #radioButtons("radio", label = h3("Especies"),
                                        #choices = list("científicos" = "species_latin", "inglés" = "species"), 
                                        #selected = 1),
                           
                             tags$h3("Especies"),
                             
                             # Input: which species ----
                             selectizeInput("species_picked",
                                            multiple = TRUE,
                                            selected = c("Dives dives", "Pharomachrus mocinno", "Falco rufigularis", "Streptoprocne zonaris", "Crax rubra", "Spizaetus ornatus"),
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
                                         value = c(1994, year_min_max[2])),
                             
                             helpText(tags$ul(
                                 tags$li("Tenga en cuenta que los ejes verticales no siempre empiezan en 0"),
                                 
                                 #tags$li("Los datos no incluyen aves avistados únicamente durante los tres días antes o después del conteo (semana de conteo)"),
                                 
                                 #tags$li("El conteo se ha realizao todos los años el 28 de diciembre"),
                                 
                                 #tags$li("No incluye aves no identificadas a nivel de especie"),
                                 
                                 tags$li("Por cambios en taxonomía y nomenclatura algunas especies aparecen bajo nombres científicos desactualizados")
                             
                    
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
                 
                 
                 tabPanel(
                     
                     titlePanel(tags$h4("Especies con mayor detecciones por año")),
                     
                     # Sidebar layout with input and output definitions ----
                     sidebarLayout(
                         
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                             
                             # Input: which year ----
                             selectInput("individual_year_picked",
                                         label = "¿Para cuál año quisiera visualizar los resultados del Conteo Navideño de Aves Monteverde?",
                                         choices = years_list),
                             
                             
                             
                         ),
                         
                         # Main panel for displaying outputs ----
                         mainPanel(
                             
                             # Output: Data table ---- 
                             # can also do dataTableOutput
                             tableOutput(outputId = "count_table")
                             
                         )
                     )
                     
                     
                     
                 )
)

# Define server logic required to draw a plot and table ----
server <- function(input, output) {
    
    # First navbar output ----
    data_input <- reactive({
        
        #update with CRMO variables
        CRMO %>% 
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
                ylab("Inividuos por hora") + #can change to "número/hora"
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
                scale_x_continuous(breaks = seq(1994, 2019, by = 4)) +
                scale_y_continuous(labels = comma)#+
                #scale_color_tanagr(palette_name = "tangara_chilensis")
            
            
        }
        
        plotting_function(data_input())
        
    })
    
    
    # Second navbar output ----
    
    output$count_table <- renderTable({
        
        ##update with CRMO
        CRMO %>%
            filter(year == input$individual_year_picked) %>%
            count(species_latin, how_many_counted) %>%
            select(-n) %>%
            arrange(-how_many_counted) %>%
            filter(how_many_counted > 0) %>%
            mutate(how_many_counted = how_many_counted %>%
                       scales::number(big.mark = ",")) %>%
            rename(Especie = species_latin, `Individuos contados` = how_many_counted)
        
    },
    
    align = "lr")
}

shinyApp(ui = ui, server = server)
