#------------------------------------------------------------------------------#
# load libraries  ----
#------------------------------------------------------------------------------#
library(tidyverse)
library(readxl)
library(treemapify)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)
library(janitor)
library(shiny)
library(plotly)

#------------------------------------------------------------------------------#
# load data  ----
#------------------------------------------------------------------------------#

irena <- read_excel(
    'IRENA_Statistics_Extract_2025H2.xlsx',
    sheet = 'Country'
) %>%
    janitor::clean_names()


## get world sf from rnaturalearth
world_sf <- ne_countries(
  #scale = "medium", 
  returnclass = "sf"
) %>%
    mutate( iso3_code  = str_trim(str_to_upper(adm0_a3_us)))

#------------------------------------------------------------------------------#
# Aggregate data for visualizations  ----
#------------------------------------------------------------------------------#
world_sf_data <- world_sf %>%
    as.data.frame() %>%
    select(iso3_code, name_en, pop_est, pop_year, gdp_md, gdp_year, economy, income_grp, continent, subregion, region_un) %>%
    unique()

by_coutry_renew_summ <- irena %>% 
filter(year<2024) %>%
group_by(region, sub_region, country, iso3_code,year, re_or_non_re) %>%
summarise(ttl_generation_g_wh = sum(electricity_generation_g_wh, na.rm = TRUE)) %>%
group_by(region, sub_region, country, iso3_code,year)%>%
mutate(ttl_gen = sum(ttl_generation_g_wh)) %>%
ungroup() %>%
mutate(pct_renewable = ttl_generation_g_wh/ttl_gen) %>%
merge.data.frame(
    world_sf_data,
    by = 'iso3_code',
    all.x = TRUE
)

tree_data_summ <- 
irena %>% 
  filter(year==2023) %>% 
  select(country,re_or_non_re, iso3_code, group_technology, technology,electricity_generation_g_wh) %>%
  group_by(country,iso3_code, re_or_non_re, group_technology, technology) %>%
  summarise(total_electricity_generation_g_wh = sum(electricity_generation_g_wh, na.rm = TRUE)) %>%
  mutate(
  re_or_non_re = fct_reorder(re_or_non_re,total_electricity_generation_g_wh),
  group_technology_nm = ifelse(str_trim(group_technology)=='Hydropower (excl. Pumped Storage)', 'Hydropower', group_technology), 
  group_technology_nm = fct_reorder(group_technology_nm, total_electricity_generation_g_wh),
  size_label = ifelse(technology == group_technology, 0, 7)
)  %>%
  filter(total_electricity_generation_g_wh>0)%>%
merge.data.frame(
    world_sf_data,
    by = 'iso3_code',
    all.x = TRUE
)



## World map of % renewable energy generation 2023 

irena_bc <- irena %>%
    filter(year == 2023) %>%

    group_by(country,iso3_code, region, re_or_non_re) %>%
    summarise(ttl_energy_generation_by_re_non_re = sum(electricity_generation_g_wh, na.rm = TRUE))  %>% 

    group_by(country) %>%
    mutate(ttl_energy_generation_by_country = sum(ttl_energy_generation_by_re_non_re, na.rm = TRUE)) %>% 

    ungroup() %>%
    mutate(pct_renewable = ttl_energy_generation_by_re_non_re/ttl_energy_generation_by_country,
           is_usa = ifelse(
            iso3_code=='USA',TRUE, FALSE
           ),
           iso3_code = str_trim(str_to_upper(iso3_code))
           ) %>%
        
    filter(re_or_non_re=='Total Renewable', 
           !iso3_code %in% c('XOC','XMX','XME','XLA','XDX','XCS','XAS','XAF','XAA')) %>%
    arrange(desc(pct_renewable)) 

world_sf <- world_sf %>%
    merge(
        irena_bc,
        by = 'iso3_code',
        all.x = TRUE
    ) %>%
    mutate(label_text = str_c(
        name_en, 
        ": ", 
        scales::percent(
            pct_renewable, 
            accuracy =.1
            )
        ))


wm <- ggplot(data = world_sf, aes(text = label_text)) +
  geom_sf(aes(fill = pct_renewable), color = "white", linewidth = 0.1) +
  scale_fill_gradient2(name = "% Energy from Renewable Sources", 
                      low = "black", 
                      mid = "white",
                      high = "#265436",
                      guide = guide_colorbar(
                            barwidth = unit(0.5, "cm"), 
                            barheight = unit(5, "cm") ,
                            direction = 'horizontal'
                            ),
                       labels = scales::percent_format()
                    ) + 
  theme_void() +
  labs(
    title = "Click on a country to filter the visualizations below",
    subtitle = "2023",
    caption = "Source: IRENA"
  )+
  theme (legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(face = "italic", size = 6),
        plot.caption = element_text(face = "italic", size = 6),
  )


#------------------------------------------------------------------------------#
# Build app  ----
#------------------------------------------------------------------------------#

ui <- fluidPage(
    
    titlePanel("US Renewable Energy Generation Comparisons"),

    fluidRow(
    column(
        10,
        tags$p(
        HTML("These charts show the percentage of total electricity generation and the composition of energy generation by technology
            that comes from renewable sources, compared to non-renewable sources, 
            for the United States and your selected comparison country. 
            <span style='font-weight:bold;'>Use the map below to select a country to compare with the United States.</span>")
        )
    )
),

    fluidRow(
column(width = 10, align = "center", plotlyOutput("country_map",  width = "75%"))
    ),

    fluidRow(
        column(
            width = 12,
            align = "center",
            tags$h3("Selected Comparison Country:"),
            uiOutput("selected_country_display")
        )
    ),


    br(),

    fluidRow(
        column(10, plotlyOutput("facet_plot", width = "100%"))
        
    ),

    br(),
    
    fluidRow(
        column(10, plotOutput("tree_plot", height = "700px", width = "100%"))
    )
)


server <- function(input, output, session) {

    #-------------------------------
    # Render world choropleth map
    #-------------------------------
    output$country_map <- renderPlotly({
      
        # Add 'key' for capturing clicks
        wm_keyed <- wm +
            aes(key = name_en)
        
        g <- ggplotly(wm_keyed, tooltip = "label_text", source = "world_map") %>%
            layout(
                font = list(family = "Helvetica", size = 12)
            )

        # Adjust colorbar
        for (i in seq_along(g$x$data)) {
            tr <- g$x$data[[i]]
            if (!is.null(tr$marker) && !is.null(tr$marker$colorbar)) {
                g$x$data[[i]]$marker$colorbar$orientation <- "h"
                g$x$data[[i]]$marker$colorbar$x <- 0.5
                g$x$data[[i]]$marker$colorbar$y <- -0.15
                g$x$data[[i]]$marker$colorbar$xanchor <- "center"
                g$x$data[[i]]$marker$colorbar$len <- 0.6
                g$x$data[[i]]$marker$colorbar$thickness <- 10
                g$x$data[[i]]$marker$colorbar$title <- list(text = "% Energy from Renewable Sources", side = "bottom")
            }
        }

        g
    })

    #----------------------------------
    # Reactive value for selected country
    #----------------------------------
    selected_country_rv <- reactiveVal(NULL)

    observeEvent(event_data("plotly_click", source = "world_map"), {
        click <- event_data("plotly_click", source = "world_map")
        if (!is.null(click$key)) {
            selected_country_rv(click$key)
        }
    })

    output$selected_country_display <- renderUI({
    sel <- selected_country_rv()
    
    if (is.null(sel)) {
        # default text before any selection
        tags$span("China", style = "color:#EDAA1C; font-weight:bold; font-size:20px;")
    } else {
        # display the selected country
        tags$span(sel, style = "color:#EDAA1C; font-weight:bold; font-size:20px;")
    }
})

    #----------------------------------
    # Reactive dataset for area/facet plot
    #----------------------------------
    selected_data <- reactive({
        sel <- selected_country_rv()
        if (is.null(sel)) sel <- "People's Republic of China" 
        
        df <- by_coutry_renew_summ %>%
            filter(name_en %in% c("United States of America", sel))
        
        df$name_en <- forcats::fct_relevel(df$name_en, "United States of America", after = 0)

        df
    })

    #----------------------------------
    # Reactive dataset for treemap
    #----------------------------------
    tree_data <- reactive({
        sel <- selected_country_rv()
        if (is.null(sel)) sel <- "People's Republic of China"  # default comparison country
        
        df <- tree_data_summ %>%
            filter(name_en %in% c("United States of America", sel))
        
        df$name_en <- forcats::fct_relevel(df$name_en, "United States of America", after = 0)

        df
    })

    #----------------------------------
    # Render area/facet plot
    #----------------------------------
    output$facet_plot <- renderPlotly({
    facet_ <- ggplot(data = selected_data(), 
                    aes(x = year, 
                        y = pct_renewable, 
                        fill = re_or_non_re)) +
        geom_area(alpha = 0.9) +
        scale_fill_manual(values = c("Total Renewable" = "#265436","Total Non-Renewable"="#5D6770")) +
        scale_y_continuous(labels = scales::percent_format()) +
        scale_x_continuous(breaks = c(2000,2005,2010,2015,2020,2023)) +
        facet_wrap(~name_en) +
        labs(
            x = "",
            y = '% Energy Generation from Renewable Sources',
            title = "Percent of Energy from Renewable Sources",
            caption = "Source: International Renewable Energy Agency"
        ) +
        theme_minimal() +
        theme(
            legend.position = 'none',
            strip.text = element_text(face = "bold"),
            plot.title = element_text(hjust = 0.5, face = 'bold')
        )

    ggplotly(facet_) %>%
        layout(
            font = list(family = "Helvetica", size = 12),
            hovermode = "x unified"
        ) %>%
        style(
            hovertemplate = paste0("<b>%{fullData.name}</b><br>",
                                  "Year: %{x}<br>",
                                  "Renewable: %{y:.1%}<extra></extra>")
        )
})



    #----------------------------------
    # Render treemap plot
    #----------------------------------
    output$tree_plot <- renderPlot({
        tree_ <- ggplot(data = tree_data(),
            aes(
                area = total_electricity_generation_g_wh,
                fill = group_technology_nm,
                subgroup = re_or_non_re,
                subgroup2 = group_technology_nm,
                subgroup3 = technology
            )) +
            geom_treemap(start = 'topleft', alpha = 0.9) +
            geom_treemap_subgroup_border(colour = "white", size = 2) +
            facet_wrap(~name_en, nrow = 2) +
            geom_treemap_subgroup2_text(
                color = "white",
                place = "top",
                reflow = TRUE,
                size = 10,
                fontface = "bold",
                start = 'topleft'
            ) +
            geom_treemap_subgroup3_text(
                aes(size = size_label),
                color = "white",
                place = "center",
                reflow = TRUE,
                start = 'topleft'
            ) +
            scale_fill_manual(values = c(
                "Fossil fuels" = "#1C1C1C",
                "Nuclear" = "#2E2E2E",
                "Other non-renewable energy" = "#4F4F4F",
                "Pumped storage" = "#7A7A7A",
                "Bioenergy" = "#1E8449",
                "Geothermal energy" = "#A9DFBF",
                "Hydropower" = "#0B3D0B",
                "Marine energy" = "#27AE60",
                "Solar energy" = "#7DCEA0",
                "Wind energy" = "#145A32"
            )) +
            labs(
                title = "2023 Renewable Electricity Generation (GWh) by Technology",
                caption = "Source: International Renewable Energy Agency"
            ) +
            theme(
                panel.grid = element_blank(),
                legend.position = "none",
                strip.text = element_text(size = 14, face = "bold", hjust = 0),
                plot.margin = margin(10, 10, 10, 10),
                strip.background = element_blank(),
                plot.title = element_text(hjust = 0.5, face = 'bold')
            )

        tree_
    })

}

shinyApp(ui = ui, server = server)