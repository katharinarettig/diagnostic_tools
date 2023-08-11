# Loading required packages

options(repos=BiocManager::repositories()) 
library(shiny)
library(radarchart)
load(file = "data_for_shiny.Rdata",verbose = F)
attach(variables_for_shiny)
print(labs_SRPAR)
print(labs_SLMR)
print(labs_RLMR)
library(shinyBS)
library(fmsb)
library(scales)
#library(gRain)
library(gRbase)
library(openxlsx)
library(shinythemes)

# App Layout (one window) -----------------------------------------------------

ui <- fluidPage(
  theme=shinytheme("spacelab"),
  tags$style(HTML(".tabbable > .nav > li > a {color:#737373}
              .tabbable > .nav > li[class=active]    > a {color:#0099FF}")), 
  navbarPage(
    "Diagnostic Tools",
    
    # Tab: diagnostic tool SRPAR ------------------------------------------------
    
    tabPanel(title_panel_SRPAR,
             fluidRow(
               
               # Layout interactive selection bar
               
               column(width = 2, 
                      style = "background-color:#E5E5E5;",
                      h5("Selection of biological metrics",
                         style = "font-size:15px;font-weight:bold;"),
                      lapply(1:length(evidence_SRPAR), function(i) {
                        selectInput(inputId = names(evidence_SRPAR[i]), 
                                    
                                    # ? symbols(help entries) for metrics
                                    
                                    label =  h4(input_description_SRPAR[i],style = "font-size:13px;font-weight:bold;",
                                                bsButton(inputId = paste('q_SRPAR',names(evidence_SRPAR)[i],sep = "_"),
                                                         label = "",
                                                         icon = icon("question"),
                                                         style = "info",
                                                         size = "extra-small")
                                    ),
                                    choices = input_choices_SRPAR[[i]],
                                    selected = NA)
                      }),
                      lapply(1:length(evidence_SRPAR), function(i) {
                        bsPopover(id = paste('q_SRPAR',names(evidence_SRPAR)[i],sep = "_"),
                                  title = "",
                                  content = input_help_SRPAR[i],
                                  placement = "right",
                                  trigger = "focus",
                                  options = list(container = "body")
                        )
                        
                      }),
                      
                      
                      
                      br(),
                      
                      # Slider for % display in selection bar
                      
                      sliderInput(inputId = "threshold_SRPAR",
                                  label = h5("Adjustment of %-scale", style = "font-size:13px;font-weight:bold;",
                                             bsButton(inputId = "q_slider_SRPAR",label = "",icon = icon("question"),style = "info", size = "extra-small")
                                  ),
                                  
                                  min = min_threshold_scale_SRPAR, max = max_threshold_scale_SRPAR, value = default_threshold_scale_SRPAR ,step = 1,animate = T,post = "%"),
                      bsPopover(id = "q_slider_SRPAR", title = "",
                                
                                # ? slider icon
                                
                                content = paste0(p("Moving the slider to the right or left adjusts the %-display.")
                                ),
                                placement = "right", 
                                trigger = "focus", 
                                options = list(container = "body")
                      )
               ),
               
               # Output: text describing the diagnostic tool 
               
               column(width = 10,
                      tabsetPanel(
                        tabPanel("Stressor overview",
                                 column(width = 4,#style = "background-color:#D3D4FF;",
                                        includeHTML(path = "html_files/SRPAR_model_description_dia.html"),style="text-align:justif;"
                                        
                                 ),
                                 
                                 # Output: radar plot
                                 
                                 column(width = 8, #style = "background-color:#D5D6CE;"
                                        plotOutput("radar_SRPAR",height = "800px",width = "100%",
                                                   click = "radarclick_SRPAR"),
                                        bsModal(id = "more_info_SRPAR", title = "" , trigger = "radar_SRPAR", size = "large",
                                                htmlOutput("explanation_SRPAR")),
                                        tags$head(tags$style("#more_info_SRPAR .modal-footer{display: none}")),
                                 )
                        ),
                        
                        # Tab: Stressor hierarchy
                        
                        tabPanel("Stressor hierarchy",
                                 column (width = 4,
                                         h3("Increase in probability"),
                                         tableOutput('table_SRPAR')),
                                 column (width =8,
                                         h3(""),
                                         includeHTML(path = 'html_files/hierarchy_dia.html'))
                        ),
                        
                        # Tab: download
                        
                        tabPanel("Download",
                                 h3("Download of stressor overview and hierarchy"),
                                 includeHTML(path = "html_files/download_dia.html"),
                                 downloadButton('downloadOverview_SRPAR', 'Stressor_overview.png'), br(),br(),
                                 downloadButton('downloadHierarchy_SRPAR', 'Stressor_hierarchy.xlsx'), downloadButton('downloadHierarchy_SRPARcsv', 'Stressor_hierarchy.csv')
                        ),
                        
                        # Tab: further information
                        
                        tabPanel("Further information",
                                 h3("Further information on the diagnostic approach"),
                                 includeHTML(path = "html_files/readmore_dia.html")
                        )
                      )
               )
             )
             ),
    
    # Tab: diagnostic tool SLMR ------------------------------------------------
    
    tabPanel(title_panel_SLMR,
             fluidRow( 
               
               # Layout interactive selection bar 
               
               column(width = 2, 
                      style = "background-color:#E5E5E5;",
                      h5("Selection of biological metrics",
                         style = "font-size:15px;font-weight:bold;"),
                      lapply(1:length(evidence_SLMR), function(i) {
                        selectInput(inputId = names(evidence_SLMR[i]), 
                                    
                                    # ? symbols(help entries) for metrics
                                    
                                    label =  h4(input_description_SLMR[i],style = "font-size:13px;font-weight:bold;",
                                                bsButton(inputId = paste('q_SLMR',names(evidence_SLMR)[i],sep = "_"),
                                                         label = "",
                                                         icon = icon("question"),
                                                         style = "info",
                                                         size = "extra-small")
                                    ),
                                    choices = input_choices_SLMR[[i]],
                                    selected = NA)
                      }),
                      lapply(1:length(evidence_SLMR), function(i) {
                        bsPopover(id = paste('q_SLMR',names(evidence_SLMR)[i],sep = "_"),
                                  title = "",
                                  content = input_help_SLMR[i],
                                  placement = "right",
                                  trigger = "focus",
                                  options = list(container = "body")
                        )
                        
                      }),
                      
                      
                      
                      br(),
                      
                      # Slider for % display in selection bar
                      
                      sliderInput(inputId = "threshold_SLMR",
                                  label = h5("Adjustment of %-scale", style = "font-size:13px;font-weight:bold;",
                                             bsButton(inputId = "q_slider_SLMR",label = "",icon = icon("question"),style = "info", size = "extra-small")
                                  ),
                                  
                                  min = min_threshold_scale_SLMR, max = max_threshold_scale_SLMR, value = default_threshold_scale_SLMR ,step = 1,animate = T,post = "%"),
                      bsPopover(id = "q_slider_SLMR", title = "",
                                
                                # ? slider icon
                                
                                content = paste0(p("Moving the slider to the right or left adjusts the %-display.")
                                ),
                                placement = "right", 
                                trigger = "focus", 
                                options = list(container = "body")
                      )
               ),
               
               # Output: text describing the diagnostic tool
               
               column(width = 10,
                      tabsetPanel(
                        tabPanel("Stressor overview",
                                 column(width = 4,#style = "background-color:#D3D4FF;",
                                        includeHTML(path = "html_files/SLMR_model_description_dia.html"),style="text-align:justif;"
                                        
                                 ),
                                 
                                 # Output: radar plot
                                 
                                 column(width = 8, #style = "background-color:#D5D6CE;"
                                        plotOutput("radar_SLMR",height = "800px",width = "100%",
                                                   click = "radarclick_SLMR"),
                                        bsModal(id = "more_info_SLMR", title = "" , trigger= "radar_SLMR", size = "large",
                                                htmlOutput("explanation_SLMR")),
                                        tags$head(tags$style("#more_info_SLMR .modal-footer{display: none}")),
                                 )
                        ),
                        
                        # Tab: Stressor hierarchy
                      
                        tabPanel("Stressor hierarchy",
                                 column (width = 4,
                                         h3("Increase in probability"),
                                         tableOutput('table_SLMR')),
                                 column (width =8,
                                         h3(""),
                                         includeHTML(path = 'html_files/hierarchy_dia.html'))
                        ),
                        
                        # Tab: download
                        
                        tabPanel("Download",
                                 h3("Download of stressor overview and hierarchy"),
                                 includeHTML(path = "html_files/download_dia.html"),
                                 downloadButton('downloadOverview_SLMR', 'Stressor_overview.png'), br(),br(),
                                 downloadButton('downloadHierarchy_SLMR', 'Stressor_hierarchy.xlsx'), downloadButton('downloadHierarchy_SLMRcsv', 'Stressor_hierarchy.csv')
                                 
                        ),
                        
                        # Tab: further information
                        
                        tabPanel("Further information",
                                 h3("Further information on the diagnostic approach"),
                                 includeHTML(path = "html_files/readmore_dia.html")
                        )
                      )
               )
             )
    ),
    
    # Tab: diagnostic tool RLMR ------------------------------------------------
    
    tabPanel(title_panel_RLMR,
             fluidRow( 
               
               # Layout interactive selection bar
               
               column(width = 2, 
                      style = "background-color:#E5E5E5;",
                      h5("Selection of biological metrics",
                         style = "font-size:15px;font-weight:bold;"),
                      lapply(1:length(evidence_RLMR), function(i) {
                        selectInput(inputId = names(evidence_RLMR[i]), 
                                    
                                    # ? symbols(help entries) for metrics
                                    
                                    label =  h4(input_description_RLMR[i],style = "font-size:13px;font-weight:bold;",
                                                bsButton(inputId = paste('q_RLMR',names(evidence_RLMR)[i],sep = "_"),
                                                         label = "",
                                                         icon = icon("question"),
                                                         style = "info",
                                                         size = "extra-small")
                                    ),
                                    choices = input_choices_RLMR[[i]],
                                    selected = NA)
                      }),
                      lapply(1:length(evidence_RLMR), function(i) {
                        bsPopover(id = paste('q_RLMR',names(evidence_RLMR)[i],sep = "_"),
                                  title = "",
                                  content = input_help_RLMR[i],
                                  placement = "right",
                                  trigger = "focus",
                                  options = list(container = "body")
                        )
                        
                      }),
                      
                      
                      
                      br(),
                      
                      # Slider for % display in selection bar
                      
                      sliderInput(inputId = "threshold_RLMR",
                                  label = h5("Adjustment of %-scale", style = "font-size:13px;font-weight:bold;",
                                             bsButton(inputId = "q_slider_RLMR",label = "",icon = icon("question"),style = "info", size = "extra-small")
                                  ),
                                  
                                  min = min_threshold_scale_RLMR, max = max_threshold_scale_RLMR, value = default_threshold_scale_RLMR ,step = 1,animate = T,post = "%"),
                      bsPopover(id = "q_slider_RLMR", title = "",
                                
                                # ? slider icon
                                
                                content = paste0(p("Moving the slider to the right or left adjusts the %-display.")
                                ),
                                placement = "right", 
                                trigger = "focus", 
                                options = list(container = "body")
                      )
               ),
               
               # Output: text describing the diagnostic tool
               
               column(width = 10,
                      tabsetPanel(
                        tabPanel("Stressor overview",
                                 column(width = 4,#style = "background-color:#D3D4FF;",
                                        includeHTML(path = "html_files/RLMR_model_description_dia.html"),style="text-align:justif;"
                                        
                                 ),
                                 
                                 # Output: radar plot 
                                 
                                 column(width = 8, #style = "background-color:#D5D6CE;"
                                        plotOutput("radar_RLMR",height = "800px",width = "100%",
                                                   click = "radarclick_RLMR"),
                                        bsModal(id = "more_info_RLMR", title = "" , trigger = "radar_RLMR", size = "large",
                                                htmlOutput("explanation_RLMR")),
                                        tags$head(tags$style("#more_info_RLMR .modal-footer{display: none}")),
                                 )
                        ),
                        
                        # Tab: Stressor hierarchy 
                        
                        tabPanel("Stressor hierarchy",
                                 column (width = 4,
                                         h3("Increase in probability"),
                                         tableOutput('table_RLMR')),
                                 column (width =8,
                                         h3(""),
                                         includeHTML(path = 'html_files/hierarchy_dia.html'))
                        ),
                        
                        # Tab: download
                        
                        tabPanel("Download",
                                 h3("Download of stressor overview and hierarchy"),
                                 includeHTML(path = "html_files/download_dia.html"),
                                 downloadButton('downloadOverview_RLMR', 'Stressor_overview.png'), br(),br(),
                                 downloadButton('downloadHierarchy_RLMR', 'Stressor_hierarchy.xlsx'),  downloadButton('downloadHierarchy_RLMR.csv', 'Stressor_hierarchy.csv')
                                 #downloadButton('downloadPDF', 'Download')
                        ),
                        
                        # Tab: further information
                        
                        tabPanel("Further information",
                                 h3("Further information on the diagnostic approach"),
                                 includeHTML(path = "html_files/readmore_dia.html")
                        )
                      )
               )
             )
    )
  ),style = "min-width: 80em;"
)
    
    
# Server logic -----------------------------------------------------------------

    server <- function(input, output) {
      
      # SRPAR - Posteriors
      
      posteriors_SRPAR <- reactive({
        
        # Selection of metrics
        
        for(ei in 1:length(evidence_SRPAR)){
          evidence_SRPAR[[ei]] <- input[[names(evidence_SRPAR)[ei]]]
        }
 
        # Calculation of posteriors
        
        post_SRPAR <- gRain::querygrain(object = grain_net_SRPAR, nodes = nodes_for_observation_SRPAR, 
                                      type = "marginal", evidence = evidence_SRPAR)
        post_SRPAR <- post_SRPAR[nodes_for_observation_SRPAR]

        return(post_SRPAR)  
      })
      
      # SLMR - Posteriors
      
      posteriors_SLMR <- reactive({
        
        # Selection of metrics
        
        for(ei in 1:length(evidence_SLMR)){
          evidence_SLMR[[ei]] <- input[[names(evidence_SLMR)[ei]]]
        }
        
        # Calculation of posteriors
        
        post_SLMR <- gRain::querygrain(object = grain_net_SLMR, nodes = nodes_for_observation_SLMR, 
                                      type = "marginal", evidence = evidence_SLMR)
        post_SLMR <- post_SLMR[nodes_for_observation_SLMR]
        
        return(post_SLMR)  
      })
      
      # RLMR - Posteriors
      
      posteriors_RLMR <- reactive({
        
        # Selection of metrics
        
        for(ei in 1:length(evidence_RLMR)){
          evidence_RLMR[[ei]] <- input[[names(evidence_RLMR)[ei]]]
        }
        
        # Calculation of posteriors
        
        post_RLMR <- gRain::querygrain(object = grain_net_RLMR, nodes = nodes_for_observation_RLMR, 
                                      type = "marginal", evidence = evidence_RLMR)
        post_RLMR <- post_RLMR[nodes_for_observation_RLMR]
        
        return(post_RLMR)  
      })
      
      # SRPAR - radar plot
     
      output$radar_SRPAR <- renderPlot({
        plot_radar_chart(prior = priors_SRPAR, post = posteriors_SRPAR(),
                         pscale = input$threshold_SRPAR/100,for_plot = for_plot_SRPAR,
                         rchartlabs = rchartlabs_SRPAR, col = rgb(0/256,61/256,144/256))
      })
      
      # SLMR - radar plot
      
      output$radar_SLMR <- renderPlot({
        plot_radar_chart(prior = priors_SLMR, post = posteriors_SLMR(),
                         pscale = input$threshold_SLMR/100,for_plot = for_plot_SLMR,
                         rchartlabs = rchartlabs_SLMR, col = rgb(0/256,61/256,144/256))
      })
      
      # RLMR - radar plot
      
      output$radar_RLMR <- renderPlot({
        plot_radar_chart(prior = priors_RLMR, post = posteriors_RLMR(),
                         pscale = input$threshold_RLMR/100,for_plot = for_plot_RLMR,
                         rchartlabs = rchartlabs_RLMR, col = rgb(0/256,61/256,144/256))
      })
      
      # SRPAR - hierarchy
      
      output$table_SRPAR <- renderTable({
        table_radar_chart(prior = priors_SRPAR, post = posteriors_SRPAR(), pscale = input$threshold_SRPAR/100,
                          for_plot = for_plot_SRPAR, rchartlabs = rchartlabs_SRPAR, 
                          columnames = c("Potential stressor","Increase [%]"))
      },digits = 1)
      
      # SLMR - hierarchy 
      
      output$table_SLMR <- renderTable({
        table_radar_chart(prior = priors_SLMR, post = posteriors_SLMR(), pscale = input$threshold_SLMR/100,
                          for_plot = for_plot_SLMR, rchartlabs = rchartlabs_SLMR, 
                          columnames = c("Potential stressor","Increase [%]"))
      },digits = 1)
      
      # RLMR - hierarchy
      
      output$table_RLMR <- renderTable({
        table_radar_chart(prior = priors_RLMR, post = posteriors_RLMR(), pscale = input$threshold_RLMR/100,
                          for_plot = for_plot_RLMR, rchartlabs = rchartlabs_RLMR, 
                          columnames = c("Potential stressor","Increase [%]"))
      },digits = 1)
      
      # SRPAR - reaction to selection of metrics
      
      lab_for_modal_SRPAR <- eventReactive(input$radarclick_SRPAR, {
        rad_click_SRPAR <- (input$radarclick_SRPAR)
        labs_click_SRPAR <- clicked_lab(rad_click = rad_click_SRPAR,labs = labs_SRPAR)
      })
      
      # SLMR - reaction to selection of metrics
      
      lab_for_modal_SLMR <- eventReactive(input$radarclick_SLMR, {
        rad_click_SLMR <- (input$radarclick_SLMR)
        labs_click_SLMR <- clicked_lab(rad_click = rad_click_SLMR,labs = labs_SLMR)
      })
      
      # RLMR - reaction to selection of metrics
      
      lab_for_modal_RLMR <- eventReactive(input$radarclick_RLMR, {
        rad_click_RLMR <- (input$radarclick_RLMR)
        labs_click_RLMR <- clicked_lab(rad_click = rad_click_RLMR,labs = labs_RLMR)
      })
      
      # SRPAR - Pop-up windows with information on each potential degradation cause
      
      output$explanation_SRPAR <- renderUI({
        rad_click_SRPAR <- (input$radarclick_SRPAR)
        labs_click_SRPAR <- clicked_lab(rad_click = rad_click_SRPAR,labs = labs_SRPAR)
        tagList(
          h2(rchartlabs_SRPAR[which(labs_click_SRPAR)]),
          br(),
          
          tags$table( class = "image",
                      align = "right", 
                      style = "max-width:350px;max-height:350px;padding: 20px 10px 10px 10px;",
                      tags$caption(popup_plot_textandimage_SRPAR[which(labs_click_SRPAR),3],
                                   align="bottom",
                                   style = "text-align: left; font-size:x-small;padding: 1px 10px 10px 10px"
                      ),
                      
                      tags$tr(
                        tags$td(
                          img(
                            src=popup_plot_textandimage_SRPAR[which(labs_click_SRPAR),2], 
                            align = "right", 
                            style = "max-width:350px;max-height:350px;padding: 20px 10px 10px 10px;"
                          )
                        )
                      )
          ),
          
          includeHTML(paste("html_files/",popup_plot_textandimage_SRPAR[which(labs_click_SRPAR),1],sep = "")),
        )})
      
      # SLMR - Pop-up windows with information on each potential degradation cause
      
      output$explanation_SLMR <- renderUI({
        rad_click_SLMR <- (input$radarclick_SLMR)
        labs_click_SLMR <- clicked_lab(rad_click = rad_click_SLMR,labs = labs_SLMR)
        tagList(
          h2(rchartlabs_SLMR[which(labs_click_SLMR)]),
          br(),
          
          tags$table( class = "image",
                      align = "right", 
                      style = "max-width:350px;max-height:350px;padding: 20px 10px 10px 10px;",
                      tags$caption(popup_plot_textandimage_SLMR[which(labs_click_SLMR),3],
                                   align="bottom",
                                   style = "text-align: left; font-size:x-small;padding: 1px 10px 10px 10px;"
                      ),
                      
                      tags$tr(
                        tags$td(
                          img(
                            src=popup_plot_textandimage_SLMR[which(labs_click_SLMR),2], 
                            align = "right", 
                            style = "max-width:350px;max-height:350px;padding: 20px 10px 10px 10px;"
                          )
                        )
                      )
          ),
          
          includeHTML(paste("html_files/",popup_plot_textandimage_SLMR[which(labs_click_SLMR),1],sep = "")),
         )})
      
      # RLMR - Pop-up windows with information on each potential degradation cause
      
      output$explanation_RLMR <- renderUI({
        rad_click_RLMR <- (input$radarclick_RLMR)
        labs_click_RLMR <- clicked_lab(rad_click = rad_click_RLMR,labs = labs_RLMR)
        tagList(
          h2(rchartlabs_RLMR[which(labs_click_RLMR)]),
          br(),
          
          tags$table( class = "image",
                      align = "right", 
                      style = "max-width:350px;max-height:350px;padding: 20px 10px 10px 10px;",
                      tags$caption(popup_plot_textandimage_RLMR[which(labs_click_RLMR),3],
                                   align="bottom",
                                   style = "text-align: left; font-size:x-small;padding: 1px 10px 10px 10px;"
                      ),
                      
                      tags$tr(
                        tags$td(
                          img(
                            src=popup_plot_textandimage_RLMR[which(labs_click_RLMR),2], 
                            align = "right", 
                            style = "max-width:350px;max-height:350px;padding: 20px 10px 10px 10px;"
                          )
                        )
                      )
          ),
          
          includeHTML(paste("html_files/",popup_plot_textandimage_RLMR[which(labs_click_RLMR),1],sep = "")),
          )})
      
      # SRPAR - radar plot
      
      output$plot_SRPAR <- renderPlot({
        rad_click_SRPAR <- (input$radarclick_SRPAR)
        labs_click_SRPAR <- clicked_lab(rad_click = rad_click_SRPAR,labs = labs_SRPAR)
        },width = 450, height =200)
      
      # SLMR - radar plot
      
      output$plot_SLMR <- renderPlot({
        rad_click_SLMR <- (input$radarclick_SLMR)
        labs_click_SLMR <- clicked_lab(rad_click = rad_click_SLMR,labs = labs_SLMR)
        },width = 450, height =200)
      
      # RLMR - radar plot
      
      output$plot_RLMR <- renderPlot({
        rad_click_RLMR <- (input$radarclick_RLMR)
        labs_click_RLMR <- clicked_lab(rad_click = rad_click_RLMR,labs = labs_RLMR)
        },width = 450, height =200)
      
      # SRPAR - Download
      
      # Stressor overview (radar plot)
      
      plotInput_SRPAR <- function(){
        plot_radar_chart(prior = priors_SRPAR, post = posteriors_SRPAR(), pscale = input$threshold_SRPAR/100,
                         for_plot = for_plot_SRPAR, rchartlabs = rchartlabs_SRPAR, col = rgb(0/256,61/256,144/256))
      }
      
      output$downloadOverview_SRPAR <- downloadHandler(
        filename = "Stessor_overview.png",
        content = function(file) {
          png(file,width = 800, height = 800, units = "px")
          plotInput_SRPAR()
          dev.off()
        })
      
      tableInput_SRPAR <- function(){
        table_radar_chart(prior = priors_SRPAR, post = posteriors_SRPAR(), pscale = input$threshold_SRPAR/100,
                          for_plot = for_plot_SRPAR, rchartlabs = rchartlabs_SRPAR, columnames = c("Potential stressor","Increase [%]"))
        }
      
      # Stressor hierarchy (table)
      
      output$downloadHierarchy_SRPAR <- downloadHandler(
        filename = "Stessor_hierarchy.xlsx",
        content = function(file) {
          write.xlsx(tableInput_SRPAR(), file, fileEncoding = 'WINDOWS-1252', row.names = F)
        })
      
      output$downloadHierarchy_SRPARcsv <- downloadHandler(
        filename = "Stessor_hierarchy.csv",
        content = function(file) {
          write.csv(tableInput_SRPAR(), file, fileEncoding = 'WINDOWS-1252', row.names = F)
        })
      
      # SLMR - Download 
      
      plotInput_SLMR <- function(){
        plot_radar_chart(prior = priors_SLMR, post = posteriors_SLMR(), pscale = input$threshold_SLMR/100,
                         for_plot = for_plot_SLMR, rchartlabs = rchartlabs_SLMR, col = rgb(0/256,61/256,144/256))
      }
      
      # Stressor overview (radar plot)
      
      output$downloadOverview_SLMR <- downloadHandler(
        filename = "Stessor_overview.png",
        content = function(file) {
          png(file,width = 800, height = 800, units = "px")
          plotInput_SLMR()
          dev.off()
        })
      
      tableInput_SLMR <- function(){
        table_radar_chart(prior = priors_SLMR, post = posteriors_SLMR(), pscale = input$threshold_SLMR/100,
                          for_plot = for_plot_SLMR, rchartlabs = rchartlabs_SLMR, columnames = c("Potential stressor","Increase [%]"))
      }
      
      # Stressor hierarchy (table)
      
      output$downloadHierarchy_SLMR <- downloadHandler(
        filename = "Stessor_hierarchy.xlsx",
        content = function(file) {
          write.xlsx(tableInput_SLMR(), file, fileEncoding = 'WINDOWS-1252', row.names = F)
        })
      
      output$downloadHierarchy_SLMRcsv <- downloadHandler(
        filename = "Stessor_hierarchy.csv",
        content = function(file) {
          write.csv(tableInput_SLMR(), file, fileEncoding = 'WINDOWS-1252', row.names = F)
        })
      
      # RLMR - Download
      
      plotInput_RLMR <- function(){
        plot_radar_chart(prior = priors_RLMR, post = posteriors_RLMR(), pscale = input$threshold_RLMR/100,
                         for_plot = for_plot_RLMR, rchartlabs = rchartlabs_RLMR, col = rgb(0/256,61/256,144/256))
      }
      
      # Stressor overview (radar plot)
      
      output$downloadOverview_RLMR <- downloadHandler(
        filename = "Stessor_overview.png",
        content = function(file) {
          png(file,width = 800, height = 800, units = "px")
          plotInput_RLMR()
          dev.off()
        })
      
      tableInput_RLMR <- function(){
        table_radar_chart(prior = priors_RLMR, post = posteriors_RLMR(), pscale = input$threshold_RLMR/100,
                          for_plot = for_plot_RLMR, rchartlabs = rchartlabs_RLMR, columnames = c("Potential stressor","Increase [%]"))
      }
      
      # Stressor hierarchy (table)
      
      output$downloadHierarchy_RLMR <- downloadHandler(
        filename = "Stessor_hierarchy.xlsx",
        content = function(file) {
          write.xlsx(tableInput_RLMR(), file, fileEncoding = 'WINDOWS-1252', row.names = F)
        })
      
      output$downloadHierarchy_RLMRcsv <- downloadHandler(
        filename = "Stessor_hierarchy.csv",
        content = function(file) {
          write.csv(tableInput_RLMR(), file, fileEncoding = 'WINDOWS-1252', row.names = F)
        })
      
    }
    
    # Run desktop-based application 
    shinyApp(ui = ui, server = server)
    
    
    