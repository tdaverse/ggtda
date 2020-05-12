#' Interactive Exploration of Dataset Topology using Shiny
#' 
#' Allows user to change simplicial complex radius for a 2-dimensional
#' point cloud and observe dynamic changes in simplicial complex structure.
#' 
#' @param dataset two-column data frame or tibble
#' @import shiny
#' @importFrom TDAstats calculate_homology
#' @return shiny object
interact_phom <- function(dataset) {
  colnames(dataset) <- c("x", "y")
  phom <- as.data.frame(TDAstats::calculate_homology(dataset))
  phom$dimension <- factor(phom$dimension)
  min_radius <- 0
  max_radius <- max(phom[, "death"])
  
  shinyApp(
    ui = fluidPage(
      fluidRow(
        column(4, plotOutput("circles", height = "275px")),
        column(4, plotOutput("skeleton", height = "275px")),
        column(4, plotOutput("empty", height = "275px")),
        column(4, plotOutput("barcode", height = "275px")),
        column(4, plotOutput("persist", height = "275px"))
      ),
      fluidRow(
        column(12, sliderInput("radius", "Radius",
                               min = min_radius, max = max_radius,
                               value = 0, step = NULL))
      )
    ),
    server = function(input, output) {
      output$circles <- renderPlot({
        ggplot(dataset, aes(x = x, y = y)) +
          coord_fixed() +
          stat_disk(radius = input$radius, fill = "aquamarine3") +
          geom_point() +
          theme_bw()
      })
      
      output$skeleton <- renderPlot({
        ggplot(dataset, aes(x = x, y = y)) +
          coord_fixed() +
          stat_vietoris2(diameter = input$radius * 2, fill = "darkgoldenrod") +
          stat_vietoris1(diameter = input$radius * 2, alpha = 0.25) +
          stat_vietoris0() +
          theme_bw()
      })
      
      output$barcode <- renderPlot({
        ggplot(phom, aes(start = birth, end = death, colour = dimension)) +
          theme_tda() +
          geom_barcode() +
          labs(x = "Diameter", y = "Homological Features") +
          geom_vline(xintercept = input$radius * 2, linetype = "dashed")
      })
      
      output$persist <- renderPlot({
        ggplot(phom, aes(start = birth, end = death, colour = dimension, shape = dimension)) +
          theme_tda() +
          stat_persistence(diagram = "diagonal") +
          labs(x = "Birth", y = "Death") +
          lims(x = c(0, NA), y = c(0, NA)) +
          geom_abline(intercept = 0, slope = 1,
                      color = "black") +
          geom_vline(xintercept = input$radius * 2,
                     color = "darkgoldenrod", linetype = "dashed") +
          geom_hline(yintercept = input$radius * 2,
                     color = "darkgoldenrod", linetype = "dashed")
      })
    }
  )
}
