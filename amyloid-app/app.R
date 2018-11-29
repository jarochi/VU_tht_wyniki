library(shiny)
library(dplyr)
library(DT)

dat <- read.csv("amyloid_heatmap_Marlena.csv") %>% 
  select(peptide, AmyLoad, ThT) %>% 
  mutate(FTIR = "N",
         ThT = ifelse(ThT == "tak", "T", "N"),
         AmyLoad = ifelse(AmyLoad == "tak", "T", "N"))

ui <- fluidPage(
  titlePanel("Amyloid results app"),
  DT::dataTableOutput("amyloid_table")
)

server <- function(input, output) {
  
  proxy = dataTableProxy("amyloid_table")
  
  observeEvent(input[["amyloid_table_cell_edit"]], {
    info <- input[["amyloid_table_cell_edit"]]
    i <- info[["row"]]
    j <- info[["col"]]
    v <- info[["value"]]
    dat[i, j] <<- DT::coerceValue(v, dat[i, j])
    replaceData(proxy, dat, resetPaging = FALSE)  # important
  })
  
  dt_r <- reactive({
    datatable(dat, options = list(pageLength = 24), editable = TRUE) %>% 
      formatStyle(c("AmyLoad", "ThT", "FTIR"),
                  backgroundColor = styleEqual(c("T", "N"), c("indianred", "lightblue")))
  })
  
  output[["amyloid_table"]] <- DT::renderDataTable({
    dt_r()
  })
  
}


shinyApp(ui = ui, server = server)