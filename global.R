require(shiny)
require(shiny.fluent)
require(shiny.router)
require(glue)
require(bslib)
require(gt)
require(jsonlite)
require(tinytex)
require(ggplot2)
require(plotly)
require(ggpubr)
require("readxl")
require("dplyr")
require("tidyr")
require(stringr)




## Table

presenceTable<-c("Non-hospitalized infections in the absence of vaccine", "Non-hospitalized infections in the presence of vaccine", "Non-hospitalized infections averted", "NNV non-hospitalized infections",
                 "Total infections in the absence of vaccine", "Total infections in the presence of vaccine", "Total infections averted", "NNV total infections",
                 "Hospitalized infections in the absence of vaccine", "Hospitalized infection in the presence of vaccine", "Hospitalizations averted", "NNV hospitalized infections",
                 "Medically attended infections in the absence of vaccine", "Medically attended infections in the presence of vaccine", "Medically attended infections averted", "NNV medically attended infections",
                 "Prevented fraction")


indicatorType<-c("Non-hospitalized infections", "Non-hospitalized infections", "Non-hospitalized infections", "Non-hospitalized infections",
                 "Total infections", "Total infections", "Total infections", "Total infections",
                 "Hospitalized infections", "Hospitalized infections", "Hospitalized infections", "Hospitalized infections",
                 "Medically attended infections", "Medically attended infections", "Medically attended infections", "Medically attended infections",
                 "Prevented fraction")

presenceTable<-presenceTable%>%
  as.data.frame()%>%
  rename("indicators"=1)

indicatorType<-indicatorType%>%
  as.data.frame()%>%
  rename("indicator_type"=1)

presenceTable<-cbind(presenceTable, indicatorType)



## Helper function to make pages

makePage <- function (title, subtitle, contents) {
  tagList(div(
    class = "page-title",
    span(title, class = "ms-fontSize-32 ms-fontWeight-semibold", style =
           "color: #323130"),
    span(subtitle, class = "ms-fontSize-14 ms-fontWeight-regular", style =
           "color: #605E5C; margin: 14px;")
  ),
  contents)
}


## Helper function to make cards
makeCard <- function(title, content, size = 12, style = "") {
  div(
    class = glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = style,
    Stack(
      tokens = list(childrenGap = 5),
      Text(variant = "xLarge", title, block = TRUE),
      content
    )
  )
}

makeCard2 <- function(title, content, size = 12, style = "") {
  div(
    class = glue("card ms-depth-8 ms-sm{size} ms-xl{size} ms-fontSize-16 ms-fontWeight-regular"),
    style = style,
    Stack(
      tokens = list(childrenGap = 5),
      Text(variant = "xLarge", title, block = TRUE),
      br(),
      br(),
      content
    )
  )
}



