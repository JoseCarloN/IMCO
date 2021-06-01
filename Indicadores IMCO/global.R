library(highcharter)
library(shiny)
library(shinydashboard)
library(bs4Dash)
library(dplyr)
library(purrr)
library(waiter)
library(shinyjs)
library(shinyBS)

# Custom ValueBox function -----------------------------------------------------
valueBoxSpark = function(value, title, sparkobj = NULL, subtitle, info = NULL, 
                          icon = NULL, color = "aqua", width = 4, href = NULL){
  
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  
  info_icon = tags$small(
    tags$i(
      class = "fa fa-info-circle fa-lg",
      title = info,
      `data-toggle` = "tooltip",
      style = "color: rgba(255, 255, 255, 0.75);"
    ),
    # bs3 pull-right 
    # bs4 float-right
    class = "pull-right float-right"
  )
  
  boxContent = div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      tags$small(title),
      if (!is.null(sparkobj)) info_icon,
      h3(value),
      if (!is.null(sparkobj)) sparkobj,
      p(subtitle)
    ),
    # bs3 icon-large
    # bs4 icon
    if (!is.null(icon)) div(class = "icon-large icon", icon, style = "z-index; 0")
  )
  
  if (!is.null(href)) 
    boxContent = a(href = href, boxContent)
  
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
  )
}

# Custom hc theme --------------------------------------------------------------
hc_theme_sparkline_vb = function(...) {
  
  theme = list(
    chart = list(
      backgroundColor = NULL,
      margins = c(0, 0, 0, 0),
      spacingTop = 0,
      spacingRight = 0,
      spacingBottom = 0,
      spacingLeft = 0,
      plotBorderWidth = 0,
      borderWidth = 0,
      style = list(overflow = "visible")
    ),
    xAxis = list(
      visible = FALSE, 
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    yAxis = list(
      visible = FALSE,
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    tooltip = list(
      outside = FALSE,
      shadow = FALSE,
      borderColor = "transparent",
      botderWidth = 0,
      backgroundColor = "transparent",
      style = list(textOutline = "5px white")
    ),
    plotOptions = list(
      series = list(
        marker = list(enabled = FALSE),
        enableMouseTracking = FALSE,
        lineWidth = 2,
        shadow = FALSE,
        fillOpacity = 0.25,
        color = "#FFFFFFBF",
        fillColor = list(
          linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
          stops = list(
            list(0.00, "#FFFFFF00"),
            list(0.50, "#FFFFFF7F"),
            list(1.00, "#FFFFFFFF")
          )
        )
      )
    ),
    credits = list(
      enabled = FALSE,
      text = ""
    )
  )
  
  theme = structure(theme, class = "hc_theme")
  
  if (length(list(...)) > 0) {
    theme = hc_theme_merge(
      theme,
      hc_theme(...)
    )
  }
  
  theme
}

# Custom hc theme --------------------------------------------------------------
hc_tooltip_theme = function (...) 
{
  theme <- list(colors = c("#219ebc", "#F90101", "#F2B50F", "#636464", "#C4C4C4"), 
                chart = list(style = list(fontFamily = "Roboto",
                                          color = "#444444")), 
                xAxis = list(gridLineWidth = 0, 
                             gridLineColor = "#F3F3F3", 
                             lineColor = "#F3F3F3", 
                             minorGridLineColor = "#F3F3F3", 
                             tickColor = "#F3F3F3", 
                             tickWidth = 1), 
                yAxis = list(gridLineColor = NULL, 
                             lineColor = NULL, 
                             minorGridLineColor = NULL,
                             tickColor = NULL, 
                             tickWidth = 1, 
                             endOnTick = FALSE, 
                             startOnTick = FALSE), 
                legendBackgroundColor = "rgba(0, 0, 0, 0.5)", 
                tooltip = list(backgroundColor = "rgba(0, 0, 0, 0.85)", 
                               style = list(color = "#F0F0F0")),
                background2 = "#505053", 
                dataLabelsColor = "#B0B0B3", 
                textColor = "#C0C0C0", 
                contrastTextColor = "#F0F0F3", 
                maskColor = "rgba(255,255,255,0.3)")
  theme <- structure(theme, class = "hc_theme")
  if (length(list(...)) > 0) {
    theme <- hc_theme_merge(theme, hc_theme(...))
  }
  theme
}

set.seed(123)

N = 20

x = cumsum(rnorm(N)) + 0.5 * cumsum(runif(N))
x = round(200*x)

df = data.frame(
  x = sort(as.Date(Sys.time() - lubridate::days(1:N))),
  y = abs(x)
)

# Sistema de derecho confiable y objetivo --------------------------------------

hc_derecho = hchart(df, "area", hcaes(x, y), name = "lines of code")  %>% 
  hc_size(height = 100)

hc_derecho = hc_derecho %>% 
  hc_add_theme(hc_theme_sparkline_vb()) %>% 
  hc_credits(enabled = FALSE)

vb_derecho = valueBoxSpark(
  value = tagList(HTML("1<sup><small>er</small></sup>"), "lugar"),
  title = toupper("Sistema de derecho confiable y objetivo"),
  sparkobj = hc_derecho,
  subtitle = tagList(HTML("&uarr;"), "25% Since last day"),
  info = NULL,
  icon = icon("code"),
  width = 4,
  color = "green",
  href = "#"
)

# Manejo sustentable del medio ambiente ----------------------------------------

hc_MA = hchart(df, "area", hcaes(x, y), name = "lines of code")  %>% 
  hc_size(height = 100)

hc_MA = hc_MA %>% 
  hc_add_theme(hc_theme_sparkline_vb()) %>% 
  hc_credits(enabled = FALSE)

vb_MA = valueBoxSpark(
  value = tagList(HTML("1<sup><small>er</small></sup>"), "lugar"),
  title = toupper("Manejo sustentable del medio ambiente"),
  sparkobj = hc_MA,
  subtitle = tagList(HTML("&uarr;"), "25% Since last day"),
  info = NULL,
  icon = icon("code"),
  width = 4,
  color = "red",
  href = NULL
)

# Aprovechamiento de las relaciones internacionales  ---------------------------

hc_ri = hchart(df, "area", hcaes(x, y), name = "lines of code")  %>% 
  hc_size(height = 100)

hc_ri = hc_ri %>% 
  hc_add_theme(hc_theme_sparkline_vb()) %>% 
  hc_credits(enabled = FALSE)

vb_ri = valueBoxSpark(
  value = tagList(HTML("1<sup><small>er</small></sup>"), "lugar"),
  title = toupper(HTML("Aprovechamiento de las relaciones internacionales")),
  sparkobj = hc_ri,
  subtitle = tagList(HTML("&uarr;"), "25% Since last day"),
  info = NULL,
  icon = icon("code"),
  width = 4,
  color = "olive",
  href = NULL
)

# Innovación de los sectores económicos  ---------------------------------------

hc_innovacion = hchart(df, "area", hcaes(x, y), name = "lines of code")  %>% 
  hc_size(height = 100)

hc_innovacion = hc_innovacion %>% 
  hc_add_theme(hc_theme_sparkline_vb()) %>% 
  hc_credits(enabled = FALSE)

vb_innovacion = valueBoxSpark(
  value = tagList(HTML("1<sup><small>er</small></sup>"), "lugar"),
  title = toupper(HTML("Innovación de los sectores económicos")),
  sparkobj = hc_innovacion,
  subtitle = tagList(HTML("&uarr;"), "25% Since last day"),
  info = NULL,
  icon = icon("code"),
  width = 4,
  color = "lime",
  href = NULL
)

# Sociedad incluyente, preparada y sana ----------------------------------------

hc_sociedad = hchart(df, "area", hcaes(x, y), name = "lines of code")  %>% 
  hc_size(height = 100)

hc_sociedad = hc_sociedad %>% 
  hc_add_theme(hc_theme_sparkline_vb()) %>% 
  hc_credits(enabled = FALSE)

vb_sociedad = valueBoxSpark(
  value = tagList(HTML("1<sup><small>er</small></sup>"), "lugar"),
  title = toupper(HTML("Sociedad incluyente, preparada y sana")),
  sparkobj = hc_sociedad,
  subtitle = tagList(HTML("&uarr;"), "25% Since last day"),
  info = NULL,
  icon = icon("code"),
  width = 4,
  color = "aqua",
  href = NULL
)

# Sistema político estable y funcional  ----------------------------------------

hc_politico = hchart(df, "area", hcaes(x, y), name = "lines of code")  %>% 
  hc_size(height = 100)

hc_politico = hc_politico %>% 
  hc_add_theme(hc_theme_sparkline_vb()) %>% 
  hc_credits(enabled = FALSE)

vb_politico = valueBoxSpark(
  value = tagList(HTML("1<sup><small>er</small></sup>"), "lugar"),
  title = toupper(HTML("Sistema político estable y funcional")),
  sparkobj = hc_politico,
  subtitle = tagList(HTML("&uarr;"), "25% Since last day"),
  info = NULL,
  icon = icon("code"),
  width = 4,
  color = "blue",
  href = NULL
)

# Gobiernos eficientes y eficaces  ---------------------------------------------

hc_gobierno = hchart(df, "area", hcaes(x, y), name = "lines of code")  %>% 
  hc_size(height = 100)

hc_gobierno = hc_gobierno %>% 
  hc_add_theme(hc_theme_sparkline_vb()) %>% 
  hc_credits(enabled = FALSE)

vb_gobierno = valueBoxSpark(
  value = tagList(HTML("1<sup><small>er</small></sup>"), "lugar"),
  title = toupper(HTML("Gobiernos eficientes y eficaces")),
  sparkobj = hc_gobierno,
  subtitle = tagList(HTML("&uarr;"), "25% Since last day"),
  info = NULL,
  icon = icon("code"),
  width = 4,
  color = "light-blue",
  href = NULL
)

# Mercado de factores  ---------------------------------------------------------

hc_factores = hchart(df, "area", hcaes(x, y), name = "lines of code")  %>% 
  hc_size(height = 100)

hc_factores = hc_factores %>% 
  hc_add_theme(hc_theme_sparkline_vb()) %>% 
  hc_credits(enabled = FALSE)

vb_factores = valueBoxSpark(
  value = tagList(HTML("1<sup><small>er</small></sup>"), "lugar"),
  title = toupper(HTML("Mercado de factores")),
  sparkobj = hc_factores,
  subtitle = tagList(HTML("&uarr;"), "25% Since last day"),
  info = NULL,
  icon = icon("code"),
  width = 4,
  color = "fuchsia",
  href = NULL
)

# Economía estable  ------------------------------------------------------------

hc_economia = hchart(df, "area", hcaes(x, y), name = "lines of code")  %>% 
  hc_size(height = 100)

hc_economia = hc_economia %>% 
  hc_add_theme(hc_theme_sparkline_vb()) %>% 
  hc_credits(enabled = FALSE)

vb_economia = valueBoxSpark(
  value = tagList(HTML("1<sup><small>er</small></sup>"), "lugar"),
  title = toupper(HTML("Economía estable")),
  sparkobj = hc_economia,
  subtitle = tagList(HTML("&uarr;"), "25% Since last day"),
  info = NULL,
  icon = icon("code"),
  width = 4,
  color = "navy",
  href = NULL
)

# Precursores  -----------------------------------------------------------------

hc_precursores = hchart(df, "area", hcaes(x, y), name = "lines of code")  %>% 
  hc_size(height = 100)

hc_precursores = hc_precursores %>% 
  hc_add_theme(hc_theme_sparkline_vb()) %>% 
  hc_credits(enabled = FALSE)

vb_precursores = valueBoxSpark(
  value = tagList(HTML("1<sup><small>er</small></sup>"), "lugar"),
  title = toupper(HTML("Precursores")),
  sparkobj = hc_precursores,
  subtitle = tagList(HTML("&uarr;"), "25% Since last day"),
  info = NULL,
  icon = icon("code"),
  width = 4,
  color = "teal",
  href = NULL
)

# Desempeño Estatal  -----------------------------------------------------------

indices = read.csv("Formato Subíndices.csv", encoding = "UTF-8")
colnames(indices) = c("Año", "Estado", "Subindice", "Valor")

indices = indices %>%
  group_by(Año, Subindice) %>% 
  mutate(Valor = round(Valor, 0),
         rank = rank(-Valor, ties.method = "first"),
         color = ifelse(Estado == "Yucatán", "#023047", "#219ebc"),
         visible = ifelse(Estado == "Yucatán", TRUE, FALSE))

# Line chart del valor de los subindices  --------------------------------------

ds = indices %>%
  filter(Subindice == "Derecho") %>%
  group_by(Estado, visible) %>% 
  do(ds = list(
    data = list_parse2(data.frame(.$Año, .$Valor))
  )) %>%
  {
    pmap(list(.$Estado, .$visible, .$ds), function(x, y, z) {
      a = append(list(name = x), list(visible = y))
      
      return(append(a, z))
    })
  }

hc_indices = highchart() %>%
  hc_chart(type = "spline") %>% 
  hc_add_series_list(ds) %>%
  hc_add_theme(
    hc_tooltip_theme()
  ) %>%
  hc_tooltip(
    pointFormat = "<span  style='color: {series.color}'> {series.name} <b>{point.y}</b><br/></span>",
    shadow = FALSE,
    backgroundColor = "transparent",
    style = list(textOutline = "3px #404040"),
    borderColor = "transparent",
    borderWidth = 0,
    followPointer = FALSE,
    sort = TRUE
  ) %>%
  hc_plotOptions(
    series = list(
      states = list(
        inactive = list(
          enabled = FALSE
        )
      )
    )
  )

# Bar chart del rank estatal de los subindices  --------------------------------


hc_rank = hchart(indices[indices$Subindice == "Derecho" & indices$Año == "2019",], type = "column",
                    hcaes(x = Estado, 
                          y = Valor,
                          color = color),
                 connectorWidth = 1,
                 dataSorting = list(
                   enabled = TRUE,
                   sortKey = "y"
                 )) %>%
  hc_add_theme(
    hc_tooltip_theme()
    )%>%
  hc_tooltip(
    pointFormat = "<span  style='color: #ffb703'> Valor <b>{point.y}</b><br/></span>
    <span  style='color: #fb8500'> Lugar <b>{point.rank}</b><br/></span>",
    shadow = FALSE,
    backgroundColor = "transparent",
    style = list(textOutline = "3px #404040"),
    borderColor = "transparent",
    borderWidth = 0,
    followPointer = TRUE
  ) %>%
  hc_exporting(
    enabled = TRUE
  )