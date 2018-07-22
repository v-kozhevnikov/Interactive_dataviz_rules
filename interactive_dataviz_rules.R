library(highcharter)
library(readr)
library(stringr)
library(tidyr)
library(dplyr)


#clearing data for smartphones

smartphones <- read_csv("data/smartphones.csv") %>%
  select(-1) %>%
  t() %>%
  as.data.frame(.) %>%
  transmute(company = rownames(.),
            units = (V1 + V2 + V3 + V4))

smartphones$company <- ifelse( smartphones$units < 50, "Others", smartphones$company) %>%
  str_replace( ., "[*]+", "")

smartphones <- smartphones %>%
  group_by(company) %>%
  summarise(units = sum(units, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(product = rep("Smartphones", n_distinct(smartphones$company)))

#clearing data for tablets

tablets <- read_csv("data/tablets.csv") %>%
  select(-1) %>%
  t() %>%
  as.data.frame(.) %>%
  transmute(company = rownames(.),
            units = (V1 + V2 + V3 + V4))

tablets$company <- ifelse(tablets$units < 5, "Others", tablets$company) 

tablets <- tablets %>%
  group_by(company) %>%
  summarise(units = sum(units, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(product = rep("Tablets", n_distinct(tablets$company)))

#clearing data for PC

PC <- read_csv("data/PC.csv") %>%
  select(-1) %>%
  t() %>%
  as.data.frame(.) %>%
  transmute(company = rownames(.),
            units = (V1 + V2 + V3 + V4))

PC$company <- ifelse(PC$units < 5, "Others", PC$company) 

PC <- PC %>%
  group_by(company) %>%
  summarise(units = sum(units, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(product = rep("PC", n_distinct(PC$company)))

# merging into one file

sales <- rbind(PC, smartphones, tablets)


# initial visualization

highchart() %>%
  hc_add_series(data = sales, type = "sankey",
                hcaes(from = product, to = company, weight = units))


# ordered chart

sales_1 <- sales %>%
  group_by(product) %>%
  mutate(product_units = sum(units)) %>%
  group_by(company) %>%
  mutate(company_units = sum(units)) %>%
  arrange(desc(product_units), desc(company_units))

highchart() %>%
  hc_add_series(data = sales_1, type = "sankey",
                hcaes(from = product, to = company, weight = units)) %>%
  hc_plotOptions(sankey = list(
    colorByPoint = FALSE,
    curveFactor = 0.5,
    linkOpacity = 0.33
  ))


#filtering and zooming
highchart() %>%
  hc_add_series(data = sales_1, type = "sankey",
                hcaes(from = product, to = company, weight = units)) %>%
  hc_plotOptions(sankey = list(
    colorByPoint = FALSE,
    curveFactor = 0.5,
    linkOpacity = 0.33,
    point = list(
      events = list(
        mouseOver = JS(
          "function () { if (this.isNode) {
          this.linksFrom.forEach(function(l) {
          l.setState('hover');
          });
          this.linksTo.forEach(function(l) {
          l.setState('hover');
          });
          }
          }  "
          
      ),
      mouseOut = JS(
        "function () { if (this.isNode) {
        this.linksFrom.forEach(function(l) {
        l.setState('');
        });
        this.linksTo.forEach(function(l) {
        l.setState('');
        });
        }
        }  "
          
    )
        )
  )))

#stacked bar chart

sales_stacked <- expand.grid(unique(sales$company), unique(sales$product)) 

colnames(sales_stacked) <- c("company", "product")

sales_stacked <- sales_stacked %>%
  left_join(sales, by = c("product", "company"))

highchart() %>%
  hc_add_series(data = sales_stacked, 
                type = "column",
                hcaes(x = product, y = units, group = company)) %>%
  hc_plotOptions(column = list(stacking = "normal")) %>%
  hc_xAxis(categories = unique(sales_stacked$product))

#final chart

sales_2 <- sales_1 %>%
  mutate(product_share = round( units/product_units*100,0) , company_share = round(units/company_units*100,0))


highchart() %>%
  hc_add_series(data = sales_2, type = "sankey",
                hcaes(from = product, to = company, weight = units)) %>%
  hc_plotOptions(sankey = list(
    colorByPoint = FALSE,
    curveFactor = 0.5,
    linkOpacity = 0.33,
    point = list(
      events = list(
        mouseOver = JS(
          "function () { if (this.isNode) {
          this.linksFrom.forEach(function(l) {
          l.setState('hover');
          });
          this.linksTo.forEach(function(l) {
          l.setState('hover');
          });
          }
          }  "
          
      ),
      mouseOut = JS(
        "function () { if (this.isNode) {
        this.linksFrom.forEach(function(l) {
        l.setState('');
        });
        this.linksTo.forEach(function(l) {
        l.setState('');
        });
        }
        }  "
          
    )
        )
        ),
    tooltip = list(
      pointFormat =  "{point.fromNode.name} -> {point.toNode.name}: <b>{point.weight}</b> Mio units<br/>
      {point.product} contribute <b>{point.product_share} %</b> in {point.company} sales: <br/>
      {point.company} contributes <b>{point.company_share} %</b> in {point.product} sales "
    )
    )) %>%
  hc_exporting(enabled = TRUE,
                    buttons = list(
                      contextButton = list(
                        y = -30
                      )
                    ))%>%
  hc_chart(spacingTop = 30)
