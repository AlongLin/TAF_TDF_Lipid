## Flowchart for the study
## 2023年12月27日
## Use R to draw a flowchat

# Load required R packages -----------------------------------------------------  
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here,
               tidyverse,
               DiagrammeR,
               DiagrammeRsvg,
               rsvg, # For exporting graphs into svgs
               xml2, # For exporting graphs into svgs without prior rendering
               plotly) # For exporting mermaid graphs

# Create flow chart with flexible parameter inputs -----------------------------  

a <- 2344 # Total patients
b <- 2344 - 856/2  # Excluded patients without sufficient data
c <- (856 - 854)/2   # Excluded patients younger than 18
d <- (854 - 836)/2  # Excluded patients switch treatment

e <- 836/2 # For analysis (1:1 PSM)

f <- 265 # Assigned TDF
g <- 153 # Assigned TAF
f1 <- 129
g1 <- 129

h = 292

i <- 238 # Assigned TDF
j <- 54 # Assigned TFF

i1 <- 101
j1 <- 52

k = 216

l <- 174 # Assigned TDF
m <- 42 # Assigned TFF


# Store parameters inside a list
flow_chart_data <- list(a, b, c, d, e, f, g, f1, g1, h, i, j, i1, j1, k,l, m) 

# Create grVis() graph
simple_trial <- grViz(
  "digraph {
         
    graph[layout = dot]
                     
    node[shape = rectangle, style = filled, margin = 0.2, fillcolor = 'aliceblue']  
                     
    a[label = '@@1']
    b[label = 'Patients excluded: \n @@2 \n @@3 \n @@4', fillcolor = 'mistyrose']
    # c[label = '@@3', fillcolor = 'mistyrose']
    # d[label = '@@4', fillcolor = 'mistyrose']
    e[label = '1:1 PSM 48W \n @@5 \n @@6',  fillcolor = 'aliceblue']
    h[label = '1:2 PSM 96W \n @@7 \n @@8',  fillcolor = 'aliceblue']
    f1[label = '@@9']
    g1[label = '@@10']
    i1[label = '@@11']
    j1[label = '@@12']
    k[label = 'Sequential analysis \n @@13 \n @@14',  fillcolor = 'aliceblue']
    
    edge[color = black]   
                
    a -> b[style = 'dashed']
    a -> e
    a -> h
    e -> f1
    e -> g1
    h -> i1
    h -> j1
    h -> k
          
  }
                     
  [1]: paste0('Patients screened (n = ', flow_chart_data[[1]], ')')
  [2]: paste0('Without sufficient data (n = ', flow_chart_data[[2]], ')')
  [3]: paste0('Younger than 18 yrs (n = ', flow_chart_data[[3]], ')')
  [4]: paste0('Switch treatment (n = ', flow_chart_data[[4]], ')')
  [5]: paste0('TDF n = ', flow_chart_data[[6]])
  [6]: paste0('TAF n = ', flow_chart_data[[7]])
  [7]: paste0('TDF n = ', flow_chart_data[[11]])
  [8]: paste0('TAF n = ', flow_chart_data[[12]])
  [9]: paste0('TDF n = ', flow_chart_data[[8]])
  [10]: paste0('TAF n = ', flow_chart_data[[9]])
  [11]: paste0('TDF n = ', flow_chart_data[[13]])
  [12]: paste0('TAF n = ', flow_chart_data[[14]])
  [13]: paste0('TDF n = ', flow_chart_data[[16]])
  [14]: paste0('TAF n = ', flow_chart_data[[17]]) 
  
  "
) 

simple_trial 

# Export graph as an svg -------------------------------------------------------


simple_trial %>% 
  export_svg%>%
  charToRaw %>%
  rsvg_svg(file="~/Rspace/clinical_investigation_2022/Output_for_publication/New20240115/flowchart_clinical_single.svg")

simple_trial %>% 
  export_svg%>%
  charToRaw %>%
  rsvg_png(file="~/Rspace/clinical_investigation_2022/Output_for_publication/New20240115/flowchart_clinical_single.png")
