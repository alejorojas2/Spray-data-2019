---
title: "Spray data - _B. glumae_"
output:
  html_document: 
    keep_md: yes
    fig_caption: yes
editor_options: 
  chunk_output_type: inline
---

Data of 10 cultivars of rice inoculated with _B. glumae_ or mock inoculated.  Discoloration of spikelets were recorded and presented as percentage.


```r
library(tidyverse)
library(readxl)
library(ggplot2)
```



```r
rice_data <- read_excel("Spray-Data-06.18.20.xlsx", 
                        col_types = c("text", "numeric", "numeric",
                                      "numeric", "numeric","numeric"))
rice_data
```

```
## # A tibble: 320 x 6
##    Genotype   Rep `Mock_30C-22C` `Mock_30C-28C` `Pathogen_30C-2…
##    <chr>    <dbl>          <dbl>          <dbl>            <dbl>
##  1 310111       1           5.88           18.6             86.5
##  2 310111       2           2.48           79.7             82.6
##  3 310111       3           2.9            61.7            100  
##  4 310111       4           0              67.2             86.1
##  5 310111       5           1.76           75.4             98.6
##  6 310111       6           0              76.6             93.8
##  7 310111       7          NA              NA               85.7
##  8 310111       8          NA              NA              100  
##  9 310111       9          NA              NA              100  
## 10 310111      10          NA              NA              100  
## # … with 310 more rows, and 1 more variable: `Pathogen_30C-28C` <dbl>
```

We still have to "reshape" the table to make it in longer format coding a column for treatment (Mock vs Inoculated) and temperature profile (30-22 vs 30-28).


```r
rice_data_long <- rice_data %>% 
  pivot_longer(cols = c("Mock_30C-22C", "Mock_30C-28C", 
                        "Pathogen_30C-22C", "Pathogen_30C-28C"),
               names_to = "Inoculation", 
               values_to = "DiscPerc") %>%
  separate(col = Inoculation, 
            sep = "_",
            into = c("Inoculation", "TempProfile")) %>% 
  unite("Inoc_Temp", Inoculation:TempProfile, remove = FALSE)

#kableExtra::kable(rice_data_long, format = "markdown")
```

# Data Exploration
 
Separating mock from pathogen inoculated:


```r
ggplot(data = rice_data_long, aes(x = Genotype, y = DiscPerc)) +
  geom_boxplot(aes(fill = TempProfile)) +
  facet_grid(. ~ Inoculation) +
  coord_flip()
```

![](Data_analysis_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Looking at genotype effect:


```r
ggplot(data = rice_data_long, aes(x = Inoc_Temp, y = DiscPerc)) +
  geom_boxplot(aes(fill = TempProfile)) +
  facet_wrap(Genotype ~ ., ncol = 5) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
```

![](Data_analysis_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

# Clustering analysis

Since we are dealing with continous data on four different conditions with need to scale them to estimate
their coordinates.

```r
library(FactoMineR)
library(factoextra)

rice_scaled <- scale(rice_data[,3:6])
row.names(rice_scaled) <- rice_data$Genotype

#kableExtra::kable(rice_scaled, format = "markdown")

#Need to remove NAs
rice_scaled_NoNAs <- na.omit(rice_scaled)

#Calculating euclidean distances
rice_dist <- dist(rice_scaled_NoNAs, method = "euclidean")

#Hierarchical clustering
rice_clust <- hclust(rice_dist, method = "complete")

plot(rice_clust)
```

![](Data_analysis_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

