# Spray-data-2019 Percentage values taken from the number of discolored spikelets over the total number of spikelets on each panicle

Data analysis for _B. glumae_ inoculations on rice cultivars
* 10 genotypes
* Two temperature profiles: 30ºC/22ºC; 30ºC/28ºC
* Mock vs Inoculated

## Cluster and PCA analysis

* [Data analysis](Data_analysis.md)

## Packages and R version

```
R version 4.0.0 (2020-04-24)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Catalina 10.15.7

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] scales_1.1.1       plyr_1.8.6         viridis_0.5.1      viridisLite_0.3.0  RColorBrewer_1.1-2
 [6] factoextra_1.0.7   FactoMineR_2.3     readxl_1.3.1       forcats_0.5.0      stringr_1.4.0     
[11] dplyr_1.0.2        purrr_0.3.4        readr_1.4.0        tidyr_1.1.2        tibble_3.0.3      
[16] ggplot2_3.3.2      tidyverse_1.3.0   

loaded via a namespace (and not attached):
 [1] fs_1.4.2             lubridate_1.7.9      httr_1.4.2           ggbiplot_0.55        tools_4.0.0         
 [6] backports_1.1.10     R6_2.4.1             DBI_1.1.0            lazyeval_0.2.2       colorspace_1.4-2    
[11] withr_2.3.0          tidyselect_1.1.0     gridExtra_2.3        curl_4.3             compiler_4.0.0      
[16] cli_2.0.2            rvest_0.3.5          flashClust_1.01-2    xml2_1.3.2           plotly_4.9.2.1      
[21] labeling_0.3         digest_0.6.25        foreign_0.8-80       rmarkdown_2.3        rio_0.5.16          
[26] pkgconfig_2.0.3      htmltools_0.5.0      dbplyr_1.4.4         htmlwidgets_1.5.2    rlang_0.4.8         
[31] rstudioapi_0.11      farver_2.0.3         generics_0.0.2       jsonlite_1.7.1       crosstalk_1.1.0.1   
[36] dendextend_1.13.4    zip_2.0.4            car_3.0-9            magrittr_1.5         leaps_3.1           
[41] Rcpp_1.0.5           munsell_0.5.0        fansi_0.4.1          abind_1.4-7          lifecycle_0.2.0     
[46] scatterplot3d_0.3-41 stringi_1.5.3        yaml_2.2.1           carData_3.0-4        MASS_7.3-51.6       
[51] blob_1.2.1           ggrepel_0.9.0        crayon_1.3.4         lattice_0.20-41      haven_2.3.1         
[56] hms_0.5.3            knitr_1.29           pillar_1.4.6         ggpubr_0.4.0         ggsignif_0.6.0      
[61] reprex_0.3.0         glue_1.4.2           evaluate_0.14        data.table_1.13.0    modelr_0.1.8        
[66] vctrs_0.3.4          cellranger_1.1.0     gtable_0.3.0         assertthat_0.2.1     xfun_0.15           
[71] openxlsx_4.1.5       broom_0.7.0          rstatix_0.6.0        cluster_2.1.0        ellipsis_0.3.1     

```