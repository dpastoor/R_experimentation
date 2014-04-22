dplyr vs data.table comparions
====================================

Benchmark 1 - subsetting from a large dataframe

```r
suppressMessages(library(dplyr))
suppressMessages(library(data.table))
### create large data source -----------
database <- data.frame(expand.grid(ID = 1:50, TIME = seq(0, 100, 1), REP = 1:250))
database <- database[order(database$REP, database$ID, database$TIME), ]
database <- within(database, {
    CONC <- runif(nrow(database), 1, 10)
    DV <- runif(nrow(database), 1, 10)
    IPRED <- runif(nrow(database), 1, 10)
    PRED <- runif(nrow(database), 1, 10)
    ISM <- ifelse(ID%%2 == 0, 1, 0)
})

# 
database_dt <- data.table(database)
head(database)
```

```
##     ID TIME REP ISM  PRED IPRED    DV  CONC
## 1    1    0   1   0 8.916 8.476 7.203 1.777
## 51   1    1   1   0 4.749 7.615 2.996 6.356
## 101  1    2   1   0 9.384 3.750 4.315 1.592
## 151  1    3   1   0 2.419 5.602 4.790 2.957
## 201  1    4   1   0 6.535 1.549 6.931 1.492
## 251  1    5   1   0 4.094 8.737 2.849 8.270
```

```r
### functions
generate_samples <- function(ID, REP, num_inds) {
    id <- sample(x = ID, size = num_inds, replace = TRUE)
    rep <- sample.int(REP, size = num_inds, replace = FALSE)
    data.frame(ID = id, REP = rep)
}

# test key
generate_samples(unique(database$ID), REP = length(unique(database$REP)), num_inds = 10)
```

```
##    ID REP
## 1  31  82
## 2   3 234
## 3  28  97
## 4  27 100
## 5  44  65
## 6  23  50
## 7  41 127
## 8  15 198
## 9   7 161
## 10 41  77
```

```r


generate_dataset_dt <- function(database_dt, inds) {
    num_inds <- inds
    m <- generate_samples(unique(database_dt$ID), REP = length(unique(database_dt$REP)), 
        num_inds = num_inds)
    setkey(database_dt, ID, REP)
    database_subset <- database_dt[J(m)]
    return(database_subset)
}

generate_dataset_dplyr <- function(database, inds) {
    num_inds <- inds
    m <- generate_samples(unique(database$ID), REP = length(unique(database$REP)), 
        num_inds = num_inds)
    database_subset <- semi_join(database, m)
    return(database_subset)
}
suppressMessages(library(ggplot2))
suppressMessages(library(microbenchmark))

# check varying subset sizes 20 - 200 individuals
tm <- microbenchmark(suppressMessages(generate_dataset_dplyr(database, 20)), 
    generate_dataset_dt(database_dt, 20), suppressMessages(generate_dataset_dplyr(database, 
        50)), generate_dataset_dt(database_dt, 50), suppressMessages(generate_dataset_dplyr(database, 
        100)), generate_dataset_dt(database_dt, 100), suppressMessages(generate_dataset_dplyr(database, 
        200)), generate_dataset_dt(database_dt, 200), times = 500L)
tm
```

```
## Unit: milliseconds
##                                                     expr   min    lq
##   suppressMessages(generate_dataset_dplyr(database, 20)) 81.84 84.95
##                     generate_dataset_dt(database_dt, 20) 32.56 34.46
##   suppressMessages(generate_dataset_dplyr(database, 50)) 82.24 84.99
##                     generate_dataset_dt(database_dt, 50) 31.71 34.60
##  suppressMessages(generate_dataset_dplyr(database, 100)) 82.61 85.21
##                    generate_dataset_dt(database_dt, 100) 32.07 34.90
##  suppressMessages(generate_dataset_dplyr(database, 200)) 82.58 85.53
##                    generate_dataset_dt(database_dt, 200) 32.88 35.40
##  median    uq    max neval
##   88.17 94.27 136.03   500
##   36.43 43.80  76.71   500
##   88.59 95.06 144.37   500
##   36.38 44.57  86.34   500
##   88.21 95.03 136.95   500
##   36.76 44.45  78.28   500
##   89.13 95.50 128.22   500
##   37.34 44.95  93.40   500
```

```r
autoplot(tm)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 



```r
sessionInfo()
```

```
## R version 3.0.2 (2013-09-25)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] microbenchmark_1.3-0 ggplot2_0.9.3.1      data.table_1.9.2    
## [4] dplyr_0.1.2          knitr_1.5           
## 
## loaded via a namespace (and not attached):
##  [1] assertthat_0.1.0.99 colorspace_1.2-4    dichromat_2.0-0    
##  [4] digest_0.6.4        evaluate_0.5.1      formatR_0.10       
##  [7] grid_3.0.2          gtable_0.1.2        labeling_0.2       
## [10] MASS_7.3-29         munsell_0.4.2       plyr_1.8           
## [13] proto_0.3-10        RColorBrewer_1.0-5  Rcpp_0.11.0        
## [16] reshape2_1.2.2      scales_0.2.3        stringr_0.6.2      
## [19] tools_3.0.2
```


