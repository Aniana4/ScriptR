Dividir Tablas
--------------

Vamos a dividir la tabla iris en dos tablas de manera que aleatoriamente cada tabla tenga las mismas observaciones de "Species".

``` r
library(plyr)
library(data.table)
mi.iris <- iris
mi.iris <- data.table(mi.iris)
head(mi.iris)
```

    ##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1:          5.1         3.5          1.4         0.2  setosa
    ## 2:          4.9         3.0          1.4         0.2  setosa
    ## 3:          4.7         3.2          1.3         0.2  setosa
    ## 4:          4.6         3.1          1.5         0.2  setosa
    ## 5:          5.0         3.6          1.4         0.2  setosa
    ## 6:          5.4         3.9          1.7         0.4  setosa

``` r
library(dplyr)
  partido <- mi.iris %>%  #parte por grupos
  group_by(Species)%>%sample_n((length(mi.iris$Species)/2)/(length(unique(mi.iris$Species))),replace=FALSE)
summary(partido)
```

    ##   Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
    ##  Min.   :4.300   Min.   :2.200   Min.   :1.000   Min.   :0.100  
    ##  1st Qu.:5.050   1st Qu.:2.700   1st Qu.:1.600   1st Qu.:0.300  
    ##  Median :5.800   Median :3.000   Median :4.300   Median :1.300  
    ##  Mean   :5.837   Mean   :2.984   Mean   :3.712   Mean   :1.189  
    ##  3rd Qu.:6.450   3rd Qu.:3.300   3rd Qu.:5.000   3rd Qu.:1.800  
    ##  Max.   :7.700   Max.   :4.000   Max.   :6.900   Max.   :2.500  
    ##        Species  
    ##  setosa    :25  
    ##  versicolor:25  
    ##  virginica :25  
    ##                 
    ##                 
    ## 

``` r
dim(partido)
```

    ## [1] 75  5

``` r
head(partido)
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1          5.1         3.8          1.5         0.3  setosa
    ## 2          5.0         3.4          1.5         0.2  setosa
    ## 3          5.8         4.0          1.2         0.2  setosa
    ## 4          5.0         3.4          1.6         0.4  setosa
    ## 5          5.1         3.5          1.4         0.2  setosa
    ## 6          5.1         3.8          1.9         0.4  setosa

``` r
partidocompl <- setdiff(mi.iris,partido)
head(partidocompl)
```

    ##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1:          4.7         3.2          1.3         0.2  setosa
    ## 2:          4.6         3.1          1.5         0.2  setosa
    ## 3:          5.4         3.9          1.7         0.4  setosa
    ## 4:          4.6         3.4          1.4         0.3  setosa
    ## 5:          4.4         2.9          1.4         0.2  setosa
    ## 6:          4.9         3.1          1.5         0.1  setosa

``` r
sum(iris$Sepal.Length)#876.5
```

    ## [1] 876.5

``` r
sum(partido$Sepal.Length)#447.4
```

    ## [1] 437.8

``` r
sum(partidocompl$Sepal.Length)#429.1
```

    ## [1] 432.9
