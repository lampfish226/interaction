Iteration and listcols
================
Jinghan Zhao
2024-10-29

## Here’s some lists

``` r
l = list(
  vec_numeric = 1:4,
  unif_sample = runif(100),
  mat = matrix(1:8, nrow = 2, ncol = 4, byrow = TRUE),
  summary = summary(rnorm(1000))
)

l$mat
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8

``` r
l[["mat"]][1, 3]
```

    ## [1] 3

``` r
l[[1]]
```

    ## [1] 1 2 3 4

Make a list that’s hopefully a bit more useful

``` r
list_norm = 
  list(
    a = rnorm(20, 0, 5),
    b = rnorm(20, 4, 5),
    c = rnorm(20, 0, 10),
    d = rnorm(20, 4, 10)
  )

list_norm[["b"]]
```

    ##  [1]  9.5425725  8.0544640  1.1730438  4.3403048  5.6456708 11.5129136
    ##  [7] -0.5614324  6.9146473  2.9036578 -0.4458351 14.3971332  4.3883510
    ## [13]  8.8916898  1.0788506  3.2912415 13.4144229  9.2276748  2.7857779
    ## [19]  2.6147541  5.8926312

Let’s reuse function from last time

``` r
mean_and_sd = function(x){
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  out_df = tibble(
    mean = mean_x,
    sd = sd_x
  )
  
  return(out_df)
}
```

Let’s use this function to take mean and sd for all samples

``` r
mean_and_sd(list_norm[["a"]])
```

    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.442  3.60

``` r
mean_and_sd(list_norm[["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.75  4.40

``` r
mean_and_sd(list_norm[["c"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.83  9.57

``` r
mean_and_sd(list_norm[["d"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.18  7.34

## Use a for loop

Create output list and run a for loop

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])
  
}
```

## Do the same thing

but with `map` instead

``` r
map(list_norm, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.442  3.60
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.75  4.40
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.83  9.57
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.18  7.34

Let’s do a couple of other things

`map_dfr` and `map_dbl` is useful

`map` always return a list

``` r
output = 
  map(list_norm, mean_and_sd) %>% 
  bind_rows()

output = map_dfr(list_norm, mean_and_sd)

output = map(list_norm, median)
output = map(list_norm, IQR)

output = map_dbl(list_norm, IQR)
```

## LIST COLUMNS!!!

`filter` and `select` still working

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norm
  )

listcol_df
```

    ## # A tibble: 4 × 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>  
    ## 2 b     <dbl [20]>  
    ## 3 c     <dbl [20]>  
    ## 4 d     <dbl [20]>

``` r
listcol_df %>% 
  filter(name %in% c("a", "b"))
```

    ## # A tibble: 2 × 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>  
    ## 2 b     <dbl [20]>

``` r
listcol_df %>% 
  select(-samp)
```

    ## # A tibble: 4 × 1
    ##   name 
    ##   <chr>
    ## 1 a    
    ## 2 b    
    ## 3 c    
    ## 4 d

``` r
mean_and_sd(listcol_df[["samp"]][["a"]])
```

    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.442  3.60

``` r
mean_and_sd(listcol_df[["samp"]][["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.75  4.40

``` r
map(listcol_df[["samp"]], mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.442  3.60
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.75  4.40
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.83  9.57
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.18  7.34

ADD A LIST COLUMN!!!

``` r
listcol_df %>% 
  mutate(
    output = map(samp, mean_and_sd),
    iqr = map_dbl(samp, IQR))
```

    ## # A tibble: 4 × 4
    ##   name  samp         output             iqr
    ##   <chr> <named list> <named list>     <dbl>
    ## 1 a     <dbl [20]>   <tibble [1 × 2]>  4.35
    ## 2 b     <dbl [20]>   <tibble [1 × 2]>  6.23
    ## 3 c     <dbl [20]>   <tibble [1 × 2]> 13.4 
    ## 4 d     <dbl [20]>   <tibble [1 × 2]> 10.7

``` r
listcol_df %>% 
  mutate(
    output = map(samp, mean_and_sd),
    iqr = map_dbl(samp, IQR)) %>% 
  select(-samp) %>% 
  unnest(output)
```

    ## # A tibble: 4 × 4
    ##   name    mean    sd   iqr
    ##   <chr>  <dbl> <dbl> <dbl>
    ## 1 a     -0.442  3.60  4.35
    ## 2 b      5.75   4.40  6.23
    ## 3 c      2.83   9.57 13.4 
    ## 4 d      5.18   7.34 10.7

## NSDUH

This is a version of our function last time

``` r
nsduh_table_format = function(html, table_num) {
  
  out_table = 
    html %>% 
    html_table() %>% 
    nth(table_num) %>% 
    slice(-1) %>% 
    select(-contains("P Value"))
  
  return(out_table)
}
```

We need to import the html, and then extract the correct tables

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)
```

``` r
nsduh_table_format(html = nsduh_html, table_num = 1)
```

    ## # A tibble: 56 × 11
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 12.90a           13.36            13.28b             12.86             
    ##  2 Nort… 13.88a           14.66            13.98              13.51             
    ##  3 Midw… 12.40b           12.76            12.45              12.33             
    ##  4 South 11.24a           11.64            12.02              11.88             
    ##  5 West  15.27            15.62            15.53a             14.43             
    ##  6 Alab… 9.98             9.60             9.90               9.71              
    ##  7 Alas… 19.60a           21.92            17.30              18.44             
    ##  8 Ariz… 13.69            13.12            15.12              13.45             
    ##  9 Arka… 11.37            11.59            12.79              12.14             
    ## 10 Cali… 14.49            15.25            15.03              14.11             
    ## # ℹ 46 more rows
    ## # ℹ 6 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>

``` r
nsduh_table_format(html = nsduh_html, table_num = 4)
```

    ## # A tibble: 56 × 11
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 1.66a            1.76             0.60               0.64              
    ##  2 Nort… 1.94a            2.18             0.60               0.66              
    ##  3 Midw… 1.37             1.43             0.48               0.54              
    ##  4 South 1.45b            1.56             0.53               0.57              
    ##  5 West  2.03             2.05             0.82               0.85              
    ##  6 Alab… 1.23             1.22             0.42               0.41              
    ##  7 Alas… 1.54a            2.00             0.51               0.65              
    ##  8 Ariz… 2.25             2.29             1.01               0.85              
    ##  9 Arka… 0.93             1.07             0.41               0.48              
    ## 10 Cali… 2.14             2.16             0.89               0.94              
    ## # ℹ 46 more rows
    ## # ℹ 6 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>

``` r
nsduh_table_format(html = nsduh_html, table_num = 5)
```

    ## # A tibble: 56 × 11
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 0.30             0.33             0.12               0.10              
    ##  2 Nort… 0.43a            0.54             0.13               0.13              
    ##  3 Midw… 0.30             0.31             0.11               0.10              
    ##  4 South 0.27             0.26             0.12               0.08              
    ##  5 West  0.25             0.29             0.13               0.11              
    ##  6 Alab… 0.22             0.27             0.10               0.08              
    ##  7 Alas… 0.70a            1.23             0.11               0.08              
    ##  8 Ariz… 0.32a            0.55             0.17               0.20              
    ##  9 Arka… 0.19             0.17             0.10               0.07              
    ## 10 Cali… 0.20             0.20             0.13               0.09              
    ## # ℹ 46 more rows
    ## # ℹ 6 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>

``` r
nsduh_df = 
  tibble(
    drug = c("marj", "cocaine", "herion"),
    table_n = c(1, 4, 5)
  ) %>% 
  mutate(table = map(table_n, nsduh_table_format, html = nsduh_html)) %>% 
  unnest(table)

nsduh_df = 
  tibble(
    drug = c("marj", "cocaine", "herion"),
    table_n = c(1, 4, 5)
  ) %>% 
  mutate(table = map(table_n, 
                     \(x) nsduh_table_format(html = nsduh_html, table_num = x))) %>% 
  unnest(table)

nsduh_df %>% 
  filter(State == "New York")
```

    ## # A tibble: 3 × 13
    ##   drug    table_n State    `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)`
    ##   <chr>     <dbl> <chr>    <chr>            <chr>            <chr>             
    ## 1 marj          1 New York 14.24b           15.04            13.94             
    ## 2 cocaine       4 New York 2.28             2.54             0.71              
    ## 3 herion        5 New York 0.38a            0.52             0.13              
    ## # ℹ 7 more variables: `12-17(2014-2015)` <chr>, `18-25(2013-2014)` <chr>,
    ## #   `18-25(2014-2015)` <chr>, `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>,
    ## #   `18+(2013-2014)` <chr>, `18+(2014-2015)` <chr>

## Operations on nested data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USW00022534", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2021-01-01",
    date_max = "2022-12-31") |>
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USW00022534 = "Molokai_HI",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) |>
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: C:\Users\xlu12\AppData\Local/R/cache/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2024-09-26 10:19:39.017559 (8.668)

    ## file min/max dates: 1869-01-01 / 2024-09-30

    ## using cached file: C:\Users\xlu12\AppData\Local/R/cache/R/rnoaa/noaa_ghcnd/USW00022534.dly

    ## date created (size, mb): 2024-09-26 10:19:49.60742 (3.94)

    ## file min/max dates: 1949-10-01 / 2024-09-30

    ## using cached file: C:\Users\xlu12\AppData\Local/R/cache/R/rnoaa/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2024-09-26 10:19:53.198147 (1.038)

    ## file min/max dates: 1999-09-01 / 2024-09-30

Create a list column.

``` r
weather_nest = 
  weather_df %>% 
  nest(data = date:tmin)
```

``` r
weather_nest[["data"]][[1]]
```

    ## # A tibble: 730 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2021-01-01   157   4.4   0.6
    ##  2 2021-01-02    13  10.6   2.2
    ##  3 2021-01-03    56   3.3   1.1
    ##  4 2021-01-04     5   6.1   1.7
    ##  5 2021-01-05     0   5.6   2.2
    ##  6 2021-01-06     0   5     1.1
    ##  7 2021-01-07     0   5    -1  
    ##  8 2021-01-08     0   2.8  -2.7
    ##  9 2021-01-09     0   2.8  -4.3
    ## 10 2021-01-10     0   5    -1.6
    ## # ℹ 720 more rows

Let’s try regressing tmax on tmin.

``` r
lm(tmax ~ tmin, data = weather_nest[["data"]][[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest[["data"]][[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.514        1.034

``` r
lm(tmax ~ tmin, data = weather_nest[["data"]][[2]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest[["data"]][[2]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     21.7547       0.3222

``` r
lm(tmax ~ tmin, data = weather_nest[["data"]][[3]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest[["data"]][[3]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.532        1.137

Add regression result to a column.

``` r
weather_nest %>% 
  mutate(model_fit = map(data, \(x) lm(tmax ~ tmin, data = x)))
```

    ## # A tibble: 3 × 4
    ##   name           id          data               model_fit
    ##   <chr>          <chr>       <list>             <list>   
    ## 1 CentralPark_NY USW00094728 <tibble [730 × 4]> <lm>     
    ## 2 Molokai_HI     USW00022534 <tibble [730 × 4]> <lm>     
    ## 3 Waterhole_WA   USS0023B17S <tibble [730 × 4]> <lm>
