HW1
================
Team Syria

-   [Data](#data)
    -   [Read and Inspect Raw Data](#read-and-inspect-raw-data)
    -   [Normalize](#normalize)
        -   [Normalize Formula](#normalize-formula)
    -   [Weight](#weight)
    -   [Mutate](#mutate)
    -   [Score](#score)
    -   [Data Restructuring](#data-restructuring)
-   [Summary Table](#summary-table)
-   [Map](#map)

<!-- <style> -->
<!--     body .main-container { -->
<!--         max-width: 90%; -->
<!--     } -->
<!-- </style> -->
Environment Prep

``` r
# load packages
library(tidyverse)
library(readxl)
library(pander)
library(scales)
```

``` r
# set options so tables render nicely
pander::panderOptions('table.split.table', Inf)
```

Data
====

Data were retrieved from:

-   [World Bank Data](https://data.worldbank.org)
    -   Improved water source (% of population with access)
    -   Life expectancy at birth, total (years)
    -   Urban population growth (annual %)
-   [UNDP Human Development Reports](http://hdr.undp.org/en/data)
    -   Adult Literacy Rate (% Ages 15 and older)
    -   Population with at least some secondary education (% ages 25 and older)
    -   Mean years of schooling (years)
    -   Inequality in education (%)
    -   GNI per capita (2011 PPP$)

Read and Inspect Raw Data
-------------------------

We read the excel file containing our raw data and store it in a `data_frame` variable named `combo`.

``` r
combo <- read_excel("~/Development-Economics/data/combo.xlsx")
```

We then inspect our `combo` data.

``` r
pander(combo)
```

<table>
<colgroup>
<col width="5%" />
<col width="10%" />
<col width="9%" />
<col width="9%" />
<col width="11%" />
<col width="11%" />
<col width="9%" />
<col width="10%" />
<col width="10%" />
<col width="11%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Country</th>
<th align="center">Improved water source (% of population with access)</th>
<th align="center">Life expectancy at birth, total (years)</th>
<th align="center">Urban population growth (annual %)</th>
<th align="center">Adult Literacy Rate (% Ages 15 and older)</th>
<th align="center">Population with at least some secondary education (% ages 25 and older)</th>
<th align="center">Mean years of schooling (years)</th>
<th align="center">Inequality in education (%)</th>
<th align="center">GNI per capita (2011 PPP$)</th>
<th align="center">Press Freedom Score (0 worst - 100 best)</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">Cuba</td>
<td align="center">94.6</td>
<td align="center">79.39</td>
<td align="center">0.369</td>
<td align="center">99.7</td>
<td align="center">84.8</td>
<td align="center">11.8</td>
<td align="center">11.3</td>
<td align="center">7280</td>
<td align="center">90</td>
</tr>
<tr class="even">
<td align="center">Ghana</td>
<td align="center">87.6</td>
<td align="center">62.11</td>
<td align="center">3.55</td>
<td align="center">76.6</td>
<td align="center">59.8</td>
<td align="center">6.9</td>
<td align="center">36.7</td>
<td align="center">3472</td>
<td align="center">28</td>
</tr>
<tr class="odd">
<td align="center">India</td>
<td align="center">94.1</td>
<td align="center">68.05</td>
<td align="center">2.345</td>
<td align="center">72.1</td>
<td align="center">48.7</td>
<td align="center">6.1</td>
<td align="center">42.1</td>
<td align="center">5329</td>
<td align="center">39</td>
</tr>
<tr class="even">
<td align="center">Mexico</td>
<td align="center">96.1</td>
<td align="center">76.7</td>
<td align="center">1.721</td>
<td align="center">94.4</td>
<td align="center">57.4</td>
<td align="center">8.4</td>
<td align="center">19.7</td>
<td align="center">16154</td>
<td align="center">61</td>
</tr>
<tr class="odd">
<td align="center">Russia</td>
<td align="center">96.9</td>
<td align="center">70.74</td>
<td align="center">0.3165</td>
<td align="center">99.7</td>
<td align="center">94.6</td>
<td align="center">12</td>
<td align="center">2.3</td>
<td align="center">24067</td>
<td align="center">81</td>
</tr>
<tr class="even">
<td align="center">South Africa</td>
<td align="center">92.8</td>
<td align="center">60.95</td>
<td align="center">2.35</td>
<td align="center">94.3</td>
<td align="center">74.9</td>
<td align="center">10.3</td>
<td align="center">16.1</td>
<td align="center">12113</td>
<td align="center">33</td>
</tr>
<tr class="odd">
<td align="center">Syria</td>
<td align="center">90.1</td>
<td align="center">70.16</td>
<td align="center">-2.406</td>
<td align="center">86.4</td>
<td align="center">38.9</td>
<td align="center">5.6</td>
<td align="center">31.5</td>
<td align="center">2905</td>
<td align="center">89</td>
</tr>
<tr class="even">
<td align="center">Thailand</td>
<td align="center">97.8</td>
<td align="center">74.86</td>
<td align="center">2.936</td>
<td align="center">96.7</td>
<td align="center">43.3</td>
<td align="center">7.9</td>
<td align="center">16.1</td>
<td align="center">14169</td>
<td align="center">64</td>
</tr>
</tbody>
</table>

Normalize
---------

### Normalize Formula

In order to normalize our values to a `0` to `1` scale, we use the following formula:

> Normalization Process: Divide (cardinal number – minimum) by (maximum – minimum)

We create a function called `normalize` to achieve this result, which also rounds our values to 2 digits following the decimal point.

``` r
normalize <- function(x){
  round((x - min(x)) / (max(x) - min(x)), digits = 2)
}
```

Weight
------

We also create a function to weight our variables, simply called `weight`.

``` r
# weight <- function(x){
  # x <- x * -1
# }
```

Mutate
------

We take our `normalize` and `weight` functions and apply them to our `combo` data, and store the results in a new `data_frame` called `normalize_weighted`. Since `Country` is a non-numeric column, `mutate_if` allows us to only apply our functions to columns where `is.numeric()` returns `TRUE`.

``` r
combo_norm <- combo %>%
  mutate_if(is.numeric, funs(normalize))
```

Score
-----

By summing our normalized and weighted variables, we create our `Score` data.

``` r
Score <- combo_norm %>%
  group_by(Country) %>%
  summarise(Score = sum(`Improved water source (% of population with access)`,
                      `Life expectancy at birth, total (years)`,
                      `Urban population growth (annual %)`,
                      `Adult Literacy Rate (% Ages 15 and older)`,
                      `Population with at least some secondary education (% ages 25 and older)`,
                      `Mean years of schooling (years)`,
                      `Inequality in education (%)`,
                      `GNI per capita (2011 PPP$)`,
                      `Press Freedom Score (0 worst - 100 best)`))

pander(Score)
```

<table style="width:31%;">
<colgroup>
<col width="20%" />
<col width="9%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Country</th>
<th align="center">Score</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">Cuba</td>
<td align="center">6.39</td>
</tr>
<tr class="even">
<td align="center">Ghana</td>
<td align="center">2.69</td>
</tr>
<tr class="odd">
<td align="center">India</td>
<td align="center">3.37</td>
</tr>
<tr class="even">
<td align="center">Mexico</td>
<td align="center">5.55</td>
</tr>
<tr class="odd">
<td align="center">Russia</td>
<td align="center">6.75</td>
</tr>
<tr class="even">
<td align="center">South Africa</td>
<td align="center">4.36</td>
</tr>
<tr class="odd">
<td align="center">Syria</td>
<td align="center">2.98</td>
</tr>
<tr class="even">
<td align="center">Thailand</td>
<td align="center">5.44</td>
</tr>
</tbody>
</table>

With our `Score` data, we can then create our `Rank` data.

``` r
Score_Rank <- Score %>%
  mutate(Rank = dense_rank(desc(Score)))
```

We then inspect our `Score_Rank` data.

``` r
pander(Score_Rank)
```

<table style="width:43%;">
<colgroup>
<col width="20%" />
<col width="11%" />
<col width="11%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Country</th>
<th align="center">Score</th>
<th align="center">Rank</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">Cuba</td>
<td align="center">6.39</td>
<td align="center">2</td>
</tr>
<tr class="even">
<td align="center">Ghana</td>
<td align="center">2.69</td>
<td align="center">8</td>
</tr>
<tr class="odd">
<td align="center">India</td>
<td align="center">3.37</td>
<td align="center">6</td>
</tr>
<tr class="even">
<td align="center">Mexico</td>
<td align="center">5.55</td>
<td align="center">3</td>
</tr>
<tr class="odd">
<td align="center">Russia</td>
<td align="center">6.75</td>
<td align="center">1</td>
</tr>
<tr class="even">
<td align="center">South Africa</td>
<td align="center">4.36</td>
<td align="center">5</td>
</tr>
<tr class="odd">
<td align="center">Syria</td>
<td align="center">2.98</td>
<td align="center">7</td>
</tr>
<tr class="even">
<td align="center">Thailand</td>
<td align="center">5.44</td>
<td align="center">4</td>
</tr>
</tbody>
</table>

The example format that we have seen looks like so:

![Alt text](https://github.com/DocSynaptogenesis/Development-Economics/blob/master/HW/images/trial_run_screencap.png)

To match this we need to convert our `data_frame` to a `matrix`, transpose the data, and store it in a variable that we'll call `Score`.

``` r
Score_Rank <- Score_Rank %>%
  as.matrix() %>%
  t()

pander(Score_Rank)
```

<table>
<colgroup>
<col width="15%" />
<col width="7%" />
<col width="9%" />
<col width="9%" />
<col width="10%" />
<col width="10%" />
<col width="17%" />
<col width="9%" />
<col width="11%" />
</colgroup>
<tbody>
<tr class="odd">
<td align="center"><strong>Country</strong></td>
<td align="center">Cuba</td>
<td align="center">Ghana</td>
<td align="center">India</td>
<td align="center">Mexico</td>
<td align="center">Russia</td>
<td align="center">South Africa</td>
<td align="center">Syria</td>
<td align="center">Thailand</td>
</tr>
<tr class="even">
<td align="center"><strong>Score</strong></td>
<td align="center">6.39</td>
<td align="center">2.69</td>
<td align="center">3.37</td>
<td align="center">5.55</td>
<td align="center">6.75</td>
<td align="center">4.36</td>
<td align="center">2.98</td>
<td align="center">5.44</td>
</tr>
<tr class="odd">
<td align="center"><strong>Rank</strong></td>
<td align="center">2</td>
<td align="center">8</td>
<td align="center">6</td>
<td align="center">3</td>
<td align="center">1</td>
<td align="center">5</td>
<td align="center">7</td>
<td align="center">4</td>
</tr>
</tbody>
</table>

We're then going to detach the `Country` row so that we can bind the `Score` and `Rank` rows to our summary table.

``` r
Score_Rank <- Score_Rank[-1, ]

Score_Rank
```

    ##       [,1]   [,2]   [,3]   [,4]   [,5]   [,6]   [,7]   [,8]  
    ## Score "6.39" "2.69" "3.37" "5.55" "6.75" "4.36" "2.98" "5.44"
    ## Rank  "2"    "8"    "6"    "3"    "1"    "5"    "7"    "4"

Data Restructuring
------------------

In order bold the column for Syria, we use the following:

``` r
emphasize.strong.cols(which(combo_norm$Country == "Syria",
                            arr.ind = TRUE))
```

To produce the desired table, we convert our `combo_norm` variable to a matrix, which we then transpose and store in a variable called `combo_norm_matr_trans`.

``` r
combo_norm_matr_trans <- combo_norm %>%
  as.matrix() %>%
  t()
```

Next, we bind `Score_Rank` to `combo_norm_matr_trans` by row and store the result in a variable called `combo_matrix`.

``` r
combo_matrix <- rbind(combo_norm_matr_trans, Score_Rank)
```

Before viewing the result, we bold our `Score` and `Rank` rows.

``` r
emphasize.strong.rows(c(1, nrow(combo_matrix), nrow(combo_matrix) - 1))
```

Summary Table
=============

``` r
pander(combo_matrix)
```

<table>
<colgroup>
<col width="23%" />
<col width="7%" />
<col width="8%" />
<col width="8%" />
<col width="9%" />
<col width="9%" />
<col width="13%" />
<col width="8%" />
<col width="10%" />
</colgroup>
<tbody>
<tr class="odd">
<td align="center"><strong>Country</strong></td>
<td align="center"><strong>Cuba</strong></td>
<td align="center"><strong>Ghana</strong></td>
<td align="center"><strong>India</strong></td>
<td align="center"><strong>Mexico</strong></td>
<td align="center"><strong>Russia</strong></td>
<td align="center"><strong>South Africa</strong></td>
<td align="center"><strong>Syria</strong></td>
<td align="center"><strong>Thailand</strong></td>
</tr>
<tr class="even">
<td align="center"><strong>Improved water source (% of population with access)</strong></td>
<td align="center">0.69</td>
<td align="center">0.00</td>
<td align="center">0.64</td>
<td align="center">0.83</td>
<td align="center">0.91</td>
<td align="center">0.51</td>
<td align="center"><strong>0.25</strong></td>
<td align="center">1.00</td>
</tr>
<tr class="odd">
<td align="center"><strong>Life expectancy at birth, total (years)</strong></td>
<td align="center">1.00</td>
<td align="center">0.06</td>
<td align="center">0.38</td>
<td align="center">0.85</td>
<td align="center">0.53</td>
<td align="center">0.00</td>
<td align="center"><strong>0.50</strong></td>
<td align="center">0.75</td>
</tr>
<tr class="even">
<td align="center"><strong>Urban population growth (annual %)</strong></td>
<td align="center">0.47</td>
<td align="center">1.00</td>
<td align="center">0.80</td>
<td align="center">0.69</td>
<td align="center">0.46</td>
<td align="center">0.80</td>
<td align="center"><strong>0.00</strong></td>
<td align="center">0.90</td>
</tr>
<tr class="odd">
<td align="center"><strong>Adult Literacy Rate (% Ages 15 and older)</strong></td>
<td align="center">1.00</td>
<td align="center">0.16</td>
<td align="center">0.00</td>
<td align="center">0.81</td>
<td align="center">1.00</td>
<td align="center">0.80</td>
<td align="center"><strong>0.52</strong></td>
<td align="center">0.89</td>
</tr>
<tr class="even">
<td align="center"><strong>Population with at least some secondary education (% ages 25 and older)</strong></td>
<td align="center">0.82</td>
<td align="center">0.38</td>
<td align="center">0.18</td>
<td align="center">0.33</td>
<td align="center">1.00</td>
<td align="center">0.65</td>
<td align="center"><strong>0.00</strong></td>
<td align="center">0.08</td>
</tr>
<tr class="odd">
<td align="center"><strong>Mean years of schooling (years)</strong></td>
<td align="center">0.97</td>
<td align="center">0.20</td>
<td align="center">0.08</td>
<td align="center">0.44</td>
<td align="center">1.00</td>
<td align="center">0.73</td>
<td align="center"><strong>0.00</strong></td>
<td align="center">0.36</td>
</tr>
<tr class="even">
<td align="center"><strong>Inequality in education (%)</strong></td>
<td align="center">0.23</td>
<td align="center">0.86</td>
<td align="center">1.00</td>
<td align="center">0.44</td>
<td align="center">0.00</td>
<td align="center">0.35</td>
<td align="center"><strong>0.73</strong></td>
<td align="center">0.35</td>
</tr>
<tr class="odd">
<td align="center"><strong>GNI per capita (2011 PPP$)</strong></td>
<td align="center">0.21</td>
<td align="center">0.03</td>
<td align="center">0.11</td>
<td align="center">0.63</td>
<td align="center">1.00</td>
<td align="center">0.44</td>
<td align="center"><strong>0.00</strong></td>
<td align="center">0.53</td>
</tr>
<tr class="even">
<td align="center"><strong>Press Freedom Score (0 worst - 100 best)</strong></td>
<td align="center">1.00</td>
<td align="center">0.00</td>
<td align="center">0.18</td>
<td align="center">0.53</td>
<td align="center">0.85</td>
<td align="center">0.08</td>
<td align="center"><strong>0.98</strong></td>
<td align="center">0.58</td>
</tr>
<tr class="odd">
<td align="center"><strong>Score</strong></td>
<td align="center"><strong>6.39</strong></td>
<td align="center"><strong>2.69</strong></td>
<td align="center"><strong>3.37</strong></td>
<td align="center"><strong>5.55</strong></td>
<td align="center"><strong>6.75</strong></td>
<td align="center"><strong>4.36</strong></td>
<td align="center"><strong>2.98</strong></td>
<td align="center"><strong>5.44</strong></td>
</tr>
<tr class="even">
<td align="center"><strong>Rank</strong></td>
<td align="center"><strong>2</strong></td>
<td align="center"><strong>8</strong></td>
<td align="center"><strong>6</strong></td>
<td align="center"><strong>3</strong></td>
<td align="center"><strong>1</strong></td>
<td align="center"><strong>5</strong></td>
<td align="center"><strong>7</strong></td>
<td align="center"><strong>4</strong></td>
</tr>
</tbody>
</table>

Map
===

To faciliate understanding the data, we can visualize it with a map.

``` r
map_world <- map_data(map = "world")

index_countries <- combo_norm %>%
  group_by(Country) %>%
  summarise(`Index Sum` = sum(`Improved water source (% of population with access)`,
                    `Life expectancy at birth, total (years)`,
                    `Urban population growth (annual %)`,
                    `Adult Literacy Rate (% Ages 15 and older)`,
                    `Population with at least some secondary education (% ages 25 and older)`,
                    `Mean years of schooling (years)`,
                    `Inequality in education (%)`,
                    `GNI per capita (2011 PPP$)`,
                    `Press Freedom Score (0 worst - 100 best)`)) %>%
  inner_join(map_world, by = c("Country" = "region")) 

nonindex_countries <- map_world %>%
  anti_join(combo_norm, by = c("region" = "Country"))


index_map <- NULL
```

``` r
index_map <- ggplot() +
  geom_polygon(data = index_countries,
               aes(x = long, y = lat,
                   group = group,
                   fill = `Index Sum`),  size = 0.01) +
  geom_polygon(data = nonindex_countries,
               aes(x = long, y = lat, group = group),
               fill = "white",
               size = 0.1) +
  coord_map() +
  scale_fill_distiller(type = "seq",
                       palette = "RdBu",
                       breaks = pretty_breaks(n=10), 
                       direction = 1) +
  coord_fixed(ratio = 1.5) +
  guides(fill = guide_legend(keywidth = 1, keyheight = 3, reverse = TRUE)) +
  ggtitle("Development Index") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
        )
```

``` r
index_map
```

![](HW1_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-20-1.png)
