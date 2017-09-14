Homework 1
================

-   [Data](#data)
    -   [Read](#read)
    -   [Inspect](#inspect)
    -   [Normalize](#normalize)
    -   [Weighting](#weighting)
        -   [Assign Weights](#assign-weights)
        -   [Weight Data](#weight-data)

``` r
# load dependencies
library(tidyverse)
library(pander)
```

Data
====

Read
----

``` r
df <- as_tibble(read.csv("~/Development-Economics/data/index_test_data.csv"))
```

Inspect
-------

``` r
df %>%
  pander()
```

<table>
<colgroup>
<col width="18%" />
<col width="23%" />
<col width="22%" />
<col width="14%" />
<col width="21%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">City</th>
<th align="center">Pollution.index</th>
<th align="center">Cost.of.living</th>
<th align="center">Language</th>
<th align="center">Misc.Amenities</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">Shanghai</td>
<td align="center">80</td>
<td align="center">30</td>
<td align="center">9</td>
<td align="center">44</td>
</tr>
<tr class="even">
<td align="center">Mexico DF</td>
<td align="center">73</td>
<td align="center">17</td>
<td align="center">22</td>
<td align="center">29</td>
</tr>
<tr class="odd">
<td align="center">Panama City</td>
<td align="center">16</td>
<td align="center">20</td>
<td align="center">22</td>
<td align="center">32</td>
</tr>
<tr class="even">
<td align="center">Hanoi</td>
<td align="center">28</td>
<td align="center">14</td>
<td align="center">18</td>
<td align="center">25</td>
</tr>
<tr class="odd">
<td align="center">Singapore</td>
<td align="center">44</td>
<td align="center">70</td>
<td align="center">70</td>
<td align="center">71</td>
</tr>
<tr class="even">
<td align="center">Chittagong</td>
<td align="center">46</td>
<td align="center">11</td>
<td align="center">27</td>
<td align="center">14</td>
</tr>
</tbody>
</table>

Normalize
---------

"Normalization Process: Divide (cardinal number – minimum) by (maximum – minimum)"

``` r
normalize<-function(x){
   (x - min(x)) / (max(x) - min(x))
 }

norm.df <- df %>%
  mutate_if(is.numeric, funs(normalize))

norm.df %>%
  pander()
```

<table>
<colgroup>
<col width="18%" />
<col width="23%" />
<col width="22%" />
<col width="14%" />
<col width="21%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">City</th>
<th align="center">Pollution.index</th>
<th align="center">Cost.of.living</th>
<th align="center">Language</th>
<th align="center">Misc.Amenities</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">Shanghai</td>
<td align="center">1</td>
<td align="center">0.322</td>
<td align="center">0</td>
<td align="center">0.5263</td>
</tr>
<tr class="even">
<td align="center">Mexico DF</td>
<td align="center">0.8906</td>
<td align="center">0.1017</td>
<td align="center">0.2131</td>
<td align="center">0.2632</td>
</tr>
<tr class="odd">
<td align="center">Panama City</td>
<td align="center">0</td>
<td align="center">0.1525</td>
<td align="center">0.2131</td>
<td align="center">0.3158</td>
</tr>
<tr class="even">
<td align="center">Hanoi</td>
<td align="center">0.1875</td>
<td align="center">0.05085</td>
<td align="center">0.1475</td>
<td align="center">0.193</td>
</tr>
<tr class="odd">
<td align="center">Singapore</td>
<td align="center">0.4375</td>
<td align="center">1</td>
<td align="center">1</td>
<td align="center">1</td>
</tr>
<tr class="even">
<td align="center">Chittagong</td>
<td align="center">0.4688</td>
<td align="center">0</td>
<td align="center">0.2951</td>
<td align="center">0</td>
</tr>
</tbody>
</table>

Weighting
---------

### Assign Weights

``` r
# weights
pollution.index.wt <- 0.25
cost.of.living.wt <- 0.2
language.wt <- 0.15
misc.amen.wt <- 0.4

weights <- c(pollution.index.wt,
             cost.of.living.wt,
             language.wt,
             misc.amen.wt)

# confirm weights sum to 1
sum(weights) == 1
```

    ## [1] TRUE

### Weight Data

``` r
weighted.norm.df <- norm.df %>%
  mutate(Pollution.index = Pollution.index * pollution.index.wt) %>%
  mutate(Cost.of.living = Cost.of.living * cost.of.living.wt) %>%
  mutate(Language = Language * language.wt) %>%
  mutate(Misc.Amenities = Misc.Amenities * misc.amen.wt)

weighted.norm.df %>%
  pander()
```

<table>
<colgroup>
<col width="18%" />
<col width="23%" />
<col width="22%" />
<col width="14%" />
<col width="21%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">City</th>
<th align="center">Pollution.index</th>
<th align="center">Cost.of.living</th>
<th align="center">Language</th>
<th align="center">Misc.Amenities</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">Shanghai</td>
<td align="center">0.25</td>
<td align="center">0.06441</td>
<td align="center">0</td>
<td align="center">0.2105</td>
</tr>
<tr class="even">
<td align="center">Mexico DF</td>
<td align="center">0.2227</td>
<td align="center">0.02034</td>
<td align="center">0.03197</td>
<td align="center">0.1053</td>
</tr>
<tr class="odd">
<td align="center">Panama City</td>
<td align="center">0</td>
<td align="center">0.03051</td>
<td align="center">0.03197</td>
<td align="center">0.1263</td>
</tr>
<tr class="even">
<td align="center">Hanoi</td>
<td align="center">0.04688</td>
<td align="center">0.01017</td>
<td align="center">0.02213</td>
<td align="center">0.07719</td>
</tr>
<tr class="odd">
<td align="center">Singapore</td>
<td align="center">0.1094</td>
<td align="center">0.2</td>
<td align="center">0.15</td>
<td align="center">0.4</td>
</tr>
<tr class="even">
<td align="center">Chittagong</td>
<td align="center">0.1172</td>
<td align="center">0</td>
<td align="center">0.04426</td>
<td align="center">0</td>
</tr>
</tbody>
</table>

``` r
weighted.norm.df %>%
  filter(City %in% "Shanghai") %>%
  pander()
```

<table>
<colgroup>
<col width="15%" />
<col width="24%" />
<col width="23%" />
<col width="15%" />
<col width="21%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">City</th>
<th align="center">Pollution.index</th>
<th align="center">Cost.of.living</th>
<th align="center">Language</th>
<th align="center">Misc.Amenities</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">Shanghai</td>
<td align="center">0.25</td>
<td align="center">0.06441</td>
<td align="center">0</td>
<td align="center">0.2105</td>
</tr>
</tbody>
</table>

``` r
emphasize.strong.rows(which(weighted.norm.df$City == "Shanghai", arr.ind = TRUE))

pander(weighted.norm.df)
```

<table>
<colgroup>
<col width="19%" />
<col width="23%" />
<col width="22%" />
<col width="14%" />
<col width="20%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">City</th>
<th align="center">Pollution.index</th>
<th align="center">Cost.of.living</th>
<th align="center">Language</th>
<th align="center">Misc.Amenities</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center"><strong>Shanghai</strong></td>
<td align="center"><strong>0.25</strong></td>
<td align="center"><strong>0.06441</strong></td>
<td align="center"><strong>0</strong></td>
<td align="center"><strong>0.2105</strong></td>
</tr>
<tr class="even">
<td align="center">Mexico DF</td>
<td align="center">0.2227</td>
<td align="center">0.02034</td>
<td align="center">0.03197</td>
<td align="center">0.1053</td>
</tr>
<tr class="odd">
<td align="center">Panama City</td>
<td align="center">0</td>
<td align="center">0.03051</td>
<td align="center">0.03197</td>
<td align="center">0.1263</td>
</tr>
<tr class="even">
<td align="center">Hanoi</td>
<td align="center">0.04688</td>
<td align="center">0.01017</td>
<td align="center">0.02213</td>
<td align="center">0.07719</td>
</tr>
<tr class="odd">
<td align="center">Singapore</td>
<td align="center">0.1094</td>
<td align="center">0.2</td>
<td align="center">0.15</td>
<td align="center">0.4</td>
</tr>
<tr class="even">
<td align="center">Chittagong</td>
<td align="center">0.1172</td>
<td align="center">0</td>
<td align="center">0.04426</td>
<td align="center">0</td>
</tr>
</tbody>
</table>
