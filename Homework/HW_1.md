Homework 1
================
Team Syria

-   [Data](#data)
    -   [Extract](#extract)
    -   [Read](#read)
    -   [Clean](#clean)
    -   [Normalize](#normalize)
    -   [Weight](#weight)
    -   [Score from sums](#score-from-sums)
    -   [Prep Output](#prep-output)
    -   [Summary Table](#summary-table)

<!-- <style> -->
<!--     body .main-container { -->
<!--         max-width: 90%; -->
<!--     } -->
<!-- </style> -->
<!-- <style> -->
<!-- th, td { -->
<!--     border-bottom: 1px solid black; -->
<!--     border-right: 1px solid black; -->
<!-- } -->
<!-- </style> -->
``` r
library(googledrive)
library(tidyverse)
library(readxl)
library(lubridate)
library(pander)
```

Data
====

Extract
-------

Data downloaded from [World Bank Data](https://data.worldbank.org) and stored in Google Drive folder.

``` r
# not run, file already in place
# drive_download(file = "WBD_WDI_Main.xlsx",
#                path = "~/Development-Economics/data/WBD_WDI_Main",
#                type = "xlsx")
```

Read
----

``` r
WBD <- read_excel("~/Development-Economics/data/WBD_WDI_Main.xlsx", sheet = "Data") %>%
  select(-`Time Code`, -`Country Code`)
```

Clean
-----

``` r
# header cleaner function
WBD_cleaner <- function(x) {
  new_df <- x %>%
    rename(`Adolescent fertility rate` = 
             `Adolescent fertility rate (births per 1,000 women ages 15-19) [SP.ADO.TFRT]`) %>%
    rename(`Agriculture (% of GDP)` = `Agriculture, value added (% of GDP) [NV.AGR.TOTL.ZS]`) %>%
    rename(`Freshwater withdrawals` = 
             `Annual freshwater withdrawals, total (% of internal resources) [ER.H2O.FWTL.ZS]`) %>%
    rename(`Births w/ skilled health staff` = 
             `Births attended by skilled health staff (% of total) [SH.STA.BRTC.ZS]`) %>%
    rename(`CO2 emissions` = `CO2 emissions (metric tons per capita) [EN.ATM.CO2E.PC]`) %>%
    rename(`Contraceptive prevalence` = 
             `Contraceptive prevalence, any methods (% of women ages 15-49) [SP.DYN.CONU.ZS]`) %>%
    rename(`Domestic credit from financial sector` = 
             `Domestic credit provided by financial sector (% of GDP) [FS.AST.DOMS.GD.ZS]`) %>%
    rename(`Electric power consumption` = `Electric power consumption (kWh per capita) [EG.USE.ELEC.KH.PC]`) %>%
    rename(`Exports of goods and services` = `Exports of goods and services (% of GDP) [NE.EXP.GNFS.ZS]`) %>%
    rename(`External debt stocks` = `External debt stocks, total (DOD, current US$) [DT.DOD.DECT.CD]`) %>%
    rename(`Fertility rate` = `Fertility rate, total (births per woman) [SP.DYN.TFRT.IN]`) %>%
    rename(`FDI` = `Foreign direct investment, net inflows (BoP, current US$) [BX.KLT.DINV.CD.WD]`) %>%
    rename(`Forest area` = `Forest area (sq. km) [AG.LND.FRST.K2]`) %>%
    rename(`GDP` = `GDP (current US$) [NY.GDP.MKTP.CD]`) %>%
    rename(`GDP growth` = `GDP growth (annual %) [NY.GDP.MKTP.KD.ZG]`) %>%
    rename(`GNI per capita, Atlas` = `GNI per capita, Atlas method (current US$) [NY.GNP.PCAP.CD]`) %>%
    rename(`GNI per capita, PPP` = `GNI per capita, PPP (current international $) [NY.GNP.PCAP.PP.CD]`) %>%
    rename(`GNI, Atlas` = `GNI, Atlas method (current US$) [NY.GNP.ATLS.CD]`) %>%
    rename(`GNI, PPP ` = `GNI, PPP (current international $) [NY.GNP.MKTP.PP.CD]`) %>%
    rename(`Gross capital formation` = `Gross capital formation (% of GDP) [NE.GDI.TOTL.ZS]`) %>%
    rename(`High-tech exports` = `High-technology exports (% of manufactured exports) [TX.VAL.TECH.MF.ZS]`) %>%
    rename(`Immunization, measles` = `Immunization, measles (% of children ages 12-23 months) [SH.IMM.MEAS]`) %>%
    rename(`Imports of goods and services` = `Imports of goods and services (% of GDP) [NE.IMP.GNFS.ZS]`) %>%
    rename(`Improved sanitation facilities` = 
           `Improved sanitation facilities (% of population with access) [SH.STA.ACSN]`) %>%
    rename(`Improved water source` = `Improved water source (% of population with access) [SH.H2O.SAFE.ZS]`) %>%
    rename(`Income share held by lowest 20%` = `Income share held by lowest 20% [SI.DST.FRST.20]`) %>%
    rename(`Industry, value added` = `Industry, value added (% of GDP) [NV.IND.TOTL.ZS]`) %>%
    rename(`Inflation, GDP deflator` = `Inflation, GDP deflator (annual %) [NY.GDP.DEFL.KD.ZG]`) %>%
    rename(`Life expectancy at birth` = `Life expectancy at birth, total (years) [SP.DYN.LE00.IN]`) %>%
    rename(`Merchandise trade` = `Merchandise trade (% of GDP) [TG.VAL.TOTL.GD.ZS]`) %>%
    rename(`Military expenditure` = `Military expenditure (% of GDP) [MS.MIL.XPND.GD.ZS]`) %>%
    rename(`Mobile cellular subscriptions` = `Mobile cellular subscriptions (per 100 people) [IT.CEL.SETS.P2]`) %>%
    rename(`Mortality rate, under 5` = `Mortality rate, under-5 (per 1,000 live births) [SH.DYN.MORT]`) %>%
    rename(`Development assistance and official aid received` = `Net official development assistance and official aid received (current US$) [DT.ODA.ALLD.CD]`) %>%
    rename(`Overall level of statistical capacity` = 
             `Overall level of statistical capacity (scale 0 - 100) [IQ.SCI.OVRL]`) %>%
    rename(`Personal remittances, received` = `Personal remittances, received (current US$) [BX.TRF.PWKR.CD.DT]`) %>%
    rename(`Population density` = `Population density (people per sq. km of land area) [EN.POP.DNST]`) %>%
    rename(`Population growth` = `Population growth (annual %) [SP.POP.GROW]`) %>%
    rename(`Population` = `Population, total [SP.POP.TOTL]`) %>%
    rename(`Prevalence of HIV` = `Prevalence of HIV, total (% of population ages 15-49) [SH.DYN.AIDS.ZS]`) %>%
    rename(`Primary completion rate` = `Primary completion rate, total (% of relevant age group) [SE.PRM.CMPT.ZS]`) %>%
    rename(`Revenue, excluding grants` = `Revenue, excluding grants (% of GDP) [GC.REV.XGRT.GD.ZS]`) %>%
    rename(`School enrollment, primary` = `School enrollment, primary (% gross) [SE.PRM.ENRR]`) %>%
    rename(`School enrollment, primary and secondary` = 
             `School enrollment, primary and secondary (gross), gender parity index (GPI) [SE.ENR.PRSC.FM.ZS]`) %>%
    rename(`School enrollment, secondary` = `School enrollment, secondary (% gross) [SE.SEC.ENRR]`) %>%
    rename(`Services` = `Services, etc., value added (% of GDP) [NV.SRV.TETC.ZS]`) %>%
    rename(`Tax revenue` = `Tax revenue (% of GDP) [GC.TAX.TOTL.GD.ZS]`) %>%
    rename(`Time required to start a business` = `Time required to start a business (days) [IC.REG.DURS]`) %>%
    rename(`Total debt service` = 
             `Total debt service (% of exports of goods, services and primary income) [DT.TDS.DECT.EX.ZS]`) %>%
    rename(`Urban population growth` = `Urban population growth (annual %) [SP.URB.GROW]`) %>%
    rename(Year = Time) %>%
    rename(`Energy use` = `Energy use (kg of oil equivalent per capita) [EG.USE.PCAP.KG.OE]`) %>%
    rename(`Protected areas` = `Terrestrial and marine protected areas (% of total territorial area) [ER.PTD.TOTL.ZS]`) %>%
    rename(`Net barter terms of trade` = `Net barter terms of trade index (2000 = 100) [TT.PRI.MRCH.XD.WD]`) %>%
    rename(`Surface area` = `Surface area (sq. km) [AG.SRF.TOTL.K2]`)

  return(new_df)
}
```

``` r
WBD_clean <- WBD %>%
  WBD_cleaner()
```

Normalize
---------

"Normalization Process: Divide (cardinal number – minimum) by (maximum – minimum)"

``` r
# round results to 2 digits
normalize <- function(x){
  round((x - min(x)) / (max(x) - min(x)), digits = 2)
 }

weight_negatively <- function(x){
  x <- x * -1
}

WBD_norm <- WBD_clean %>%
  filter(Year %in% "2014") %>%
  mutate_if(is.numeric, funs(normalize)) %>%
  mutate(`CO2 emissions` = weight_negatively(`CO2 emissions`)) %>%
  mutate(`CO2 emissions` = weight_negatively(`Electric power consumption`)) %>%
  mutate(`Energy use` = weight_negatively(`Energy use`)) %>%
  mutate(`Mortality rate, under 5` = weight_negatively(`Mortality rate, under 5`))
```

Weight
------

``` r
# example

# dummy weight assignment
weights <- c(0.05, 0.05, 0.05, 0.05, 0.05,
             0.05, 0.05, 0.05, 0.05, 0.05,
             0.05, 0.05, 0.05, 0.05, 0.05,
             0.05, 0.05, 0.05, 0.05, 0.05)

# confirm weights sum to 1
sum(weights) == 1
```

    ## [1] TRUE

``` r
weighter <- function(x){
  x <- x * 0.05
}

WBD_weighted <- WBD_norm %>%
  mutate_if(is.numeric, funs(weighter))
```

Score from sums
---------------

``` r
Score <- WBD_weighted %>%
# Score <- WBD_norm %>%
# WBD_sums <- WBD_norm %>%
  group_by(`Country Name`) %>%
  summarise(sum = sum(`Adolescent fertility rate`,
                      `Freshwater withdrawals`,
                      `CO2 emissions`,
                      `Electric power consumption`,
                      `Energy use`,
                      `Fertility rate`,
                      `Forest area`,
                      `Immunization, measles`,
                      `Improved sanitation facilities`,
                      `Improved water source`,
                      `Life expectancy at birth`,
                      `Mobile cellular subscriptions`,
                      `Mortality rate, under 5`,
                      `Population density`,
                      `Population growth`,
                      `Population`,
                      `Protected areas`,
                      `Urban population growth`
                      )) %>%
  as.matrix() %>%
  t()

Score <- Score[-1,]
```

Prep Output
-----------

``` r
pander::panderOptions('table.split.table', Inf)

emphasize.strong.cols(which(WBD_norm$`Country Name` == "Syrian Arab Republic",
                            arr.ind = TRUE))

WBD.matrix.t <- WBD_norm %>%
  as.matrix() %>%
  t() %>%
  na.omit() 

combo.matrix <- rbind(WBD.matrix.t, Score)

# emphasize.strong.rows(1)
emphasize.strong.rows(c(1, nrow(combo.matrix)))
```

Summary Table
-------------

``` r
pander(combo.matrix)
```

<table>
<colgroup>
<col width="19%" />
<col width="7%" />
<col width="7%" />
<col width="7%" />
<col width="7%" />
<col width="14%" />
<col width="11%" />
<col width="15%" />
<col width="8%" />
</colgroup>
<tbody>
<tr class="odd">
<td align="center"><strong>Country Name</strong></td>
<td align="center"><strong>Cuba</strong></td>
<td align="center"><strong>Ghana</strong></td>
<td align="center"><strong>India</strong></td>
<td align="center"><strong>Mexico</strong></td>
<td align="center"><strong>Russian Federation</strong></td>
<td align="center"><strong>South Africa</strong></td>
<td align="center"><strong>Syrian Arab Republic</strong></td>
<td align="center"><strong>Thailand</strong></td>
</tr>
<tr class="even">
<td align="center"><strong>Adolescent fertility rate</strong></td>
<td align="center">0.51</td>
<td align="center">1.00</td>
<td align="center">0.04</td>
<td align="center">0.91</td>
<td align="center">0.00</td>
<td align="center">0.52</td>
<td align="center"><strong>0.37</strong></td>
<td align="center">0.47</td>
</tr>
<tr class="odd">
<td align="center"><strong>Freshwater withdrawals</strong></td>
<td align="center">0.07</td>
<td align="center">0.01</td>
<td align="center">0.22</td>
<td align="center">0.08</td>
<td align="center">0.00</td>
<td align="center">0.14</td>
<td align="center"><strong>1.00</strong></td>
<td align="center">0.10</td>
</tr>
<tr class="even">
<td align="center"><strong>CO2 emissions</strong></td>
<td align="center">-0.17</td>
<td align="center">0.00</td>
<td align="center">-0.07</td>
<td align="center">-0.28</td>
<td align="center">-1.00</td>
<td align="center">-0.62</td>
<td align="center"><strong>-0.10</strong></td>
<td align="center">-0.35</td>
</tr>
<tr class="odd">
<td align="center"><strong>Electric power consumption</strong></td>
<td align="center">0.17</td>
<td align="center">0.00</td>
<td align="center">0.07</td>
<td align="center">0.28</td>
<td align="center">1.00</td>
<td align="center">0.62</td>
<td align="center"><strong>0.10</strong></td>
<td align="center">0.35</td>
</tr>
<tr class="even">
<td align="center"><strong>Energy use</strong></td>
<td align="center">-0.15</td>
<td align="center">0.00</td>
<td align="center">-0.07</td>
<td align="center">-0.26</td>
<td align="center">-1.00</td>
<td align="center">-0.52</td>
<td align="center"><strong>-0.05</strong></td>
<td align="center">-0.35</td>
</tr>
<tr class="odd">
<td align="center"><strong>Fertility rate</strong></td>
<td align="center">0.08</td>
<td align="center">1.00</td>
<td align="center">0.34</td>
<td align="center">0.28</td>
<td align="center">0.09</td>
<td align="center">0.39</td>
<td align="center"><strong>0.58</strong></td>
<td align="center">0.00</td>
</tr>
<tr class="even">
<td align="center"><strong>Forest area</strong></td>
<td align="center">0.00</td>
<td align="center">0.01</td>
<td align="center">0.09</td>
<td align="center">0.08</td>
<td align="center">1.00</td>
<td align="center">0.01</td>
<td align="center"><strong>0.00</strong></td>
<td align="center">0.02</td>
</tr>
<tr class="odd">
<td align="center"><strong>Immunization, measles</strong></td>
<td align="center">1.00</td>
<td align="center">0.84</td>
<td align="center">0.69</td>
<td align="center">0.96</td>
<td align="center">0.98</td>
<td align="center">0.38</td>
<td align="center"><strong>0.00</strong></td>
<td align="center">1.00</td>
</tr>
<tr class="even">
<td align="center"><strong>Improved sanitation facilities</strong></td>
<td align="center">0.97</td>
<td align="center">0.00</td>
<td align="center">0.31</td>
<td align="center">0.87</td>
<td align="center">0.71</td>
<td align="center">0.63</td>
<td align="center"><strong>1.00</strong></td>
<td align="center">0.97</td>
</tr>
<tr class="odd">
<td align="center"><strong>Improved water source</strong></td>
<td align="center">0.69</td>
<td align="center">0.00</td>
<td align="center">0.64</td>
<td align="center">0.83</td>
<td align="center">0.91</td>
<td align="center">0.51</td>
<td align="center"><strong>0.25</strong></td>
<td align="center">1.00</td>
</tr>
<tr class="even">
<td align="center"><strong>Life expectancy at birth</strong></td>
<td align="center">1.00</td>
<td align="center">0.06</td>
<td align="center">0.38</td>
<td align="center">0.85</td>
<td align="center">0.53</td>
<td align="center">0.00</td>
<td align="center"><strong>0.50</strong></td>
<td align="center">0.75</td>
</tr>
<tr class="odd">
<td align="center"><strong>Mobile cellular subscriptions</strong></td>
<td align="center">0.00</td>
<td align="center">0.70</td>
<td align="center">0.39</td>
<td align="center">0.47</td>
<td align="center">1.00</td>
<td align="center">0.96</td>
<td align="center"><strong>0.31</strong></td>
<td align="center">0.92</td>
</tr>
<tr class="even">
<td align="center"><strong>Mortality rate, under 5</strong></td>
<td align="center">0.00</td>
<td align="center">-1.00</td>
<td align="center">-0.76</td>
<td align="center">-0.14</td>
<td align="center">-0.07</td>
<td align="center">-0.61</td>
<td align="center"><strong>-0.15</strong></td>
<td align="center">-0.12</td>
</tr>
<tr class="odd">
<td align="center"><strong>Net barter terms of trade</strong></td>
<td align="center">0.36</td>
<td align="center">0.54</td>
<td align="center">0.00</td>
<td align="center">0.08</td>
<td align="center">0.55</td>
<td align="center">0.21</td>
<td align="center"><strong>1.00</strong></td>
<td align="center">0.00</td>
</tr>
<tr class="even">
<td align="center"><strong>Population density</strong></td>
<td align="center">0.24</td>
<td align="center">0.26</td>
<td align="center">1.00</td>
<td align="center">0.13</td>
<td align="center">0.00</td>
<td align="center">0.08</td>
<td align="center"><strong>0.22</strong></td>
<td align="center">0.29</td>
</tr>
<tr class="odd">
<td align="center"><strong>Population growth</strong></td>
<td align="center">0.62</td>
<td align="center">1.00</td>
<td align="center">0.79</td>
<td align="center">0.83</td>
<td align="center">0.61</td>
<td align="center">0.86</td>
<td align="center"><strong>0.00</strong></td>
<td align="center">0.65</td>
</tr>
<tr class="even">
<td align="center"><strong>Population</strong></td>
<td align="center">0.00</td>
<td align="center">0.01</td>
<td align="center">1.00</td>
<td align="center">0.09</td>
<td align="center">0.10</td>
<td align="center">0.03</td>
<td align="center"><strong>0.01</strong></td>
<td align="center">0.04</td>
</tr>
<tr class="odd">
<td align="center"><strong>Surface area</strong></td>
<td align="center">0.00</td>
<td align="center">0.01</td>
<td align="center">0.19</td>
<td align="center">0.11</td>
<td align="center">1.00</td>
<td align="center">0.07</td>
<td align="center"><strong>0.00</strong></td>
<td align="center">0.02</td>
</tr>
<tr class="even">
<td align="center"><strong>Protected areas</strong></td>
<td align="center">0.37</td>
<td align="center">0.60</td>
<td align="center">0.21</td>
<td align="center">0.45</td>
<td align="center">0.68</td>
<td align="center">0.81</td>
<td align="center"><strong>0.00</strong></td>
<td align="center">1.00</td>
</tr>
<tr class="odd">
<td align="center"><strong>Urban population growth</strong></td>
<td align="center">0.47</td>
<td align="center">1.00</td>
<td align="center">0.80</td>
<td align="center">0.69</td>
<td align="center">0.46</td>
<td align="center">0.80</td>
<td align="center"><strong>0.00</strong></td>
<td align="center">0.90</td>
</tr>
<tr class="even">
<td align="center"><strong>Score</strong></td>
<td align="center"><strong>0.2935</strong></td>
<td align="center"><strong>0.2745</strong></td>
<td align="center"><strong>0.3035</strong></td>
<td align="center"><strong>0.3560</strong></td>
<td align="center"><strong>0.3000</strong></td>
<td align="center"><strong>0.2495</strong></td>
<td align="center"><strong>0.2020</strong></td>
<td align="center"><strong>0.3820</strong></td>
</tr>
</tbody>
</table>
