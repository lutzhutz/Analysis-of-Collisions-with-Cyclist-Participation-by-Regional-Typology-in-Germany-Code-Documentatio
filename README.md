This is the R Skript corresponding to the Bachelor Thesis "Analysis of Collisions with Cyclist Participation by Regional Typology in Germany". In the first section the functions for downloading and processing collision data from the collision dataset of the German Federal Statistical Office will be presented and explained step by step. The sections that follow represent the workflow for all statistics and graphs that are included in the thesis. To run the full script at once **you will need the b_osm.xlsx file** provided in the GitHub repository [Analysis of Collisions with Cyclist Participation by Regional Typology in Germany Code Documentation](https://github.com/lutzhutz/Analysis-of-Collisions-with-Cyclist-Participation-by-Regional-Typology-in-Germany-Code-Documentation) starting from the section *"Nearest road maximum speed limit"* as some of the steps have been made in the software QGIS. 

Note: The library [collisionsDE](https://github.com/lutzhutz/collisionsDE) is a self written library by the author. The Bachelor Thesis is available at https://www.researchgate.net/profile/Lasse_Harkort. 

#Libraries

```r
library("devtools")# for downloading GitHub packages
```

```
## Loading required package: usethis
```

```r
#install_github("lutzhutz/collisionsDE")
library(collisionsDE)
#install.packages("colorspace")
library(colorspace)
```

```
## Warning: package 'colorspace' was built under R version 4.0.2
```

```r
#install.packages("dplyr)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
#install.packages("sf")
library(sf)
```

```
## Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1
```

```r
#install.packages("ggplot2")
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 4.0.2
```

```r
#install.packages("lemon")
library(lemon)
```

```
## 
## Attaching package: 'lemon'
```

```
## The following objects are masked from 'package:ggplot2':
## 
##     CoordCartesian, element_render
```

```r
#install.packages("tidyr")
library(tidyr)
#install.packages("stringr")
library(stringr)
#install.packages("ggpubr")
library(ggpubr)
#install.packages("grid")
library(grid)
#install.packages("readxl")
library(readxl)
```

Legend for data name extensions (chronologically):

- y.## = year (e.g. 2018 -> y.18)
- .df = data frame of all collisions
- .b = all collisions with at least one cyclist involved
- .f = frequency
- .k = killed
- .si = seriously injured
- .sli = slightly injured
- .sev = severity
- .ct = collision type
- .ck = collision - killed
- .c = collisions
- .m = month
- .h = hours (daytime)
- .w = weekdays
- .osm = highway data from Open Street Map
- .s = subset
- .m = maxspeed categories
- .mot = motorized
- .g = grouped
- .wi = with information (roads with information on maximum speed limit)
- .ni = no information (roads with no information on maximum speed limit)
- .mfc = merged feature classes (merged datasets of .wi and .ni)


#Pre-processing

```r
#disable e format
options(scipen = 999)

#import all reported collisions with personal injury from 2018 (function from collisionsDE)
y.18<-import_2018()

#add regional information to the collision events (function from collisionsDE)
y.18<-add_regions(y.18)

#transform from spatial feature object to data frame
y.18.df <- y.18 %>% st_drop_geometry()

#subset all accident with at least one cyclist involved
y.18.df.b <- y.18.df[y.18.df$IstRad == 1,]

#data frame overview
head(y.18.df.b)
```

```
##        gem ULAND UREGBEZ UKREIS UGEMEINDE UJAHR UMONAT USTUNDE UWOCHENTAG
## 2  1001000    01       0     01       000  2018     06      14          6
## 4  1001000    01       0     01       000  2018     05      14          3
## 6  1001000    01       0     01       000  2018     05      07          6
## 10 1001000    01       0     01       000  2018     05      20          4
## 12 1001000    01       0     01       000  2018     09      06          2
## 15 1001000    01       0     01       000  2018     06      12          5
##    UKATEGORIE UART UTYP1 ULICHTVERH IstRad IstPKW IstFuss IstKrad IstGkfz
## 2           3    5     2          0      1      0       0       0       0
## 4           3    5     2          0      1      1       0       0       0
## 6           3    5     2          0      1      1       0       0       0
## 10          3    5     3          0      1      1       0       0       0
## 12          3    7     1          2      1      0       0       0       0
## 15          3    0     1          0      1      0       0       0       0
##    IstSonstig STRZUSTAND  LINREFX LINREFY XGCSWGS84 YGCSWGS84     gemrs
## 2           1          0 527894.9 6070227  9.433687  54.77847 1.001e+10
## 4           0          0 527259.3 6069459  9.423734  54.77161 1.001e+10
## 6           0          0 528324.8 6070596  9.440407  54.78177 1.001e+10
## 10          0          0 526405.1 6073327  9.410808  54.80641 1.001e+10
## 12          0          0 529990.8 6070393  9.466288  54.77984 1.001e+10
## 15          0          0 527747.0 6071610  9.431520  54.79091 1.001e+10
##             gemname gembev gemfl     vbg    vbgrs          vbgname land
## 2  Flensburg, Stadt  89504 56.73 1001000 10010000 Flensburg, Stadt    1
## 4  Flensburg, Stadt  89504 56.73 1001000 10010000 Flensburg, Stadt    1
## 6  Flensburg, Stadt  89504 56.73 1001000 10010000 Flensburg, Stadt    1
## 10 Flensburg, Stadt  89504 56.73 1001000 10010000 Flensburg, Stadt    1
## 12 Flensburg, Stadt  89504 56.73 1001000 10010000 Flensburg, Stadt    1
## 15 Flensburg, Stadt  89504 56.73 1001000 10010000 Flensburg, Stadt    1
##    RegioStaR2 RegioStaR4 RegioStaR17 RegioStaR7 RegioStaR5 RegioStaRGem7
## 2           2         22         221         75         54            74
## 4           2         22         221         75         54            74
## 6           2         22         221         75         54            74
## 10          2         22         221         75         54            74
## 12          2         22         221         75         54            74
## 15          2         22         221         75         54            74
##    RegioStaRGem5 Stadtregion nameStadtregion regio7bez
## 2             53          NA            <NA>       R_C
## 4             53          NA            <NA>       R_C
## 6             53          NA            <NA>       R_C
## 10            53          NA            <NA>       R_C
## 12            53          NA            <NA>       R_C
## 15            53          NA            <NA>       R_C
```

```r
#suppress note message from dplyr
options(dplyr.summarise.inform=F) 
```

#Collision frequency

```r
# Here the values of table 2 in the thesis are created

y.18.f <-
  y.18.df %>%
  mutate(countb = if_else(y.18.df$IstRad == 1, 1, 0)) %>% #indicate all collisions where at least one cyclist has been involved
  group_by(gemname, regio7bez, gembev, gemfl) %>%
  summarise(count = sum(n()),
            countb = sum(countb)) %>% #sum up the number of all collisions, the number of collisions with cyclist participation, population and area
  group_by(regio7bez) %>% #sum these up again to the seven regional levels
  summarise(
    gsm_fl = sum(gemfl), #total area
    gsm_bev = sum(gembev), #total population
    count = sum(count), #number of collisions
    countb = sum(countb), #number of collisions with cyclist participation
    popdens = gsm_bev / gsm_fl,
    .groups = "drop" #population density
  ) %>%
  mutate(
    gsm_fl_s = gsm_fl / sum(gsm_fl) * 100,
    gsm_bev_s = gsm_bev / sum(gsm_bev) * 100,
    share_s = count / sum(count) * 100,
    shareb_s = countb / sum(countb) * 100,
    acc_pers = count * 10000 / gsm_bev,
    acc_persb = countb * 10000 / gsm_bev #calculate the shares of each of the variables calculated before as well as the collisions per 10.000 residents of each region
  )

round(y.18.f$acc_persb, 1) #round the numbers to the first decimal
```

```
## [1] 14.2 13.8  8.5  4.9 13.0  8.2  4.8
```

```r
cor.test(y.18.f$popdens, y.18.f$acc_pers, method = "pearson") #calculate Pearson correlation
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  y.18.f$popdens and y.18.f$acc_pers
## t = 1.7972, df = 5, p-value = 0.1322
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.2396493  0.9373264
## sample estimates:
##       cor 
## 0.6264673
```

```r
cor.test(y.18.f$popdens, y.18.f$acc_persb, method = "pearson") #calculate Pearson correlation
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  y.18.f$popdens and y.18.f$acc_persb
## t = 2.6142, df = 5, p-value = 0.04743
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.01606603 0.96229446
## sample estimates:
##     cor 
## 0.75993
```

#Collision severity

```r
y.18.df.k <-
  y.18.df.b[y.18.df.b$UKATEGORIE == 1, ] #subset all collisions with at least one person killed

#transform data
y.18.df.k <- y.18.df.k %>%
  group_by(regio7bez) %>%
  summarise(count = n()) %>%
  mutate(share = count / sum(count) * 100,
         cat = "killed") #calculate the shares of all fatal crashes by each region

y.18.df.si <-
  y.18.df.b[y.18.df.b$UKATEGORIE == 2, ] #subset all collisions with at least one person seriously injured

#transform data
y.18.df.si <- y.18.df.si %>%
  group_by(regio7bez) %>%
  summarise(count = n()) %>%
  mutate(share = count / sum(count) * 100,
         cat = "seriously_injured") #calculate the shares of all crashes with at least one person seriously injured by each region

y.18.df.sli <-
  y.18.df.b[y.18.df.b$UKATEGORIE == 3, ] #subset all collisions with at least one person slightly injured

#transform data
y.18.df.sli <- y.18.df.sli %>%
  group_by(regio7bez) %>%
  summarise(count = n()) %>%
  mutate(share = count / sum(count) * 100,
         cat = "injury") #calculate the shares of all crashes with at least one person slightly injured by each region


y.18.df.sev <-
  rbind(y.18.df.k, y.18.df.si, y.18.df.sli) #combine the three created datasets

#check total counts
sum(y.18.df.k$count)
```

```
## [1] 306
```

```r
sum(y.18.df.si$count)
```

```
## [1] 10039
```

```r
sum(y.18.df.sli$count)
```

```
## [1] 47077
```

```r
#visualization (Figure 9 in Thesis)
ggplot(
  transform(
    y.18.df.sev,
    cat = factor(
      cat,
      levels =
        c("injury",
          "seriously_injured",
          "killed"),
      label = c(
        "Slightly Injured (100% = 47.077)",
        "Seriously injured (100% = 10.039)",
        "Killed (100% = 306)"
      )
    ),
    regio7bez = factor(
      regio7bez,
      levels =
        c(
          "U_Metro",
          "U_Regiop",
          "U_Medium",
          "U_Small",
          "R_C",
          "R_Med_C",
          "R_Small_C"
        ),
      label = c(
        "Metropolis",
        "Regiopolis",
        "Medium-sized City (u)",
        "Small-town Area (u)",
        "Central City",
        "Medium sized City (r)",
        "Small-town Area (r)"
      )
    )
  ),
  aes(regio7bez, share, fill = cat)
) +
  geom_bar(stat = "identity",
           position = "dodge",
           width = 0.8) +
  geom_text(
    aes(label = round(share, 1)),
    position = position_dodge(width = 0.8),
    vjust = -0.2,
    color = "gray30",
    size = 3.5
  ) +
  ylab("Share of reported collisions with cyclist participation in each category in %") +
  theme_light() +
  scale_fill_manual("",
                    values = c("grey80",
                               "grey60",
                               "grey40")) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(colour = "black", size = 10.5),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 10.5)
  ) +
  annotate("text",
           label = "* u=urban, r=rural",
           x = 7,
           y = 30)
```

![](Thesis_Code_Documentation_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
#save in output workspace
#ggsave("severity.png",width = 11.5,height = 6.31,dpi = 300)
```

#Collision type

```r
#calculate share of each collision type by region
y.18.df.b.ct <- y.18.df.b %>%
  group_by(regio7bez, UTYP1) %>%
  summarise(count = n()) %>%
  mutate(share = count / sum(count) * 100) 

#visualize (Figure 10 in thesis)
type<-ggplot(
    transform(
    y.18.df.b.ct,
    UTYP1 = factor(
      UTYP1,
      levels = c(
        "3",
        "2",
        "1",
        "7",
        "6",
        "5",
        "4"
      ),
      label = c(
        "collision when turing into/crossing",
        "collision when turning off",
        "driving collision",
        "other collision",
        "collision while moving along",
        "collision with stationary vehicle",
        "collision while pedestrian crossing"
      )
    ),
    regio7bez = factor(
      regio7bez,
      levels =
        c(
          "R_Small_C",
          "R_Med_C",
          "R_C",
          "U_Small",
          "U_Medium",
          "U_Regiop",
          "U_Metro"
        ),
      label = c(
        "Small-town Area (r)",
        "Medium sized City (r)",
        "Central City",
        "Small-town Area (u)",
        "Medium-sized City (u)",
        "Regiopolis",
        "Metropolis"
      )
    )
  ),
  aes(x = share, y = regio7bez)
) +
  facet_wrap( ~ UTYP1, ncol = 4)  +
  geom_bar(stat = "identity", fill = rep(c("#B42A5D",
    "#FE2F7C",
    "#FF7776",
    "#FFE0DC",
    "#00858B",
    "#41B8B9",
    "#D2E38C"
  ),7)) +
  geom_text(aes(label = round(share,1)), hjust = -0.08, size = 3) +
  theme_light() +
  scale_x_continuous(limits = c(0, 42)) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(color = 'black',size = 10),
    strip.text = element_text(color = 'black',size = 10, face = "italic"),
    strip.background = element_blank()
  )

#check total counts
y.18.df.b.ct %>%
  group_by(regio7bez) %>%
  summarise(count = sum(count))
```

```
## # A tibble: 7 x 2
##   regio7bez count
##   <fct>     <int>
## 1 U_Metro   16232
## 2 U_Regiop   8870
## 3 U_Medium  12781
## 4 U_Small    2137
## 5 R_C        4754
## 6 R_Med_C    7531
## 7 R_Small_C  5117
```

```r
#add counts to ggplot graph
print(type)
grid.text(
  paste0(
    "Metropolis - 100% = 16.232",
    "\n",
    "Regiopolis - 100% = 8.870",
    "\n",
    "Medium-sized City (u) - 100% = 12.781",
    "\n",
    "Small-town Area (u) - 100% = 2.137",
    "\n",
    "Central City - 100% = 4754",
    "\n",
    "Medium sized City (r) - 100% = 7.531",
    "\n",
    "Small-town Area (r) - 100% = 5.117",
    "\n",
    "\n",
    "*u=urban, r=rural"
  ),
  x = 0.79,
  y = 0.32,
  just = "left",
  gp = gpar(fontsize = 10)
)
```

![](Thesis_Code_Documentation_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
#export:Save as image .. 1122x590
```

#Collisions between cyclists and other parties with fatal outcome

```r
#subset all collisions with at least one person killed
y.18.df.b.ck <- y.18.df.b[y.18.df.b$UKATEGORIE == 1,]

#add collision type with collision_types() (function from collisionsDE)
y.18.df.b.ck <- collision_types(y.18.df.b.ck)

#group by region and collision type, compute shares
y.18.df.b.ck <- y.18.df.b.ck %>%
  group_by(regio7bez, coll_typ) %>%
  summarise(count = n()) %>%
  mutate(share = count / sum(count) * 100)

#visualize (Figure 11 in the thesis)
b <- ggplot(
  transform(
    y.18.df.b.ck,
    coll_typ = factor(
      coll_typ,
      levels = c(
        "bicyclecar",
        "bicycle",
        "bicyclefoot",
        "bicycleother",
        "bicyclemcycle",
        "bicycletruck",
        "three"
      ),
      labels = c(
        "bicycle-car",
        "bicycle only accidents",
        "bicycle-foot",
        "bicycle-other parties",
        "bicycle-motorcycle",
        "bicycle-truck",
        "more than two participants"
      )
    ),
    regio7bez = factor(
      regio7bez,
      levels =
        c(
          "R_Small_C",
          "R_Med_C",
          "R_C",
          "U_Small",
          "U_Medium",
          "U_Regiop",
          "U_Metro"
        ),
      label = c(
        "Small-town Area (r)",
        "Medium sized City (r)",
        "Central City",
        "Small-town Area (u)",
        "Medium-sized City (u)",
        "Regiopolis",
        "Metropolis"
      )
    )
  ),
  aes(x = share, y = regio7bez)
) +
  facet_wrap( ~ coll_typ, ncol = 4)  +
  geom_bar(
    stat = "identity",
    fill = c(
      "#B42A5D",
      "#FE2F7C",
      "#FF7776",
      "#FFE0DC",
      "#00858B",
      "#41B8B9",
      "#D2E38C",
      "#B42A5D",
      "#FE2F7C",
      "#FF7776",
      "#FFE0DC",
      "#00858B",
      "#41B8B9",
      "#D2E38C",
      "#B42A5D",
      "#FE2F7C",
      "#41B8B9",
      "#D2E38C",
      "#B42A5D",
      "#FE2F7C",
      "#FF7776",
      "#FFE0DC",
      "#00858B",
      "#41B8B9",
      "#D2E38C",
      "#B42A5D",
      "#FE2F7C",
      "#FF7776",
      "#00858B",
      "#D2E38C",
      "#B42A5D",
      "#FE2F7C",
      "#FF7776",
      "#FFE0DC",
      "#00858B",
      "#41B8B9",
      "#FFE0DC",
      "#D2E38C"
    )
  ) +
  geom_text(aes(label = round(share, 1)), hjust = -0.08, size = 3) +
  theme_light() +
  scale_x_continuous(limits = c(0, 76)) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(color = 'black', size = 10),
    strip.text = element_text(color = 'black', size = 10, face = "italic"),
    strip.background = element_blank()
  )

#check total counts
y.18.df.b.ck %>%
  group_by(regio7bez) %>%
  summarise(count = sum(count))
```

```
## # A tibble: 7 x 2
##   regio7bez count
##   <fct>     <int>
## 1 U_Metro      37
## 2 U_Regiop     19
## 3 U_Medium     75
## 4 U_Small      23
## 5 R_C          21
## 6 R_Med_C      55
## 7 R_Small_C    76
```

```r
#add counts to ggplot graph
print(b)
grid.text(
  paste0(
    "Metropolis - 100% = 37",
    "\n",
    "Regiopolis - 100% = 19",
    "\n",
    "Medium-sized City (u) - 100% = 75",
    "\n",
    "Small-town Area (u) - 100% = 23",
    "\n",
    "Central City - 100% = 21",
    "\n",
    "Medium sized City (r) - 100% = 55",
    "\n",
    "Small-town Area (r) - 100% = 76",
    "\n",
    "\n",
    "*u=urban, r=rural"
  ),
  x = 0.79,
  y = 0.32,
  just = "left",
  gp = gpar(fontsize = 10)
)
```

![](Thesis_Code_Documentation_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
#export:Save as image .. 1122x590
```

#Collisions between cyclists and other parties, all collisions

```r
#add collision type with collision_types() function by collisionsDE
y.18.df.b.c <- collision_types(y.18.df.b)

#group by region and collision type, compute shares
y.18.df.b.c <- y.18.df.b.c %>%
  group_by(regio7bez, coll_typ) %>%
  summarise(count = n()) %>%
  mutate(share = count / sum(count) * 100)

#visualize (Figure 12 in the thesis)
a <- ggplot(
  transform(
    y.18.df.b.c,
    coll_typ = factor(
      coll_typ,
      levels = c(
        "bicyclecar",
        "bicycle",
        "bicyclefoot",
        "bicycleother",
        "bicyclemcycle",
        "bicycletruck",
        "three"
      ),
      labels = c(
        "bicycle-car",
        "bicycle only accidents",
        "bicycle-foot",
        "bicycle-other parties",
        "bicycle-motorcycle",
        "bicycle-truck",
        "more than two participants"
      )
    ),
    regio7bez = factor(
      regio7bez,
      levels =
        c(
          "R_Small_C",
          "R_Med_C",
          "R_C",
          "U_Small",
          "U_Medium",
          "U_Regiop",
          "U_Metro"
        ),
      label = c(
        "Small-town Area (r)",
        "Medium sized City (r)",
        "Central City",
        "Small-town Area (u)",
        "Medium-sized City (u)",
        "Regiopolis",
        "Metropolis"
      )
    )
  ),
  aes(x = share, y = regio7bez)
) +
  facet_wrap(~ coll_typ, ncol = 4)  +
  geom_bar(stat = "identity", fill = rep(
    c(
      "#B42A5D",
      "#FE2F7C",
      "#FF7776",
      "#FFE0DC",
      "#00858B",
      "#41B8B9",
      "#D2E38C"
    ),
    7
  )) +
  geom_text(aes(label = round(share, 1)), hjust = -0.08, size = 3) +
  theme_light() +
  scale_x_continuous(limits = c(0, 70)) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(color = 'black', size = 10),
    strip.text = element_text(color = 'black', size = 10, face = "italic"),
    strip.background = element_blank()
  )

#check total counts
y.18.df.b.c %>%
  group_by(regio7bez) %>%
  summarise(count = sum(count))
```

```
## # A tibble: 7 x 2
##   regio7bez count
##   <fct>     <int>
## 1 U_Metro   16232
## 2 U_Regiop   8870
## 3 U_Medium  12781
## 4 U_Small    2137
## 5 R_C        4754
## 6 R_Med_C    7531
## 7 R_Small_C  5117
```

```r
#add counts to ggplot graph
print(a)
grid.text(
  paste0(
    "Metropolis - 100% = 16.232",
    "\n",
    "Regiopolis - 100% = 8.870",
    "\n",
    "Medium-sized City (u) - 100% = 12.781",
    "\n",
    "Small-town Area (u) - 100% = 2.137",
    "\n",
    "Central City - 100% = 4754",
    "\n",
    "Medium sized City (r) - 100% = 7.531",
    "\n",
    "Small-town Area (r) - 100% = 5.117",
    "\n",
    "\n",
    "*u=urban, r=rural"
  ),
  x = 0.79,
  y = 0.32,
  just = "left",
  gp = gpar(fontsize = 10)
)
```

![](Thesis_Code_Documentation_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
#export:Save as image .. 1122x590
```

#Temporal distribution

```r
#calculate shares of collisions per month by region, compute shares
y.18.df.b.m <- y.18.df.b %>%
  group_by(regio7bez, UMONAT) %>%
  summarise(count = n()) %>%
  mutate(share = count / sum(count) * 100)

#visualization for month (warning message can be ignored)
m <-
  ggplot(y.18.df.b.m,
         aes(UMONAT, share, group = regio7bez, color = regio7bez)) +
  geom_line(size = 0.7) +
  theme_bw() +
  ggtitle("Month") +
  scale_x_discrete(
    labels = c(
      "Jan",
      "Feb",
      "Mar",
      "Apr",
      "May",
      "Jun",
      "Jul",
      "Aug",
      "Sep",
      "Oct",
      "Nov",
      "Dez"
    )
  ) +
  scale_colour_manual(
    "legend",
    values = c(
      "#B42A5D",
      "#FE2F7C",
      "#FF7776",
      "#FFE0DC",
      "#00858B",
      "#41B8B9",
      "#D2E38C"
    ),
    labels = c(
      "Metropolis",
      "Regiopolis",
      "Medium-sized City (u)",
      "Small-town Area (u)",
      "Central City",
      "Medium sized City (r)",
      "Small-town Area (r)"
    )
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    legend.position = "bottom",
    legend.title = element_blank()
  )

#calculate shares of collisions per weekday by region, compute shares
y.18.df.b.w <- y.18.df.b %>%
  group_by(regio7bez, UWOCHENTAG) %>%
  summarise(count = n()) %>%
  mutate(share = count / sum(count) * 100)

#visualization for weekdays
w <- ggplot(y.18.df.b.w,
            aes(UWOCHENTAG, share, group = regio7bez, color = regio7bez)) +
  geom_line(size = 0.7) +
  theme_bw() +
  ggtitle("Weekdays") +
  scale_x_discrete(
    limits = c(2, 3, 4, 5, 6, 7, 1),
    labels = c("Tue",
               "Wed",
               "Thu",
               "Fri",
               "Sat",
               "Sun",
               "Mon")
  ) +
  scale_colour_manual(
    "legend",
    values = c(
      "#B42A5D",
      "#FE2F7C",
      "#FF7776",
      "#FFE0DC",
      "#00858B",
      "#41B8B9",
      "#D2E38C"
    ),
    labels = c(
      "Metropolis",
      "Regiopolis",
      "Medium-sized City",
      "Small-town Area",
      "Central City",
      "Medium sized City",
      "Small-town Area"
    )
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    legend.position = "bottom",
    legend.title = element_blank()
  )
```

```
## Warning: Continuous limits supplied to discrete scale.
## Did you mean `limits = factor(...)` or `scale_*_continuous()`?
```

```r
#extract only working days for the hourly shares of collision frequency
y.18.df.b.working <-
  y.18.df.b[!(y.18.df.b$UWOCHENTAG == 7 | y.18.df.b$UWOCHENTAG == 1),]

#calculate shares of collisions per daytime by region, compute shares
y.18.df.b.h <- y.18.df.b.working %>%
  mutate(
    #revalue to daytime categories
    hourcat = case_when(
      USTUNDE %in% c('02', '03', '04', '05') ~ 'night_m',
      USTUNDE %in% c('06', '07', '08', '09') ~ 'morning',
      USTUNDE %in% c('10', '11', '12', '13') ~ 'noon',
      USTUNDE %in% c('14', '15', '16', '17') ~ 'afternoon',
      USTUNDE %in% c('18', '19', '20', '21') ~ 'evening',
      USTUNDE %in% c('22', '23', '00', '01') ~ 'night'
    )
  ) %>%
  group_by(regio7bez, hourcat) %>%
  summarise(count = n()) %>%
  mutate(share = count / sum(count) * 100)

#visualization for daytimes
h <- ggplot(y.18.df.b.h,
            aes(hourcat, share, group = regio7bez, color = regio7bez)) +
  geom_line(size = 0.7) +
  theme_bw() +
  ggtitle("Daytime*") +
  scale_x_discrete(
    limits = c('night_m',
               'morning',
               'noon',
               'afternoon',
               'evening',
               'night'),
    labels = c(
      "early monring (2-6am)",
      "morning (6-10am)",
      "noon (10am - 1pm)",
      "afternoon (2-6pm)",
      "evening (6-10pm)",
      "night (10pm-2am)"
    )
  ) +
  scale_colour_manual(
    "legend",
    values = c(
      "#B42A5D",
      "#FE2F7C",
      "#FF7776",
      "#FFE0DC",
      "#00858B",
      "#41B8B9",
      "#D2E38C"
    ),
    labels = c(
      "Metropolis",
      "Regiopolis",
      "Medium-sized City",
      "Small-town Area",
      "Central City",
      "Medium sized City",
      "Small-town Area"
    )
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    legend.position = "bottom",
    legend.title = element_blank()
  )

#arrange all plots to one graph (Figure 13 in the thesis)
figure <-
  ggarrange(
    m,
    w,
    h,
    ncol = 1,
    nrow = 3,
    common.legend = T,
    legend = "bottom"
  )
annotate_figure(
  figure,
  left = text_grob(
    "Share of all collisions with cyclists participation per region in %",
    rot = 90,
    size = 9
  ),
  bottom = text_grob(
    paste0("u=urban, r=rural      *weekends excluded"),
    hjust = 1.2,
    x = 1,
    face = "italic",
    size = 9
  )
)
```

![](Thesis_Code_Documentation_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
#save graph as image
ggsave(
  "month_hour_week2.png",
  width = 8.04,
  height = 6.99,
  dpi = 300
) 
```

WARNING: this step takes approximately 30min. It is recommended to use the processed file provided in the folder

However, if you would like to reproduce this step you need to specify the outputs of the shapefiles that will be downloaded (note: combined size of all shapefiles is about 8GB)

#Download osm highway dataset

```r
# #set output directory - needs to be specified!
# outdir=""
# 
# #function for downloading osm highway data from Germany
# dwl_state_highway <- function(state,output) {
#   temp <- tempfile()
#   download.file(
#     paste0(
#       "http://download.geofabrik.de/europe/germany/",
#       state,
#       "-latest-free.shp.zip"
#     ),
#     temp
#   )
#   unzip(temp)
#   state_data <-
#     st_read("./gis_osm_roads_free_1.shp", "gis_osm_roads_free_1")
#   state_data$length<-st_length(state_data)
#   output=output
#   write_sf(state_data,output,state,driver = "ESRI Shapefile")
# }
# 
# #download osm highway data by federal state - the data for all of Germany would have been to huge in size
# dwl_state_highway(state = "bremen", output = outdir)
# dwl_state_highway(state = "hamburg", output = outdir)
# dwl_state_highway(state = "schleswig-holstein", output = outdir)
# dwl_state_highway(state = "niedersachsen", output = outdir)
# dwl_state_highway(state = "bayern", output = outdir)
# dwl_state_highway(state = "baden-wuerttemberg", output = outdir)
# dwl_state_highway(state = "saarland", output = outdir)
# dwl_state_highway(state = "rheinland-pfalz", output = outdir)
# dwl_state_highway(state = "hessen", output = outdir)
# dwl_state_highway(state = "sachsen", output = outdir)
# dwl_state_highway(state = "sachsen-anhalt", output = outdir)
# dwl_state_highway(state = "brandenburg", output = outdir)
# dwl_state_highway(state = "berlin", output = outdir)
```
To add features from the nearest road of the crash location to each collision event, the algorithm âjoin attribute by nearestâ provided by the geographic information system software QGIS (Version 3.12.2) has been used.

The steps undertaken in QGIS were as follows:

1. merge all federal states to one layer ("merge vector layers")
2. delete duplicates by scanning osm_id ("delete duplicates by attribute")
3. project bicycle chrash data and osm highway germany to ETRS98 (Germany)
4. create Id field by using rownumber (use "@row_number" as expression)
5. use algorithm "join by nearest location" for bicycle crash data to osm highway Germany ("join by nearest location")
6. export as .csv, the .csv was then imported to excel and saved as .xslx in order to minimize the data size

#Nearest road maximum speed limit **works only with b_osm.xslx file!**

```r
#here, the b_osm.xslx file provided in the GitHub repository needs to be put into your workspace (if you prefer to have it elsewhere the read_excel directory needs to be updated as well)
y.18.df.b.osm <-
  read_excel("./b_osm.xlsx")

#all 252 points that have been found to be the same distance to two line segments are dropped
y.18.df.b.osm.s <-
  y.18.df.b.osm[!(duplicated(y.18.df.b.osm$ID) |
            duplicated(y.18.df.b.osm$ID, fromLast = TRUE)), , drop = FALSE]

#all points with a distance bigger than 10m to the next road feature are dropped too
y.18.df.b.osm.s <- y.18.df.b.osm.s[y.18.df.b.osm.s$distance < 10, ]

#mean and median distance from a collision event to the nearest road
round(mean(y.18.df.b.osm.s$distance),2)
```

```
## [1] 1.16
```

```r
round(median(y.18.df.b.osm.s$distance),2)
```

```
## [1] 0.83
```

```r
#classify maximum speed limit into new categories
y.18.df.b.osm.m <- y.18.df.b.osm.s %>%
  mutate(
    maxspeed_c = case_when(
      maxspeed == 0 ~ "n_a",
      maxspeed > 0 & maxspeed <= 30 ~ "_30",
      maxspeed > 30 &
        maxspeed <= 60  ~ "40_60",
      maxspeed > 60 &
        maxspeed <= 90 ~ "70_90",
      maxspeed > 90 ~ "100_"
    )
  )

#subset all collision between cyclist and motorized vehicle
y.18.df.b.osm.mot <- y.18.df.b.osm.m %>%
  #compute TRUE or FALSE for motorized or not motorized vehicle
  mutate(bicyclem = y.18.df.b.osm.m$IstPKW == 1 |
           y.18.df.b.osm.m$IstKrad == 1 |
           y.18.df.b.osm.m$IstGkfz == 1 |
           y.18.df.b.osm.m$IstSnst == 1)

#subset all TRUE
y.18.df.b.osm.mot <- y.18.df.b.osm.mot[y.18.df.b.osm.mot$bicyclem == TRUE, ]

#group all collisions by their nearest road maximum speed limit and the region and calculate the shares
y.18.df.b.osm.mot.g <- y.18.df.b.osm.mot %>%
  group_by(regi7bz, maxspeed_c) %>%
  summarise(count = n()) %>%
  mutate(share = count / sum(count) * 100)

#visualize (Figure 15 in the thesis)
s <- ggplot(
  transform(
    y.18.df.b.osm.mot.g,
    maxspeed_c = factor(
      maxspeed_c,
      levels = c("_30",
                 "40_60",
                 "70_90",
                 "100_",
                 "n_a"),
      labels = c(
        "30 km/h or less",
        "40, 50 or 60 km/h",
        "70, 80 or 90 km/h",
        "100 km/h or more",
        "no information on speed limit"
      )
    ),
    regi7bz = factor(
      regi7bz,
      levels =
        c(
          "R_Small_C",
          "R_Med_C",
          "R_C",
          "U_Small",
          "U_Medium",
          "U_Regiop",
          "U_Metro"
        ),
      label = c(
        "Small-town Area (r)",
        "Medium sized City (r)",
        "Central City",
        "Small-town Area (u)",
        "Medium-sized City (u)",
        "Regiopolis",
        "Metropolis"
      )
    )
  ),
  aes(x = share, y = regi7bz)
) +
  facet_wrap(~ maxspeed_c, ncol = 3)  +
  geom_bar(stat = "identity", fill = rep(
    c(
      "#00858B",
      "#41B8B9",
      "#D2E38C",
      "#FF7776",
      "#B42A5D",
      "#FE2F7C",
      "#FFE0DC"
    ),
    5
  )) +
  geom_text(aes(label = round(share, 1)), hjust = -0.08, size = 3) +
  theme_light() +
  scale_x_continuous(limits = c(0, 58)) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(color = 'black', size = 10),
    strip.text = element_text(color = 'black', size = 10, face = "italic"),
    strip.background = element_blank()
  )

#check total counts
y.18.df.b.osm.mot.g %>%
  group_by(regi7bz) %>%
  summarise(count = sum(count))
```

```
## # A tibble: 7 x 2
##   regi7bz   count
##   <chr>     <int>
## 1 R_C        3162
## 2 R_Med_C    4883
## 3 R_Small_C  2910
## 4 U_Medium   8743
## 5 U_Metro   12230
## 6 U_Regiop   6272
## 7 U_Small    1347
```

```r
#add counts to ggplot graph (Figure 15 in the thesis)
print(s)
grid.text(
  paste0(
    "Metropolis - 100% = 12.230",
    "\n",
    "Regiopolis - 100% = 6.272",
    "\n",
    "Medium-sized City (u) - 100% = 8.743",
    "\n",
    "Small-town Area (u) - 100% = 1.347",
    "\n",
    "Central City - 100% = 3.162",
    "\n",
    "Medium sized City (r) - 100% = 4.883",
    "\n",
    "Small-town Area (r) - 100% = 2.910",
    "\n",
    "\n",
    "*u=urban, r=rural"
  ),
  x = 0.72,
  y = 0.32,
  just = "left",
  gp = gpar(fontsize = 10)
)
```

![](Thesis_Code_Documentation_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
#export:Save as image .. 1122x590
```

#Nearest road maximum speed limit extrapolation of no information roads

```r
#calculations of values from Table 4 in the thesis

#extract all collisions where the nearest road did have a maximum speed limit
y.18.df.b.osm.mot.wi <- y.18.df.b.osm.mot[!y.18.df.b.osm.mot$maxspeed_c == "n_a", ]

#group all road features and calculate the shares
y.18.df.b.osm.mot.wi <- y.18.df.b.osm.mot.wi %>%
  group_by(fclass) %>%
  summarise(count = n()) %>%
  mutate(share = count / sum(count) * 100)

#extract all collisions where the nearest road did not have a maximum speed limit
y.18.df.b.osm.mot.ni <- y.18.df.b.osm.mot[y.18.df.b.osm.mot$maxspeed_c == "n_a", ]

#group all road features and calculate the shares
y.18.df.b.osm.mot.ni <- y.18.df.b.osm.mot.ni %>%
  group_by(fclass) %>%
  summarise(count_na = n()) %>%
  mutate(share_na = count_na / sum(count_na) * 100)

#merge both datasets by feature class
mfc <- merge(y.18.df.b.osm.mot.wi, y.18.df.b.osm.mot.ni, by = "fclass")

#calculate the differences in the shares
mfc$diff <- mfc$share_na - mfc$share

#round up to two decimals
mfc$share_rounded <- round(mfc$share, 2)
mfc$share_na_rounded <- round(mfc$share_na, 2)
mfc$diff_rounded <- mfc$share_na_rounded - mfc$share_rounded

#extract all feature classes that differ either more than 1 or less than -1
mfc.s <- mfc[(mfc$diff >= 1 | mfc$diff <= -1), ]
```

#Trip purpose

```r
#create dataset
regio7bez <-
  c("Metropolis",
    "Regiopolis",
    "Medium-sized City (u)",
    "Small-town Area (u)",
    "Central City",
    "Medium-sized City (r)",
    "Small-town Area (r)"
  )

Work <-
  c(26, 25, 22, 18, 23, 23, 17) #data from MobilitÃ¤t in Tabellen https://mobilitaet-in-tabellen.dlr.de/mit/ , Zeile: Hauptzweck des Weges, Spalte: Zusammengefasster regionalsttistischer Raumtyp, Untergliederung: Verkehrsmittel (Mehrfachantworten-Set) [Accessed: 05.07.2020])
Leisure <-
  c(30, 32, 32, 37, 30, 32, 41) #data from MobilitÃ¤t in Tabellen https://mobilitaet-in-tabellen.dlr.de/mit/, Zeile: Hauptzweck des Weges, Spalte: Zusammengefasster regionalsttistischer Raumtyp, Untergliederung: Verkehrsmittel (Mehrfachantworten-Set) [Accessed: 05.07.2020])

#create dataframe
df.commuting <- data.frame(regio7bez, Work, Leisure)
df.commuting <- df.commuting %>%
  pivot_longer(c(Work, Leisure))

#visualize (Figure 14 in the thesis)
purpose<-ggplot(transform(df.commuting,
                 regio7bez = factor(
                   regio7bez,
                   levels =
                     c(
                       "Small-town Area (r)",
                       "Medium-sized City (r)",
                       "Central City",
                       "Small-town Area (u)",
                       "Medium-sized City (u)",
                       "Regiopolis",
                       "Metropolis"
                     )
                 )),
       aes(regio7bez, value)) +
  theme_light() +
  geom_bar(stat = "identity", fill = rep(
    c(
      "#B42A5D",
      "#FE2F7C",
      "#FF7776",
      "#FFE0DC",
      "#00858B",
      "#41B8B9",
      "#D2E38C"
    ),
    2
  )) +
  coord_flip() +
  facet_wrap( ~ name, ncol = 2)  +
  geom_text(aes(label = round(value, 1)), hjust = -0.08, size = 2.7) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(color = 'black', size = 10),
    strip.text = element_text(color = 'black', size = 10, face = "italic"),
    strip.background = element_blank()
  )

#add footnote to ggplot graph
print(purpose)
grid.text(
  paste0("*u=urban, r=rural"),
  x = 0.87,
  y = 0.12,
  just = "left",
  gp = gpar(fontsize = 10)
)
```

![](Thesis_Code_Documentation_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
#export:Save as image .. 860x404
```
