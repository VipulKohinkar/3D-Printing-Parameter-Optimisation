
R version 4.5.2 (2025-10-31 ucrt) -- "[Not] Part in a Rumble"
Copyright (C) 2025 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[Workspace loaded from ~/.RData]

> # ---- Setup ----
> library(tidyverse)
── Attaching core tidyverse packages ─────────────────────────────────────── tidyverse 2.0.0 ──
✔ dplyr     1.2.0     ✔ readr     2.2.0
✔ forcats   1.0.1     ✔ stringr   1.6.0
✔ ggplot2   4.0.2     ✔ tibble    3.3.1
✔ lubridate 1.9.5     ✔ tidyr     1.3.2
✔ purrr     1.2.1     
── Conflicts ───────────────────────────────────────────────────────── tidyverse_conflicts() ──
✖ dplyr::filter() masks stats::filter()
✖ dplyr::lag()    masks stats::lag()
ℹ Use the conflicted package to force all conflicts to become errors
Warning messages:
  1: package ‘tidyverse’ was built under R version 4.5.3 
2: package ‘ggplot2’ was built under R version 4.5.3 
3: package ‘tibble’ was built under R version 4.5.3 
4: package ‘tidyr’ was built under R version 4.5.3 
5: package ‘readr’ was built under R version 4.5.3 
6: package ‘purrr’ was built under R version 4.5.3 
7: package ‘dplyr’ was built under R version 4.5.3 
8: package ‘stringr’ was built under R version 4.5.3 
9: package ‘forcats’ was built under R version 4.5.3 
10: package ‘lubridate’ was built under R version 4.5.3 
> library(zoo)

Attaching package: ‘zoo’

The following objects are masked from ‘package:base’:
  
  as.Date, as.Date.numeric

Warning message:
  package ‘zoo’ was built under R version 4.5.3 
> 
  > setwd("C:/Users/vipul/Desktop/MSc Project/New Folder/Testing Data/Vipul Dude/Modulus/SET-2")
> 
  > files <- list.files(pattern = "\\.txt$", recursive = TRUE, full.names = TRUE)
> files
[1] "./CF PETG/S2 CF PETG100%_1.txt" "./CF PETG/S2 CF PETG100%_2.txt"
[3] "./CF PETG/S2 CF PETG100%_3.txt" "./CF PETG/S2 CF PETG100%_4.txt"
[5] "./CF PETG/S2 CF PETG100%_5.txt" "./CF PETG/S2 CF PETG110%_1.txt"
[7] "./CF PETG/S2 CF PETG110%_2.txt" "./CF PETG/S2 CF PETG110%_3.txt"
[9] "./CF PETG/S2 CF PETG110%_4.txt" "./CF PETG/S2 CF PETG110%_5.txt"
[11] "./CF PETG/S2 CF PETG80%_1.txt"  "./CF PETG/S2 CF PETG80%_2.txt" 
[13] "./CF PETG/S2 CF PETG80%_3.txt"  "./CF PETG/S2 CF PETG80%_4.txt" 
[15] "./CF PETG/S2 CF PETG80%_5.txt"  "./CF PETG/S2 CF PETG90%_1.txt" 
[17] "./CF PETG/S2 CF PETG90%_2.txt"  "./CF PETG/S2 CF PETG90%_3.txt" 
[19] "./CF PETG/S2 CF PETG90%_4.txt"  "./CF PETG/S2 CF PETG90%_5.txt" 
[21] "./PETG/S2 PETG100%_1.txt"       "./PETG/S2 PETG100%_2.txt"      
[23] "./PETG/S2 PETG100%_3.txt"       "./PETG/S2 PETG100%_4.txt"      
[25] "./PETG/S2 PETG100%_5.txt"       "./PETG/S2 PETG110%_1.txt"      
[27] "./PETG/S2 PETG110%_2.txt"       "./PETG/S2 PETG110%_3.txt"      
[29] "./PETG/S2 PETG110%_4.txt"       "./PETG/S2 PETG110%_5.txt"      
[31] "./PETG/S2 PETG80%_1.txt"        "./PETG/S2 PETG80%_2.txt"       
[33] "./PETG/S2 PETG80%_3.txt"        "./PETG/S2 PETG80%_4.txt"       
[35] "./PETG/S2 PETG80%_5.txt"        "./PETG/S2 PETG90%_1.txt"       
[37] "./PETG/S2 PETG90%_2.txt"        "./PETG/S2 PETG90%_3.txt"       
[39] "./PETG/S2 PETG90%_4.txt"        "./PETG/S2 PETG90%_5.txt"       
> # ---- Cleaning function ----
> read_clean <- function(f) {
  +     parts <- str_split(f, "/")[[1]]
  +     material <- parts[length(parts) - 1]
  +     
    +     flow   <- str_extract(basename(f), "\\d+(?=%)")
    +     sample <- str_extract(basename(f), "(?<=_)\\d+(?=\\.txt$)")
    +     
      +     df <- read_tsv(f, skip = 1, locale = locale(encoding = "UTF-16LE"),
                           +                    col_names = c("Time","Load","Ext","Stress","ExtPreload",
                                                              +                                  "Strain","PctStrain","GaugeLen"),
                           +                    show_col_types = FALSE) %>%
        +         drop_na(Stress, Strain) %>%
        +         filter(Strain >= cummax(Strain))
      +     
        +     smax <- max(df$Stress)
        +     df <- df %>% filter(Stress > 0.02 * smax)
        +     
          +     win <- df %>% filter(between(Strain, quantile(Strain, .05), quantile(Strain, .4)))
          +     fit <- lm(Stress ~ Strain, data = win)
          +     strain0 <- -coef(fit)[1] / coef(fit)[2]
          +     df <- df %>% mutate(Strain = Strain - strain0) %>% filter(Strain >= 0)
          +     
            +     df %>%
            +         mutate(StressSmooth = rollmean(Stress, 5, fill = NA, align = "center"),
                             +                material = material, flow = flow, sample = sample, file = basename(f))
          + }
> 
  > all_data <- map_dfr(files, read_clean)

> # ---- Verify labeling ----
> all_data %>%
  +     distinct(material, flow, sample, file) %>%
  +     arrange(material, flow, sample) %>%
  +     print(n = 100)
# A tibble: 40 × 4
material flow  sample file                
<chr>    <chr> <chr>  <chr>               
  1 CF PETG  100   1      S2 CF PETG100%_1.txt
2 CF PETG  100   2      S2 CF PETG100%_2.txt
3 CF PETG  100   3      S2 CF PETG100%_3.txt
4 CF PETG  100   4      S2 CF PETG100%_4.txt
5 CF PETG  100   5      S2 CF PETG100%_5.txt
6 CF PETG  110   1      S2 CF PETG110%_1.txt
7 CF PETG  110   2      S2 CF PETG110%_2.txt
8 CF PETG  110   3      S2 CF PETG110%_3.txt
9 CF PETG  110   4      S2 CF PETG110%_4.txt
10 CF PETG  110   5      S2 CF PETG110%_5.txt
11 CF PETG  80    1      S2 CF PETG80%_1.txt 
12 CF PETG  80    2      S2 CF PETG80%_2.txt 
13 CF PETG  80    3      S2 CF PETG80%_3.txt 
14 CF PETG  80    4      S2 CF PETG80%_4.txt 
15 CF PETG  80    5      S2 CF PETG80%_5.txt 
16 CF PETG  90    1      S2 CF PETG90%_1.txt 
17 CF PETG  90    2      S2 CF PETG90%_2.txt 
18 CF PETG  90    3      S2 CF PETG90%_3.txt 
19 CF PETG  90    4      S2 CF PETG90%_4.txt 
20 CF PETG  90    5      S2 CF PETG90%_5.txt 
21 PETG     100   1      S2 PETG100%_1.txt   
22 PETG     100   2      S2 PETG100%_2.txt   
23 PETG     100   3      S2 PETG100%_3.txt   
24 PETG     100   4      S2 PETG100%_4.txt   
25 PETG     100   5      S2 PETG100%_5.txt   
26 PETG     110   1      S2 PETG110%_1.txt   
27 PETG     110   2      S2 PETG110%_2.txt   
28 PETG     110   3      S2 PETG110%_3.txt   
29 PETG     110   4      S2 PETG110%_4.txt   
30 PETG     110   5      S2 PETG110%_5.txt   
31 PETG     80    1      S2 PETG80%_1.txt    
32 PETG     80    2      S2 PETG80%_2.txt    
33 PETG     80    3      S2 PETG80%_3.txt    
34 PETG     80    4      S2 PETG80%_4.txt    
35 PETG     80    5      S2 PETG80%_5.txt    
36 PETG     90    1      S2 PETG90%_1.txt    
37 PETG     90    2      S2 PETG90%_2.txt    
38 PETG     90    3      S2 PETG90%_3.txt    
39 PETG     90    4      S2 PETG90%_4.txt    
40 PETG     90    5      S2 PETG90%_5.txt    
> # ---- Window comparison ----
> candidate_windows <- list(
  +     "ISO_0.05-0.25" = c(0.0005, 0.0025),
  +     "later_0.10-0.30" = c(0.0010, 0.0030),
  +     "later_0.15-0.35" = c(0.0015, 0.0035)
  + )
> 
  > window_comparison <- map_dfr(names(candidate_windows), function(wname) {
    +     rng <- candidate_windows[[wname]]
    +     all_data %>%
      +         filter(Strain >= rng[1], Strain <= rng[2]) %>%
      +         group_by(material, flow, sample) %>%
      +         filter(n() >= 5) %>%
      +         summarise(
        +             modulus_GPa = coef(lm(Stress ~ Strain))[2] / 1000,
        +             r2 = summary(lm(Stress ~ Strain))$r.squared,
        +             .groups = "drop"
        +         ) %>%
      +         mutate(window = wname)
    + })
> 
  > window_summary <- window_comparison %>%
  +     group_by(window) %>%
  +     summarise(mean_modulus = mean(modulus_GPa), sd_modulus = sd(modulus_GPa),
                  +               mean_r2 = mean(r2), min_r2 = min(r2), .groups = "drop")
> 
  > print(window_summary)
# A tibble: 3 × 5
window          mean_modulus sd_modulus mean_r2 min_r2
<chr>                  <dbl>      <dbl>   <dbl>  <dbl>
  1 ISO_0.05-0.25           4.22       2.15   0.993  0.779
2 later_0.10-0.30         4.21       2.17   0.999  0.987
3 later_0.15-0.35         4.21       2.14   0.999  0.987
> 
  > # find the worst-fitting sample(s) in the ISO window specifically
  > all_data %>%
  +     filter(Strain >= 0.0005, Strain <= 0.0025) %>%
  +     group_by(material, flow, sample) %>%
  +     summarise(r2 = summary(lm(Stress ~ Strain))$r.squared, .groups = "drop") %>%
  +     arrange(r2) %>%
  +     print(n = 10)
# A tibble: 40 × 4
material flow  sample    r2
<chr>    <chr> <chr>  <dbl>
  1 PETG     110   1      0.779
2 CF PETG  100   3      0.987
3 CF PETG  80    2      0.996
4 PETG     110   4      0.997
5 PETG     100   3      0.997
6 PETG     100   5      0.998
7 PETG     80    5      0.998
8 PETG     110   5      0.998
9 CF PETG  100   4      0.998
10 PETG     110   3      0.998
# ℹ 30 more rows
# ℹ Use `print(n = ...)` to see more rows
> # ---- Summary table ----
> summary_table <- all_data %>%
  +     mutate(flow = factor(flow, levels = c("80", "90", "100", "110"))) %>%
  +     group_by(material, flow, sample) %>%
  +     summarise(
    +         modulus_GPa = {
      +             w <- pick(everything()) %>% filter(Strain >= 0.0005, Strain <= 0.0025)
      +             coef(lm(Stress ~ Strain, data = w))[2] / 1000
      +         },
    +         tensile_strength_MPa = max(Stress, na.rm = TRUE),
    +         maximum_strain_pct = max(Strain, na.rm = TRUE) * 100,
    +         .groups = "drop"
    +     )
> 
  > print(summary_table, n = 100)
# A tibble: 40 × 6
material flow  sample modulus_GPa tensile_strength_MPa maximum_strain_pct
<chr>    <fct> <chr>        <dbl>                <dbl>              <dbl>
  1 CF PETG  80    1             6.07                29.8              0.510 
2 CF PETG  80    2             5.28                26.2              0.513 
3 CF PETG  80    3             6.14                30.3              0.510 
4 CF PETG  80    4             5.81                28.7              0.520 
5 CF PETG  80    5             5.84                28.9              0.505 
6 CF PETG  90    1             6.46                32.2              0.513 
7 CF PETG  90    2             6.26                31.0              0.511 
8 CF PETG  90    3             6.70                33.4              0.506 
9 CF PETG  90    4             6.33                31.1              0.508 
10 CF PETG  90    5             6.01                28.7              0.491 
11 CF PETG  100   1             5.87                30.1              0.514 
12 CF PETG  100   2             6.72                33.5              0.510 
13 CF PETG  100   3             6.05                29.3              0.497 
14 CF PETG  100   4             6.38                32.9              0.518 
15 CF PETG  100   5             6.44                33.2              0.521 
16 CF PETG  110   1             6.48                33.1              0.519 
17 CF PETG  110   2             6.54                32.1              0.503 
18 CF PETG  110   3             7.17                35.2              0.498 
19 CF PETG  110   4             6.48                31.9              0.517 
20 CF PETG  110   5             6.15                31.2              0.516 
21 PETG     80    1             2.10                11.0              0.535 
22 PETG     80    2             2.01                10.4              0.527 
23 PETG     80    3             2.12                11.1              0.530 
24 PETG     80    4             1.65                 9.14             0.522 
25 PETG     80    5             1.78                10.1              0.538 
26 PETG     90    1             2.16                11.2              0.525 
27 PETG     90    2             1.95                10.8              0.544 
28 PETG     90    3             1.66                 9.69             0.543 
29 PETG     90    4             1.94                10.6              0.540 
30 PETG     90    5             2.20                11.7              0.530 
31 PETG     100   1             1.95                10.6              0.536 
32 PETG     100   2             2.23                11.6              0.529 
33 PETG     100   3             1.87                10.5              0.532 
34 PETG     100   4             2.23                11.3              0.514 
35 PETG     100   5             1.89                10.6              0.528 
36 PETG     110   1             5.14                 7.80             0.0815
37 PETG     110   2             2.26                12.1              0.535 
38 PETG     110   3             2.37                12.1              0.519 
39 PETG     110   4             1.84                10.7              0.539 
40 PETG     110   5             2.11                11.4              0.528 
> write_csv(summary_table, "summary_table_SET-2.csv")

> # ---- ANOVA + Tukey ----
> aov_modulus <- aov(modulus_GPa ~ material * flow, data = summary_table)
> summary(aov_modulus)
Df Sum Sq Mean Sq F value Pr(>F)    
material       1 166.79  166.79 559.813 <2e-16 ***
  flow           3   3.09    1.03   3.462 0.0276 *  
  material:flow  3   0.55    0.18   0.612 0.6125    
Residuals     32   9.53    0.30                   
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> TukeyHSD(aov_modulus)
Tukey multiple comparisons of means
95% family-wise confidence level

Fit: aov(formula = modulus_GPa ~ material * flow, data = summary_table)

$material
diff       lwr       upr p adj
PETG-CF PETG -4.083944 -4.435533 -3.732355     0

$flow
diff        lwr       upr     p adj
90-80    0.288278129 -0.3730853 0.9496415 0.6429880
100-80   0.281435894 -0.3799275 0.9427993 0.6601154
110-80   0.773321827  0.1119584 1.4346852 0.0168523
100-90  -0.006842234 -0.6682056 0.6545211 0.9999919
110-90   0.485043698 -0.1763197 1.1464071 0.2139775
110-100  0.491885932 -0.1694774 1.1532493 0.2037115

$`material:flow`
diff        lwr       upr     p adj
PETG:80-CF PETG:80      -3.89339991 -5.0116505 -2.775149 0.0000000
CF PETG:90-CF PETG:80    0.52574446 -0.5925061  1.643995 0.7896444
PETG:90-CF PETG:80      -3.84258812 -4.9608387 -2.724338 0.0000000
CF PETG:100-CF PETG:80   0.46339045 -0.6548601  1.581641 0.8754990
PETG:100-CF PETG:80     -3.79391857 -4.9121691 -2.675668 0.0000000
CF PETG:110-CF PETG:80   0.73498896 -0.3832616  1.853239 0.4189898
PETG:110-CF PETG:80     -3.08174522 -4.1999958 -1.963495 0.0000000
CF PETG:90-PETG:80       4.41914438  3.3008938  5.537395 0.0000000
PETG:90-PETG:80          0.05081179 -1.0674387  1.169062 0.9999999
CF PETG:100-PETG:80      4.35679036  3.2385398  5.475041 0.0000000
PETG:100-PETG:80         0.09948134 -1.0187692  1.217732 0.9999898
CF PETG:110-PETG:80      4.62838887  3.5101383  5.746639 0.0000000
PETG:110-PETG:80         0.81165470 -0.3065958  1.929905 0.2987802
PETG:90-CF PETG:90      -4.36833258 -5.4865831 -3.250082 0.0000000
CF PETG:100-CF PETG:90  -0.06235402 -1.1806046  1.055897 0.9999996
PETG:100-CF PETG:90     -4.31966303 -5.4379136 -3.201412 0.0000000
CF PETG:110-CF PETG:90   0.20924449 -0.9090060  1.327495 0.9985448
PETG:110-CF PETG:90     -3.60748968 -4.7257402 -2.489239 0.0000000
CF PETG:100-PETG:90      4.30597857  3.1877280  5.424229 0.0000000
PETG:100-PETG:90         0.04866955 -1.0695810  1.166920 0.9999999
CF PETG:110-PETG:90      4.57757707  3.4593265  5.695828 0.0000000
PETG:110-PETG:90         0.76084290 -0.3574076  1.879093 0.3760309
PETG:100-CF PETG:100    -4.25730902 -5.3755596 -3.139058 0.0000000
CF PETG:110-CF PETG:100  0.27159851 -0.8466520  1.389849 0.9926779
PETG:110-CF PETG:100    -3.54513566 -4.6633862 -2.426885 0.0000000
CF PETG:110-PETG:100     4.52890753  3.4106570  5.647158 0.0000000
PETG:110-PETG:100        0.71217335 -0.4060772  1.830424 0.4585835
PETG:110-CF PETG:110    -3.81673417 -4.9349847 -2.698484 0.0000000

> 
  > aov_strength <- aov(tensile_strength_MPa ~ material * flow, data = summary_table)
> summary(aov_strength)
Df Sum Sq Mean Sq  F value  Pr(>F)    
material       1   4170    4170 2050.948 < 2e-16 ***
  flow           3     28       9    4.580 0.00888 ** 
  material:flow  3     16       5    2.605 0.06881 .  
Residuals     32     65       2                     
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> TukeyHSD(aov_strength)
Tukey multiple comparisons of means
95% family-wise confidence level

Fit: aov(formula = tensile_strength_MPa ~ material * flow, data = summary_table)

$material
diff       lwr       upr p adj
PETG-CF PETG -20.42084 -21.33933 -19.50236     0

$flow
diff        lwr     upr     p adj
90-80   1.49031 -0.2374298 3.21805 0.1107602
100-80  1.81906  0.0913202 3.54680 0.0360151
110-80  2.20718  0.4794402 3.93492 0.0080059
100-90  0.32875 -1.3989898 2.05649 0.9547702
110-90  0.71687 -1.0108698 2.44461 0.6775686
110-100 0.38812 -1.3396198 2.11586 0.9285898

$`material:flow`
diff         lwr        upr     p adj
PETG:80-CF PETG:80      -18.43232 -21.3536277 -15.511012 0.0000000
CF PETG:90-CF PETG:80     2.50880  -0.4125077   5.430108 0.1354584
PETG:90-CF PETG:80      -17.96050 -20.8818077 -15.039192 0.0000000
CF PETG:100-CF PETG:80    3.04380   0.1224923   5.965108 0.0362902
PETG:100-CF PETG:80     -17.83800 -20.7593077 -14.916692 0.0000000
CF PETG:110-CF PETG:80    3.94100   1.0196923   6.862308 0.0027575
PETG:110-CF PETG:80     -17.95896 -20.8802677 -15.037652 0.0000000
CF PETG:90-PETG:80       20.94112  18.0198123  23.862428 0.0000000
PETG:90-PETG:80           0.47182  -2.4494877   3.393128 0.9994374
CF PETG:100-PETG:80      21.47612  18.5548123  24.397428 0.0000000
PETG:100-PETG:80          0.59432  -2.3269877   3.515628 0.9975301
CF PETG:110-PETG:80      22.37332  19.4520123  25.294628 0.0000000
PETG:110-PETG:80          0.47336  -2.4479477   3.394668 0.9994253
PETG:90-CF PETG:90      -20.46930 -23.3906077 -17.547992 0.0000000
CF PETG:100-CF PETG:90    0.53500  -2.3863077   3.456308 0.9987316
PETG:100-CF PETG:90     -20.34680 -23.2681077 -17.425492 0.0000000
CF PETG:110-CF PETG:90    1.43220  -1.4891077   4.353508 0.7536237
PETG:110-CF PETG:90     -20.46776 -23.3890677 -17.546452 0.0000000
CF PETG:100-PETG:90      21.00430  18.0829923  23.925608 0.0000000
PETG:100-PETG:90          0.12250  -2.7988077   3.043808 0.9999999
CF PETG:110-PETG:90      21.90150  18.9801923  24.822808 0.0000000
PETG:110-PETG:90          0.00154  -2.9197677   2.922848 1.0000000
PETG:100-CF PETG:100    -20.88180 -23.8031077 -17.960492 0.0000000
CF PETG:110-CF PETG:100   0.89720  -2.0241077   3.818508 0.9718987
PETG:110-CF PETG:100    -21.00276 -23.9240677 -18.081452 0.0000000
CF PETG:110-PETG:100     21.77900  18.8576923  24.700308 0.0000000
PETG:110-PETG:100        -0.12096  -3.0422677   2.800348 0.9999999
PETG:110-CF PETG:110    -21.89996 -24.8212677 -18.978652 0.0000000

> 
  > aov_strain <- aov(maximum_strain_pct ~ material * flow, data = summary_table)
> summary(aov_strain)
Df  Sum Sq  Mean Sq F value Pr(>F)
material       1 0.00002 0.000018   0.004  0.953
flow           3 0.01531 0.005105   1.001  0.405
material:flow  3 0.01605 0.005350   1.049  0.384
Residuals     32 0.16314 0.005098               
> TukeyHSD(aov_strain)
Tukey multiple comparisons of means
95% family-wise confidence level

Fit: aov(formula = maximum_strain_pct ~ material * flow, data = summary_table)

$material
diff        lwr        upr     p adj
PETG-CF PETG -0.001338073 -0.0473305 0.04465435 0.9531126

$flow
diff         lwr        upr     p adj
90-80   -7.557376e-05 -0.08659059 0.08643944 1.0000000
100-80  -1.151446e-03 -0.08766646 0.08536357 0.9999828
110-80  -4.558352e-02 -0.13209854 0.04093150 0.4920105
100-90  -1.075872e-03 -0.08759089 0.08543914 0.9999859
110-90  -4.550795e-02 -0.13202296 0.04100707 0.4934266
110-100 -4.443208e-02 -0.13094709 0.04208294 0.5137093

$`material:flow`
diff        lwr        upr     p adj
PETG:80-CF PETG:80       0.0185755290 -0.1277063 0.16485739 0.9998856
CF PETG:90-CF PETG:80   -0.0061142253 -0.1523961 0.14016763 0.9999999
PETG:90-CF PETG:80       0.0245386067 -0.1217433 0.17082046 0.9992799
CF PETG:100-CF PETG:80   0.0004141021 -0.1458678 0.14669596 1.0000000
PETG:100-CF PETG:80      0.0158585343 -0.1304233 0.16214039 0.9999607
CF PETG:110-CF PETG:80  -0.0012832147 -0.1475651 0.14499864 1.0000000
PETG:110-CF PETG:80     -0.0713082993 -0.2175902 0.07497356 0.7587509
CF PETG:90-PETG:80      -0.0246897542 -0.1709716 0.12159210 0.9992506
PETG:90-PETG:80          0.0059630777 -0.1403188 0.15224494 1.0000000
CF PETG:100-PETG:80     -0.0181614268 -0.1644433 0.12812043 0.9999017
PETG:100-PETG:80        -0.0027169946 -0.1489989 0.14356486 1.0000000
CF PETG:110-PETG:80     -0.0198587437 -0.1661406 0.12642311 0.9998213
PETG:110-PETG:80        -0.0898838283 -0.2361657 0.05639803 0.5034923
PETG:90-CF PETG:90       0.0306528320 -0.1156290 0.17693469 0.9970290
CF PETG:100-CF PETG:90   0.0065283274 -0.1397535 0.15281019 0.9999999
PETG:100-CF PETG:90      0.0219727596 -0.1243091 0.16825462 0.9996505
CF PETG:110-CF PETG:90   0.0048310106 -0.1414508 0.15111287 1.0000000
PETG:110-CF PETG:90     -0.0651940740 -0.2114759 0.08108778 0.8300942
CF PETG:100-PETG:90     -0.0241245046 -0.1704064 0.12215735 0.9993553
PETG:100-PETG:90        -0.0086800724 -0.1549619 0.13760179 0.9999994
CF PETG:110-PETG:90     -0.0258218214 -0.1721037 0.12046004 0.9989986
PETG:110-PETG:90        -0.0958469060 -0.2421288 0.05043495 0.4228945
PETG:100-CF PETG:100     0.0154444322 -0.1308374 0.16172629 0.9999671
CF PETG:110-CF PETG:100 -0.0016973168 -0.1479792 0.14458454 1.0000000
PETG:110-CF PETG:100    -0.0717224014 -0.2180043 0.07455946 0.7535445
CF PETG:110-PETG:100    -0.0171417491 -0.1634236 0.12914011 0.9999334
PETG:110-PETG:100       -0.0871668336 -0.2334487 0.05911502 0.5415024
PETG:110-CF PETG:110    -0.0700250846 -0.2163069 0.07625677 0.7746102

> # ---- Average curves ----
> strain_grid <- seq(0, 0.005, length.out = 200)
> 
  > interpolate_sample <- function(df) {
    +     approx(x = df$Strain, y = df$Stress, xout = strain_grid, rule = 1)$y
    + }
> 
  > interpolated <- all_data %>%
  +     group_by(material, flow, sample) %>%
  +     group_modify(~ tibble(Strain = strain_grid, Stress = interpolate_sample(.x))) %>%
  +     ungroup()
> 
  > avg_curves <- interpolated %>%
  +     group_by(material, flow, Strain) %>%
  +     summarise(mean_stress = mean(Stress, na.rm = TRUE),
                  +               sd_stress = sd(Stress, na.rm = TRUE), .groups = "drop") %>%
  +     filter(!is.na(mean_stress))
> # ---- Output folder ----
> output_dir <- "C:/Users/vipul/Desktop/MSc Project/New Folder/Testing Data/Vipul Dude/Modulus/SET-2/CF Graphs"
> dir.create(output_dir, showWarnings = FALSE)
> # ---- Figure 1: Individual specimen plots (full + zoomed) ----
> combos <- all_data %>% distinct(material, flow, sample)
> 
  > for (i in seq_len(nrow(combos))) {
    +     mat <- combos$material[i]; fl <- combos$flow[i]; sm <- combos$sample[i]
    +     sample_data <- all_data %>% filter(material == mat, flow == fl, sample == sm)
    +     
      +     fit_data <- sample_data %>% filter(Strain >= 0.0005, Strain <= 0.0025)
      +     fit <- lm(Stress ~ Strain, data = fit_data)
      +     E_GPa <- round(coef(fit)[2] / 1000, 2)
      +     
        +     fit_line <- tibble(Strain = seq(0.0005, 0.0025, length.out = 100)) %>%
          +         mutate(Stress = predict(fit, newdata = .))
        +     
          +     base_plot <- function(data, xmax) {
            +         ggplot(data, aes(x = Strain*100, y = StressSmooth)) +
              +             annotate("rect", xmin = 0.05, xmax = 0.25, ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "orange") +
              +             geom_line(color = "steelblue", linewidth = 0.9) +
              +             geom_line(data = fit_line, aes(x = Strain*100, y = Stress), color = "red", linewidth = 1) +
              +             annotate("text", x = 0.25, y = max(data$Stress, na.rm = TRUE) * 0.15,
                                     +                      label = paste0("E = ", E_GPa, " GPa"), color = "red", size = 3.5, hjust = 0) +
              +             xlim(0, xmax) + labs(x = "Strain (%)", y = "Stress (MPa)") + theme_minimal()
            +     }
          +     
            +     p_full <- base_plot(sample_data, 0.55) + labs(title = paste0(mat, " — ", fl, "% flow — Sample ", sm, " (full)"))
            +     p_zoom <- base_plot(sample_data %>% filter(Strain*100 <= 0.30), 0.30) + labs(title = paste0(mat, " — ", fl, "% flow — Sample ", sm, " (zoomed)"))
            +     
              +     fname_base <- paste0(gsub(" ", "_", mat), "_", fl, "pct_sample", sm)
              +     ggsave(file.path(output_dir, paste0("fig1_full_", fname_base, ".png")), p_full, width = 6, height = 5, dpi = 150)
              +     ggsave(file.path(output_dir, paste0("fig1_zoom_", fname_base, ".png")), p_zoom, width = 6, height = 5, dpi = 150)
              + }
There were 50 or more warnings (use warnings() to see the first 50)

> warnings()
Warning messages:
  1: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
2: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
3: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
4: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
5: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
6: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
7: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
8: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
9: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
10: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
11: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
12: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
13: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
14: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
15: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
16: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
17: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
18: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
19: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
20: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
21: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
22: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
23: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
24: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
25: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
26: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
27: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
28: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
29: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
30: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
31: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
32: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
33: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
34: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
35: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
36: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
37: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
38: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
39: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
40: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
41: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
42: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
43: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
44: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
45: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
46: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
47: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
48: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
49: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
50: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).

> warnings()
Warning messages:
  1: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
2: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
3: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
4: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
5: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
6: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
7: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
8: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
9: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
10: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
11: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
12: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
13: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
14: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
15: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
16: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
17: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
18: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
19: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
20: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
21: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
22: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
23: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
24: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
25: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
26: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
27: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
28: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
29: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
30: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
31: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
32: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
33: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
34: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
35: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
36: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
37: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
38: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
39: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
40: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
41: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
42: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
43: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
44: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
45: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
46: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
47: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
48: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
49: Removed 4 rows containing missing values or values outside the scale range (`geom_line()`).
50: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).

> # ---- Figure 2: Average curves with E label ----
> avg_modulus_per_flow <- summary_table %>%
  +     group_by(material, flow) %>%
  +     summarise(mean_E_GPa = mean(modulus_GPa), sd_E_GPa = sd(modulus_GPa), .groups = "drop")
> 
  > combos_avg <- avg_curves %>% distinct(material, flow)
> 
  > for (i in seq_len(nrow(combos_avg))) {
    +     mat <- combos_avg$material[i]; fl <- combos_avg$flow[i]
    +     E_row <- avg_modulus_per_flow %>% filter(material == mat, flow == fl)
    +     E_label <- paste0("Avg E = ", round(E_row$mean_E_GPa, 2), " ± ", round(E_row$sd_E_GPa, 2), " GPa")
    +     
      +     plot_data <- avg_curves %>% filter(material == mat, flow == fl)
      +     fit_window <- plot_data %>% filter(Strain >= 0.0005, Strain <= 0.0025)
      +     fit <- lm(mean_stress ~ Strain, data = fit_window)
      +     fit_line <- tibble(Strain = seq(0.0005, 0.0025, length.out = 100)) %>%
        +         mutate(mean_stress = predict(fit, newdata = .))
      +     
        +     p <- plot_data %>%
          +         ggplot(aes(x = Strain*100, y = mean_stress)) +
          +         annotate("rect", xmin = 0.05, xmax = 0.25, ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "orange") +
          +         geom_ribbon(aes(ymin = mean_stress - sd_stress, ymax = mean_stress + sd_stress), alpha = 0.2, fill = "steelblue") +
          +         geom_line(color = "steelblue", linewidth = 1) +
          +         geom_line(data = fit_line, aes(x = Strain*100, y = mean_stress), color = "red", linewidth = 1) +
          +         annotate("text", x = 0.27, y = max(plot_data$mean_stress, na.rm = TRUE) * 0.15,
                             +                  label = E_label, color = "red", size = 3.5, hjust = 0) +
          +         labs(x = "Strain (%)", y = "Stress (MPa)",
                         +              title = paste0(mat, " — ", fl, "% flow — Average curve (n=5, ±1 SD) — SET-2")) +
          +         theme_minimal()
        +     
          +     ggsave(file.path(output_dir, paste0("fig2_average_", gsub(" ", "_", mat), "_", fl, "pct.png")), p, width = 7, height = 5, dpi = 150)
        + }
Warning messages:
  1: Removed 1 row containing missing values or values outside the scale range (`geom_ribbon()`). 
2: Removed 1 row containing missing values or values outside the scale range (`geom_ribbon()`). 
3: Removed 1 row containing missing values or values outside the scale range (`geom_ribbon()`). 
4: Removed 2 rows containing missing values or values outside the scale range (`geom_ribbon()`). 
5: Removed 1 row containing missing values or values outside the scale range (`geom_ribbon()`). 
6: Removed 1 row containing missing values or values outside the scale range (`geom_ribbon()`). 
7: Removed 1 row containing missing values or values outside the scale range (`geom_ribbon()`). 

> # ---- Figure 3: Boxplots ----
> p_modulus <- ggplot(summary_table, aes(x = flow, y = modulus_GPa, fill = material)) +
  +     geom_boxplot(position = position_dodge(0.8), width = 0.6, alpha = 0.7) +
  +     geom_point(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), alpha = 0.5) +
  +     labs(x = "Flow %", y = "Modulus (GPa)", title = "Young's Modulus by Material and Flow % — SET-2") +
  +     theme_minimal()
> 
  > p_strength <- ggplot(summary_table, aes(x = flow, y = tensile_strength_MPa, fill = material)) +
  +     geom_boxplot(position = position_dodge(0.8), width = 0.6, alpha = 0.7) +
  +     geom_point(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), alpha = 0.5) +
  +     labs(x = "Flow %", y = "Tensile Strength (MPa)", title = "Tensile Strength by Material and Flow % — SET-2") +
  +     theme_minimal()
> 
  > p_strain <- ggplot(summary_table, aes(x = flow, y = maximum_strain_pct, fill = material)) +
  +     geom_boxplot(position = position_dodge(0.8), width = 0.6, alpha = 0.7) +
  +     geom_point(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), alpha = 0.5) +
  +     labs(x = "Flow %", y = "Maximum Strain (%)", title = "Maximum Strain by Material and Flow % — SET-2") +
  +     theme_minimal()
> 
  > ggsave(file.path(output_dir, "fig3_boxplot_modulus_SET-2.png"), p_modulus, width = 9, height = 6, dpi = 150)
> ggsave(file.path(output_dir, "fig3_boxplot_strength_SET-2.png"), p_strength, width = 9, height = 6, dpi = 150)
> ggsave(file.path(output_dir, "fig3_boxplot_strain_SET-2.png"), p_strain, width = 9, height = 6, dpi = 150)
> all_data %>%
  +     filter(material == "PETG", flow == "110", Strain >= 0.0005, Strain <= 0.0025) %>%
  +     group_by(sample) %>%
  +     summarise(r2 = summary(lm(Stress ~ Strain))$r.squared, .groups = "drop") %>%
  +     arrange(r2)
# A tibble: 5 × 2
sample    r2
<chr>  <dbl>
  1 1      0.779
2 4      0.997
3 5      0.998
4 3      0.998
5 2      1.000
> raw_check <- read_tsv("PUT_EXACT_FILENAME_HERE.txt", skip = 1, locale = locale(encoding = "UTF-16LE"),
                        +                       col_names = c("Time","Load","Ext","Stress","ExtPreload","Strain","PctStrain","GaugeLen"),
                        +                       show_col_types = FALSE)
Error in open.connection(3L, "rb") : cannot open the connection
In addition: Warning message:
  In open.connection(3L, "rb") :
  cannot open file 'PUT_EXACT_FILENAME_HERE.txt': No such file or directory

> # find the exact file path for PETG 110% sample 1
  > target_file <- files[str_detect(files, "PETG") & str_detect(files, "110") & 
                           +                          str_detect(basename(files), "_1\\.txt$")]
> target_file
[1] "./CF PETG/S2 CF PETG110%_1.txt" "./PETG/S2 PETG110%_1.txt"      
> raw_check <- read_tsv(target_file, skip = 1, locale = locale(encoding = "UTF-16LE"),
                        +                       col_names = c("Time","Load","Ext","Stress","ExtPreload","Strain","PctStrain","GaugeLen"),
                        +                       show_col_types = FALSE)