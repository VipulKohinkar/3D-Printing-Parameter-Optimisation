> cat("Done — 16 audit plots saved to:", output_dir, "\n")
Error: object 'output_dir' not found

> exists("output_dir")
[1] FALSE
> exists("all_data")
[1] FALSE
> exists("summary_table")
[1] FALSE
> exists("representative_modulus")
[1] FALSE
> library(tidyverse)
── Attaching core tidyverse packages ────────────────────────────────────────────────── tidyverse 2.0.0 ──
✔ dplyr     1.2.0     ✔ readr     2.2.0
✔ forcats   1.0.1     ✔ stringr   1.6.0
✔ ggplot2   4.0.2     ✔ tibble    3.3.1
✔ lubridate 1.9.5     ✔ tidyr     1.3.2
✔ purrr     1.2.1     
── Conflicts ──────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
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

> path_set1 <- "C:/Users/vipul/Desktop/MSc Project/New Folder/Testing Data/Vipul Dude/Modulus/SET-1"
> path_set2 <- "C:/Users/vipul/Desktop/MSc Project/New Folder/Testing Data/Vipul Dude/Modulus/SET-2"
> output_dir <- "C:/Users/vipul/Desktop/MSc Project/New Folder/Testing Data/Vipul Dude/Modulus/Graphs"
> dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
> 
  > exists("output_dir")
[1] TRUE
> read_clean_modulus <- function(f, set_label) {
  +     parts <- str_split(f, "/")[[1]]
  +     material <- parts[length(parts) - 1]
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
                           +                material = material, flow = flow, sample = sample,
                           +                file = basename(f), set = set_label)
        + }
> 
  > exists("read_clean_modulus")
[1] TRUE
> library(zoo)

Attaching package: ‘zoo’

The following objects are masked from ‘package:base’:
  
  as.Date, as.Date.numeric

Warning message:
  package ‘zoo’ was built under R version 4.5.3 
> 
  > files_set1 <- list.files(path_set1, pattern = "\\.txt$", recursive = TRUE, full.names = TRUE)
> files_set2 <- list.files(path_set2, pattern = "\\.txt$", recursive = TRUE, full.names = TRUE)
> 
  > length(files_set1)
[1] 40
> length(files_set2)
[1] 40
> all_data <- bind_rows(
  +     map_dfr(files_set1, read_clean_modulus, set_label = "SET-1"),
  +     map_dfr(files_set2, read_clean_modulus, set_label = "SET-2")
  + )
>                                                                                                         
  > nrow(all_data)
[1] 70872
> label_check <- all_data %>%
  +     distinct(set, material, flow, sample, file) %>%
  +     arrange(set, material, flow, sample)
> 
  > nrow(label_check)
[1] 80
> print(label_check, n = 100)
# A tibble: 80 × 5
set   material flow  sample file                
<chr> <chr>    <chr> <chr>  <chr>               
  1 SET-1 CF PETG  100   1      CF PETG 100%_1.txt  
2 SET-1 CF PETG  100   2      CF PETG 100%_2.txt  
3 SET-1 CF PETG  100   3      CF PETG 100%_3.txt  
4 SET-1 CF PETG  100   4      CF PETG 100%_4.txt  
5 SET-1 CF PETG  100   5      CF PETG 100%_5.txt  
6 SET-1 CF PETG  110   1      CF PETG 110%_1.txt  
7 SET-1 CF PETG  110   2      CF PETG 110%_2.txt  
8 SET-1 CF PETG  110   3      CF PETG 110%_3.txt  
9 SET-1 CF PETG  110   4      CF PETG 110%_4.txt  
10 SET-1 CF PETG  110   5      CF PETG 110%_5.txt  
11 SET-1 CF PETG  80    1      CF PETG 80%_1.txt   
12 SET-1 CF PETG  80    2      CF PETG 80%_2.txt   
13 SET-1 CF PETG  80    3      CF PETG 80%_3.txt   
14 SET-1 CF PETG  80    4      CF PETG 80%_4.txt   
15 SET-1 CF PETG  80    5      CF PETG 80%_5.txt   
16 SET-1 CF PETG  90    1      CF PETG 90%_1.txt   
17 SET-1 CF PETG  90    2      CF PETG 90%_2.txt   
18 SET-1 CF PETG  90    3      CF PETG 90%_3.txt   
19 SET-1 CF PETG  90    4      CF PETG 90%_4.txt   
20 SET-1 CF PETG  90    5      CF PETG 90%_5.txt   
21 SET-1 PETG     100   1      PETG 100%_1.txt     
22 SET-1 PETG     100   2      PETG 100%_2.txt     
23 SET-1 PETG     100   3      PETG 100%_3.txt     
24 SET-1 PETG     100   4      PETG 100%_4.txt     
25 SET-1 PETG     100   5      PETG 100%_5.txt     
26 SET-1 PETG     110   1      PETG 110%_1.txt     
27 SET-1 PETG     110   2      PETG 110%_2.txt     
28 SET-1 PETG     110   3      PETG 110%_3.txt     
29 SET-1 PETG     110   4      PETG 110%_4.txt     
30 SET-1 PETG     110   5      PETG 110%_5.txt     
31 SET-1 PETG     80    1      PETG 80%_1.txt      
32 SET-1 PETG     80    2      PETG 80%_2.txt      
33 SET-1 PETG     80    3      PETG 80%_3.txt      
34 SET-1 PETG     80    4      PETG 80%_4.txt      
35 SET-1 PETG     80    5      PETG 80%_5.txt      
36 SET-1 PETG     90    1      PETG 90%_1.txt      
37 SET-1 PETG     90    2      PETG 90%_2.txt      
38 SET-1 PETG     90    3      PETG 90%_3.txt      
39 SET-1 PETG     90    4      PETG 90%_4.txt      
40 SET-1 PETG     90    5      PETG 90%_5.txt      
41 SET-2 CF PETG  100   1      S2 CF PETG100%_1.txt
42 SET-2 CF PETG  100   2      S2 CF PETG100%_2.txt
43 SET-2 CF PETG  100   3      S2 CF PETG100%_3.txt
44 SET-2 CF PETG  100   4      S2 CF PETG100%_4.txt
45 SET-2 CF PETG  100   5      S2 CF PETG100%_5.txt
46 SET-2 CF PETG  110   1      S2 CF PETG110%_1.txt
47 SET-2 CF PETG  110   2      S2 CF PETG110%_2.txt
48 SET-2 CF PETG  110   3      S2 CF PETG110%_3.txt
49 SET-2 CF PETG  110   4      S2 CF PETG110%_4.txt
50 SET-2 CF PETG  110   5      S2 CF PETG110%_5.txt
51 SET-2 CF PETG  80    1      S2 CF PETG80%_1.txt 
52 SET-2 CF PETG  80    2      S2 CF PETG80%_2.txt 
53 SET-2 CF PETG  80    3      S2 CF PETG80%_3.txt 
54 SET-2 CF PETG  80    4      S2 CF PETG80%_4.txt 
55 SET-2 CF PETG  80    5      S2 CF PETG80%_5.txt 
56 SET-2 CF PETG  90    1      S2 CF PETG90%_1.txt 
57 SET-2 CF PETG  90    2      S2 CF PETG90%_2.txt 
58 SET-2 CF PETG  90    3      S2 CF PETG90%_3.txt 
59 SET-2 CF PETG  90    4      S2 CF PETG90%_4.txt 
60 SET-2 CF PETG  90    5      S2 CF PETG90%_5.txt 
61 SET-2 PETG     100   1      S2 PETG100%_1.txt   
62 SET-2 PETG     100   2      S2 PETG100%_2.txt   
63 SET-2 PETG     100   3      S2 PETG100%_3.txt   
64 SET-2 PETG     100   4      S2 PETG100%_4.txt   
65 SET-2 PETG     100   5      S2 PETG100%_5.txt   
66 SET-2 PETG     110   1      S2 PETG110%_1.txt   
67 SET-2 PETG     110   2      S2 PETG110%_2.txt   
68 SET-2 PETG     110   3      S2 PETG110%_3.txt   
69 SET-2 PETG     110   4      S2 PETG110%_4.txt   
70 SET-2 PETG     110   5      S2 PETG110%_5.txt   
71 SET-2 PETG     80    1      S2 PETG80%_1.txt    
72 SET-2 PETG     80    2      S2 PETG80%_2.txt    
73 SET-2 PETG     80    3      S2 PETG80%_3.txt    
74 SET-2 PETG     80    4      S2 PETG80%_4.txt    
75 SET-2 PETG     80    5      S2 PETG80%_5.txt    
76 SET-2 PETG     90    1      S2 PETG90%_1.txt    
77 SET-2 PETG     90    2      S2 PETG90%_2.txt    
78 SET-2 PETG     90    3      S2 PETG90%_3.txt    
79 SET-2 PETG     90    4      S2 PETG90%_4.txt    
80 SET-2 PETG     90    5      S2 PETG90%_5.txt    
> summary_table <- all_data %>%
  +     mutate(flow = factor(flow, levels = c("80", "90", "100", "110"))) %>%
  +     group_by(set, material, flow, sample) %>%
  +     summarise(
    +         fit_stats = list({
      +             w <- pick(everything()) %>% filter(Strain >= 0.0005, Strain <= 0.0025)
      +             min_strain <- min(pick(everything())$Strain)
      +             max_strain <- max(pick(everything())$Strain)
      +             covers_window <- (min_strain <= 0.0005 & max_strain >= 0.0025)
      +             if (nrow(w) < 5) {
        +                 tibble(modulus_GPa = NA_real_, r2 = NA_real_, n_fit_points = nrow(w), covers_window = covers_window)
        +             } else {
          +                 m <- lm(Stress ~ Strain, data = w)
          +                 tibble(modulus_GPa = coef(m)[2]/1000, r2 = summary(m)$r.squared,
                                   +                        n_fit_points = nrow(w), covers_window = covers_window)
          +             }
      +         }),
    +         .groups = "drop"
    +     ) %>%
  +     unnest(fit_stats)
> 
  > nrow(summary_table)
[1] 80
> print(summary_table, n = 100)
# A tibble: 80 × 8
set   material flow  sample modulus_GPa    r2 n_fit_points covers_window
<chr> <chr>    <fct> <chr>        <dbl> <dbl>        <int> <lgl>        
  1 SET-1 CF PETG  80    1             4.97 0.999          377 TRUE         
2 SET-1 CF PETG  80    2             5.02 1.000          364 TRUE         
3 SET-1 CF PETG  80    3             4.23 0.998          370 TRUE         
4 SET-1 CF PETG  80    4             5.26 1.000          372 TRUE         
5 SET-1 CF PETG  80    5             5.01 1.000          353 TRUE         
6 SET-1 CF PETG  90    1             5.01 1.000          366 TRUE         
7 SET-1 CF PETG  90    2             4.64 0.999          342 TRUE         
8 SET-1 CF PETG  90    3             4.91 1.000          349 TRUE         
9 SET-1 CF PETG  90    4             5.17 1.000           33 TRUE         
10 SET-1 CF PETG  90    5             5.69 0.998          371 TRUE         
11 SET-1 CF PETG  100   1             5.11 1.000           36 TRUE         
12 SET-1 CF PETG  100   2             5.08 1.000          353 TRUE         
13 SET-1 CF PETG  100   3             5.33 1.000          364 TRUE         
14 SET-1 CF PETG  100   4             5.11 1.000          352 TRUE         
15 SET-1 CF PETG  100   5             5.29 0.999          356 TRUE         
16 SET-1 CF PETG  110   1             4.87 0.996          329 TRUE         
17 SET-1 CF PETG  110   2             4.44 0.997          316 TRUE         
18 SET-1 CF PETG  110   3             4.95 0.999          347 TRUE         
19 SET-1 CF PETG  110   4             5.32 1.000          359 TRUE         
20 SET-1 CF PETG  110   5             5.22 1.000          358 TRUE         
21 SET-1 PETG     80    1             2.03 1.000          334 TRUE         
22 SET-1 PETG     80    2             2.01 0.999          334 TRUE         
23 SET-1 PETG     80    3             1.96 0.999          318 TRUE         
24 SET-1 PETG     80    4             1.89 1.000          343 TRUE         
25 SET-1 PETG     80    5             2.47 0.992          352 TRUE         
26 SET-1 PETG     90    1             2.08 1.000          337 TRUE         
27 SET-1 PETG     90    2             1.95 0.999          297 TRUE         
28 SET-1 PETG     90    3             1.91 0.993          302 TRUE         
29 SET-1 PETG     90    4             2.23 1.000          342 TRUE         
30 SET-1 PETG     90    5             2.22 0.999          327 TRUE         
31 SET-1 PETG     100   1             2.17 1.000          329 TRUE         
32 SET-1 PETG     100   2             2.31 0.999          338 TRUE         
33 SET-1 PETG     100   3             2.27 1.000          333 TRUE         
34 SET-1 PETG     100   4             2.14 1.000          332 TRUE         
35 SET-1 PETG     100   5             2.11 1.000          330 TRUE         
36 SET-1 PETG     110   1             2.58 0.999          359 TRUE         
37 SET-1 PETG     110   2             2.01 0.999          329 TRUE         
38 SET-1 PETG     110   3             2.17 1.000          335 TRUE         
39 SET-1 PETG     110   4             2.11 0.999          339 TRUE         
40 SET-1 PETG     110   5             2.31 0.999          337 TRUE         
41 SET-2 CF PETG  80    1             6.07 0.999          354 TRUE         
42 SET-2 CF PETG  80    2             5.28 0.996          338 TRUE         
43 SET-2 CF PETG  80    3             6.14 1.000          374 TRUE         
44 SET-2 CF PETG  80    4             5.81 0.999          365 TRUE         
45 SET-2 CF PETG  80    5             5.84 0.999          360 TRUE         
46 SET-2 CF PETG  90    1             6.46 0.999          357 TRUE         
47 SET-2 CF PETG  90    2             6.26 1.000          372 TRUE         
48 SET-2 CF PETG  90    3             6.70 1.000          371 TRUE         
49 SET-2 CF PETG  90    4             6.33 1.000          372 TRUE         
50 SET-2 CF PETG  90    5             6.01 0.999          358 TRUE         
51 SET-2 CF PETG  100   1             5.87 1.000          352 TRUE         
52 SET-2 CF PETG  100   2             6.72 1.000          369 TRUE         
53 SET-2 CF PETG  100   3             6.05 0.987          352 TRUE         
54 SET-2 CF PETG  100   4             6.38 0.998          358 TRUE         
55 SET-2 CF PETG  100   5             6.44 0.999          343 TRUE         
56 SET-2 CF PETG  110   1             6.48 1.000          362 TRUE         
57 SET-2 CF PETG  110   2             6.54 0.999          361 TRUE         
58 SET-2 CF PETG  110   3             7.17 1.000          380 TRUE         
59 SET-2 CF PETG  110   4             6.48 1.000          378 TRUE         
60 SET-2 CF PETG  110   5             6.15 1.000          364 TRUE         
61 SET-2 PETG     80    1             2.10 0.998          358 TRUE         
62 SET-2 PETG     80    2             2.01 0.999          347 TRUE         
63 SET-2 PETG     80    3             2.12 1.000          336 TRUE         
64 SET-2 PETG     80    4             1.65 0.999          337 TRUE         
65 SET-2 PETG     80    5             1.78 0.998          332 TRUE         
66 SET-2 PETG     90    1             2.16 1.000          344 TRUE         
67 SET-2 PETG     90    2             1.95 0.999          331 TRUE         
68 SET-2 PETG     90    3             1.66 0.998          320 TRUE         
69 SET-2 PETG     90    4             1.94 1.000          334 TRUE         
70 SET-2 PETG     90    5             2.20 0.999          346 TRUE         
71 SET-2 PETG     100   1             1.95 0.999          333 TRUE         
72 SET-2 PETG     100   2             2.23 0.998          349 TRUE         
73 SET-2 PETG     100   3             1.87 0.997          340 TRUE         
74 SET-2 PETG     100   4             2.23 0.998          317 TRUE         
75 SET-2 PETG     100   5             1.89 0.998          306 TRUE         
76 SET-2 PETG     110   1             5.14 0.779           88 FALSE        
77 SET-2 PETG     110   2             2.26 1.000          332 TRUE         
78 SET-2 PETG     110   3             2.37 0.998          335 TRUE         
79 SET-2 PETG     110   4             1.84 0.997          329 TRUE         
80 SET-2 PETG     110   5             2.11 0.998          349 TRUE         
> summary_table <- summary_table %>%
  +     mutate(excluded_from_modulus = (r2 < 0.95) | (!covers_window))
> 
  > exclusions <- summary_table %>% filter(excluded_from_modulus)
> print(exclusions)
# A tibble: 1 × 9
set   material flow  sample modulus_GPa    r2 n_fit_points covers_window excluded_from_modulus
<chr> <chr>    <fct> <chr>        <dbl> <dbl>        <int> <lgl>         <lgl>                
  1 SET-2 PETG     110   1             5.14 0.779           88 FALSE         TRUE                 
> representative_modulus <- summary_table %>%
  +     filter(!excluded_from_modulus) %>%
  +     group_by(set, material, flow) %>%
  +     mutate(
    +         mod_IQR = IQR(modulus_GPa),
    +         mod_z = ifelse(mod_IQR == 0, 0, abs(modulus_GPa - median(modulus_GPa)) / mod_IQR)
    +     ) %>%
  +     slice_min(mod_z, n = 1, with_ties = FALSE) %>%
  +     ungroup() %>%
  +     select(set, material, flow, sample, modulus_GPa, r2)
> 
  > print(representative_modulus, n = 20)
# A tibble: 16 × 6
set   material flow  sample modulus_GPa    r2
<chr> <chr>    <fct> <chr>        <dbl> <dbl>
  1 SET-1 CF PETG  80    5             5.01 1.000
2 SET-1 CF PETG  90    1             5.01 1.000
3 SET-1 CF PETG  100   1             5.11 1.000
4 SET-1 CF PETG  110   3             4.95 0.999
5 SET-1 PETG     80    2             2.01 0.999
6 SET-1 PETG     90    1             2.08 1.000
7 SET-1 PETG     100   1             2.17 1.000
8 SET-1 PETG     110   3             2.17 1.000
9 SET-2 CF PETG  80    5             5.84 0.999
10 SET-2 CF PETG  90    4             6.33 1.000
11 SET-2 CF PETG  100   4             6.38 0.998
12 SET-2 CF PETG  110   1             6.48 1.000
13 SET-2 PETG     80    2             2.01 0.999
14 SET-2 PETG     90    2             1.95 0.999
15 SET-2 PETG     100   1             1.95 0.999
16 SET-2 PETG     110   2             2.26 1.000
> write_csv(representative_modulus, file.path(output_dir, "representative_modulus_picks.csv"))

> verify_modulus_individual <- function(set_name, mat, fl) {
  +     group_data <- all_data %>%
    +         filter(set == set_name, material == mat, flow == fl) %>%
    +         group_by(sample) %>%
    +         arrange(Strain, .by_group = TRUE) %>%
    +         mutate(StressSmoothHeavy = rollmean(Stress, 11, fill = NA, align = "center")) %>%
    +         ungroup()
  +     
    +     rep_sample <- representative_modulus %>%
      +         filter(set == set_name, material == mat, flow == fl) %>%
      +         pull(sample)
    +     
      +     group_data <- group_data %>%
        +         mutate(is_rep = if_else(sample == rep_sample, "Representative", "Other"))
      +     
        +     p <- ggplot(group_data, aes(x = Strain*100, y = StressSmoothHeavy, group = sample,
                                          +                                 color = is_rep, linewidth = is_rep)) +
          +         geom_line() +
          +         scale_color_manual(values = c("Representative" = "red", "Other" = "grey60")) +
          +         scale_linewidth_manual(values = c("Representative" = 1.2, "Other" = 0.6)) +
          +         labs(title = paste0(set_name, " — ", mat, " — ", fl, "% flow — Selection audit"),
                         +              x = "Strain (%)", y = "Stress (MPa)", color = NULL, linewidth = NULL) +
          +         theme_minimal()
        +     
          +     fname <- paste0("audit_", set_name, "_", gsub(" ", "_", mat), "_", fl, "pct.png")
          +     ggsave(file.path(output_dir, fname), p, width = 7, height = 5, dpi = 150)
          +     p
          + }
> 
  > exists("verify_modulus_individual")
[1] TRUE
> p_test <- verify_modulus_individual("SET-1", "CF PETG", "80")
Warning message:
  Removed 50 rows containing missing values or values outside the scale range (`geom_line()`). 
> p_test
Warning message:
  Removed 50 rows containing missing values or values outside the scale range (`geom_line()`). 

> conditions <- expand_grid(set = c("SET-1", "SET-2"), material = c("CF PETG", "PETG"), flow = c("80","90","100","110"))
> nrow(conditions)
[1] 16
> for (i in seq_len(nrow(conditions))) {
  +     verify_modulus_individual(conditions$set[i], conditions$material[i], conditions$flow[i])
  + }
There were 16 warnings (use warnings() to see them)

> warnings()
Warning messages:
  1: Removed 50 rows containing missing values or values outside the scale range (`geom_line()`).
2: Removed 50 rows containing missing values or values outside the scale range (`geom_line()`).
3: Removed 50 rows containing missing values or values outside the scale range (`geom_line()`).
4: Removed 50 rows containing missing values or values outside the scale range (`geom_line()`).
5: Removed 50 rows containing missing values or values outside the scale range (`geom_line()`).
6: Removed 50 rows containing missing values or values outside the scale range (`geom_line()`).
7: Removed 50 rows containing missing values or values outside the scale range (`geom_line()`).
8: Removed 50 rows containing missing values or values outside the scale range (`geom_line()`).
9: Removed 50 rows containing missing values or values outside the scale range (`geom_line()`).
10: Removed 50 rows containing missing values or values outside the scale range (`geom_line()`).
11: Removed 50 rows containing missing values or values outside the scale range (`geom_line()`).
12: Removed 50 rows containing missing values or values outside the scale range (`geom_line()`).
13: Removed 50 rows containing missing values or values outside the scale range (`geom_line()`).
14: Removed 50 rows containing missing values or values outside the scale range (`geom_line()`).
15: Removed 50 rows containing missing values or values outside the scale range (`geom_line()`).
16: Removed 50 rows containing missing values or values outside the scale range (`geom_line()`).

> list.files(output_dir, pattern = "^audit_")
[1] "audit_SET-1_CF_PETG_100pct.png" "audit_SET-1_CF_PETG_110pct.png" "audit_SET-1_CF_PETG_80pct.png" 
[4] "audit_SET-1_CF_PETG_90pct.png"  "audit_SET-1_PETG_100pct.png"    "audit_SET-1_PETG_110pct.png"   
[7] "audit_SET-1_PETG_80pct.png"     "audit_SET-1_PETG_90pct.png"     "audit_SET-2_CF_PETG_100pct.png"
[10] "audit_SET-2_CF_PETG_110pct.png" "audit_SET-2_CF_PETG_80pct.png"  "audit_SET-2_CF_PETG_90pct.png" 
[13] "audit_SET-2_PETG_100pct.png"    "audit_SET-2_PETG_110pct.png"    "audit_SET-2_PETG_80pct.png"    
[16] "audit_SET-2_PETG_90pct.png"     "audit_SET1_modulus.png"         "audit_SET2_modulus.png"        
> plot_modulus_representative <- function(set_name, mat, fl) {
  +     rep_info <- representative_modulus %>%
    +         filter(set == set_name, material == mat, flow == fl)
  +     
    +     sample_data <- all_data %>%
      +         filter(set == set_name, material == mat, flow == fl, sample == rep_info$sample) %>%
      +         arrange(Strain) %>%
      +         mutate(StressSmooth = rollmean(Stress, 7, fill = NA, align = "center"))
    +     
      +     fit_data <- sample_data %>% filter(Strain >= 0.0005, Strain <= 0.0025)
      +     fit <- lm(Stress ~ Strain, data = fit_data)
      +     fit_line <- tibble(Strain = seq(0.0005, 0.0025, length.out = 100)) %>%
        +         mutate(Stress = predict(fit, newdata = .))
      +     
        +     E_val <- round(rep_info$modulus_GPa, 2)
        +     r2_val <- round(rep_info$r2, 3)
        +     
          +     p <- ggplot(sample_data, aes(x = Strain*100, y = StressSmooth)) +
            +         annotate("rect", xmin = 0.05, xmax = 0.25, ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "orange") +
            +         geom_line(aes(color = "Measured stress-strain curve"), linewidth = 0.9) +
            +         geom_line(data = fit_line, aes(x = Strain*100, y = Stress, color = "Linear fit (modulus)"), linewidth = 1) +
            +         annotate("text", x = 0.28, y = max(sample_data$Stress, na.rm = TRUE) * 0.15,
                               +                  label = paste0("E = ", E_val, " GPa  (R² = ", r2_val, ")"),
                               +                  color = "red", size = 3.5, hjust = 0) +
            +         scale_color_manual(name = NULL,
                                         +                            values = c("Measured stress-strain curve" = "steelblue",
                                                                                 +                                       "Linear fit (modulus)" = "red")) +
            +         labs(x = "Strain (%)", y = "Stress (MPa)",
                           +              title = paste0(set_name, " — ", mat, " — ", fl, "% flow — Representative Curve (Sample ", rep_info$sample, ")"),
                           +              subtitle = "Shaded region: ISO 527 strain interval (0.05-0.25%) used for modulus determination") +
            +         theme_minimal() +
            +         theme(legend.position = "bottom")
          +     
            +     fname <- paste0("representative_", set_name, "_", gsub(" ", "_", mat), "_", fl, "pct.png")
            +     ggsave(file.path(output_dir, fname), p, width = 7.5, height = 5.5, dpi = 150)
            +     p
            + }
> 
  > exists("plot_modulus_representative")
[1] TRUE
> p_test2 <- plot_modulus_representative("SET-1", "CF PETG", "90")
Warning message:
  Removed 6 rows containing missing values or values outside the scale range (`geom_line()`). 
> p_test2
Warning message:
  Removed 6 rows containing missing values or values outside the scale range (`geom_line()`). 

> plot_modulus_representative <- function(set_name, mat, fl) {
  +     rep_info <- representative_modulus %>%
    +         filter(set == set_name, material == mat, flow == fl)
  +     
    +     sample_data <- all_data %>%
      +         filter(set == set_name, material == mat, flow == fl, sample == rep_info$sample) %>%
      +         arrange(Strain) %>%
      +         mutate(StressSmooth = rollmean(Stress, 7, fill = NA, align = "center"))
    +     
      +     fit_data <- sample_data %>% filter(Strain >= 0.0005, Strain <= 0.0025)
      +     fit <- lm(Stress ~ Strain, data = fit_data)
      +     fit_line <- tibble(Strain = seq(0.0005, 0.0025, length.out = 100)) %>%
        +         mutate(Stress = predict(fit, newdata = .))
      +     
        +     E_val <- round(rep_info$modulus_GPa, 2)
        +     r2_val <- round(rep_info$r2, 3)
        +     
          +     p <- ggplot(sample_data, aes(x = Strain*100, y = StressSmooth)) +
            +         annotate("rect", xmin = 0.05, xmax = 0.25, ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "orange") +
            +         annotate("text", x = 0.15, y = max(sample_data$Stress, na.rm = TRUE) * 0.95,
                               +                  label = "Modulus fitting region", color = "darkorange4", size = 3, hjust = 0.5) +
            +         geom_line(aes(color = "Measured stress-strain curve"), linewidth = 0.9) +
            +         geom_line(data = fit_line, aes(x = Strain*100, y = Stress, color = "Linear fit (modulus)"), linewidth = 1) +
            +         annotate("text", x = 0.28, y = max(sample_data$Stress, na.rm = TRUE) * 0.15,
                               +                  label = paste0("E = ", E_val, " GPa  (R² = ", r2_val, ")"),
                               +                  color = "red", size = 3.5, hjust = 0) +
            +         scale_color_manual(name = NULL,
                                         +                            values = c("Measured stress-strain curve" = "steelblue",
                                                                                 +                                       "Linear fit (modulus)" = "red")) +
            +         labs(x = "Strain (%)", y = "Stress (MPa)",
                           +              title = paste0(set_name, " — ", mat, " — ", fl, "% flow — Representative Curve (Sample ", rep_info$sample, ")")) +
            +         theme_minimal() +
            +         theme(legend.position = "bottom")
          +     
            +     fname <- paste0("representative_", set_name, "_", gsub(" ", "_", mat), "_", fl, "pct.png")
            +     ggsave(file.path(output_dir, fname), p, width = 7.5, height = 5, dpi = 150)
            +     p
            + }
> 
  > exists("plot_modulus_representative")
[1] TRUE
> p_test2 <- plot_modulus_representative("SET-1", "CF PETG", "90")
Warning message:
  Removed 6 rows containing missing values or values outside the scale range (`geom_line()`). 
> p_test2
Warning message:
  Removed 6 rows containing missing values or values outside the scale range (`geom_line()`). 

> representative_modulus %>% filter(set == "SET-1", material == "CF PETG", flow == "90") %>% pull(r2)
[1] 0.9995299
> r2_val <- round(rep_info$r2, 4)   # was 3, now 4 decimal places
Error: object 'rep_info' not found

> modulus_stats <- summary_table %>%
  +     filter(!excluded_from_modulus) %>%
  +     group_by(set, material, flow) %>%
  +     summarise(mean_modulus_GPa = mean(modulus_GPa),
                  +               sd_modulus_GPa = sd(modulus_GPa),
                  +               n = n(), .groups = "drop")
> 
  > print(modulus_stats, n = 20)
# A tibble: 16 × 6
set   material flow  mean_modulus_GPa sd_modulus_GPa     n
<chr> <chr>    <fct>            <dbl>          <dbl> <int>
  1 SET-1 CF PETG  80                4.90         0.393      5
2 SET-1 CF PETG  90                5.08         0.387      5
3 SET-1 CF PETG  100               5.18         0.114      5
4 SET-1 CF PETG  110               4.96         0.345      5
5 SET-1 PETG     80                2.07         0.228      5
6 SET-1 PETG     90                2.08         0.150      5
7 SET-1 PETG     100               2.20         0.0859     5
8 SET-1 PETG     110               2.23         0.221      5
9 SET-2 CF PETG  80                5.83         0.335      5
10 SET-2 CF PETG  90                6.35         0.253      5
11 SET-2 CF PETG  100               6.29         0.336      5
12 SET-2 CF PETG  110               6.56         0.372      5
13 SET-2 PETG     80                1.93         0.207      5
14 SET-2 PETG     90                1.98         0.215      5
15 SET-2 PETG     100               2.03         0.181      5
16 SET-2 PETG     110               2.15         0.227      4
> p_modulus_vs_flow <- ggplot(modulus_stats, aes(x = flow, y = mean_modulus_GPa, color = material, group = interaction(material, set))) +
  +     geom_line(aes(linetype = set), linewidth = 0.9) +
  +     geom_point(size = 2.5) +
  +     geom_errorbar(aes(ymin = mean_modulus_GPa - sd_modulus_GPa, ymax = mean_modulus_GPa + sd_modulus_GPa), width = 0.15) +
  +     labs(x = "Flow Rate (%)", y = "Young's Modulus (GPa)",
             +          title = "Young's Modulus vs Flow Rate — SET-1 vs SET-2",
             +          linetype = "Set", color = "Material") +
  +     theme_minimal()
> 
  > p_modulus_vs_flow
> 
  > ggsave(file.path(output_dir, "modulus_vs_flow_both_sets.png"), p_modulus_vs_flow, width = 9, height = 6, dpi = 300)
> path_break_set1 <- "C:/Users/vipul/Desktop/MSc Project/New Folder/Testing Data/Vipul Dude/Pull to Break/SET-1"
> path_break_set2 <- "C:/Users/vipul/Desktop/MSc Project/New Folder/Testing Data/Vipul Dude/Pull to Break/SET-2"
> output_dir_break <- "C:/Users/vipul/Desktop/MSc Project/New Folder/Testing Data/Vipul Dude/Pull to Break/Graphs"
> dir.create(output_dir_break, showWarnings = FALSE, recursive = TRUE)
> 
  > files_break_set2 <- list.files(path_break_set2, pattern = "\\.txt$", recursive = TRUE, full.names = TRUE)
> length(files_break_set2)
[1] 40
> compute_strain0 <- function(f) {
  +     parts <- str_split(f, "/")[[1]]
  +     material <- parts[length(parts) - 1]
  +     flow <- str_extract(basename(f), "\\d+(?=%)")
  +     sample <- str_extract(basename(f), "(?<=_)\\d+(?=\\.txt$)")
  +     
    +     df <- read_tsv(f, skip = 1, locale = locale(encoding = "UTF-16LE"),
                         +                    col_names = c("Time","Load","Ext","Stress","ExtPreload","Strain","PctStrain","GaugeLen"),
                         +                    show_col_types = FALSE) %>%
      +         drop_na(Stress, Strain, Load) %>% filter(Strain >= cummax(Strain))
    +     smax <- max(df$Stress)
    +     df <- df %>% filter(Stress > 0.02 * smax)
    +     win <- df %>% filter(between(Strain, 0.0005, 0.0025))
    +     
      +     if (nrow(win) < 5) {
        +         return(tibble(file = basename(f), material = material, flow = flow, sample = sample, strain0 = NA_real_))
        +     }
    +     fit <- lm(Stress ~ Strain, data = win)
    +     slope <- coef(fit)[2]
    +     tibble(file = basename(f), material = material, flow = flow, sample = sample,
                 +            strain0 = if (is.na(slope) || slope <= 0) NA_real_ else -coef(fit)[1]/slope)
    + }
> 
  > exists("compute_strain0")
[1] TRUE
> strain0_table_set2 <- map_dfr(files_break_set2, compute_strain0) %>%
  +     group_by(material, flow) %>%
  +     mutate(strain0_final = if_else(is.na(strain0), mean(strain0, na.rm = TRUE), strain0)) %>%
  +     ungroup()
>                                                                                                         
  > print(strain0_table_set2, n = 40)
# A tibble: 40 × 6
file                material flow  sample     strain0 strain0_final
<chr>               <chr>    <chr> <chr>        <dbl>         <dbl>
  1 S2 CFPETG100%_1.txt CF PETG  100   1      -0.0000413    -0.0000413 
2 S2 CFPETG100%_2.txt CF PETG  100   2       0.0000160     0.0000160 
3 S2 CFPETG100%_3.txt CF PETG  100   3      -0.0000837    -0.0000837 
4 S2 CFPETG100%_4.txt CF PETG  100   4      -0.0000595    -0.0000595 
5 S2 CFPETG100%_5.txt CF PETG  100   5      -0.0000975    -0.0000975 
6 S2 CFPETG110%_1.txt CF PETG  110   1      -0.0000288    -0.0000288 
7 S2 CFPETG110%_2.txt CF PETG  110   2      -0.0000446    -0.0000446 
8 S2 CFPETG110%_3.txt CF PETG  110   3      -0.0000624    -0.0000624 
9 S2 CFPETG110%_4.txt CF PETG  110   4      -0.0000487    -0.0000487 
10 S2 CFPETG110%_5.txt CF PETG  110   5      -0.0000946    -0.0000946 
11 S2 CFPETG80%_1.txt  CF PETG  80    1      -0.0000717    -0.0000717 
12 S2 CFPETG80%_2.txt  CF PETG  80    2      -0.00000842   -0.00000842
13 S2 CFPETG80%_3.txt  CF PETG  80    3      -0.0000881    -0.0000881 
14 S2 CFPETG80%_4.txt  CF PETG  80    4      -0.0000602    -0.0000602 
15 S2 CFPETG80%_5.txt  CF PETG  80    5      -0.0000565    -0.0000565 
16 S2 CFPETG90%_1.txt  CF PETG  90    1      -0.0000639    -0.0000639 
17 S2 CFPETG90%_2.txt  CF PETG  90    2      -0.0000512    -0.0000512 
18 S2 CFPETG90%_3.txt  CF PETG  90    3      -0.0000467    -0.0000467 
19 S2 CFPETG90%_4.txt  CF PETG  90    4      -0.0000279    -0.0000279 
20 S2 CFPETG90%_5.txt  CF PETG  90    5      -0.0000570    -0.0000570 
21 S2 PETG100%_1.txt   PETG     100   1       0.0000245     0.0000245 
22 S2 PETG100%_2.txt   PETG     100   2      -0.0000557    -0.0000557 
23 S2 PETG100%_3.txt   PETG     100   3       0.0000903     0.0000903 
24 S2 PETG100%_4.txt   PETG     100   4       0.000110      0.000110  
25 S2 PETG100%_5.txt   PETG     100   5       0.000212      0.000212  
26 S2 PETG110%_1.txt   PETG     110   1      NA             0.0000138 
27 S2 PETG110%_2.txt   PETG     110   2      -0.00000498   -0.00000498
28 S2 PETG110%_3.txt   PETG     110   3      NA             0.0000138 
29 S2 PETG110%_4.txt   PETG     110   4       0.0000326     0.0000326 
30 S2 PETG110%_5.txt   PETG     110   5      NA             0.0000138 
31 S2 PETG80%_1.txt    PETG     80    1      NA            -0.0000649 
32 S2 PETG80%_2.txt    PETG     80    2      -0.0000827    -0.0000827 
33 S2 PETG80%_3.txt    PETG     80    3      -0.000208     -0.000208  
34 S2 PETG80%_4.txt    PETG     80    4       0.00000931    0.00000931
35 S2 PETG80%_5.txt    PETG     80    5       0.0000217     0.0000217 
36 S2 PETG90%_1.txt    PETG     90    1       0.0000344     0.0000344 
37 S2 PETG90%_2.txt    PETG     90    2       0.000178      0.000178  
38 S2 PETG90%_3.txt    PETG     90    3       0.0000177     0.0000177 
39 S2 PETG90%_4.txt    PETG     90    4       0.0000307     0.0000307 
40 S2 PETG90%_5.txt    PETG     90    5      NA             0.0000652 
> detect_break <- function(df) {
  +   load_smooth <- rollmean(df$Load, k = 9, fill = NA, align = "center")
  +   load_smooth[is.na(load_smooth)] <- df$Load[is.na(load_smooth)]
  + 
    +   peak_load <- max(load_smooth, na.rm = TRUE)
    +   threshold <- 0.2 * peak_load
    + 
      +   above <- load_smooth > threshold
      +   last_above <- max(which(above))
      + 
        +   min(last_above + 5, nrow(df))
      + }
> 
  > read_clean_break_set2 <- function(f) {
    +   parts <- str_split(f, "/")[[1]]
    +   material <- parts[length(parts) - 1]
    +   flow   <- str_extract(basename(f), "\\d+(?=%)")
    +   sample <- str_extract(basename(f), "(?<=_)\\d+(?=\\.txt$)")
    +   fname  <- basename(f)
    + 
      +   df <- read_tsv(f, skip = 1, locale = locale(encoding = "UTF-16LE"),
                         +                   col_names = c("Time","Load","Ext","Stress","ExtPreload","Strain","PctStrain","GaugeLen"),
                         +                   show_col_types = FALSE) %>%
        +     drop_na(Stress, Strain, Load) %>%
        +     filter(Strain >= cummax(Strain))
      + 
        +   smax_initial <- max(df$Stress)
        +   df <- df %>% filter(Stress > 0.02 * smax_initial)
        + 
          +   strain0 <- strain0_table_set2 %>% filter(file == fname) %>% pull(strain0_final)
          +   df <- df %>% mutate(Strain = Strain - strain0) %>% filter(Strain >= 0)
          + 
            +   break_idx <- detect_break(df)
            +   df <- df[1:break_idx, ]
            + 
              +   df %>%
              +     mutate(StressSmooth = rollmean(Stress, 5, fill = NA, align = "center"),
                           +            material = material, flow = flow, sample = sample, file = fname, set = "SET-2")
            + }
> 
  > exists("detect_break")
[1] TRUE
> exists("read_clean_break_set2")
[1] TRUE
> all_break_data_set2 <- map_dfr(files_break_set2, read_clean_break_set2)
> nrow(all_break_data_set2)                                                                               
[1] 35962
> label_check_break_set2 <- all_break_data_set2 %>%
  +     distinct(material, flow, sample, file) %>%
  +     arrange(material, flow, sample)
> 
  > nrow(label_check_break_set2)
[1] 40
> print(label_check_break_set2, n = 40)
# A tibble: 40 × 4
material flow  sample file               
<chr>    <chr> <chr>  <chr>              
  1 CF PETG  100   1      S2 CFPETG100%_1.txt
2 CF PETG  100   2      S2 CFPETG100%_2.txt
3 CF PETG  100   3      S2 CFPETG100%_3.txt
4 CF PETG  100   4      S2 CFPETG100%_4.txt
5 CF PETG  100   5      S2 CFPETG100%_5.txt
6 CF PETG  110   1      S2 CFPETG110%_1.txt
7 CF PETG  110   2      S2 CFPETG110%_2.txt
8 CF PETG  110   3      S2 CFPETG110%_3.txt
9 CF PETG  110   4      S2 CFPETG110%_4.txt
10 CF PETG  110   5      S2 CFPETG110%_5.txt
11 CF PETG  80    1      S2 CFPETG80%_1.txt 
12 CF PETG  80    2      S2 CFPETG80%_2.txt 
13 CF PETG  80    3      S2 CFPETG80%_3.txt 
14 CF PETG  80    4      S2 CFPETG80%_4.txt 
15 CF PETG  80    5      S2 CFPETG80%_5.txt 
16 CF PETG  90    1      S2 CFPETG90%_1.txt 
17 CF PETG  90    2      S2 CFPETG90%_2.txt 
18 CF PETG  90    3      S2 CFPETG90%_3.txt 
19 CF PETG  90    4      S2 CFPETG90%_4.txt 
20 CF PETG  90    5      S2 CFPETG90%_5.txt 
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
> break_summary_set2 <- all_break_data_set2 %>%
  +     mutate(flow = factor(flow, levels = c("80", "90", "100", "110"))) %>%
  +     group_by(material, flow, sample) %>%
  +     summarise(
    +         UTS_MPa = max(Stress, na.rm = TRUE),
    +         elongation_at_break_pct = max(Strain, na.rm = TRUE) * 100,
    +         .groups = "drop"
    +     )
> 
  > print(break_summary_set2, n = 40)
# A tibble: 40 × 5
material flow  sample UTS_MPa elongation_at_break_pct
<chr>    <fct> <chr>    <dbl>                   <dbl>
  1 CF PETG  80    1         62.7                    2.62
2 CF PETG  80    2         61.2                    2.45
3 CF PETG  80    3         65.4                    3.13
4 CF PETG  80    4         65.0                    2.71
5 CF PETG  80    5         58.2                    2.36
6 CF PETG  90    1         70.1                    2.70
7 CF PETG  90    2         64.5                    3.18
8 CF PETG  90    3         65.2                    2.79
9 CF PETG  90    4         81.4                    3.10
10 CF PETG  90    5         74.0                    3.01
11 CF PETG  100   1         74.7                    2.71
12 CF PETG  100   2         77.5                    2.61
13 CF PETG  100   3         78.5                    3.49
14 CF PETG  100   4         80.9                    3.10
15 CF PETG  100   5         69.1                    2.75
16 CF PETG  110   1         89.1                    4.31
17 CF PETG  110   2         86.0                    3.24
18 CF PETG  110   3         80.5                    3.07
19 CF PETG  110   4         85.4                    3.33
20 CF PETG  110   5         83.3                    3.44
21 PETG     80    1         50.4                   59.0 
22 PETG     80    2         51.2                   28.2 
23 PETG     80    3         50.9                   16.5 
24 PETG     80    4         55.2                   15.8 
25 PETG     80    5         57.8                    4.59
26 PETG     90    1         51.5                   12.0 
27 PETG     90    2         53.7                   10.7 
28 PETG     90    3         56.7                   20.2 
29 PETG     90    4         57.3                   23.2 
30 PETG     90    5         51.7                   42.8 
31 PETG     100   1         49.2                   19.1 
32 PETG     100   2         51.7                    3.92
33 PETG     100   3         59.5                   19.1 
34 PETG     100   4         57.8                   14.4 
35 PETG     100   5         60.4                   17.0 
36 PETG     110   1         54.4                  126.  
37 PETG     110   2         56.1                   15.3 
38 PETG     110   3         60.9                   54.9 
39 PETG     110   4         55.6                   21.3 
40 PETG     110   5         60.9                  164.  
> break_summary_set2 <- break_summary_set2 %>%
  +     mutate(failure_mode = if_else(elongation_at_break_pct > 20, "Extensive drawing", "Limited/brittle"))
> 
  > break_summary_set2 %>% count(material, flow, failure_mode)
# A tibble: 11 × 4
material flow  failure_mode          n
<chr>    <fct> <chr>             <int>
  1 CF PETG  80    Limited/brittle       5
2 CF PETG  90    Limited/brittle       5
3 CF PETG  100   Limited/brittle       5
4 CF PETG  110   Limited/brittle       5
5 PETG     80    Extensive drawing     2
6 PETG     80    Limited/brittle       3
7 PETG     90    Extensive drawing     3
8 PETG     90    Limited/brittle       2
9 PETG     100   Limited/brittle       5
10 PETG     110   Extensive drawing     4
11 PETG     110   Limited/brittle       1
> composite_selection_set2 <- break_summary_set2 %>%
  +   group_by(material, flow) %>%
  +   add_count(failure_mode, name = "mode_count") %>%
  +   filter(mode_count == max(mode_count)) %>%
  +   group_by(material, flow) %>%
  +   mutate(
    +     UTS_IQR = IQR(UTS_MPa),
    +     elong_IQR = IQR(elongation_at_break_pct),
    +     UTS_z = ifelse(UTS_IQR == 0, 0, abs(UTS_MPa - median(UTS_MPa)) / UTS_IQR),
    +     elong_z = ifelse(elong_IQR == 0, 0, abs(elongation_at_break_pct - median(elongation_at_break_pct)) / elong_IQR),
    +     composite_score = UTS_z + elong_z
    +   ) %>%
  +   arrange(material, flow, composite_score) %>%
  +   select(material, flow, sample, UTS_MPa, elongation_at_break_pct, failure_mode, mode_count, composite_score)
> 
  > print(composite_selection_set2, n = 100)
# A tibble: 35 × 8
# Groups:   material, flow [8]
material flow  sample UTS_MPa elongation_at_break_pct failure_mode      mode_count composite_score
<chr>    <fct> <chr>    <dbl>                   <dbl> <chr>                  <int>           <dbl>
  1 CF PETG  80    1         62.7                    2.62 Limited/brittle            5           0    
2 CF PETG  80    4         65.0                    2.71 Limited/brittle            5           0.945
3 CF PETG  80    2         61.2                    2.45 Limited/brittle            5           1.06 
4 CF PETG  80    5         58.2                    2.36 Limited/brittle            5           2.20 
5 CF PETG  80    3         65.4                    3.13 Limited/brittle            5           2.65 
6 CF PETG  90    5         74.0                    3.01 Limited/brittle            5           0.446
7 CF PETG  90    1         70.1                    2.70 Limited/brittle            5           0.999
8 CF PETG  90    2         64.5                    3.18 Limited/brittle            5           1.19 
9 CF PETG  90    3         65.2                    2.79 Limited/brittle            5           1.26 
10 CF PETG  90    4         81.4                    3.10 Limited/brittle            5           1.59 
11 CF PETG  100   2         77.5                    2.61 Limited/brittle            5           0.353
12 CF PETG  100   1         74.7                    2.71 Limited/brittle            5           0.871
13 CF PETG  100   4         80.9                    3.10 Limited/brittle            5           1.75 
14 CF PETG  100   3         78.5                    3.49 Limited/brittle            5           2.09 
15 CF PETG  100   5         69.1                    2.75 Limited/brittle            5           2.22 
16 CF PETG  110   4         85.4                    3.33 Limited/brittle            5           0    
17 CF PETG  110   2         86.0                    3.24 Limited/brittle            5           0.699
18 CF PETG  110   5         83.3                    3.44 Limited/brittle            5           1.30 
19 CF PETG  110   3         80.5                    3.07 Limited/brittle            5           3.10 
20 CF PETG  110   1         89.1                    4.31 Limited/brittle            5           6.25 
21 PETG     80    4         55.2                   15.8  Limited/brittle            3           0    
22 PETG     80    3         50.9                   16.5  Limited/brittle            3           1.35 
23 PETG     80    5         57.8                    4.59 Limited/brittle            3           2.65 
24 PETG     90    4         57.3                   23.2  Extensive drawing          3           0.220
25 PETG     90    3         56.7                   20.2  Extensive drawing          3           0.269
26 PETG     90    5         51.7                   42.8  Extensive drawing          3           3.51 
27 PETG     100   5         60.4                   17.0  Limited/brittle            5           0.336
28 PETG     100   4         57.8                   14.4  Limited/brittle            5           0.564
29 PETG     100   3         59.5                   19.1  Limited/brittle            5           0.650
30 PETG     100   1         49.2                   19.1  Limited/brittle            5           1.57 
31 PETG     100   2         51.7                    3.92 Limited/brittle            5           3.59 
32 PETG     110   3         60.9                   54.9  Extensive drawing          4           0.877
33 PETG     110   1         54.4                  126.   Extensive drawing          4           1.09 
34 PETG     110   4         55.6                   21.3  Extensive drawing          4           1.25 
35 PETG     110   5         60.9                  164.   Extensive drawing          4           1.30 
> verify_selection_set2 <- function(mat, fl) {
  +     group_data <- all_break_data_set2 %>%
    +         filter(material == mat, flow == fl) %>%
    +         group_by(sample) %>%
    +         arrange(Strain, .by_group = TRUE) %>%
    +         mutate(StressLightSmooth = zoo::rollmean(Stress, 7, fill = NA, align = "center")) %>%
    +         ungroup()
  +     
    +     rep_sample <- composite_selection_set2 %>%
      +         filter(material == mat, flow == fl) %>%
      +         slice_min(composite_score, n = 1) %>%
      +         pull(sample)
    +     
      +     group_data <- group_data %>%
        +         mutate(is_representative = if_else(sample == rep_sample, "Representative", "Other"))
      +     
        +     p <- ggplot(group_data, aes(x = Strain*100, y = StressLightSmooth,
                                          +                                 group = sample, color = is_representative, linewidth = is_representative)) +
          +         geom_line() +
          +         scale_color_manual(values = c("Representative" = "red", "Other" = "grey60")) +
          +         scale_linewidth_manual(values = c("Representative" = 1.3, "Other" = 0.6)) +
          +         labs(x = "Strain (%)", y = "Stress (MPa)",
                         +              title = paste0("SET-2 — ", mat, " — ", fl, "% flow — All 5 (red = representative)"),
                         +              color = NULL, linewidth = NULL) +
          +         theme_minimal()
        +     
          +     fname <- paste0("audit_SET2_break_", gsub(" ", "_", mat), "_", fl, "pct.png")
          +     ggsave(file.path(output_dir_break, fname), p, width = 7, height = 5, dpi = 150)
          +     p
          + }
> 
  > exists("verify_selection_set2")
[1] TRUE
> p_test3 <- verify_selection_set2("PETG", "90")
Warning message:
  Removed 30 rows containing missing values or values outside the scale range (`geom_line()`). 
> p_test3
Warning message:
  Removed 30 rows containing missing values or values outside the scale range (`geom_line()`). 

> verify_selection_set2("CF PETG", "80")
Warning messages:
  1: Removed 30 rows containing missing values or values outside the scale range (`geom_line()`). 
2: Removed 30 rows containing missing values or values outside the scale range (`geom_line()`). 
> verify_selection_set2("CF PETG", "90")
Warning messages:
  1: Removed 30 rows containing missing values or values outside the scale range (`geom_line()`). 
2: Removed 30 rows containing missing values or values outside the scale range (`geom_line()`). 
> verify_selection_set2("CF PETG", "100")
Warning messages:
  1: Removed 30 rows containing missing values or values outside the scale range (`geom_line()`). 
2: Removed 30 rows containing missing values or values outside the scale range (`geom_line()`). 
> verify_selection_set2("CF PETG", "110")
Warning messages:
  1: Removed 30 rows containing missing values or values outside the scale range (`geom_line()`). 
2: Removed 30 rows containing missing values or values outside the scale range (`geom_line()`). 
> verify_selection_set2("PETG", "80")
Warning messages:
  1: Removed 30 rows containing missing values or values outside the scale range (`geom_line()`). 
2: Removed 30 rows containing missing values or values outside the scale range (`geom_line()`). 
> verify_selection_set2("PETG", "100")
Warning messages:
  1: Removed 30 rows containing missing values or values outside the scale range (`geom_line()`). 
2: Removed 30 rows containing missing values or values outside the scale range (`geom_line()`). 
> verify_selection_set2("PETG", "110")
Warning messages:
  1: Removed 30 rows containing missing values or values outside the scale range (`geom_line()`). 
2: Removed 30 rows containing missing values or values outside the scale range (`geom_line()`). 

> representative_picks_set2 <- composite_selection_set2 %>%
  +     group_by(material, flow) %>%
  +     slice_min(composite_score, n = 1, with_ties = FALSE) %>%
  +     ungroup() %>%
  +     select(material, flow, sample)
> 
  > print(representative_picks_set2)
# A tibble: 8 × 3
material flow  sample
<chr>    <fct> <chr> 
  1 CF PETG  80    1     
2 CF PETG  90    5     
3 CF PETG  100   2     
4 CF PETG  110   4     
5 PETG     80    4     
6 PETG     90    4     
7 PETG     100   5     
8 PETG     110   3     
> representative_curves_set2 <- all_break_data_set2 %>%
  +     inner_join(representative_picks_set2, by = c("material", "flow", "sample")) %>%
  +     mutate(flow = factor(flow, levels = c("80", "90", "100", "110"))) %>%
  +     group_by(material, flow, sample) %>%
  +     arrange(Strain, .by_group = TRUE) %>%
  +     mutate(StressLightSmooth = zoo::rollmean(Stress, 7, fill = NA, align = "center")) %>%
  +     ungroup()
> 
  > p_cf_set2 <- representative_curves_set2 %>%
  +     filter(material == "CF PETG") %>%
  +     ggplot(aes(x = Strain*100, y = StressLightSmooth, color = flow)) +
  +     geom_line(linewidth = 0.9) +
  +     labs(x = "Strain (%)", y = "Stress (MPa)",
             +          title = "CF PETG — Representative Stress-Strain Curves by Flow Rate — SET-2") +
  +     theme_minimal()
> 
  > p_cf_set2
Warning message:
  Removed 24 rows containing missing values or values outside the scale range (`geom_line()`). 

> representative_curves_set2 <- all_break_data_set2 %>%
  +     inner_join(representative_picks_set2, by = c("material", "flow", "sample")) %>%
  +     mutate(flow = factor(flow, levels = c("80", "90", "100", "110"))) %>%
  +     group_by(material, flow, sample) %>%
  +     arrange(Strain, .by_group = TRUE) %>%
  +     mutate(StressLightSmooth = zoo::rollmean(Stress, 7, fill = NA, align = "center")) %>%
  +     ungroup()
> 
  > p_cf_set2 <- representative_curves_set2 %>%
  +     filter(material == "CF PETG") %>%
  +     ggplot(aes(x = Strain*100, y = StressLightSmooth, color = flow)) +
  +     geom_line(linewidth = 0.9) +
  +     labs(x = "Strain (%)", y = "Stress (MPa)",
             +          title = "CF PETG — Representative Stress-Strain Curves by Flow Rate — SET-2") +
  +     theme_minimal()
> 
  > p_cf_set2
Warning message:
  Removed 24 rows containing missing values or values outside the scale range (`geom_line()`). 

> representative_curves_set2_petg <- representative_curves_set2 %>%
  +     filter(material == "PETG") %>%
  +     left_join(break_summary_set2 %>% select(material, flow, sample, failure_mode),
                  +               by = c("material", "flow", "sample")) %>%
  +     mutate(mode_group = factor(
    +         if_else(failure_mode == "Limited/brittle", "Limited/brittle", "Extensive drawing"),
    +         levels = c("Limited/brittle", "Extensive drawing")
    +     ))
> 
  > p_petg_set2 <- representative_curves_set2_petg %>%
  +     ggplot(aes(x = Strain*100, y = StressLightSmooth, color = flow)) +
  +     geom_line(linewidth = 0.9) +
  +     facet_wrap(~mode_group, scales = "free_x") +
  +     labs(x = "Strain (%)", y = "Stress (MPa)",
             +          title = "PETG — Representative Stress-Strain Curves by Flow Rate — SET-2",
             +          subtitle = "Selected from individual specimens; split according to observed failure behaviour") +
  +     theme_minimal()
> 
  > p_petg_set2
Warning message:
  Removed 24 rows containing missing values or values outside the scale range (`geom_line()`). 

> ggsave(file.path(output_dir_break, "representative_CF_PETG_SET2_all_flows.png"), p_cf_set2, width = 9, height = 6, dpi = 300)
Warning message:
  Removed 24 rows containing missing values or values outside the scale range (`geom_line()`). 
> ggsave(file.path(output_dir_break, "representative_PETG_SET2_split_failure_mode.png"), p_petg_set2, width = 10, height = 6, dpi = 300)
Warning message:
  Removed 24 rows containing missing values or values outside the scale range (`geom_line()`). 

> toughness_summary_set2 <- all_break_data_set2 %>%
  +     group_by(material, flow, sample) %>%
  +     arrange(Strain, .by_group = TRUE) %>%
  +     summarise(toughness_MJ_per_m3 = sum(diff(Strain) * (head(Stress, -1) + tail(Stress, -1)) / 2),
                  +               .groups = "drop")
> 
  > print(toughness_summary_set2, n = 40)
# A tibble: 40 × 4
material flow  sample toughness_MJ_per_m3
<chr>    <chr> <chr>                <dbl>
  1 CF PETG  100   1                    1.25 
2 CF PETG  100   2                    1.21 
3 CF PETG  100   3                    1.77 
4 CF PETG  100   4                    1.55 
5 CF PETG  100   5                    1.18 
6 CF PETG  110   1                    1.99 
7 CF PETG  110   2                    1.74 
8 CF PETG  110   3                    1.58 
9 CF PETG  110   4                    1.84 
10 CF PETG  110   5                    1.81 
11 CF PETG  80    1                    1.01 
12 CF PETG  80    2                    0.889
13 CF PETG  80    3                    1.30 
14 CF PETG  80    4                    1.13 
15 CF PETG  80    5                    0.849
16 CF PETG  90    1                    1.18 
17 CF PETG  90    2                    1.28 
18 CF PETG  90    3                    1.19 
19 CF PETG  90    4                    1.56 
20 CF PETG  90    5                    1.39 
21 PETG     100   1                    4.49 
22 PETG     100   2                    1.20 
23 PETG     100   3                    7.39 
24 PETG     100   4                    4.93 
25 PETG     100   5                    5.70 
26 PETG     110   1                   43.7  
27 PETG     110   2                    5.14 
28 PETG     110   3                   22.5  
29 PETG     110   4                    6.99 
30 PETG     110   5                   67.8  
31 PETG     80    1                   15.5  
32 PETG     80    2                    8.69 
33 PETG     80    3                    4.43 
34 PETG     80    4                    5.24 
35 PETG     80    5                    1.61 
36 PETG     90    1                    3.55 
37 PETG     90    2                    3.17 
38 PETG     90    3                    7.20 
39 PETG     90    4                    7.93 
40 PETG     90    5                   14.5  
> set2_property_summary <- break_summary_set2 %>%
  +     left_join(toughness_summary_set2, by = c("material", "flow", "sample")) %>%
  +     group_by(material, flow) %>%
  +     summarise(
    +         mean_UTS = mean(UTS_MPa), sd_UTS = sd(UTS_MPa),
    +         mean_elongation = mean(elongation_at_break_pct), sd_elongation = sd(elongation_at_break_pct),
    +         mean_toughness = mean(toughness_MJ_per_m3), sd_toughness = sd(toughness_MJ_per_m3),
    +         .groups = "drop"
    +     )
> 
  > print(set2_property_summary, n = 20)
# A tibble: 8 × 8
material flow  mean_UTS sd_UTS mean_elongation sd_elongation mean_toughness sd_toughness
<chr>    <chr>    <dbl>  <dbl>           <dbl>         <dbl>          <dbl>        <dbl>
  1 CF PETG  100       76.1   4.52            2.93         0.361           1.39        0.257
2 CF PETG  110       84.9   3.18            3.48         0.486           1.79        0.148
3 CF PETG  80        62.5   2.92            2.66         0.302           1.03        0.182
4 CF PETG  90        71.0   6.93            2.96         0.201           1.32        0.160
5 PETG     100       55.7   4.99           14.7          6.34            4.74        2.27 
6 PETG     110       57.6   3.09           76.3         65.9            29.2        26.5  
7 PETG     80        53.1   3.26           24.8         20.9             7.09        5.33 
8 PETG     90        54.1   2.72           21.8         12.9             7.28        4.58 
> set2_property_summary <- set2_property_summary %>%
  +     mutate(flow = factor(flow, levels = c("80", "90", "100", "110"))) %>%
  +     arrange(material, flow)
> 
  > print(set2_property_summary, n = 20)
# A tibble: 8 × 8
material flow  mean_UTS sd_UTS mean_elongation sd_elongation mean_toughness sd_toughness
<chr>    <fct>    <dbl>  <dbl>           <dbl>         <dbl>          <dbl>        <dbl>
  1 CF PETG  80        62.5   2.92            2.66         0.302           1.03        0.182
2 CF PETG  90        71.0   6.93            2.96         0.201           1.32        0.160
3 CF PETG  100       76.1   4.52            2.93         0.361           1.39        0.257
4 CF PETG  110       84.9   3.18            3.48         0.486           1.79        0.148
5 PETG     80        53.1   3.26           24.8         20.9             7.09        5.33 
6 PETG     90        54.1   2.72           21.8         12.9             7.28        4.58 
7 PETG     100       55.7   4.99           14.7          6.34            4.74        2.27 
8 PETG     110       57.6   3.09           76.3         65.9            29.2        26.5  
> break_summary_combined <- bind_rows(
  +     break_summary %>% left_join(toughness_summary, by = c("material", "flow", "sample")) %>% mutate(set = "SET-1"),
  +     break_summary_set2 %>% left_join(toughness_summary_set2, by = c("material", "flow", "sample")) %>% mutate(set = "SET-2")
  + ) %>%
  +     mutate(flow = factor(flow, levels = c("80", "90", "100", "110")))
Error: object 'break_summary' not found

> path_break_set1 <- "C:/Users/vipul/Desktop/MSc Project/New Folder/Testing Data/Vipul Dude/Pull to Break/SET-1"
> files_break_set1 <- list.files(path_break_set1, pattern = "\\.txt$", recursive = TRUE, full.names = TRUE)
> length(files_break_set1)
[1] 40
> strain0_table_set1 <- map_dfr(files_break_set1, compute_strain0) %>%
  +     group_by(material, flow) %>%
  +     mutate(strain0_final = if_else(is.na(strain0), mean(strain0, na.rm = TRUE), strain0)) %>%
  +     ungroup()
>                                                                                                         
  > print(strain0_table_set1, n = 40)
# A tibble: 40 × 6
file               material flow  sample     strain0 strain0_final
<chr>              <chr>    <chr> <chr>        <dbl>         <dbl>
  1 CF PETG 100%_1.txt CF PETG  100   1       0.0000652     0.0000652 
2 CF PETG 100%_2.txt CF PETG  100   2      -0.0000571    -0.0000571 
3 CF PETG 100%_3.txt CF PETG  100   3      -0.0000846    -0.0000846 
4 CF PETG 100%_4.txt CF PETG  100   4      -0.0000721    -0.0000721 
5 CF PETG 100%_5.txt CF PETG  100   5      -0.0000340    -0.0000340 
6 CF PETG 110%_1.txt CF PETG  110   1      -0.0000777    -0.0000777 
7 CF PETG 110%_2.txt CF PETG  110   2      -0.0000558    -0.0000558 
8 CF PETG 110%_3.txt CF PETG  110   3       0.000108      0.000108  
9 CF PETG 110%_4.txt CF PETG  110   4       0.0000147     0.0000147 
10 CF PETG 110%_5.txt CF PETG  110   5      -0.0000538    -0.0000538 
11 CF PETG 80%_1.txt  CF PETG  80    1      -0.0000514    -0.0000514 
12 CF PETG 80%_2.txt  CF PETG  80    2      -0.0000509    -0.0000509 
13 CF PETG 80%_3.txt  CF PETG  80    3      -0.000107     -0.000107  
14 CF PETG 80%_4.txt  CF PETG  80    4       0.000173      0.000173  
15 CF PETG 80%_5.txt  CF PETG  80    5      -0.0000292    -0.0000292 
16 CF PETG 90%_1.txt  CF PETG  90    1       0.0000645     0.0000645 
17 CF PETG 90%_2.txt  CF PETG  90    2       0.00000689    0.00000689
18 CF PETG 90%_3.txt  CF PETG  90    3      -0.0000208    -0.0000208 
19 CF PETG 90%_4.txt  CF PETG  90    4       0.0000327     0.0000327 
20 CF PETG 90%_5.txt  CF PETG  90    5      -0.0000494    -0.0000494 
21 PETG 100%_1.txt    PETG     100   1       0.000118      0.000118  
22 PETG 100%_2.txt    PETG     100   2      NA             0.0000370 
23 PETG 100%_3.txt    PETG     100   3      NA             0.0000370 
24 PETG 100%_4.txt    PETG     100   4      -0.0000440    -0.0000440 
25 PETG 100%_5.txt    PETG     100   5      NA             0.0000370 
26 PETG 110%_1.txt    PETG     110   1       0.0000648     0.0000648 
27 PETG 110%_2.txt    PETG     110   2      -0.0000566    -0.0000566 
28 PETG 110%_3.txt    PETG     110   3      NA            -0.0000182 
29 PETG 110%_4.txt    PETG     110   4      -0.0000627    -0.0000627 
30 PETG 110%_5.txt    PETG     110   5      NA            -0.0000182 
31 PETG 80%_1.txt     PETG     80    1       0.0000796     0.0000796 
32 PETG 80%_2.txt     PETG     80    2      -0.0000446    -0.0000446 
33 PETG 80%_3.txt     PETG     80    3       0.000178      0.000178  
34 PETG 80%_4.txt     PETG     80    4      -0.0000382    -0.0000382 
35 PETG 80%_5.txt     PETG     80    5      -0.0000518    -0.0000518 
36 PETG 90%_1.txt     PETG     90    1      NA            -0.0000600 
37 PETG 90%_2.txt     PETG     90    2      -0.000108     -0.000108  
38 PETG 90%_3.txt     PETG     90    3      -0.0000497    -0.0000497 
39 PETG 90%_4.txt     PETG     90    4      -0.0000222    -0.0000222 
40 PETG 90%_5.txt     PETG     90    5      NA            -0.0000600 
> read_clean_break_set1 <- function(f) {
  +     parts <- str_split(f, "/")[[1]]
  +     material <- parts[length(parts) - 1]
  +     flow   <- str_extract(basename(f), "\\d+(?=%)")
  +     sample <- str_extract(basename(f), "(?<=_)\\d+(?=\\.txt$)")
  +     fname  <- basename(f)
  +     
    +     df <- read_tsv(f, skip = 1, locale = locale(encoding = "UTF-16LE"),
                         +                    col_names = c("Time","Load","Ext","Stress","ExtPreload","Strain","PctStrain","GaugeLen"),
                         +                    show_col_types = FALSE) %>%
      +         drop_na(Stress, Strain, Load) %>%
      +         filter(Strain >= cummax(Strain))
    +     
      +     smax_initial <- max(df$Stress)
      +     df <- df %>% filter(Stress > 0.02 * smax_initial)
      +     
        +     strain0 <- strain0_table_set1 %>% filter(file == fname) %>% pull(strain0_final)
        +     df <- df %>% mutate(Strain = Strain - strain0) %>% filter(Strain >= 0)
        +     
          +     break_idx <- detect_break(df)
          +     df <- df[1:break_idx, ]
          +     
            +     df %>%
            +         mutate(StressSmooth = rollmean(Stress, 5, fill = NA, align = "center"),
                             +                material = material, flow = flow, sample = sample, file = fname, set = "SET-1")
          + }
> 
  > all_break_data_set1 <- map_dfr(files_break_set1, read_clean_break_set1)
> nrow(all_break_data_set1)                                                                               
[1] 36348
> break_summary <- all_break_data_set1 %>%
  +     mutate(flow = factor(flow, levels = c("80", "90", "100", "110"))) %>%
  +     group_by(material, flow, sample) %>%
  +     summarise(
    +         UTS_MPa = max(Stress, na.rm = TRUE),
    +         elongation_at_break_pct = max(Strain, na.rm = TRUE) * 100,
    +         .groups = "drop"
    +     )
> 
  > print(break_summary, n = 40)
# A tibble: 40 × 5
material flow  sample UTS_MPa elongation_at_break_pct
<chr>    <fct> <chr>    <dbl>                   <dbl>
  1 CF PETG  80    1         61.5                    2.56
2 CF PETG  80    2         71.5                    3.08
3 CF PETG  80    3         65.6                    3.14
4 CF PETG  80    4         65.9                    2.70
5 CF PETG  80    5         69.7                   20.1 
6 CF PETG  90    1         66.3                    2.76
7 CF PETG  90    2         68.3                    3.15
8 CF PETG  90    3         61.5                    2.93
9 CF PETG  90    4         62.9                    2.65
10 CF PETG  90    5         68.5                    3.08
11 CF PETG  100   1         71.9                    2.84
12 CF PETG  100   2         72.8                    2.97
13 CF PETG  100   3         54.0                    2.36
14 CF PETG  100   4         63.4                    2.37
15 CF PETG  100   5         66.4                    2.70
16 CF PETG  110   1         69.7                    2.82
17 CF PETG  110   2         75.4                    3.03
18 CF PETG  110   3         75.6                    3.40
19 CF PETG  110   4         79.6                    3.55
20 CF PETG  110   5         69.7                    2.70
21 PETG     80    1         62.5                    4.58
22 PETG     80    2         59.1                    5.21
23 PETG     80    3         63.9                    4.81
24 PETG     80    4         59.0                    4.44
25 PETG     80    5         61.6                    4.54
26 PETG     90    1         61.0                  157.  
27 PETG     90    2         59.4                    4.37
28 PETG     90    3         60.8                    4.49
29 PETG     90    4         63.4                    4.44
30 PETG     90    5         63.1                  104.  
31 PETG     100   1         64.4                   14.5 
32 PETG     100   2         61.1                   87.4 
33 PETG     100   3         58.0                  168.  
34 PETG     100   4         62.8                   27.4 
35 PETG     100   5         61.0                  166.  
36 PETG     110   1         67.6                   13.0 
37 PETG     110   2         59.0                   11.8 
38 PETG     110   3         59.2                  102.  
39 PETG     110   4         53.5                   25.5 
40 PETG     110   5         66.1                  130.  
> toughness_summary <- all_break_data_set1 %>%
  +     group_by(material, flow, sample) %>%
  +     arrange(Strain, .by_group = TRUE) %>%
  +     summarise(toughness_MJ_per_m3 = sum(diff(Strain) * (head(Stress, -1) + tail(Stress, -1)) / 2),
                  +               .groups = "drop")
> 
  > print(toughness_summary, n = 40)
# A tibble: 40 × 4
material flow  sample toughness_MJ_per_m3
<chr>    <chr> <chr>                <dbl>
  1 CF PETG  100   1                    1.25 
2 CF PETG  100   2                    1.34 
3 CF PETG  100   3                    0.773
4 CF PETG  100   4                    0.876
5 CF PETG  100   5                    1.08 
6 CF PETG  110   1                    1.15 
7 CF PETG  110   2                    1.40 
8 CF PETG  110   3                    1.67 
9 CF PETG  110   4                    1.78 
10 CF PETG  110   5                    1.11 
11 CF PETG  80    1                    0.906
12 CF PETG  80    2                    1.35 
13 CF PETG  80    3                    1.28 
14 CF PETG  80    4                    1.07 
15 CF PETG  80    5                    4.52 
16 CF PETG  90    1                    1.12 
17 CF PETG  90    2                    1.34 
18 CF PETG  90    3                    1.12 
19 CF PETG  90    4                    1.00 
20 CF PETG  90    5                    1.32 
21 PETG     100   1                    5.40 
22 PETG     100   2                   32.1  
23 PETG     100   3                   60.9  
24 PETG     100   4                   10.4  
25 PETG     100   5                   65.0  
26 PETG     110   1                    4.54 
27 PETG     110   2                    3.72 
28 PETG     110   3                   37.1  
29 PETG     110   4                    7.51 
30 PETG     110   5                   53.6  
31 PETG     80    1                    1.69 
32 PETG     80    2                    1.84 
33 PETG     80    3                    1.88 
34 PETG     80    4                    1.55 
35 PETG     80    5                    1.69 
36 PETG     90    1                   63.5  
37 PETG     90    2                    1.57 
38 PETG     90    3                    1.61 
39 PETG     90    4                    1.67 
40 PETG     90    5                   42.9  
> break_summary_combined <- bind_rows(
  +     break_summary %>% left_join(toughness_summary, by = c("material", "flow", "sample")) %>% mutate(set = "SET-1"),
  +     break_summary_set2 %>% left_join(toughness_summary_set2, by = c("material", "flow", "sample")) %>% mutate(set = "SET-2")
  + ) %>%
  +     mutate(flow = factor(flow, levels = c("80", "90", "100", "110")))
> 
  > property_stats_combined <- break_summary_combined %>%
  +     group_by(set, material, flow) %>%
  +     summarise(
    +         mean_UTS = mean(UTS_MPa), sd_UTS = sd(UTS_MPa),
    +         mean_elongation = mean(elongation_at_break_pct), sd_elongation = sd(elongation_at_break_pct),
    +         mean_toughness = mean(toughness_MJ_per_m3), sd_toughness = sd(toughness_MJ_per_m3),
    +         .groups = "drop"
    +     )
> 
  > print(property_stats_combined, n = 20)
# A tibble: 16 × 9
set   material flow  mean_UTS sd_UTS mean_elongation sd_elongation mean_toughness sd_toughness
<chr> <chr>    <fct>    <dbl>  <dbl>           <dbl>         <dbl>          <dbl>        <dbl>
  1 SET-1 CF PETG  80        66.8   3.91            6.31         7.69            1.83        1.52 
2 SET-1 CF PETG  90        65.5   3.17            2.91         0.210           1.18        0.144
3 SET-1 CF PETG  100       65.7   7.63            2.65         0.275           1.06        0.240
4 SET-1 CF PETG  110       74.0   4.26            3.10         0.367           1.42        0.301
5 SET-1 PETG     80        61.2   2.14            4.72         0.309           1.73        0.131
6 SET-1 PETG     90        61.5   1.67           54.9         71.5            22.3        29.2  
7 SET-1 PETG     100       61.5   2.41           92.7         73.2            34.7        27.7  
8 SET-1 PETG     110       61.1   5.76           56.6         55.7            21.3        22.8  
9 SET-2 CF PETG  80        62.5   2.92            2.66         0.302           1.03        0.182
10 SET-2 CF PETG  90        71.0   6.93            2.96         0.201           1.32        0.160
11 SET-2 CF PETG  100       76.1   4.52            2.93         0.361           1.39        0.257
12 SET-2 CF PETG  110       84.9   3.18            3.48         0.486           1.79        0.148
13 SET-2 PETG     80        53.1   3.26           24.8         20.9             7.09        5.33 
14 SET-2 PETG     90        54.1   2.72           21.8         12.9             7.28        4.58 
15 SET-2 PETG     100       55.7   4.99           14.7          6.34            4.74        2.27 
16 SET-2 PETG     110       57.6   3.09           76.3         65.9            29.2        26.5  
> p_uts_combined <- ggplot(property_stats_combined, aes(x = flow, y = mean_UTS, color = material, group = interaction(material, set))) +
  +     geom_line(aes(linetype = set), linewidth = 0.9) +
  +     geom_point(size = 2.5) +
  +     geom_errorbar(aes(ymin = mean_UTS - sd_UTS, ymax = mean_UTS + sd_UTS), width = 0.15) +
  +     labs(x = "Flow Rate (%)", y = "UTS (MPa)",
             +          title = "Ultimate Tensile Strength vs Flow Rate — SET-1 vs SET-2",
             +          linetype = "Set", color = "Material") +
  +     theme_minimal()
> 
  > p_uts_combined
> ggsave(file.path(output_dir_break, "UTS_vs_flow_both_sets.png"), p_uts_combined, width = 9, height = 6, dpi = 300)
> p_elong_combined <- ggplot(property_stats_combined, aes(x = flow, y = mean_elongation, color = material, group = interaction(material, set))) +
  +     geom_line(aes(linetype = set), linewidth = 0.9) +
  +     geom_point(size = 2.5) +
  +     geom_errorbar(aes(ymin = pmax(mean_elongation - sd_elongation, 0.1), ymax = mean_elongation + sd_elongation), width = 0.15) +
  +     scale_y_log10() +
  +     labs(x = "Flow Rate (%)", y = "Elongation at Break (%) [log scale]",
             +          title = "Elongation at Break vs Flow Rate — SET-1 vs SET-2",
             +          linetype = "Set", color = "Material") +
  +     theme_minimal()
> 
  > p_elong_combined
> ggsave(file.path(output_dir_break, "elongation_vs_flow_both_sets.png"), p_elong_combined, width = 9, height = 6, dpi = 300)
> p_tough_combined <- ggplot(property_stats_combined, aes(x = flow, y = mean_toughness, color = material, group = interaction(material, set))) +
  +     geom_line(aes(linetype = set), linewidth = 0.9) +
  +     geom_point(size = 2.5) +
  +     geom_errorbar(aes(ymin = pmax(mean_toughness - sd_toughness, 0.1), ymax = mean_toughness + sd_toughness), width = 0.15) +
  +     scale_y_log10() +
  +     labs(x = "Flow Rate (%)", y = "Toughness (MJ/m³) [log scale]",
             +          title = "Toughness vs Flow Rate — SET-1 vs SET-2",
             +          linetype = "Set", color = "Material") +
  +     theme_minimal()
> 
  > p_tough_combined
> ggsave(file.path(output_dir_break, "toughness_vs_flow_both_sets.png"), p_tough_combined, width = 9, height = 6, dpi = 300)
> property_stats_median <- break_summary_combined %>%
  +     group_by(set, material, flow) %>%
  +     summarise(
    +         median_elongation = median(elongation_at_break_pct),
    +         q1_elongation = quantile(elongation_at_break_pct, 0.25),
    +         q3_elongation = quantile(elongation_at_break_pct, 0.75),
    +         median_toughness = median(toughness_MJ_per_m3),
    +         q1_toughness = quantile(toughness_MJ_per_m3, 0.25),
    +         q3_toughness = quantile(toughness_MJ_per_m3, 0.75),
    +         .groups = "drop"
    +     )
> 
  > print(property_stats_median, n = 20)
# A tibble: 16 × 9
set   material flow  median_elongation q1_elongation q3_elongation median_toughness q1_toughness
<chr> <chr>    <fct>             <dbl>         <dbl>         <dbl>            <dbl>        <dbl>
  1 SET-1 CF PETG  80                 3.08          2.70          3.14             1.28        1.07 
2 SET-1 CF PETG  90                 2.93          2.76          3.08             1.12        1.12 
3 SET-1 CF PETG  100                2.70          2.37          2.84             1.08        0.876
4 SET-1 CF PETG  110                3.03          2.82          3.40             1.40        1.15 
5 SET-1 PETG     80                 4.58          4.54          4.81             1.69        1.69 
6 SET-1 PETG     90                 4.49          4.44        104.               1.67        1.61 
7 SET-1 PETG     100               87.4          27.4         166.              32.1        10.4  
8 SET-1 PETG     110               25.5          13.0         102.               7.51        4.54 
9 SET-2 CF PETG  80                 2.62          2.45          2.71             1.01        0.889
10 SET-2 CF PETG  90                 3.01          2.79          3.10             1.28        1.19 
11 SET-2 CF PETG  100                2.75          2.71          3.10             1.25        1.21 
12 SET-2 CF PETG  110                3.33          3.24          3.44             1.81        1.74 
13 SET-2 PETG     80                16.5          15.8          28.2              5.24        4.43 
14 SET-2 PETG     90                20.2          12.0          23.2              7.20        3.55 
15 SET-2 PETG     100               17.0          14.4          19.1              4.93        4.49 
16 SET-2 PETG     110               54.9          21.3         126.              22.5         6.99 
# ℹ 1 more variable: q3_toughness <dbl>
> p_elong_median <- ggplot(property_stats_median, aes(x = flow, y = median_elongation, color = material, group = interaction(material, set))) +
  +     geom_line(aes(linetype = set), linewidth = 0.9) +
  +     geom_point(size = 2.5) +
  +     geom_errorbar(aes(ymin = q1_elongation, ymax = q3_elongation), width = 0.15) +
  +     scale_y_log10() +
  +     labs(x = "Flow Rate (%)", y = "Elongation at Break (%) [log scale]",
             +          title = "Elongation at Break vs Flow Rate — SET-1 vs SET-2",
             +          subtitle = "Points = median; error bars = IQR (25th-75th percentile)",
             +          linetype = "Set", color = "Material") +
  +     theme_minimal()
> 
  > p_elong_median
> ggsave(file.path(output_dir_break, "elongation_vs_flow_both_sets.png"), p_elong_median, width = 9, height = 6, dpi = 300)
> p_tough_median <- ggplot(property_stats_median, aes(x = flow, y = median_toughness, color = material, group = interaction(material, set))) +
  +     geom_line(aes(linetype = set), linewidth = 0.9) +
  +     geom_point(size = 2.5) +
  +     geom_errorbar(aes(ymin = q1_toughness, ymax = q3_toughness), width = 0.15) +
  +     scale_y_log10() +
  +     labs(x = "Flow Rate (%)", y = "Toughness (MJ/m³) [log scale]",
             +          title = "Toughness vs Flow Rate — SET-1 vs SET-2",
             +          subtitle = "Points = median; error bars = IQR (25th-75th percentile)",
             +          linetype = "Set", color = "Material") +
  +     theme_minimal()
> 
  > p_tough_median
> ggsave(file.path(output_dir_break, "toughness_vs_flow_both_sets.png"), p_tough_median, width = 9, height = 6, dpi = 300)
> aov_UTS_combined <- aov(UTS_MPa ~ material * flow * set, data = break_summary_combined)
> summary(aov_UTS_combined)
Df Sum Sq Mean Sq F value   Pr(>F)    
material           1   3172    3172 176.394  < 2e-16 ***
  flow               3    775     258  14.372 2.91e-07 ***
  set                1      2       2   0.091 0.764248    
material:flow      3    437     146   8.100 0.000118 ***
  material:set       1    696     696  38.682 4.25e-08 ***
  flow:set           3    291      97   5.392 0.002264 ** 
  material:flow:set  3    114      38   2.109 0.107729    
Residuals         64   1151      18                     
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> break_summary_combined <- break_summary_combined %>%
  +     mutate(log_elongation = log(elongation_at_break_pct),
               +            log_toughness = log(toughness_MJ_per_m3))
> 
  > aov_elongation_combined <- aov(log_elongation ~ material * flow * set, data = break_summary_combined)
> summary(aov_elongation_combined)
Df Sum Sq Mean Sq F value  Pr(>F)    
material           1  74.72   74.72 125.810 < 2e-16 ***
  flow               3   6.03    2.01   3.382 0.02344 *  
  set                1   0.00    0.00   0.001 0.97789    
material:flow      3   7.01    2.34   3.937 0.01216 *  
  material:set       1   0.08    0.08   0.141 0.70845    
flow:set           3   4.07    1.36   2.282 0.08755 .  
material:flow:set  3   7.72    2.57   4.330 0.00767 ** 
  Residuals         64  38.01    0.59                    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> 
  > aov_toughness_combined <- aov(log_toughness ~ material * flow * set, data = break_summary_combined)
> summary(aov_toughness_combined)
Df Sum Sq Mean Sq F value  Pr(>F)    
material           1  59.54   59.54  96.783   2e-14 ***
  flow               3   8.78    2.93   4.755 0.00469 ** 
  set                1   0.00    0.00   0.002 0.96251    
material:flow      3   5.47    1.82   2.962 0.03871 *  
  material:set       1   0.06    0.06   0.095 0.75860    
flow:set           3   4.08    1.36   2.209 0.09557 .  
material:flow:set  3   8.14    2.71   4.412 0.00697 ** 
  Residuals         64  39.38    0.62                    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> list.files(output_dir, pattern = "^representative_")
[1] "representative_modulus_picks.csv"       "representative_SET-1_CF_PETG_90pct.png"
> exists("plot_modulus_representative")
[1] TRUE
> exists("representative_modulus")
[1] TRUE
> exists("all_data")
[1] TRUE
> conditions <- expand_grid(set = c("SET-1", "SET-2"), material = c("CF PETG", "PETG"), flow = c("80","90","100","110"))
> nrow(conditions)
[1] 16
> for (i in seq_len(nrow(conditions))) {
  +     plot_modulus_representative(conditions$set[i], conditions$material[i], conditions$flow[i])
  + }
There were 16 warnings (use warnings() to see them)

> list.files(output_dir, pattern = "^representative_")
[1] "representative_modulus_picks.csv"        "representative_SET-1_CF_PETG_100pct.png"
[3] "representative_SET-1_CF_PETG_110pct.png" "representative_SET-1_CF_PETG_80pct.png" 
[5] "representative_SET-1_CF_PETG_90pct.png"  "representative_SET-1_PETG_100pct.png"   
[7] "representative_SET-1_PETG_110pct.png"    "representative_SET-1_PETG_80pct.png"    
[9] "representative_SET-1_PETG_90pct.png"     "representative_SET-2_CF_PETG_100pct.png"
[11] "representative_SET-2_CF_PETG_110pct.png" "representative_SET-2_CF_PETG_80pct.png" 
[13] "representative_SET-2_CF_PETG_90pct.png"  "representative_SET-2_PETG_100pct.png"   
[15] "representative_SET-2_PETG_110pct.png"    "representative_SET-2_PETG_80pct.png"    
[17] "representative_SET-2_PETG_90pct.png"    
> list.files(output_dir)
[1] "audit_SET-1_CF_PETG_100pct.png"          "audit_SET-1_CF_PETG_110pct.png"         
[3] "audit_SET-1_CF_PETG_80pct.png"           "audit_SET-1_CF_PETG_90pct.png"          
[5] "audit_SET-1_PETG_100pct.png"             "audit_SET-1_PETG_110pct.png"            
[7] "audit_SET-1_PETG_80pct.png"              "audit_SET-1_PETG_90pct.png"             
[9] "audit_SET-2_CF_PETG_100pct.png"          "audit_SET-2_CF_PETG_110pct.png"         
[11] "audit_SET-2_CF_PETG_80pct.png"           "audit_SET-2_CF_PETG_90pct.png"          
[13] "audit_SET-2_PETG_100pct.png"             "audit_SET-2_PETG_110pct.png"            
[15] "audit_SET-2_PETG_80pct.png"              "audit_SET-2_PETG_90pct.png"             
[17] "audit_SET1_modulus.png"                  "audit_SET2_modulus.png"                 
[19] "label_check.csv"                         "modulus_summary_table_all.csv"          
[21] "modulus_vs_flow_both_sets.png"           "representative_modulus_picks.csv"       
[23] "representative_SET-1_CF_PETG_100pct.png" "representative_SET-1_CF_PETG_110pct.png"
[25] "representative_SET-1_CF_PETG_80pct.png"  "representative_SET-1_CF_PETG_90pct.png" 
[27] "representative_SET-1_PETG_100pct.png"    "representative_SET-1_PETG_110pct.png"   
[29] "representative_SET-1_PETG_80pct.png"     "representative_SET-1_PETG_90pct.png"    
[31] "representative_SET-2_CF_PETG_100pct.png" "representative_SET-2_CF_PETG_110pct.png"
[33] "representative_SET-2_CF_PETG_80pct.png"  "representative_SET-2_CF_PETG_90pct.png" 
[35] "representative_SET-2_PETG_100pct.png"    "representative_SET-2_PETG_110pct.png"   
[37] "representative_SET-2_PETG_80pct.png"     "representative_SET-2_PETG_90pct.png"    
> list.files(output_dir_break)
[1] "audit_SET2_break_CF_PETG_100pct.png"             "audit_SET2_break_CF_PETG_110pct.png"            
[3] "audit_SET2_break_CF_PETG_80pct.png"              "audit_SET2_break_CF_PETG_90pct.png"             
[5] "audit_SET2_break_PETG_100pct.png"                "audit_SET2_break_PETG_110pct.png"               
[7] "audit_SET2_break_PETG_80pct.png"                 "audit_SET2_break_PETG_90pct.png"                
[9] "elongation_vs_flow_both_sets.png"                "representative_CF_PETG_SET2_all_flows.png"      
[11] "representative_PETG_SET2_split_failure_mode.png" "toughness_vs_flow_both_sets.png"                
[13] "UTS_vs_flow_both_sets.png"                      
> # ---- Modulus ANOVA ----
> aov_modulus_combined <- aov(modulus_GPa ~ material * flow * set, data = combined_modulus)
Error: object 'combined_modulus' not found

> combined_modulus <- summary_table %>%
  +     filter(!excluded_from_modulus) %>%
  +     select(material, flow, set, sample, modulus_GPa)
> 
  > nrow(combined_modulus)
[1] 79
> aov_modulus_combined <- aov(modulus_GPa ~ material * flow * set, data = combined_modulus)
> summary(aov_modulus_combined)
Df Sum Sq Mean Sq  F value   Pr(>F)    
material           1 250.56  250.56 3418.745  < 2e-16 ***
  flow               3   0.99    0.33    4.502  0.00633 ** 
  set                1   6.22    6.22   84.830 2.81e-13 ***
  material:flow      3   0.30    0.10    1.362  0.26268    
material:set       1   8.95    8.95  122.124  < 2e-16 ***
  flow:set           3   0.38    0.13    1.721  0.17163    
material:flow:set  3   0.24    0.08    1.106  0.35345    
Residuals         63   4.62    0.07                      
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> aov_UTS_combined <- aov(UTS_MPa ~ material * flow * set, data = break_summary_combined)
> summary(aov_UTS_combined)
Df Sum Sq Mean Sq F value   Pr(>F)    
material           1   3172    3172 176.394  < 2e-16 ***
  flow               3    775     258  14.372 2.91e-07 ***
  set                1      2       2   0.091 0.764248    
material:flow      3    437     146   8.100 0.000118 ***
  material:set       1    696     696  38.682 4.25e-08 ***
  flow:set           3    291      97   5.392 0.002264 ** 
  material:flow:set  3    114      38   2.109 0.107729    
Residuals         64   1151      18                     
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> break_summary_combined <- break_summary_combined %>%
  +     mutate(log_elongation = log(elongation_at_break_pct),
               +            log_toughness = log(toughness_MJ_per_m3))
> 
  > aov_elongation_combined <- aov(log_elongation ~ material * flow * set, data = break_summary_combined)
> summary(aov_elongation_combined)
Df Sum Sq Mean Sq F value  Pr(>F)    
material           1  74.72   74.72 125.810 < 2e-16 ***
  flow               3   6.03    2.01   3.382 0.02344 *  
  set                1   0.00    0.00   0.001 0.97789    
material:flow      3   7.01    2.34   3.937 0.01216 *  
  material:set       1   0.08    0.08   0.141 0.70845    
flow:set           3   4.07    1.36   2.282 0.08755 .  
material:flow:set  3   7.72    2.57   4.330 0.00767 ** 
  Residuals         64  38.01    0.59                    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> 
  > aov_toughness_combined <- aov(log_toughness ~ material * flow * set, data = break_summary_combined)
> summary(aov_toughness_combined)
Df Sum Sq Mean Sq F value  Pr(>F)    
material           1  59.54   59.54  96.783   2e-14 ***
  flow               3   8.78    2.93   4.755 0.00469 ** 
  set                1   0.00    0.00   0.002 0.96251    
material:flow      3   5.47    1.82   2.962 0.03871 *  
  material:set       1   0.06    0.06   0.095 0.75860    
flow:set           3   4.08    1.36   2.209 0.09557 .  
material:flow:set  3   8.14    2.71   4.412 0.00697 ** 
  Residuals         64  39.38    0.62                    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> break_summary_combined <- break_summary_combined %>%
  +     mutate(log_elongation = log(elongation_at_break_pct),
               +            log_toughness = log(toughness_MJ_per_m3))
> 
  > aov_elongation_combined <- aov(log_elongation ~ material * flow * set, data = break_summary_combined)
> summary(aov_elongation_combined)
Df Sum Sq Mean Sq F value  Pr(>F)    
material           1  74.72   74.72 125.810 < 2e-16 ***
  flow               3   6.03    2.01   3.382 0.02344 *  
  set                1   0.00    0.00   0.001 0.97789    
material:flow      3   7.01    2.34   3.937 0.01216 *  
  material:set       1   0.08    0.08   0.141 0.70845    
flow:set           3   4.07    1.36   2.282 0.08755 .  
material:flow:set  3   7.72    2.57   4.330 0.00767 ** 
  Residuals         64  38.01    0.59                    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> 
  > aov_toughness_combined <- aov(log_toughness ~ material * flow * set, data = break_summary_combined)
> summary(aov_toughness_combined)
Df Sum Sq Mean Sq F value  Pr(>F)    
material           1  59.54   59.54  96.783   2e-14 ***
  flow               3   8.78    2.93   4.755 0.00469 ** 
  set                1   0.00    0.00   0.002 0.96251    
material:flow      3   5.47    1.82   2.962 0.03871 *  
  material:set       1   0.06    0.06   0.095 0.75860    
flow:set           3   4.08    1.36   2.209 0.09557 .  
material:flow:set  3   8.14    2.71   4.412 0.00697 ** 
  Residuals         64  39.38    0.62                    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> # ---- Modulus: Material:Set interaction ----
> TukeyHSD(aov_modulus_combined, "material:set")
Tukey multiple comparisons of means
95% family-wise confidence level

Fit: aov(formula = modulus_GPa ~ material * flow * set, data = combined_modulus)

$`material:set`
diff        lwr        upr     p adj
PETG:SET-1-CF PETG:SET-1    -2.890365 -3.1162831 -2.6644460 0.0000000
CF PETG:SET-2-CF PETG:SET-1  1.225543  0.9996245  1.4514616 0.0000000
PETG:SET-2-CF PETG:SET-1    -3.011377 -3.2402484 -2.7825047 0.0000000
CF PETG:SET-2-PETG:SET-1     4.115908  3.8899890  4.3418262 0.0000000
PETG:SET-2-PETG:SET-1       -0.121012 -0.3498839  0.1078599 0.5069778
PETG:SET-2-CF PETG:SET-2    -4.236920 -4.4657915 -4.0080477 0.0000000

> TukeyHSD(aov_UTS_combined, "material:set")
Tukey multiple comparisons of means
95% family-wise confidence level

Fit: aov(formula = UTS_MPa ~ material * flow * set, data = break_summary_combined)

$`material:set`
diff        lwr        upr     p adj
PETG:SET-1-CF PETG:SET-1     -6.69580 -10.232893  -3.158707 0.0000283
CF PETG:SET-2-CF PETG:SET-1   5.61150   2.074407   9.148593 0.0005046
PETG:SET-2-CF PETG:SET-1    -12.87845 -16.415543  -9.341357 0.0000000
CF PETG:SET-2-PETG:SET-1     12.30730   8.770207  15.844393 0.0000000
PETG:SET-2-PETG:SET-1        -6.18265  -9.719743  -2.645557 0.0001140
PETG:SET-2-CF PETG:SET-2    -18.48995 -22.027043 -14.952857 0.0000000

> TukeyHSD(aov_UTS_combined, "flow:set")
Tukey multiple comparisons of means
95% family-wise confidence level

Fit: aov(formula = UTS_MPa ~ material * flow * set, data = break_summary_combined)

$`flow:set`
diff         lwr        upr     p adj
90:SET-1-80:SET-1   -0.5065  -6.4483826  5.4353826 0.9999946
100:SET-1-80:SET-1  -0.4535  -6.3953826  5.4883826 0.9999975
110:SET-1-80:SET-1   3.5170  -2.4248826  9.4588826 0.5861818
80:SET-2-80:SET-1   -6.2180 -12.1598826 -0.2761174 0.0339180
90:SET-2-80:SET-1   -1.4457  -7.3875826  4.4961826 0.9944312
100:SET-2-80:SET-1   1.8981  -4.0437826  7.8399826 0.9726755
110:SET-2-80:SET-1   7.1803   1.2384174 13.1221826 0.0077199
100:SET-1-90:SET-1   0.0530  -5.8888826  5.9948826 1.0000000
110:SET-1-90:SET-1   4.0235  -1.9183826  9.9653826 0.4125838
80:SET-2-90:SET-1   -5.7115 -11.6533826  0.2303826 0.0681489
90:SET-2-90:SET-1   -0.9392  -6.8810826  5.0026826 0.9996469
100:SET-2-90:SET-1   2.4046  -3.5372826  8.3464826 0.9071455
110:SET-2-90:SET-1   7.6868   1.7449174 13.6286826 0.0033073
110:SET-1-100:SET-1  3.9705  -1.9713826  9.9123826 0.4299472
80:SET-2-100:SET-1  -5.7645 -11.7063826  0.1773826 0.0635379
90:SET-2-100:SET-1  -0.9922  -6.9340826  4.9496826 0.9994930
100:SET-2-100:SET-1  2.3516  -3.5902826  8.2934826 0.9165888
110:SET-2-100:SET-1  7.6338   1.6919174 13.5756826 0.0036211
80:SET-2-110:SET-1  -9.7350 -15.6768826 -3.7931174 0.0000753
90:SET-2-110:SET-1  -4.9627 -10.9045826  0.9791826 0.1689812
100:SET-2-110:SET-1 -1.6189  -7.5607826  4.3229826 0.9890077
110:SET-2-110:SET-1  3.6633  -2.2785826  9.6051826 0.5349060
90:SET-2-80:SET-2    4.7723  -1.1695826 10.7141826 0.2073455
100:SET-2-80:SET-2   8.1161   2.1742174 14.0579826 0.0015627
110:SET-2-80:SET-2  13.3983   7.4564174 19.3401826 0.0000000
100:SET-2-90:SET-2   3.3438  -2.5980826  9.2856826 0.6464386
110:SET-2-90:SET-2   8.6260   2.6841174 14.5678826 0.0006208
110:SET-2-100:SET-2  5.2822  -0.6596826 11.2240826 0.1169463

> TukeyHSD(aov_UTS_combined, "material:flow")
Tukey multiple comparisons of means
95% family-wise confidence level

Fit: aov(formula = UTS_MPa ~ material * flow * set, data = break_summary_combined)

$`material:flow`
diff         lwr         upr     p adj
PETG:80-CF PETG:80       -7.4966 -13.4384826  -1.5547174 0.0045694
CF PETG:90-CF PETG:80     3.6006  -2.3412826   9.5424826 0.5568615
PETG:90-CF PETG:80       -6.8314 -12.7732826  -0.8895174 0.0134863
CF PETG:100-CF PETG:80    6.2439   0.3020174  12.1857826 0.0326759
PETG:100-CF PETG:80      -6.0779 -12.0197826  -0.1360174 0.0413921
CF PETG:110-CF PETG:80   14.7699   8.8280174  20.7117826 0.0000000
PETG:110-CF PETG:80      -5.3512 -11.2930826   0.5906826 0.1075900
CF PETG:90-PETG:80       11.0972   5.1553174  17.0390826 0.0000049
PETG:90-PETG:80           0.6652  -5.2766826   6.6070826 0.9999652
CF PETG:100-PETG:80      13.7405   7.7986174  19.6823826 0.0000000
PETG:100-PETG:80          1.4187  -4.5231826   7.3605826 0.9950387
CF PETG:110-PETG:80      22.2665  16.3246174  28.2083826 0.0000000
PETG:110-PETG:80          2.1454  -3.7964826   8.0872826 0.9473549
PETG:90-CF PETG:90      -10.4320 -16.3738826  -4.4901174 0.0000189
CF PETG:100-CF PETG:90    2.6433  -3.2985826   8.5851826 0.8567116
PETG:100-CF PETG:90      -9.6785 -15.6203826  -3.7366174 0.0000841
CF PETG:110-CF PETG:90   11.1693   5.2274174  17.1111826 0.0000042
PETG:110-CF PETG:90      -8.9518 -14.8936826  -3.0099174 0.0003385
CF PETG:100-PETG:90      13.0753   7.1334174  19.0171826 0.0000001
PETG:100-PETG:90          0.7535  -5.1883826   6.6953826 0.9999190
CF PETG:110-PETG:90      21.6013  15.6594174  27.5431826 0.0000000
PETG:110-PETG:90          1.4802  -4.4616826   7.4220826 0.9935709
PETG:100-CF PETG:100    -12.3218 -18.2636826  -6.3799174 0.0000004
CF PETG:110-CF PETG:100   8.5260   2.5841174  14.4678826 0.0007459
PETG:110-CF PETG:100    -11.5951 -17.5369826  -5.6532174 0.0000018
CF PETG:110-PETG:100     20.8478  14.9059174  26.7896826 0.0000000
PETG:110-PETG:100         0.7267  -5.2151826   6.6685826 0.9999366
PETG:110-CF PETG:110    -20.1211 -26.0629826 -14.1792174 0.0000000

> tukey_elong_3way <- TukeyHSD(aov_elongation_combined, "material:flow:set")
> tukey_elong_df <- as.data.frame(tukey_elong_3way$`material:flow:set`)
> tukey_elong_df$comparison <- rownames(tukey_elong_df)
> 
  > # filter to just PETG comparisons at the SAME K, flat vs on-edge (the most relevant comparisons)
  > tukey_elong_df %>%
  +     filter(str_detect(comparison, "PETG:")) %>%
  +     filter(str_detect(comparison, "SET-1") & str_detect(comparison, "SET-2")) %>%
  +     select(comparison, diff, `p adj`)
comparison         diff        p adj
CF PETG:80:SET-2-CF PETG:80:SET-1     CF PETG:80:SET-2-CF PETG:80:SET-1 -0.468177974 9.998172e-01
PETG:80:SET-2-CF PETG:80:SET-1           PETG:80:SET-2-CF PETG:80:SET-1  1.461526861 1.990700e-01
CF PETG:90:SET-2-CF PETG:80:SET-1     CF PETG:90:SET-2-CF PETG:80:SET-1 -0.357730158 9.999941e-01
PETG:90:SET-2-CF PETG:80:SET-1           PETG:90:SET-2-CF PETG:80:SET-1  1.513359279 1.578337e-01
CF PETG:100:SET-2-CF PETG:80:SET-1   CF PETG:100:SET-2-CF PETG:80:SET-1 -0.369858173 9.999908e-01
PETG:100:SET-2-CF PETG:80:SET-1         PETG:100:SET-2-CF PETG:80:SET-1  1.113958759 6.352207e-01
CF PETG:110:SET-2-CF PETG:80:SET-1   CF PETG:110:SET-2-CF PETG:80:SET-1 -0.200591778 1.000000e+00
PETG:110:SET-2-CF PETG:80:SET-1         PETG:110:SET-2-CF PETG:80:SET-1  2.505430563 2.941280e-04
CF PETG:80:SET-2-PETG:80:SET-1           CF PETG:80:SET-2-PETG:80:SET-1 -0.578205334 9.979188e-01
PETG:80:SET-2-PETG:80:SET-1                 PETG:80:SET-2-PETG:80:SET-1  1.351499502 3.107561e-01
CF PETG:90:SET-2-PETG:80:SET-1           CF PETG:90:SET-2-PETG:80:SET-1 -0.467757518 9.998192e-01
PETG:90:SET-2-PETG:80:SET-1                 PETG:90:SET-2-PETG:80:SET-1  1.403331919 2.540494e-01
CF PETG:100:SET-2-PETG:80:SET-1         CF PETG:100:SET-2-PETG:80:SET-1 -0.479885533 9.997538e-01
PETG:100:SET-2-PETG:80:SET-1               PETG:100:SET-2-PETG:80:SET-1  1.003931399 7.823436e-01
CF PETG:110:SET-2-PETG:80:SET-1         CF PETG:110:SET-2-PETG:80:SET-1 -0.310619138 9.999991e-01
PETG:110:SET-2-PETG:80:SET-1               PETG:110:SET-2-PETG:80:SET-1  2.395403203 6.650380e-04
CF PETG:80:SET-2-CF PETG:90:SET-1     CF PETG:80:SET-2-CF PETG:90:SET-1 -0.095778905 1.000000e+00
PETG:80:SET-2-CF PETG:90:SET-1           PETG:80:SET-2-CF PETG:90:SET-1  1.833925931 2.874649e-02
CF PETG:90:SET-2-CF PETG:90:SET-1     CF PETG:90:SET-2-CF PETG:90:SET-1  0.014668911 1.000000e+00
PETG:90:SET-2-CF PETG:90:SET-1           PETG:90:SET-2-CF PETG:90:SET-1  1.885758348 2.103990e-02
CF PETG:100:SET-2-CF PETG:90:SET-1   CF PETG:100:SET-2-CF PETG:90:SET-1  0.002540896 1.000000e+00
PETG:100:SET-2-CF PETG:90:SET-1         PETG:100:SET-2-CF PETG:90:SET-1  1.486357828 1.784238e-01
CF PETG:110:SET-2-CF PETG:90:SET-1   CF PETG:110:SET-2-CF PETG:90:SET-1  0.171807291 1.000000e+00
PETG:110:SET-2-CF PETG:90:SET-1         PETG:110:SET-2-CF PETG:90:SET-1  2.877829632 1.648555e-05
CF PETG:80:SET-2-PETG:90:SET-1           CF PETG:80:SET-2-PETG:90:SET-1 -1.862545085 2.422269e-02
PETG:80:SET-2-PETG:90:SET-1                 PETG:80:SET-2-PETG:90:SET-1  0.067159750 1.000000e+00
CF PETG:90:SET-2-PETG:90:SET-1           CF PETG:90:SET-2-PETG:90:SET-1 -1.752097269 4.617107e-02
PETG:90:SET-2-PETG:90:SET-1                 PETG:90:SET-2-PETG:90:SET-1  0.118992168 1.000000e+00
CF PETG:100:SET-2-PETG:90:SET-1         CF PETG:100:SET-2-PETG:90:SET-1 -1.764225284 4.310568e-02
PETG:100:SET-2-PETG:90:SET-1               PETG:100:SET-2-PETG:90:SET-1 -0.280408352 9.999998e-01
CF PETG:110:SET-2-PETG:90:SET-1         CF PETG:110:SET-2-PETG:90:SET-1 -1.594958889 1.066477e-01
PETG:110:SET-2-PETG:90:SET-1               PETG:110:SET-2-PETG:90:SET-1  1.111063452 6.393547e-01
CF PETG:80:SET-2-CF PETG:100:SET-1   CF PETG:80:SET-2-CF PETG:100:SET-1  0.002503229 1.000000e+00
PETG:80:SET-2-CF PETG:100:SET-1         PETG:80:SET-2-CF PETG:100:SET-1  1.932208064 1.579046e-02
CF PETG:90:SET-2-CF PETG:100:SET-1   CF PETG:90:SET-2-CF PETG:100:SET-1  0.112951045 1.000000e+00
PETG:90:SET-2-CF PETG:100:SET-1         PETG:90:SET-2-CF PETG:100:SET-1  1.984040481 1.137511e-02
CF PETG:100:SET-2-CF PETG:100:SET-1 CF PETG:100:SET-2-CF PETG:100:SET-1  0.100823030 1.000000e+00
PETG:100:SET-2-CF PETG:100:SET-1       PETG:100:SET-2-CF PETG:100:SET-1  1.584639961 1.122604e-01
CF PETG:110:SET-2-CF PETG:100:SET-1 CF PETG:110:SET-2-CF PETG:100:SET-1  0.270089424 9.999999e-01
PETG:110:SET-2-CF PETG:100:SET-1       PETG:110:SET-2-CF PETG:100:SET-1  2.976111766 7.525881e-06
CF PETG:80:SET-2-PETG:100:SET-1         CF PETG:80:SET-2-PETG:100:SET-1 -3.166359880 1.617115e-06
PETG:80:SET-2-PETG:100:SET-1               PETG:80:SET-2-PETG:100:SET-1 -1.236655045 4.589732e-01
CF PETG:90:SET-2-PETG:100:SET-1         CF PETG:90:SET-2-PETG:100:SET-1 -3.055912064 3.959911e-06
PETG:90:SET-2-PETG:100:SET-1               PETG:90:SET-2-PETG:100:SET-1 -1.184822627 5.328007e-01
CF PETG:100:SET-2-PETG:100:SET-1       CF PETG:100:SET-2-PETG:100:SET-1 -3.068040079 3.590342e-06
PETG:100:SET-2-PETG:100:SET-1             PETG:100:SET-2-PETG:100:SET-1 -1.584223147 1.124920e-01
CF PETG:110:SET-2-PETG:100:SET-1       CF PETG:110:SET-2-PETG:100:SET-1 -2.898773684 1.395808e-05
PETG:110:SET-2-PETG:100:SET-1             PETG:110:SET-2-PETG:100:SET-1 -0.192751343 1.000000e+00
CF PETG:80:SET-2-CF PETG:110:SET-1   CF PETG:80:SET-2-CF PETG:110:SET-1 -0.154638377 1.000000e+00
PETG:80:SET-2-CF PETG:110:SET-1         PETG:80:SET-2-CF PETG:110:SET-1  1.775066458 4.051954e-02
CF PETG:90:SET-2-CF PETG:110:SET-1   CF PETG:90:SET-2-CF PETG:110:SET-1 -0.044190561 1.000000e+00
PETG:90:SET-2-CF PETG:110:SET-1         PETG:90:SET-2-CF PETG:110:SET-1  1.826898875 2.996814e-02
CF PETG:100:SET-2-CF PETG:110:SET-1 CF PETG:100:SET-2-CF PETG:110:SET-1 -0.056318577 1.000000e+00
PETG:100:SET-2-CF PETG:110:SET-1       PETG:100:SET-2-CF PETG:110:SET-1  1.427498355 2.300897e-01
CF PETG:110:SET-2-CF PETG:110:SET-1 CF PETG:110:SET-2-CF PETG:110:SET-1  0.112947818 1.000000e+00
PETG:110:SET-2-CF PETG:110:SET-1       PETG:110:SET-2-CF PETG:110:SET-1  2.818970160 2.626068e-05
CF PETG:80:SET-2-PETG:110:SET-1         CF PETG:80:SET-2-PETG:110:SET-1 -2.581787935 1.651832e-04
PETG:80:SET-2-PETG:110:SET-1               PETG:80:SET-2-PETG:110:SET-1 -0.652083099 9.927263e-01
CF PETG:90:SET-2-PETG:110:SET-1         CF PETG:90:SET-2-PETG:110:SET-1 -2.471340119 3.794962e-04
PETG:90:SET-2-PETG:110:SET-1               PETG:90:SET-2-PETG:110:SET-1 -0.600250682 9.968919e-01
CF PETG:100:SET-2-PETG:110:SET-1       CF PETG:100:SET-2-PETG:110:SET-1 -2.483468134 3.466758e-04
PETG:100:SET-2-PETG:110:SET-1             PETG:100:SET-2-PETG:110:SET-1 -0.999651202 7.874815e-01
CF PETG:110:SET-2-PETG:110:SET-1       CF PETG:110:SET-2-PETG:110:SET-1 -2.314201739 1.198397e-03
PETG:110:SET-2-PETG:110:SET-1             PETG:110:SET-2-PETG:110:SET-1  0.391820603 9.999804e-01
> target_comparisons <- c(
  +     "CF PETG:80:SET-2-CF PETG:80:SET-1",
  +     "CF PETG:90:SET-2-CF PETG:90:SET-1",
  +     "CF PETG:100:SET-2-CF PETG:100:SET-1",
  +     "CF PETG:110:SET-2-CF PETG:110:SET-1",
  +     "PETG:80:SET-2-PETG:80:SET-1",
  +     "PETG:90:SET-2-PETG:90:SET-1",
  +     "PETG:100:SET-2-PETG:100:SET-1",
  +     "PETG:110:SET-2-PETG:110:SET-1"
  + )
> 
  > tukey_elong_df %>% filter(comparison %in% target_comparisons) %>% select(comparison, diff, `p adj`)
comparison        diff     p adj
CF PETG:80:SET-2-CF PETG:80:SET-1     CF PETG:80:SET-2-CF PETG:80:SET-1 -0.46817797 0.9998172
PETG:80:SET-2-PETG:80:SET-1                 PETG:80:SET-2-PETG:80:SET-1  1.35149950 0.3107561
CF PETG:90:SET-2-CF PETG:90:SET-1     CF PETG:90:SET-2-CF PETG:90:SET-1  0.01466891 1.0000000
PETG:90:SET-2-PETG:90:SET-1                 PETG:90:SET-2-PETG:90:SET-1  0.11899217 1.0000000
CF PETG:100:SET-2-CF PETG:100:SET-1 CF PETG:100:SET-2-CF PETG:100:SET-1  0.10082303 1.0000000
PETG:100:SET-2-PETG:100:SET-1             PETG:100:SET-2-PETG:100:SET-1 -1.58422315 0.1124920
CF PETG:110:SET-2-CF PETG:110:SET-1 CF PETG:110:SET-2-CF PETG:110:SET-1  0.11294782 1.0000000
PETG:110:SET-2-PETG:110:SET-1             PETG:110:SET-2-PETG:110:SET-1  0.39182060 0.9999804
> tukey_tough_3way <- TukeyHSD(aov_toughness_combined, "material:flow:set")
> tukey_tough_df <- as.data.frame(tukey_tough_3way$`material:flow:set`)
> tukey_tough_df$comparison <- rownames(tukey_tough_df)
> 
  > target_comparisons <- c(
    +     "CF PETG:80:SET-2-CF PETG:80:SET-1",
    +     "CF PETG:90:SET-2-CF PETG:90:SET-1",
    +     "CF PETG:100:SET-2-CF PETG:100:SET-1",
    +     "CF PETG:110:SET-2-CF PETG:110:SET-1",
    +     "PETG:80:SET-2-PETG:80:SET-1",
    +     "PETG:90:SET-2-PETG:90:SET-1",
    +     "PETG:100:SET-2-PETG:100:SET-1",
    +     "PETG:110:SET-2-PETG:110:SET-1"
    + )
> 
  > tukey_tough_df %>% filter(comparison %in% target_comparisons) %>% select(comparison, diff, `p adj`)
comparison        diff      p adj
CF PETG:80:SET-2-CF PETG:80:SET-1     CF PETG:80:SET-2-CF PETG:80:SET-1 -0.38470396 0.99998773
PETG:80:SET-2-PETG:80:SET-1                 PETG:80:SET-2-PETG:80:SET-1  1.15948091 0.59884499
CF PETG:90:SET-2-CF PETG:90:SET-1     CF PETG:90:SET-2-CF PETG:90:SET-1  0.11268093 1.00000000
PETG:90:SET-2-PETG:90:SET-1                 PETG:90:SET-2-PETG:90:SET-1 -0.04261537 1.00000000
CF PETG:100:SET-2-CF PETG:100:SET-1 CF PETG:100:SET-2-CF PETG:100:SET-1  0.27657456 0.99999986
PETG:100:SET-2-PETG:100:SET-1             PETG:100:SET-2-PETG:100:SET-1 -1.75093898 0.05511503
CF PETG:110:SET-2-CF PETG:110:SET-1 CF PETG:110:SET-2-CF PETG:110:SET-1  0.24508140 0.99999997
PETG:110:SET-2-PETG:110:SET-1             PETG:110:SET-2-PETG:110:SET-1  0.45065359 0.99990779
> elong_tough_exact <- break_summary_combined %>%
  +     group_by(set, material, flow) %>%
  +     summarise(
    +         median_elongation = median(elongation_at_break_pct),
    +         q1_elongation = quantile(elongation_at_break_pct, 0.25),
    +         q3_elongation = quantile(elongation_at_break_pct, 0.75),
    +         median_toughness = median(toughness_MJ_per_m3),
    +         q1_toughness = quantile(toughness_MJ_per_m3, 0.25),
    +         q3_toughness = quantile(toughness_MJ_per_m3, 0.75),
    +         .groups = "drop"
    +     ) %>%
  +     arrange(material, flow, set)
> 
  > print(elong_tough_exact, n = 20)
# A tibble: 16 × 9
set   material flow  median_elongation q1_elongation q3_elongation median_toughness q1_toughness
<chr> <chr>    <fct>             <dbl>         <dbl>         <dbl>            <dbl>        <dbl>
  1 SET-1 CF PETG  80                 3.08          2.70          3.14             1.28        1.07 
2 SET-2 CF PETG  80                 2.62          2.45          2.71             1.01        0.889
3 SET-1 CF PETG  90                 2.93          2.76          3.08             1.12        1.12 
4 SET-2 CF PETG  90                 3.01          2.79          3.10             1.28        1.19 
5 SET-1 CF PETG  100                2.70          2.37          2.84             1.08        0.876
6 SET-2 CF PETG  100                2.75          2.71          3.10             1.25        1.21 
7 SET-1 CF PETG  110                3.03          2.82          3.40             1.40        1.15 
8 SET-2 CF PETG  110                3.33          3.24          3.44             1.81        1.74 
9 SET-1 PETG     80                 4.58          4.54          4.81             1.69        1.69 
10 SET-2 PETG     80                16.5          15.8          28.2              5.24        4.43 
11 SET-1 PETG     90                 4.49          4.44        104.               1.67        1.61 
12 SET-2 PETG     90                20.2          12.0          23.2              7.20        3.55 
13 SET-1 PETG     100               87.4          27.4         166.              32.1        10.4  
14 SET-2 PETG     100               17.0          14.4          19.1              4.93        4.49 
15 SET-1 PETG     110               25.5          13.0         102.               7.51        4.54 
16 SET-2 PETG     110               54.9          21.3         126.              22.5         6.99 
# ℹ 1 more variable: q3_toughness <dbl>
> elong_tough_exact %>% select(set, material, flow, median_toughness, q1_toughness, q3_toughness) %>% print(n = 20)
# A tibble: 16 × 6
set   material flow  median_toughness q1_toughness q3_toughness
<chr> <chr>    <fct>            <dbl>        <dbl>        <dbl>
  1 SET-1 CF PETG  80                1.28        1.07          1.35
2 SET-2 CF PETG  80                1.01        0.889         1.13
3 SET-1 CF PETG  90                1.12        1.12          1.32
4 SET-2 CF PETG  90                1.28        1.19          1.39
5 SET-1 CF PETG  100               1.08        0.876         1.25
6 SET-2 CF PETG  100               1.25        1.21          1.55
7 SET-1 CF PETG  110               1.40        1.15          1.67
8 SET-2 CF PETG  110               1.81        1.74          1.84
9 SET-1 PETG     80                1.69        1.69          1.84
10 SET-2 PETG     80                5.24        4.43          8.69
11 SET-1 PETG     90                1.67        1.61         42.9 
12 SET-2 PETG     90                7.20        3.55          7.93
13 SET-1 PETG     100              32.1        10.4          60.9 
14 SET-2 PETG     100               4.93        4.49          5.70
15 SET-1 PETG     110               7.51        4.54         37.1 
16 SET-2 PETG     110              22.5         6.99         43.7 
> final_dir <- "C:/Users/vipul/Desktop/MSc Project/New Folder/Testing Data/Vipul Dude/Final_Figures"
> dir.create(final_dir, showWarnings = FALSE, recursive = TRUE)
> 
  > # helper to relabel material and set consistently across all data
  > relabel <- function(df) {
    +     df %>%
      +         mutate(material = if_else(material == "CF PETG", "CF-PETG", material),
                       +                orientation = if_else(set == "SET-1", "Flat", "On-edge"))
    + }
> modulus_stats_final <- relabel(modulus_stats)
> p_modulus_final <- ggplot(modulus_stats_final, aes(x = flow, y = mean_modulus_GPa, color = material, group = interaction(material, orientation))) +
  +     geom_line(aes(linetype = orientation), linewidth = 0.9) +
  +     geom_point(size = 2.5) +
  +     geom_errorbar(aes(ymin = mean_modulus_GPa - sd_modulus_GPa, ymax = mean_modulus_GPa + sd_modulus_GPa), width = 0.15) +
  +     labs(x = "K (%)", y = "Young's Modulus (GPa)",
             +          title = "Young's Modulus vs K — Flat vs On-edge Orientation",
             +          linetype = "Orientation", color = "Material") +
  +     theme_minimal()
> ggsave(file.path(final_dir, "modulus_vs_K_both_orientations.png"), p_modulus_final, width = 9, height = 6, dpi = 300)
> 
  > property_stats_final <- relabel(property_stats_combined)
> p_uts_final <- ggplot(property_stats_final, aes(x = flow, y = mean_UTS, color = material, group = interaction(material, orientation))) +
  +     geom_line(aes(linetype = orientation), linewidth = 0.9) +
  +     geom_point(size = 2.5) +
  +     geom_errorbar(aes(ymin = mean_UTS - sd_UTS, ymax = mean_UTS + sd_UTS), width = 0.15) +
  +     labs(x = "K (%)", y = "UTS (MPa)",
             +          title = "Ultimate Tensile Strength vs K — Flat vs On-edge Orientation",
             +          linetype = "Orientation", color = "Material") +
  +     theme_minimal()
> ggsave(file.path(final_dir, "UTS_vs_K_both_orientations.png"), p_uts_final, width = 9, height = 6, dpi = 300)
> 
  > property_stats_median_final <- relabel(property_stats_median)
> p_elong_final <- ggplot(property_stats_median_final, aes(x = flow, y = median_elongation, color = material, group = interaction(material, orientation))) +
  +     geom_line(aes(linetype = orientation), linewidth = 0.9) +
  +     geom_point(size = 2.5) +
  +     geom_errorbar(aes(ymin = q1_elongation, ymax = q3_elongation), width = 0.15) +
  +     scale_y_log10() +
  +     labs(x = "K (%)", y = "Elongation at Break (%) [log scale]",
             +          title = "Elongation at Break vs K — Flat vs On-edge Orientation",
             +          subtitle = "Points = median; error bars = IQR (25th-75th percentile)",
             +          linetype = "Orientation", color = "Material") +
  +     theme_minimal()
> ggsave(file.path(final_dir, "elongation_vs_K_both_orientations.png"), p_elong_final, width = 9, height = 6, dpi = 300)
> 
  > p_tough_final <- ggplot(property_stats_median_final, aes(x = flow, y = median_toughness, color = material, group = interaction(material, orientation))) +
  +     geom_line(aes(linetype = orientation), linewidth = 0.9) +
  +     geom_point(size = 2.5) +
  +     geom_errorbar(aes(ymin = q1_toughness, ymax = q3_toughness), width = 0.15) +
  +     scale_y_log10() +
  +     labs(x = "K (%)", y = "Toughness (MJ/m³) [log scale]",
             +          title = "Toughness vs K — Flat vs On-edge Orientation",
             +          subtitle = "Points = median; error bars = IQR (25th-75th percentile)",
             +          linetype = "Orientation", color = "Material") +
  +     theme_minimal()
> ggsave(file.path(final_dir, "toughness_vs_K_both_orientations.png"), p_tough_final, width = 9, height = 6, dpi = 300)
> # ---- CF-PETG, Flat (was SET-1) ----
> p_cf_flat_final <- representative_curves %>%
  +     filter(material == "CF PETG") %>%
  +     ggplot(aes(x = Strain*100, y = StressLightSmooth, color = flow)) +
  +     geom_line(linewidth = 0.9) +
  +     labs(x = "Strain (%)", y = "Stress (MPa)",
             +          title = "CF-PETG — Representative Stress-Strain Curves by K — Flat Orientation") +
  +     theme_minimal()
Error: object 'representative_curves' not found

> exists("all_break_data_set1")
[1] TRUE
> exists("representative_picks")
[1] FALSE
> representative_picks <- tribble(
  +     ~material, ~flow, ~sample,
  +     "CF PETG", "80",  "4",
  +     "CF PETG", "90",  "1",
  +     "CF PETG", "100", "5",
  +     "CF PETG", "110", "2",
  +     "PETG",    "80",  "5",
  +     "PETG",    "90",  "3",
  +     "PETG",    "100", "2",
  +     "PETG",    "110", "3"
  + ) %>%
  +     mutate(flow = factor(flow, levels = c("80", "90", "100", "110")))
> 
  > print(representative_picks)
# A tibble: 8 × 3
material flow  sample
<chr>    <fct> <chr> 
  1 CF PETG  80    4     
2 CF PETG  90    1     
3 CF PETG  100   5     
4 CF PETG  110   2     
5 PETG     80    5     
6 PETG     90    3     
7 PETG     100   2     
8 PETG     110   3     
> representative_curves <- all_break_data_set1 %>%
  +     inner_join(representative_picks, by = c("material", "flow", "sample")) %>%
  +     mutate(flow = factor(flow, levels = c("80", "90", "100", "110"))) %>%
  +     group_by(material, flow, sample) %>%
  +     arrange(Strain, .by_group = TRUE) %>%
  +     mutate(StressLightSmooth = zoo::rollmean(Stress, 7, fill = NA, align = "center")) %>%
  +     ungroup()
> 
  > nrow(representative_curves)
[1] 7663
> representative_curves_petg <- representative_curves %>%
  +     filter(material == "PETG") %>%
  +     mutate(
    +         mode_group = factor(
      +             if_else(flow %in% c("80", "90"), "Limited/brittle (80%, 90%)", "Extensive drawing (100%, 110%)"),
      +             levels = c("Limited/brittle (80%, 90%)", "Extensive drawing (100%, 110%)")
      +         )
    +     )
> p_cf_flat_final <- representative_curves %>%
  +     filter(material == "CF PETG") %>%
  +     ggplot(aes(x = Strain*100, y = StressLightSmooth, color = flow)) +
  +     geom_line(linewidth = 0.9) +
  +     labs(x = "Strain (%)", y = "Stress (MPa)",
             +          title = "CF-PETG — Representative Stress-Strain Curves by K — Flat Orientation") +
  +     theme_minimal()
> ggsave(file.path(final_dir, "representative_CF-PETG_flat_all_K.png"), p_cf_flat_final, width = 9, height = 6, dpi = 300)
Warning message:
  Removed 24 rows containing missing values or values outside the scale range (`geom_line()`). 
> 
  > p_petg_flat_final <- representative_curves_petg %>%
  +     ggplot(aes(x = Strain*100, y = StressLightSmooth, color = flow)) +
  +     geom_line(linewidth = 0.9) +
  +     facet_wrap(~mode_group, scales = "free_x") +
  +     labs(x = "Strain (%)", y = "Stress (MPa)",
             +          title = "PETG — Representative Stress-Strain Curves by K — Flat Orientation",
             +          subtitle = "Selected from individual specimens; split according to observed failure behaviour") +
  +     theme_minimal()
> ggsave(file.path(final_dir, "representative_PETG_flat_split_failure_mode.png"), p_petg_flat_final, width = 10, height = 6, dpi = 300)
Warning message:
  Removed 24 rows containing missing values or values outside the scale range (`geom_line()`). 
> 
  > p_cf_onedge_final <- representative_curves_set2 %>%
  +     filter(material == "CF PETG") %>%
  +     ggplot(aes(x = Strain*100, y = StressLightSmooth, color = flow)) +
  +     geom_line(linewidth = 0.9) +
  +     labs(x = "Strain (%)", y = "Stress (MPa)",
             +          title = "CF-PETG — Representative Stress-Strain Curves by K — On-edge Orientation") +
  +     theme_minimal()
> ggsave(file.path(final_dir, "representative_CF-PETG_onedge_all_K.png"), p_cf_onedge_final, width = 9, height = 6, dpi = 300)
Warning message:
  Removed 24 rows containing missing values or values outside the scale range (`geom_line()`). 
> 
  > p_petg_onedge_final <- representative_curves_set2_petg %>%
  +     ggplot(aes(x = Strain*100, y = StressLightSmooth, color = flow)) +
  +     geom_line(linewidth = 0.9) +
  +     facet_wrap(~mode_group, scales = "free_x") +
  +     labs(x = "Strain (%)", y = "Stress (MPa)",
             +          title = "PETG — Representative Stress-Strain Curves by K — On-edge Orientation",
             +          subtitle = "Selected from individual specimens; split according to observed failure behaviour") +
  +     theme_minimal()
> ggsave(file.path(final_dir, "representative_PETG_onedge_split_failure_mode.png"), p_petg_onedge_final, width = 10, height = 6, dpi = 300)
Warning message:
  Removed 24 rows containing missing values or values outside the scale range (`geom_line()`). 

> list.files(final_dir)
[1] "elongation_vs_K_both_orientations.png"            
[2] "modulus_vs_K_both_orientations.png"               
[3] "representative_CF-PETG_flat_all_K.png"            
[4] "representative_CF-PETG_onedge_all_K.png"          
[5] "representative_PETG_flat_split_failure_mode.png"  
[6] "representative_PETG_onedge_split_failure_mode.png"
[7] "toughness_vs_K_both_orientations.png"             
[8] "UTS_vs_K_both_orientations.png"                   
> p_cf_flat_final <- representative_curves %>%
  +     filter(material == "CF PETG") %>%
  +     ggplot(aes(x = Strain*100, y = StressLightSmooth, color = flow)) +
  +     geom_line(linewidth = 0.9) +
  +     labs(x = "Strain (%)", y = "Stress (MPa)", color = "K (%)",
             +          title = "CF-PETG — Representative Stress–Strain Curves by K — Flat Orientation") +
  +     theme_minimal()
> ggsave(file.path(final_dir, "representative_CF-PETG_flat_all_K.png"), p_cf_flat_final, width = 9, height = 6, dpi = 300)
Warning message:
  Removed 24 rows containing missing values or values outside the scale range (`geom_line()`). 
> 
  > p_petg_flat_final <- representative_curves_petg %>%
  +     ggplot(aes(x = Strain*100, y = StressLightSmooth, color = flow)) +
  +     geom_line(linewidth = 0.9) +
  +     facet_wrap(~mode_group, scales = "free_x") +
  +     labs(x = "Strain (%)", y = "Stress (MPa)", color = "K (%)",
             +          title = "PETG — Representative Stress–Strain Curves by K — Flat Orientation",
             +          subtitle = "Selected from individual specimens; split according to observed failure behaviour") +
  +     theme_minimal()
> ggsave(file.path(final_dir, "representative_PETG_flat_split_failure_mode.png"), p_petg_flat_final, width = 10, height = 6, dpi = 300)
Warning message:
  Removed 24 rows containing missing values or values outside the scale range (`geom_line()`). 
> 
  > p_cf_onedge_final <- representative_curves_set2 %>%
  +     filter(material == "CF PETG") %>%
  +     ggplot(aes(x = Strain*100, y = StressLightSmooth, color = flow)) +
  +     geom_line(linewidth = 0.9) +
  +     labs(x = "Strain (%)", y = "Stress (MPa)", color = "K (%)",
             +          title = "CF-PETG — Representative Stress–Strain Curves by K — On-edge Orientation") +
  +     theme_minimal()
> ggsave(file.path(final_dir, "representative_CF-PETG_onedge_all_K.png"), p_cf_onedge_final, width = 9, height = 6, dpi = 300)
Warning message:
  Removed 24 rows containing missing values or values outside the scale range (`geom_line()`). 
> 
  > p_petg_onedge_final <- representative_curves_set2_petg %>%
  +     ggplot(aes(x = Strain*100, y = StressLightSmooth, color = flow)) +
  +     geom_line(linewidth = 0.9) +
  +     facet_wrap(~mode_group, scales = "free_x") +
  +     labs(x = "Strain (%)", y = "Stress (MPa)", color = "K (%)",
             +          title = "PETG — Representative Stress–Strain Curves by K — On-edge Orientation",
             +          subtitle = "Selected from individual specimens; split according to observed failure behaviour") +
  +     theme_minimal()
> ggsave(file.path(final_dir, "representative_PETG_onedge_split_failure_mode.png"), p_petg_onedge_final, width = 10, height = 6, dpi = 300)
Warning message:
  Removed 24 rows containing missing values or values outside the scale range (`geom_line()`). 
