Beta regression between vaccine rate \~ accessibility + IMD decile +
ethnic composition
================

# read data

``` r
fca<- read.csv("accessibility_imd_ethnic_exclude.csv", sep=",")
```

``` r
#fca$MSOADECILE <- as.factor(fca$MSOADECILE)
fca$MSOADECILE <- factor(fca$MSOADECILE, levels = c(1,2,3,4,5,6,7,8,9,10))
```

# Import beta regression library

``` r
library(betareg)
```

    ## Warning: package 'betareg' was built under R version 4.0.5

``` r
head(fca)
```

    ##    MSOA11CD X2sfca_10 X2sfca_15 X2sfca_20 X2sfca_25 X2sfca_30 E2sfca_10
    ## 1 E02000984       5.8       5.3       7.4       7.4       7.0       4.4
    ## 2 E02000985       5.3       5.6       7.0       7.5       6.7       4.9
    ## 3 E02000986       4.7       7.7       8.1       7.7       7.1       5.3
    ## 4 E02000987       5.1       3.2       5.0       6.9       7.2       3.5
    ## 5 E02000988       4.8       8.1       8.3       7.8       7.2       6.3
    ## 6 E02000989       5.1       6.5       8.0       7.6       6.8       5.9
    ##   E2sfca_15 E2sfca_20 E2sfca_25 E2sfca_30 X3sfca_10 X3sfca_15 X3sfca_20
    ## 1       5.2       5.6       6.0       6.3       4.9       5.2       5.4
    ## 2       5.3       5.5       5.8       6.1       5.4       5.4       5.5
    ## 3       5.4       5.8       6.2       6.6       6.1       5.7       5.5
    ## 4       4.0       4.0       4.2       4.6       4.3       4.5       5.2
    ## 5       6.0       6.2       6.6       6.9       6.6       6.0       5.6
    ## 6       6.0       6.1       6.4       6.6       5.4       5.7       5.7
    ##   X3sfca_25 X3sfca_30 IMD19.SCORE MSOADECILE White. Mixed. Asian. Black. Other.
    ## 1       5.4       5.4    5.686451         10 0.9684 0.0104 0.0177 0.0013 0.0022
    ## 2       5.5       5.5    8.467447          9 0.9753 0.0098 0.0107 0.0029 0.0013
    ## 3       5.4       5.4   12.653141          8 0.8818 0.0130 0.0954 0.0053 0.0045
    ## 4       5.3       5.1   29.730829          3 0.9760 0.0097 0.0085 0.0032 0.0026
    ## 5       5.5       5.5   32.753429          2 0.7080 0.0205 0.2523 0.0156 0.0037
    ## 6       5.6       5.6   12.145260          8 0.9768 0.0089 0.0104 0.0024 0.0015
    ##   Per_cent_of_households_with_at_least_one_car_or_van
    ## 1                                           0.9421488
    ## 2                                           0.8761310
    ## 3                                           0.8370451
    ## 4                                           0.7128954
    ## 5                                           0.6787757
    ## 6                                           0.8641738
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   geometry
    ## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              POLYGON ((372121.740692092 414318.5820284747, 372147.1839180924 413616.0947827902, 371702.6454096416 413659.7824457224, 371827.7002863795 413133.9144642824, 371127.5921295869 413520.6338658539, 371044.1355199007 412457.7789452779, 370563.1300719351 412570.4830982488, 370344.1973272361 411735.6423399099, 368805.8519260563 413441.5264223354, 369273.03834101 414007.6864742129, 370082.7246238173 414033.0940174414, 369786.4439685926 414330.5955067142, 370439.1287112203 415133.4772392786, 370328.6379910178 416196.3783576144, 371095.1401160229 416704.8787693487, 371919.4464567718 415472.1040790394, 372121.740692092 414318.5820284747))
    ## 2                                                                                                                                                                                                                                                                                                          POLYGON ((372971.32544739 411456.0757330438, 373104.9660326273 411786.9346117839, 372403.4818300866 411682.3939734377, 372192.2923809371 411876.1650843857, 372549.9492235794 412191.2826706693, 372260.1359181411 412998.7853403708, 372506.840112354 413010.974232301, 372592.4822673436 413615.8473160079, 372503.9486068891 413391.7829738613, 372147.1839180924 413616.0947827902, 372121.740692092 414318.5820284747, 373292.3317096322 414365.1066220087, 373505.652913931 413431.1165834295, 374264.038610657 413116.3574516368, 373775.3448951351 412765.7052222025, 373778.1373174939 412458.7877736727, 373992.4521501801 412633.8743023995, 374145.9236210133 412356.4355328308, 373777.1377712048 412164.7864449316, 374003.8413448701 411866.1018520696, 373390.1380285973 411879.7839072304, 372971.32544739 411456.0757330438))
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                POLYGON ((372147.1839180924 413616.0947827902, 372503.9486068891 413391.7829738613, 372592.4822673436 413615.8473160079, 372506.840112354 413010.974232301, 372260.1359181411 412998.7853403708, 372549.9492235794 412191.2826706693, 372192.2923809371 411876.1650843857, 371567.5700009299 411759.8473628205, 371044.1355199007 412457.7789452779, 371127.5921295869 413520.6338658539, 371827.7002863795 413133.9144642824, 371702.6454096416 413659.7824457224, 372147.1839180924 413616.0947827902))
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              POLYGON ((363078.5560578937 411480.5286678746, 363041.920424702 411516.8692610519, 363593.9119929793 412513.5128240865, 364294.0656929562 412769.050901489, 364577.5179104541 411472.6705110331, 363933.33884928 411417.4733997922, 364071.0460017346 410956.528897219, 363645.4667881263 410864.7453871562, 363344.7899978039 411020.0870752722, 363498.5323147997 411509.986285847, 363078.5560578937 411480.5286678746))
    ## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                POLYGON ((371044.1355199007 412457.7789452779, 371567.5700009299 411759.8473628205, 372192.2923809371 411876.1650843857, 372403.4818300866 411682.3939734377, 372015.1375330461 411660.0904985466, 372346.6384834584 411171.0894849913, 371961.5443420812 411112.2768025493, 372068.6690501694 410687.1862619451, 371852.1392506702 410478.772534886, 371727.0995036518 410936.8941232641, 371489.1389071815 410639.7718630652, 371488.1994683027 411222.4107704869, 370664.1461853441 411366.8416224947, 370344.1973272361 411735.6423399099, 370563.1300719351 412570.4830982488, 371044.1355199007 412457.7789452779))
    ## 6 POLYGON ((375025.5385550009 414993.896433391, 374739.5421874931 414406.6108748912, 375207.5380997168 414012.9877312119, 375000.1361411166 413832.2031654804, 376080.2083505784 412704.2709914471, 375847.3883579105 411760.1339557605, 375722.6377981862 411119.6920298627, 375150.9981421491 410811.5879417898, 375204.6383989609 410110.1747155235, 374753.5930790966 410126.8584318775, 374751.4846462585 410701.1453159083, 374263.4197291153 410630.216265487, 374156.8351022587 410258.182940204, 374113.6400539136 410934.6881216902, 373684.3285153967 411054.2832090846, 372997.3308984101 411161.7825125497, 372971.32544739 411456.0757330438, 373390.1380285973 411879.7839072304, 374003.8413448701 411866.1018520696, 373777.1377712048 412164.7864449316, 374145.9236210133 412356.4355328308, 373992.4521501801 412633.8743023995, 373778.1373174939 412458.7877736727, 373775.3448951351 412765.7052222025, 374264.038610657 413116.3574516368, 373505.652913931 413431.1165834295, 373292.3317096322 414365.1066220087, 373561.6345520437 414304.2016083957, 373788.8537351935 415149.5173290095, 375025.5385550009 414993.896433391))
    ##   pop0_17 pop18over      MSOA X18over1st_dose X18over2nd_dose
    ## 1    1462      5998 E02000984            5896            5734
    ## 2    1328      5968 E02000985            5774            5600
    ## 3    1714      6758 E02000986            6087            5822
    ## 4    1251      5891 E02000987            5496            5209
    ## 5    2176      6749 E02000988            5998            5534
    ## 6    1388      6687 E02000989            6479            6260
    ##   X18over_2dose_total vaccination_percentage_1stdose
    ## 1               11630                      0.9829112
    ## 2               11374                      0.9674128
    ## 3               11909                      0.9006413
    ## 4               10705                      0.9328740
    ## 5               11532                      0.8886573
    ## 6               12739                      0.9688141
    ##   vaccination_percentage_2nddose vaccination_percentage_total
    ## 1                      0.9559068                    0.9694090
    ## 2                      0.9382623                    0.9528376
    ## 3                      0.8614352                    0.8810383
    ## 4                      0.8841640                    0.9085190
    ## 5                      0.8199182                    0.8542878
    ## 6                      0.9360697                    0.9524419

# 2SFCA

## 10 miles

``` r
scfa2_10 = betareg(vaccination_percentage_total ~ X2sfca_10 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
data = fca)
summary(scfa2_10)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_total ~ X2sfca_10 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7738 -0.6456 -0.2648  0.2019 14.5680 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                     Estimate Std. Error z value
    ## (Intercept)                                         -0.42338    0.08296  -5.103
    ## X2sfca_10                                            0.01635    0.00232   7.045
    ## MSOADECILE2                                         -0.12508    0.03394  -3.685
    ## MSOADECILE3                                         -0.13569    0.03833  -3.540
    ## MSOADECILE4                                         -0.19755    0.04118  -4.797
    ## MSOADECILE5                                         -0.18824    0.04410  -4.268
    ## MSOADECILE6                                         -0.12978    0.04671  -2.779
    ## MSOADECILE7                                          0.10097    0.05060   1.996
    ## MSOADECILE8                                          0.13195    0.05289   2.495
    ## MSOADECILE9                                          0.39575    0.05530   7.157
    ## MSOADECILE10                                         0.63360    0.05845  10.840
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.43358    0.12635  27.176
    ## Mixed.                                              -5.48597    0.95009  -5.774
    ## Asian.                                              -0.26972    0.08812  -3.061
    ## Black.                                               0.29326    0.37613   0.780
    ## Other.                                              -4.71399    1.00719  -4.680
    ##                                                     Pr(>|z|)    
    ## (Intercept)                                         3.34e-07 ***
    ## X2sfca_10                                           1.85e-12 ***
    ## MSOADECILE2                                         0.000229 ***
    ## MSOADECILE3                                         0.000400 ***
    ## MSOADECILE4                                         1.61e-06 ***
    ## MSOADECILE5                                         1.97e-05 ***
    ## MSOADECILE6                                         0.005458 ** 
    ## MSOADECILE7                                         0.045972 *  
    ## MSOADECILE8                                         0.012605 *  
    ## MSOADECILE9                                         8.25e-13 ***
    ## MSOADECILE10                                         < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  < 2e-16 ***
    ## Mixed.                                              7.73e-09 ***
    ## Asian.                                              0.002208 ** 
    ## Black.                                              0.435580    
    ## Other.                                              2.86e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.4290     0.3742   51.93   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  9145 on 17 Df
    ## Pseudo R-squared: 0.2885
    ## Number of iterations: 26 (BFGS) + 2 (Fisher scoring)

``` r
AIC(scfa2_10)
```

    ## [1] -18256.1

## 15 miles

``` r
scfa2_15 = betareg(vaccination_percentage_total ~ X2sfca_15 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
data = fca)
summary(scfa2_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_total ~ X2sfca_15 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7703 -0.6479 -0.2678  0.1962 14.6150 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.404349   0.083756
    ## X2sfca_15                                            0.015671   0.002881
    ## MSOADECILE2                                         -0.126277   0.034005
    ## MSOADECILE3                                         -0.134031   0.038401
    ## MSOADECILE4                                         -0.193543   0.041239
    ## MSOADECILE5                                         -0.187767   0.044169
    ## MSOADECILE6                                         -0.129431   0.046785
    ## MSOADECILE7                                          0.099704   0.050669
    ## MSOADECILE8                                          0.137696   0.052972
    ## MSOADECILE9                                          0.397367   0.055371
    ## MSOADECILE10                                         0.633492   0.058534
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.409131   0.126417
    ## Mixed.                                              -5.396676   0.952208
    ## Asian.                                              -0.260039   0.088486
    ## Black.                                               0.297904   0.376953
    ## Other.                                              -4.691105   1.009908
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -4.828 1.38e-06 ***
    ## X2sfca_15                                             5.440 5.33e-08 ***
    ## MSOADECILE2                                          -3.713 0.000204 ***
    ## MSOADECILE3                                          -3.490 0.000482 ***
    ## MSOADECILE4                                          -4.693 2.69e-06 ***
    ## MSOADECILE5                                          -4.251 2.13e-05 ***
    ## MSOADECILE6                                          -2.767 0.005666 ** 
    ## MSOADECILE7                                           1.968 0.049095 *  
    ## MSOADECILE8                                           2.599 0.009338 ** 
    ## MSOADECILE9                                           7.176 7.16e-13 ***
    ## MSOADECILE10                                         10.823  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  26.967  < 2e-16 ***
    ## Mixed.                                               -5.668 1.45e-08 ***
    ## Asian.                                               -2.939 0.003295 ** 
    ## Black.                                                0.790 0.429355    
    ## Other.                                               -4.645 3.40e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.3469     0.3726   51.92   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  9133 on 17 Df
    ## Pseudo R-squared: 0.2871
    ## Number of iterations: 27 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_15)
```

    ## [1] -18232.05

## 20 mmiles

``` r
scfa2_20 = betareg(vaccination_percentage_total ~ X2sfca_20 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
data = fca)
summary(scfa2_20)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_total ~ X2sfca_20 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7368 -0.6482 -0.2664  0.1934 14.4869 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.336385   0.084714
    ## X2sfca_20                                            0.007186   0.003206
    ## MSOADECILE2                                         -0.123036   0.034064
    ## MSOADECILE3                                         -0.130817   0.038487
    ## MSOADECILE4                                         -0.191486   0.041314
    ## MSOADECILE5                                         -0.185549   0.044233
    ## MSOADECILE6                                         -0.124977   0.046865
    ## MSOADECILE7                                          0.105789   0.050756
    ## MSOADECILE8                                          0.137480   0.053062
    ## MSOADECILE9                                          0.401684   0.055457
    ## MSOADECILE10                                         0.635420   0.058607
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.383082   0.126636
    ## Mixed.                                              -5.259676   0.953964
    ## Asian.                                              -0.241964   0.088605
    ## Black.                                               0.325579   0.377914
    ## Other.                                              -4.708849   1.011747
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -3.971 7.16e-05 ***
    ## X2sfca_20                                             2.241 0.025003 *  
    ## MSOADECILE2                                          -3.612 0.000304 ***
    ## MSOADECILE3                                          -3.399 0.000676 ***
    ## MSOADECILE4                                          -4.635 3.57e-06 ***
    ## MSOADECILE5                                          -4.195 2.73e-05 ***
    ## MSOADECILE6                                          -2.667 0.007659 ** 
    ## MSOADECILE7                                           2.084 0.037135 *  
    ## MSOADECILE8                                           2.591 0.009572 ** 
    ## MSOADECILE9                                           7.243 4.38e-13 ***
    ## MSOADECILE10                                         10.842  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  26.715  < 2e-16 ***
    ## Mixed.                                               -5.513 3.52e-08 ***
    ## Asian.                                               -2.731 0.006318 ** 
    ## Black.                                                0.862 0.388954    
    ## Other.                                               -4.654 3.25e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.2552     0.3709   51.92   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  9120 on 17 Df
    ## Pseudo R-squared: 0.286
    ## Number of iterations: 27 (BFGS) + 4 (Fisher scoring)

``` r
AIC(scfa2_20)
```

    ## [1] -18206.8

## 25 miles

``` r
scfa2_25 = betareg(vaccination_percentage_total ~ X2sfca_25 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
data = fca)
summary(scfa2_25)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_total ~ X2sfca_25 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7236 -0.6485 -0.2643  0.1936 14.3701 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.261614   0.084725
    ## X2sfca_25                                           -0.001617   0.003380
    ## MSOADECILE2                                         -0.121899   0.034075
    ## MSOADECILE3                                         -0.127509   0.038508
    ## MSOADECILE4                                         -0.190445   0.041325
    ## MSOADECILE5                                         -0.182128   0.044249
    ## MSOADECILE6                                         -0.120684   0.046884
    ## MSOADECILE7                                          0.111181   0.050778
    ## MSOADECILE8                                          0.136718   0.053075
    ## MSOADECILE9                                          0.405599   0.055477
    ## MSOADECILE10                                         0.637944   0.058622
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.354510   0.126561
    ## Mixed.                                              -5.175010   0.953751
    ## Asian.                                              -0.231835   0.088725
    ## Black.                                               0.346560   0.378671
    ## Other.                                              -4.783793   1.012312
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -3.088 0.002017 ** 
    ## X2sfca_25                                            -0.478 0.632295    
    ## MSOADECILE2                                          -3.577 0.000347 ***
    ## MSOADECILE3                                          -3.311 0.000929 ***
    ## MSOADECILE4                                          -4.608 4.06e-06 ***
    ## MSOADECILE5                                          -4.116 3.86e-05 ***
    ## MSOADECILE6                                          -2.574 0.010050 *  
    ## MSOADECILE7                                           2.190 0.028559 *  
    ## MSOADECILE8                                           2.576 0.009996 ** 
    ## MSOADECILE9                                           7.311 2.65e-13 ***
    ## MSOADECILE10                                         10.882  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  26.505  < 2e-16 ***
    ## Mixed.                                               -5.426 5.76e-08 ***
    ## Asian.                                               -2.613 0.008976 ** 
    ## Black.                                                0.915 0.360086    
    ## Other.                                               -4.726 2.29e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.2388     0.3706   51.91   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  9118 on 17 Df
    ## Pseudo R-squared: 0.286
    ## Number of iterations: 28 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_25)
```

    ## [1] -18202.03

## 30 miles

``` r
scfa2_30 = betareg(vaccination_percentage_total ~ X2sfca_30 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,data = fca)
summary(scfa2_30)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_total ~ X2sfca_30 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7167 -0.6471 -0.2643  0.1942 14.3459 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.224810   0.084769
    ## X2sfca_30                                           -0.006209   0.003446
    ## MSOADECILE2                                         -0.120598   0.034068
    ## MSOADECILE3                                         -0.124312   0.038522
    ## MSOADECILE4                                         -0.188170   0.041325
    ## MSOADECILE5                                         -0.179365   0.044253
    ## MSOADECILE6                                         -0.118039   0.046878
    ## MSOADECILE7                                          0.114372   0.050785
    ## MSOADECILE8                                          0.137443   0.053057
    ## MSOADECILE9                                          0.408103   0.055473
    ## MSOADECILE10                                         0.640516   0.058618
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.342829   0.126540
    ## Mixed.                                              -5.199271   0.953362
    ## Asian.                                              -0.222055   0.088793
    ## Black.                                               0.366476   0.378795
    ## Other.                                              -4.851454   1.012231
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -2.652  0.00800 ** 
    ## X2sfca_30                                            -1.802  0.07159 .  
    ## MSOADECILE2                                          -3.540  0.00040 ***
    ## MSOADECILE3                                          -3.227  0.00125 ** 
    ## MSOADECILE4                                          -4.553 5.28e-06 ***
    ## MSOADECILE5                                          -4.053 5.05e-05 ***
    ## MSOADECILE6                                          -2.518  0.01180 *  
    ## MSOADECILE7                                           2.252  0.02432 *  
    ## MSOADECILE8                                           2.590  0.00958 ** 
    ## MSOADECILE9                                           7.357 1.88e-13 ***
    ## MSOADECILE10                                         10.927  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  26.417  < 2e-16 ***
    ## Mixed.                                               -5.454 4.94e-08 ***
    ## Asian.                                               -2.501  0.01239 *  
    ## Black.                                                0.967  0.33331    
    ## Other.                                               -4.793 1.64e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.2500     0.3708   51.91   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  9119 on 17 Df
    ## Pseudo R-squared: 0.2862
    ## Number of iterations: 29 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_30)
```

    ## [1] -18204.95

# E2SFCA

## 10 miles

``` r
E2scfa_10 = betareg(vaccination_percentage_total ~ E2sfca_10 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,data = fca)
summary(E2scfa_10)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_total ~ E2sfca_10 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7413 -0.6453 -0.2622  0.2011 14.4480 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.395048   0.084726
    ## E2sfca_10                                            0.008807   0.001984
    ## MSOADECILE2                                         -0.131345   0.034079
    ## MSOADECILE3                                         -0.144555   0.038534
    ## MSOADECILE4                                         -0.209113   0.041404
    ## MSOADECILE5                                         -0.204808   0.044440
    ## MSOADECILE6                                         -0.142348   0.046973
    ## MSOADECILE7                                          0.088625   0.050891
    ## MSOADECILE8                                          0.113529   0.053197
    ## MSOADECILE9                                          0.383762   0.055576
    ## MSOADECILE10                                         0.616774   0.058714
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.472720   0.128475
    ## Mixed.                                              -5.413843   0.952779
    ## Asian.                                              -0.282199   0.088740
    ## Black.                                               0.332415   0.377370
    ## Other.                                              -4.684517   1.009825
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -4.663 3.12e-06 ***
    ## E2sfca_10                                             4.440 9.01e-06 ***
    ## MSOADECILE2                                          -3.854 0.000116 ***
    ## MSOADECILE3                                          -3.751 0.000176 ***
    ## MSOADECILE4                                          -5.051 4.40e-07 ***
    ## MSOADECILE5                                          -4.609 4.05e-06 ***
    ## MSOADECILE6                                          -3.030 0.002442 ** 
    ## MSOADECILE7                                           1.741 0.081601 .  
    ## MSOADECILE8                                           2.134 0.032831 *  
    ## MSOADECILE9                                           6.905 5.01e-12 ***
    ## MSOADECILE10                                         10.505  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  27.030  < 2e-16 ***
    ## Mixed.                                               -5.682 1.33e-08 ***
    ## Asian.                                               -3.180 0.001472 ** 
    ## Black.                                                0.881 0.378386    
    ## Other.                                               -4.639 3.50e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.3097     0.3719   51.92   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  9128 on 17 Df
    ## Pseudo R-squared: 0.286
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(E2scfa_10)
```

    ## [1] -18221.1

## 15 miles

``` r
E2scfa_15 = betareg(vaccination_percentage_total ~ E2sfca_15 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other., data = fca)
summary(E2scfa_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_total ~ E2sfca_15 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7638 -0.6447 -0.2608  0.2009 14.5353 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.463648   0.085057
    ## E2sfca_15                                            0.016203   0.002443
    ## MSOADECILE2                                         -0.133586   0.034001
    ## MSOADECILE3                                         -0.150025   0.038422
    ## MSOADECILE4                                         -0.212979   0.041292
    ## MSOADECILE5                                         -0.209058   0.044292
    ## MSOADECILE6                                         -0.147415   0.046840
    ## MSOADECILE7                                          0.081991   0.050744
    ## MSOADECILE8                                          0.111595   0.053010
    ## MSOADECILE9                                          0.381227   0.055434
    ## MSOADECILE10                                         0.614590   0.058572
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.510664   0.127789
    ## Mixed.                                              -5.624374   0.951275
    ## Asian.                                              -0.296817   0.088426
    ## Black.                                               0.312785   0.376405
    ## Other.                                              -4.643081   1.008174
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -5.451 5.01e-08 ***
    ## E2sfca_15                                             6.633 3.29e-11 ***
    ## MSOADECILE2                                          -3.929 8.53e-05 ***
    ## MSOADECILE3                                          -3.905 9.44e-05 ***
    ## MSOADECILE4                                          -5.158 2.50e-07 ***
    ## MSOADECILE5                                          -4.720 2.36e-06 ***
    ## MSOADECILE6                                          -3.147 0.001648 ** 
    ## MSOADECILE7                                           1.616 0.106142    
    ## MSOADECILE8                                           2.105 0.035277 *  
    ## MSOADECILE9                                           6.877 6.11e-12 ***
    ## MSOADECILE10                                         10.493  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  27.472  < 2e-16 ***
    ## Mixed.                                               -5.912 3.37e-09 ***
    ## Asian.                                               -3.357 0.000789 ***
    ## Black.                                                0.831 0.405984    
    ## Other.                                               -4.605 4.12e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.4022     0.3737   51.93   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  9140 on 17 Df
    ## Pseudo R-squared: 0.287
    ## Number of iterations: 26 (BFGS) + 2 (Fisher scoring)

``` r
AIC(E2scfa_15)
```

    ## [1] -18246.74

## 20 miles

``` r
E2scfa_20 = betareg(vaccination_percentage_total ~ E2sfca_20 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other., data = fca)
summary(E2scfa_20)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_total ~ E2sfca_20 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7815 -0.6432 -0.2654  0.2062 14.5985 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.501813   0.085345
    ## E2sfca_20                                            0.021224   0.002784
    ## MSOADECILE2                                         -0.134299   0.033955
    ## MSOADECILE3                                         -0.151246   0.038355
    ## MSOADECILE4                                         -0.211496   0.041213
    ## MSOADECILE5                                         -0.207461   0.044187
    ## MSOADECILE6                                         -0.148629   0.046761
    ## MSOADECILE7                                          0.080060   0.050647
    ## MSOADECILE8                                          0.115365   0.052907
    ## MSOADECILE9                                          0.380117   0.055349
    ## MSOADECILE10                                         0.615182   0.058492
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.520969   0.127348
    ## Mixed.                                              -5.739981   0.950559
    ## Asian.                                              -0.299964   0.088317
    ## Black.                                               0.305186   0.375799
    ## Other.                                              -4.633233   1.007486
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -5.880 4.11e-09 ***
    ## E2sfca_20                                             7.623 2.49e-14 ***
    ## MSOADECILE2                                          -3.955 7.65e-05 ***
    ## MSOADECILE3                                          -3.943 8.04e-05 ***
    ## MSOADECILE4                                          -5.132 2.87e-07 ***
    ## MSOADECILE5                                          -4.695 2.67e-06 ***
    ## MSOADECILE6                                          -3.179 0.001480 ** 
    ## MSOADECILE7                                           1.581 0.113938    
    ## MSOADECILE8                                           2.181 0.029219 *  
    ## MSOADECILE9                                           6.868 6.53e-12 ***
    ## MSOADECILE10                                         10.517  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  27.648  < 2e-16 ***
    ## Mixed.                                               -6.039 1.56e-09 ***
    ## Asian.                                               -3.396 0.000683 ***
    ## Black.                                                0.812 0.416736    
    ## Other.                                               -4.599 4.25e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.4569     0.3747   51.93   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  9148 on 17 Df
    ## Pseudo R-squared: 0.2878
    ## Number of iterations: 27 (BFGS) + 4 (Fisher scoring)

``` r
AIC(E2scfa_20)
```

    ## [1] -18262.29

## 25 miles

``` r
E2scfa_25 = betareg(vaccination_percentage_total ~ E2sfca_25 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other., data = fca)
summary(E2scfa_25)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_total ~ E2sfca_25 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7808 -0.6432 -0.2658  0.2055 14.6115 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.497740   0.085674
    ## E2sfca_25                                            0.021876   0.003034
    ## MSOADECILE2                                         -0.132816   0.033964
    ## MSOADECILE3                                         -0.148242   0.038362
    ## MSOADECILE4                                         -0.206747   0.041204
    ## MSOADECILE5                                         -0.202428   0.044164
    ## MSOADECILE6                                         -0.145343   0.046756
    ## MSOADECILE7                                          0.083873   0.050634
    ## MSOADECILE8                                          0.121730   0.052904
    ## MSOADECILE9                                          0.382703   0.055344
    ## MSOADECILE10                                         0.618564   0.058491
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.504064   0.127175
    ## Mixed.                                              -5.708856   0.951305
    ## Asian.                                              -0.290827   0.088382
    ## Black.                                               0.305976   0.375962
    ## Other.                                              -4.636984   1.008229
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -5.810 6.26e-09 ***
    ## E2sfca_25                                             7.211 5.56e-13 ***
    ## MSOADECILE2                                          -3.911 9.21e-05 ***
    ## MSOADECILE3                                          -3.864 0.000111 ***
    ## MSOADECILE4                                          -5.018 5.23e-07 ***
    ## MSOADECILE5                                          -4.584 4.57e-06 ***
    ## MSOADECILE6                                          -3.109 0.001880 ** 
    ## MSOADECILE7                                           1.656 0.097629 .  
    ## MSOADECILE8                                           2.301 0.021393 *  
    ## MSOADECILE9                                           6.915 4.68e-12 ***
    ## MSOADECILE10                                         10.575  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  27.553  < 2e-16 ***
    ## Mixed.                                               -6.001 1.96e-09 ***
    ## Asian.                                               -3.291 0.001000 ***
    ## Black.                                                0.814 0.415733    
    ## Other.                                               -4.599 4.24e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.4339     0.3743   51.93   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  9145 on 17 Df
    ## Pseudo R-squared: 0.2877
    ## Number of iterations: 27 (BFGS) + 3 (Fisher scoring)

``` r
AIC(E2scfa_25)
```

    ## [1] -18256.03

## 30 miles

``` r
E2scfa_30 = betareg(vaccination_percentage_total ~ E2sfca_30 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other., data = fca)
summary(E2scfa_30)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_total ~ E2sfca_30 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7725 -0.6443 -0.2655  0.1994 14.5940 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.464280   0.085900
    ## E2sfca_30                                            0.019334   0.003213
    ## MSOADECILE2                                         -0.130485   0.034001
    ## MSOADECILE3                                         -0.143615   0.038407
    ## MSOADECILE4                                         -0.201736   0.041237
    ## MSOADECILE5                                         -0.196938   0.044187
    ## MSOADECILE6                                         -0.139652   0.046793
    ## MSOADECILE7                                          0.090445   0.050671
    ## MSOADECILE8                                          0.127761   0.052951
    ## MSOADECILE9                                          0.387978   0.055381
    ## MSOADECILE10                                         0.623212   0.058530
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.471801   0.127110
    ## Mixed.                                              -5.589726   0.952623
    ## Asian.                                              -0.277197   0.088513
    ## Black.                                               0.306290   0.376573
    ## Other.                                              -4.644723   1.009547
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -5.405 6.48e-08 ***
    ## E2sfca_30                                             6.017 1.78e-09 ***
    ## MSOADECILE2                                          -3.838 0.000124 ***
    ## MSOADECILE3                                          -3.739 0.000185 ***
    ## MSOADECILE4                                          -4.892 9.98e-07 ***
    ## MSOADECILE5                                          -4.457 8.32e-06 ***
    ## MSOADECILE6                                          -2.984 0.002841 ** 
    ## MSOADECILE7                                           1.785 0.074273 .  
    ## MSOADECILE8                                           2.413 0.015830 *  
    ## MSOADECILE9                                           7.006 2.46e-12 ***
    ## MSOADECILE10                                         10.648  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  27.313  < 2e-16 ***
    ## Mixed.                                               -5.868 4.42e-09 ***
    ## Asian.                                               -3.132 0.001738 ** 
    ## Black.                                                0.813 0.416011    
    ## Other.                                               -4.601 4.21e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.3731     0.3731   51.92   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  9137 on 17 Df
    ## Pseudo R-squared: 0.287
    ## Number of iterations: 27 (BFGS) + 3 (Fisher scoring)

``` r
AIC(E2scfa_30)
```

    ## [1] -18239.26

# 3SFCA

## 10 miles

``` r
scfa3_10 = betareg(vaccination_percentage_total ~ X3sfca_10 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
data = fca)
summary(scfa3_10)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_total ~ X3sfca_10 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7401 -0.6442 -0.2625  0.1937 14.4342 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.374485   0.083426
    ## X3sfca_10                                            0.008152   0.001867
    ## MSOADECILE2                                         -0.128624   0.034048
    ## MSOADECILE3                                         -0.139322   0.038478
    ## MSOADECILE4                                         -0.204014   0.041324
    ## MSOADECILE5                                         -0.197971   0.044300
    ## MSOADECILE6                                         -0.136152   0.046868
    ## MSOADECILE7                                          0.095453   0.050788
    ## MSOADECILE8                                          0.119674   0.053106
    ## MSOADECILE9                                          0.388013   0.055475
    ## MSOADECILE10                                         0.622495   0.058640
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.441876   0.127347
    ## Mixed.                                              -5.346069   0.952538
    ## Asian.                                              -0.270476   0.088550
    ## Black.                                               0.358571   0.377372
    ## Other.                                              -4.648872   1.009794
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -4.489 7.16e-06 ***
    ## X3sfca_10                                             4.367 1.26e-05 ***
    ## MSOADECILE2                                          -3.778 0.000158 ***
    ## MSOADECILE3                                          -3.621 0.000294 ***
    ## MSOADECILE4                                          -4.937 7.93e-07 ***
    ## MSOADECILE5                                          -4.469 7.86e-06 ***
    ## MSOADECILE6                                          -2.905 0.003672 ** 
    ## MSOADECILE7                                           1.879 0.060184 .  
    ## MSOADECILE8                                           2.253 0.024228 *  
    ## MSOADECILE9                                           6.994 2.66e-12 ***
    ## MSOADECILE10                                         10.616  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  27.028  < 2e-16 ***
    ## Mixed.                                               -5.612 1.99e-08 ***
    ## Asian.                                               -3.055 0.002254 ** 
    ## Black.                                                0.950 0.342021    
    ## Other.                                               -4.604 4.15e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.3090     0.3719   51.92   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  9127 on 17 Df
    ## Pseudo R-squared: 0.2859
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa3_10)
```

    ## [1] -18220.06

## 15 miles

``` r
scfa3_15 = betareg(vaccination_percentage_total ~ X3sfca_15 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa3_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_total ~ X3sfca_15 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7442 -0.6479 -0.2603  0.1988 14.4705 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.407439   0.083950
    ## X3sfca_15                                            0.012505   0.002329
    ## MSOADECILE2                                         -0.129242   0.034018
    ## MSOADECILE3                                         -0.142176   0.038434
    ## MSOADECILE4                                         -0.205801   0.041291
    ## MSOADECILE5                                         -0.201027   0.044277
    ## MSOADECILE6                                         -0.139383   0.046833
    ## MSOADECILE7                                          0.091372   0.050751
    ## MSOADECILE8                                          0.118485   0.053038
    ## MSOADECILE9                                          0.388324   0.055433
    ## MSOADECILE10                                         0.621623   0.058591
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.452332   0.127077
    ## Mixed.                                              -5.418397   0.951971
    ## Asian.                                              -0.275956   0.088387
    ## Black.                                               0.343674   0.376913
    ## Other.                                              -4.552713   1.009221
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -4.853 1.21e-06 ***
    ## X3sfca_15                                             5.370 7.87e-08 ***
    ## MSOADECILE2                                          -3.799 0.000145 ***
    ## MSOADECILE3                                          -3.699 0.000216 ***
    ## MSOADECILE4                                          -4.984 6.22e-07 ***
    ## MSOADECILE5                                          -4.540 5.62e-06 ***
    ## MSOADECILE6                                          -2.976 0.002919 ** 
    ## MSOADECILE7                                           1.800 0.071795 .  
    ## MSOADECILE8                                           2.234 0.025486 *  
    ## MSOADECILE9                                           7.005 2.47e-12 ***
    ## MSOADECILE10                                         10.609  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  27.167  < 2e-16 ***
    ## Mixed.                                               -5.692 1.26e-08 ***
    ## Asian.                                               -3.122 0.001795 ** 
    ## Black.                                                0.912 0.361867    
    ## Other.                                               -4.511 6.45e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.3500     0.3727   51.92   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  9132 on 17 Df
    ## Pseudo R-squared: 0.2863
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa3_15)
```

    ## [1] -18230.76

## 20 miles

``` r
scfa3_20 = betareg(vaccination_percentage_total ~ X3sfca_20 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa3_20)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_total ~ X3sfca_20 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7501 -0.6485 -0.2626  0.1963 14.5161 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.434621   0.084333
    ## X3sfca_20                                            0.016321   0.002672
    ## MSOADECILE2                                         -0.129119   0.033988
    ## MSOADECILE3                                         -0.142735   0.038393
    ## MSOADECILE4                                         -0.205655   0.041255
    ## MSOADECILE5                                         -0.201189   0.044229
    ## MSOADECILE6                                         -0.139833   0.046789
    ## MSOADECILE7                                          0.089498   0.050700
    ## MSOADECILE8                                          0.120519   0.052966
    ## MSOADECILE9                                          0.390948   0.055380
    ## MSOADECILE10                                         0.623601   0.058535
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.455683   0.126811
    ## Mixed.                                              -5.434800   0.951360
    ## Asian.                                              -0.276626   0.088253
    ## Black.                                               0.325735   0.376459
    ## Other.                                              -4.500740   1.008579
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -5.154 2.55e-07 ***
    ## X3sfca_20                                             6.108 1.01e-09 ***
    ## MSOADECILE2                                          -3.799 0.000145 ***
    ## MSOADECILE3                                          -3.718 0.000201 ***
    ## MSOADECILE4                                          -4.985 6.20e-07 ***
    ## MSOADECILE5                                          -4.549 5.39e-06 ***
    ## MSOADECILE6                                          -2.989 0.002802 ** 
    ## MSOADECILE7                                           1.765 0.077520 .  
    ## MSOADECILE8                                           2.275 0.022883 *  
    ## MSOADECILE9                                           7.059 1.67e-12 ***
    ## MSOADECILE10                                         10.653  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  27.251  < 2e-16 ***
    ## Mixed.                                               -5.713 1.11e-08 ***
    ## Asian.                                               -3.134 0.001722 ** 
    ## Black.                                                0.865 0.386895    
    ## Other.                                               -4.462 8.10e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.3854     0.3734   51.92   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  9137 on 17 Df
    ## Pseudo R-squared: 0.2868
    ## Number of iterations: 28 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa3_20)
```

    ## [1] -18240.53

## 25 miles

``` r
scfa3_25 = betareg(vaccination_percentage_total ~ X3sfca_25 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa3_25)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_total ~ X3sfca_25 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7602 -0.6454 -0.2660  0.1997 14.5593 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.466937   0.084616
    ## X3sfca_25                                            0.020647   0.002944
    ## MSOADECILE2                                         -0.129813   0.033951
    ## MSOADECILE3                                         -0.143922   0.038346
    ## MSOADECILE4                                         -0.205383   0.041205
    ## MSOADECILE5                                         -0.200982   0.044166
    ## MSOADECILE6                                         -0.140576   0.046733
    ## MSOADECILE7                                          0.087583   0.050630
    ## MSOADECILE8                                          0.123021   0.052896
    ## MSOADECILE9                                          0.392024   0.055318
    ## MSOADECILE10                                         0.625709   0.058476
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.461435   0.126548
    ## Mixed.                                              -5.420276   0.950419
    ## Asian.                                              -0.279091   0.088127
    ## Black.                                               0.302934   0.375811
    ## Other.                                              -4.455387   1.007531
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -5.518 3.42e-08 ***
    ## X3sfca_25                                             7.013 2.33e-12 ***
    ## MSOADECILE2                                          -3.824 0.000132 ***
    ## MSOADECILE3                                          -3.753 0.000175 ***
    ## MSOADECILE4                                          -4.984 6.22e-07 ***
    ## MSOADECILE5                                          -4.551 5.35e-06 ***
    ## MSOADECILE6                                          -3.008 0.002629 ** 
    ## MSOADECILE7                                           1.730 0.083654 .  
    ## MSOADECILE8                                           2.326 0.020034 *  
    ## MSOADECILE9                                           7.087 1.37e-12 ***
    ## MSOADECILE10                                         10.700  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  27.353  < 2e-16 ***
    ## Mixed.                                               -5.703 1.18e-08 ***
    ## Asian.                                               -3.167 0.001541 ** 
    ## Black.                                                0.806 0.420196    
    ## Other.                                               -4.422 9.78e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.4339     0.3743   51.92   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  9144 on 17 Df
    ## Pseudo R-squared: 0.2875
    ## Number of iterations: 28 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa3_25)
```

    ## [1] -18253.75

## 30 miles

``` r
scfa3_30 = betareg(vaccination_percentage_total ~ X3sfca_30 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa3_30)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_total ~ X3sfca_30 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7697 -0.6437 -0.2669  0.2045 14.6177 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.499586   0.084941
    ## X3sfca_30                                            0.024706   0.003155
    ## MSOADECILE2                                         -0.130910   0.033915
    ## MSOADECILE3                                         -0.145435   0.038301
    ## MSOADECILE4                                         -0.205078   0.041156
    ## MSOADECILE5                                         -0.200752   0.044108
    ## MSOADECILE6                                         -0.142009   0.046681
    ## MSOADECILE7                                          0.085675   0.050564
    ## MSOADECILE8                                          0.124973   0.052838
    ## MSOADECILE9                                          0.391191   0.055260
    ## MSOADECILE10                                         0.627026   0.058426
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.470431   0.126372
    ## Mixed.                                              -5.407483   0.949416
    ## Asian.                                              -0.280930   0.088015
    ## Black.                                               0.290855   0.375105
    ## Other.                                              -4.427186   1.006418
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -5.882 4.06e-09 ***
    ## X3sfca_30                                             7.832 4.82e-15 ***
    ## MSOADECILE2                                          -3.860 0.000113 ***
    ## MSOADECILE3                                          -3.797 0.000146 ***
    ## MSOADECILE4                                          -4.983 6.26e-07 ***
    ## MSOADECILE5                                          -4.551 5.33e-06 ***
    ## MSOADECILE6                                          -3.042 0.002349 ** 
    ## MSOADECILE7                                           1.694 0.090193 .  
    ## MSOADECILE8                                           2.365 0.018020 *  
    ## MSOADECILE9                                           7.079 1.45e-12 ***
    ## MSOADECILE10                                         10.732  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  27.462  < 2e-16 ***
    ## Mixed.                                               -5.696 1.23e-08 ***
    ## Asian.                                               -3.192 0.001414 ** 
    ## Black.                                                0.775 0.438104    
    ## Other.                                               -4.399 1.09e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.4843     0.3752   51.93   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  9151 on 17 Df
    ## Pseudo R-squared: 0.2882
    ## Number of iterations: 27 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa3_30)
```

    ## [1] -18267.38
