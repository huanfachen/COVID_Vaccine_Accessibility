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
scfa2_10 = betareg(vaccination_percentage_2nddose ~ X2sfca_10 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
data = fca)
summary(scfa2_10)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_2nddose ~ X2sfca_10 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.5189 -0.6084 -0.2152  0.2404 17.3592 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.573432   0.075741
    ## X2sfca_10                                            0.014325   0.002104
    ## MSOADECILE2                                         -0.137465   0.030909
    ## MSOADECILE3                                         -0.157051   0.034854
    ## MSOADECILE4                                         -0.202765   0.037523
    ## MSOADECILE5                                         -0.202615   0.040149
    ## MSOADECILE6                                         -0.170685   0.042438
    ## MSOADECILE7                                          0.023723   0.045876
    ## MSOADECILE8                                          0.070349   0.048096
    ## MSOADECILE9                                          0.295420   0.050252
    ## MSOADECILE10                                         0.521969   0.053366
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.437996   0.115346
    ## Mixed.                                              -5.758641   0.864079
    ## Asian.                                              -0.376306   0.079847
    ## Black.                                              -0.049680   0.342434
    ## Other.                                              -4.411954   0.935326
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -7.571 3.71e-14 ***
    ## X2sfca_10                                             6.809 9.84e-12 ***
    ## MSOADECILE2                                          -4.447 8.69e-06 ***
    ## MSOADECILE3                                          -4.506 6.61e-06 ***
    ## MSOADECILE4                                          -5.404 6.53e-08 ***
    ## MSOADECILE5                                          -5.047 4.50e-07 ***
    ## MSOADECILE6                                          -4.022 5.77e-05 ***
    ## MSOADECILE7                                           0.517    0.605    
    ## MSOADECILE8                                           1.463    0.144    
    ## MSOADECILE9                                           5.879 4.13e-09 ***
    ## MSOADECILE10                                          9.781  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  29.806  < 2e-16 ***
    ## Mixed.                                               -6.664 2.66e-11 ***
    ## Asian.                                               -4.713 2.44e-06 ***
    ## Black.                                               -0.145    0.885    
    ## Other.                                               -4.717 2.39e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  21.6870     0.4097   52.94   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  8480 on 17 Df
    ## Pseudo R-squared: 0.3156
    ## Number of iterations: 27 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_10)
```

    ## [1] -16925.3

## 15 miles

``` r
scfa2_15 = betareg(vaccination_percentage_2nddose ~ X2sfca_15 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
data = fca)
summary(scfa2_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_2nddose ~ X2sfca_15 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.5118 -0.6084 -0.2168  0.2375 17.4024 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.557303   0.076430
    ## X2sfca_15                                            0.014070   0.002613
    ## MSOADECILE2                                         -0.137441   0.030954
    ## MSOADECILE3                                         -0.155119   0.034902
    ## MSOADECILE4                                         -0.197522   0.037566
    ## MSOADECILE5                                         -0.200733   0.040196
    ## MSOADECILE6                                         -0.168526   0.042495
    ## MSOADECILE7                                          0.024271   0.045929
    ## MSOADECILE8                                          0.077061   0.048154
    ## MSOADECILE9                                          0.299903   0.050316
    ## MSOADECILE10                                         0.524996   0.053437
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.413021   0.115377
    ## Mixed.                                              -5.692049   0.865694
    ## Asian.                                              -0.368892   0.080121
    ## Black.                                              -0.051313   0.343002
    ## Other.                                              -4.378009   0.937352
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -7.292 3.06e-13 ***
    ## X2sfca_15                                             5.385 7.25e-08 ***
    ## MSOADECILE2                                          -4.440 8.99e-06 ***
    ## MSOADECILE3                                          -4.444 8.81e-06 ***
    ## MSOADECILE4                                          -5.258 1.46e-07 ***
    ## MSOADECILE5                                          -4.994 5.92e-07 ***
    ## MSOADECILE6                                          -3.966 7.32e-05 ***
    ## MSOADECILE7                                           0.528    0.597    
    ## MSOADECILE8                                           1.600    0.110    
    ## MSOADECILE9                                           5.960 2.52e-09 ***
    ## MSOADECILE10                                          9.825  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  29.581  < 2e-16 ***
    ## Mixed.                                               -6.575 4.86e-11 ***
    ## Asian.                                               -4.604 4.14e-06 ***
    ## Black.                                               -0.150    0.881    
    ## Other.                                               -4.671 3.00e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  21.6110     0.4082   52.94   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  8469 on 17 Df
    ## Pseudo R-squared: 0.3143
    ## Number of iterations: 28 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_15)
```

    ## [1] -16904.49

## 20 mmiles

``` r
scfa2_20 = betareg(vaccination_percentage_2nddose ~ X2sfca_20 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
data = fca)
summary(scfa2_20)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_2nddose ~ X2sfca_20 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.4823 -0.6067 -0.2134  0.2417 17.2962 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.502628   0.077279
    ## X2sfca_20                                            0.007325   0.002907
    ## MSOADECILE2                                         -0.134606   0.030998
    ## MSOADECILE3                                         -0.153209   0.034964
    ## MSOADECILE4                                         -0.195584   0.037624
    ## MSOADECILE5                                         -0.198322   0.040248
    ## MSOADECILE6                                         -0.164816   0.042558
    ## MSOADECILE7                                          0.029537   0.045997
    ## MSOADECILE8                                          0.077677   0.048228
    ## MSOADECILE9                                          0.305237   0.050392
    ## MSOADECILE10                                         0.528897   0.053504
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.391152   0.115551
    ## Mixed.                                              -5.598438   0.866906
    ## Asian.                                              -0.352772   0.080202
    ## Black.                                              -0.035674   0.343528
    ## Other.                                              -4.358337   0.938671
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -6.504 7.82e-11 ***
    ## X2sfca_20                                             2.520 0.011747 *  
    ## MSOADECILE2                                          -4.342 1.41e-05 ***
    ## MSOADECILE3                                          -4.382 1.18e-05 ***
    ## MSOADECILE4                                          -5.198 2.01e-07 ***
    ## MSOADECILE5                                          -4.928 8.33e-07 ***
    ## MSOADECILE6                                          -3.873 0.000108 ***
    ## MSOADECILE7                                           0.642 0.520777    
    ## MSOADECILE8                                           1.611 0.107263    
    ## MSOADECILE9                                           6.057 1.38e-09 ***
    ## MSOADECILE10                                          9.885  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  29.348  < 2e-16 ***
    ## Mixed.                                               -6.458 1.06e-10 ***
    ## Asian.                                               -4.399 1.09e-05 ***
    ## Black.                                               -0.104 0.917291    
    ## Other.                                               -4.643 3.43e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  21.5253     0.4067   52.93   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  8458 on 17 Df
    ## Pseudo R-squared: 0.313
    ## Number of iterations: 28 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_20)
```

    ## [1] -16881.44

## 25 miles

``` r
scfa2_25 = betareg(vaccination_percentage_2nddose ~ X2sfca_25 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
data = fca)
summary(scfa2_25)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_2nddose ~ X2sfca_25 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.4708 -0.6082 -0.2166  0.2382 17.1869 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.435054   0.077284
    ## X2sfca_25                                           -0.000668   0.003064
    ## MSOADECILE2                                         -0.134010   0.031009
    ## MSOADECILE3                                         -0.150907   0.034982
    ## MSOADECILE4                                         -0.195096   0.037636
    ## MSOADECILE5                                         -0.195648   0.040265
    ## MSOADECILE6                                         -0.161894   0.042574
    ## MSOADECILE7                                          0.033849   0.046019
    ## MSOADECILE8                                          0.076576   0.048242
    ## MSOADECILE9                                          0.309136   0.050417
    ## MSOADECILE10                                         0.531751   0.053525
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.366140   0.115483
    ## Mixed.                                              -5.522125   0.866621
    ## Asian.                                              -0.342342   0.080315
    ## Black.                                              -0.022766   0.344064
    ## Other.                                              -4.408837   0.939233
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -5.629 1.81e-08 ***
    ## X2sfca_25                                            -0.218 0.827397    
    ## MSOADECILE2                                          -4.322 1.55e-05 ***
    ## MSOADECILE3                                          -4.314 1.60e-05 ***
    ## MSOADECILE4                                          -5.184 2.17e-07 ***
    ## MSOADECILE5                                          -4.859 1.18e-06 ***
    ## MSOADECILE6                                          -3.803 0.000143 ***
    ## MSOADECILE7                                           0.736 0.462004    
    ## MSOADECILE8                                           1.587 0.112440    
    ## MSOADECILE9                                           6.132 8.70e-10 ***
    ## MSOADECILE10                                          9.935  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  29.148  < 2e-16 ***
    ## Mixed.                                               -6.372 1.87e-10 ***
    ## Asian.                                               -4.262 2.02e-05 ***
    ## Black.                                               -0.066 0.947243    
    ## Other.                                               -4.694 2.68e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  21.5058     0.4063   52.93   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  8455 on 17 Df
    ## Pseudo R-squared: 0.3125
    ## Number of iterations: 28 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_25)
```

    ## [1] -16875.19

## 30 miles

``` r
scfa2_30 = betareg(vaccination_percentage_2nddose ~ X2sfca_30 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,data = fca)
summary(scfa2_30)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_2nddose ~ X2sfca_30 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.4677 -0.6076 -0.2157  0.2392 17.1685 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.411639   0.077328
    ## X2sfca_30                                           -0.003618   0.003123
    ## MSOADECILE2                                         -0.133253   0.031007
    ## MSOADECILE3                                         -0.149115   0.034999
    ## MSOADECILE4                                         -0.193806   0.037641
    ## MSOADECILE5                                         -0.193982   0.040275
    ## MSOADECILE6                                         -0.160484   0.042573
    ## MSOADECILE7                                          0.035729   0.046029
    ## MSOADECILE8                                          0.076861   0.048233
    ## MSOADECILE9                                          0.310672   0.050417
    ## MSOADECILE10                                         0.533520   0.053527
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.359081   0.115472
    ## Mixed.                                              -5.536904   0.866430
    ## Asian.                                              -0.336023   0.080378
    ## Black.                                              -0.010550   0.344114
    ## Other.                                              -4.449000   0.939280
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -5.323 1.02e-07 ***
    ## X2sfca_30                                            -1.158 0.246702    
    ## MSOADECILE2                                          -4.298 1.73e-05 ***
    ## MSOADECILE3                                          -4.261 2.04e-05 ***
    ## MSOADECILE4                                          -5.149 2.62e-07 ***
    ## MSOADECILE5                                          -4.816 1.46e-06 ***
    ## MSOADECILE6                                          -3.770 0.000163 ***
    ## MSOADECILE7                                           0.776 0.437619    
    ## MSOADECILE8                                           1.594 0.111038    
    ## MSOADECILE9                                           6.162 7.18e-10 ***
    ## MSOADECILE10                                          9.967  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  29.090  < 2e-16 ***
    ## Mixed.                                               -6.390 1.65e-10 ***
    ## Asian.                                               -4.181 2.91e-05 ***
    ## Black.                                               -0.031 0.975541    
    ## Other.                                               -4.737 2.17e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  21.5126     0.4064   52.93   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  8455 on 17 Df
    ## Pseudo R-squared: 0.3125
    ## Number of iterations: 29 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_30)
```

    ## [1] -16876.45

# E2SFCA

## 10 miles

``` r
E2scfa_10 = betareg(vaccination_percentage_2nddose ~ E2sfca_10 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,data = fca)
summary(E2scfa_10)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_2nddose ~ E2sfca_10 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.4851 -0.6065 -0.2194  0.2385 17.2354 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.536840   0.077335
    ## E2sfca_10                                            0.006990   0.001794
    ## MSOADECILE2                                         -0.141332   0.031028
    ## MSOADECILE3                                         -0.163355   0.035031
    ## MSOADECILE4                                         -0.209756   0.037727
    ## MSOADECILE5                                         -0.213839   0.040455
    ## MSOADECILE6                                         -0.178591   0.042680
    ## MSOADECILE7                                          0.015981   0.046144
    ## MSOADECILE8                                          0.058495   0.048378
    ## MSOADECILE9                                          0.290303   0.050521
    ## MSOADECILE10                                         0.513067   0.053620
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.458982   0.117288
    ## Mixed.                                              -5.688594   0.866309
    ## Asian.                                              -0.381564   0.080406
    ## Black.                                              -0.028502   0.343323
    ## Other.                                              -4.355271   0.937409
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -6.942 3.87e-12 ***
    ## E2sfca_10                                             3.896 9.79e-05 ***
    ## MSOADECILE2                                          -4.555 5.24e-06 ***
    ## MSOADECILE3                                          -4.663 3.11e-06 ***
    ## MSOADECILE4                                          -5.560 2.70e-08 ***
    ## MSOADECILE5                                          -5.286 1.25e-07 ***
    ## MSOADECILE6                                          -4.184 2.86e-05 ***
    ## MSOADECILE7                                           0.346    0.729    
    ## MSOADECILE8                                           1.209    0.227    
    ## MSOADECILE9                                           5.746 9.13e-09 ***
    ## MSOADECILE10                                          9.569  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  29.491  < 2e-16 ***
    ## Mixed.                                               -6.566 5.15e-11 ***
    ## Asian.                                               -4.745 2.08e-06 ***
    ## Black.                                               -0.083    0.934    
    ## Other.                                               -4.646 3.38e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  21.5597     0.4073   52.93   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  8462 on 17 Df
    ## Pseudo R-squared: 0.3129
    ## Number of iterations: 26 (BFGS) + 3 (Fisher scoring)

``` r
AIC(E2scfa_10)
```

    ## [1] -16889.99

## 15 miles

``` r
E2scfa_15 = betareg(vaccination_percentage_2nddose ~ E2sfca_15 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other., data = fca)
summary(E2scfa_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_2nddose ~ E2sfca_15 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.5040 -0.6105 -0.2169  0.2370 17.3025 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                     Estimate Std. Error z value
    ## (Intercept)                                         -0.59066    0.07766  -7.605
    ## E2sfca_15                                            0.01274    0.00221   5.767
    ## MSOADECILE2                                         -0.14311    0.03098  -4.620
    ## MSOADECILE3                                         -0.16697    0.03496  -4.776
    ## MSOADECILE4                                         -0.21306    0.03765  -5.659
    ## MSOADECILE5                                         -0.21749    0.04034  -5.391
    ## MSOADECILE6                                         -0.18254    0.04258  -4.287
    ## MSOADECILE7                                          0.01102    0.04603   0.239
    ## MSOADECILE8                                          0.05662    0.04823   1.174
    ## MSOADECILE9                                          0.28718    0.05041   5.697
    ## MSOADECILE10                                         0.50949    0.05350   9.522
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.48874    0.11672  29.889
    ## Mixed.                                              -5.83450    0.86564  -6.740
    ## Asian.                                              -0.39326    0.08019  -4.904
    ## Black.                                              -0.03826    0.34287  -0.112
    ## Other.                                              -4.34900    0.93657  -4.644
    ##                                                     Pr(>|z|)    
    ## (Intercept)                                         2.84e-14 ***
    ## E2sfca_15                                           8.07e-09 ***
    ## MSOADECILE2                                         3.84e-06 ***
    ## MSOADECILE3                                         1.78e-06 ***
    ## MSOADECILE4                                         1.52e-08 ***
    ## MSOADECILE5                                         6.99e-08 ***
    ## MSOADECILE6                                         1.81e-05 ***
    ## MSOADECILE7                                            0.811    
    ## MSOADECILE8                                            0.240    
    ## MSOADECILE9                                         1.22e-08 ***
    ## MSOADECILE10                                         < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  < 2e-16 ***
    ## Mixed.                                              1.58e-11 ***
    ## Asian.                                              9.38e-07 ***
    ## Black.                                                 0.911    
    ## Other.                                              3.42e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  21.6286     0.4086   52.94   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  8471 on 17 Df
    ## Pseudo R-squared: 0.3138
    ## Number of iterations: 27 (BFGS) + 3 (Fisher scoring)

``` r
AIC(E2scfa_15)
```

    ## [1] -16908.54

## 20 miles

``` r
E2scfa_20 = betareg(vaccination_percentage_2nddose ~ E2sfca_20 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other., data = fca)
summary(E2scfa_20)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_2nddose ~ E2sfca_20 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.5198 -0.6069 -0.2112  0.2446 17.3564 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.624133   0.077941
    ## E2sfca_20                                            0.017056   0.002523
    ## MSOADECILE2                                         -0.143532   0.030942
    ## MSOADECILE3                                         -0.167684   0.034904
    ## MSOADECILE4                                         -0.211999   0.037581
    ## MSOADECILE5                                         -0.216264   0.040251
    ## MSOADECILE6                                         -0.183264   0.042518
    ## MSOADECILE7                                          0.009755   0.045957
    ## MSOADECILE8                                          0.059330   0.048143
    ## MSOADECILE9                                          0.285732   0.050335
    ## MSOADECILE10                                         0.509155   0.053435
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.498402   0.116342
    ## Mixed.                                              -5.927596   0.865270
    ## Asian.                                              -0.397331   0.080100
    ## Black.                                              -0.040410   0.342494
    ## Other.                                              -4.358705   0.936188
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -8.008 1.17e-15 ***
    ## E2sfca_20                                             6.761 1.37e-11 ***
    ## MSOADECILE2                                          -4.639 3.51e-06 ***
    ## MSOADECILE3                                          -4.804 1.55e-06 ***
    ## MSOADECILE4                                          -5.641 1.69e-08 ***
    ## MSOADECILE5                                          -5.373 7.75e-08 ***
    ## MSOADECILE6                                          -4.310 1.63e-05 ***
    ## MSOADECILE7                                           0.212    0.832    
    ## MSOADECILE8                                           1.232    0.218    
    ## MSOADECILE9                                           5.677 1.37e-08 ***
    ## MSOADECILE10                                          9.529  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  30.070  < 2e-16 ***
    ## Mixed.                                               -6.851 7.36e-12 ***
    ## Asian.                                               -4.960 7.03e-07 ***
    ## Black.                                               -0.118    0.906    
    ## Other.                                               -4.656 3.23e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  21.6767     0.4095   52.94   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  8478 on 17 Df
    ## Pseudo R-squared: 0.3146
    ## Number of iterations: 28 (BFGS) + 3 (Fisher scoring)

``` r
AIC(E2scfa_20)
```

    ## [1] -16921.66

## 25 miles

``` r
E2scfa_25 = betareg(vaccination_percentage_2nddose ~ E2sfca_25 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other., data = fca)
summary(E2scfa_25)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_2nddose ~ E2sfca_25 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.5193 -0.6078 -0.2109  0.2446 17.3718 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.622628   0.078228
    ## E2sfca_25                                            0.017843   0.002752
    ## MSOADECILE2                                         -0.142135   0.030943
    ## MSOADECILE3                                         -0.165356   0.034901
    ## MSOADECILE4                                         -0.207933   0.037565
    ## MSOADECILE5                                         -0.211818   0.040222
    ## MSOADECILE6                                         -0.180185   0.042506
    ## MSOADECILE7                                          0.013143   0.045936
    ## MSOADECILE8                                          0.064646   0.048129
    ## MSOADECILE9                                          0.288319   0.050323
    ## MSOADECILE10                                         0.512601   0.053427
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.484973   0.116162
    ## Mixed.                                              -5.915039   0.865729
    ## Asian.                                              -0.391108   0.080126
    ## Black.                                              -0.039736   0.342536
    ## Other.                                              -4.359328   0.936627
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -7.959 1.73e-15 ***
    ## E2sfca_25                                             6.485 8.90e-11 ***
    ## MSOADECILE2                                          -4.593 4.36e-06 ***
    ## MSOADECILE3                                          -4.738 2.16e-06 ***
    ## MSOADECILE4                                          -5.535 3.11e-08 ***
    ## MSOADECILE5                                          -5.266 1.39e-07 ***
    ## MSOADECILE6                                          -4.239 2.24e-05 ***
    ## MSOADECILE7                                           0.286    0.775    
    ## MSOADECILE8                                           1.343    0.179    
    ## MSOADECILE9                                           5.729 1.01e-08 ***
    ## MSOADECILE10                                          9.594  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  30.001  < 2e-16 ***
    ## Mixed.                                               -6.832 8.35e-12 ***
    ## Asian.                                               -4.881 1.05e-06 ***
    ## Black.                                               -0.116    0.908    
    ## Other.                                               -4.654 3.25e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  21.6627     0.4092   52.94   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  8476 on 17 Df
    ## Pseudo R-squared: 0.3145
    ## Number of iterations: 28 (BFGS) + 3 (Fisher scoring)

``` r
AIC(E2scfa_25)
```

    ## [1] -16918.07

## 30 miles

``` r
E2scfa_30 = betareg(vaccination_percentage_2nddose ~ E2sfca_30 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other., data = fca)
summary(E2scfa_30)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_2nddose ~ E2sfca_30 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.5110 -0.6072 -0.2123  0.2388 17.3621 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.597012   0.078405
    ## E2sfca_30                                            0.016003   0.002915
    ## MSOADECILE2                                         -0.140274   0.030964
    ## MSOADECILE3                                         -0.162084   0.034926
    ## MSOADECILE4                                         -0.203795   0.037581
    ## MSOADECILE5                                         -0.207216   0.040230
    ## MSOADECILE6                                         -0.175621   0.042524
    ## MSOADECILE7                                          0.018333   0.045954
    ## MSOADECILE8                                          0.069633   0.048156
    ## MSOADECILE9                                          0.293251   0.050345
    ## MSOADECILE10                                         0.517314   0.053452
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.459406   0.116061
    ## Mixed.                                              -5.835183   0.866498
    ## Asian.                                              -0.380723   0.080200
    ## Black.                                              -0.042217   0.342863
    ## Other.                                              -4.350515   0.937408
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -7.614 2.65e-14 ***
    ## E2sfca_30                                             5.489 4.03e-08 ***
    ## MSOADECILE2                                          -4.530 5.89e-06 ***
    ## MSOADECILE3                                          -4.641 3.47e-06 ***
    ## MSOADECILE4                                          -5.423 5.87e-08 ***
    ## MSOADECILE5                                          -5.151 2.59e-07 ***
    ## MSOADECILE6                                          -4.130 3.63e-05 ***
    ## MSOADECILE7                                           0.399    0.690    
    ## MSOADECILE8                                           1.446    0.148    
    ## MSOADECILE9                                           5.825 5.72e-09 ***
    ## MSOADECILE10                                          9.678  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  29.807  < 2e-16 ***
    ## Mixed.                                               -6.734 1.65e-11 ***
    ## Asian.                                               -4.747 2.06e-06 ***
    ## Black.                                               -0.123    0.902    
    ## Other.                                               -4.641 3.47e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  21.6161     0.4083   52.94   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  8470 on 17 Df
    ## Pseudo R-squared: 0.314
    ## Number of iterations: 28 (BFGS) + 3 (Fisher scoring)

``` r
AIC(E2scfa_30)
```

    ## [1] -16905.79

# 3SFCA

## 10 miles

``` r
scfa3_10 = betareg(vaccination_percentage_2nddose ~ X3sfca_10 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
data = fca)
summary(scfa3_10)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_2nddose ~ X3sfca_10 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.4871 -0.6060 -0.2186  0.2426 17.2350 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.530511   0.076169
    ## X3sfca_10                                            0.007274   0.001697
    ## MSOADECILE2                                         -0.139663   0.030991
    ## MSOADECILE3                                         -0.160517   0.034968
    ## MSOADECILE4                                         -0.206963   0.037645
    ## MSOADECILE5                                         -0.209949   0.040319
    ## MSOADECILE6                                         -0.175096   0.042573
    ## MSOADECILE7                                          0.019544   0.046037
    ## MSOADECILE8                                          0.061754   0.048284
    ## MSOADECILE9                                          0.291457   0.050415
    ## MSOADECILE10                                         0.516248   0.053543
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.443144   0.116249
    ## Mixed.                                              -5.657535   0.865797
    ## Asian.                                              -0.376157   0.080196
    ## Black.                                              -0.004355   0.343170
    ## Other.                                              -4.309265   0.937063
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -6.965 3.29e-12 ***
    ## X3sfca_10                                             4.287 1.81e-05 ***
    ## MSOADECILE2                                          -4.507 6.59e-06 ***
    ## MSOADECILE3                                          -4.590 4.42e-06 ***
    ## MSOADECILE4                                          -5.498 3.85e-08 ***
    ## MSOADECILE5                                          -5.207 1.92e-07 ***
    ## MSOADECILE6                                          -4.113 3.91e-05 ***
    ## MSOADECILE7                                           0.425    0.671    
    ## MSOADECILE8                                           1.279    0.201    
    ## MSOADECILE9                                           5.781 7.42e-09 ***
    ## MSOADECILE10                                          9.642  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  29.619  < 2e-16 ***
    ## Mixed.                                               -6.534 6.38e-11 ***
    ## Asian.                                               -4.690 2.73e-06 ***
    ## Black.                                               -0.013    0.990    
    ## Other.                                               -4.599 4.25e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  21.5742     0.4076   52.93   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  8463 on 17 Df
    ## Pseudo R-squared: 0.3129
    ## Number of iterations: 25 (BFGS) + 4 (Fisher scoring)

``` r
AIC(scfa3_10)
```

    ## [1] -16892.94

## 15 miles

``` r
scfa3_15 = betareg(vaccination_percentage_2nddose ~ X3sfca_15 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa3_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_2nddose ~ X3sfca_15 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.4911 -0.6076 -0.2194  0.2433 17.2588 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.553608   0.076653
    ## X3sfca_15                                            0.010506   0.002111
    ## MSOADECILE2                                         -0.139949   0.030974
    ## MSOADECILE3                                         -0.161842   0.034943
    ## MSOADECILE4                                         -0.207992   0.037626
    ## MSOADECILE5                                         -0.211978   0.040307
    ## MSOADECILE6                                         -0.176972   0.042555
    ## MSOADECILE7                                          0.016955   0.046016
    ## MSOADECILE8                                          0.061521   0.048236
    ## MSOADECILE9                                          0.292042   0.050387
    ## MSOADECILE10                                         0.515110   0.053506
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.448092   0.116034
    ## Mixed.                                              -5.694704   0.865634
    ## Asian.                                              -0.378772   0.080089
    ## Black.                                              -0.016586   0.343002
    ## Other.                                              -4.245415   0.936853
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -7.222 5.11e-13 ***
    ## X3sfca_15                                             4.977 6.47e-07 ***
    ## MSOADECILE2                                          -4.518 6.24e-06 ***
    ## MSOADECILE3                                          -4.632 3.63e-06 ***
    ## MSOADECILE4                                          -5.528 3.24e-08 ***
    ## MSOADECILE5                                          -5.259 1.45e-07 ***
    ## MSOADECILE6                                          -4.159 3.20e-05 ***
    ## MSOADECILE7                                           0.368    0.713    
    ## MSOADECILE8                                           1.275    0.202    
    ## MSOADECILE9                                           5.796 6.79e-09 ***
    ## MSOADECILE10                                          9.627  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  29.716  < 2e-16 ***
    ## Mixed.                                               -6.579 4.75e-11 ***
    ## Asian.                                               -4.729 2.25e-06 ***
    ## Black.                                               -0.048    0.961    
    ## Other.                                               -4.532 5.85e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  21.6005     0.4081   52.94   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  8467 on 17 Df
    ## Pseudo R-squared: 0.3133
    ## Number of iterations: 27 (BFGS) + 2 (Fisher scoring)

``` r
AIC(scfa3_15)
```

    ## [1] -16899.77

## 20 miles

``` r
scfa3_20 = betareg(vaccination_percentage_2nddose ~ X3sfca_20 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa3_20)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_2nddose ~ X3sfca_20 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.4971 -0.6112 -0.2187  0.2425 17.2958 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.575742   0.077005
    ## X3sfca_20                                            0.013621   0.002421
    ## MSOADECILE2                                         -0.139777   0.030954
    ## MSOADECILE3                                         -0.161958   0.034914
    ## MSOADECILE4                                         -0.207907   0.037599
    ## MSOADECILE5                                         -0.212117   0.040269
    ## MSOADECILE6                                         -0.177100   0.042521
    ## MSOADECILE7                                          0.015850   0.045979
    ## MSOADECILE8                                          0.063206   0.048176
    ## MSOADECILE9                                          0.293949   0.050343
    ## MSOADECILE10                                         0.515908   0.053456
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.450352   0.115807
    ## Mixed.                                              -5.697959   0.865294
    ## Asian.                                              -0.379321   0.079988
    ## Black.                                              -0.028510   0.342773
    ## Other.                                              -4.213905   0.936435
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -7.477 7.62e-14 ***
    ## X3sfca_20                                             5.627 1.84e-08 ***
    ## MSOADECILE2                                          -4.516 6.31e-06 ***
    ## MSOADECILE3                                          -4.639 3.50e-06 ***
    ## MSOADECILE4                                          -5.530 3.21e-08 ***
    ## MSOADECILE5                                          -5.267 1.38e-07 ***
    ## MSOADECILE6                                          -4.165 3.11e-05 ***
    ## MSOADECILE7                                           0.345    0.730    
    ## MSOADECILE8                                           1.312    0.190    
    ## MSOADECILE9                                           5.839 5.25e-09 ***
    ## MSOADECILE10                                          9.651  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  29.794  < 2e-16 ***
    ## Mixed.                                               -6.585 4.55e-11 ***
    ## Asian.                                               -4.742 2.11e-06 ***
    ## Black.                                               -0.083    0.934    
    ## Other.                                               -4.500 6.80e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  21.6285     0.4086   52.94   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  8471 on 17 Df
    ## Pseudo R-squared: 0.3138
    ## Number of iterations: 29 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa3_20)
```

    ## [1] -16907.39

## 25 miles

``` r
scfa3_25 = betareg(vaccination_percentage_2nddose ~ X3sfca_25 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa3_25)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_2nddose ~ X3sfca_25 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.5072 -0.6103 -0.2195  0.2445 17.3325 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.602749   0.077274
    ## X3sfca_25                                            0.017232   0.002668
    ## MSOADECILE2                                         -0.140117   0.030928
    ## MSOADECILE3                                         -0.162563   0.034880
    ## MSOADECILE4                                         -0.207586   0.037561
    ## MSOADECILE5                                         -0.211845   0.040218
    ## MSOADECILE6                                         -0.177302   0.042479
    ## MSOADECILE7                                          0.014945   0.045926
    ## MSOADECILE8                                          0.065267   0.048119
    ## MSOADECILE9                                          0.294549   0.050290
    ## MSOADECILE10                                         0.516948   0.053405
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.454684   0.115588
    ## Mixed.                                              -5.677832   0.864692
    ## Asian.                                              -0.381667   0.079895
    ## Black.                                              -0.042649   0.342386
    ## Other.                                              -4.188777   0.935681
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -7.800 6.18e-15 ***
    ## X3sfca_25                                             6.459 1.05e-10 ***
    ## MSOADECILE2                                          -4.530 5.88e-06 ***
    ## MSOADECILE3                                          -4.661 3.15e-06 ***
    ## MSOADECILE4                                          -5.527 3.26e-08 ***
    ## MSOADECILE5                                          -5.267 1.38e-07 ***
    ## MSOADECILE6                                          -4.174 2.99e-05 ***
    ## MSOADECILE7                                           0.325    0.745    
    ## MSOADECILE8                                           1.356    0.175    
    ## MSOADECILE9                                           5.857 4.71e-09 ***
    ## MSOADECILE10                                          9.680  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  29.888  < 2e-16 ***
    ## Mixed.                                               -6.566 5.16e-11 ***
    ## Asian.                                               -4.777 1.78e-06 ***
    ## Black.                                               -0.125    0.901    
    ## Other.                                               -4.477 7.58e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  21.6693     0.4093   52.94   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  8476 on 17 Df
    ## Pseudo R-squared: 0.3145
    ## Number of iterations: 28 (BFGS) + 2 (Fisher scoring)

``` r
AIC(scfa3_25)
```

    ## [1] -16918.17

## 30 miles

``` r
scfa3_30 = betareg(vaccination_percentage_2nddose ~ X3sfca_30 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa3_30)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_2nddose ~ X3sfca_30 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.5173 -0.6061 -0.2169  0.2420 17.3820 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.630344   0.077583
    ## X3sfca_30                                            0.020666   0.002861
    ## MSOADECILE2                                         -0.140684   0.030902
    ## MSOADECILE3                                         -0.163399   0.034848
    ## MSOADECILE4                                         -0.207059   0.037522
    ## MSOADECILE5                                         -0.211411   0.040172
    ## MSOADECILE6                                         -0.177909   0.042440
    ## MSOADECILE7                                          0.014080   0.045876
    ## MSOADECILE8                                          0.067049   0.048072
    ## MSOADECILE9                                          0.293722   0.050242
    ## MSOADECILE10                                         0.517712   0.053363
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.461618   0.115448
    ## Mixed.                                              -5.660539   0.864032
    ## Asian.                                              -0.383513   0.079811
    ## Black.                                              -0.047759   0.341926
    ## Other.                                              -4.177664   0.934868
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -8.125 4.48e-16 ***
    ## X3sfca_30                                             7.223 5.07e-13 ***
    ## MSOADECILE2                                          -4.553 5.30e-06 ***
    ## MSOADECILE3                                          -4.689 2.75e-06 ***
    ## MSOADECILE4                                          -5.518 3.42e-08 ***
    ## MSOADECILE5                                          -5.263 1.42e-07 ***
    ## MSOADECILE6                                          -4.192 2.76e-05 ***
    ## MSOADECILE7                                           0.307    0.759    
    ## MSOADECILE8                                           1.395    0.163    
    ## MSOADECILE9                                           5.846 5.03e-09 ***
    ## MSOADECILE10                                          9.702  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  29.984  < 2e-16 ***
    ## Mixed.                                               -6.551 5.70e-11 ***
    ## Asian.                                               -4.805 1.55e-06 ***
    ## Black.                                               -0.140    0.889    
    ## Other.                                               -4.469 7.87e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  21.7128     0.4102   52.94   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  8482 on 17 Df
    ## Pseudo R-squared: 0.3152
    ## Number of iterations: 28 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa3_30)
```

    ## [1] -16929.38
