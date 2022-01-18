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
scfa2_10 = betareg(vaccination_percentage_1stdose ~ X2sfca_10 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
data = fca)
summary(scfa2_10)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_1stdose ~ X2sfca_10 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5587 -0.6784 -0.3117  0.1821 15.8181 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.211696   0.091814
    ## X2sfca_10                                            0.015557   0.002548
    ## MSOADECILE2                                         -0.159136   0.037686
    ## MSOADECILE3                                         -0.162433   0.042559
    ## MSOADECILE4                                         -0.204794   0.045778
    ## MSOADECILE5                                         -0.162937   0.049154
    ## MSOADECILE6                                         -0.082703   0.052072
    ## MSOADECILE7                                          0.169050   0.056277
    ## MSOADECILE8                                          0.211634   0.058744
    ## MSOADECILE9                                          0.465938   0.061019
    ## MSOADECILE10                                         0.688030   0.064023
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.422458   0.139973
    ## Mixed.                                              -5.244466   1.054261
    ## Asian.                                              -0.040175   0.099836
    ## Black.                                               0.087225   0.416443
    ## Other.                                              -4.005101   1.104898
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -2.306 0.021127 *  
    ## X2sfca_10                                             6.105 1.03e-09 ***
    ## MSOADECILE2                                          -4.223 2.41e-05 ***
    ## MSOADECILE3                                          -3.817 0.000135 ***
    ## MSOADECILE4                                          -4.474 7.69e-06 ***
    ## MSOADECILE5                                          -3.315 0.000917 ***
    ## MSOADECILE6                                          -1.588 0.112230    
    ## MSOADECILE7                                           3.004 0.002665 ** 
    ## MSOADECILE8                                           3.603 0.000315 ***
    ## MSOADECILE9                                           7.636 2.24e-14 ***
    ## MSOADECILE10                                         10.747  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  24.451  < 2e-16 ***
    ## Mixed.                                               -4.975 6.54e-07 ***
    ## Asian.                                               -0.402 0.687380    
    ## Black.                                                0.209 0.834096    
    ## Other.                                               -3.625 0.000289 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  17.1447     0.3408   50.31   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood: 1.021e+04 on 17 Df
    ## Pseudo R-squared: 0.2784
    ## Number of iterations: 24 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_10)
```

    ## [1] -20391.39

## 15 miles

``` r
scfa2_15 = betareg(vaccination_percentage_1stdose ~ X2sfca_15 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
data = fca)
summary(scfa2_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_1stdose ~ X2sfca_15 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5615 -0.6811 -0.3131  0.1807 15.6438 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.185323   0.092726
    ## X2sfca_15                                            0.013176   0.003177
    ## MSOADECILE2                                         -0.161688   0.037759
    ## MSOADECILE3                                         -0.161840   0.042647
    ## MSOADECILE4                                         -0.203688   0.045849
    ## MSOADECILE5                                         -0.164532   0.049232
    ## MSOADECILE6                                         -0.083089   0.052172
    ## MSOADECILE7                                          0.164853   0.056362
    ## MSOADECILE8                                          0.212616   0.058832
    ## MSOADECILE9                                          0.461935   0.061097
    ## MSOADECILE10                                         0.681801   0.064107
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.404728   0.140079
    ## Mixed.                                              -5.116201   1.056722
    ## Asian.                                              -0.026910   0.100279
    ## Black.                                               0.096228   0.417548
    ## Other.                                              -3.983642   1.108093
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -1.999 0.045650 *  
    ## X2sfca_15                                             4.148 3.36e-05 ***
    ## MSOADECILE2                                          -4.282 1.85e-05 ***
    ## MSOADECILE3                                          -3.795 0.000148 ***
    ## MSOADECILE4                                          -4.443 8.89e-06 ***
    ## MSOADECILE5                                          -3.342 0.000832 ***
    ## MSOADECILE6                                          -1.593 0.111254    
    ## MSOADECILE7                                           2.925 0.003446 ** 
    ## MSOADECILE8                                           3.614 0.000302 ***
    ## MSOADECILE9                                           7.561 4.01e-14 ***
    ## MSOADECILE10                                         10.635  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  24.306  < 2e-16 ***
    ## Mixed.                                               -4.842 1.29e-06 ***
    ## Asian.                                               -0.268 0.788430    
    ## Black.                                                0.230 0.817735    
    ## Other.                                               -3.595 0.000324 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  17.0660     0.3393    50.3   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood: 1.02e+04 on 17 Df
    ## Pseudo R-squared: 0.2771
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_15)
```

    ## [1] -20367.17

## 20 mmiles

``` r
scfa2_20 = betareg(vaccination_percentage_1stdose ~ X2sfca_20 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
data = fca)
summary(scfa2_20)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_1stdose ~ X2sfca_20 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5531 -0.6750 -0.3146  0.1744 15.2550 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.113142   0.093758
    ## X2sfca_20                                            0.004325   0.003537
    ## MSOADECILE2                                         -0.158100   0.037798
    ## MSOADECILE3                                         -0.157586   0.042712
    ## MSOADECILE4                                         -0.201221   0.045900
    ## MSOADECILE5                                         -0.161520   0.049270
    ## MSOADECILE6                                         -0.077135   0.052229
    ## MSOADECILE7                                          0.171593   0.056418
    ## MSOADECILE8                                          0.213405   0.058894
    ## MSOADECILE9                                          0.465443   0.061145
    ## MSOADECILE10                                         0.684767   0.064151
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.376028   0.140242
    ## Mixed.                                              -4.969958   1.058020
    ## Asian.                                              -0.010746   0.100343
    ## Black.                                               0.123109   0.418438
    ## Other.                                              -4.048477   1.109411
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -1.207 0.227528    
    ## X2sfca_20                                             1.223 0.221450    
    ## MSOADECILE2                                          -4.183 2.88e-05 ***
    ## MSOADECILE3                                          -3.690 0.000225 ***
    ## MSOADECILE4                                          -4.384 1.17e-05 ***
    ## MSOADECILE5                                          -3.278 0.001044 ** 
    ## MSOADECILE6                                          -1.477 0.139710    
    ## MSOADECILE7                                           3.041 0.002354 ** 
    ## MSOADECILE8                                           3.624 0.000291 ***
    ## MSOADECILE9                                           7.612 2.70e-14 ***
    ## MSOADECILE10                                         10.674  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  24.073  < 2e-16 ***
    ## Mixed.                                               -4.697 2.63e-06 ***
    ## Asian.                                               -0.107 0.914712    
    ## Black.                                                0.294 0.768597    
    ## Other.                                               -3.649 0.000263 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  17.0119     0.3382    50.3   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood: 1.019e+04 on 17 Df
    ## Pseudo R-squared: 0.2764
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_20)
```

    ## [1] -20350.78

## 25 miles

``` r
scfa2_25 = betareg(vaccination_percentage_1stdose ~ X2sfca_25 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
data = fca)
summary(scfa2_25)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_1stdose ~ X2sfca_25 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5517 -0.6764 -0.3155  0.1718 14.8490 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                       Estimate Std. Error
    ## (Intercept)                                         -0.0300683  0.0937484
    ## X2sfca_25                                           -0.0056013  0.0037285
    ## MSOADECILE2                                         -0.1555426  0.0377918
    ## MSOADECILE3                                         -0.1526468  0.0427169
    ## MSOADECILE4                                         -0.1990518  0.0458896
    ## MSOADECILE5                                         -0.1570647  0.0492627
    ## MSOADECILE6                                         -0.0703823  0.0522329
    ## MSOADECILE7                                          0.1778626  0.0564175
    ## MSOADECILE8                                          0.2133260  0.0588835
    ## MSOADECILE9                                          0.4695791  0.0611391
    ## MSOADECILE10                                         0.6891881  0.0641474
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.3455911  0.1401124
    ## Mixed.                                              -4.9067039  1.0573250
    ## Asian.                                               0.0006082  0.1004318
    ## Black.                                               0.1561480  0.4191621
    ## Other.                                              -4.1761696  1.1094016
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -0.321 0.748412    
    ## X2sfca_25                                            -1.502 0.133020    
    ## MSOADECILE2                                          -4.116 3.86e-05 ***
    ## MSOADECILE3                                          -3.573 0.000352 ***
    ## MSOADECILE4                                          -4.338 1.44e-05 ***
    ## MSOADECILE5                                          -3.188 0.001431 ** 
    ## MSOADECILE6                                          -1.347 0.177829    
    ## MSOADECILE7                                           3.153 0.001618 ** 
    ## MSOADECILE8                                           3.623 0.000291 ***
    ## MSOADECILE9                                           7.681 1.58e-14 ***
    ## MSOADECILE10                                         10.744  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  23.878  < 2e-16 ***
    ## Mixed.                                               -4.641 3.47e-06 ***
    ## Asian.                                                0.006 0.995168    
    ## Black.                                                0.373 0.709503    
    ## Other.                                               -3.764 0.000167 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  17.0163     0.3383    50.3   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood: 1.019e+04 on 17 Df
    ## Pseudo R-squared: 0.2766
    ## Number of iterations: 26 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_25)
```

    ## [1] -20351.5

## 30 miles

``` r
scfa2_30 = betareg(vaccination_percentage_1stdose ~ X2sfca_30 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,data = fca)
summary(scfa2_30)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_1stdose ~ X2sfca_30 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5362 -0.6783 -0.3102  0.1704 14.6790 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                          0.007377   0.093791
    ## X2sfca_30                                           -0.010242   0.003801
    ## MSOADECILE2                                         -0.153721   0.037779
    ## MSOADECILE3                                         -0.147951   0.042728
    ## MSOADECILE4                                         -0.195764   0.045882
    ## MSOADECILE5                                         -0.153607   0.049257
    ## MSOADECILE6                                         -0.066784   0.052220
    ## MSOADECILE7                                          0.182059   0.056418
    ## MSOADECILE8                                          0.215548   0.058858
    ## MSOADECILE9                                          0.473415   0.061129
    ## MSOADECILE10                                         0.692778   0.064135
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.332702   0.140077
    ## Mixed.                                              -4.946060   1.056727
    ## Asian.                                               0.013140   0.100503
    ## Black.                                               0.174875   0.419262
    ## Other.                                              -4.264255   1.109030
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                           0.079 0.937306    
    ## X2sfca_30                                            -2.695 0.007042 ** 
    ## MSOADECILE2                                          -4.069 4.72e-05 ***
    ## MSOADECILE3                                          -3.463 0.000535 ***
    ## MSOADECILE4                                          -4.267 1.98e-05 ***
    ## MSOADECILE5                                          -3.119 0.001818 ** 
    ## MSOADECILE6                                          -1.279 0.200930    
    ## MSOADECILE7                                           3.227 0.001251 ** 
    ## MSOADECILE8                                           3.662 0.000250 ***
    ## MSOADECILE9                                           7.745 9.59e-15 ***
    ## MSOADECILE10                                         10.802  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  23.792  < 2e-16 ***
    ## Mixed.                                               -4.681 2.86e-06 ***
    ## Asian.                                                0.131 0.895980    
    ## Black.                                                0.417 0.676604    
    ## Other.                                               -3.845 0.000121 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  17.0327     0.3386    50.3   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood: 1.02e+04 on 17 Df
    ## Pseudo R-squared: 0.277
    ## Number of iterations: 26 (BFGS) + 4 (Fisher scoring)

``` r
AIC(scfa2_30)
```

    ## [1] -20356.36

# E2SFCA

## 10 miles

``` r
E2scfa_10 = betareg(vaccination_percentage_1stdose ~ E2sfca_10 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,data = fca)
summary(E2scfa_10)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_1stdose ~ E2sfca_10 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5563 -0.6759 -0.3132  0.1771 15.6157 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.201746   0.093702
    ## E2sfca_10                                            0.009589   0.002186
    ## MSOADECILE2                                         -0.166479   0.037803
    ## MSOADECILE3                                         -0.173304   0.042742
    ## MSOADECILE4                                         -0.218766   0.045983
    ## MSOADECILE5                                         -0.181813   0.049480
    ## MSOADECILE6                                         -0.096348   0.052324
    ## MSOADECILE7                                          0.153749   0.056548
    ## MSOADECILE8                                          0.189113   0.059022
    ## MSOADECILE9                                          0.447994   0.061260
    ## MSOADECILE10                                         0.666421   0.064254
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.477351   0.142192
    ## Mixed.                                              -5.217174   1.056352
    ## Asian.                                              -0.061189   0.100396
    ## Black.                                               0.128047   0.417450
    ## Other.                                              -3.981475   1.107196
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -2.153 0.031314 *  
    ## E2sfca_10                                             4.387 1.15e-05 ***
    ## MSOADECILE2                                          -4.404 1.06e-05 ***
    ## MSOADECILE3                                          -4.055 5.02e-05 ***
    ## MSOADECILE4                                          -4.757 1.96e-06 ***
    ## MSOADECILE5                                          -3.674 0.000238 ***
    ## MSOADECILE6                                          -1.841 0.065570 .  
    ## MSOADECILE7                                           2.719 0.006549 ** 
    ## MSOADECILE8                                           3.204 0.001355 ** 
    ## MSOADECILE9                                           7.313 2.61e-13 ***
    ## MSOADECILE10                                         10.372  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  24.455  < 2e-16 ***
    ## Mixed.                                               -4.939 7.86e-07 ***
    ## Asian.                                               -0.609 0.542211    
    ## Black.                                                0.307 0.759045    
    ## Other.                                               -3.596 0.000323 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  17.0725     0.3394    50.3   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood: 1.02e+04 on 17 Df
    ## Pseudo R-squared: 0.2766
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(E2scfa_10)
```

    ## [1] -20368.76

## 15 miles

``` r
E2scfa_15 = betareg(vaccination_percentage_1stdose ~ E2sfca_15 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other., data = fca)
summary(E2scfa_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_1stdose ~ E2sfca_15 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5661 -0.6761 -0.3106  0.1848 15.9532 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.263538   0.094105
    ## E2sfca_15                                            0.016481   0.002696
    ## MSOADECILE2                                         -0.168613   0.037730
    ## MSOADECILE3                                         -0.178508   0.042636
    ## MSOADECILE4                                         -0.221929   0.045878
    ## MSOADECILE5                                         -0.185438   0.049336
    ## MSOADECILE6                                         -0.101036   0.052198
    ## MSOADECILE7                                          0.147691   0.056410
    ## MSOADECILE8                                          0.188486   0.058844
    ## MSOADECILE9                                          0.447153   0.061132
    ## MSOADECILE10                                         0.665636   0.064123
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.509480   0.141493
    ## Mixed.                                              -5.432867   1.054989
    ## Asian.                                              -0.073151   0.100095
    ## Black.                                               0.111263   0.416514
    ## Other.                                              -3.908217   1.105700
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -2.800 0.005103 ** 
    ## E2sfca_15                                             6.114 9.72e-10 ***
    ## MSOADECILE2                                          -4.469 7.86e-06 ***
    ## MSOADECILE3                                          -4.187 2.83e-05 ***
    ## MSOADECILE4                                          -4.837 1.32e-06 ***
    ## MSOADECILE5                                          -3.759 0.000171 ***
    ## MSOADECILE6                                          -1.936 0.052912 .  
    ## MSOADECILE7                                           2.618 0.008841 ** 
    ## MSOADECILE8                                           3.203 0.001359 ** 
    ## MSOADECILE9                                           7.315 2.58e-13 ***
    ## MSOADECILE10                                         10.381  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  24.803  < 2e-16 ***
    ## Mixed.                                               -5.150 2.61e-07 ***
    ## Asian.                                               -0.731 0.464892    
    ## Black.                                                0.267 0.789371    
    ## Other.                                               -3.535 0.000408 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  17.1385     0.3407   50.31   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood: 1.021e+04 on 17 Df
    ## Pseudo R-squared: 0.2773
    ## Number of iterations: 23 (BFGS) + 4 (Fisher scoring)

``` r
AIC(E2scfa_15)
```

    ## [1] -20388.28

## 20 miles

``` r
E2scfa_20 = betareg(vaccination_percentage_1stdose ~ E2sfca_20 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other., data = fca)
summary(E2scfa_20)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_1stdose ~ E2sfca_20 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5669 -0.6781 -0.3115  0.1842 16.1369 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.295927   0.094470
    ## E2sfca_20                                            0.020748   0.003075
    ## MSOADECILE2                                         -0.169838   0.037697
    ## MSOADECILE3                                         -0.179892   0.042584
    ## MSOADECILE4                                         -0.220945   0.045813
    ## MSOADECILE5                                         -0.184368   0.049244
    ## MSOADECILE6                                         -0.103139   0.052133
    ## MSOADECILE7                                          0.144963   0.056331
    ## MSOADECILE8                                          0.191568   0.058759
    ## MSOADECILE9                                          0.445893   0.061069
    ## MSOADECILE10                                         0.665365   0.064062
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.518354   0.141072
    ## Mixed.                                              -5.526890   1.054627
    ## Asian.                                              -0.073640   0.100032
    ## Black.                                               0.105636   0.416067
    ## Other.                                              -3.879368   1.105275
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -3.132 0.001733 ** 
    ## E2sfca_20                                             6.747 1.51e-11 ***
    ## MSOADECILE2                                          -4.505 6.63e-06 ***
    ## MSOADECILE3                                          -4.224 2.40e-05 ***
    ## MSOADECILE4                                          -4.823 1.42e-06 ***
    ## MSOADECILE5                                          -3.744 0.000181 ***
    ## MSOADECILE6                                          -1.978 0.047886 *  
    ## MSOADECILE7                                           2.573 0.010070 *  
    ## MSOADECILE8                                           3.260 0.001113 ** 
    ## MSOADECILE9                                           7.301 2.85e-13 ***
    ## MSOADECILE10                                         10.386  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  24.940  < 2e-16 ***
    ## Mixed.                                               -5.241 1.60e-07 ***
    ## Asian.                                               -0.736 0.461633    
    ## Black.                                                0.254 0.799579    
    ## Other.                                               -3.510 0.000448 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  17.1689     0.3413   50.31   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood: 1.022e+04 on 17 Df
    ## Pseudo R-squared: 0.2778
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(E2scfa_20)
```

    ## [1] -20397.53

## 25 miles

``` r
E2scfa_25 = betareg(vaccination_percentage_1stdose ~ E2sfca_25 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other., data = fca)
summary(E2scfa_25)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_1stdose ~ E2sfca_25 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5581 -0.6781 -0.3134  0.1809 16.1021 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.287452   0.094858
    ## E2sfca_25                                            0.020748   0.003353
    ## MSOADECILE2                                         -0.168702   0.037713
    ## MSOADECILE3                                         -0.176846   0.042599
    ## MSOADECILE4                                         -0.216781   0.045810
    ## MSOADECILE5                                         -0.179677   0.049226
    ## MSOADECILE6                                         -0.100317   0.052136
    ## MSOADECILE7                                          0.148261   0.056324
    ## MSOADECILE8                                          0.197192   0.058762
    ## MSOADECILE9                                          0.447629   0.061071
    ## MSOADECILE10                                         0.667499   0.064067
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.500906   0.140907
    ## Mixed.                                              -5.465009   1.055611
    ## Asian.                                              -0.062223   0.100136
    ## Black.                                               0.106010   0.416366
    ## Other.                                              -3.889127   1.106196
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -3.030 0.002443 ** 
    ## E2sfca_25                                             6.188 6.09e-10 ***
    ## MSOADECILE2                                          -4.473 7.70e-06 ***
    ## MSOADECILE3                                          -4.151 3.30e-05 ***
    ## MSOADECILE4                                          -4.732 2.22e-06 ***
    ## MSOADECILE5                                          -3.650 0.000262 ***
    ## MSOADECILE6                                          -1.924 0.054335 .  
    ## MSOADECILE7                                           2.632 0.008481 ** 
    ## MSOADECILE8                                           3.356 0.000791 ***
    ## MSOADECILE9                                           7.330 2.31e-13 ***
    ## MSOADECILE10                                         10.419  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  24.846  < 2e-16 ***
    ## Mixed.                                               -5.177 2.25e-07 ***
    ## Asian.                                               -0.621 0.534344    
    ## Black.                                                0.255 0.799026    
    ## Other.                                               -3.516 0.000438 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  17.1433     0.3408   50.31   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood: 1.021e+04 on 17 Df
    ## Pseudo R-squared: 0.2776
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(E2scfa_25)
```

    ## [1] -20389.92

## 30 miles

``` r
E2scfa_30 = betareg(vaccination_percentage_1stdose ~ E2sfca_30 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other., data = fca)
summary(E2scfa_30)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_1stdose ~ E2sfca_30 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5503 -0.6778 -0.3104  0.1821 15.9171 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.249203   0.095111
    ## E2sfca_30                                            0.017598   0.003551
    ## MSOADECILE2                                         -0.166209   0.037751
    ## MSOADECILE3                                         -0.171633   0.042647
    ## MSOADECILE4                                         -0.211704   0.045842
    ## MSOADECILE5                                         -0.173811   0.049247
    ## MSOADECILE6                                         -0.093967   0.052173
    ## MSOADECILE7                                          0.155124   0.056359
    ## MSOADECILE8                                          0.203232   0.058806
    ## MSOADECILE9                                          0.452420   0.061102
    ## MSOADECILE10                                         0.671858   0.064101
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.466636   0.140830
    ## Mixed.                                              -5.317516   1.057002
    ## Asian.                                              -0.046688   0.100287
    ## Black.                                               0.105076   0.417055
    ## Other.                                              -3.918777   1.107577
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -2.620 0.008790 ** 
    ## E2sfca_30                                             4.955 7.22e-07 ***
    ## MSOADECILE2                                          -4.403 1.07e-05 ***
    ## MSOADECILE3                                          -4.025 5.71e-05 ***
    ## MSOADECILE4                                          -4.618 3.87e-06 ***
    ## MSOADECILE5                                          -3.529 0.000417 ***
    ## MSOADECILE6                                          -1.801 0.071692 .  
    ## MSOADECILE7                                           2.752 0.005915 ** 
    ## MSOADECILE8                                           3.456 0.000548 ***
    ## MSOADECILE9                                           7.404 1.32e-13 ***
    ## MSOADECILE10                                         10.481  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  24.616  < 2e-16 ***
    ## Mixed.                                               -5.031 4.89e-07 ***
    ## Asian.                                               -0.466 0.641542    
    ## Black.                                                0.252 0.801082    
    ## Other.                                               -3.538 0.000403 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  17.0934     0.3398    50.3   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood: 1.02e+04 on 17 Df
    ## Pseudo R-squared: 0.277
    ## Number of iterations: 25 (BFGS) + 4 (Fisher scoring)

``` r
AIC(E2scfa_30)
```

    ## [1] -20375.08

# 3SFCA

## 10 miles

``` r
scfa3_10 = betareg(vaccination_percentage_1stdose ~ X3sfca_10 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
data = fca)
summary(scfa3_10)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_1stdose ~ X3sfca_10 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5553 -0.6773 -0.3169  0.1774 15.5387 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.181225   0.092240
    ## X3sfca_10                                            0.009062   0.002046
    ## MSOADECILE2                                         -0.163321   0.037766
    ## MSOADECILE3                                         -0.167383   0.042679
    ## MSOADECILE4                                         -0.213036   0.045893
    ## MSOADECILE5                                         -0.174147   0.049325
    ## MSOADECILE6                                         -0.089538   0.052206
    ## MSOADECILE7                                          0.161900   0.056434
    ## MSOADECILE8                                          0.194998   0.058915
    ## MSOADECILE9                                          0.453401   0.061150
    ## MSOADECILE10                                         0.672446   0.064168
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.444419   0.140940
    ## Mixed.                                              -5.134564   1.056071
    ## Asian.                                              -0.048739   0.100180
    ## Black.                                               0.154788   0.417434
    ## Other.                                              -3.958011   1.107137
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -1.965 0.049449 *  
    ## X3sfca_10                                             4.428 9.49e-06 ***
    ## MSOADECILE2                                          -4.325 1.53e-05 ***
    ## MSOADECILE3                                          -3.922 8.79e-05 ***
    ## MSOADECILE4                                          -4.642 3.45e-06 ***
    ## MSOADECILE5                                          -3.531 0.000415 ***
    ## MSOADECILE6                                          -1.715 0.086326 .  
    ## MSOADECILE7                                           2.869 0.004120 ** 
    ## MSOADECILE8                                           3.310 0.000933 ***
    ## MSOADECILE9                                           7.415 1.22e-13 ***
    ## MSOADECILE10                                         10.480  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  24.439  < 2e-16 ***
    ## Mixed.                                               -4.862 1.16e-06 ***
    ## Asian.                                               -0.487 0.626602    
    ## Black.                                                0.371 0.710780    
    ## Other.                                               -3.575 0.000350 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  17.0751     0.3394    50.3   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood: 1.02e+04 on 17 Df
    ## Pseudo R-squared: 0.2767
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa3_10)
```

    ## [1] -20369.03

## 15 miles

``` r
scfa3_15 = betareg(vaccination_percentage_1stdose ~ X3sfca_15 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa3_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_1stdose ~ X3sfca_15 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5700 -0.6770 -0.3148  0.1796 15.7307 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                     Estimate Std. Error z value
    ## (Intercept)                                         -0.21548    0.09282  -2.321
    ## X3sfca_15                                            0.01379    0.00256   5.386
    ## MSOADECILE2                                         -0.16400    0.03773  -4.346
    ## MSOADECILE3                                         -0.17078    0.04263  -4.006
    ## MSOADECILE4                                         -0.21480    0.04586  -4.684
    ## MSOADECILE5                                         -0.17742    0.04930  -3.599
    ## MSOADECILE6                                         -0.09357    0.05217  -1.794
    ## MSOADECILE7                                          0.15770    0.05640   2.796
    ## MSOADECILE8                                          0.19477    0.05885   3.310
    ## MSOADECILE9                                          0.45403    0.06111   7.430
    ## MSOADECILE10                                         0.67247    0.06412  10.488
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.45391    0.14064  24.558
    ## Mixed.                                              -5.23101    1.05536  -4.957
    ## Asian.                                              -0.05500    0.09999  -0.550
    ## Black.                                               0.14026    0.41684   0.336
    ## Other.                                              -3.84012    1.10667  -3.470
    ##                                                     Pr(>|z|)    
    ## (Intercept)                                         0.020262 *  
    ## X3sfca_15                                           7.21e-08 ***
    ## MSOADECILE2                                         1.38e-05 ***
    ## MSOADECILE3                                         6.17e-05 ***
    ## MSOADECILE4                                         2.81e-06 ***
    ## MSOADECILE5                                         0.000320 ***
    ## MSOADECILE6                                         0.072868 .  
    ## MSOADECILE7                                         0.005168 ** 
    ## MSOADECILE8                                         0.000934 ***
    ## MSOADECILE9                                         1.08e-13 ***
    ## MSOADECILE10                                         < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  < 2e-16 ***
    ## Mixed.                                              7.17e-07 ***
    ## Asian.                                              0.582314    
    ## Black.                                              0.736512    
    ## Other.                                              0.000521 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  17.1135     0.3402    50.3   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood: 1.021e+04 on 17 Df
    ## Pseudo R-squared: 0.2772
    ## Number of iterations: 24 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa3_15)
```

    ## [1] -20379.95

## 20 miles

``` r
scfa3_20 = betareg(vaccination_percentage_1stdose ~ X3sfca_20 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa3_20)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_1stdose ~ X3sfca_20 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5678 -0.6778 -0.3129  0.1851 15.8623 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.238264   0.093273
    ## X3sfca_20                                            0.017219   0.002945
    ## MSOADECILE2                                         -0.163752   0.037710
    ## MSOADECILE3                                         -0.171001   0.042594
    ## MSOADECILE4                                         -0.214235   0.045829
    ## MSOADECILE5                                         -0.177137   0.049259
    ## MSOADECILE6                                         -0.093614   0.052130
    ## MSOADECILE7                                          0.155781   0.056353
    ## MSOADECILE8                                          0.197917   0.058786
    ## MSOADECILE9                                          0.457291   0.061063
    ## MSOADECILE10                                         0.675686   0.064072
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.453854   0.140392
    ## Mixed.                                              -5.240309   1.054920
    ## Asian.                                              -0.053822   0.099880
    ## Black.                                               0.121580   0.416426
    ## Other.                                              -3.783352   1.106383
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -2.554 0.010635 *  
    ## X3sfca_20                                             5.848 4.99e-09 ***
    ## MSOADECILE2                                          -4.342 1.41e-05 ***
    ## MSOADECILE3                                          -4.015 5.95e-05 ***
    ## MSOADECILE4                                          -4.675 2.94e-06 ***
    ## MSOADECILE5                                          -3.596 0.000323 ***
    ## MSOADECILE6                                          -1.796 0.072533 .  
    ## MSOADECILE7                                           2.764 0.005703 ** 
    ## MSOADECILE8                                           3.367 0.000761 ***
    ## MSOADECILE9                                           7.489 6.95e-14 ***
    ## MSOADECILE10                                         10.546  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  24.602  < 2e-16 ***
    ## Mixed.                                               -4.967 6.78e-07 ***
    ## Asian.                                               -0.539 0.589979    
    ## Black.                                                0.292 0.770316    
    ## Other.                                               -3.420 0.000627 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  17.1350     0.3406   50.31   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood: 1.021e+04 on 17 Df
    ## Pseudo R-squared: 0.2774
    ## Number of iterations: 25 (BFGS) + 2 (Fisher scoring)

``` r
AIC(scfa3_20)
```

    ## [1] -20386.14

## 25 miles

``` r
scfa3_25 = betareg(vaccination_percentage_1stdose ~ X3sfca_25 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa3_25)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_1stdose ~ X3sfca_25 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5664 -0.6794 -0.3144  0.1886 16.0038 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.264105   0.093622
    ## X3sfca_25                                            0.020803   0.003247
    ## MSOADECILE2                                         -0.164470   0.037684
    ## MSOADECILE3                                         -0.171888   0.042561
    ## MSOADECILE4                                         -0.213716   0.045793
    ## MSOADECILE5                                         -0.176563   0.049209
    ## MSOADECILE6                                         -0.094299   0.052089
    ## MSOADECILE7                                          0.153728   0.056298
    ## MSOADECILE8                                          0.200783   0.058734
    ## MSOADECILE9                                          0.458746   0.061019
    ## MSOADECILE10                                         0.678120   0.064031
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.456770   0.140159
    ## Mixed.                                              -5.217798   1.054265
    ## Asian.                                              -0.054429   0.099792
    ## Black.                                               0.099827   0.415895
    ## Other.                                              -3.736941   1.105792
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -2.821 0.004788 ** 
    ## X3sfca_25                                             6.407 1.48e-10 ***
    ## MSOADECILE2                                          -4.364 1.27e-05 ***
    ## MSOADECILE3                                          -4.039 5.38e-05 ***
    ## MSOADECILE4                                          -4.667 3.06e-06 ***
    ## MSOADECILE5                                          -3.588 0.000333 ***
    ## MSOADECILE6                                          -1.810 0.070244 .  
    ## MSOADECILE7                                           2.731 0.006322 ** 
    ## MSOADECILE8                                           3.419 0.000630 ***
    ## MSOADECILE9                                           7.518 5.56e-14 ***
    ## MSOADECILE10                                         10.591  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  24.663  < 2e-16 ***
    ## Mixed.                                               -4.949 7.45e-07 ***
    ## Asian.                                               -0.545 0.585459    
    ## Black.                                                0.240 0.810307    
    ## Other.                                               -3.379 0.000726 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  17.1619     0.3411   50.31   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood: 1.021e+04 on 17 Df
    ## Pseudo R-squared: 0.2778
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa3_25)
```

    ## [1] -20393.88

## 30 miles

``` r
scfa3_30 = betareg(vaccination_percentage_1stdose ~ X3sfca_30 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa3_30)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_percentage_1stdose ~ X3sfca_30 + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5658 -0.6784 -0.3158  0.1894 16.1444 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                         -0.291380   0.094017
    ## X3sfca_30                                            0.024182   0.003482
    ## MSOADECILE2                                         -0.165767   0.037660
    ## MSOADECILE3                                         -0.173393   0.042530
    ## MSOADECILE4                                         -0.213569   0.045758
    ## MSOADECILE5                                         -0.176379   0.049166
    ## MSOADECILE6                                         -0.096304   0.052052
    ## MSOADECILE7                                          0.151350   0.056249
    ## MSOADECILE8                                          0.202470   0.058695
    ## MSOADECILE9                                          0.457924   0.060983
    ## MSOADECILE10                                         0.678915   0.063998
    ## Per_cent_of_households_with_at_least_one_car_or_van  3.464476   0.140023
    ## Mixed.                                              -5.201036   1.053555
    ## Asian.                                              -0.055229   0.099715
    ## Black.                                               0.089404   0.415317
    ## Other.                                              -3.707211   1.105079
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                          -3.099 0.001940 ** 
    ## X3sfca_30                                             6.945 3.77e-12 ***
    ## MSOADECILE2                                          -4.402 1.07e-05 ***
    ## MSOADECILE3                                          -4.077 4.56e-05 ***
    ## MSOADECILE4                                          -4.667 3.05e-06 ***
    ## MSOADECILE5                                          -3.587 0.000334 ***
    ## MSOADECILE6                                          -1.850 0.064293 .  
    ## MSOADECILE7                                           2.691 0.007130 ** 
    ## MSOADECILE8                                           3.450 0.000562 ***
    ## MSOADECILE9                                           7.509 5.96e-14 ***
    ## MSOADECILE10                                         10.608  < 2e-16 ***
    ## Per_cent_of_households_with_at_least_one_car_or_van  24.742  < 2e-16 ***
    ## Mixed.                                               -4.937 7.95e-07 ***
    ## Asian.                                               -0.554 0.579669    
    ## Black.                                                0.215 0.829559    
    ## Other.                                               -3.355 0.000795 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  17.1903     0.3417   50.31   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood: 1.022e+04 on 17 Df
    ## Pseudo R-squared: 0.2783
    ## Number of iterations: 25 (BFGS) + 4 (Fisher scoring)

``` r
AIC(scfa3_30)
```

    ## [1] -20402.14
