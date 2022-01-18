# betareg_residual_analysis_December_vaccine_data.R

## Exmaple codes of beta regression
# test the 'residuals' function in betareg package

require(betareg)
require(magrittr)
require(dplyr)
options(digits = 6)
data("GasolineYield", package = "betareg")
gy <- betareg(yield ~ gravity + pressure + temp10 + temp, data = GasolineYield)
gy_res <- cbind(
  gy$
  residuals(gy, type = "pearson"),
  residuals(gy, type = "deviance"),
  residuals(gy, type = "response"),
  residuals(gy, type = "weighted"),
  residuals(gy, type = "sweighted"),
  residuals(gy, type = "sweighted2")
)
colnames(gy_res) <- c("pearson", "deviance", "response",
                      "weighted", "sweighted", "sweighted2")
pairs(gy_res)

# statndardised residuals vs. indices of obs.
plot(residuals(gy, type = "pearson"))

# deviance residuals. vs. indices of obs.
plot(residuals(gy, type = "deviance"))

plot(cooks.distance(gy))

plot(gy, which=1:4, type="pearson")
plot.betareg

## Ferrari and Cribari-Neto (2004)
gy2 <- betareg(yield ~ batch + temp, data = GasolineYield)
## Table 1
summary(gy2)
## Figure 2
par(mfrow = c(3, 2))
plot(gy2, which = 1, type = "pearson", sub.caption = "")
plot(gy2, which = 1, type = "deviance", sub.caption = "")
plot(gy2, which = 5, type = "deviance", sub.caption = "")
plot(gy2, which = 4, type = "pearson", sub.caption = "")
plot(gy2, which = 2:3)
par(mfrow = c(1, 1))

############################
## the code for this study
# read data. Change folder if necessary
# the optimal accessibility model is 3SFCA with d=30 miles
fca<- read.csv("accessibility_imd_ethnic_exclude.csv", sep=",")
fca$MSOADECILE <- factor(fca$MSOADECILE, levels = c(1,2,3,4,5,6,7,8,9,10))

# fit 3SFCA and 30-mile
scfa3_30 <- betareg(vaccination_percentage_2nddose ~ X3sfca_30 + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
                   data = fca)
summary(scfa3_30)
AIC(scfa3_30)

# pearson residual
plot(residuals(scfa3_30, type = "pearson"))
plot(residuals(scfa3_30, type = "deviance"))
plot(cooks.distance(scfa3_30))

plot(scfa3_30, which = 1:4)

par(mfrow = c(3, 2))
plot(scfa3_30, which = 1, type = "pearson", sub.caption = "")
plot(scfa3_30, which = 1, type = "deviance", sub.caption = "")
plot(scfa3_30, which = 5, type = "deviance", sub.caption = "")
plot(scfa3_30, which = 4, type = "pearson", sub.caption = "")
plot(scfa3_30, which = 2:3)

# largest standardised residual in absolute value

residual_standard <- residuals(scfa3_30, type = "pearson")
residual_standard[abs(residual_standard) > 6] %>% length()
# 5

list_msoa_std_residual_dev_gt_6 <- residual_standard[abs(residual_standard) > 6] %>% names() %>% as.integer()
# removing these 5 points that have standardised residual in absolute value greater than 6
scfa3_30_update <- update(scfa3_30, subset = -list_msoa_std_residual_dev_gt_6)
summary(scfa3_30_update)
AIC(scfa3_30_update)

# copy the table of coefficients into clipboard
write.table(summary(scfa3_30_update)$coef, "clipboard-16384", sep = "\t", quote = FALSE)

# largest deviance residual in absolute value (greater than 4)
residual_dev <- residuals(scfa3_30, type = "deviance")
residual_dev[abs(residual_dev) > 4] %>% length()
# 27

residual_dev[abs(residual_dev) > 4]
# 712    726    917   1359   1368   1377   1807   1808   1811   1868   2101   2492   2530   2676   2680   2681   2686   2687   2905   3263   3691   3750   4371 
# 4.342 -4.493 -7.227 -5.612 -5.519 -4.010  7.592  4.106  7.671 -4.644  6.367  6.860 -4.382  5.959  4.391  5.149  4.239  4.855  4.368 -6.539  4.705  5.121 -5.283 
# 4859   4871   5689   5730 
# 6.513  4.257  6.101 -4.493 

# largest gleverage
sort(gleverage(scfa3_30), decreasing = T) %>% head(40)
glev <- gleverage(scfa3_30)
glev[glev > 1] %>% length()
glev[glev > 1]

cooks_dist <- cooks.distance(scfa3_10)


## Examples of MSOA with the lowest uptake rate and lowest accessibility score
# which msoa has the lowest fitted value
serial_num_lowest_fitted_uptake <- which.min(scfa3_30$fitted.values)
# 396
serial_num_lowest_access_score <- which.min(fca$X3sfca_30)
# 2207

# creating the subset data
fca_subset <- fca %>% select(MSOA11CD, access = X3sfca_30, IMD_decile = MSOADECILE, car_ownership = Per_cent_of_households_with_at_least_one_car_or_van, White., Mixed., Asian., Black., Other., uptake = vaccination_percentage_2nddose) %>%
  mutate(fitted_uptake = scfa3_30$fitted.values)

# subset of the two special MSOAs
fca_subset[c(serial_num_lowest_fitted_uptake, serial_num_lowest_access_score),]
# copied to clipboard
fca_subset[c(serial_num_lowest_fitted_uptake, serial_num_lowest_access_score),] %>% write.table("clipboard-16384", sep = "\t", quote = FALSE)

fca_subset_ori_name <- fca %>% 
  select(MSOA11CD, X3sfca_30, MSOADECILE, Per_cent_of_households_with_at_least_one_car_or_van, White., Mixed., Asian., Black., Other., vaccination_percentage_2nddose)

## increase access by 1
msoa_lowest_fitted_uptake <- fca_subset_ori_name[serial_num_lowest_fitted_uptake,]
msoa_lowest_fitted_uptake$X3sfca_30 <- msoa_lowest_fitted_uptake$X3sfca_30 + 1
# original
predict(scfa3_30, fca_subset_ori_name[serial_num_lowest_fitted_uptake,])
# increased access
predict(scfa3_30, msoa_lowest_fitted_uptake)

rate_1 = 0.2707 
rate_2 = 0.2742
identical(rate_2 / (1-rate_2) / (rate_1 / (1-rate_1)), exp(0.018))

msoa_lowest_access <- fca_subset_ori_name[serial_num_lowest_access_score,]
msoa_lowest_access$X3sfca_30 <- msoa_lowest_access$X3sfca_30 + 1
# original
predict(scfa3_30, fca_subset_ori_name[serial_num_lowest_access_score,])
# increased access
predict(scfa3_30, msoa_lowest_access)

## increase decile to 10
msoa_lowest_fitted_uptake <- fca_subset_ori_name[serial_num_lowest_fitted_uptake,]
msoa_lowest_fitted_uptake$MSOADECILE <- '2'
# original
predict(scfa3_30, fca_subset_ori_name[serial_num_lowest_fitted_uptake,])
# increased access
predict(scfa3_30, msoa_lowest_fitted_uptake)

msoa_lowest_access <- fca_subset_ori_name[serial_num_lowest_access_score,]
msoa_lowest_access$MSOADECILE <- '10'
# original
predict(scfa3_10, fca_subset_ori_name[serial_num_lowest_access_score,])
# increased access
predict(scfa3_10, msoa_lowest_access)

## increase car ownership by 0.01
msoa_lowest_fitted_uptake <- fca_subset_ori_name[serial_num_lowest_fitted_uptake,]
msoa_lowest_fitted_uptake$Per_cent_of_households_with_at_least_one_car_or_van <- msoa_lowest_fitted_uptake$Per_cent_of_households_with_at_least_one_car_or_van + 0.01
# original
predict(scfa3_30, fca_subset_ori_name[serial_num_lowest_fitted_uptake,])
# increased access
predict(scfa3_30, msoa_lowest_fitted_uptake)

msoa_lowest_access <- fca_subset_ori_name[serial_num_lowest_access_score,]
msoa_lowest_access$Per_cent_of_households_with_at_least_one_car_or_van <- msoa_lowest_access$Per_cent_of_households_with_at_least_one_car_or_van + 0.01
# original
predict(scfa3_30, fca_subset_ori_name[serial_num_lowest_access_score,])
# increased access
predict(scfa3_30, msoa_lowest_access)

####
# save the standardised residual plot as tiff (DPI=300)
# using tiff() or Cairo(). If these methods do not work, in the RStudio 'Plots' window, save it as a pdf file and then use GIMP software to convert it to tiff with DPI=300
par(mfrow = c(1, 1))

tiff("beta_regression_standardised_residual_plot", units="in", res=300)
plot(scfa3_30, which = 1, type = "pearson", sub.caption = "", caption = "Standardised residual plot", pch = 4)
rect(0, -8, 6000, -6, col = NA, border = "red")
# insert ggplot code
dev.off()