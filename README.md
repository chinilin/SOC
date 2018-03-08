# SOC
## Large-scale digital mapping of soil organic carbon content by using machine learning algorithms

This repo describes the development and accuracy assessment of soil organic carbon content digitals maps: the case of Voronezh oblast, Central Russian Upland. The study area is located at steppe zone. Soil cover is represented by Haplic and Gleyic Chernozems. The spatial prediction was based on the 22 sampling points (used for training and accuracy assessment) and a set of covariates represented by digital elevation models, its derivatives and remote sensing data. We calibrated next models: Random Forest (RF), Gradient Boosting Machine (GBM) and Bayesian Additive Regression Trees (BART) and uses these models to predict soil organic carbon content across the study area. For accuracy assessment, we used 5-fold repeated cross-validation and for each model we derived the coefficient of determination (R-squared), mean absolute error (MAE) and root mean square error(RMSE). The results of spatial prediction show that models obtained on DEM, its derivatives and Landsat 8 data explains 60-70% of the variance, while models obtained on DEM, its derivatives and Sentinel 2 data explains 47-55% of variance.

![research area](https://github.com/chinilin/SOC/blob/master/research_area.png)

 Citation:
 * Chinilin A.V., Savin I. Yu. [The large scale digital mapping of soil organic carbon using machine learning algorithms](http://esoil.ru/publications/bulletin/ns912018/912018ns4.html), Dokuchaev Soil Bulletin, 2018, Vol. 91, pp. 46-62. doi: 10.19047/0136-1694-2018-91-46-62
 
Contact: andreychinilin@gmail.com