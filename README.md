<!-- README.md is generated from README.Rmd. Please edit that file -->
lue
===

The purpose of designing light use efficiency model in r is to calculate the biomass and yield of any crop .Many climatic parametres which affect the biomass of any crop should also been considered in this model. The basic working of this code starts with the input variables named Fraction of photosynthetically active radiation (fPAR) and clear sky Photosynthetically active radiation (PAR). Both these parameters are used to calculate the Absorbed photosynthetically active radition (APAR). Second step of this model is to calculate the actual Light use efficiency (LUE) by considering the minimum tempertature (tmin), maximum temperature (tmax) and dewpoint tempertaure (tdew)and optimal LUE. The maximum and dew point temperature is further used to calculate vapour pressure deficit (vpd). For example, the optimal LUE for wheat is 3.0 (Djumaniyazova et al., 2010), in that case we have to consider the optimal value 3 and then to multipy with tmin and vpd. In this study we consider the maximum and minimum values of minimum temperature and vpd which means that the minmum teperature and vpd which destroys the crop would get the actual LUE value equals 0 and maximum temperature and vpd, which is more than suitable for any crop would have the actal LUE equals to optimal LUE.The final step includes the estimation of biomass which is being calculated by multipying the outputs of step 1 i.e. APAr and step 2 i.e. actual light use efficiency.

Working
=======

The working of this model is very simple by using "LUE\_BIOMASS(), LUE\_BIOMASS\_VPD,LUE\_YIELD(), LUE\_YIEL\_VPD" in the package named "lue".The input variables are same as described in the description with fpar raster, par file, tmin,tmax, tdew, tmin\_mim,tmin\_max, vpd\_max,vpd\_min,Lue\_optimal. In much brief we will see it in the code below.

Example
-------

``` r
#step1: Loading fpar, par and tmin data from data file of the package
 library(raster)
#> Loading required package: sp
 fparr <- raster(nc=2, nr=2)
 values(fparr)<-runif(ncell(fparr),min =0.2,max= 0.8)
 par11<- brick(nc=2, nr=2, nl=2)
 values(par11)<-runif(ncell(par11),min =169076.9,max= 924474.6)
 tminn <- brick(nc=2, nr=2, nl=2)
 values(tminn)<-runif(ncell(tminn),min = 278,max= 281)
 tmaxx <- brick(nc=2, nr=2, nl=2)
 values(tmaxx)<-runif(ncell(tmaxx),min = 278,max= 281)
 tdeww <- brick(nc=2, nr=2, nl=2)
 values(tdeww)<-runif(ncell(tdeww),min = 278,max= 281)
 
```

``` r
#step2: Generating the function
LUE_BIOMASS<-function(fpar_raster,par,tmin,tmin_min,tmin_max,LUE_optimal) {
      #Summing the PAR for a day
      par<-sum(par)
      # converting PAR from J*m^-2 to MJ*m^-2
      par <- par/1000000 # convert PAR from J*m^-2 to MJ*m^-2
      #par1 <- projectRaster(pa1r, fpar_raster, method = "bilinear", verbose = TRUE)
      # calculating apar by multipying par with fpar
      apar <- par * fpar_raster
      # including tmin with a mean value in a day and making it in degree celsius
      tmin1<-mean(as.vector(tmin))-273.15
      # applying the criteria with diffrent thresholds of tmin for every crop
      # Threshold values tmin:min and tmin_max depends on the crop type. For example in this case we have       considered the wheat case with tmin_min = -2 degree celsius (Single 1985) and tmin:max = 12 degree       celsius (Russel and Wilson, 1994). 
          if (tmin1 <= tmin_min){
            tmin1 <- 0
                                  }
          else if (tmin1 >= tmin_max){
            tmin1 <- 1
                                        }
          else {
          tmin1<- (tmin1 - tmin_min)* ((1/(tmin_max-tmin_min)))
          }
          # Result of tmin obtained from the thresholds should be multiplied with optimal LUE
          # In this case we have considered otimal values of wheat crop i.e. 3.0.
lue_act <- tmin1 * LUE_optimal
biomass<-apar*lue_act
# the function returns the biomass
return(biomass)
}
```

``` r
# Calculationg yield of any crop

LUE_YIELD<-function(fpar_raster,par,tmin,tmin_min,tmin_max,LUE_optimal) {
  #Summing the PAR for a day
  #par1<-as.vector(par)

  par_1<-sum(par)
  # converting PAR from J*m^-2 to MJ*m^-2
  par_1 <- par_1/1000000 # convert PAR from J*m^-2 to MJ*m^-2
  #par1 <- projectRaster(pa1r, fpar_raster, method = "bilinear", verbose = TRUE)
  # calculating apar by multipying par with fpar
  apar <- par_1 * fpar_raster
  # including tmin with a mean value in a day and making it in degree celsius
  tmin_1<-mean(as.vector(tmin))-273.15
  # applying the criteria with diffrent thresholds of tmin for every crop

  if (tmin_1 <= tmin_min){
    tmin_1 <- 0
  }
  else if (tmin_1 >= tmin_max){
    tmin_1 <- 1
  }
  else {
    tmin_1<- (tmin_1 - tmin_min)* ((1/(tmin_max-tmin_min)))
  }
  lue_act <- tmin_1 * LUE_optimal
  biomass<-apar*lue_act
  yield<-(-106.97)+(0.47*biomass)
  return(yield)
}
```

``` r
# Calling the function
Biomass<-LUE_BIOMASS(fparr,par11,tminn,-2,12,3)
Biomass
#> class       : RasterLayer 
#> dimensions  : 2, 2, 4  (nrow, ncol, ncell)
#> resolution  : 180, 90  (x, y)
#> extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
#> coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
#> data source : in memory
#> names       : layer 
#> values      : 0.8051856, 1.623377  (min, max)

yield<-LUE_YIELD(fparr,par11,tminn,-2,12,3)
yield
#> class       : RasterLayer 
#> dimensions  : 2, 2, 4  (nrow, ncol, ncell)
#> resolution  : 180, 90  (x, y)
#> extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
#> coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
#> data source : in memory
#> names       : layer 
#> values      : -106.5916, -106.207  (min, max)
```

References
----------

Djumaniyazova Y, Sommer R, Ibragimov N, Ruzimov J, Lamers J & Vlek P (2010) Simulating water use and N response of winter wheat in the irrigated floodplains of Northwest Uzbekistan. Field Crops Research 116, 239-251.

Shi Z, Ruecker G R,Mueller M, Conrad C, Ibragimov N, Lamers J P A, Martius C, Strunz G, Dech S & Vlek P L G (2007) Modeling of Cotton Yields in the Amu Darya River Floodplains of Uzbekistan Integrating Multitemporal Remote Sensing and Minimum Field Data. Agronomy Journal 99, 1317-1326.

Single, W.V., 1985. Frost injury and the physiology of the wheat winter wheat plant. J. Aust. Inst. Agric. Sci. 51 (2), 128–134.

Russell, G., Wilson, G.W., 1994. An Agri-Pedo-Climatological 27 Knowledge-Base of Wheat in Europe. Joint Research Centre, European Commission, Luxembourg, pp. 158.
