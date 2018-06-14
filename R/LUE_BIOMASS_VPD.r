#' @title Light Use Efficiency Model to Estimate Biomass with Vapour Pressure Deficit
#' @usage LUE_BIOMASS_VPD(fpar_raster,par,tmin,tmax,tdew,
#' tmin_min,tmin_max,vpd_max, vpd_min,LUE_optimal)
#' @format A Biomass raster
#' @description LUE_BIOMASS_VPD() to estimate aboveground biomass firstly by calculating the Absorbed Photosynthetically Active Radiation (APAR) and secondly the actual values of light use efficiency by including vapour pressure deficit of the crops Shi et al.(2007) <doi:10.2134/agronj2006.0260>.
#' @param fpar_raster fraction of photosynthetically active radiation (fpar) per day raster with .tif format
#' @param par clear sky surface photosynthetically active radiation (par) per day raster with .nc file format.
#' @param tmin Minimum temperature at 2 metres since previous post-processing per day raster with .nc file format.
#' @param tmax Maximum temperature at 2 metres since previous post-processing per day raster with .nc file format.
#' @param tdew Dewpoint temperature at 2 metres since previous post-processing per day raster with .nc file format.
#' @param tmin_min minimum value of tmin used for the threshold
#' @param tmin_max maximum value of tmin used for the threshold
#' @param vpd_min minimum value of vapour pressure deficit used for the threshold
#' @param vpd_max maximum value of vapour pressure deficit used for the threshold
#' @param LUE_optimal optical lue value with respect to crop type for example wheat crop LUE_optimal is 3.0 (Djumaniyazova et al., 2010)
#' @import fpar,par,tmin
#' @export
#' @references Djumaniyazova Y, Sommer R, Ibragimov N, Ruzimov J, Lamers J & Vlek P (2010) Simulating water use and N response of winter wheat in the irrigated floodplains of Northwest Uzbekistan. Field Crops Research 116, 239-251.
#' @references Shi Z, Ruecker G R,Mueller M, Conrad C, Ibragimov N, Lamers J P A, Martius C, Strunz G, Dech S & Vlek P L G (2007) Modeling of Cotton Yields in the Amu Darya River Floodplains of Uzbekistan Integrating Multitemporal Remote Sensing and Minimum Field Data. Agronomy Journal 99, 1317-1326.
#' @keywords datasets
#' @return Biomass raster
#' @examples \dontrun{
#' ## load the data
#' data(fpar)
#' data(par1)
#' data(tmin)
#' data(tmax)
#' data(tdew)
#' LUE_BIOMASS_VPD(fpar,par1,tmin,tmax,tdew,-2,12,1.5,4,3)
#' }
#' @examples
#' library(raster)
#' fparr <- raster(nc=2, nr=2)
#' values(fparr)<-runif(ncell(fparr),min =0.2,max= 0.8)
#' par11<- brick(nc=2, nr=2, nl=2)
#' values(par11)<-runif(ncell(par11),min =169076.9,max= 924474.6)
#' tminn <- brick(nc=2, nr=2, nl=2)
#' values(tminn)<-runif(ncell(tminn),min = 278,max= 281)
#' tmaxx <- brick(nc=2, nr=2, nl=2)
#' values(tmaxx)<-runif(ncell(tmaxx),min = 278,max= 281)
#' tdeww <- brick(nc=2, nr=2, nl=2)
#' values(tdeww)<-runif(ncell(tdeww),min = 278,max= 281)
#' LUE_BIOMASS_VPD(fparr,par11,tminn,tmaxx,tdeww,-2,12,1.5,4,3)

LUE_BIOMASS_VPD<-function(fpar_raster,par,tmin,tmax,tdew,tmin_min,tmin_max,vpd_max, vpd_min,LUE_optimal) {
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
  tmax_1<-mean(as.vector(tmax))-273.15
  tdew_1<-mean(as.vector(tdew))-273.15
  # applying the criteria with diffrent thresholds of tmin for every crop

  if (tmin_1 <= tmin_min){
    tmin_1 <- 0
  }
  if (tmin_1 >= tmin_max){
    tmin_1 <- 1
  }
  if(tmin_1 > tmin_min & tmin_1<tmin_max) {
    tmin_1<- (tmin_1 - tmin_min)* ((1/(tmin_max-tmin_min)))
  }
  # calculating VPD


  Es_min<-0.6108*exp(((17.27*tmin_1)/(tmin_1+237.3)))
  Es_max<-0.6108*exp(((17.27*tmax_1)/(tmax_1+237.3)))
  Es_day<-(Es_max+Es_min)/2
  Ea_day<-0.6108*exp(((17.27*tdew_1)/(tdew_1+237.3)))
  VPD_day<-Es_day-Ea_day

  if (VPD_day <= vpd_min){
    vpd_sv <- 1
  }
  if (VPD_day >= vpd_max){
    vpd_sv <- 0
  }
  if(VPD_day > vpd_min & VPD_day < vpd_max)  {
    vpd_sv <- (VPD_day - vpd_min)* ((1/(vpd_max-vpd_min)))
  }

  lue_act <- tmin_1 * LUE_optimal* vpd_sv
  biomass<-apar*lue_act
  return(biomass)
}


