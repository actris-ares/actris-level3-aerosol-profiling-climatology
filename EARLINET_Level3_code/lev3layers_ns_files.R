load("lev3int_layers.Rda")
load("lev2db_layers_base_top.Rda")
load("System.Rda")
load("lev2db.Rda")

#db_int_layers[,13:26]=round(db_int_layers[,13:26],6)
db_int_layers$Extinction = db_int_layers$Extinction * 10 ^ 3
db_int_layers$Error_Extinction = db_int_layers$Error_Extinction * 10 ^ 3
db_int_layers$IntBs = db_int_layers$IntBs * 10 ^ 3
db_int_layers$Error_IntBs = db_int_layers$Error_IntBs * 10 ^ 3
db_int_layers$Backscatter = db_int_layers$Backscatter * 10 ^ 6
db_int_layers$Error_Backscatter = db_int_layers$Error_Backscatter * 10 ^
  6

vrm = c(16, 17, 26)

loc=c("Aberystwyth, United Kingdom","Andoya, Norway","Athens, Greece","Barcelona, Spain",
      "Cabauw, Netherlands","Belsk, Poland","Dushanbe, Tajikistan","Evora, Portugal",
      "Garmisch-Partenkirchen, Germany","Granada, Spain","Hamburg, Germany","Observatory Hohenpeissenberg, Germany",
      "Bucharest, Romania","Ispra, Italy","Kuehlungsborn, Germany","Kuopio, Finland",
      "L'Aquila, Italy","Leipzig, Germany","Linkoping, Sweden","Lille, France",
      "Minsk, Belarus","Madrid, Spain","Melpitz, Germany","Maisach, Germany",
      "Naples, Italy","Payerne, Switzerland","Potenza, Italy","Clermont-Ferrand, France",
      "Roma-Tor Vergata, Italy","Lecce, Italy","Palaiseau, France","Sofia, Bulgaria",
      "St. Petersburg, Russia","Thessaloniki, Greece","Cork, Ireland","Warsaw, Poland")
loc = loc[-vrm]

loc2 = unique(lev2db$Station)

latit=c(51.858,69.278,37.96,41.393,51.97,51.83,38.5594,38.5678,47.477,37.164,53.568,
        47.8019,44.348,45.8167,54.12,62.7333,42.344,51.35,58.392,50.6117,53.917,
        40.4565,51.53,48.209,40.838,46.8167,40.6,45.761,41.833,40.333,48.713,
        42.65,59.9427,40.63,51.8933,52.21)
latit = latit[-vrm]

longit=c(-4.252,16.008,23.78,2.12,4.93,20.78,68.8561,-7.9115,11.064,-3.605,9.973,11.0119,
         26.029,8.6167,11.77,27.55,13.327,12.433,15.575,3.1417,27.605,-3.7257,12.93,
         11.258,14.183,6.9333,15.72,3.111,12.65,18.1,2.208,23.38,30.273,22.95,-8.4942,
         20.98)
longit = longit[-vrm]

station_alt=c(15,380,212,115,0,180,864,293,730,680,25,974,93,209,70,190,683,125,70,
              60,200,669,84,516,118,491,760,420,110,30,156,550,35,50,75,112)
station_alt = station_alt[-vrm]

institution=c("University of Wales, Aberystwyth","ALOMAR/Andøya Space Center",
              "National Technical University of Athens, Physics Department",
              "Universitat Politecnica de Catalunya, Barcelona",
              "Royal Netherlands Meteorological Institute","Institute of Geophysics, Polish Academy of Sciences, Warsaw",
              "Leibniz Institute for Tropospheric Research","Institute for Earth Sciences(Instituto de Ciencias da Terra)",
              "Karlsruher Institut für Technologie, Garmisch-Partenkirchen",
              "Andalusian Institute for Earth System Research, University of Granada",
              "Max-Planck-Institut für Meteorologie","DWD Meteorological Observatory Hohenpeissenberg",
              "National Institute of R&D for Optoelectronics","Joint Research Centre - Institute for Environment and Sustainability, Ispra",
              "Leibniz Institute for Atmospheric Physics","Finnish Meteorological Institute, Atmospheric Research Centre of Eastern Finland, Kuopio",
              "Università degli Studi dell’Aquila - Dipartimento di Fisica, L’Aquila",
              "Leibniz Institute for Tropospheric Research","Swedish Defence Research Agency",
              "Lille 1 University - Science and Technology","B.I. Stepanov Institute of Physiscs, Minsk",
              "Centro de Investigaciones Energéticas, Medioambientales y Tecnológicas, Madrid",
              "Leibniz Institute for Tropospheric Research","Meteorologisches Institut der Ludwig-Maximilians-Universität, München",
              "Dipartimento di Fisica - Università degli Studi di Napoli Federico II",
              "MeteoSwiss Aerological Station, Payerne, Losanne",
              "Consiglio Nazionale delle Ricerche - Istituto di Metodologie per l'Analisi Ambientale, Potenza",
              "Observatoire de Physique du Globe","Consiglio Nazionale delle Ricerche - Istituto di Scienze Marine",
              "University of Salento, Physics Department, Lecce","Centre National de la Recherche Scientifique - Institut Pierre Simon Laplace, Paris",
              "Institute of Electronics, Bulgarian Academy of Sciences, Sofia",
              "Research Park of Saint-Petersburg State University - Observatory of Environmental Safety",
              "Aristotle University of Thessaloniki","Phys. Dep. and Env. Res. Institute, University College Cork",
              "University of Warsaw, Faculty of Physics")
institution = institution[-vrm]

acronym=c("UWA","ALOMAR","NTUA","TSC-UPC","KNMI","IGF PAN","TROPOS","ICT","KIT","IISTA-CEAMA",
          "DKRZ","DWD","INOE","JRC","IAP","FMI","CETEMPS","TROPOS","FOI","USTL","IFAN",
          "CIEMAT","TROPOS","MIM-LMU","UNINA","EPFL","CNR-IMAA","OPGC-LaMP","CNR-ISAC",
          "UNISALENTO","SIRTA","IE-BAS","SPBU","AUTH","UCC","IGF-FUW")
acronym = acronym[-vrm]

PI=c("Geraint Vaughan","Michael Gausa","Alexandros Papayannis","Adolfo Comeron",
     "Arnoud Apituley","Aleksander Pietruczuk","Dietrich Althausen","Daniele Bortoli",
     "Hannes Vogelmann","Lucas Alados Arboledas","Jens Bosenberg","Ina Mattis",
     "Doina Nicolae","Jean Putaud","Matthias Alpers","Mika Komppula","Vincenzo Rizi",
     "Holger Baars, Ulla Wandinger","Ove Gustafsson","Philippe Goloub","Anatoli Chaikovsky",
     "Manuel Pujadas","Holger Baars","Matthias Wiegner","Salvatore Amoruso, Nicola Spinelli",
     "Alexander Haefele","Aldo Amodeo, Gelsomina Pappalardo","Patrick Freville",
     "Davide Dionisi","Maria Rita Perrone","Christophe Pietras, Martial Haeffelin, Francois Ravetta, Jacques Pelon",
     "Dimitar V. Stoyanov","Dmitry Samulenkov","Dimitris Balis","Albert A. Ruth",
     "Iwona S. Stachlewska")
PI = PI[-vrm]

PI_contact=c("","michael.gausa@rocketrange.no","apdlidar@central.ntua.gr",
             "comeron@tsc.upc.es","apituley@knmi.nl","alek@igf.edu.pl","dietrich@tropos.de",
             "db@uevora.pt","hannes.vogelmann@kit.edu","alados@ugr.es","boesenberg@dkrz.de",
             "Ina.Mattis@dwd.de","nnicol@inoe.ro","jean.putaud@ec.europa.eu","",
             "mika.komppula@fmi.fi","vincenzo.rizi@aquila.infn.it","baars@tropos.de",
             "ove.gustafsson@foi.se","philippe.goloub@univ-lille1.fr","chaikov@dragon.bas-net.by",
             "manuel.pujadas@ciemat.es","baars@tropos.de","m.wiegner@lmu.de",
             "salvatore.amoruso@unina.it","Alexander.Haefele@meteoswiss.ch",
             "aldo.amodeo@imaa.cnr.it","P.Freville@opgc.fr","d.dionisi@isac.cnr.it",
             "maria.rita.perrone@le.infn.it","christophe.pietras@lmd.polytechnique.fr",
             "dvstoyan@ie.bas.bg","dmitriy.samulenkov@spbu.ru","balis@auth.gr",
             "a.ruth@ucc.ie","iwona.stachlewska@igf.fuw.edu.pl")
PI_contact = PI_contact[-vrm]

syst = as.matrix(syst_df)

sist = rep(NA, times = 33)  # Codice di Sergio: sist=rep(NA,times=36)
sist[1] = syst[1, 1]
sist[2] = syst[2, 11]
sist[3] = "NTUA Raman Lidar System ; EOLE"
sist[4] = syst[4, 19]
sist[5] = syst[5, 11]
sist[6] = syst[6, 16]
sist[7] = syst[7, 16]
sist[8] = syst[8, 13]
sist[9] = syst[9, 18]
sist[10] = syst[10, 18]
sist[11] = syst[11, 2]
sist[12] = syst[12, 19]
sist[13] = syst[13, 20]
sist[14] = "JRC Ispra ; ADAM noew"
sist[15] = syst[15, 3]
sist[16] = syst[16, 17]
sist[17] = syst[17, 13]
sist[18] = paste0(syst[18, 14], " ; Arielle")
sist[19] = syst[19, 10]
sist[20] = syst[20, 18]
sist[21] = paste0(syst[21, 19], " ; MSTL-2")
sist[22] = syst[22, 19]
sist[23] = paste0(syst[23, 16], " ; ", syst[23, 18])
sist[24] = syst[24, 16]
sist[25] = syst[25, 18]
sist[26] = syst[26, 19]
sist[27] = "PEARL ; MUSA"
sist[28] = syst[28, 20]
sist[29] = syst[29, 20]
sist[30] = syst[30, 20]
sist[31] = paste0(syst[31, 11], " ; ", syst[31, 19])
sist[32] = syst[32, 20]
sist[33] = syst[33, 20]
# sist[34] = syst[34, 20]  # Codice di Sergio: righe 34, 35 e 36 non commentate
# sist[35] = syst[35, 18]
# sist[36] = syst[36, 20]
sist = sist[-vrm]

altitude_breaks = seq(from = 0, to = 20000, by = 1000)
altitude_intervals_data = altitude_breaks[1:20]

vvv = db_int_layers$LidarRatio
vvv = vvv[!is.na(vvv)]
nnn = as.numeric(quantile(vvv, probs = c(0.1, 0.9)))
k = (nnn[2] - nnn[1]) / 18
lr_breaks = c(min(vvv), seq(from = nnn[1], to = nnn[2], by = k), max(vvv))
lr_intervals_data = lr_breaks[1:20]

vvv = db_int_layers$ParticleDep
vvv = vvv[!is.na(vvv)]
nnn = as.numeric(quantile(vvv, probs = c(0.1, 0.9)))
k = (nnn[2] - nnn[1]) / 18
pd_breaks = c(min(vvv), seq(from = nnn[1], to = nnn[2], by = k), max(vvv))
pd_intervals_data = pd_breaks[1:20]

vvv = db_int_layers$AOD
vvv = vvv[!is.na(vvv)]
nnn = as.numeric(quantile(vvv, probs = c(0.1, 0.9)))
k = (nnn[2] - nnn[1]) / 18
aod_breaks = c(min(vvv), seq(from = nnn[1], to = nnn[2], by = k), max(vvv))
aod_intervals_data = aod_breaks[1:20]

vvv = db_int_layers$Extinction
vvv = vvv[!is.na(vvv)]
nnn = as.numeric(quantile(vvv, probs = c(0.1, 0.9)))
k = (nnn[2] - nnn[1]) / 18
extinction_breaks = c(min(vvv), seq(from = nnn[1], to = nnn[2], by = k), max(vvv))
extinction_intervals_data = extinction_breaks[1:20]

vvv = db_int_layers$IntBs
vvv = vvv[!is.na(vvv)]
nnn = as.numeric(quantile(vvv, probs = c(0.1, 0.9)))
k = (nnn[2] - nnn[1]) / 18
ib_breaks = c(min(vvv), seq(from = nnn[1], to = nnn[2], by = k), max(vvv))
ib_intervals_data = ib_breaks[1:20]

vvv = db_int_layers$Backscatter
vvv = vvv[!is.na(vvv)]
nnn = as.numeric(quantile(vvv, probs = c(0.1, 0.9)))
k = (nnn[2] - nnn[1]) / 18
backscatter_breaks = c(min(vvv), seq(from = nnn[1], to = nnn[2], by = k), max(vvv))
backscatter_intervals_data = backscatter_breaks[1:20]

ssn = list()
ssn[[1]] = 3:5
ssn[[2]] = 6:8
ssn[[3]] = 9:11
ssn[[4]] = c(1, 2, 12)

bl_counts = matrix(NA, nrow = 4, ncol = 20)
tl_counts = matrix(NA, nrow = 4, ncol = 20)
lr_counts = array(NA, dim = c(4, 3, 20))
pd_counts = array(NA, dim = c(4, 3, 20))
aod_counts = array(NA, dim = c(4, 3, 20))
extinction_counts = array(NA, dim = c(4, 3, 20))
com_counts = array(NA, dim = c(4, 3, 20))
ib_counts = array(NA, dim = c(4, 3, 20))
backscatter_counts = array(NA, dim = c(4, 3, 20))

time_bounds_data = matrix(NA, nrow = 2, ncol = 4)
time_bounds_data[, 4] = c(as.numeric(
  as.POSIXct("1999-12-01 00:00:00", tz = "GMT", origin = "1970-01-01 00:00:00")
), as.numeric(
  as.POSIXct("2019-02-28 23:59:59", tz = "GMT", origin = "1970-01-01 00:00:00")
))
time_bounds_data[, 1] = c(as.numeric(
  as.POSIXct("2000-03-01 00:00:00", tz = "GMT", origin = "1970-01-01 00:00:00")
), as.numeric(
  as.POSIXct("2019-05-31 23:59:59", tz = "GMT", origin = "1970-01-01 00:00:00")
))
time_bounds_data[, 2] = c(as.numeric(
  as.POSIXct("2000-06-01 00:00:00", tz = "GMT", origin = "1970-01-01 00:00:00")
), as.numeric(
  as.POSIXct("2019-08-31 23:59:59", tz = "GMT", origin = "1970-01-01 00:00:00")
))
time_bounds_data[, 3] = c(as.numeric(
  as.POSIXct("2000-09-01 00:00:00", tz = "GMT", origin = "1970-01-01 00:00:00")
), as.numeric(
  as.POSIXct("2019-11-30 23:59:59", tz = "GMT", origin = "1970-01-01 00:00:00")
))

for (p in 1:33)
{
  for (k in 1:4)
  {
    db_355 = db_int_layers[db_int_layers$Station == loc2[p] &
                             as.numeric(db_int_layers$Month) %in% ssn[[k]] &
                             as.numeric(db_int_layers$Wavelength) == 355, ]
    db_532 = db_int_layers[db_int_layers$Station == loc2[p] &
                             as.numeric(db_int_layers$Month) %in% ssn[[k]] &
                             as.numeric(db_int_layers$Wavelength) == 532, ]
    db_1064 = db_int_layers[db_int_layers$Station == loc2[p] &
                              as.numeric(db_int_layers$Month) %in% ssn[[k]] &
                              as.numeric(db_int_layers$Wavelength) == 1064, ]
    db1 = layers_base_top[layers_base_top$Station == loc2[p] &
                            as.numeric(layers_base_top$Month) %in% ssn[[k]], ]
    pd_vtt_355 = as.numeric(db_355$ParticleDep[db_355$Type == "b" |
                                                 db_355$PdF == 1])
    pd_vtt_532 = as.numeric(db_532$ParticleDep[db_532$Type == "b" |
                                                 db_532$PdF == 1])
    pd_vtt_1064 = as.numeric(db_1064$ParticleDep)
    bl_counts[k, ] = hist(db1$Base_Layer, breaks = altitude_breaks, plot = F)$counts
    tl_counts[k, ] = hist(db1$Top_Layer, breaks = altitude_breaks, plot = F)$counts
    lr_counts_355 = hist(db_355$LidarRatio, breaks = lr_breaks, plot = F)$counts
    lr_counts_532 = hist(db_532$LidarRatio, breaks = lr_breaks, plot = F)$counts
    lr_counts_1064 = hist(db_1064$LidarRatio, breaks = lr_breaks, plot = F)$counts
    lr_counts[k, , ] = rbind(lr_counts_355, lr_counts_532, lr_counts_1064)
    pd_counts_355 = hist(pd_vtt_355, breaks = pd_breaks, plot = F)$counts
    pd_counts_532 = hist(pd_vtt_532, breaks = pd_breaks, plot = F)$counts
    pd_counts_1064 = hist(pd_vtt_1064, breaks = pd_breaks, plot = F)$counts
    pd_counts[k, , ] = rbind(pd_counts_355, pd_counts_532, pd_counts_1064)
    aod_counts_355 = hist(db_355$AOD, breaks = aod_breaks, plot = F)$counts
    aod_counts_532 = hist(db_532$AOD, breaks = aod_breaks, plot = F)$counts
    aod_counts_1064 = rep(NA, times = 20)
    aod_counts[k, , ] = rbind(aod_counts_355, aod_counts_532, aod_counts_1064)
    extinction_counts_355 = hist(db_355$Extinction, breaks = extinction_breaks, plot =
                                   F)$counts
    extinction_counts_532 = hist(db_532$Extinction, breaks = extinction_breaks, plot =
                                   F)$counts
    extinction_counts_1064 = rep(NA, times = 20)
    extinction_counts[k, , ] = rbind(extinction_counts_355,
                                     extinction_counts_532,
                                     extinction_counts_1064)
    com_counts_355 = hist(db_355$CenterMass, breaks = altitude_breaks, plot =
                            F)$counts
    com_counts_532 = hist(db_532$CenterMass, breaks = altitude_breaks, plot =
                            F)$counts
    com_counts_1064 = hist(db_1064$CenterMass, breaks = altitude_breaks, plot =
                             F)$counts
    com_counts[k, , ] = rbind(com_counts_355, com_counts_532, com_counts_1064)
    ib_counts_355 = hist(db_355$IntBs, breaks = ib_breaks, plot = F)$counts
    ib_counts_532 = hist(db_532$IntBs, breaks = ib_breaks, plot = F)$counts
    ib_counts_1064 = hist(db_1064$IntBs, breaks = ib_breaks, plot = F)$counts
    ib_counts[k, , ] = rbind(ib_counts_355, ib_counts_532, ib_counts_1064)
    backscatter_counts_355 = hist(db_355$Backscatter, breaks = backscatter_breaks, plot =
                                    F)$counts
    backscatter_counts_532 = hist(db_532$Backscatter, breaks = backscatter_breaks, plot =
                                    F)$counts
    backscatter_counts_1064 = hist(db_1064$Backscatter, breaks = backscatter_breaks, plot =
                                     F)$counts
    backscatter_counts[k, , ] = rbind(backscatter_counts_355,
                                      backscatter_counts_532,
                                      backscatter_counts_1064)
  }
  
  bl_counts[is.na(bl_counts)] = 9.9692099683868690e+36
  tl_counts[is.na(tl_counts)] = 9.9692099683868690e+36
  lr_counts[is.na(lr_counts)] = 9.9692099683868690e+36
  pd_counts[is.na(pd_counts)] = 9.9692099683868690e+36
  aod_counts[is.na(aod_counts)] = 9.9692099683868690e+36
  extinction_counts[is.na(extinction_counts)] = 9.9692099683868690e+36
  com_counts[is.na(com_counts)] = 9.9692099683868690e+36
  ib_counts[is.na(ib_counts)] = 9.9692099683868690e+36
  backscatter_counts[is.na(backscatter_counts)] = 9.9692099683868690e+36
  
  source_name = lev2db[lev2db$Station == loc2[p], ]
  source_data = ""
  k = nrow(source_name)
  for (i in 1:k)
  {
    source_data = paste0(source_data, " ", source_name[i, 1])
  }
  
  
  val_time = c(
    as.numeric(
      as.POSIXct("2000-04-15 23:59:59", tz = "GMT", origin = "1970-01-01 00:00:00")
    ),
    as.numeric(
      as.POSIXct("2000-07-15 23:59:59", tz = "GMT", origin = "1970-01-01 00:00:00")
    ),
    as.numeric(
      as.POSIXct("2000-10-15 23:59:59", tz = "GMT", origin = "1970-01-01 00:00:00")
    ),
    as.numeric(
      as.POSIXct("2000-01-15 23:59:59", tz = "GMT", origin = "1970-01-01 00:00:00")
    )
  )
  
  #creiamo le dimensioni
  nv = ncdim_def(
    name = "nv",
    units = "",
    create_dimvar = T,
    vals = 1:2,
    longname = ""
  )
  wavelength = ncdim_def(
    name = "wavelength",
    units = "nm",
    create_dimvar = TRUE,
    vals = c(355, 532, 1064),
    longname = "Wavelength of the transmitted laser pulse"
  )
  time = ncdim_def(
    name = "time",
    units = "seconds since 1970-01-01T00:00:00Z",
    create_dimvar = TRUE,
    calendar = "gregorian",
    vals = val_time,
    longname = "Time"
  )
  n_char = ncdim_def(
    name = "n_char",
    units = "",
    create_dimvar = T,
    vals = 1:nchar(source_data),
    longname = ""
  )
  breaks = ncdim_def(
    name = "breaks",
    units = "",
    create_dimvar = TRUE,
    vals = 1:20,
    longname = "Histogram breaks"
  )
  
  altitude_intervals = ncvar_def(
    "altitude_intervals",
    units = "m",
    dim = breaks,
    prec = "double"
  )
  lidar_ratio_intervals = ncvar_def(
    "lidar_ratio_intervals",
    units = "sr",
    dim = breaks,
    prec = "double"
  )
  extinction_intervals = ncvar_def(
    "extinction_intervals",
    units = "1/km",
    dim = breaks,
    prec = "double"
  )
  particle_depolarization_intervals = ncvar_def(
    "particle_depolarization_intervals",
    units = "1",
    dim = breaks,
    prec = "double"
  )
  aerosol_optical_depth_intervals = ncvar_def(
    "aerosol_optical_depth_intervals",
    units = "1",
    dim = breaks,
    prec = "double"
  )
  integrated_backscatter_intervals = ncvar_def(
    "integrated_backscatter_intervals",
    units = "(10^3 sr)^-1",
    dim = breaks,
    prec = "double"
  )
  backscatter_intervals = ncvar_def(
    "backscatter_intervals",
    units = "1/Mm*sr",
    dim = breaks,
    prec = "double"
  )
  
  base_layer_altitude_frequency = ncvar_def(
    "base_layer_altitude_frequency",
    units = "",
    dim = list(time, breaks),
    missval = 9.9692099683868690e+36,
    longname = "Frequency distribution of the base layer altitude values",
    prec = "double"
  )
  top_layer_altitude_frequency = ncvar_def(
    "top_layer_altitude_frequency",
    units = "",
    dim = list(time, breaks),
    missval = 9.9692099683868690e+36,
    longname = "Frequency distribution of the top layer altitude values",
    prec = "double"
  )
  center_of_mass_altitude_frequency = ncvar_def(
    "center_of_mass_altitude_frequency",
    units = "",
    dim = list(time, wavelength, breaks),
    missval = 9.9692099683868690e+36,
    longname = "Frequency distribution of the center of mass altitude values",
    prec = "double"
  )
  lidar_ratio_frequency = ncvar_def(
    "lidar_ratio_frequency",
    units = "",
    dim = list(time, wavelength, breaks),
    missval = 9.9692099683868690e+36,
    longname = "Frequency distribution of the lidar ratio values",
    prec = "double"
  )
  extinction_frequency = ncvar_def(
    "extinction_frequency",
    units = "",
    dim = list(time, wavelength, breaks),
    missval = 9.9692099683868690e+36,
    longname = "Frequency distribution of the extinction frequency",
    prec = "double"
  )
  particle_depolarization_frequency = ncvar_def(
    "particle_depolarization_frequency",
    units = "",
    dim = list(time, wavelength, breaks),
    missval = 9.9692099683868690e+36,
    longname = "Frequency distribution of the linear particle depolarization ratio",
    prec = "double"
  )
  aerosol_optical_depth_frequency = ncvar_def(
    "aerosol_optical_depth_frequency",
    units = "",
    dim = list(time, wavelength, breaks),
    missval = 9.9692099683868690e+36,
    longname = "Frequency distribution of the aerosol optical depth values",
    prec = "double"
  )
  integrated_backscatter_frequency = ncvar_def(
    "integrated_backscatter_frequency",
    units = "",
    dim = list(time, wavelength, breaks),
    missval = 9.9692099683868690e+36,
    longname = "Frequency distribution of the integrated backscatter values",
    prec = "double"
  )
  backscatter_frequency = ncvar_def(
    "backscatter_frequency",
    units = "",
    dim = list(time, wavelength, breaks),
    missval = 9.9692099683868690e+36,
    longname = "Frequency distribution of the backscatter values",
    prec = "double"
  )
  
  time_bounds = ncvar_def(
    "time_bounds",
    units = "seconds since 1970-01-01T00:00:00Z",
    dim = list(time, nv),
    prec = "double"
  )
  latitude = ncvar_def(
    "latitude",
    units = "degrees_north",
    dim = list(),
    longname = "latitude of station",
    prec = "float"
  )
  longitude = ncvar_def(
    "longitude",
    units = "degrees_east",
    dim = list(),
    longname = "longitude of station",
    prec = "float"
  )
  source = ncvar_def(
    "source",
    units = "",
    dim = n_char,
    longname = "source files",
    prec = "char"
  )
  station_altitude = ncvar_def(
    "station_altitude",
    units = "m",
    dim = list(),
    longname = "station altitude above sea level",
    prec = "float"
  )
  
  variabili = list(
    base_layer_altitude_frequency,
    top_layer_altitude_frequency,
    center_of_mass_altitude_frequency,
    lidar_ratio_frequency,
    extinction_frequency,
    particle_depolarization_frequency,
    aerosol_optical_depth_frequency,
    integrated_backscatter_frequency,
    backscatter_frequency,
    altitude_intervals,
    lidar_ratio_intervals,
    extinction_intervals,
    particle_depolarization_intervals,
    aerosol_optical_depth_intervals,
    integrated_backscatter_intervals,
    backscatter_intervals,
    time_bounds,
    latitude,
    longitude,
    source,
    station_altitude
  )
  
  #creiamo il file netcdf
  season_file = nc_create(
    paste0(
      "ACTRIS_AerRemSen_",
      loc2[p],
      "_Lev03_NorSea_0019_Lay_v02_qc030.nc"
    ),
    variabili,
    force_v4 = TRUE
  )
  
  ncatt_put(season_file, "time", "axis", "T", prec = "text")
  ncatt_put(season_file, "time", "standard_name", "time", prec = "text")
  ncatt_put(season_file, "time", "bounds", "time_bounds", prec = "text")
  
  ncatt_put(season_file, latitude, "standard_name", "latitude", prec = "text")
  
  ncatt_put(season_file, longitude, "standard_name", "longitude", prec =
              "text")
  
  ncatt_put(
    season_file,
    source,
    "description",
    "List of level 2 files from which are retrieved values averaged in this file",
    prec = "text"
  )
  
  descr = "Histogram interval bounds are reported. The n-th value represents the lower bound of the n-th interval, while the higher bound is the (n+1)-th value, since intervals are adiacent. The last interval (n=20) has no higher bound, since it is right-open."
  
  ncatt_put(season_file, altitude_intervals, "description", descr)
  
  ncatt_put(season_file, lidar_ratio_intervals, "description", descr)
  
  ncatt_put(season_file, extinction_intervals, "description", descr)
  
  ncatt_put(season_file,
            particle_depolarization_intervals,
            "description",
            descr)
  
  ncatt_put(season_file,
            aerosol_optical_depth_intervals,
            "description",
            descr)
  
  ncatt_put(season_file,
            integrated_backscatter_intervals,
            "description",
            descr)
  
  ncatt_put(season_file, backscatter_intervals, "description", descr)
  
  ncatt_put(
    season_file,
    base_layer_altitude_frequency,
    "histogram_intervals",
    "altitude_intervals"
  )
  
  ncatt_put(
    season_file,
    top_layer_altitude_frequency,
    "histogram_intervals",
    "altitude_intervals"
  )
  
  ncatt_put(
    season_file,
    center_of_mass_altitude_frequency,
    "histogram_intervals",
    "altitude_intervals"
  )
  
  ncatt_put(
    season_file,
    lidar_ratio_frequency,
    "histogram_intervals",
    "lidar_ratio_intervals"
  )
  
  ncatt_put(
    season_file,
    extinction_frequency,
    "histogram_intervals",
    "extinction_intervals"
  )
  
  ncatt_put(
    season_file,
    particle_depolarization_frequency,
    "histogram_intervals",
    "particle_depolarization_intervals"
  )
  
  ncatt_put(
    season_file,
    aerosol_optical_depth_frequency,
    "histogram_intervals",
    "aerosol_optical_depth_intervals"
  )
  
  ncatt_put(
    season_file,
    integrated_backscatter_frequency,
    "histogram_intervals",
    "integrated_backscatter_intervals"
  )
  
  ncatt_put(
    season_file,
    backscatter_frequency,
    "histogram_intervals",
    "backscatter_intervals"
  )
  
  #inseriamo i dati
  ncvar_put(season_file, base_layer_altitude_frequency, bl_counts)
  ncvar_put(season_file, top_layer_altitude_frequency, tl_counts)
  ncvar_put(season_file, center_of_mass_altitude_frequency, com_counts)
  ncvar_put(season_file, lidar_ratio_frequency, lr_counts)
  ncvar_put(season_file, extinction_frequency, extinction_counts)
  ncvar_put(season_file, particle_depolarization_frequency, pd_counts)
  ncvar_put(season_file, aerosol_optical_depth_frequency, aod_counts)
  ncvar_put(season_file, integrated_backscatter_frequency, ib_counts)
  ncvar_put(season_file, backscatter_frequency, backscatter_counts)
  
  ncvar_put(season_file, altitude_intervals, altitude_intervals_data)
  ncvar_put(season_file, lidar_ratio_intervals, lr_intervals_data)
  ncvar_put(season_file,
            extinction_intervals,
            extinction_intervals_data)
  ncvar_put(season_file,
            particle_depolarization_intervals,
            pd_intervals_data)
  ncvar_put(season_file,
            aerosol_optical_depth_intervals,
            aod_intervals_data)
  ncvar_put(season_file,
            integrated_backscatter_intervals,
            ib_intervals_data)
  ncvar_put(season_file,
            backscatter_intervals,
            backscatter_intervals_data)
  
  ncvar_put(season_file, time_bounds, time_bounds_data)
  ncvar_put(season_file, source, source_data)
  ncvar_put(season_file, latitude, latit[p])
  ncvar_put(season_file, longitude, longit[p])
  ncvar_put(season_file, station_altitude, station_alt[p])
  
  #definiamo gli attributi globali
  ncatt_put(season_file,
            0,
            attname = "processor_name",
            attval = "EAR_clim_v1.exe",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "processor_version",
            attval = "",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "processor_institution",
            attval = "CNR - IMAA",
            prec = "text")
  ncatt_put(
    season_file,
    0,
    attname = "system",
    attval = sist[p],
    prec = "text"
  )
  ncatt_put(
    season_file,
    0,
    attname = "location",
    attval = loc[p],
    prec = "text"
  )
  ncatt_put(
    season_file,
    0,
    attname = "institution",
    attval = institution[p],
    prec = "text"
  )
  ncatt_put(
    season_file,
    0,
    attname = "PI",
    attval = PI[p],
    prec = "text"
  )
  ncatt_put(
    season_file,
    0,
    attname = "PI_affiliation",
    attval = institution[p],
    prec = "text"
  )
  ncatt_put(
    season_file,
    0,
    attname = "PI_affiliation_acronym",
    attval = acronym[p],
    prec = "text"
  )
  ncatt_put(season_file,
            0,
            attname = "PI_address",
            attval = "",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "PI_phone",
            attval = "",
            prec = "text")
  ncatt_put(
    season_file,
    0,
    attname = "PI_email",
    attval = PI_contact[p],
    prec = "text"
  )
  ncatt_put(season_file,
            0,
            attname = "data_originator",
            attval = "Consiglio Nazionale delle Ricerche - Istituto di Metodologie per l'Analisi Ambientale",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "data_originator_affiliation",
            attval = "Consiglio Nazionale delle Ricerche - Istituto di Metodologie per l'Analisi Ambientale",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "data_originator_affiliation_acronym",
            attval = "CNR - IMAA",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "data_originator_address",
            attval = "",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "data_originator_phone",
            attval = "",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "data_originator_email",
            attval = "earlinetdb@actris.imaa.cnr.it",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "data_provider",
            attval = "ACTRIS ARES",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "data_provider_affiliation",
            attval = "Consiglio Nazionale delle Ricerche - Istituto di Metodologie per l'Analisi Ambientale",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "data_provider_affiliation_acronym",
            attval = "CNR - IMAA",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "data_provider_address",
            attval = "",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "data_provider_phone",
            attval = "",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "data_provider_email",
            attval = "earlinetdb@actris.imaa.cnr.it",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "conventions",
            attval = "C.F. - 1.8",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "references",
            attval = "link doc earlinet.org",
            prec = "text")
  ncatt_put(
    season_file,
    0,
    attname = "station_ID",
    attval = loc2[p],
    prec = "text"
  )
  ncatt_put(season_file,
            0,
            attname = "__file_format_version",
            attval = "",
            prec = "text")
  ncatt_put(
    season_file,
    0,
    attname = "history",
    attval = paste0(Sys.time(), "Generated by free software R, using package ncdf4"),
    prec = "text"
  )
  ncatt_put(season_file, 0, attname = "title", attval = "Seasonal distribution of layer optical values - Normal")
  
  nc_close(season_file)
  gw = getwd()
  file.rename(
    from = paste0(
      gw,
      "/ACTRIS_AerRemSen_",
      loc2[p],
      "_Lev03_NorSea_0019_Lay_v02_qc030.nc"
    ),
    to = paste0(
      gw,
      "/Level3/Layers/",
      loc2[p],
      "/ACTRIS_AerRemSen_",
      loc2[p],
      "_Lev03_NorSea_0019_Lay_v02_qc030.nc"
    )
  )
  
}