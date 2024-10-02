#carichiamo db che ci servono
load("lev2db.Rda")
load("Lev3Int_b_nm_355.Rda")
load("Lev3Int_e_nm_355.Rda")
load("Lev3Int_b_nm_532.Rda")
load("Lev3Int_e_nm_532.Rda")
load("Lev3Int_b_nm_1064.Rda")
load("Lev3Int_ang_nm.Rda")
load("System.Rda")
load("Lev3pbl_nm.Rda")
load("Lev3PartDep_nm_355.Rda")
load("Lev3PartDep_nm_532.Rda")
load("Lev3PartDep_nm_1064.Rda")

db_b_nm_355[, 3:54] = signif(db_b_nm_355[, 3:54], 6)
db_b_nm_532[, 3:54] = signif(db_b_nm_532[, 3:54], 6)
db_b_nm_1064[, 3:54] = signif(db_b_nm_1064[, 3:54], 6)
db_e_nm_355[, 3:54] = signif(db_e_nm_355[, 3:54], 6)
db_e_nm_532[, 3:54] = signif(db_e_nm_532[, 3:54], 6)

db_ang_nm_tot[, 3:26] = signif(db_ang_nm_tot[, 3:26], 6)
db_pbl_nm_tot[, 3:6] = signif(db_pbl_nm_tot[, 3:6], 6)
db_pd_nm_355[, 3:26] = signif(db_pd_nm_355[, 3:26], 6)
db_pd_nm_532[, 3:26] = signif(db_pd_nm_532[, 3:26], 6)
db_pd_nm_1064[, 3:26] = signif(db_pd_nm_1064[, 3:26], 6)

#generiamo i dati

integral_bounds_data = c(0, 1)

vrm = c(16, 17, 26)

time_bounds_data = matrix(NA, nrow = 2, ncol = 12)
time_bounds_data[,1]=c(as.numeric(as.POSIXct("2000-01-01 00:00:00",tz="GMT",origin="1970-01-01 00:00:00")),as.numeric(as.POSIXct("2019-01-31 23:59:59",tz="GMT",origin="1970-01-01 00:00:00")))
time_bounds_data[,2]=c(as.numeric(as.POSIXct("2000-02-01 00:00:00",tz="GMT",origin="1970-01-01 00:00:00")),as.numeric(as.POSIXct("2019-02-28 23:59:59",tz="GMT",origin="1970-01-01 00:00:00")))
time_bounds_data[,3]=c(as.numeric(as.POSIXct("2000-03-01 00:00:00",tz="GMT",origin="1970-01-01 00:00:00")),as.numeric(as.POSIXct("2019-03-31 23:59:59",tz="GMT",origin="1970-01-01 00:00:00")))
time_bounds_data[,4]=c(as.numeric(as.POSIXct("2000-04-01 00:00:00",tz="GMT",origin="1970-01-01 00:00:00")),as.numeric(as.POSIXct("2019-04-30 23:59:59",tz="GMT",origin="1970-01-01 00:00:00")))
time_bounds_data[,5]=c(as.numeric(as.POSIXct("2000-05-01 00:00:00",tz="GMT",origin="1970-01-01 00:00:00")),as.numeric(as.POSIXct("2019-05-31 23:59:59",tz="GMT",origin="1970-01-01 00:00:00")))
time_bounds_data[,6]=c(as.numeric(as.POSIXct("2000-06-01 00:00:00",tz="GMT",origin="1970-01-01 00:00:00")),as.numeric(as.POSIXct("2019-06-30 23:59:59",tz="GMT",origin="1970-01-01 00:00:00")))
time_bounds_data[,7]=c(as.numeric(as.POSIXct("2000-07-01 00:00:00",tz="GMT",origin="1970-01-01 00:00:00")),as.numeric(as.POSIXct("2019-07-31 23:59:59",tz="GMT",origin="1970-01-01 00:00:00")))
time_bounds_data[,8]=c(as.numeric(as.POSIXct("2000-08-01 00:00:00",tz="GMT",origin="1970-01-01 00:00:00")),as.numeric(as.POSIXct("2019-08-31 23:59:59",tz="GMT",origin="1970-01-01 00:00:00")))
time_bounds_data[,9]=c(as.numeric(as.POSIXct("2000-09-01 00:00:00",tz="GMT",origin="1970-01-01 00:00:00")),as.numeric(as.POSIXct("2019-09-30 23:59:59",tz="GMT",origin="1970-01-01 00:00:00")))
time_bounds_data[,10]=c(as.numeric(as.POSIXct("2000-10-01 00:00:00",tz="GMT",origin="1970-01-01 00:00:00")),as.numeric(as.POSIXct("2019-10-31 23:59:59",tz="GMT",origin="1970-01-01 00:00:00")))
time_bounds_data[,11]=c(as.numeric(as.POSIXct("2000-11-01 00:00:00",tz="GMT",origin="1970-01-01 00:00:00")),as.numeric(as.POSIXct("2019-11-30 23:59:59",tz="GMT",origin="1970-01-01 00:00:00")))
time_bounds_data[,12]=c(as.numeric(as.POSIXct("2000-12-01 00:00:00",tz="GMT",origin="1970-01-01 00:00:00")),as.numeric(as.POSIXct("2019-12-31 23:59:59",tz="GMT",origin="1970-01-01 00:00:00")))

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

sist = rep(NA, times = 33)
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
# sist[34] = syst[34,20]
# sist[35] = syst[35,18]
# sist[36] = syst[36,20]
sist = sist[-vrm]

app = data.frame(NA, NA)
mnt = c("01",
        "02",
        "03",
        "04",
        "05",
        "06",
        "07",
        "08",
        "09",
        "10",
        "11",
        "12")
colnames(app) = c("Month", "Null")
for (i in 1:12)
{
  v = data.frame(mnt[i], NA)
  colnames(v) = c("Month", "Null")
  app = rbind(app, v)
}
app = app[-1, ]

for (p in 1:33)
{
  db_e355 = db_e_nm_355[db_e_nm_355$Station == loc2[p], ]
  db_e355 = merge(db_e355, app, by = "Month", all = TRUE)
  db_e532 = db_e_nm_532[db_e_nm_532$Station == loc2[p], ]
  db_e532 = merge(db_e532, app, by = "Month", all = TRUE)
  db_b355 = db_b_nm_355[db_b_nm_355$Station == loc2[p], ]
  db_b355 = merge(db_b355, app, by = "Month", all = TRUE)
  db_b532 = db_b_nm_532[db_b_nm_532$Station == loc2[p], ]
  db_b532 = merge(db_b532, app, by = "Month", all = TRUE)
  db_b1064 = db_b_nm_1064[db_b_nm_1064$Station == loc2[p], ]
  db_b1064 = merge(db_b1064, app, by = "Month", all = TRUE)
  db_pbl = db_pbl_nm_tot[db_pbl_nm_tot$Station == loc2[p], ]
  db_pbl = merge(db_pbl, app, by = "Month", all = TRUE)
  db_pd355 = db_pd_nm_355[db_pd_nm_355$Station == loc2[p], ]
  db_pd355 = merge(db_pd355, app, by = "Month", all = TRUE)
  db_pd532 = db_pd_nm_532[db_pd_nm_532$Station == loc2[p], ]
  db_pd532 = merge(db_pd532, app, by = "Month", all = TRUE)
  db_pd1064 = db_pd_nm_1064[db_pd_nm_1064$Station == loc2[p], ]
  db_pd1064 = merge(db_pd1064, app, by = "Month", all = TRUE)
  db_ang = db_ang_nm_tot[db_ang_nm_tot$Station == loc2[p], ]
  db_ang = merge(db_ang, app, by = "Month", all = TRUE)
  
  angstrom_data = array(NA, dim = c(12, 2, 5))
  angstrom_data[, 1, ] = cbind(db_ang[, 3], db_ang[, 4], db_ang[, 9], db_ang[, 15], db_ang[, 21])
  angstrom_data[, 2, ] = cbind(db_ang[, 5], db_ang[, 6], db_ang[, 11], db_ang[, 17], db_ang[, 23])
  angstrom_data[is.na(angstrom_data)] = 9.9692099683868690e+36
  
  aerosol_optical_depth_data = array(NA, dim = c(12, 2, 3, 5))
  aerosol_optical_depth_data[, 1, 1, ] = cbind(db_e355[, 4], db_e355[, 5], db_e355[, 17], db_e355[, 30], db_e355[, 43])
  aerosol_optical_depth_data[, 2, 1, ] = cbind(db_e355[, 6], db_e355[, 7], db_e355[, 19], db_e355[, 32], db_e355[, 45])
  aerosol_optical_depth_data[, 1, 2, ] = cbind(db_e532[, 4], db_e532[, 5], db_e532[, 17], db_e532[, 30], db_e532[, 43])
  aerosol_optical_depth_data[, 2, 2, ] = cbind(db_e532[, 6], db_e532[, 7], db_e532[, 19], db_e532[, 32], db_e532[, 45])
  aerosol_optical_depth_data[, , 3, ] = array(NA, dim = c(12, 2, 5))
  aerosol_optical_depth_data[is.na(aerosol_optical_depth_data)] = 9.9692099683868690e+36
  
  lidar_ratio_data = array(NA, dim = c(12, 2, 3, 5))
  lidar_ratio_data[, 1, 1, ] = cbind(db_e355[, 10], db_e355[, 11], db_e355[, 23], db_e355[, 36], db_e355[, 49])
  lidar_ratio_data[, 2, 1, ] = cbind(db_e355[, 12], db_e355[, 13], db_e355[, 25], db_e355[, 38], db_e355[, 51])
  lidar_ratio_data[, 1, 2, ] = cbind(db_e532[, 10], db_e532[, 11], db_e532[, 23], db_e532[, 36], db_e532[, 49])
  lidar_ratio_data[, 2, 2, ] = cbind(db_e532[, 12], db_e532[, 13], db_e532[, 25], db_e532[, 38], db_e532[, 51])
  lidar_ratio_data[, , 3, ] = array(NA, dim = c(12, 2, 5))
  lidar_ratio_data[is.na(lidar_ratio_data)] = 9.9692099683868690e+36
  
  integrated_backscatter_data = array(NA, dim = c(12, 2, 3, 5))
  integrated_backscatter_data[, 1, 1, ] = cbind(db_b355[, 4], db_b355[, 5], db_b355[, 17], db_b355[, 30], db_b355[, 43])
  integrated_backscatter_data[, 2, 1, ] = cbind(db_b355[, 6], db_b355[, 7], db_b355[, 19], db_b355[, 32], db_b355[, 45])
  integrated_backscatter_data[, 1, 2, ] = cbind(db_b532[, 4], db_b532[, 5], db_b532[, 17], db_b532[, 30], db_b532[, 43])
  integrated_backscatter_data[, 2, 2, ] = cbind(db_b532[, 6], db_b532[, 7], db_b532[, 19], db_b532[, 32], db_b532[, 45])
  integrated_backscatter_data[, 1, 3, ] = cbind(db_b1064[, 4], db_b1064[, 5], db_b1064[, 17], db_b1064[, 30], db_b1064[, 43])
  integrated_backscatter_data[, 2, 3, ] = cbind(db_b1064[, 6], db_b1064[, 7], db_b1064[, 19], db_b1064[, 32], db_b1064[, 45])
  integrated_backscatter_data[is.na(integrated_backscatter_data)] = 9.9692099683868690e+36
  
  center_of_mass_data = array(NA, dim = c(12, 2, 3, 5))
  center_of_mass_data[, 1, 1, ] = cbind(db_b355[, 10], db_b355[, 11], db_b355[, 23], db_b355[, 36], db_b355[, 49])
  center_of_mass_data[, 2, 1, ] = cbind(db_b355[, 12], db_b355[, 13], db_b355[, 25], db_b355[, 38], db_b355[, 51])
  center_of_mass_data[, 1, 2, ] = cbind(db_b532[, 10], db_b532[, 11], db_b532[, 23], db_b532[, 36], db_b532[, 49])
  center_of_mass_data[, 2, 2, ] = cbind(db_b532[, 12], db_b532[, 13], db_b532[, 25], db_b532[, 38], db_b532[, 51])
  center_of_mass_data[, 1, 3, ] = cbind(db_b1064[, 10], db_b1064[, 11], db_b1064[, 23], db_b1064[, 36], db_b1064[, 49])
  center_of_mass_data[, 2, 3, ] = cbind(db_b1064[, 12], db_b1064[, 13], db_b1064[, 25], db_b1064[, 38], db_b1064[, 51])
  center_of_mass_data[is.na(center_of_mass_data)] = 9.9692099683868690e+36
  
  aerosol_boundary_layer_data = cbind(db_pbl[, 3], rep(NA, times = 4), db_pbl[, 4], db_pbl[, 5], db_pbl[, 6])
  aerosol_boundary_layer_data[is.na(aerosol_boundary_layer_data)] = 9.9692099683868690e+36
  
  h63_of_aerosol_optical_depth_data = array(NA, dim = c(12, 3, 5))
  h63_of_aerosol_optical_depth_data[, 1, 1] = db_e355[, 3]
  h63_of_aerosol_optical_depth_data[, 1, 3:5] = cbind(db_e355[, 16], db_e355[, 29], db_e355[, 42])
  h63_of_aerosol_optical_depth_data[, 2, 1] = db_e532[, 3]
  h63_of_aerosol_optical_depth_data[, 2, 3:5] = cbind(db_e532[, 16], db_e532[, 29], db_e532[, 42])
  h63_of_aerosol_optical_depth_data[is.na(h63_of_aerosol_optical_depth_data)] =
    9.9692099683868690e+36
  
  h63_of_integrated_backscatter_data = array(NA, dim = c(12, 3, 5))
  h63_of_integrated_backscatter_data[, 1, 1] = db_b355[, 3]
  h63_of_integrated_backscatter_data[, 1, 3:5] = cbind(db_b355[, 16], db_b355[, 29], db_b355[, 42])
  h63_of_integrated_backscatter_data[, 2, 1] = db_b532[, 3]
  h63_of_integrated_backscatter_data[, 2, 3:5] = cbind(db_b532[, 16], db_b532[, 29], db_b532[, 42])
  h63_of_integrated_backscatter_data[, 3, 1] = db_b1064[, 3]
  h63_of_integrated_backscatter_data[, 3, 3:5] = cbind(db_b1064[, 16], db_b1064[, 29], db_b1064[, 42])
  h63_of_integrated_backscatter_data[is.na(h63_of_integrated_backscatter_data)] =
    9.9692099683868690e+36
  
  particle_depolarization_data = array(NA, dim = c(12, 2, 3, 5))
  particle_depolarization_data[, 1, 1, ] = cbind(db_pd355[, 3], db_pd355[, 4], db_pd355[, 9], db_pd355[, 15], db_pd355[, 21])
  particle_depolarization_data[, 2, 1, ] = cbind(db_pd355[, 5], db_pd355[, 6], db_pd355[, 11], db_pd355[, 17], db_pd355[, 23])
  particle_depolarization_data[, 1, 2, ] = cbind(db_pd532[, 3], db_pd532[, 4], db_pd532[, 9], db_pd532[, 15], db_pd532[, 21])
  particle_depolarization_data[, 2, 2, ] = cbind(db_pd532[, 5], db_pd532[, 6], db_pd532[, 11], db_pd532[, 17], db_pd532[, 23])
  particle_depolarization_data[, 1, 3, ] = cbind(db_pd1064[, 3], db_pd1064[, 4], db_pd1064[, 9], db_pd1064[, 15], db_pd1064[, 21])
  particle_depolarization_data[, 2, 3, ] = cbind(db_pd1064[, 5], db_pd1064[, 6], db_pd1064[, 11], db_pd1064[, 17], db_pd1064[, 23])
  particle_depolarization_data[is.na(particle_depolarization_data)] = 9.9692099683868690e+36
  
  source_name = lev2db[lev2db$Place == loc2[p], 1]
  source_data = " "
  for (n in source_name)
  {
    source_data = paste0(source_data, " ", n)
  }
  
  val_time = c(
    as.numeric(
      as.POSIXct("2000-01-15 23:59:59", tz = "GMT", origin = "1970-01-01 00:00:00")
    ),
    as.numeric(
      as.POSIXct("2000-02-14 23:59:59", tz = "GMT", origin = "1970-01-01 00:00:00")
    ),
    as.numeric(
      as.POSIXct("2000-03-15 23:59:59", tz = "GMT", origin = "1970-01-01 00:00:00")
    ),
    as.numeric(
      as.POSIXct("2000-04-15 23:59:59", tz = "GMT", origin = "1970-01-01 00:00:00")
    ),
    as.numeric(
      as.POSIXct("2000-05-15 23:59:59", tz = "GMT", origin = "1970-01-01 00:00:00")
    ),
    as.numeric(
      as.POSIXct("2000-06-15 23:59:59", tz = "GMT", origin = "1970-01-01 00:00:00")
    ),
    as.numeric(
      as.POSIXct("2000-07-15 23:59:59", tz = "GMT", origin = "1970-01-01 00:00:00")
    ),
    as.numeric(
      as.POSIXct("2000-08-15 23:59:59", tz = "GMT", origin = "1970-01-01 00:00:00")
    ),
    as.numeric(
      as.POSIXct("2000-09-15 23:59:59", tz = "GMT", origin = "1970-01-01 00:00:00")
    ),
    as.numeric(
      as.POSIXct("2000-10-15 23:59:59", tz = "GMT", origin = "1970-01-01 00:00:00")
    ),
    as.numeric(
      as.POSIXct("2000-11-15 23:59:59", tz = "GMT", origin = "1970-01-01 00:00:00")
    ),
    as.numeric(
      as.POSIXct("2000-12-15 23:59:59", tz = "GMT", origin = "1970-01-01 00:00:00")
    )
  )
  
  #creiamo le dimensioni
  
  nv = ncdim_def(
    name = "nv",
    units = "",
    create_dimvar = FALSE,
    vals = 1:2
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
    create_dimvar = FALSE,
    vals = 1:nchar(source_data)
  )
  stats = ncdim_def(
    name = "stats",
    units = "",
    create_dimvar = TRUE,
    vals = 0:4,
    longname = "statistics"
  )
  
  integral_bounds = ncvar_def(
    "integral_bounds",
    units = "",
    dim = nv,
    longname = "integral bounds of integrated values",
    prec = "byte"
  )
  aerosol_optical_depth = ncvar_def(
    "aerosol_optical_depth",
    units = "1",
    dim = list(time, nv, wavelength, stats),
    missval = 9.9692099683868690e+36,
    longname = "aerosol optical depth",
    prec = "double"
  )
  integrated_backscatter = ncvar_def(
    "integrated_backscatter",
    units = "1/sr",
    dim = list(time, nv, wavelength, stats),
    missval = 9.9692099683868690e+36,
    longname = "integrated backscatter",
    prec = "double"
  )
  lidar_ratio = ncvar_def(
    "lidar_ratio",
    units = "sr",
    dim = list(time, nv, wavelength, stats),
    missval = 9.9692099683868690e+36,
    longname = "aerosol extinction-to-backscatter ratio",
    prec = "double"
  )
  aerosol_boundary_layer = ncvar_def(
    "aerosol_boundary_layer",
    units = "m",
    dim = list(time, stats),
    longname = "altitude of the upper bound of the aerosol planet boundary layer",
    missval = 9.9692099683868690e+36,
    prec = "double"
  )
  h63_of_aerosol_optical_depth = ncvar_def(
    "h63_of_aerosol_optical_depth",
    units = "m",
    dim = list(time, wavelength, stats),
    missval = 9.9692099683868690e+36,
    longname = "altitude below which stays the 63% of the total aerosol optical depth",
    prec = "double"
  )
  h63_of_integrated_backscatter = ncvar_def(
    "h63_of_integrated_backscatter",
    units = "m",
    dim = list(time, wavelength, stats),
    missval = 9.9692099683868690e+36,
    longname = "altitude below which stays the 63% of the total integrated backscatter",
    prec = "double"
  )
  center_of_mass = ncvar_def(
    "center_of_mass",
    units = "m",
    dim = list(time, nv, wavelength, stats),
    missval = 9.9692099683868690e+36,
    longname = "center of mass",
    prec = "double"
  )
  particle_depolarization = ncvar_def(
    "particle_depolarization",
    units = "1",
    dim = list(time, nv, wavelength, stats),
    missval = 9.9692099683868690e+36,
    longname = "aerosol linear particle depolarization ratio",
    prec = "double"
  )
  angstrom_coefficient = ncvar_def(
    "angstrom_coefficient",
    units = "1",
    dim = list(time, nv, stats),
    missval = 9.9692099683868690e+36,
    longname = "angstrom coefficient",
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
    longname = "latitude of the station",
    prec = "float"
  )
  longitude = ncvar_def(
    "longitude",
    units = "degrees_east",
    dim = list(),
    longname = "longitude of the station",
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
    integral_bounds,
    aerosol_optical_depth,
    integrated_backscatter,
    lidar_ratio,
    aerosol_boundary_layer,
    h63_of_aerosol_optical_depth,
    h63_of_integrated_backscatter,
    center_of_mass,
    particle_depolarization,
    angstrom_coefficient,
    time_bounds,
    latitude,
    longitude,
    source,
    station_altitude
  )
  
  #creiamo il file netcdf
  month_file = nc_create(
    paste0(
      "ACTRIS_AerRemSen_",
      loc2[p],
      "_Lev03_NorMon_0019_Int_v02_qc030.nc"
    ),
    variabili,
    force_v4 = TRUE
  )
  
  #aggiungiamo gli attributi
  ncatt_put(month_file, integral_bounds, "flag_value", "0,1", prec = "text")
  ncatt_put(
    month_file,
    integral_bounds,
    "flag_meaning",
    "0:total, 1:aerosol boundary layer",
    prec = "text"
  )
  
  ncatt_put(month_file, "stats", "flag_value", "0,1,2,3,4", prec = "text")
  ncatt_put(
    month_file,
    "stats",
    "flag_meaning",
    "0:mean, 1:statistical error mean, 2:median, 3: standard deviation, 4:number of values aggregated"
  )
  
  ncatt_put(month_file, "time", "axis", "T", prec = "text")
  ncatt_put(month_file, "time", "standard_name", "time", prec = "text")
  ncatt_put(month_file, "time", "bounds", "time_bounds", prec = "text")
  
  ncatt_put(
    month_file,
    aerosol_optical_depth,
    "standard_name",
    "atmosphere_optical_
            thickness_due_to_ambient_aerosol_particles",
    prec = "text"
  )
  
  ncatt_put(month_file, latitude, "standard_name", "latitude", prec = "text")
  
  ncatt_put(month_file, longitude, "standard_name", "longitude", prec =
              "text")
  ncatt_put(
    month_file,
    source,
    "description",
    "List of level 2 files from which are retrieved values averaged in this file",
    prec = "text"
  )
  
  #inseriamo i dati
  ncvar_put(month_file, integral_bounds, integral_bounds_data)
  ncvar_put(month_file,
            aerosol_optical_depth,
            aerosol_optical_depth_data)
  ncvar_put(month_file,
            integrated_backscatter,
            integrated_backscatter_data)
  ncvar_put(month_file, lidar_ratio, lidar_ratio_data)
  ncvar_put(month_file,
            aerosol_boundary_layer,
            aerosol_boundary_layer_data)
  ncvar_put(month_file,
            h63_of_aerosol_optical_depth,
            h63_of_aerosol_optical_depth_data)
  ncvar_put(month_file,
            h63_of_integrated_backscatter,
            h63_of_integrated_backscatter_data)
  ncvar_put(month_file, center_of_mass, center_of_mass_data)
  ncvar_put(month_file,
            particle_depolarization,
            particle_depolarization_data)
  ncvar_put(month_file, angstrom_coefficient, angstrom_data)
  
  ncvar_put(month_file, time_bounds, time_bounds_data)
  ncvar_put(month_file, source, source_data)
  ncvar_put(month_file, latitude, latit[p])
  ncvar_put(month_file, longitude, longit[p])
  ncvar_put(month_file, station_altitude, station_alt[p])
  
  #definiamo gli attributi globali
  ncatt_put(month_file,
            0,
            attname = "processor_name",
            attval = "EAR_clim_v1.exe",
            prec = "text")
  ncatt_put(month_file,
            0,
            attname = "processor_version",
            attval = "",
            prec = "text")
  ncatt_put(month_file,
            0,
            attname = "processor_institution",
            attval = "CNR - IMAA",
            prec = "text")
  ncatt_put(month_file,
            0,
            attname = "system",
            attval = sist[p],
            prec = "text")
  ncatt_put(month_file,
            0,
            attname = "location",
            attval = loc[p],
            prec = "text")
  ncatt_put(month_file,
            0,
            attname = "institution",
            attval = institution[p],
            prec = "text")
  ncatt_put(month_file,
            0,
            attname = "PI",
            attval = PI[p],
            prec = "text")
  ncatt_put(month_file,
            0,
            attname = "PI_affiliation",
            attval = institution[p],
            prec = "text")
  ncatt_put(month_file,
            0,
            attname = "PI_affiliation_acronym",
            attval = acronym[p],
            prec = "text")
  ncatt_put(month_file,
            0,
            attname = "PI_address",
            attval = "",
            prec = "text")
  ncatt_put(month_file,
            0,
            attname = "PI_phone",
            attval = "",
            prec = "text")
  ncatt_put(month_file,
            0,
            attname = "PI_email",
            attval = PI_contact[p],
            prec = "text")
  ncatt_put(month_file,
            0,
            attname = "data_originator",
            attval = "Consiglio Nazionale delle Ricerche - Istituto di Metodologie per l'Analisi Ambientale",
            prec = "text")
  ncatt_put(month_file,
            0,
            attname = "data_originator_affiliation",
            attval = "Consiglio Nazionale delle Ricerche - Istituto di Metodologie per l'Analisi Ambientale",
            prec = "text")
  ncatt_put(month_file,
            0,
            attname = "data_originator_affiliation_acronym",
            attval = "CNR - IMAA",
            prec = "text")
  ncatt_put(month_file,
            0,
            attname = "data_originator_address",
            attval = "",
            prec = "text")
  ncatt_put(month_file,
            0,
            attname = "data_originator_phone",
            attval = "",
            prec = "text")
  ncatt_put(month_file,
            0,
            attname = "data_originator_email",
            attval = "earlinetdb@actris.imaa.cnr.it",
            prec = "text")
  ncatt_put(month_file,
            0,
            attname = "data_provider",
            attval = "ACTRIS ARES",
            prec = "text")
  ncatt_put(month_file,
            0,
            attname = "data_provider_affiliation",
            attval = "Consiglio Nazionale delle Ricerche - Istituto di Metodologie per l'Analisi Ambientale",
            prec = "text")
  ncatt_put(month_file,
            0,
            attname = "data_provider_affiliation_acronym",
            attval = "CNR - IMAA",
            prec = "text")
  ncatt_put(month_file,
            0,
            attname = "data_provider_address",
            attval = "",
            prec = "text")
  ncatt_put(month_file,
            0,
            attname = "data_provider_phone",
            attval = "",
            prec = "text")
  ncatt_put(month_file,
            0,
            attname = "data_provider_email",
            attval = "earlinetdb@actris.imaa.cnr.it",
            prec = "text")
  ncatt_put(month_file,
            0,
            attname = "conventions",
            attval = "C.F. - 1.8",
            prec = "text")
  ncatt_put(month_file,
            0,
            attname = "references",
            attval = "link doc earlinet.org",
            prec = "text")
  ncatt_put(month_file,
            0,
            attname = "station_ID",
            attval = loc2[p],
            prec = "text")
  ncatt_put(month_file,
            0,
            attname = "__file_format_version",
            attval = "",
            prec = "text")
  ncatt_put(
    month_file,
    0,
    attname = "history",
    attval = paste0(Sys.time(), "Generated by free software R, using package ncdf4"),
    prec = "text"
  )
  ncatt_put(month_file, 0, attname = "title", attval = "Monthly average integrated measurements - Normal")
  
  nc_close(month_file)
  gw = getwd()
  file.rename(
    from = paste0(
      gw,
      "/ACTRIS_AerRemSen_",
      loc2[p],
      "_Lev03_NorMon_0019_Int_v02_qc030.nc"
    ),
    to = paste0(
      gw,
      "/Level3/Integrated/",
      loc2[p],
      "/ACTRIS_AerRemSen_",
      loc2[p],
      "_Lev03_NorMon_0019_Int_v02_qc030.nc"
    )
  )
  
}