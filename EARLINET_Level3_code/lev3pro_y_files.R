#carichiamo db che ci servono
load("lev2db.Rda")
load("ProfilesYear355.Rda")
load("ProfilesYear532.Rda")
load("ProfilesYear1064.Rda")
load("System.Rda")

db_prof_year_355[, 4:21] = signif(db_prof_year_355[, 4:21], 6)
db_prof_year_532[, 4:21] = signif(db_prof_year_532[, 4:21], 6)
db_prof_year_1064[, c(4, 5, 8, 9, 12, 13)] = signif(db_prof_year_1064[, c(4, 5, 8, 9, 12, 13)], 6)

#dati globali
altitude_data = seq(from = 200, to = 12000, by = 200)
appo = data.frame(matrix(NA, nrow = 60, ncol = 2))
colnames(appo) = c("Altitude", "Null")
appo$Altitude = altitude_data

loc=c("Aberystwyth, United Kingdom","Andoya, Norway","Athens, Greece","Barcelona, Spain",
      "Cabauw, Netherlands","Belsk, Poland","Dushanbe, Tajikistan","Evora, Portugal",
      "Garmisch-Partenkirchen, Germany","Granada, Spain","Hamburg, Germany","Observatory Hohenpeissenberg, Germany",
      "Bucharest, Romania","Ispra, Italy","Kuehlungsborn, Germany",
      "Leipzig, Germany","Linkoping, Sweden","Lille, France",
      "Minsk, Belarus","Madrid, Spain","Melpitz, Germany","Maisach, Germany",
      "Naples, Italy","Potenza, Italy","Clermont-Ferrand, France",
      "Roma-Tor Vergata, Italy","Lecce, Italy","Palaiseau, France","Sofia, Bulgaria",
      "St. Petersburg, Russia","Thessaloniki, Greece","Cork, Ireland","Warsaw, Poland")

loc2 = unique(lev2db$Station)

latit=c(51.858,69.278,37.96,41.393,51.97,51.83,38.5594,38.5678,47.477,37.164,53.568,
        47.8019,44.348,45.8167,54.12,51.35,58.392,50.6117,53.917,
        40.4565,51.53,48.209,40.838,40.6,45.761,41.833,40.333,48.713,
        42.65,59.9427,40.63,51.8933,52.21)

longit=c(-4.252,16.008,23.78,2.12,4.93,20.78,68.8561,-7.9115,11.064,-3.605,9.973,11.0119,
         26.029,8.6167,11.77,12.433,15.575,3.1417,27.605,-3.7257,12.93,
         11.258,14.183,15.72,3.111,12.65,18.1,2.208,23.38,30.273,22.95,-8.4942,
         20.98)

station_alt=c(15,380,212,115,0,180,864,293,730,680,25,974,93,209,70,125,70,
              60,200,669,84,516,118,760,420,110,30,156,550,35,50,75,112)

institution=c("University of Wales, Aberystwyth","ALOMAR/Andøya Space Center",
              "National Technical University of Athens, Physics Department",
              "Universitat Politecnica de Catalunya, Barcelona",
              "Royal Netherlands Meteorological Institute","Institute of Geophysics, Polish Academy of Sciences, Warsaw",
              "Leibniz Institute for Tropospheric Research","Institute for Earth Sciences(Instituto de Ciencias da Terra)",
              "Karlsruher Institut für Technologie, Garmisch-Partenkirchen",
              "Andalusian Institute for Earth System Research, University of Granada",
              "Max-Planck-Institut für Meteorologie","DWD Meteorological Observatory Hohenpeissenberg",
              "National Institute of R&D for Optoelectronics","Joint Research Centre - Institute for Environment and Sustainability, Ispra",
              "Leibniz Institute for Atmospheric Physics",
              "Leibniz Institute for Tropospheric Research","Swedish Defence Research Agency",
              "Lille 1 University - Science and Technology","B.I. Stepanov Institute of Physiscs, Minsk",
              "Centro de Investigaciones Energéticas, Medioambientales y Tecnológicas, Madrid",
              "Leibniz Institute for Tropospheric Research","Meteorologisches Institut der Ludwig-Maximilians-Universität, München",
              "Dipartimento di Fisica - Università degli Studi di Napoli Federico II",
              "Consiglio Nazionale delle Ricerche - Istituto di Metodologie per l'Analisi Ambientale, Potenza",
              "Observatoire de Physique du Globe","Consiglio Nazionale delle Ricerche - Istituto di Scienze Marine",
              "University of Salento, Physics Department, Lecce","Centre National de la Recherche Scientifique - Institut Pierre Simon Laplace, Paris",
              "Institute of Electronics, Bulgarian Academy of Sciences, Sofia",
              "Research Park of Saint-Petersburg State University - Observatory of Environmental Safety",
              "Aristotle University of Thessaloniki","Phys. Dep. and Env. Res. Institute, University College Cork",
              "University of Warsaw, Faculty of Physics")

acronym=c("UWA","ALOMAR","NTUA","TSC-UPC","KNMI","IGF PAN","TROPOS","ICT","KIT","IISTA-CEAMA",
          "DKRZ","DWD","INOE","JRC","IAP","TROPOS","FOI","USTL","IFAN",
          "CIEMAT","TROPOS","MIM-LMU","UNINA","CNR-IMAA","OPGC-LaMP","CNR-ISAC",
          "UNISALENTO","SIRTA","IE-BAS","SPBU","AUTH","UCC","IGF-FUW")

PI=c("Geraint Vaughan","Michael Gausa","Alexandros Papayannis","Adolfo Comeron",
     "Arnoud Apituley","Aleksander Pietruczuk","Dietrich Althausen","Daniele Bortoli",
     "Hannes Vogelmann","Lucas Alados Arboledas","Jens Bosenberg","Ina Mattis",
     "Doina Nicolae","Jean Putaud","Matthias Alpers",
     "Holger Baars, Ulla Wandinger","Ove Gustafsson","Philippe Goloub","Anatoli Chaikovsky",
     "Manuel Pujadas","Holger Baars","Matthias Wiegner","Salvatore Amoruso, Nicola Spinelli",
     "Aldo Amodeo, Gelsomina Pappalardo","Patrick Freville",
     "Davide Dionisi","Maria Rita Perrone","Christophe Pietras, Martial Haeffelin, Francois Ravetta, Jacques Pelon",
     "Dimitar V. Stoyanov","Dmitry Samulenkov","Dimitris Balis","Albert A. Ruth",
     "Iwona S. Stachlewska")

PI_contact=c("","michael.gausa@rocketrange.no","apdlidar@central.ntua.gr",
             "comeron@tsc.upc.es","apituley@knmi.nl","alek@igf.edu.pl","dietrich@tropos.de",
             "db@uevora.pt","hannes.vogelmann@kit.edu","alados@ugr.es","boesenberg@dkrz.de",
             "Ina.Mattis@dwd.de","nnicol@inoe.ro","jean.putaud@ec.europa.eu","",
             "baars@tropos.de",
             "ove.gustafsson@foi.se","philippe.goloub@univ-lille1.fr","chaikov@dragon.bas-net.by",
             "manuel.pujadas@ciemat.es","baars@tropos.de","m.wiegner@lmu.de",
             "salvatore.amoruso@unina.it",
             "aldo.amodeo@imaa.cnr.it","P.Freville@opgc.fr","d.dionisi@isac.cnr.it",
             "maria.rita.perrone@le.infn.it","christophe.pietras@lmd.polytechnique.fr",
             "dvstoyan@ie.bas.bg","dmitriy.samulenkov@spbu.ru","balis@auth.gr",
             "a.ruth@ucc.ie","iwona.stachlewska@igf.fuw.edu.pl")

syst = as.matrix(syst_df)
# syst = syst[-c(16, 17, 26), ]

for (p in 1:33)
{
  for (y in 2000:2019)
  {
    db_355 = db_prof_year_355[db_prof_year_355$Station == loc2[p] &
                                db_prof_year_355$Year == y, c(3:27)]
    colnames(db_355)[2:25] = paste0(colnames(db_355)[2:25], "_355")
    
    db_532 = db_prof_year_532[db_prof_year_532$Station == loc2[p] &
                                db_prof_year_532$Year == y, c(3:27)]
    colnames(db_532)[2:25] = paste0(colnames(db_532)[2:25], "_532")
    
    db_1064 = db_prof_year_1064[db_prof_year_1064$Station == loc2[p] &
                                  db_prof_year_1064$Year == y, c(3:19)]
    colnames(db_1064)[2:17] = paste0(colnames(db_1064)[2:17], "_1064")
    
    db_355 = merge(db_355, appo, by = "Altitude", all = TRUE)
    db_532 = merge(db_532, appo, by = "Altitude", all = TRUE)
    db_1064 = merge(db_1064, appo, by = "Altitude", all = TRUE)
    
    extinction_data = array(NA, dim = c(60, 3, 5))
    extinction_data[, 1, ] = as.matrix(db_355[, c(2, 3, 8, 14, 23)])
    extinction_data[, 2, ] = as.matrix(db_532[, c(2, 3, 8, 14, 23)])
    extinction_data[, 3, ] = matrix(NA, nrow = 60, ncol = 5)
    extinction_data[is.na(extinction_data)] = 9.9692099683868690e+36
    
    backscatter_data = array(NA, dim = c(60, 3, 5))
    backscatter_data[, 1, ] = as.matrix(db_355[, c(4, 5, 10, 16, 24)])
    backscatter_data[, 2, ] = as.matrix(db_532[, c(4, 5, 10, 16, 24)])
    backscatter_data[, 3, ] = as.matrix(db_1064[, c(2, 3, 6, 10, 16)])
    backscatter_data[is.na(backscatter_data)] = 9.9692099683868690e+36
    
    volume_depolarization_data = array(NA, dim = c(60, 3, 5))
    volume_depolarization_data[, 1, ] = as.matrix(db_355[, c(6, 7, 12, 18, 25)])
    volume_depolarization_data[, 2, ] = as.matrix(db_532[, c(6, 7, 12, 18, 25)])
    volume_depolarization_data[, 3, ] = as.matrix(db_1064[, c(4, 5, 8, 12, 17)])
    volume_depolarization_data[is.na(volume_depolarization_data)] = 9.9692099683868690e+36
    
    time_bounds_data = c(as.numeric(
      as.POSIXct(
        paste0(as.character(y), "-01-01 00:00:00"),
        tz = "GMT",
        origin = "1970-01-01 00:00:00"
      )
    ), as.numeric(
      as.POSIXct(
        paste0(as.character(y), "-12-31 23:59:59"),
        tz = "GMT",
        origin = "1970-01-01 00:00:00"
      )
    ))
    
    source_name = lev2db[lev2db$Station == loc2[p] & lev2db$Year == y, 1]
    source_data = " "
    for (n in source_name)
    {
      source_data = paste0(source_data, " ", n)
    }
    
    #creiamo dimensioni
    nv = ncdim_def(
      name = "nv",
      units = "",
      vals = 1:2,
      longname = ""
    )
    altitude = ncdim_def(
      name = "altitude",
      units = "m",
      create_dimvar = TRUE,
      vals = altitude_data,
      longname = "Altitude"
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
      vals = as.numeric(
        as.POSIXct(
          paste0(as.character(y), "-06-30 23:59:59"),
          tz = "GMT",
          origin = "1970-01-01 00:00:00"
        )
      ),
      longname = "Time"
    )
    n_char = ncdim_def(
      name = "n_char",
      units = "",
      vals = 1:nchar(source_data),
      longname = ""
    )
    stats = ncdim_def(
      name = "stats",
      units = "",
      create_dimvar = TRUE,
      vals = 0:4,
      longname = "statistics"
    )
    
    #creiamo le variabili
    extinction = ncvar_def(
      "extinction",
      units = "1/m",
      dim = list(altitude, time, wavelength, stats),
      missval = 9.9692099683868690e+36,
      longname = "aerosol particle extinction coefficient",
      prec = "double"
    )
    backscatter = ncvar_def(
      "backscatter",
      units = "1/m*sr",
      dim = list(altitude, time, wavelength, stats),
      missval = 9.9692099683868690e+36,
      longname = "aerosol particle backscatter coefficient",
      prec = "double"
    )
    volume_depolarization = ncvar_def(
      "volume_depolarization",
      units = "",
      dim = list(altitude, time, wavelength, stats),
      missval = 9.9692099683868690e+36,
      longname = "aerosol volume depolarization coefficient",
      prec = "double"
    )
    
    time_bounds = ncvar_def(
      "time_bounds",
      units = "seconds since 1970-01-01T00:00:00Z",
      dim = list(nv, time),
      prec = "double"
    )
    source = ncvar_def(
      "source",
      units = "",
      dim = n_char,
      longname = "source files",
      prec = "char"
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
    station_altitude = ncvar_def(
      "station_altitude",
      units = "m",
      dim = list(),
      longname = "station altitude above sea level",
      prec = "float"
    )
    
    variabili = list(
      extinction,
      backscatter,
      volume_depolarization,
      time_bounds,
      source,
      latitude,
      longitude,
      station_altitude
    )
    
    #definiamo il file mettendoci le variabili dentro
    year_file = nc_create(
      paste0(
        "ACTRIS_AerRemSen_",
        loc2[p],
        "_Lev03_Annual_",
        as.character(y),
        "_Pro_v02_qc030.nc"
      ),
      variabili,
      force_v4 = TRUE
    )
    
    #aggiungiamo attributi alle variabili
    ncatt_put(year_file, "altitude", "axis", "Z", prec = "text")
    ncatt_put(year_file, "altitude", "positive", "up", prec = "text")
    ncatt_put(year_file, "altitude", "standard_name", "altitude", prec = "text")
    
    ncatt_put(year_file, "stats", "flag_value", "0,1,2,3,4", prec = "text")
    ncatt_put(
      year_file,
      "stats",
      "flag_meaning",
      "0:mean, 1:statistical error mean, 2:median, 3: standard deviation, 4:number of profiles aggregated"
    )
    
    ncatt_put(year_file, "time", "axis", "T", prec = "text")
    ncatt_put(year_file, "time", "standard_name", "time", prec = "text")
    ncatt_put(year_file, "time", "bounds", "time_bounds", prec = "text")
    
    ncatt_put(
      year_file,
      extinction,
      "standard_name",
      "volume_extinction_coefficient_in_air_due_to_ambient_aerosol_particles",
      prec = "text"
    )
    
    ncatt_put(year_file, latitude, "standard_name", "latitude", prec = "text")
    ncatt_put(year_file, longitude, "standard_name", "longitude", prec = "text")
    ncatt_put(
      year_file,
      source,
      "description",
      "List of level 2 files from which are retrieved values averaged in this file"
    )
    
    #inseriamo i valori nel file
    ncvar_put(year_file, "altitude", altitude_data)
    
    ncvar_put(year_file, extinction, extinction_data)
    
    ncvar_put(year_file, backscatter, backscatter_data)
    
    ncvar_put(year_file,
              volume_depolarization,
              volume_depolarization_data)
    
    ncvar_put(year_file, time_bounds, time_bounds_data)
    ncvar_put(year_file, latitude, latit[p])
    ncvar_put(year_file, longitude, longit[p])
    ncvar_put(year_file, station_altitude, station_alt[p])
    ncvar_put(year_file, source, source_data)
    
    ncatt_put(year_file,
              0,
              attname = "processor_name",
              attval = "EAR_clim_v1.exe",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "processor_version",
              attval = "",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "processor_institution",
              attval = "CNR - IMAA",
              prec = "text")
    ncatt_put(
      year_file,
      0,
      attname = "system",
      attval = syst[p, y - 1999],
      prec = "text"
    )
    ncatt_put(
      year_file,
      0,
      attname = "location",
      attval = loc[p],
      prec = "text"
    )
    ncatt_put(
      year_file,
      0,
      attname = "institution",
      attval = institution[p],
      prec = "text"
    )
    ncatt_put(
      year_file,
      0,
      attname = "PI",
      attval = PI[p],
      prec = "text"
    )
    ncatt_put(
      year_file,
      0,
      attname = "PI_affiliation",
      attval = institution[p],
      prec = "text"
    )
    ncatt_put(
      year_file,
      0,
      attname = "PI_affiliation_acronym",
      attval = acronym[p],
      prec = "text"
    )
    ncatt_put(year_file,
              0,
              attname = "PI_address",
              attval = "",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "PI_phone",
              attval = "",
              prec = "text")
    ncatt_put(
      year_file,
      0,
      attname = "PI_email",
      attval = PI_contact[p],
      prec = "text"
    )
    ncatt_put(year_file,
              0,
              attname = "data_originator",
              attval = "Consiglio Nazionale delle Ricerche - Istituto di Metodologie per l'Analisi Ambientale",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "data_originator_affiliation",
              attval = "Consiglio Nazionale delle Ricerche - Istituto di Metodologie per l'Analisi Ambientale",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "data_originator_affiliation_acronym",
              attval = "CNR - IMAA",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "data_originator_address",
              attval = "",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "data_originator_phone",
              attval = "",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "data_originator_email",
              attval = "earlinetdb@actris.imaa.cnr.it",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "data_provider",
              attval = "ACTRIS ARES",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "data_provider_affiliation",
              attval = "Consiglio Nazionale delle Ricerche - Istituto di Metodologie per l'Analisi Ambientale",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "data_provider_affiliation_acronym",
              attval = "CNR - IMAA",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "data_provider_address",
              attval = "",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "data_provider_phone",
              attval = "",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "data_provider_email",
              attval = "earlinetdb@actris.imaa.cnr.it",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "conventions",
              attval = "C.F. - 1.8",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "references",
              attval = "link doc earlinet.org",
              prec = "text")
    ncatt_put(
      year_file,
      0,
      attname = "station_ID",
      attval = loc2[p],
      prec = "text"
    )
    ncatt_put(year_file,
              0,
              attname = "__file_format_version",
              attval = "",
              prec = "text")
    ncatt_put(
      year_file,
      0,
      attname = "history",
      attval = paste0(
        Sys.time(),
        "Generated by free software R, using package ncdf4"
      ),
      prec = "text"
    )
    ncatt_put(
      year_file,
      0,
      attname = "title",
      attval = paste0(
        "Annual average profile measurements - year ",
        as.character(y)
      )
    )
    nc_close(year_file)
    gw = getwd()
    file.rename(
      from = paste0(
        gw,
        "/ACTRIS_AerRemSen_",
        loc2[p],
        "_Lev03_Annual_",
        as.character(y),
        "_Pro_v02_qc030.nc"
      ),
      to = paste0(
        gw,
        "/Level3/Profiles/",
        loc2[p],
        "/ACTRIS_AerRemSen_",
        loc2[p],
        "_Lev03_Annual_",
        as.character(y),
        "_Pro_v02_qc030.nc"
      )
    )
  }
}
