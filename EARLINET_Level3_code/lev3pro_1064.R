#caricare pacchetto isotone per weighted median e radiant.data per weighted standard deviation

load("lev2db.Rda")
loc2 = unique(lev2db$Station)
lev2db_1064 = lev2db[lev2db$Wavelength == "1064", ]
k = nrow(lev2db_1064)
db_allmeasures_1064 = data.frame(NA)

len_0 = function(x)
{
  x = x[x > 0]
  l = length(x)
  return(l)
}

cnt = function(x)
{
  x = x[!is.na(x)]
  x = x[x > 0]
  s = length(x)
  return(s)
}

unq = function(x)
{
  x = x[!is.na(x)]
  s = length(unique(x))
  return(s)
}

f1 = function(x, i)
{
  r = rep(NA, times = length(x))
  ind = which(!is.na(x))
  r[ind] = i
  return(r)
}

sson1 = function(x)
{
  x = as.numeric(x)
  y = 0
  if (x %in% c(3, 4, 5))
  {
    y = "MarAprMay"
  }
  else if (x %in% c(6, 7, 8))
  {
    y = "JunJulAug"
  }
  else if (x %in% c(9, 10, 11))
  {
    y = "SepOctNov"
  }
  else if (x %in% c(1, 2, 12))
  {
    y = "DecJanFeb"
  }
  return(y)
}

sson2 = function(m, y, s)
{
  q = 0
  m = as.numeric(m)
  y = as.numeric(y)
  if (s != "DecJanFeb")
  {
    q = paste0(s, "_", y)
  }
  else if (s == "DecJanFeb" & m == 12)
  {
    q = paste0(s, "_", y, "/", (y + 1))
  }
  else if (s == "DecJanFeb" & m %in% 1:2)
  {
    q = paste0(s, "_", (y - 1), "/", y)
  }
  return(q)
}

w_median = function(x, w)
{
  wm = NA
  ind = which(!is.na(x) & !is.na(w))
  if (length(ind) > 0)
  {
    x = x[ind]
    w = w[ind]
    wm = weighted.median(x, w)
  }
  return(wm)
}

w_sd = function(x, w)
{
  wsd = NA
  ind = which(!is.na(x) & !is.na(w))
  if (length(ind) > 1)
  {
    x = x[ind]
    w = w[ind]
    wsd = weighted.sd(x, w)
  }
  return(wsd)
}

for (i in 1:k)
{
  file0 = nc_open(paste0("./New/", lev2db_1064[i, 2], "/", lev2db_1064[i, 1]))
  alt = ncvar_get(file0, "altitude")
  l1 = which(!is.na(alt) & alt >= 100 & alt <= 12100)
  alt = alt[l1]
  lt = length(alt)
  alt2 = seq(from = 200, to = 12000, by = 200)
  d_b = data.frame(matrix(NA, nrow = lt, ncol = 10))
  d_b[, 1] = rep(lev2db_1064[i, 2], times = lt)
  d_b[, 2] = rep(lev2db_1064[i, 5], times = lt)
  d_b[, 3] = rep(lev2db_1064[i, 6], times = lt)
  alt0 = rep(NA, times = lt)
  for (j in 1:lt)
  {
    ind = which.min(abs(alt2 - alt[j]))
    alt0[j] = alt2[ind]
  }
  d_b[, 4] = alt0
  vd = try(ncvar_get(file0, "volumedepolarization"), silent = TRUE)
  err_vd = try(ncvar_get(file0, "error_volumedepolarization"), silent = TRUE)
  if (class(vd) != "try-error")
  {
    vd = vd[l1]
    d_b[, 7] = vd
    if (sum(is.na(vd)) < lt)
    {
      d_b[, 10] = rep(i, times = lt)
    }
  }
  if (class(err_vd) != "try-error")
  {
    err_vd = err_vd[l1]
    d_b[, 8] = err_vd
  }
  backscatter = try(ncvar_get(file0, "backscatter"), silent = TRUE)
  err_backscatter = try(ncvar_get(file0, "error_backscatter"), silent = TRUE)
  if (length(class(backscatter)) > 1)
  {
    backscatter = rowMeans(backscatter)
    err_backscatter = rowMeans(err_backscatter)
  }
  if (class(backscatter) != "try-error")
  {
    backscatter = backscatter[l1]
    d_b[, 5] = backscatter
    if (sum(is.na(backscatter)) < lt)
    {
      d_b[, 9] = rep(i, times = lt)
    }
  }
  if (class(err_backscatter) != "try-error")
  {
    err_backscatter = err_backscatter[l1]
    d_b[, 6] = err_backscatter
  }
  nc_close(file0)
  if (i == 1)
  {
    db_allmeasures_1064 = d_b
  }
  else
  {
    db_allmeasures_1064 = rbind(db_allmeasures_1064, d_b)
  }
  print(i)
}

colnames(db_allmeasures_1064) = c(
  "Station",
  "Year",
  "Month",
  "Altitude",
  "Backscatter",
  "Error_Backscatter",
  "VolDep",
  "Error_VolDep",
  "FileBackscatter",
  "FileVolDep"
)
db_allmeasures_1064 = db_allmeasures_1064[!(is.na(db_allmeasures_1064$Backscatter) &
                                              is.na(db_allmeasures_1064$VolDep)), ]
db_allmeasures_1064$Season = sapply(db_allmeasures_1064$Month, sson1)
db_allmeasures_1064$Season_Year = mapply(
  sson2,
  db_allmeasures_1064$Month,
  db_allmeasures_1064$Year,
  db_allmeasures_1064$Season
)
db_allmeasures_1064[is.na(db_allmeasures_1064)] = NA
db_allmeasures_1064$Backscatter[is.infinite(db_allmeasures_1064$Backscatter)] =
  NA
db_allmeasures_1064$Error_Backscatter[is.infinite(db_allmeasures_1064$Error_Backscatter)] =
  NA
db_allmeasures_1064$VolDep[is.infinite(db_allmeasures_1064$VolDep)] = NA
db_allmeasures_1064$Error_VolDep[is.infinite(db_allmeasures_1064$Error_VolDep)] =
  NA
save(db_allmeasures_1064, file = "AllMeasures1064.Rda")

df_m_1064_mean = aggregate(
  db_allmeasures_1064[5:8],
  by = list(
    db_allmeasures_1064$Station,
    db_allmeasures_1064$Year,
    db_allmeasures_1064$Month,
    db_allmeasures_1064$Altitude
  ),
  FUN = mean,
  na.rm = TRUE
)
names(df_m_1064_mean)[1:4] = c("Station", "Year", "Month", "Altitude")
df_m_1064_mean = df_m_1064_mean[order(
  df_m_1064_mean$Station,
  df_m_1064_mean$Year,
  df_m_1064_mean$Month,
  df_m_1064_mean$Altitude
), ]
df_m_1064_mean[is.na(df_m_1064_mean)] = NA

df_m_1064_cnt = aggregate(
  db_allmeasures_1064[9:10],
  by = list(
    db_allmeasures_1064$Station,
    db_allmeasures_1064$Year,
    db_allmeasures_1064$Month,
    db_allmeasures_1064$Altitude
  ),
  FUN = cnt
)
colnames(df_m_1064_cnt) = c(
  "Station",
  "Year",
  "Month",
  "Altitude",
  "NumberMeasuresBackscatter_Month",
  "NumberMeasuresVolDep_Month"
)
df_m_1064_cnt = df_m_1064_cnt[order(
  df_m_1064_cnt$Station,
  df_m_1064_cnt$Year,
  df_m_1064_cnt$Month,
  df_m_1064_cnt$Altitude
), ]

df_m_1064_unq = aggregate(
  db_allmeasures_1064[9:10],
  by = list(
    db_allmeasures_1064$Station,
    db_allmeasures_1064$Year,
    db_allmeasures_1064$Month,
    db_allmeasures_1064$Altitude
  ),
  FUN = unq
)
colnames(df_m_1064_unq) = c(
  "Station",
  "Year",
  "Month",
  "Altitude",
  "NumberProfilesBackscatter_Month",
  "NumberProfilesVolDep_Month"
)
df_m_1064_unq = df_m_1064_unq[order(
  df_m_1064_unq$Station,
  df_m_1064_unq$Year,
  df_m_1064_unq$Month,
  df_m_1064_unq$Altitude
), ]

db_prof_month_1064 = cbind(df_m_1064_mean, df_m_1064_cnt[5:6], df_m_1064_unq[5:6])
save(db_prof_month_1064, file = "ProfilesMonth1064.Rda")

df_s_1064_mean = aggregate(
  db_allmeasures_1064[5:8],
  by = list(
    db_allmeasures_1064$Station,
    db_allmeasures_1064$Season_Year,
    db_allmeasures_1064$Altitude
  ),
  FUN = mean,
  na.rm = TRUE
)
colnames(df_s_1064_mean) = c(
  "Station",
  "Season",
  "Altitude",
  "Backscatter_Mean",
  "Error_Backscatter_Mean",
  "VolDep_Mean",
  "Error_VolDep_Mean"
)
df_s_1064_mean = df_s_1064_mean[order(
  df_s_1064_mean$Station,
  substr(df_s_1064_mean$Season, 11, 14),
  match(
    substr(df_s_1064_mean$Season, 1, 9),
    c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
  ),
  df_s_1064_mean$Altitude
), ]
df_s_1064_mean[is.na(df_s_1064_mean)] = NA

df_s_1064_median = aggregate(
  db_allmeasures_1064[5:8],
  by = list(
    db_allmeasures_1064$Station,
    db_allmeasures_1064$Season_Year,
    db_allmeasures_1064$Altitude
  ),
  FUN = median,
  na.rm = TRUE
)
colnames(df_s_1064_median) = c(
  "Station",
  "Season",
  "Altitude",
  "Backscatter_Median",
  "Error_Backscatter_Median",
  "VolDep_Median",
  "Error_VolDep_Median"
)
df_s_1064_median = df_s_1064_median[order(
  df_s_1064_median$Station,
  substr(df_s_1064_median$Season, 11, 14),
  match(
    substr(df_s_1064_median$Season, 1, 9),
    c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
  ),
  df_s_1064_median$Altitude
), ]
df_s_1064_median[is.na(df_s_1064_median)] = NA

df_s_1064_cnt = aggregate(
  db_allmeasures_1064[9:10],
  by = list(
    db_allmeasures_1064$Station,
    db_allmeasures_1064$Season_Year,
    db_allmeasures_1064$Altitude
  ),
  FUN = cnt
)
colnames(df_s_1064_cnt) = c(
  "Station",
  "Season",
  "Altitude",
  "NumberMeasuresBackscatter",
  "NumberMeasuresVolDep"
)
df_s_1064_cnt = df_s_1064_cnt[order(
  df_s_1064_cnt$Station,
  substr(df_s_1064_cnt$Season, 11, 14),
  match(
    substr(df_s_1064_cnt$Season, 1, 9),
    c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
  ),
  df_s_1064_cnt$Altitude
), ]

df_s_1064_unq = aggregate(
  db_allmeasures_1064[9:10],
  by = list(
    db_allmeasures_1064$Station,
    db_allmeasures_1064$Season_Year,
    db_allmeasures_1064$Altitude
  ),
  FUN = unq
)
colnames(df_s_1064_unq) = c(
  "Station",
  "Season",
  "Altitude",
  "NumberProfilesBackscatter",
  "NumberProfilesVolDep"
)
df_s_1064_unq = df_s_1064_unq[order(
  df_s_1064_unq$Station,
  substr(df_s_1064_unq$Season, 11, 14),
  match(
    substr(df_s_1064_unq$Season, 1, 9),
    c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
  ),
  df_s_1064_unq$Altitude
), ]

df_s_1064_sd = aggregate(
  db_allmeasures_1064[5:8],
  by = list(
    db_allmeasures_1064$Station,
    db_allmeasures_1064$Season_Year,
    db_allmeasures_1064$Altitude
  ),
  FUN = sd,
  na.rm = TRUE
)
colnames(df_s_1064_sd) = c(
  "Station",
  "Season",
  "Altitude",
  "Backscatter_StDev",
  "Error_Backscatter_StDev",
  "VolDep_StDev",
  "Error_VolDep_StDev"
)
df_s_1064_sd = df_s_1064_sd[order(
  df_s_1064_sd$Station,
  substr(df_s_1064_sd$Season, 11, 14),
  match(
    substr(df_s_1064_sd$Season, 1, 9),
    c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
  ),
  df_s_1064_sd$Altitude
), ]
df_s_1064_sd[is.na(df_s_1064_sd)] = NA

db_prof_season_1064 = cbind(df_s_1064_mean,
                            df_s_1064_median[4:7],
                            df_s_1064_sd[4:7],
                            df_s_1064_cnt[4:5],
                            df_s_1064_unq[4:5])
save(db_prof_season_1064, file = "ProfilesSeason1064.Rda")

s0 = aggregate(
  db_prof_month_1064[9:10],
  by = list(
    db_prof_month_1064$Station,
    db_prof_month_1064$Year,
    db_prof_month_1064$Altitude
  ),
  FUN = len_0
)
colnames(s0) = c(
  "Station",
  "Year",
  "Altitude",
  "Number_Month_Measured_Bs",
  "Number_Month_Measured_VolDep"
)

wgs = merge(
  db_prof_month_1064[c(1:4, 9:10)],
  s0,
  by = c("Station", "Year", "Altitude"),
  all = TRUE
)
wgs$Weights_Bs = (1 / wgs$NumberMeasuresBackscatter_Month) * (1 / wgs$Number_Month_Measured_Bs)
wgs$Weights_Bs[wgs$Weights_Bs == Inf] = NA
wgs$Weights_VolDep = (1 / wgs$NumberMeasuresVolDep_Month) * (1 / wgs$Number_Month_Measured_VolDep)
wgs$Weights_VolDep[wgs$Weights_VolDep == Inf] = NA

db_allmeasures_1064_year = merge(
  db_allmeasures_1064,
  wgs[c(1:4, 9:10)],
  by = c("Station", "Year", "Month", "Altitude"),
  all = TRUE
)
df_y_1064_mean = aggregate(
  db_prof_month_1064[5:8],
  by = list(
    db_prof_month_1064$Station,
    db_prof_month_1064$Year,
    db_prof_month_1064$Altitude
  ),
  FUN = mean,
  na.rm = TRUE
)
colnames(df_y_1064_mean) = c(
  "Station",
  "Year",
  "Altitude",
  "Backscatter_Mean",
  "Error_Backscatter_Mean",
  "VolDep_Mean",
  "Error_VolDep_Mean"
)
df_y_1064_mean = df_y_1064_mean[order(df_y_1064_mean$Station,
                                      df_y_1064_mean$Year,
                                      df_y_1064_mean$Altitude), ]
df_y_1064_mean[is.na(df_y_1064_mean)] = NA

df_y_1064_c = aggregate(
  db_allmeasures_1064_year[c(5:8, 13:14)],
  by = list(
    db_allmeasures_1064_year$Station,
    db_allmeasures_1064_year$Year,
    db_allmeasures_1064_year$Altitude
  ),
  FUN = c
)
colnames(df_y_1064_c)[1:3] = c("Station", "Year", "Altitude")
df_y_1064_c = df_y_1064_c[order(df_y_1064_c$Station, df_y_1064_c$Year, df_y_1064_c$Altitude), ]

df_y_1064_median = df_y_1064_c[1:3]
for (j in 4:5)
{
  df_y_1064_median = cbind(df_y_1064_median,
                           mapply(w_median, df_y_1064_c[, j], df_y_1064_c$Weights_Bs))
}
for (j in 6:7)
{
  df_y_1064_median = cbind(df_y_1064_median,
                           mapply(w_median, df_y_1064_c[, j], df_y_1064_c$Weights_VolDep))
}
colnames(df_y_1064_median)[4:7] = c(
  "Backscatter_Median",
  "Error_Backscatter_Median",
  "VolDep_Median",
  "Error_VolDep_Median"
)

df_y_1064_sd = df_y_1064_c[1:3]
for (j in 4:5)
{
  df_y_1064_sd = cbind(df_y_1064_sd,
                       mapply(w_sd, df_y_1064_c[, j], df_y_1064_c$Weights_Bs))
}
for (j in 6:7)
{
  df_y_1064_sd = cbind(df_y_1064_sd,
                       mapply(w_sd, df_y_1064_c[, j], df_y_1064_c$Weights_VolDep))
}
colnames(df_y_1064_sd)[4:7] = c(
  "Backscatter_StDev",
  "Error_Backscatter_StDev",
  "VolDep_StDev",
  "Error_VolDep_StDev"
)

df_y_1064_unq = aggregate(
  db_allmeasures_1064[9:10],
  by = list(
    db_allmeasures_1064$Station,
    db_allmeasures_1064$Year,
    db_allmeasures_1064$Altitude
  ),
  FUN = unq
)
colnames(df_y_1064_unq) = c("Station",
                            "Year",
                            "Altitude",
                            "NumberProfilesBackscatter",
                            "NumberProfilesVolDep")
df_y_1064_unq = df_y_1064_unq[order(df_y_1064_unq$Station,
                                    df_y_1064_unq$Year,
                                    df_y_1064_unq$Altitude), ]

df_y_1064_cnt = aggregate(
  db_allmeasures_1064[9:10],
  by = list(
    db_allmeasures_1064$Station,
    db_allmeasures_1064$Year,
    db_allmeasures_1064$Altitude
  ),
  FUN = cnt
)
colnames(df_y_1064_cnt) = c("Station",
                            "Year",
                            "Altitude",
                            "NumberMeasuresBackscatter",
                            "NumberMeasuresVolDep")
df_y_1064_cnt = df_y_1064_cnt[order(df_y_1064_cnt$Station,
                                    df_y_1064_cnt$Year,
                                    df_y_1064_cnt$Altitude), ]

db_prof_year_1064 = cbind(df_y_1064_mean,
                          df_y_1064_median[4:7],
                          df_y_1064_sd[4:7],
                          df_y_1064_cnt[4:5],
                          df_y_1064_unq[4:5])
save(db_prof_year_1064, file = "ProfilesYear1064.Rda")

s1 = aggregate(
  db_prof_month_1064[9:10],
  by = list(
    db_prof_month_1064$Station,
    db_prof_month_1064$Month,
    db_prof_month_1064$Altitude
  ),
  FUN = len_0
)
colnames(s1) = c(
  "Station",
  "Month",
  "Altitude",
  "Number_Year_Measured_Bs",
  "Number_Year_Measured_VolDep"
)

wgs1 = merge(
  db_prof_month_1064[c(1:4, 9:10)],
  s1,
  by = c("Station", "Month", "Altitude"),
  all = TRUE
)
wgs1$Weights_Bs = (1 / wgs1$NumberMeasuresBackscatter_Month) * (1 / wgs1$Number_Year_Measured_Bs)
wgs1$Weights_Bs[wgs1$Weights_Bs == Inf] = NA
wgs1$Weights_VolDep = (1 / wgs1$NumberMeasuresVolDep_Month) * (1 / wgs1$Number_Year_Measured_VolDep)
wgs1$Weights_VolDep[wgs1$Weights_VolDep == Inf] = NA

db_allmeasures_1064_nm = merge(
  db_allmeasures_1064,
  wgs1[c(1:4, 9:10)],
  by = c("Station", "Year", "Month", "Altitude"),
  all = TRUE
)
df_nm_1064_mean = aggregate(
  db_prof_month_1064[5:8],
  by = list(
    db_prof_month_1064$Station,
    db_prof_month_1064$Month,
    db_prof_month_1064$Altitude
  ),
  FUN = mean,
  na.rm = TRUE
)
colnames(df_nm_1064_mean) = c(
  "Station",
  "Month",
  "Altitude",
  "Backscatter_Mean",
  "Error_Backscatter_Mean",
  "VolDep_Mean",
  "Error_VolDep_Mean"
)
df_nm_1064_mean = df_nm_1064_mean[order(df_nm_1064_mean$Station,
                                        df_nm_1064_mean$Month,
                                        df_nm_1064_mean$Altitude), ]
df_nm_1064_mean[is.na(df_nm_1064_mean)] = NA

df_nm_1064_c = aggregate(
  db_allmeasures_1064_nm[c(5:8, 13:14)],
  by = list(
    db_allmeasures_1064_nm$Station,
    db_allmeasures_1064_nm$Month,
    db_allmeasures_1064_nm$Altitude
  ),
  FUN = c
)
colnames(df_nm_1064_c)[1:3] = c("Station", "Month", "Altitude")
df_nm_1064_c = df_nm_1064_c[order(df_nm_1064_c$Station,
                                  df_nm_1064_c$Month,
                                  df_nm_1064_c$Altitude), ]

df_nm_1064_median = df_nm_1064_c[1:3]
for (j in 4:5)
{
  df_nm_1064_median = cbind(df_nm_1064_median,
                            mapply(w_median, df_nm_1064_c[, j], df_nm_1064_c$Weights_Bs))
}
for (j in 6:7)
{
  df_nm_1064_median = cbind(df_nm_1064_median,
                            mapply(w_median, df_nm_1064_c[, j], df_nm_1064_c$Weights_VolDep))
}
colnames(df_nm_1064_median)[4:7] = c(
  "Backscatter_Median",
  "Error_Backscatter_Median",
  "VolDep_Median",
  "Error_VolDep_Median"
)

df_nm_1064_sd = df_nm_1064_c[1:3]
for (j in 4:5)
{
  df_nm_1064_sd = cbind(df_nm_1064_sd,
                        mapply(w_sd, df_nm_1064_c[, j], df_nm_1064_c$Weights_Bs))
}
for (j in 6:7)
{
  df_nm_1064_sd = cbind(df_nm_1064_sd,
                        mapply(w_sd, df_nm_1064_c[, j], df_nm_1064_c$Weights_VolDep))
}
colnames(df_nm_1064_sd)[4:7] = c(
  "Backscatter_StDev",
  "Error_Backscatter_StDev",
  "VolDep_StDev",
  "Error_VolDep_StDev"
)

df_nm_1064_unq = aggregate(
  db_allmeasures_1064[9:10],
  by = list(
    db_allmeasures_1064$Station,
    db_allmeasures_1064$Month,
    db_allmeasures_1064$Altitude
  ),
  FUN = unq
)
colnames(df_nm_1064_unq) = c(
  "Station",
  "Month",
  "Altitude",
  "NumberProfilesBackscatter",
  "NumberProfilesVolDep"
)
df_nm_1064_unq = df_nm_1064_unq[order(df_nm_1064_unq$Station,
                                      df_nm_1064_unq$Month,
                                      df_nm_1064_unq$Altitude), ]

df_nm_1064_cnt = aggregate(
  db_allmeasures_1064[9:10],
  by = list(
    db_allmeasures_1064$Station,
    db_allmeasures_1064$Month,
    db_allmeasures_1064$Altitude
  ),
  FUN = cnt
)
colnames(df_nm_1064_cnt) = c(
  "Station",
  "Month",
  "Altitude",
  "NumberMeasuresBackscatter",
  "NumberMeasuresVolDep"
)
df_nm_1064_cnt = df_nm_1064_cnt[order(df_nm_1064_cnt$Station,
                                      df_nm_1064_cnt$Month,
                                      df_nm_1064_cnt$Altitude), ]

db_prof_nm_1064 = cbind(
  df_nm_1064_mean,
  df_nm_1064_median[4:7],
  df_nm_1064_sd[4:7],
  df_nm_1064_cnt[4:5],
  df_nm_1064_unq[4:5]
)
save(db_prof_nm_1064, file = "ProfilesNormalMonth1064.Rda")

s2 = aggregate(
  db_prof_season_1064[16:17],
  by = list(
    db_prof_season_1064$Station,
    substr(db_prof_season_1064$Season, 1, 9),
    db_prof_season_1064$Altitude
  ),
  FUN = len_0
)
colnames(s2) = c(
  "Station",
  "Season",
  "Altitude",
  "Number_Year_Measured_Bs",
  "Number_Year_Measured_VolDep"
)

db_season = db_prof_season_1064[c(1:3, 16:17)]
colnames(db_season)[2] = "Season_Year"
db_season$Season = substr(db_season$Season_Year, 1, 9)
wgs2 = merge(db_season,
             s2,
             by = c("Station", "Season", "Altitude"),
             all = TRUE)
wgs2$Weights_Bs = (1 / wgs2$NumberMeasuresBackscatter) * (1 / wgs2$Number_Year_Measured_Bs)
wgs2$Weights_Bs[wgs2$Weights_Bs == Inf] = NA
wgs2$Weights_VolDep = (1 / wgs2$NumberMeasuresVolDep) * (1 / wgs2$Number_Year_Measured_VolDep)
wgs2$Weights_VolDep[wgs2$Weights_VolDep == Inf] = NA

db_allmeasures_1064_ns = merge(
  db_allmeasures_1064[c(1, 4:12)],
  wgs2[c(1:4, 9:10)],
  by = c("Station", "Season_Year", "Season", "Altitude"),
  all = TRUE
)
df_ns_1064_mean = aggregate(
  db_prof_season_1064[4:7],
  by = list(
    db_prof_season_1064$Station,
    substr(db_prof_season_1064$Season, 1, 9),
    db_prof_season_1064$Altitude
  ),
  FUN = mean,
  na.rm = TRUE
)
colnames(df_ns_1064_mean) = c(
  "Station",
  "Season",
  "Altitude",
  "Backscatter_Mean",
  "Error_Backscatter_Mean",
  "VolDep_Mean",
  "Error_VolDep_Mean"
)
df_ns_1064_mean = df_ns_1064_mean[order(
  df_ns_1064_mean$Station,
  match(
    df_ns_1064_mean$Season,
    c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
  ),
  df_ns_1064_mean$Altitude
), ]
df_ns_1064_mean[is.na(df_ns_1064_mean)] = NA

df_ns_1064_c = aggregate(
  db_allmeasures_1064_ns[c(5:8, 11:12)],
  by = list(
    db_allmeasures_1064_ns$Station,
    db_allmeasures_1064_ns$Season,
    db_allmeasures_1064_ns$Altitude
  ),
  FUN = c
)
colnames(df_ns_1064_c)[1:3] = c("Station", "Season", "Altitude")
df_ns_1064_c = df_ns_1064_c[order(df_ns_1064_c$Station,
                                  match(
                                    df_ns_1064_c$Season,
                                    c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
                                  ),
                                  df_ns_1064_c$Altitude), ]

df_ns_1064_median = df_ns_1064_c[1:3]
for (j in 4:5)
{
  df_ns_1064_median = cbind(df_ns_1064_median,
                            mapply(w_median, df_ns_1064_c[, j], df_ns_1064_c$Weights_Bs))
}
for (j in 6:7)
{
  df_ns_1064_median = cbind(df_ns_1064_median,
                            mapply(w_median, df_ns_1064_c[, j], df_ns_1064_c$Weights_VolDep))
}
colnames(df_ns_1064_median)[4:7] = c(
  "Backscatter_Median",
  "Error_Backscatter_Median",
  "VolDep_Median",
  "Error_VolDep_Median"
)

df_ns_1064_sd = df_ns_1064_c[1:3]
for (j in 4:5)
{
  df_ns_1064_sd = cbind(df_ns_1064_sd,
                        mapply(w_sd, df_ns_1064_c[, j], df_ns_1064_c$Weights_Bs))
}
for (j in 6:7)
{
  df_ns_1064_sd = cbind(df_ns_1064_sd,
                        mapply(w_sd, df_ns_1064_c[, j], df_ns_1064_c$Weights_VolDep))
}
colnames(df_ns_1064_sd)[4:7] = c(
  "Backscatter_StDev",
  "Error_Backscatter_StDev",
  "VolDep_StDev",
  "Error_VolDep_StDev"
)

df_ns_1064_unq = aggregate(
  db_allmeasures_1064[9:10],
  by = list(
    db_allmeasures_1064$Station,
    db_allmeasures_1064$Season,
    db_allmeasures_1064$Altitude
  ),
  FUN = unq
)
colnames(df_ns_1064_unq) = c(
  "Station",
  "Season",
  "Altitude",
  "NumberProfilesBackscatter",
  "NumberProfilesVolDep"
)
df_ns_1064_unq = df_ns_1064_unq[order(df_ns_1064_unq$Station,
                                      match(
                                        df_ns_1064_unq$Season,
                                        c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
                                      ),
                                      df_ns_1064_unq$Altitude), ]

df_ns_1064_cnt = aggregate(
  db_allmeasures_1064[9:10],
  by = list(
    db_allmeasures_1064$Station,
    db_allmeasures_1064$Season,
    db_allmeasures_1064$Altitude
  ),
  FUN = cnt
)
colnames(df_ns_1064_cnt) = c(
  "Station",
  "Season",
  "Altitude",
  "NumberMeasuresBackscatter",
  "NumberMeasuresVolDep"
)
df_ns_1064_cnt = df_ns_1064_cnt[order(df_ns_1064_cnt$Station,
                                      match(
                                        df_ns_1064_cnt$Season,
                                        c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
                                      ),
                                      df_ns_1064_cnt$Altitude), ]

db_prof_ns_1064 = cbind(
  df_ns_1064_mean,
  df_ns_1064_median[4:7],
  df_ns_1064_sd[4:7],
  df_ns_1064_cnt[4:5],
  df_ns_1064_unq[4:5]
)
save(db_prof_ns_1064, file = "ProfilesNormalSeason1064.Rda")
