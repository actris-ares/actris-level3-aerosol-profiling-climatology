load("lev2db.Rda")
load("lev2db_layers.Rda")
lev2db_e = lev2db[lev2db$Type == "e", ]
k = nrow(lev2db_e)

db_int_e = data.frame(matrix(NA, nrow = k, ncol = 20))
nms = c(
  colnames(lev2db)[c(1:7)],
  "H63",
  "TotalAOD",
  "Error_TotalAOD",
  "PBLAOD",
  "Error_PBLAOD",
  "FTAOD",
  "Error_FTAOD",
  "TotalLidarRatio",
  "Error_TotalLidarRatio",
  "PBLLidarRatio",
  "Error_PBLLidarRatio",
  "FTLidarRatio",
  "Error_FTLidarRatio"
)
colnames(db_int_e) = nms

for (i in 1:k)
{
  file1 = nc_open(paste0(
    "./New/",
    substr(lev2db_e$File_name[i], 20, 22),
    "/",
    lev2db_e$File_name[i]
  ))
  lydb = lev2db_layers[lev2db_layers$File_name == lev2db_e$File_name[i], ]
  lydb = lydb[!is.na(lydb$Top_Layer), ]
  alt = ncvar_get(file1, "altitude")
  m = length(alt)
  l1 = which(!is.na(alt) & alt <= 12000)
  alt = alt[l1]
  aslv = ncvar_get(file1, "station_altitude")
  ext = try(ncvar_get(file1, "extinction"), silent = TRUE)
  if (class(ext) == "try-error")
  {
    ext = rep(NA, times = m)
  }
  err_ext = try(ncvar_get(file1, "error_extinction"), silent = TRUE)
  if (class(err_ext) == "try-error")
  {
    err_ext = rep(NA, times = m)
  }
  s0 = try(ncvar_get(file1, "lidarratio"), silent = TRUE)
  if (class(s0) == "try-error")
  {
    s0 = rep(NA, times = m)
  }
  err_s0 = try(ncvar_get(file1, "error_lidarratio"), silent = TRUE)
  if (class(err_s0) == "try-error")
  {
    err_s0 = rep(NA, times = m)
  }
  ext = ext[l1]
  err_ext = err_ext[l1]
  s0 = s0[l1]
  err_s0 = err_s0[l1]
  l3 = which(!is.na(ext) & abs(ext) <= 0.01 & err_ext + ext >= 0)
  ext = ext[l3]
  err_ext = err_ext[l3]
  alt_ext = alt[l3]
  m_ext = length(alt_ext)
  l5 = which(!is.na(s0) & s0 >= -100 & s0 <= 200 & s0 + err_s0 >= 0)
  s0 = s0[l5]
  err_s0 = err_s0[l5]
  alt_s0 = alt[l5]
  m_s0 = length(alt_s0)
  if (nrow(lydb) > 0)
  {
    pbl = min(lydb$Top_Layer)
  }
  else
  {
    pbl = try(ncvar_get(file1, "aerosollayerheight"), silent = TRUE)
    if (class(pbl) == "try-error")
    {
      pbl = NA
    }
    else
    {
      pbl = as.numeric(pbl)
    }
  }
  db_int_e[i, 1:7] = lev2db_e[i, 1:7]
  tot_aod = NA
  h63 = NA
  pbl_aod = NA
  ft_aod = NA
  tot_aod_err = NA
  pbl_aod_err = NA
  ft_aod_err = NA
  tot_s0 = NA
  tot_s0_err = NA
  pbl_s0 = NA
  pbl_s0_err = NA
  ft_s0 = NA
  ft_s0_err = NA
  if (m_ext > 0)
  {
    aod = numeric(m_ext)
    aod_err = numeric(m_ext)
    aod[1] = (alt_ext[1] - aslv) * ext[1]
    aod_err[1] = (alt_ext[1] - aslv) * err_ext[1]
    p_aod = numeric(m_ext)
    p_aod_err = numeric(m_ext)
    p_aod[1] = aod[1]
    p_aod_err[1] = aod_err[1]
    if (m_ext > 1)
    {
      for (h in 2:m_ext)
      {
        aod[h] = (ext[h] + ext[h - 1]) * (alt_ext[h] - alt_ext[h - 1]) / 2
        aod_err[h] = (err_ext[h] + err_ext[h - 1]) * (alt_ext[h] - alt_ext[h - 1]) /
          2
        p_aod[h] = p_aod[h - 1] + aod[h]
        p_aod_err[h] = p_aod_err[h - 1] + aod_err[h]
      }
    }
    tot_aod = p_aod[m_ext]
    tot_aod_err = p_aod_err[m_ext]
    ind63 = which(abs(p_aod) > 0.63 * abs(tot_aod))
    if (length(ind63) > 0)
    {
      h63 = alt_ext[min(ind63)]
    }
    if (!is.na(pbl))
    {
      ind_pbl_ext = which(alt_ext <= pbl)
      n_ext = length(ind_pbl_ext)
      if (n_ext == 0)
      {
        pbl_aod = NA
        pbl_aod_err = NA
        ft_aod = tot_aod
        ft_aod_err = tot_aod_err
      }
      else
      {
        pbl_aod = p_aod[n_ext]
        ft_aod = tot_aod - pbl_aod
        pbl_aod_err = p_aod_err[n_ext]
        ft_aod_err = tot_aod_err - pbl_aod_err
      }
    }
  }
  
  if (m_s0 > 0)
  {
    tot_s0 = mean(s0, na.rm = T)
    tot_s0_err = mean(err_s0, na.rm = T)
    if (!is.na(pbl))
    {
      ind_pbl_s0 = which(alt_s0 <= pbl)
      n_s0 = length(ind_pbl_s0)
      if (n_s0 == 0)
      {
        pbl_s0 = NA
        pbl_s0_err = NA
        ft_s0 = tot_s0
        ft_s0_err = tot_s0_err
      }
      else
      {
        pbl_s0 = mean(s0[ind_pbl_s0], na.rm = T)
        pbl_s0_err = mean(err_s0[ind_pbl_s0], na.rm = T)
        ft_s0 = mean(s0[-ind_pbl_s0], na.rm = T)
        ft_s0_err = mean(err_s0[-ind_pbl_s0], na.rm = T)
      }
    }
  }
  db_int_e[i, 8] = h63
  db_int_e[i, 9] = tot_aod
  db_int_e[i, 10] = tot_aod_err
  db_int_e[i, 11] = pbl_aod
  db_int_e[i, 12] = pbl_aod_err
  db_int_e[i, 13] = ft_aod
  db_int_e[i, 14] = ft_aod_err
  db_int_e[i, 15] = tot_s0
  db_int_e[i, 16] = tot_s0_err
  db_int_e[i, 17] = pbl_s0
  db_int_e[i, 18] = pbl_s0_err
  db_int_e[i, 19] = ft_s0
  db_int_e[i, 20] = ft_s0_err
  print(i)
  nc_close(file1)
}
db_int_e[is.na(db_int_e)] = NA
save(db_int_e, file = "Lev3Int_e.Rda")

load("Lev3Int_e.Rda")

ang = function(x1, x2)
{
  num = log(x1 / x2)
  den = log(532 / 355)
  y = num / den
  return(y)
}

ang_err = function(x1, x2, e1, e2)
{
  num1 = e1 / x1
  num2 = e2 / x2
  num = num1 + num2
  den = log(532 / 355)
  y = num / den
  return(y)
}

db_int_e_355 = db_int_e[db_int_e$Wavelength == "0355" &
                          !is.na(db_int_e$TotalAOD), 1:16]
k = nrow(db_int_e_355)
db_angstrom = data.frame(matrix(NA, nrow = k, ncol = 10))
db_angstrom[, 1:4] = db_int_e_355[, c(2, 5:7)]
colnames(db_angstrom) = c(
  colnames(db_int_e_355)[c(2, 5:7)],
  "TotalAng",
  "Error_TotalAng",
  "PBLAng",
  "Error_PBLAng",
  "FTAng",
  "Error_FTAng"
)

for (i in 1:k)
{
  db1 = db_int_e[db_int_e$Station == db_int_e_355$Station[i] &
                   db_int_e$Wavelength == "0532" &
                   db_int_e$Year == db_int_e_355$Year[i] &
                   db_int_e$Month == db_int_e_355$Month[i] &
                   db_int_e$Day == db_int_e_355$Day[i], ]
  db1 = db1[!is.na(db1$TotalAOD), ]
  if (nrow(db1 > 0))
  {
    tot_aod1 = db_int_e_355$TotalAOD[i]
    err_tot_aod1 = db_int_e_355$Error_TotalAOD[i]
    tot_aod2 = db1$TotalAOD[1]
    err_tot_aod2 = db1$Error_TotalAOD[1]
    pbl_aod1 = db_int_e_355$PBLAOD[i]
    err_pbl_aod1 = db_int_e_355$Error_PBLAOD[i]
    pbl_aod2 = db1$PBLAOD[1]
    err_pbl_aod2 = db1$Error_PBLAOD[1]
    ft_aod1 = db_int_e_355$FTAOD[i]
    err_ft_aod1 = db_int_e_355$Error_FTAOD[i]
    ft_aod2 = db1$FTAOD[1]
    err_ft_aod2 = db1$Error_FTAOD[1]
    tot_ang = ang(tot_aod1, tot_aod2)
    err_tot_ang = ang_err(tot_aod1, tot_aod2, err_tot_aod1, err_tot_aod2)
    pbl_ang = ang(pbl_aod1, pbl_aod2)
    err_pbl_ang = ang_err(pbl_aod1, pbl_aod2, err_pbl_aod1, err_pbl_aod2)
    ft_ang = ang(ft_aod1, ft_aod2)
    err_ft_ang = ang_err(ft_aod1, ft_aod2, err_ft_aod1, err_ft_aod2)
    db_angstrom$TotalAng[i] = tot_ang
    db_angstrom$Error_TotalAng[i] = err_tot_ang
    db_angstrom$PBLAng[i] = pbl_ang
    db_angstrom$Error_PBLAng[i] = err_pbl_ang
    db_angstrom$FTAng[i] = ft_ang
    db_angstrom$Error_FTAng[i] = err_ft_ang
  }
  print(i)
}

db_angstrom[is.na(db_angstrom)] = NA
db_angstrom$TotalAng[db_angstrom$TotalAng == Inf |
                       db_angstrom$TotalAng == -Inf] = NA
db_angstrom$Error_TotalAng[db_angstrom$Error_TotalAng == Inf |
                             db_angstrom$Error_TotalAng == -Inf] = NA
db_angstrom$PBLAng[db_angstrom$PBLAng == Inf |
                     db_angstrom$PBLAng == -Inf] = NA
db_angstrom$Error_PBLAng[db_angstrom$Error_PBLAng == Inf |
                           db_angstrom$Error_PBLAng == -Inf] = NA
db_angstrom$FTAng[db_angstrom$FTAng == Inf |
                    db_angstrom$FTAng == -Inf] = NA
db_angstrom$Error_FTAng[db_angstrom$Error_FTAng == Inf |
                          db_angstrom$Error_FTAng == -Inf] = NA

save(db_angstrom, file = "Lev3angstrom.Rda")
