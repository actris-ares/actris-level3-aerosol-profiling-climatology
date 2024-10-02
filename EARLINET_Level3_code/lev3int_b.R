load("lev2db.Rda")
load("lev2db_layers.Rda")
lev2db_b = lev2db[lev2db$VarBool == "0", ]
k = nrow(lev2db_b)

db_int_b = data.frame(matrix(NA, nrow = k, ncol = 20))
nms = c(
  colnames(lev2db)[c(1:7)],
  "H63_Bs",
  "TotalIntBs",
  "Error_TotalIntBs",
  "PBLIntBs",
  "Error_PBLIntBs",
  "FTIntBs",
  "Error_FTIntBs",
  "TotalCenterMass",
  "Error_TotalCenterMass",
  "PBLCenterMass",
  "Error_PBLCenterMass",
  "FTCenterMass",
  "Error_FTCenterMass"
)
colnames(db_int_b) = nms

for (i in 1:k)
{
  file1 = nc_open(paste0(
    "./New/",
    substr(lev2db_b$File_name[i], 20, 22),
    "/",
    lev2db_b$File_name[i]
  ))
  lydb = lev2db_layers[lev2db_layers$File_name == lev2db_b$File_name[i], ]
  lydb = lydb[!is.na(lydb$Top_Layer), ]
  alt = ncvar_get(file1, "altitude")
  l1 = which(!is.na(alt) & alt <= 12000)
  alt = alt[l1]
  m = length(alt)
  aslv = ncvar_get(file1, "station_altitude")
  bs = try(ncvar_get(file1, "backscatter"), silent = TRUE)
  err_bs = try(ncvar_get(file1, "error_backscatter"), silent = TRUE)
  if (length(class(bs)) > 1)
  {
    bs = rowMeans(bs)
    err_bs = rowMeans(err_bs)
  }
  if (class(bs) == "try-error")
  {
    bs = rep(NA, times = m)
  }
  if (class(err_bs) == "try-error")
  {
    err_bs = rep(NA, times = m)
  }
  
  bs = bs[l1]
  err_bs = err_bs[l1]
  
  l2 = which(!is.na(bs) &
               !is.na(err_bs) & err_bs > 0 & abs(bs) <= 0.0001 & bs + err_bs >= 0)
  bs = bs[l2]
  err_bs = err_bs[l2]
  alt_bs = alt[l2]
  m_bs = length(alt_bs)
  
  if (nrow(lydb) > 0)
  {
    pbl = min(lydb$Top_Layer)
  } else {
    pbl = try(ncvar_get(file1, "aerosollayerheight"), silent = TRUE)
    if (class(pbl) == "try-error")
    {
      pbl = NA
    } else {
      pbl = as.numeric(pbl)
      if (length(pbl) > 1)
      {
        pbl = mean(pbl, na.rm = T)
      }
    }
  }
  db_int_b[i, 1:7] = lev2db_b[i, 1:7]
  
  tot_int_bs = NA
  h63 = NA
  pbl_int_bs = NA
  ft_int_bs = NA
  tot_int_bs_err = NA
  pbl_int_bs_err = NA
  ft_int_bs_err = NA
  tot_com = NA
  tot_com_err = NA
  pbl_com = NA
  pbl_com_err = NA
  ft_com = NA
  ft_com_err = NA
  
  if (m_bs > 0)
  {
    prod = alt_bs * bs
    prod_err = alt_bs * err_bs
    int_bs = (alt_bs[1] - aslv) * bs[1]
    num_com = (alt_bs[1] - aslv) * prod[1]
    int_bs_err = (alt_bs[1] - aslv) * err_bs[1]
    num_com_err = (alt_bs[1] - aslv) * prod_err[1]
    p_int_bs = numeric(m_bs)
    p_int_bs[1] = int_bs
    p_int_bs_err = numeric(m_bs)
    p_int_bs_err = int_bs_err
    p_num_com = numeric(m_bs)
    p_num_com[1] = num_com
    p_num_com_err = numeric(m_bs)
    p_num_com_err[1] = num_com_err
    if (m_bs > 1)
    {
      for (g in 2:m_bs)
      {
        int_bs = (bs[g] + bs[g - 1]) * (alt_bs[g] - alt_bs[g - 1]) / 2
        num_com = (prod[g] + prod[g - 1]) * (alt_bs[g] - alt_bs[g - 1]) / 2
        int_bs_err = (err_bs[g] + err_bs[g - 1]) * (alt_bs[g] - alt_bs[g - 1]) /
          2
        num_com_err = (prod_err[g] + prod_err[g - 1]) * (alt_bs[g] - alt_bs[g -
                                                                              1]) / 2
        p_int_bs[g] = p_int_bs[g - 1] + int_bs
        p_num_com[g] = p_num_com[g - 1] + num_com
        p_int_bs_err[g] = p_int_bs_err[g - 1] + int_bs_err
        p_num_com_err[g] = p_num_com_err[g - 1] + num_com_err
      }
    }
    tot_int_bs = p_int_bs[m_bs]
    tot_com = p_num_com[m_bs] / p_int_bs[m_bs]
    tot_int_bs_err = p_int_bs_err[m_bs]
    tot_com_err = abs(tot_com) * ((p_num_com_err[m_bs] / p_num_com[m_bs]) +
                                    (tot_int_bs_err / tot_int_bs))
    ind63 = which(abs(p_int_bs) > 0.63 * abs(tot_int_bs))
    if (length(ind63) > 0)
    {
      h63 = alt_bs[min(ind63)]
    }
    if (!is.na(pbl))
    {
      ind_pbl_bs = which(alt_bs <= pbl)
      n_bs = length(ind_pbl_bs)
      if (n_bs == 0)
      {
        pbl_int_bs = NA
        pbl_int_bs_err = NA
        ft_int_bs = tot_int_bs
        ft_int_bs_err = tot_int_bs_err
        pbl_com = NA
        pbl_com_err = NA
        ft_com = tot_com
        ft_com_err = tot_com_err
      }
      else
      {
        pbl_int_bs = p_int_bs[n_bs]
        ft_int_bs = tot_int_bs - pbl_int_bs
        pbl_int_bs_err = p_int_bs_err[n_bs]
        ft_int_bs_err = tot_int_bs_err - pbl_int_bs_err
        pbl_com = p_num_com[n_bs] / p_int_bs[n_bs]
        pbl_com_err = pbl_com * ((p_num_com_err[n_bs] / p_num_com[n_bs]) + (p_int_bs_err[n_bs] /
                                                                              p_int_bs[n_bs]))
        ft_com = tot_com - pbl_com
        ft_com_err = tot_com_err - pbl_com_err
      }
    }
  }
  
  db_int_b[i, 8] = h63
  db_int_b[i, 9] = tot_int_bs
  db_int_b[i, 10] = tot_int_bs_err
  db_int_b[i, 11] = pbl_int_bs
  db_int_b[i, 12] = pbl_int_bs_err
  db_int_b[i, 13] = ft_int_bs
  db_int_b[i, 14] = ft_int_bs_err
  db_int_b[i, 15] = tot_com
  db_int_b[i, 16] = tot_com_err
  db_int_b[i, 17] = pbl_com
  db_int_b[i, 18] = pbl_com_err
  db_int_b[i, 19] = ft_com
  db_int_b[i, 20] = ft_com_err
  print(i)
  nc_close(file1)
}

db_int_b[is.na(db_int_b)] = NA
save(db_int_b, file = "Lev3Int_b.Rda")


load("lev2db.Rda")
load("lev2db_layers.Rda")
k = nrow(lev2db)

db_pd = data.frame(matrix(NA, nrow = k, ncol = 15))
nms = c(
  colnames(lev2db)[c(1:9)],
  "TotalPartDep",
  "Error_TotalPartDep",
  "PBLPartDep",
  "Error_PBLPartDep",
  "FTPartDep",
  "Error_FTPartDep"
)
colnames(db_pd) = nms

for (i in 1:k)
{
  file1 = nc_open(paste0(
    "./New/",
    substr(lev2db$File_name[i], 20, 22),
    "/",
    lev2db$File_name[i]
  ))
  lydb = lev2db_layers[lev2db_layers$File_name == lev2db$File_name[i], ]
  lydb = lydb[!is.na(lydb$Top_Layer), ]
  alt = ncvar_get(file1, "altitude")
  l1 = which(!is.na(alt) & alt <= 12000)
  alt = alt[l1]
  m = length(alt)
  pd = try(ncvar_get(file1, "particledepolarization"), silent = TRUE)
  if (class(pd) == "try-error")
  {
    pd = rep(NA, times = m)
  }
  err_pd = try(ncvar_get(file1, "error_particledepolarization"), silent =
                 TRUE)
  if (class(err_pd) == "try-error")
  {
    err_pd = rep(NA, times = m)
  }
  pd = pd[l1]
  err_pd = err_pd[l1]
  l4 = which(!is.na(pd) &
               !is.na(err_pd) & err_pd > 0 & pd + err_pd >= 0 & pd - err_pd <= 1)
  pd = pd[l4]
  err_pd = err_pd[l4]
  alt_pd = alt[l4]
  m_pd = length(alt_pd)
  if (nrow(lydb) > 0)
  {
    pbl = min(lydb$Top_Layer)
  } else {
    pbl = try(ncvar_get(file1, "aerosollayerheight"), silent = TRUE)
    if (class(pbl) == "try-error")
    {
      pbl = NA
    } else {
      pbl = as.numeric(pbl)
      if (length(pbl) > 1)
      {
        pbl = mean(pbl, na.rm = T)
      }
    }
  }
  tot_pd = NA
  tot_pd_err = NA
  pbl_pd = NA
  pbl_pd_err = NA
  ft_pd = NA
  ft_pd_err = NA
  if (m_pd > 0)
  {
    tot_pd = mean(pd, na.rm = T)
    tot_pd_err = mean(err_pd, na.rm = T)
    if (!is.na(pbl))
    {
      ind_pbl_pd = which(alt_pd <= pbl)
      n_pd = length(ind_pbl_pd)
      if (n_pd == 0)
      {
        pbl_pd = NA
        pbl_pd_err = NA
        ft_pd = tot_pd
        ft_pd_err = tot_pd_err
      }
      else
      {
        pbl_pd = mean(pd[ind_pbl_pd], na.rm = T)
        pbl_pd_err = mean(err_pd[ind_pbl_pd], na.rm = T)
        ft_pd = mean(pd[-ind_pbl_pd], na.rm = T)
        ft_pd_err = mean(err_pd[-ind_pbl_pd], na.rm = T)
      }
    }
  }
  db_pd[i, 1:9] = lev2db[i, 1:9]
  db_pd[i, 10] = tot_pd
  db_pd[i, 11] = tot_pd_err
  db_pd[i, 12] = pbl_pd
  db_pd[i, 13] = pbl_pd_err
  db_pd[i, 14] = ft_pd
  db_pd[i, 15] = ft_pd_err
  print(i)
  nc_close(file1)
}

db_pd[is.na(db_pd)] = NA
save(db_pd, file = "Lev3PartDep.Rda")
db_pd = aggregate(
  db_pd[10:15],
  by = list(
    db_pd$Station,
    db_pd$Wavelength,
    db_pd$Year,
    db_pd$Month,
    db_pd$Day,
    db_pd$Hour,
    db_pd$Minutes
  ),
  FUN = mean,
  na.rm = T
)
db_pd[is.na(db_pd)] = NA
colnames(db_pd)[1:7] = colnames(lev2db)[c(2, 4:9)]
db_pd = db_pd[order(db_pd$Station,
                    db_pd$Year,
                    db_pd$Month,
                    db_pd$Day,
                    db_pd$Hour,
                    db_pd$Minutes), ]
rownames(db_pd) = 1:nrow(db_pd)
save(db_pd, file = "Lev3PartDep.Rda")
