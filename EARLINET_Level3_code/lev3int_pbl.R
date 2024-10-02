load("lev2db.Rda")
load("lev2db_layers.Rda")
k = nrow(lev2db)

db_pbl = data.frame(matrix(NA, nrow = k, ncol = 7))
db_pbl[, 1:6] = lev2db[, c(2, 5:9)]
colnames(db_pbl) = c(colnames(lev2db)[c(2, 5:9)], "PBL")

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
  pbl = NA
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
  db_pbl[i, 7] = pbl
  print(i)
  nc_close(file1)
}

save(db_pbl, file = "Lev3pbl.Rda")
clnms = colnames(db_pbl)
db_pbl = aggregate(
  db_pbl$PBL,
  by = list(
    db_pbl$Station,
    db_pbl$Year,
    db_pbl$Month,
    db_pbl$Day,
    db_pbl$Hour,
    db_pbl$Minutes
  ),
  FUN = mean,
  na.rm = T
)
colnames(db_pbl) = clnms
db_pbl = db_pbl[order(db_pbl$Station,
                      db_pbl$Year,
                      db_pbl$Month,
                      db_pbl$Day,
                      db_pbl$Hour,
                      db_pbl$Minutes), ]
rownames(db_pbl) = 1:nrow(db_pbl)
save(db_pbl, file = "Lev3pbl.Rda")

