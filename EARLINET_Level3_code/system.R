load("lev2db.Rda")
loc2 = unique(lev2db$Station)
syst = matrix("", nrow = 33, ncol = 20)
for (i in 1:33)
{
  for (j in 1:20)
  {
    db1 = lev2db[lev2db$Station == loc2[i] & lev2db$Year == j + 1999, ]
    s = NULL
    if (nrow(db1) > 0)
    {
      for (k in 1:nrow(db1))
      {
        file0 = nc_open(paste0("./New/", loc2[i], "/", db1[k, 1]))
        s = c(s, ncatt_get(file0, 0, attname = "system")$value) 
        nc_close(file0)
      }
      s = unique(s)
      syst[i, j] = s[1]
      if (length(s) > 1)
      {
        for (h in 2:length(s))
        {
          syst[i, j] = paste0(syst[i, j], " ; ", s[h])
        }
      }
    }
  }
}
syst_df = data.frame(syst)
rownames(syst_df) = loc2
colnames(syst_df) = as.character(seq(from = 2000, to = 2019, by = 1))

save(syst_df, file = "System.Rda")
load("System.Rda")
