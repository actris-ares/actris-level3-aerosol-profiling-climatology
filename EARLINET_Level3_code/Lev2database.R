f_aby = list.files("./New/aby")
f_arr = list.files("./New/arr")
f_atz = list.files("./New/atz")
f_brc = list.files("./New/brc")
f_cbw = list.files("./New/cbw")
f_cog = list.files("./New/cog")
f_dus = list.files("./New/dus")
f_evo = list.files("./New/evo")
f_gar = list.files("./New/gar")
f_gra = list.files("./New/gra")
f_hbu = list.files("./New/hbu")
f_hpb = list.files("./New/hpb")
f_ino = list.files("./New/ino")
f_ipr = list.files("./New/ipr")
f_kuh = list.files("./New/kuh")
f_lei = list.files("./New/lei")
f_lkp = list.files("./New/lkp")
f_lle = list.files("./New/lle")
f_mas = list.files("./New/mas")
f_mdr = list.files("./New/mdr")
f_mel = list.files("./New/mel")
f_muc = list.files("./New/muc")
f_nap = list.files("./New/nap")
f_pot = list.files("./New/pot")
f_puy = list.files("./New/puy")
f_rme = list.files("./New/rme")
f_sal = list.files("./New/sal")
f_sir = list.files("./New/sir")
f_sof = list.files("./New/sof")
f_spl = list.files("./New/spl")
f_the = list.files("./New/the")
f_ucc = list.files("./New/ucc")
f_waw = list.files("./New/waw")

sel_files = c(
  f_aby,
  f_arr,
  f_atz,
  f_brc,
  f_cbw,
  f_cog,
  f_dus,
  f_evo,
  f_gar,
  f_gra,
  f_hbu,
  f_hpb,
  f_ino,
  f_ipr,
  f_kuh,
  f_lei,
  f_lkp,
  f_lle,
  f_mas,
  f_mdr,
  f_mel,
  f_muc,
  f_nap,
  f_pot,
  f_puy,
  f_rme,
  f_sal,
  f_sir,
  f_sof,
  f_spl,
  f_the,
  f_ucc,
  f_waw
)

k = length(sel_files)

lev2db = data.frame(matrix(NA, nrow = k, ncol = 10))
colnames(lev2db) = c(
  "File_name",
  "Station",
  "Type",
  "Wavelength",
  "Year",
  "Month",
  "Day",
  "Hour",
  "Minutes",
  "VarBool"
)
lev2db$File_name = sel_files
lev2db$Station = substr(sel_files, 20, 22)
lev2db$Type = substr(sel_files, 30, 30)
lev2db$Wavelength = substr(sel_files, 31, 34)
lev2db$Year = substr(sel_files, 36, 39)
lev2db$Month = substr(sel_files, 40, 41)
lev2db$Day = substr(sel_files, 42, 43)
lev2db$Hour = substr(sel_files, 44, 45)
lev2db$Minutes = substr(sel_files, 46, 47)

for (i in 1:k)
{
  ct = 0
  if (lev2db$Type[i] == "e")
  {
    d_b = lev2db[lev2db$Station == lev2db$Station[i] &
                   lev2db$Type == "b" &
                   lev2db$Wavelength == lev2db$Wavelength[i] &
                   lev2db$Year == lev2db$Year[i] &
                   lev2db$Month == lev2db$Month[i] &
                   lev2db$Day == lev2db$Day[i] &
                   lev2db$Hour == lev2db$Hour[i] & lev2db$Minutes == lev2db$Minutes[i], ]
    if (nrow(d_b) > 0)
    {
      ct = ct + 1
    }
  }
  lev2db[i, 10] = ct
}

save(lev2db, file = "lev2db.Rda")

load("lev2db.Rda")
k = nrow(lev2db)
lev2db$PartDepFlag = rep(NA, times = k)
for (i in 1:k)
{
  file1 = nc_open(paste0(
    "./New/",
    substr(lev2db$File_name[i], 20, 22),
    "/",
    lev2db$File_name[i]
  ))
  pd_flag = 1
  pd = try(ncvar_get(file1, "particledepolarization"), silent = TRUE)
  if (class(pd) == "try-error")
  {
    pd_flag = 0
  }
  lev2db$PartDepFlag[i] = pd_flag
  print(i)
  nc_close(file1)
}

lev2db$PartDepFlag2 = rep(NA, times = k)

for (i in 1:k)
{
  ct = 0
  if (lev2db$Type[i] == "e" & lev2db$PartDepFlag[i] == 1)
  {
    d_b = lev2db[lev2db$Station == lev2db$Station[i] &
                   lev2db$Type == "b" &
                   lev2db$Wavelength == lev2db$Wavelength[i] &
                   lev2db$Year == lev2db$Year[i] &
                   lev2db$Month == lev2db$Month[i] &
                   lev2db$Day == lev2db$Day[i] &
                   lev2db$Hour == lev2db$Hour[i] & lev2db$Minutes == lev2db$Minutes[i], ]
    if (nrow(d_b) > 0)
    {
      ct = ct + 1
    }
  }
  lev2db[i, 12] = ct
}

lev2db$PdF = rep(0, times = k)
for (i in 1:k)
{
  if (lev2db$Type[i] == "e" &
      lev2db$PartDepFlag[i] == 1 & lev2db$PartDepFlag2[i] == 0)
  {
    lev2db$PdF[i] = 1
  }
}

lev2db = lev2db[, -c(11, 12)]
lev2db = lev2db[lev2db$Wavelength %in% c("0355", "0532", "1064"), ]

save(lev2db, file = "lev2db.Rda")

load("lev2db.Rda")
k = nrow(lev2db)
climatol = read.table("Climatol2.log")[-c(1:2, 35048:35049), 1]
calipso = read.table("Calipso2.log")[-c(1:2, 8941:8942), 1]

all_files = union(climatol, calipso)
all_files = sort(all_files)

vtt = NULL

for (i in 1:k)
{
  if (lev2db$File_name[i] %in% all_files)
  {
    vtt = c(vtt, i)
  }
}

lev2db = lev2db[vtt, ]
save(lev2db, file = "lev2db.Rda")

load("lev2db.Rda")
