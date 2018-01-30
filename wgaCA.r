wgaCA <- function(ga,bwt,sex){
  # ga: gestational age
  # bwt: birthweight
  # sex: gender
  # weight reference as a data.table
  #Kramer MS, Platt RW, Wen SW, et al. A new and improved population-based Canadian reference for birth weight for gestational age. Pediatrics. 2001;108(2):E35.


require(data.table)
ca.refdt = data.table(sex=rep(c("F","M"),each=22),ga=rep(c(22:43),times=2),
p10=c(385, 450, 513, 578, 645, 717, 802, 903, 1022, 1168, 1346, 1548,
 1768, 1998, 2227, 2452, 2658, 2825, 2955, 3051, 3114, 3159,401, 475, 547, 617, 686, 763, 853, 964, 1099, 1259, 1444, 1648,1866, 2091, 2321, 2552, 2766, 2942, 3079, 3179, 3233, 3249),
  p90=c(552, 669, 790, 918, 1060, 1218, 1390, 1578, 1783, 2004, 2242, 2494,
 2761, 3037, 3307, 3543, 3738, 3895, 4034, 4154, 4251, 4333,587, 714, 844, 981, 1125, 1278, 1445, 1629, 1837, 2069, 2319, 2580,2851, 3132, 3411, 3665, 3877, 4049, 4200, 4328, 4433, 4528),
id=c("F22","F23", "F24", "F25", "F26", "F27", "F28", "F29", "F30", "F31", "F32", "F33","F34", "F35", "F36", "F37", "F38", "F39", "F40", "F41","F42","F43","M22", "M23", "M24", "M25", "M26", "M27", "M28", "M29", "M30", "M31", "M32", "M33", "M34", "M35", "M36","M37", "M38", "M39", "M40", "M41","M42", "M43"))

setkey(ca.refdt,id)

id2=paste0(sex,ga) # similar to "id" of the reference table
idx=ca.refdt[id2] # selected row from the reference table
idx2=idx[,p10]  # p10 of the selected row
idx3=idx[,p90]  # p90 of the selected row

  # classification for SGA, AGA, LGA
wga=ifelse(is.na(idx2),NA, ifelse(bwt < idx2,1,ifelse(bwt >= idx3,2,0)))

as.factor(wga)
}

# use example # wgaCA(ga=c(30,30,30),bwt=c(700,1064,1782),sex=c("F","M","F"))
