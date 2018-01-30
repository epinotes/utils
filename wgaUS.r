wgaUS <- function(ga,bwt,sex){
  # ga: gestational age
  # bwt: birthweight
  # sex: gender
  # weight reference as a data.table
  #source: Olsen IE, Groveman SA, Lawson ML, Clark RH, Zemel BS. New Intrauterine Growth Curves Based on United States Data. Pediatrics. 2010;125(2):e214–e224. doi:10.1542/peds.2009-0913.

require(data.table)
us.refdt = data.table(sex=rep(c("F","M"),each=19),ga=rep(c(23:41),times=2),
p10=c(477, 509, 524, 561, 584, 626, 645, 704, 719, 789, 807, 884, 915,
 988, 1052, 1114, 1196, 1267, 1352, 1433, 1545, 1625, 1730, 1810,
 1869, 1980, 2028, 2170, 2260, 2401, 2526, 2652, 2724, 2833, 2855,
 2933, 2950, 3039),
  p90=c(687, 727, 772, 813, 885, 926, 1004, 1065, 1147, 1218, 1310, 1385,
 1489, 1560, 1693, 1761, 1897, 1984, 2116, 2218, 2379, 2488, 2661,
 2763, 2985, 3084, 3339, 3432, 3651, 3736, 3847, 3973, 3986, 4070,
 4129, 4142, 4232, 4319),
id=c("F23", "F24", "F25", "F26", "F27", "F28", "F29", "F30", "F31", "F32", "F33",
 "F34", "F35", "F36", "F37", "F38", "F39", "F40", "F41", "M23", "M24", "M25",
 "M26", "M27", "M28", "M29", "M30", "M31", "M32", "M33", "M34", "M35", "M36",
 "M37", "M38", "M39", "M40", "M41"))

setkey(us.refdt,id)

id2=paste0(sex,ga) # similar to "id" of the reference table
idx=us.refdt[id2] # selected row from the reference table
idx2=idx[,p10]  # p10 of the selected row
idx3=idx[,p90]  # p90 of the selected row

  # classification for SGA, AGA, LGA
wga=ifelse(is.na(idx2),NA, ifelse(bwt<idx2,1,ifelse(bwt>=idx3,2,0)))

as.factor(wga)

}

#wgaUS(ga=c(30,30,30),bwt=c(700,1064,1065),sex=c("F","F","F"))

#wgaUS(ref=us.ref,ga=c(30,30,30),bwt=c(900,1500,2000),sex=c("F","F","F"))

