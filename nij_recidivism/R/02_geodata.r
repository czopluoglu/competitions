################################################################################

# The code in this file downloads data from 2018 ACS 5-year estimate
# recode each variable accordingly depending on its nature,
# and then aggregate household level data at the PUMA level. 
# In addition, it runs a PCA to create principal components from these variables.

# At the end, it produces a summary dataset 72 x 300 (puma_summary.csv).
# Each row represents a PUMA, and each column represents an aggregated variable
# across households within that PUMA.

# For detailed codebook about each variable, see 2014-2018 ACS PUMS Data Dictionary

# https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2014-2018.pdf?

################################################################################


require(tidycensus)


pumas <- c(1003,4400,1008, 4300,1200, 1300, 1400, 1500, 1600,1700, 1800,2001, 
           2002, 2003, 4005,100, 200, 500,4000, 4100, 4200,5001, 6001, 6002,
           2400, 5002, 1001, 3004, 4600, 1002, 1005, 3300, 3400, 4001, 4002, 
           4006, 3101, 3102, 1900, 3900, 4003, 4004, 3001, 3002, 3003, 3005,
           2500, 4500,2800, 2900, 3200, 3500,600, 700, 800,900, 1100,300, 
           401, 402,1004, 2100,2200, 2300,1006, 1007, 2004,2600, 2700,3600, 
           3700, 3800)

geodata <- get_pums(variables = 
                      c("AGEP",'FAGEP',"PUMA",'NP','ACCESS','FACCESSP','ACR','FACRP',
                        'AGS','FAGSP','BATH','FBATHP','BLD', 'FBLDP','FBDSP',
                        'BUS','FBUSP','BROADBND','FBROADBNDP','COMPOTHX',
                        'FCOMPOTHXP','CONP','FCONP','DIALUP','FDIALUPP',
                        'ELEFP','FELEP','ELEP','FS','FFSP','FULFP','FULP',
                        'FFULP','GASFP','GASP','FGASP','HFL', 'FHFLP',
                        'HISPEED','FHISPEEDP','HOTWAT','FHOTWATP','INSP','FINSP',
                        'LAPTOP','FLAPTOPP','MHP','FMHP','MRGI','FMRGIP',
                        'MRGP','FMRGP','MRGT','FMRGTP','MRGX','FMRGXP',
                        'OTHSVCEX','FOTHSVCEXP','REFR','FREFRP','RMSP','FRMSP',
                        'RNTM','FRNTMP','RWAT','FRWATP','RWATPR','SATELLITE',
                        'FSATELLITEP','SINK','FSINKP','SMARTPHONE','FSMARTPHONP',
                        'SMP','FSMP','FSMOCP','STOV','FSTOVP','TABLET','FTABLETP',
                        'TEL','FTELP','TEN','FTENP','TOIL','FTOILP','VALP','FVALP',
                        'VEH','FVEHP','WATFP','FWATP','WATP','YBL','FYBLP','FES',
                        'FINCP','FFINCP','FPARC','GRNTP','FGRNTP','GRPIP','HHL',
                        'HHT','HINCP','FHINCP','HUGCL','HUPAC','HUPAOC','HUPARC',
                        'KIT','FKITP','LNGI','MULTG','MV','FMVP','NOC','NPF',
                        'NPP','NR','NRC','OCPIP','PARTNER','PLM','FPLMP','PSF',
                        'R18','R60','R65','SMOCP','SMX','FSMXSP','SRNT','SSMC',
                        'SVAL','TAXAMT','WIF','FSMXHP','FTAXP','FVACSP','CIT',
                        'FCITP','COW','FCOWP','DDRS','FDDRSP','DEAR','FDEARP',
                        'DEYE','FDEYEP','DOUT','FDOUTP','DPHY','FDPHYP',
                        'DRAT', 'FDRATP','DRATX','FDRATXP','DREM','FDREMP',
                        'ENG','FENGP','FER','FFERP','GCL','FGCLP','GCM','FGCMP',
                        'GCR','FGCRP','HINS1','FHINS1P','HINS2','FHINS2P','HINS3',
                        'FHINS3P','HINS4','FHINS4P','HINS5','FHINS5P','HINS6',
                        'FHINS6P','HINS7','FHINS7P','INTP','FINTP','JWMNP','JWRIP',
                        'FJWRIP','JWTR','FJWTRP','LANX','FLANXP','MAR','FMARP',
                        'MARHD','FMARHDP','MARHM', 'FMARHMP','MARHT','FMARHTP',
                        'MARHW','FMARHWP','MIG','FMIGP','MIL','FMILPP','MLPA',
                        'MLPB','MLPCD','MLPE','MLPFG','MLPH','MLPI','MLPJ','MLPK',
                        'NWAB','NWAV','NWLA','NWLK','NWRE','OIP','FOIP','PAP', 'FPAP',
                        'RETP','FRETP','SCH','SEMP', 'FSEMP','SSIP','FSSIP','SSP','FSSP',
                        'WAGP','FWAGP','WKHP','FWKHP','WKL','FWKLP','WKW','FWKWP',
                        'WRK','FWRKP','DIS','MSP','NATIVITY','OC','PAOC','PERNP',
                        'PINCP','POVPIP','PRIVCOV','FPRIVCOVP','PUBCOV','FPUBCOVP',
                        'RACAIAN','RACASN','RACBLK','RACNH','RACNUM','RACPI','RACWHT',
                        'RC','SCIENGP','SCIENGRLP','FESRP','FHICOVP','FLANP',
                        'FMARHYP','FMIGSP','FMILSP','FPERNP','FPINCP'), 
              state = "GA",
              year = 2018)

################################################################################
load("B:/Ongoing_Research/nij/nij/data/supplemental data/geodata.RData")

colnames(geodata) <- tolower(colnames(geodata))

require(psych)
require(dplyr)
################################################################################
################################################################################

str(geodata$agep)

describe(geodata$agep)


#########################
str(geodata$np)

describe(geodata$np)

#########################
str(geodata$access)
table(geodata$access)
names(table(geodata$access))


geodata$access<- recode(.x = geodata$access,
                         '1'  = 1,
                         '2'  = 1,
                         '3'  = 0)
 
#########################
str(geodata$acr)
table(geodata$acr)
names(table(geodata$acr))

geodata[which(geodata$acr=='b'),]$acr=NA
geodata$acr <- as.numeric(geodata$acr)

geodata$acr1 <- ifelse(geodata$acr == 1, 1, 0)
geodata$acr2 <- ifelse(geodata$acr == 2, 1, 0)
geodata$acr3 <- ifelse(geodata$acr == 3, 1, 0)


##########################

str(geodata$ags)
table(geodata$ags)
names(table(geodata$ags))

geodata[which(geodata$ags=='b'),]$ags=NA
geodata$ags <- as.numeric(geodata$ags)

geodata$ags1 <- ifelse(geodata$ags == 1, 1, 0)
geodata$ags2 <- ifelse(geodata$ags == 2, 1, 0)
geodata$ags3 <- ifelse(geodata$ags == 3, 1, 0)
geodata$ags4 <- ifelse(geodata$ags == 4, 1, 0)
geodata$ags5 <- ifelse(geodata$ags == 5, 1, 0)
geodata$ags6 <- ifelse(geodata$ags == 6, 1, 0)

##########################

str(geodata$bath)
table(geodata$bath)

geodata$bath <- ifelse(geodata$bath == '1', 1,
                       ifelse(geodata$bath=='2',0,NA))

##########################

str(geodata$bld)
table(geodata$bld)

geodata[which(geodata$bld=='bb'),]$bld=NA

geodata$bld1  <- ifelse(geodata$bld == '01', 1, 0)
geodata$bld2  <- ifelse(geodata$bld == '02', 1, 0)
geodata$bld3  <- ifelse(geodata$bld == '03', 1, 0)
geodata$bld4  <- ifelse(geodata$bld == '04', 1, 0)
geodata$bld5  <- ifelse(geodata$bld == '05', 1, 0)
geodata$bld6  <- ifelse(geodata$bld == '06', 1, 0)
geodata$bld7  <- ifelse(geodata$bld == '07', 1, 0)
geodata$bld8  <- ifelse(geodata$bld == '08', 1, 0)
geodata$bld9  <- ifelse(geodata$bld == '09', 1, 0)
geodata$bld10 <- ifelse(geodata$bld == '10', 1, 0)

##########################

str(geodata$bus)
table(geodata$bus)

geodata[which(geodata$bus=='b'),]$bus=NA
geodata[which(geodata$bus=='9'),]$bus=NA

geodata$bus <- ifelse(geodata$bus == '1', 1,
                       ifelse(geodata$bus=='2',0,NA))

##########################

str(geodata$broadbnd)
table(geodata$broadbnd)

geodata[which(geodata$broadbnd=='b'),]$broadbnd=NA

geodata$broadbnd <- ifelse(geodata$broadbnd == '1', 1,
                      ifelse(geodata$broadbnd=='2',0,NA))

##########################

str(geodata$compothx)
table(geodata$compothx)

geodata$compothx <- ifelse(geodata$compothx == '1', 1,
                           ifelse(geodata$compothx =='2',0,NA))

##########################

str(geodata$conp)
table(geodata$conp)

geodata[which(geodata$conp==-1),]$conp=NA
geodata$conp <- log(geodata$conp + 1)

##########################

str(geodata$dialup)
table(geodata$dialup)

geodata[which(geodata$dialup=='b'),]$dialup=NA

geodata$dialup <- ifelse(geodata$dialup == '1', 1,
                           ifelse(geodata$dialup=='2',0,NA))

##########################

str(geodata$elefp)
table(geodata$elefp)

geodata$elefp1  <- ifelse(geodata$elefp == '1', 1, 0)
geodata$elefp2  <- ifelse(geodata$elefp == '2', 1, 0)
geodata$elefp3  <- ifelse(geodata$elefp == '3', 1, 0)

##########################

str(geodata$elep)
table(geodata$elep)
describe(geodata$elep)
##########################

str(geodata$fs)
table(geodata$fs)

geodata$fs <- ifelse(geodata$fs == '1', 1,
                         ifelse(geodata$fs=='2',0,NA))


##########################

str(geodata$fulp)
table(geodata$fulp)
describe(geodata$fulp)

geodata$fulp <- log(geodata$fulp)

##########################

str(geodata$gasp)
table(geodata$gasp)
describe(geodata$gasp)

geodata$gasp <- log(geodata$gasp)

##########################

str(geodata$hfl)
table(geodata$hfl)

geodata[which(geodata$hfl==0),]$hfl=NA

geodata$hfl1  <- ifelse(geodata$hfl == 1, 1, 0)
geodata$hfl2  <- ifelse(geodata$hfl == 2, 1, 0)
geodata$hfl3  <- ifelse(geodata$hfl == 3, 1, 0)
geodata$hfl4  <- ifelse(geodata$hfl == 4, 1, 0)
geodata$hfl5  <- ifelse(geodata$hfl == 5, 1, 0)
geodata$hfl6  <- ifelse(geodata$hfl == 6, 1, 0)
geodata$hfl7  <- ifelse(geodata$hfl == 7, 1, 0)
geodata$hfl8  <- ifelse(geodata$hfl == 8, 1, 0)
geodata$hfl9  <- ifelse(geodata$hfl == 9, 1, 0)

############################


str(geodata$hispeed)
table(geodata$hispeed)

geodata[which(geodata$hispeed=='b'),]$hispeed=NA

geodata$hispeed <- ifelse(geodata$hispeed == '1', 1,
                         ifelse(geodata$hispeed=='2',0,NA))

##############################

str(geodata$insp)
table(geodata$insp)

geodata[which(geodata$insp==-1),]$insp=NA
geodata[which(geodata$insp==0),]$insp=NA

describe(geodata$insp)

##############################

str(geodata$laptop)
table(geodata$laptop)

geodata[which(geodata$laptop=='b'),]$laptop=NA

geodata$laptop <- ifelse(geodata$laptop == '1', 1,
                          ifelse(geodata$laptop=='2',0,NA))

##############################

str(geodata$mhp)
table(geodata$mhp)


geodata[which(geodata$mhp==-1),]$mhp=NA

describe(geodata$mhp)

##############################

str(geodata$mrgi)
table(geodata$mrgi)

geodata[which(geodata$mrgi=='b'),]$mrgi=NA

geodata$mrgi <- ifelse(geodata$mrgi == '1', 1,
                         ifelse(geodata$mrgi=='2',0,NA))

##############################

str(geodata$mrgp)
table(geodata$mrgp)

geodata[which(geodata$mrgp==0),]$mrgp=NA

describe(geodata$mrgp)

##############################

str(geodata$mrgt)
table(geodata$mrgt)

geodata[which(geodata$mrgt=='b'),]$mrgt=NA

geodata$mrgt <- ifelse(geodata$mrgt == '1', 1,
                       ifelse(geodata$mrgt=='2',0,NA))

##############################

str(geodata$mrgx)
table(geodata$mrgx)

geodata[which(geodata$mrgx=='b'),]$mrgx=NA

geodata$mrgx1  <- ifelse(geodata$mrgx == '1', 1, 0)
geodata$mrgx2  <- ifelse(geodata$mrgx == '2', 1, 0)
geodata$mrgx3  <- ifelse(geodata$mrgx == '3', 1, 0)


##############################

str(geodata$othsvcex)
table(geodata$othsvcex)

geodata[which(geodata$othsvcex=='b'),]$othsvcex = NA

geodata$othsvcex <- ifelse(geodata$othsvcex == '1', 1,
                       ifelse(geodata$othsvcex=='2',0,NA))


##############################

str(geodata$refr)
table(geodata$refr)

geodata[which(geodata$refr=='b'),]$refr = NA

geodata$refr <- ifelse(geodata$refr == '1', 1,
                           ifelse(geodata$refr=='2',0,NA))

##############################

str(geodata$rmsp)
table(geodata$rmsp)

geodata[which(geodata$rmsp==-1),]$rmsp = NA

describe(geodata$rmsp)


##############################

str(geodata$rntm)
table(geodata$rntm)

geodata[which(geodata$rntm=='b'),]$rntm = NA

geodata$rntm <- ifelse(geodata$rntm == '1', 1,
                       ifelse(geodata$rntm=='2',0,NA))

##############################

str(geodata$rwat)
table(geodata$rwat)

geodata[which(geodata$rwat=='b'),]$rwat = NA

geodata$rwat <- ifelse(geodata$rwat == '1', 1,
                       ifelse(geodata$rwat=='2',0,NA))


##############################

str(geodata$satellite)
table(geodata$satellite)

geodata[which(geodata$satellite=='b'),]$satellite = NA

geodata$satellite <- ifelse(geodata$satellite == '1', 1,
                       ifelse(geodata$satellite=='2',0,NA))


##############################

str(geodata$sink)
table(geodata$sink)

geodata[which(geodata$sink=='b'),]$sink = NA

geodata$sink <- ifelse(geodata$sink == '1', 1,
                            ifelse(geodata$sink=='2',0,NA))


##############################

str(geodata$smartphone)
table(geodata$smartphone)

geodata[which(geodata$smartphone=='b'),]$smartphone = NA

geodata$smartphone <- ifelse(geodata$smartphone == '1', 1,
                       ifelse(geodata$smartphone=='2',0,NA))

##############################

str(geodata$smp)
table(geodata$smp)

geodata[which(geodata$smp=='0'),]$smp = NA

describe(geodata$smp)

##############################


str(geodata$stov)
table(geodata$stov)

geodata[which(geodata$stov=='b'),]$stov = NA

geodata$stov <- ifelse(geodata$stov == '1', 1,
                       ifelse(geodata$stov=='2',0,NA))

##############################

str(geodata$tablet)
table(geodata$tablet)

geodata[which(geodata$tablet=='b'),]$tablet = NA

geodata$tablet <- ifelse(geodata$tablet == '1', 1,
                       ifelse(geodata$tablet=='2',0,NA))

##############################

str(geodata$tel)
table(geodata$tel)

geodata[which(geodata$tel=='b'),]$tel = NA

geodata$tel <- ifelse(geodata$tel == '1', 1,
                         ifelse(geodata$tel=='2',0,NA))


##############################

str(geodata$ten)
table(geodata$ten)

geodata[which(geodata$ten=='b'),]$ten = NA

geodata$ten1  <- ifelse(geodata$ten == '1', 1, 0)
geodata$ten2  <- ifelse(geodata$ten == '2', 1, 0)
geodata$ten3  <- ifelse(geodata$ten == '3', 1, 0)
geodata$ten4  <- ifelse(geodata$ten == '4', 1, 0)


##############################

str(geodata$toil)
table(geodata$toil)

geodata[which(geodata$toil=='b'),]$toil = NA
geodata[which(geodata$toil=='9'),]$toil = NA

geodata$toil <- ifelse(geodata$toil == '1', 1,
                      ifelse(geodata$toil=='2',0,NA))


##############################

str(geodata$valp)
table(geodata$valp)

geodata[which(geodata$valp==-1),]$valp = NA

describe(geodata$valp)


##############################

str(geodata$veh)
table(geodata$veh)

geodata[which(geodata$veh=='b'),]$veh = NA

geodata$veh <- as.numeric(geodata$veh)

describe(geodata$veh)

##############################

str(geodata$watfp)
table(geodata$watfp)

geodata[which(geodata$watfp=='b'),]$watfp = NA

geodata$watfp1  <- ifelse(geodata$watfp == '1', 1, 0)
geodata$watfp2  <- ifelse(geodata$watfp == '2', 1, 0)
geodata$watfp3  <- ifelse(geodata$watfp == '3', 1, 0)

##############################

str(geodata$watp)
table(geodata$watp)

geodata[which(geodata$watp=='b'),]$watp = NA

geodata$watp <- as.numeric(geodata$watp)

describe(geodata$watp)


##############################

str(geodata$ybl)
table(geodata$ybl)

geodata[which(geodata$ybl=='bb'),]$ybl = NA

geodata$ybl <- as.numeric(geodata$ybl)

geodata[which(geodata$ybl==1),]$ybl = 80
geodata[which(geodata$ybl==2),]$ybl = 73
geodata[which(geodata$ybl==3),]$ybl = 63
geodata[which(geodata$ybl==4),]$ybl = 53
geodata[which(geodata$ybl==5),]$ybl = 43
geodata[which(geodata$ybl==6),]$ybl = 33
geodata[which(geodata$ybl==7),]$ybl = 23
geodata[which(geodata$ybl==8),]$ybl = 16
geodata[which(geodata$ybl==9),]$ybl = 13
geodata[which(geodata$ybl==10),]$ybl = 12
geodata[which(geodata$ybl==11),]$ybl = 11
geodata[which(geodata$ybl==12),]$ybl = 10
geodata[which(geodata$ybl==13),]$ybl = 9
geodata[which(geodata$ybl==14),]$ybl = 8
geodata[which(geodata$ybl==15),]$ybl = 7
geodata[which(geodata$ybl==16),]$ybl = 6
geodata[which(geodata$ybl==17),]$ybl = 5
geodata[which(geodata$ybl==18),]$ybl = 4
geodata[which(geodata$ybl==19),]$ybl = 3
geodata[which(geodata$ybl==20),]$ybl = 2
geodata[which(geodata$ybl==21),]$ybl = 1
geodata[which(geodata$ybl==22),]$ybl = 0

describe(geodata$ybl)


##############################

str(geodata$fes)
table(geodata$fes)

geodata[which(geodata$fes=='b'),]$fes = NA

geodata$fes1  <- ifelse(geodata$fes == '1', 1, 0)
geodata$fes2  <- ifelse(geodata$fes == '2', 1, 0)
geodata$fes3  <- ifelse(geodata$fes == '3', 1, 0)
geodata$fes4  <- ifelse(geodata$fes == '4', 1, 0)
geodata$fes5  <- ifelse(geodata$fes == '5', 1, 0)
geodata$fes6  <- ifelse(geodata$fes == '6', 1, 0)
geodata$fes7  <- ifelse(geodata$fes == '7', 1, 0)
geodata$fes8  <- ifelse(geodata$fes == '8', 1, 0)


##############################

str(geodata$fincp)
table(geodata$fincp)

describe(geodata$fincp)


##############################

str(geodata$fparc)
table(geodata$fparc)

geodata[which(geodata$fparc=='b'),]$fparc = NA

geodata$fparc1  <- ifelse(geodata$fparc == '1', 1, 0)
geodata$fparc2  <- ifelse(geodata$fparc == '2', 1, 0)
geodata$fparc3  <- ifelse(geodata$fparc == '3', 1, 0)
geodata$fparc4  <- ifelse(geodata$fparc == '4', 1, 0)

##############################

str(geodata$grntp)
table(geodata$grntp)

geodata$grntp1  <- ifelse(geodata$grntp == '0', 1, 0)

geodata[which(geodata$grntp=='0'),]$grntp = NA

describe(geodata$grntp)

##############################

str(geodata$grpip)
table(geodata$grpip)

geodata[which(geodata$grpip=='0'),]$grpip = NA

describe(geodata$grpip)

##############################

str(geodata$hhl)
table(geodata$hhl)

geodata[which(geodata$hhl=='b'),]$hhl = NA

geodata$hhl1  <- ifelse(geodata$hhl == '1', 1, 0)
geodata$hhl2  <- ifelse(geodata$hhl == '2', 1, 0)
geodata$hhl3  <- ifelse(geodata$hhl == '3', 1, 0)
geodata$hhl4  <- ifelse(geodata$hhl == '4', 1, 0)
geodata$hhl5  <- ifelse(geodata$hhl == '5', 1, 0)

##############################

str(geodata$hht)
table(geodata$hht)

geodata[which(geodata$hht=='b'),]$hht = NA

geodata$hht1  <- ifelse(geodata$hht == '1', 1, 0)
geodata$hht2  <- ifelse(geodata$hht == '2', 1, 0)
geodata$hht3  <- ifelse(geodata$hht == '3', 1, 0)
geodata$hht4  <- ifelse(geodata$hht == '4', 1, 0)
geodata$hht5  <- ifelse(geodata$hht == '5', 1, 0)
geodata$hht6  <- ifelse(geodata$hht == '6', 1, 0)
geodata$hht7  <- ifelse(geodata$hht == '7', 1, 0)

##############################

str(geodata$hincp)
table(geodata$hincp)

describe(geodata$hincp)

##############################

str(geodata$hugcl)
table(geodata$hugcl)


geodata[which(geodata$hugcl=='b'),]$hugcl = NA

geodata$hugcl <- as.numeric(geodata$hugcl)

##############################

str(geodata$hupac)
table(geodata$hupac)

geodata[which(geodata$hupac=='b'),]$hupac = NA

geodata$hupac1  <- ifelse(geodata$hupac == '1', 1, 0)
geodata$hupac2  <- ifelse(geodata$hupac == '2', 1, 0)
geodata$hupac3  <- ifelse(geodata$hupac == '3', 1, 0)
geodata$hupac4  <- ifelse(geodata$hupac == '4', 1, 0)

##############################

str(geodata$hupaoc)
table(geodata$hupaoc)

geodata[which(geodata$hupaoc=='b'),]$hupaoc = NA

geodata$hupaoc1  <- ifelse(geodata$hupaoc == '1', 1, 0)
geodata$hupaoc2  <- ifelse(geodata$hupaoc == '2', 1, 0)
geodata$hupaoc3  <- ifelse(geodata$hupaoc == '3', 1, 0)
geodata$hupaoc4  <- ifelse(geodata$hupaoc == '4', 1, 0)

##############################

str(geodata$huparc)
table(geodata$huparc)

geodata[which(geodata$huparc=='b'),]$huparc = NA

geodata$huparc1  <- ifelse(geodata$huparc == '1', 1, 0)
geodata$huparc2  <- ifelse(geodata$huparc == '2', 1, 0)
geodata$huparc3  <- ifelse(geodata$huparc == '3', 1, 0)
geodata$huparc4  <- ifelse(geodata$huparc == '4', 1, 0)

##############################

str(geodata$kit)
table(geodata$kit)

geodata[which(geodata$kit=='b'),]$kit = NA

geodata$kit <- ifelse(geodata$kit == '1', 1,
                       ifelse(geodata$kit=='2',0,NA))

##############################

str(geodata$lngi)
table(geodata$lngi)

geodata[which(geodata$lngi=='b'),]$lngi = NA

geodata$lngi <- ifelse(geodata$lngi == '1', 1,
                      ifelse(geodata$lngi=='2',0,NA))

##############################

str(geodata$multg)
table(geodata$multg)

geodata[which(geodata$multg=='b'),]$multg = NA

geodata$multg <- ifelse(geodata$multg == '1', 1,
                       ifelse(geodata$multg=='2',0,NA))

##############################

str(geodata$mv)
table(geodata$mv)

geodata[which(geodata$mv=='b'),]$mv = NA

geodata$mv1  <- ifelse(geodata$mv == '1', 1, 0)
geodata$mv2  <- ifelse(geodata$mv == '2', 1, 0)
geodata$mv3  <- ifelse(geodata$mv == '3', 1, 0)
geodata$mv4  <- ifelse(geodata$mv == '4', 1, 0)
geodata$mv5  <- ifelse(geodata$mv == '5', 1, 0)
geodata$mv6  <- ifelse(geodata$mv == '6', 1, 0)
geodata$mv7  <- ifelse(geodata$mv == '7', 1, 0)

##############################

str(geodata$noc)
table(geodata$noc)

geodata[which(geodata$noc==-1),]$noc = NA

describe(geodata$noc)

##############################

str(geodata$npf)
table(geodata$npf)

describe(geodata$npf)

##############################

str(geodata$npp)
table(geodata$npp)


geodata[which(geodata$npp=='b'),]$npp = NA

geodata$npp <- as.numeric(geodata$npp)

##############################

str(geodata$nr)
table(geodata$nr)


geodata[which(geodata$nr=='b'),]$nr = NA

geodata$nr <- as.numeric(geodata$nr)

##############################

str(geodata$nrc)
table(geodata$nrc)


geodata[which(geodata$nrc==-1),]$nrc = NA

describe(geodata$nrc)


##############################

str(geodata$ocpip)
table(geodata$ocpip)

geodata$ocpip1 <- ifelse(geodata$ocpip==0,0,1)

geodata[which(geodata$ocpip==0),]$ocpip = NA

describe(geodata$ocpip)


##############################

str(geodata$partner)
table(geodata$partner)

geodata[which(geodata$partner=='b'),]$partner = NA

geodata$partner1  <- ifelse(geodata$partner == '0', 1, 0)
geodata$partner2  <- ifelse(geodata$partner == '1', 1, 0)
geodata$partner3  <- ifelse(geodata$partner == '2', 1, 0)
geodata$partner4  <- ifelse(geodata$partner == '3', 1, 0)
geodata$partner5  <- ifelse(geodata$partner == '4', 1, 0)

##############################

str(geodata$plm)
table(geodata$plm)

geodata[which(geodata$plm=='b'),]$plm = NA

geodata$plm <- ifelse(geodata$plm == '1', 1,
                        ifelse(geodata$plm=='2',0,NA))

##############################

str(geodata$psf)
table(geodata$psf)

geodata[which(geodata$psf=='b'),]$psf = NA

geodata$psf <- as.numeric(geodata$psf)


##############################

str(geodata$r18)
table(geodata$r18)

geodata[which(geodata$r18=='b'),]$r18 = NA

geodata$r18 <- as.numeric(geodata$r18)
##############################

str(geodata$r60)
table(geodata$r60)

geodata[which(geodata$r60=='b'),]$r60 = NA

geodata$r601 <- ifelse(geodata$r60 == '0', 1, 0)
geodata$r602 <- ifelse(geodata$r60 == '1', 1, 0)
geodata$r603 <- ifelse(geodata$r60 == '2', 1, 0)
##############################

str(geodata$r65)
table(geodata$r65)

geodata[which(geodata$r65=='b'),]$r65 = NA

geodata$r651 <- ifelse(geodata$r65 == '0', 1, 0)
geodata$r652 <- ifelse(geodata$r65 == '1', 1, 0)
geodata$r653 <- ifelse(geodata$r65 == '2', 1, 0)

##############################

str(geodata$smocp)
table(geodata$smocp)

geodata[which(geodata$smocp==-1),]$smocp = NA

describe(geodata$smocp)

##############################

str(geodata$smx)
table(geodata$smx)

geodata[which(geodata$smx=='b'),]$smx = NA

geodata$smx1 <- ifelse(geodata$smx == '1', 1, 0)
geodata$smx2 <- ifelse(geodata$smx == '2', 1, 0)
geodata$smx3 <- ifelse(geodata$smx == '3', 1, 0)
geodata$smx4 <- ifelse(geodata$smx == '4', 1, 0)


##############################

str(geodata$srnt)
table(geodata$srnt)

geodata[which(geodata$srnt=='b'),]$srnt = NA

geodata$srnt <- as.numeric(geodata$srnt)

describe(geodata$srnt)

##############################

str(geodata$ssmc)
table(geodata$ssmc)

geodata[which(geodata$ssmc=='b'),]$ssmc = NA

geodata$ssmc <- ifelse(geodata$ssmc == '0', 0, 1)


##############################

str(geodata$sval)
table(geodata$sval)

geodata[which(geodata$sval=='b'),]$sval = NA

geodata$sval <- as.numeric(geodata$sval)

describe(geodata$sval)

##############################

str(geodata$taxamt)
table(geodata$taxamt)

geodata[which(geodata$taxamt==-1),]$taxamt = NA

describe(geodata$taxamt)

##############################

str(geodata$wif)
table(geodata$wif)

geodata[which(geodata$wif=='b'),]$wif = NA

geodata$wif1 <- ifelse(geodata$wif == '1', 1, 0)
geodata$wif2 <- ifelse(geodata$wif == '1', 1, 0)
geodata$wif3 <- ifelse(geodata$wif == '1', 1, 0)
geodata$wif4 <- ifelse(geodata$wif == '1', 1, 0)

##############################

str(geodata$cit)
table(geodata$cit)

geodata$cit1 <- ifelse(geodata$cit == '1', 1, 0)
geodata$cit2 <- ifelse(geodata$cit == '2', 1, 0)
geodata$cit3 <- ifelse(geodata$cit == '3', 1, 0)
geodata$cit4 <- ifelse(geodata$cit == '4', 1, 0)
geodata$cit5 <- ifelse(geodata$cit == '5', 1, 0)

##############################

str(geodata$cow)
table(geodata$cow)

geodata$cow1 <- ifelse(geodata$cow == '1', 1, 0)
geodata$cow2 <- ifelse(geodata$cow == '2', 1, 0)
geodata$cow3 <- ifelse(geodata$cow == '3', 1, 0)
geodata$cow4 <- ifelse(geodata$cow == '4', 1, 0)
geodata$cow5 <- ifelse(geodata$cow == '5', 1, 0)
geodata$cow6 <- ifelse(geodata$cow == '6', 1, 0)
geodata$cow7 <- ifelse(geodata$cow == '7', 1, 0)
geodata$cow8 <- ifelse(geodata$cow == '8', 1, 0)
geodata$cow9 <- ifelse(geodata$cow == '9', 1, 0)
##############################

str(geodata$ddrs)
table(geodata$ddrs)

geodata[which(geodata$ddrs=='b'),]$ddrs = NA

geodata$ddrs <- ifelse(geodata$ddrs == '1', 1,
                      ifelse(geodata$ddrs=='2',0,NA))

##############################

str(geodata$dear)
table(geodata$dear)

geodata$dear <- ifelse(geodata$dear == '1', 1,
                       ifelse(geodata$dear=='2',0,NA))

##############################

str(geodata$deye)
table(geodata$deye)

geodata$deye <- ifelse(geodata$deye == '1', 1,
                       ifelse(geodata$deye=='2',0,NA))

##############################

str(geodata$dout)
table(geodata$dout)

geodata[which(geodata$dout=='b'),]$dout = NA

geodata$dout <- ifelse(geodata$dout == '1', 1,
                       ifelse(geodata$dout=='2',0,NA))

##############################

str(geodata$dphy)
table(geodata$dphy)

geodata[which(geodata$dphy=='b'),]$dphy = NA

geodata$dphy <- ifelse(geodata$dphy == '1', 1,
                       ifelse(geodata$dphy=='2',0,NA))

##############################

str(geodata$drat)
table(geodata$drat)

geodata$drat1 <- ifelse(geodata$drat=='b',0,1)

geodata[which(geodata$drat=='b'),]$drat= NA
geodata[which(geodata$drat=='6'),]$drat= NA

geodata$drat <- recode(.x = geodata$drat,
                       '1' = 0,
                       '2' = 15,
                       '3' = 35,
                       '4' = 55,
                       '5' = 80)

##############################

str(geodata$dratx)
table(geodata$dratx)

geodata[which(geodata$dratx=='b'),]$dratx = NA

geodata$dratx <- ifelse(geodata$dratx == '1', 1,
                       ifelse(geodata$dratx=='2',0,NA))


##############################

str(geodata$drem)
table(geodata$drem)

geodata[which(geodata$drem=='b'),]$drem = NA

geodata$drem <- ifelse(geodata$drem == '1', 1,
                        ifelse(geodata$drem=='2',0,NA))


##############################

str(geodata$eng)
table(geodata$eng)

geodata[which(geodata$eng=='b'),]$eng = NA

geodata$eng <- as.numeric(geodata$eng)

describe(geodata$eng)

##############################

str(geodata$fer)
table(geodata$fer)

geodata[which(geodata$fer=='b'),]$fer = NA

geodata$fer <- ifelse(geodata$fer == '1', 1,
                       ifelse(geodata$fer=='2',0,NA))

##############################

str(geodata$gcl)
table(geodata$gcl)

geodata[which(geodata$gcl=='b'),]$gcl = NA

geodata$gcl <- ifelse(geodata$gcl == '1', 1,
                      ifelse(geodata$gcl=='2',0,NA))

##############################

str(geodata$gcm)
table(geodata$gcm)

geodata[which(geodata$gcm=='b'),]$gcm = NA

geodata$gcm1 <- ifelse(geodata$gcm == '1', 1, 0)
geodata$gcm2 <- ifelse(geodata$gcm == '2', 1, 0)
geodata$gcm3 <- ifelse(geodata$gcm == '3', 1, 0)
geodata$gcm4 <- ifelse(geodata$gcm == '4', 1, 0)
geodata$gcm5 <- ifelse(geodata$gcm == '5', 1, 0)

##############################

str(geodata$gcr)
table(geodata$gcr)

geodata[which(geodata$gcr=='b'),]$gcr = NA

geodata$gcr <- ifelse(geodata$gcr == '1', 1,
                      ifelse(geodata$gcr=='2',0,NA))

##############################

str(geodata$hins1)
table(geodata$hins1)

geodata$hins1 <- ifelse(geodata$hins1 == '1', 1,
                      ifelse(geodata$hins1=='2',0,NA))

##############################

str(geodata$hins2)
table(geodata$hins2)

geodata$hins2 <- ifelse(geodata$hins2 == '1', 1,
                        ifelse(geodata$hins2=='2',0,NA))

##############################

str(geodata$hins3)
table(geodata$hins3)

geodata$hins3 <- ifelse(geodata$hins3 == '1', 1,
                        ifelse(geodata$hins3=='2',0,NA))

##############################

str(geodata$hins4)
table(geodata$hins4)

geodata$hins4 <- ifelse(geodata$hins4 == '1', 1,
                        ifelse(geodata$hins4=='2',0,NA))

##############################

str(geodata$hins5)
table(geodata$hins5)

geodata$hins5 <- ifelse(geodata$hins5 == '1', 1,
                        ifelse(geodata$hins5=='2',0,NA))

##############################

str(geodata$hins6)
table(geodata$hins6)

geodata$hins6 <- ifelse(geodata$hins6 == '1', 1,
                        ifelse(geodata$hins6=='2',0,NA))

##############################

str(geodata$hins7)
table(geodata$hins7)

geodata$hins7 <- ifelse(geodata$hins7 == '1', 1,
                        ifelse(geodata$hins7=='2',0,NA))

##############################

str(geodata$intp)
table(geodata$intp)

geodata[which(geodata$intp==-10001),]$intp = NA

describe(geodata$intp)

##############################

str(geodata$jwmnp)
table(geodata$jwmnp)

describe(geodata$jwmnp)
##############################

str(geodata$jwrip)
table(geodata$jwrip)

geodata[which(geodata$jwrip==0),]$jwrip = NA

describe(geodata$jwrip)
##############################

str(geodata$jwtr)
table(geodata$jwtr)

geodata[which(geodata$jwtr=='bb'),]$jwtr = NA

geodata$jwtr <- as.numeric(geodata$jwtr)

geodata$jwtr1  <- ifelse(geodata$jwtr == 1, 1, 0)
geodata$jwtr2  <- ifelse(geodata$jwtr == 2, 1, 0)
geodata$jwtr4  <- ifelse(geodata$jwtr == 4, 1, 0)
geodata$jwtr10 <- ifelse(geodata$jwtr == 10, 1, 0)
geodata$jwtr11 <- ifelse(geodata$jwtr == 11, 1, 0)
geodata$jwtr12 <- ifelse(geodata$jwtr == 12, 1, 0)

##############################

str(geodata$lanx)
table(geodata$lanx)

geodata[which(geodata$lanx=='b'),]$lanx = NA

geodata$lanx <- ifelse(geodata$lanx == '1', 1,
                      ifelse(geodata$lanx=='2',0,NA))

##############################

str(geodata$mar)
table(geodata$mar)

geodata$mar1  <- ifelse(geodata$mar == '1', 1, 0)
geodata$mar2  <- ifelse(geodata$mar == '2', 1, 0)
geodata$mar3  <- ifelse(geodata$mar == '3', 1, 0)
geodata$mar4  <- ifelse(geodata$mar == '4', 1, 0)
geodata$mar5  <- ifelse(geodata$mar == '5', 1, 0)

##############################

str(geodata$marhd)
table(geodata$marhd)

geodata[which(geodata$marhd=='b'),]$marhd = NA

geodata$marhd <- ifelse(geodata$marhd == '1', 1,
                       ifelse(geodata$marhd=='2',0,NA))

##############################

str(geodata$marhm)
table(geodata$marhm)

geodata[which(geodata$marhm=='b'),]$marhm = NA

geodata$marhm <- ifelse(geodata$marhm == '1', 1,
                        ifelse(geodata$marhm=='2',0,NA))

##############################

str(geodata$marht)
table(geodata$marht)

geodata[which(geodata$marht=='b'),]$marht = NA

geodata$marht1  <- ifelse(geodata$marht == '1', 1, 0)
geodata$marht2  <- ifelse(geodata$marht == '2', 1, 0)
geodata$marht3  <- ifelse(geodata$marht == '3', 1, 0)

##############################

str(geodata$marhw)
table(geodata$marhw)

geodata[which(geodata$marhw=='b'),]$marhw = NA

geodata$marhw <- ifelse(geodata$marhw == '1', 1,
                        ifelse(geodata$marhw=='2',0,NA))

##############################

str(geodata$mig)
table(geodata$mig)

geodata[which(geodata$mig=='b'),]$mig = NA

geodata$mig <- ifelse(geodata$mig== '1', 1,0)


##############################

str(geodata$mil)
table(geodata$mil)

geodata[which(geodata$mil=='b'),]$mil = NA

geodata$mil1  <- ifelse(geodata$mil == '1', 1, 0)
geodata$mil2  <- ifelse(geodata$mil == '2', 1, 0)
geodata$mil3  <- ifelse(geodata$mil == '3', 1, 0)
geodata$mil4  <- ifelse(geodata$mil == '4', 1, 0)

##############################

str(geodata$mlpa)
table(geodata$mlpa)

geodata[which(geodata$mlpa=='b'),]$mlpa = NA

geodata$mlpa <- as.numeric(geodata$mlpa)

##############################

str(geodata$mlpb)
table(geodata$mlpb)

geodata[which(geodata$mlpb=='b'),]$mlpb = NA

geodata$mlpb <- as.numeric(geodata$mlpb)

##############################

str(geodata$mlpcd)
table(geodata$mlpcd)

geodata[which(geodata$mlpcd=='b'),]$mlpcd = NA

geodata$mlpcd <- as.numeric(geodata$mlpcd)

##############################

str(geodata$mlpe)
table(geodata$mlpe)

geodata[which(geodata$mlpe=='b'),]$mlpe = NA

geodata$mlpe <- as.numeric(geodata$mlpe)

##############################

str(geodata$mlpfg)
table(geodata$mlpfg)

geodata[which(geodata$mlpfg=='b'),]$mlpfg = NA

geodata$mlpfg <- as.numeric(geodata$mlpfg)
##############################

str(geodata$mlph)
table(geodata$mlph)

geodata[which(geodata$mlph=='b'),]$mlph = NA

geodata$mlph <- as.numeric(geodata$mlph)
##############################

str(geodata$mlpi)
table(geodata$mlpi)

geodata[which(geodata$mlpi=='b'),]$mlpi = NA

geodata$mlpi <- as.numeric(geodata$mlpi)
##############################

str(geodata$mlpj)
table(geodata$mlpj)

geodata[which(geodata$mlpj=='b'),]$mlpj = NA

geodata$mlpj <- as.numeric(geodata$mlpj)
##############################

str(geodata$mlpk)
table(geodata$mlpk)

geodata[which(geodata$mlpk=='b'),]$mlpk = NA

geodata$mlpk <- as.numeric(geodata$mlpk)


##############################

str(geodata$nwab)
table(geodata$nwab)

geodata[which(geodata$nwab=='b'),]$nwab = NA

geodata$nwab1 <- ifelse(geodata$nwab == '1', 1, 0)
geodata$nwab2 <- ifelse(geodata$nwab == '2', 1, 0)
geodata$nwab3 <- ifelse(geodata$nwab == '3', 1, 0)

##############################

str(geodata$nwav)
table(geodata$nwav)

geodata[which(geodata$nwav=='b'),]$nwav = NA

geodata$nwav1 <- ifelse(geodata$nwav == '1', 1, 0)
geodata$nwav2 <- ifelse(geodata$nwav == '2', 1, 0)
geodata$nwav3 <- ifelse(geodata$nwav == '3', 1, 0)
geodata$nwav4 <- ifelse(geodata$nwav == '4', 1, 0)
geodata$nwav5 <- ifelse(geodata$nwav == '5', 1, 0)

##############################

str(geodata$nwla)
table(geodata$nwla)

geodata[which(geodata$nwla=='b'),]$nwla = NA

geodata$nwla1 <- ifelse(geodata$nwla == '1', 1, 0)
geodata$nwla2 <- ifelse(geodata$nwla == '2', 1, 0)
geodata$nwla3 <- ifelse(geodata$nwla == '3', 1, 0)

##############################

str(geodata$nwlk)
table(geodata$nwlk)

geodata[which(geodata$nwlk=='b'),]$nwlk = NA

geodata$nwlk1 <- ifelse(geodata$nwlk == '1', 1, 0)
geodata$nwlk2 <- ifelse(geodata$nwlk == '2', 1, 0)
geodata$nwlk3 <- ifelse(geodata$nwlk == '3', 1, 0)


##############################

str(geodata$nwre)
table(geodata$nwre)

geodata[which(geodata$nwre=='b'),]$nwre = NA

geodata$nwre1 <- ifelse(geodata$nwre == '1', 1, 0)
geodata$nwre2 <- ifelse(geodata$nwre == '2', 1, 0)
geodata$nwre3 <- ifelse(geodata$nwre == '3', 1, 0)

##############################

str(geodata$oip)
table(geodata$oip)

geodata[which(geodata$oip==-1),]$oip = NA

describe(geodata$oip)

##############################

str(geodata$pap)
table(geodata$pap)

geodata[which(geodata$pap==-1),]$pap = NA

describe(geodata$pap)

##############################

str(geodata$retp)
table(geodata$retp)

geodata[which(geodata$retp==-1),]$retp = NA

describe(geodata$retp)

##############################

str(geodata$semp)
table(geodata$semp)

geodata[which(geodata$semp==-10001),]$semp = NA

describe(geodata$semp)

##############################

str(geodata$ssip)
table(geodata$ssip)

geodata[which(geodata$ssip==-1),]$ssip = NA

describe(geodata$ssip)

##############################

str(geodata$ssp)
table(geodata$ssp)

geodata[which(geodata$ssp==-1),]$ssp = NA

describe(geodata$ssp)

##############################

str(geodata$wagp)
table(geodata$wagp)

geodata[which(geodata$wagp==-1),]$wagp = NA

describe(geodata$wagp)


##############################

str(geodata$wkhp)
table(geodata$wkhp)

describe(geodata$wkhp)

##############################

str(geodata$wkl)
table(geodata$wkl)

geodata[which(geodata$wkl=='b'),]$wkl = NA

geodata$wkl1 <- ifelse(geodata$wkl == '1', 1, 0)
geodata$wkl2 <- ifelse(geodata$wkl == '2', 1, 0)
geodata$wkl3 <- ifelse(geodata$wkl == '3', 1, 0)

##############################

str(geodata$wkw)
table(geodata$wkw)

geodata[which(geodata$wkw=='b'),]$wkw = NA

geodata$wkw <- recode(.x = geodata$wkw,
                      '1' = 51,
                      '2' = 48.5,
                      '3' = 43.5,
                      '4' = 33,
                      '5' = 20,
                      '6' = 7)

##############################

str(geodata$wrk)
table(geodata$wrk)

geodata[which(geodata$wrk=='b'),]$wrk = NA

geodata$wrk <- ifelse(geodata$wrk == '1', 1,
                        ifelse(geodata$wrk=='2',0,NA))

##############################

str(geodata$dis)
table(geodata$dis)

geodata$dis <- ifelse(geodata$dis == '1', 1,
                      ifelse(geodata$dis=='2',0,NA))

##############################

str(geodata$msp)
table(geodata$msp)

geodata[which(geodata$msp=='b'),]$msp = NA

geodata$msp1 <- ifelse(geodata$msp == '1', 1, 0)
geodata$msp2 <- ifelse(geodata$msp == '2', 1, 0)
geodata$msp3 <- ifelse(geodata$msp == '3', 1, 0)
geodata$msp4 <- ifelse(geodata$msp == '4', 1, 0)
geodata$msp5 <- ifelse(geodata$msp == '5', 1, 0)
geodata$msp6 <- ifelse(geodata$msp == '6', 1, 0)

##############################

str(geodata$nativity)
table(geodata$nativity)

geodata$nativity <- ifelse(geodata$nativity == '1', 1,
                      ifelse(geodata$nativity=='2',0,NA))


##############################

str(geodata$oc)
table(geodata$oc)

geodata[which(geodata$oc=='b'),]$oc = NA

geodata$oc <- as.numeric(geodata$oc)

##############################

str(geodata$paoc)
table(geodata$paoc)

geodata[which(geodata$paoc=='b'),]$paoc = NA

geodata$paoc1 <- ifelse(geodata$paoc == '1', 1, 0)
geodata$paoc2 <- ifelse(geodata$paoc == '2', 1, 0)
geodata$paoc3 <- ifelse(geodata$paoc == '3', 1, 0)
geodata$paoc4 <- ifelse(geodata$paoc == '4', 1, 0)

##############################

str(geodata$pernp)
table(geodata$pernp)

geodata[which(geodata$pernp==-10001),]$pernp = NA

describe(geodata$pernp)

##############################

str(geodata$pincp)
table(geodata$pincp)

geodata[which(geodata$pincp==-19999),]$pincp = NA

describe(geodata$pincp)

##############################

str(geodata$povpip)
table(geodata$povpip)

geodata[which(geodata$povpip==-1),]$povpip = NA

describe(geodata$povpip)

##############################

str(geodata$privcov)
table(geodata$privcov)

geodata$privcov = ifelse(geodata$privcov==1,1,0)

##############################

str(geodata$pubcov)
table(geodata$pubcov)

geodata$pubcov= ifelse(geodata$pubcov==1,1,0)

##############################

str(geodata$racaian)
table(geodata$racaian)

geodata$racaian <- as.numeric(geodata$racaian)

##############################

str(geodata$racasn)
table(geodata$racasn)

geodata$racasn <- as.numeric(geodata$racasn)

##############################

str(geodata$racblk)
table(geodata$racblk)

geodata$racblk <- as.numeric(geodata$racblk)

##############################

str(geodata$racwht)
table(geodata$racwht)

geodata$racwht <- as.numeric(geodata$racwht)

##############################

str(geodata$rc)
table(geodata$rc)

geodata[which(geodata$rc=='b'),]$rc = NA

geodata$rc <- as.numeric(geodata$rc)

##############################

str(geodata$sciengp)
table(geodata$sciengp)

geodata[which(geodata$sciengp=='b'),]$sciengp = NA

geodata$sciengp = ifelse(geodata$sciengp=='1',1,0)

##############################

str(geodata$sciengrlp)
table(geodata$sciengrlp)

geodata[which(geodata$sciengrlp=='b'),]$sciengrlp = NA

geodata$sciengrlp = ifelse(geodata$sciengrlp=='1',1,0)



################################################################################
################################################################################
################################################################################

v <- c('SERIALNO','SPORDER','FAGEP','FACCESSP','ACR','FACRP','AGS','FAGSP',
       'FBATHP','BLD', 'FBLDP','FBDSP','FBUSP','FBROADBNDP',
       'FCOMPOTHXP','FCONP','FDIALUPP','ELEFP','FELEP','FFSP','FULFP','FFULP',
       'GASFP','FGASP','HFL', 'FHFLP','FHISPEEDP','HOTWAT','FHOTWATP','FINSP',
       'FLAPTOPP','FMHP','FMRGIP','FMRGP','FMRGTP','FMRGXP','FOTHSVCEXP',
       'FREFRP','FRMSP','FRNTMP','FRWATP','FSATELLITEP','FSINKP','FSMARTPHONP',
       'FSMP','FSMOCP','FSTOVP','FTABLETP','FTELP','FTENP','FTOILP','FVALP','FVEHP',
       'FWATP','FYBLP','FFINCP','FGRNTP','FHINCP','FKITP','FMVP','FPLMP','FSMXSP',
       'FSMXHP','FTAXP','FVACSP','FCITP','FCOWP','FDDRSP','FDEARP','FDEYEP','FDOUTP',
       'FDPHYP','FDRATP','FDRATXP','FDREMP','FENGP','FFERP','FGCLP','FGCMP','FGCRP',
       'FHINS1P','FHINS2P','FHINS3P','FHINS4P','FHINS5P','FHINS6P','FHINS7P','FINTP',
       'FJWRIP','FJWTRP','FLANXP','FMARP','FMARHDP','FMARHMP','FMARHTP','FMARHWP',
       'FMIGP','FMILPP','FOIP','FPAP','FRETP','FSEMP','FSSIP','FSSP','FWAGP','FWKHP',
       'FWKLP','FWKWP','FWRKP','FPRIVCOVP','FPUBCOVP', 'MRGX','RWATPR','TEN','WATFP',
       'FES','FPARC','HHL','HHT','HUPAC','HUPAOC','HUPARC','MV','PARTNER','SMX',
       'R60','R65','WIF','CIT','COW','GCM','JWTR','MAR','MARHT','MIL','NWAB','NWAV',
       'NWLA','NWLK','NWRE','WKL', 'MSP','RACNUM','RACNH','RACPI','SCH','PAOC',
       'fesrp','fhicovp','flanp','fmarhyp','fmigsp','fmilsp','fpernp','fpincp','st')

v <- tolower(v)


geodata <- geodata[,!colnames(geodata)%in%v]

t(summary(geodata))


geodata <- as.data.frame(geodata)

sds <- c()

for(i in 1:296){
  
  sds[i] = sd(geodata[,i],na.rm=TRUE)
}

################################################################################

geodata2 <- as_tibble(geodata2)

puma_summary <- geodata2 %>% 
  group_by(puma) %>% 
  summarise_if(is.numeric,mean,na.rm=TRUE)


corr <- cor(puma_summary[,2:296])

eig <- eigen(corr)$values
plot(eig)


fit <- principal(puma_summary[,2:296],4,rotate='promax',scores=TRUE)

sc <- fit$scores

sc <- scale(sc)

puma_summary$pc1 <- sc[,1]
puma_summary$pc2 <- sc[,2]
puma_summary$pc3 <- sc[,3]
puma_summary$pc4 <- sc[,4]


write.csv(puma_summary, 
          here('data','puma_summary.csv'),
          row.names = FALSE)











