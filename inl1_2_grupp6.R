#nodvandiga paket
library(plyr)
library(data.table)
library(corrgram)


#inlasning
indata <- read.csv2("stockstaden.txt", sep="")
indata[,]
df <- indata
df[TRUE,] = NA
#ta bort V95, omnamns inte i kodboken och kan inte tolkas
df <- subset (df, select = -V95) 

#dop om variabler till lattolkade namn
names(df)[names(df)=="V3"] <- "intervjunummer"
names(df)[names(df)=="V240"] <- "alder"
names(df)[names(df)=="V238"] <- "kon"
names(df)[names(df)=="V246"] <- "utbildning"
names(df)[names(df)=="V110"] <- "tidningar"
names(df)[names(df)=="V113"] <- "polisen"
names(df)[names(df)=="V114"] <- "domstolar"
names(df)[names(df)=="V116"] <- "partier"
names(df)[names(df)=="V119"] <- "universiteten"
names(df)[names(df)=="V120"] <- "foretagen"
names(df)[names(df)=="V122"] <- "miljo"
names(df)[names(df)=="V124"] <- "bistand"
names(df)[names(df)=="V125"] <- "EU"
names(df)[names(df)=="V126"] <- "FN"
names(df)[names(df)=="V81"] <- "miljotillv"
names(df)[names(df)=="V82"] <- "donation"
names(df)[names(df)=="V83"] <- "demonstration"
names(df)[names(df)=="V193"] <- "vetenskap"
df$intervjunummer <- indata[, 1]

#alder, kon
df$alder <- indata[, 2]
df$alder[ which (df$alder < 18 | df$alder> 99 ) ] <- NA
df$kon <- indata[, 3]

#utbildningar
df$utbildning <- indata[, 4]
df$utbildningbin <- ifelse(as.numeric(indata[, 4])>=8, 1, 0) #nivå 10 är en felstavning av 9 och behöver inte bortses


#fortroendefragor

#tidningar
df$tidningar <- indata[, 5]
df$tidbin <- NA
df$tidbin [which (indata[, 5]==1 | indata[, 5]==2) ] <- 1
df$tidbin [which (indata[, 5]==3 | indata[, 5]==4) ] <- 0

#polisen
df$polisen <- indata[, 6]
df$polbin <- NA
df$polbin [which (indata[, 6]==1 | indata[, 6]==2) ] <- 1
df$polbin [which (indata[, 6]==3 | indata[, 6]==4) ] <- 0

#domstolar
df$domstolar <- indata[, 7]
df$dombin <- NA
df$dombin [which (indata[, 7]==1 | indata[, 7]==2) ] <- 1
df$dombin [which (indata[, 7]==3 | indata[, 7]==4) ] <- 0

#partier
df$partier <- indata[, 8]
df$parbin <- NA
df$parbin [which (indata[, 8]==1 | indata[, 8]==2) ] <- 1
df$parbin [which (indata[, 8]==3 | indata[, 8]==4) ] <- 0

#universiteten
df$universiteten <- indata[, 9]
df$unibin <- NA
df$unibin [which (indata[, 9]==1 | indata[, 9]==2) ] <- 1
df$unibin [which (indata[, 9]==3 | indata[, 9]==4) ] <- 0

#foretagen
df$foretagen <- indata[, 10]
df$forbin <- NA
df$forbin [which (indata[, 10]==1 | indata[, 10]==2) ] <- 1
df$forbin [which (indata[, 10]==3 | indata[, 10]==4) ] <- 0

#miljo
df$miljo <- indata[, 11]
df$milbin <- NA
df$milbin [which (indata[, 11]==1 | indata[, 11]==2) ] <- 1
df$milbin [which (indata[, 11]==3 | indata[, 11]==4) ] <- 0

#bistand
df$bistand <- indata[, 12]
df$bisbin <- NA
df$bisbin [which (indata[, 12]==1 | indata[, 12]==2) ] <- 1
df$bisbin [which (indata[, 12]==3 | indata[, 12]==4) ] <- 0

#EU
df$EU <- indata [, 13]
df$eubin <- NA
df$eubin [which (indata[, 13]==1 | indata[, 13]==2) ] <- 1
df$eubin [which (indata[, 13]==3 | indata[, 13]==4) ] <- 0

#FN
df$FN <- indata [, 14]
df$fnbin <- NA
df$fnbin [which (indata[, 14]==1 | indata[, 14]==2) ] <- 1
df$fnbin [which (indata[, 14]==3 | indata[, 14]==4) ] <- 0

#Miljotillvaxt
df$miljotillv <- indata[, 15]
df$miljotillv [which (indata[, 15]< -3) ] <- NA

#donation
df$donation <- indata[, 16]
df$donation <- NA
df$donation [which (indata[, 16]==2)] <- 0
df$donation [which (indata[, 16]==1)] <- 1

#demonstration
df$demonstration <- indata[, 17]
df$demonstration <- NA
df$demonstration [which (indata[, 17]==2)] <- 0
df$demonstration [which (indata[, 17]==1)] <- 1

#en kolumn med titel V95 omnämns inte i kodboken och importeras inte

#vetenskap
df$vetenskap <- indata[, 19]
df$vetenskap [which (indata[, 19]< 1) ] <- NA #enligt kodbok ska lägsta värde vara 1

#indexvarde
sum(df$bisbin, df$dombin, df$eubin, df$fnbin, df$forbin, df$milbin, +
      df$parbin,  df$polbin, df$tidbin, df$unibin, NA, na.rm=TRUE)

#totalt antal saknade varden
sum(is.na(df$alder), is.na(df$kon), is.na(df$utbildningbin), is.na(df$bisbin), is.na(df$dombin), +
    is.na(df$eubin), is.na(df$fnbin), is.na(df$forbin), is.na(df$milbin), +
    is.na(df$parbin),  is.na(df$polbin), is.na(df$tidbin), is.na(df$unibin))

#klassindelad variabel for alder
df$alderkategori <- NA
df$alderkategori[ which (df$alder >= 18 & df$alder<= 25 ) ] <- "18-25"
df$alderkategori[ which (df$alder >= 26 & df$alder<= 35 ) ] <- "26-35"
df$alderkategori[ which (df$alder >= 36 & df$alder<= 45 ) ] <- "36-45"
df$alderkategori[ which (df$alder >= 46 & df$alder<= 55 ) ] <- "46-55"
df$alderkategori[ which (df$alder >= 56 & df$alder<= 65 ) ] <- "56-65"
df$alderkategori[ which (df$alder >= 66 & df$alder<= 75 ) ] <- "66-75"
df$alderkategori[ which (df$alder >= 76 & df$alder<= 85 ) ] <- "76-85"

#kon omkodad till binar
df$konbin <- NA
df$konbin [which (df$kon == 'Man')] <- 1
df$konbin [which (df$kon == 'Kvinna')] <- 2


#skriv ut korstabeller for alder, kon, utbildning samt fortroendevariabler
#OBS lang kod, fortsatter till rad 260

table(df$kon)
table(df$alderkategori, df$kon)
table(df$utbildningbin, df$kon)
table(df$kon, df$demonstration)

#fortroende per kon

table(df$bisbin, df$kon)
sum(is.na(df$bisbin))

table(df$dombin, df$kon)
sum(is.na(df$dombin))

table(df$eubin, df$kon)
sum(is.na(df$eubin))

table(df$fnbin, df$kon)
sum(is.na(df$fnbin))

table(df$forbin, df$kon)
sum(is.na(df$forbin))

table(df$milbin, df$kon)
sum(is.na(df$milbin))

table(df$parbin, df$kon)
sum(is.na(df$parbin))

table(df$polbin, df$kon)
sum(is.na(df$polbin))

table(df$tidbin, df$kon)
sum(is.na(df$tidbin))

table(df$unibin, df$kon)
sum(is.na(df$unibin))

#fortroende per alder


table(df$bisbin, df$alderkategori)
sum(is.na(df$bisbin))

table(df$dombin, df$alderkategori)
sum(is.na(df$dombin))

table(df$eubin, df$alderkategori)
sum(is.na(df$eubin))

table(df$fnbin, df$alderkategori)
sum(is.na(df$fnbin))

table(df$forbin, df$alderkategori)
sum(is.na(df$forbin))

table(df$milbin, df$alderkategori)
sum(is.na(df$milbin))

table(df$parbin, df$alderkategori)
sum(is.na(df$parbin))

table(df$polbin, df$alderkategori)
sum(is.na(df$polbin))

table(df$tidbin, df$alderkategori)
sum(is.na(df$tidbin))

table(df$unibin, df$alderkategori)
sum(is.na(df$unibin))

#fortroende per utbildning

table(df$bisbin, df$utbildningbin)
sum(is.na(df$bisbin))

table(df$dombin, df$utbildningbin)
sum(is.na(df$dombin))

table(df$eubin, df$utbildningbin)
sum(is.na(df$eubin))

table(df$fnbin, df$utbildningbin)
sum(is.na(df$fnbin))

table(df$forbin, df$utbildningbin)
sum(is.na(df$forbin))

table(df$milbin, df$utbildningbin)
sum(is.na(df$milbin))

table(df$parbin, df$utbildningbin)
sum(is.na(df$parbin))

table(df$polbin, df$utbildningbin)
sum(is.na(df$polbin))

table(df$tidbin, df$utbildningbin)
sum(is.na(df$tidbin))

table(df$unibin, df$utbildningbin)
sum(is.na(df$unibin))

#chi^2-test for demonstration och donation per alder, kon och utbildning

chisq.test(df$kon, df$demonstration)
chisq.test(df$alderkategori, df$demonstration)
chisq.test(df$utbildningbin, df$demonstration)
table(df$utbildningbin, df$demonstration)

chisq.test(df$kon, df$donation)
chisq.test(df$alderkategori, df$donation)
chisq.test(df$utbildningbin, df$donation)
table(df$utbildningbin, df$donation)