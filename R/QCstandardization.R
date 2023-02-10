qcID <- "1k3PZTGZ1n1eljVbXx9qR-PtQ-7LIdj-mi7wtEmzu2iM"

qcSheet <- googlesheets4::read_sheet(qcID)
head(qcSheet)


allKeys <- googlesheets4::read_sheet("16edAnvTQiWSQm49BLYn_TaqzHtKO9awzv5C-CemwyTY")

standardKeyNames <- allKeys$name[!"paleoData_proxyGeneral" == allKeys$name]

tsNames <- sub("interpretation", "climateInterpretation1", standardKeyNames)

keyConversion <- googlesheets4::read_sheet("1T5RrAtrk3RiWIUSyO0XTAa756k6ljiYjYpvP67Ngl_w")

#QCkeys and corresponding TSName and corresponding standardKey in DF
keyConDF <- keyConversion[keyConversion$tsName %in% tsNames,c(1,2)]
keyConDF$standKey <- standardKeyNames[unlist(lapply(keyConDF$tsName, function(x) which(x == sub("interpretation", "climateInterpretation1", standardKeyNames))))]


#for each key, standardize terms in qc sheet
for (nm in standardKeyNames){
  #load corresponding standard table
  tableNow <- standardTables[eval(nm)]
  QCKeyNow <- keyConDF$qcSheetName[keyConDF$standKey == eval(nm)]
  QCTermsNow <- qcSheet[eval(QCKeyNow)]
  #check validity of the terms

}










