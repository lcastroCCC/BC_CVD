# merging the original dataframe with the added columns

library(dplyr)
library(readxl)
library(haven)

# Wrangling the two dataframes

datosCarola_invasive_seq0_1_NoID <- read_excel(
  "C:/Users/Genesis Rodriguez/Box/Sanchez-Diaz Lab/CVD and BC/Registro Central de Cáncer/Data/datosCarola_invasive_seq0&1_NoID.xlsx") %>%
  select(-c(ydiag, SeqNumCentr, yy_dlc))

encrypted_marital_meses_reducida_toshare <- read_dta(
  "C:/Users/Genesis Rodriguez/Box/Sanchez-Diaz Lab/CVD and BC/Registro Central de Cáncer/Data/encrypted & marital &meses_reducida_toshare.dta") %>%
  rename(EncryptedID = encryptedid)

merge <- inner_join(datosCarola_invasive_seq0_1_NoID, encrypted_marital_meses_reducida_toshare, by = "EncryptedID")

write.csv(merge,  "C:/Users/Genesis Rodriguez/Box/Sanchez-Diaz Lab/CVD and BC/Registro Central de Cáncer/Data/cancer_registry_20240924.csv")
