# merging the original dataframe with the added columns

library(dplyr)
library(readxl)
library(haven)

# Wrangling the two dataframes

datosCarola_invasive_seq0_1_NoID <- read_excel("~/Downloads/Registro Central de Cáncer/datosCarola_invasive_seq0&1_NoID.xlsx")
                                               #)%>%
#  select(-c(ydiag, SeqNumCentr, yy_dlc))

encrypted_marital_meses_reducida_toshare <- read_dta("~/Downloads/Registro Central de Cáncer/encrypted & marital &meses_reducida_toshare.dta") %>%
  rename(EncryptedID = encryptedid)

df <- full_join(datosCarola_invasive_seq0_1_NoID, encrypted_marital_meses_reducida_toshare, by = "EncryptedID", suffix = c(".x", ".y"))

# Rows missing from added columns in comparison with full base

anti1 <- anti_join (datosCarola_invasive_seq0_1_NoID, encrypted_marital_meses_reducida_toshare, by = "EncryptedID") %>% 
  select(ydiag, EncryptedID, yy_dlc, VitalStatus_label, CVDDeathCause); View(anti1)

# Rows missing from full base in comparison with added columns

anti2 <- anti_join(encrypted_marital_meses_reducida_toshare, datosCarola_invasive_seq0_1_NoID, by = "EncryptedID"); View(anti2)


df %>% filter(is.na(maritallabel)) %>% View()
df %>% filter(is.na(maritallabel)) %>% select(ydiag.x, ydiag.y, mm_dx, EncryptedID, yy_dlc.x, yy_dlc.y, mm_dlc, VitalStatus_label, maritallabel) %>% View()

df %>% filter(is.na(ydiag.x)) %>% View() 
