library(readr)
library(dplyr)

base_completa <- read_delim("C:/Users/Juan/Dropbox/POSGRADOS/Uba/99-Tesis/scripts/scripts/LDA/base_completa.csv",   "\t", escape_double = FALSE, col_types = cols(X1 = col_skip(), id_1 = col_skip()), trim_ws = TRUE)


glimpse(base_completa)


table(base_completa$Año,base_completa$dominant_topic)

bla <- base_completa %>%
  group_by(Año) %>%
  summarise_each(funs(mean(., na.rm = TRUE)))
  

View(bla)
table(base_completa$Topic0)


base_completa$Topic89
