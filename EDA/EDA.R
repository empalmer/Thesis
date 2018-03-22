library(tidyverse)
library(museR)


keyss <- map(bach_df,Major_minor)
keyss
scaless <- map(bach_df,scale_degree_freq)
scaless

mel_felix <- map(felix_df,mel_ints,"V")
mel_fanny <- map(fanny_pieces,mel_ints,"V")