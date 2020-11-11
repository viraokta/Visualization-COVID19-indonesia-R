#untuk akses API menggunakan fungsi GET() pada paket HTTR 
install.packages("httr")
library(httr)
resp <- GET("https://data.covid19.go.id/public/api/update.json")

#untuk mengetahui status permintaan
status_code(resp)
resp$status_code
identical(resp$status_code, status_code(resp))
headers(resp)

#untuk mengekstrak isi respon
cov_id_raw <- content(resp, as = "parsed", simplifyVector = TRUE) 
length(cov_id_raw)
names(cov_id_raw)

#mengekstrak komponen ke-2 dari nama kompone update
cov_id_update <- cov_id_raw$update
lapply(cov_id_update,names)

#mengetahui pembaharuan kasus
cov_id_update$penambahan$tanggal

#mengetahui jumlah penambahan kasus sembuh
cov_id_update$penambahan$jumlah_sembuh

#mengetahui jumlah penambahan kasus meninggal
cov_id_update$penambahan$jumlah_meninggal

#mengetahui jumlah total kasus positif hingga saat ini
cov_id_update$total$jumlah_positif

#mengetahui jumlah total kasus meninggal hingga saat ini
cov_id_update$total$jumlah_meninggal

#mengambil data jabar
resp_jabar <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_BARAT.json")
cov_jabar_raw <- content(resp_jabar, as = "parsed", simplifyVector = TRUE)
names(cov_jabar_raw)

#mengetahui kasus total
cov_jabar_raw$kasus_total

#mengetahui kasus meninggal
cov_jabar_raw$meninggal_persen

#mengetahui kasus sembuh
cov_jabar_raw$sembuh_persen
cov_jabar <- cov_jabar_raw$list_perkembangan
str(cov_jabar)
head(cov_jabar)

#menjinakkan data 
library(dplyr)

#menghapus kolom "DIRAWAT_OR_ISOLASI" dan "AKUMULASI_DIRAWAT_OR_ISOLASI
#menghapus semua kolom yang berisi nilai kumulatif
#mengganti nama kolom
#memperbaiki data pada kolom tanggal
new_cov_jabar <-
  cov_jabar %>% 
  select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
  select(-starts_with("AKUMULASI")) %>% 
  rename(
    kasus_baru = KASUS,
    meninggal = MENINGGAL,
    sembuh = SEMBUH
  ) %>% 
  mutate(
    tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
    tanggal = as.Date(tanggal)
  )
new_cov_jabar
str(new_cov_jabar)  
 
#menunjukan melalui gambar
library(ggplot2)
library(hrbrthemes)

#kasus positif
ggplot(new_cov_jabar, aes(x = tanggal, y = kasus_baru)) +
  geom_col(fill = "salmon") +
  labs(
    x = NULL,
    y = "Total Cases",
    title = "Daily Positive Cases COVID-19 in West java",
    caption = "Data source: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

#kasus sembuh
ggplot(new_cov_jabar, aes(tanggal, sembuh)) +
  geom_col(fill = "olivedrab2") +
  labs(
    x = NULL,
    y = "Total cases",
    title = "Daily Recovered Cases from COVID-19 in West Java",
    caption = "Data source: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13, 
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

#kasus_meninggal
ggplot(new_cov_jabar, aes(tanggal, meninggal)) +
  geom_col(fill = "darkslategray4") +
  labs(
    x = NULL,
    y = "Total cases",
    title = "Daily cases died from COVID-19 in West Java",
    caption = "Data source: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

library(dplyr)
library(lubridate)
cov_jabar_pekanan <- new_cov_jabar %>% 
  count(
    tahun = year(tanggal),
    pekan_ke = week(tanggal),
    wt = kasus_baru,
    name = "jumlah"
  )

#membuat kolom baru "jumlah_pekanlalu"
#mengganti nilai NA pada kalom "jumlah_pekanlalu" dengan nilai 0
#melakukan komparasi antara kolom "jumlah" dengan kolom "jumlah_pekanlalu"lalu dismpan dengan nama lebih_baik
glimpse(cov_jabar_pekanan)
library(dplyr)
cov_jabar_pekanan <-
  cov_jabar_pekanan %>% 
  mutate(
    jumlah_pekanlalu = dplyr::lag(jumlah, 1),
    jumlah_pekanlalu = ifelse(is.na(jumlah_pekanlalu), 0, jumlah_pekanlalu),
    lebih_baik = jumlah < jumlah_pekanlalu
  )

#membuat bar chart
glimpse(cov_jabar_pekanan)
library(ggplot2)
library(hrbrthemes)
ggplot(cov_jabar_pekanan, aes(pekan_ke, jumlah, fill = lebih_baik)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(breaks = 9:29, expand = c(0, 0)) +
  scale_fill_manual(values = c("TRUE" = "yellow", "FALSE" = "salmon")) +
  labs(
    x = NULL,
    y = "Total cases",
    title = "Positive Weekly Cases of COVID-19 in West Java",
    subtitle = "The green column shows the addition of new cases was less than a week earlier",
    caption = "Data source: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")
cov_jabar_akumulasi <- 
  new_cov_jabar %>% 
  transmute(
    tanggal,
    akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
    akumulasi_sembuh = cumsum(sembuh),
    akumulasi_meninggal = cumsum(meninggal)
  )

tail(cov_jabar_akumulasi)

#cara 1 untuk membuat grafik
library(ggplot2)
ggplot(data = cov_jabar_akumulasi, aes(x = tanggal, y = akumulasi_aktif)) +
  geom_line()

ggplot(data = cov_jabar_akumulasi, aes(x = tanggal)) +
  geom_line(aes(x = tanggal, y = akumulasi_aktif), color = "black") +
  geom_line(aes(x = tanggal, y = akumulasi_sembuh), color = "orange") +
  geom_line(aes(x = tanggal, y = akumulasi_meninggal), color = "blue")

install.packages("tidyr")
library(tidyr)
dim(cov_jabar_akumulasi)

#cara 2 membuat grafik dengan melakukan pivot pada data
cov_jabar_akumulasi_pivot <- 
  cov_jabar_akumulasi %>% 
  gather(
    key = "kategori",
    value = "jumlah",
    -tanggal
  ) %>% 
  mutate(
    kategori = sub(pattern = "akumulasi_", replacement = "", kategori)
  )
dim(cov_jabar_akumulasi_pivot)
glimpse(cov_jabar_akumulasi_pivot)

cov_jabar_akumulasi_pivot <-
  cov_jabar_akumulasi %>%
  pivot_longer(
    cols = -tanggal,
    names_to = "kategori",
    names_prefix = "akumulasi_",
    values_to = "jumlah"
  )

dim(cov_jabar_akumulasi_pivot)
glimpse(cov_jabar_akumulasi_pivot)

ggplot(cov_jabar_akumulasi_pivot, aes(tanggal, jumlah, colour = (kategori))) +
  geom_line(size = 0.9) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
  scale_colour_manual(
    values = c(
      "aktif" = "salmon",
      "meninggal" = "darkslategray4",
      "sembuh" = "olivedrab2"
    ),
    labels = c("Aktif", "Meninggal", "Sembuh")
  ) +
  labs(
    x = NULL,
    y = "Jumlah kasus akumulasi",
    colour = NULL,
    title = "Dinamika Kasus COVID-19 di Jawa Barat",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )

