# Membaca Data External
data = read.csv("https://storage.googleapis.com/dqlab-dataset/project.csv")

# Inspeksi data

# Enam baris teratas data
head(data)

# Tampilkan tipe data setiap kolomnya
str(data)

# Statistik Dekriptif data
summary(data)

# Menghapus kolom X dan nama nasabah
data_reduce = data[-c(1,2)]
colnames(data_reduce)

# Pemilihan data kategori
data_kategorik = data_reduce[,c("KONDISI_USAHA","KONDISI_JAMINAN","REKOMENDASI_TINDAK_LANJUT")]

data_reduce$REKOMENDASI_TINDAK_LANJUT = as.factor(data_reduce$REKOMENDASI_TINDAK_LANJUT)

chisq.test(data_kategorik$KONDISI_USAHA,data_kategorik$REKOMENDASI_TINDAK_LANJUT)
chisq.test(data_kategorik$KONDISI_JAMINAN,data_kategorik$REKOMENDASI_TINDAK_LANJUT)


# Korelasi antar variabel data
library(corrplot)
library(ggcorrplot)

M = data_reduce[,8:11]

par(mfrow=c(2,2))
corrplot(cor(M),type = "upper",order="hclust")
corrplot(cor(M), method="square",type ="upper")
corrplot(cor(M), method="number", type = "lower")
corrplot(cor(M), method ="ellipse")

par(mfrow=c(2,2))
corrplot(cor(M,method="kendall"), type = "upper", order="hclust")
corrplot(cor(M,method="kendall"), method="square", type="upper")
corrplot(cor(M,method="kendall"), method="number", type="lower")
corrplot(cor(M,method="kendall"), method = "ellipse")

corr = round(cor(M),1)
ggcorrplot(round(cor(M),1),
           hc.order = TRUE,
           type="lower",
           lab=TRUE,
           lab_size=3,
           method="circle",
           colors=c("tomato2","white","springgreen3"),
           title="Correlogram of Data Nasabah",
           ggtheme=theme_bw)

