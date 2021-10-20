library(arules)
transaksi_tabular <- read.transactions(file="https://storage.googleapis.com/dqlab-dataset/transaksi_dqlab_retail.tsv", format="single", sep="\t", cols=c(1,2), skip=1)
write(transaksi_tabular, file="test_project_retail_1.txt", sep=",")


# Statistik Top 10
library(arules)
#transaksi_tabular <-read.transactions(file="transaksi_dqlab_retail.tsv")
data_top <- itemFrequency(transaksi_tabular,type="absolute")
data_top <- sort(data_top,decreasing=TRUE)
data_top <- data_top[1:10]
data_top <-data.frame("Nama.Produk"=names(data_top), "Jumlah" =data_top,row.names=NULL)
data_top
write.csv(data_top,file="top10_item_retail.txt",sep=",")

#Statistik Bottom 10

library(arules)
#transaksi_tabular<-read.transactions(file="transaksi_dqlab_retail.tsv",format="single",sep="\t",cols=c(1,2),skip=1)
data_bottom <-itemFrequency(transaksi_tabular,type="absolute")
data_bottom <-sort(data_bottom, decreasing = FALSE)
data_bottom <- data_bottom[1:10]
data_bottom <-data.frame("Nama.Produk"=names(data_bottom),"Jumlah"=data_bottom,row.names=NULL)
data_bottom
write.csv(data_bottom,file="bottom10_item_retail.txt",sep=",")

# Mendapatkan Kombinasi Produk yang menarik
library(arules)
#transaksi_tabular<-read.transactions(file="transaksi_dqlab_retail.tsv",format="single",sep="\t",cols=c(1,2),skip=1)
komb.rules <-apriori(transaksi_tabular,parameter=list(supp=10/length(transaksi_tabular),confidence=0.5,minlen=2,maxlen=3))

kombinasi.produk <-head(sort(komb.rules,by="lift",decreasing="TRUE"),10)
inspect(kombinasi.produk)
write(kombinasi.produk,file="kombinasi_retail.txt")

# Mencari Paket Produk yang bisa dipasangkan dengan Item Slow-Moving
library(arules)
#transaksi_tabular <-read.transactions(file="transaksi_dqlab_retail.tsv",format="single",sep="\t",cols=c(1,2),skip=1)
komb_rules <-apriori(transaksi_tabular,parameter=list(supp=10/length(transaksi_tabular),confidence=0.1,minlen=2,maxlen=3))
value1 <-subset(komb_rules,rhs %in% "Tas Makeup")
value1.result <-head(sort(value1,by="lift", decreasing="TRUE"),3)
value2 <-subset(komb_rules,rhs %in% "Baju Renang Pria Anak-anak")
value2.result <-head(sort(value2,by="lift",decreasing="TRUE"),3)
result <-c(value1.result,value2.result)
inspect(result)
write(result,file="kombinasi_retail_slow_moving.txt")