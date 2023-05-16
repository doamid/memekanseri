#veri setimizden verilerimizi işlem yapabilmek için çağıralım.
eskiveri<- read.table(file.choose(), header = T, sep=";")

#naive bayes icin e1071 ve caret paketlerimizi yüklememiz gerekiyor.Paketler önemli ayrıntılar
install.packages("e1071")
library(e1071)
library(caret)


#verilerimizi türkçeye çeviriyoruz.
veri <- eskiveri[, c("Id", "Cl.thickness","Cell.size","Cell.shape","Marg.adhesion","Epith.c.size", "Bare.nuclei", "Bl.cromatin", "Normal.nucleoli", "Mitoses", "Class" )]
colnames(veri)<-c("Isım", "ceperkalinligi","hucreboyutu","hucresekli","margyapismasi","dokuboyutu","hucreyuksekligi", "agyapisi","genetikmateryalparcalar","bolunme", "Kansermi") 
#veri setini parçaladık
set.seed(1234)
ind <- sample(1:699,450)
memekanseri <- veri[ind,]
View(memekanseri)

memekanseri<- memekanseri[-1]

#naive bayes için veri setimizin özet haline bakıyoruz.Emin olabilmek için.

View(memekanseri)
str(memekanseri)


#MODEL icin e1071 paketinin icindeki naivebayes komutunu kullanmamız gerekiyor.
veri <- naiveBayes(Kansermi~., memekanseri)
print(veri)

#ongoru yapmamız gerekiyor
ongoru <- predict(veri,memekanseri)

#ongoru değerleri ile gercek değerleri karsılastıralım 

table(memekanseri$Kansermi,ongoru)

#veri isimli veri setinin tum ongorulerine bakalım 
print(ongoru)

#karisiklik matrisi
karisiklikmatrisi <- table(memekanseri[,10],ongoru)
karisiklikmatrisi

#dogruluk oranini bulmamız lazım onemli konu
sum(diag(karisiklikmatrisi))/sum(karisiklikmatrisi)



#modelin performans değerlendirme ölçütleri
#doğru pozitif
(TP <- karisiklikmatrisi [1])
#yanlış pozitif
(FP <- karisiklikmatrisi [3])
#yanlış negatif
(FN <- karisiklikmatrisi [2])
#doğru negatif
(TN <- karisiklikmatrisi [4])

#performans degerlendirme olcutleri
paste0("Dogruluk = ",(Dogruluk <- (TP+TN)/sum(karisiklikmatrisi)))
paste0("Hata = ",(Hata <- 1-Dogruluk))
#TPR=Duyarlilik orani
paste0("TPR= ", (TPR <- TP/(TP+FN)))
#SPC=Belirleyicilik orani
paste0("SPC= ", (SPC <- TN/(FP+TN)))
#PPV=kesinlik ya da pozitif ongoru degeri
paste0("PPV= ", (PPV <- TP/(TP+FP)))
#NPV=negatif ongoru degeri
paste0("NPV= ", (NPV <- TN/(TN+FN)))
#FPR=Yanlis pozitif orani
paste0("FPR = ", (FPR <- FP/sum(karisiklikmatrisi)))
#FNR=Yanlis negatif orani
paste0("FNR=",(FNR <- FN/(FN+TP)))
#F olcutu kesinlik ve duyarlilik olcutlerinin harmonik ortalamasi
paste0("F_measure = ", (F_measure <- (2*PPV*TPR)/(PPV+TPR)))



