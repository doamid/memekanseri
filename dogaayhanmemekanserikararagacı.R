#amacımız olan veri setimizi çagırıyoruz.Veri setini çağırmazsak işlemleri gerçekleştiremeyiz.
eskiveri <- read.table(file.choose(), header = T, sep = ";")
head(eskiveri)
str(eskiveri)


#veri setimizdeki verilerin adı ingilizce olduğu için isimlerini türkçeye çevirdik.
veri <- eskiveri[, c("Id", "Cl.thickness","Cell.size","Cell.shape","Marg.adhesion","Epith.c.size", "Bare.nuclei", "Bl.cromatin", "Normal.nucleoli", "Mitoses", "Class" )]
colnames(veri)<-c("Isım", "ceperkalinligi","hucreboyutu","hucresekli","margyapismasi","dokuboyutu","hucreyuksekligi", "agyapisi","genetikmateryalparcalar","bolunme", "Kansermi") 


veri <- veri[-1]

set.seed(1234)
ind <- sample(1:699,450)
memekanseri <- veri[ind,]
View(memekanseri)

#burada factore dönüştürüyoruz
memekanseri$Kansermi<- as.factor(memekanseri$Kansermi)

levels(memekanseri$Kansermi)<- c("0"="hayir","1"="evet")


head(memekanseri) 

#karar ağacımızı çalıştırmak için paketlerimizi yükledik.Paketlerimizi çalıştıramazsak istediğimiz sonucu alamayız.
library(RWeka)
kararagaci <- J48(Kansermi ~ ., data = memekanseri)
kararagaci
print(kararagaci)
summary(kararagaci)
plot(kararagaci)

#ongoru yapalım

ongoru <- predict(kararagaci)
ongoru

#modelin karışıklık matrisi tabosuna bakalım
table(memekanseri$Kansermi, ongoru)
#isim koyalım
karisiklikmatrisi <- table(memekanseri[,10], ongoru)
karisiklikmatrisi

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

