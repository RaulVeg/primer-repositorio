library(affy)
BiocManager::install("biocLite")
source("http://bioconductor.org/biocLite.R")
biocLite("simpleaffy")

gse <- getGEO("GSE100988", GSEMatrix =TRUE, AnnotGPL=TRUE)
gse
length(gse)
dim(gse)
gse <- gse[[1]]
gse
hist(gse$organism_ch1, col =rainbow)

microarreglo <- ReadAffy(gse, verbose = T)
#pData(gset) #Sample information 
#fData(gset)
exprs(gse) #EXPRESIÓN DE LOS DATOS
summary(exprs(gse))#DA INFORMACIÓN SOBRE EL MÍNIMO, EL PRIMER CUARTILE Y EL TERCERO. ADEMÁS DE MEDIA Y MEDIANA 
exprs(gse) <- log2(exprs(gse))#SE TRANSFORMAN EN LOG2 PARA VER LA EXPRESIÓN Y PODER SACAR EL FC 
boxplot(exprs(gse),outline=FALSE) # SE OBSERVA SI ESTÁ NORMALIZADO 
inf<-pData(gse)
dim(inf)
View(inf)
inf<-select(inf, characteristics_ch1,characteristics_ch1.1, characteristics_ch1.2, characteristics_ch1.3)
inf<-rename(inf, gender=characteristics_ch1, age=characteristics_ch1.1, altitude=characteristics_ch1.2, ancestry=characteristics_ch1.3)

inf #SOLO CON LO QUE INTERESA 
View(inf)
infor<- select(infor, characteristics_ch1.3, characteristics_ch1.5)
infor<- rename(infor, ancestry=characteristics_ch1.3, Sample=characteristics_ch1.5)
infor

library(pheatmap)
coMatrix <- cor(exprs(gse),use="c") #Correlacion por pares de las muestras 
pheatmap(coMatrix)
rownames(infor)
colnames(coMatrix)
rownames(infor) <- colnames(coMatrix)
pheatmap(coMatrix,
         annotation_col=infor)

install.packages("ggrepel")
library(ggrepel) #colocar etiquetas de manera mas conveniente 
library(ggplot2)
acp <- prcomp(t(exprs(gse)))
cbind(infor, acp$x) %>% 
  ggplot(aes(x = PC1, y=PC2, col=ancestry,label=paste("Sample", Sample))) + geom_point() + geom_text_repel()

library(readr)
ouput <- cbind(fData(gse),exprs(gse))
write_csv(ouput, path="Processed_data/gse_output.csv") #Exportar datos de la expresion para ser visto en excel
feature <- fData(gse)
View(feature)
featurer <- select(feature,ID, Symbol, Protein_Product, Chromosome)
ouput <- cbind(feature,exprs(gse))
write_csv(ouput, path="Processed_data/gse_output.csv")

#BASE DE DATOS# 
#AHORA SIGUE LA EXPRESIÓN DIFERENCIAL 



desig <- model.matrix(~0+infor$ancestry)
desig #Aqui los que aparecen on 1 son lo que poseen dicha cracacteristica
colnames(desig) <- c("Andinos","Europeos")
desig
summary(exprs(gse))
#para saber cuáles están expresados o no# 
fi <- lmFit(exprs(gse), desig)
head(fi$coefficients)
contrast <- makeContrasts( Andinos - Europeos, levels=desig)
fi2 <- contrasts.fit(fi, contrast)
fi2 <- eBayes(fi2)
topTable(fi2)

table<-topTable(fi2, number=20, sort.by = "p")
table
View(table)

#DECIDE TEST PARA SABER GENES DIFERENCIALMENTE EXPRESADOS O NO

fi2$genes <- feature
topTable(fi2)
View(topTable(fi2))
abr<-topTable(fi2)
volcanoplot(fi2, highlight=10, names=fi2$ID)
full_result <- topTable(fi2, number=Inf)
full_result <- tibble::rownames_to_column(full_result)

dT<- decideTests(fi2, adjust.method = "fdr", p.value = 0.05,lfc = 0)#se ve cuantos genes se expresan diferencialmente
Table(dT)
vennDiagram(dT, circle.col = palette())
ct=1
plotMD(fi2, column = ct, status = dT[,ct],legend = F, pch = 20, cex=1)

library(ggplot2)
ggplot(full_result,aes(x = logFC, y=B)) + geom_point()

p_cutoff <- 0.05
fc_cutoff <- 1

full_result %>% 
  mutate(Significant = adj.P.Val < p_cutoff, abs(logFC) > fc_cutoff ) %>% 
  ggplot(aes(x = logFC, y = B, col=Significant)) + geom_point()

#HACERLO CON PVALUE 
library(ggrepel)
p_cutoff <- 0.05
fc_cutoff <- 1
topN <-10

full_results %>% 
  mutate(Significant = adj.P.Val < p_cutoff, abs(logFC) > fc_cutoff ) %>% 
  mutate(Rank = 1:n(), Label = ifelse(Rank < topN, Symbol,"")) %>% 
  ggplot(aes(x = logFC, y = B, col=Significant,label=Label)) + geom_point() + geom_text_repel(col="black")


p_cutoff <- 0.05
fc_cutoff <- 1
filter(full_results, adj.P.Val < 0.05, abs(logFC)> 1) %>% write_csv(path="Processed_data/Filtered_results.csv")

p_cutoff <- 0.05
fc_cutoff <- 1
filter(full_results, adj.P.Val < 0.05, abs(logFC) > 1)

topN<-10
ids_of_interest<- mutate(full_results, Rank = 1:n()) %>% 
  filter(Rank < topN) %>% 
  pull(ID)
gene_names<- mutate(full_results, Rank= 1:n()) %>% 
  filter(Rank < topN) %>% 
  pull(Symbol)
gene_matrix <- exprs(gse)[ids_of_interest,]
pheatmap(gene_matrix,labels_row = gene_names) # con genes de nuestro interes 

pheatmap(gene_matrix,labels_row = gene_names, scale = "row") 
