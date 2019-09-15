 #dependencias
 Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
 install.packages(Needed, dependencies=TRUE)   
 install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")
 
 	#leitura
  setwd("~/Desktop/texts")
 	empenho <- read.csv2("tcc.csv", encoding = "latin1")
 	source("sinonimos.R")
 	View(empenho)
 	
 	library(dplyr)
 	library(tm)
 	library(stringr)
 	library(stringi)
 	library(SnowballC)
 	library(ggplot2)
 	library(wordcloud)
 	library(cluster)
 	library(fpc)
 	
 	# mineracao de orgao
 	descricoes = select(empenho, OrgaoNome, EmpenhoDescricao)
 	View(descricoes)
 	orgaos = group_by(descricoes, OrgaoNome) %>% summarise(total = n())
 	View(orgaos)
 	ggplot(orgaos,aes(x = total, y = OrgaoNome, col = OrgaoNome)) + geom_point() #quantidade

 	#mineracao de descricao
 	desc = select(empenho, EmpenhoDescricao)
 	View(desc)
 	write.table(desc, file="descricao.txt", quote=FALSE, sep=" ", row.names=FALSE)
 	descricao = readLines("descricao.txt")
  
  #tratamento
  docs <- Corpus(VectorSource(descricao))
  inspect(docs[191720])
  replaceSynonyms <- content_transformer(function(x, syn=NULL) { 
    Reduce(function(a,b) {
      gsub(paste0("\\b(", paste(b$syns, collapse="|"),")\\b"), b$word, a)}, syn, x)   
  })
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, tolower)
  docs <- tm_map(docs, removeWords, stopwords("portuguese"))
  docs <- tm_map(docs, stri_trans_general, "Latin-ASCII")
  docs <- tm_map(docs, replaceSynonyms, synonyms)
  docs <- tm_map(docs, removeWords, my_stopwords)
  docs <- tm_map(docs, stripWhitespace)
  
  # apresenta os dados
  ndocs <- length(docs)
  MIN_TERM_FREQ = 0.01
  MAX_TERM_FREQ = .9
  minTermFreq <- ndocs * MIN_TERM_FREQ
  maxTermFreq <- ndocs * MAX_TERM_FREQ
  dtm <- DocumentTermMatrix(docs, 
          control=list(wordLengths=c(4, 20),
          bounds = list(global = c(minTermFreq,maxTermFreq))))
  dtm
  dtms <- removeSparseTerms(dtm, 0.1)
  dtms
  df.dtm = data.frame(as.matrix(dtm))
  View(df.dtm)
  
  #explora os dados
  freq <- colSums(as.matrix(dtm)) # precisa de muita memoria
  length(freq)
  ord <- order(freq) 
  
  #frequencia
  freq[head(ord)]
  freq[tail(ord)]
  head(table(freq), 5)
  freq <- colSums(as.matrix(dtms))
  freq
  freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
  head(freq, 5)
  findFreqTerms(dtm, lowfreq=1200)
  wf <- data.frame(word=names(freq), freq=freq)
  head(wf)
 
  #plot
  p <- ggplot(subset(wf, freq>2000), aes(word, freq))
  p <- p + geom_bar(stat="identity")
  p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
  p
  
  set.seed(42)
  wordcloud(names(freq), freq, max.words=50)
  wordcloud(names(freq), freq, min.freq=100, scale=c(4, .2), colors=brewer.pal(5, "Dark2"))
  
  #concatenacao de tabelas
  orgao_simples = select(empenho, OrgaoNome)
  novatabela <- cbind(orgao_simples,df.dtm)
  View(novatabela)
  novatabela_porpalavra = novatabela %>% group_by(OrgaoNome) %>% summarise_all(list(sum))
  View(novatabela_porpalavra)
  kmedia = novatabela_porpalavra[,2:15]
  View(kmedia)
  
  #clustering de palavras (trocar para orgaos)
  d <- dist(t(kmedia), method="euclidian") 
  fit <- hclust(d=d, method="ward.D")   
  fit
  plot(fit, hang=-1)
  
  cluster_n = 3
  
  kfit <- kmeans(kmedia, cluster_n)
  clusplot(as.matrix(kmedia), kfit$cluster, color=T, shade=T, labels=3, lines=0)   
  
  #concatenacao de tabelas
  novatabela_cluster <- cbind(novatabela_porpalavra,kfit$cluster)
  View(novatabela_cluster)
  
  #exportacao para csv
  cluster <- as.matrix(novatabela_cluster)   
  dim(cluster)   
  write.csv(cluster, file="cluster.csv")   
  
  #label
  aux <- novatabela_cluster[,2:16]
  View(aux)
  label = aux %>% group_by(`kfit$cluster`) %>% summarise_all(list(sum))
  View(label)  
  dim(label)
  write.csv(label, file="label.csv")
  
  for(group in 1:cluster_n){
    aux_group <- label[group,]
    result <- aux_group[which.max(aux_group)]
    print(paste("cluster ",group, "de", cluster_n))
    print (result)
    group = group+1
  }
  
  