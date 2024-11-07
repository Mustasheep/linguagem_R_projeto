library(tidyverse)
library(pdftools)
library(stringr)
library(NLP)
library(tm)
library(RColorBrewer)


arquivo <- ("C:/Users/Thiago/Documents/demo/O alienista.pdf")
txt = pdf_text(arquivo)
cat(txt)

valor = ""

# Concatenar os elementos não nulos
for (i in 1:length(txt)){
  if(!is.null(txt[i])) {
    valor = paste(valor, txt[i], sep = "\n")
  }
}
valor

# Dividindo a string
tabelatxt = valor %>% str_split("\n") %>% .[[1]]
print(tabelatxt)

# Transfarmar em data frame e preparar os dados
dtTrabalho = tabelatxt %>% as_data_frame() %>% rename(Linhas = value)
dtTrabalho %<>% select(Linhas) %>% filter(!Linhas==" ")
dtTrabalho

# Transformar texto em Corpus
corpus = Corpus(VectorSource(dtTrabalho))
length(corpus)
inspect(corpus)

# Minerando o corpus
corpus = tm_map(corpus, content_transformer(tolower)) # Esta linha converte todo o texto do corpus para minúsculas.
corpus = tm_map(corpus, removePunctuation, ucp = TRUE) # Remove todos os sinais de pontuação do corpus.
corpus = tm_map(corpus, removeNumbers) # Remove todos os números do corpus.
corpus = tm_map(corpus, removeWords, stopwords('portuguese')) # Remove as stop words em português do corpus.
corpus = tm_map(corpus, content_transformer(function(x) gsub("\n", "", x)))

# Representação por frequencia de termos
tf = TermDocumentMatrix(corpus, control = list(minWordLength=1, minDocFreq=2))
length(tf)
inspect(tf)

# Convertendo: termo-documento -> matriz
m = as.matrix(tf)

# calcula frequencia
v = sort(rowSums(m), decreasing=TRUE)
myNames = names(v)
k = which(names(v)=="miners")
myNames[k] = "mining"
d = data.frame(word=myNames, freq=v)

# Gráfico
palavra <- d$word
frequencia <- d$freq
bd <- data.frame(palavra,frequencia)
gr <- bd[1:10,]

graficoFreq = ggplot(gr, aes(x=palavra, y=frequencia, fill=palavra))+
  geom_bar(stat = "identity")+ guides(fill=FALSE)
graficoFreq
