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
corpus = tm_map(corpus, content_transformer(tolower)) # Esta linha converte todo o texto do corpus para minúsculas. Isso é crucial para que palavras como "Casa" e "casa" sejam tratadas como iguais, evitando redundância e melhorando a precisão da análise.
corpus = tm_map(corpus, removePunctuation, ucp = TRUE) # Remove todos os sinais de pontuação do corpus. Pontuação geralmente não carrega significado semântico em análises de texto e pode interferir em alguns algoritmos. Este conjunto inclui vírgulas, pontos, ponto e vírgula, dois pontos, exclamações, interrogações, aspas, parênteses, colchetes, chaves, hífens, sublinhados e apóstrofos.
corpus = tm_map(corpus, removeNumbers) # Remove todos os números do corpus. Similar à pontuação, números muitas vezes não são relevantes para a análise semântica e podem ser removidos.
corpus = tm_map(corpus, removeWords, stopwords('portuguese')) # Remove as stop words em português do corpus. Stop words são palavras comuns como "a", "e", "o", "que", "de", etc., que geralmente não carregam muito significado semântico e aparecem com alta frequência. Removê-las reduz o tamanho do corpus e melhora a eficiência do processamento, além de potencialmente melhorar a precisão da análise ao focar em palavras mais relevantes.
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
