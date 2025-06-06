

#' ---
#' title: "Analiza sentymentu exposé brytyjskich premierów"
#' author: "Autor: Szymon"
#' date: "`r Sys.Date()`"
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: darkly # Wygląd (bootstrap, cerulean, darkly, journal, lumen, paper, readable, sandstone, simplex, spacelab, united, yeti)
#'     highlight: breezedark # Kolorowanie składni (haddock, kate, espresso, breezedark)
#'     toc: true            # Spis treści
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: hide    # Kod domyślnie zwinięty (estetyczniej)
#'     number_sections: true # Numeruje nagłówki (lepsza nawigacja)
#'     css: "custom.css"     # Możliwość stworzenia własnego stylowania (opcjonalne)
#' ---


knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE
)
# ^ Ukrywa ostrzezenia, ktore nie zatrzymuja dzialania kodu, oraz komunikaty generowane przez funkcje





#' # Wymagane pakiety
# Ladujemy wymagane pakiety ----
library(tm)
library(tidytext)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(SnowballC)
library(SentimentAnalysis)
library(ggthemes)
library(tidyverse)



#' # 0. Funkcja do przetwarzania tekstu z apostrofami, stemmingiem i stemCompletion
# 0. Funkcja do przetwarzania tekstu z apostrofami, stemmingiem i stemCompletion ----

# Funkcja process_text wykonuje wstepne przetwarzanie tekstu
process_text <- function(file_path) {
  text <- tolower(readLines(file_path, encoding = "UTF-8"))
  # ^ Wczytuje tekst jako wektor linii i zamienia litery na male
  text <- gsub("[\u2019\u2018\u0060\u00B4]", "'", text)
  # ^ Standaryzuje rozne typy apostrofow na zwykly
  text <- removeNumbers(text)
  # ^ Usuwa wszystkie cyfry
  words <- unlist(strsplit(text, "\\s+"))
  # ^ Dzieli tekst na pojedyncze slowa wzgledem bialych znakow i konwertuje wynik na wektor
  words <- words[words != ""]
  # ^ Usuwa puste stringi, np po usunieciu cyfr
  words <- words[!str_detect(words, "'")]
  # ^ Usuwa slowa zawierajace apostrof (np don't) aby uproscic analize
  words <- str_replace_all(words, "[[:punct:]]", "")
  # ^ Usuwa znaki interpunkcyjne (np !)
  words <- words[words != ""]
  # ^ Ponownie usuwa puste stringi, np po usunieciu znakow interpunkcyjnych
  words <- str_trim(words)
  # ^ Usuwa biale znaki z poczatku i konca kazdego slowa
  
  tidy_stopwords <- tolower(stop_words$word)
  tidy_stopwords <- gsub("[\u2019\u2018\u0060\u00B4]", "'", tidy_stopwords)
  tm_stopwords <- tolower(stopwords("en"))
  tm_stopwords <- gsub("[\u2019\u2018\u0060\u00B4]", "'", tm_stopwords)
  # ^ Laduje zbiory stopwords, zamienione na male litery i znormalizowane
  
  words <- words[!(words %in% tidy_stopwords)]
  words <- words[!(words %in% tm_stopwords)]
  # ^ Usuwa slowa wystepujace w zbiorach stopwords
  
  # Stemming + stem completion
  stemmed_doc <- stemDocument(words)
  # ^ Sprowadza slowa do "rdzenia"
  completed_doc <- stemCompletion(stemmed_doc, dictionary=words, type="prevalent")
  # ^ Wybiera najczestsza forme slowa w slowniku i przywraca niektore slowa do pelnej formy na podstawie slownika
  completed_doc <- completed_doc[completed_doc != ""]
  # ^ Usuwa puste stringi po stemmingu
  
  return(completed_doc)
  # ^ Zwraca calosc po ukonczeniu procesu
}



#' # 0. Funkcja do obliczania częstości występowania słów
# 0. Funkcja do obliczania częstości występowania słów ----
word_frequency <- function(words) {
  freq <- table(words) 
  # ^ Tworzy tabele czestosci slow
  freq_df <- data.frame(word = names(freq), freq = as.numeric(freq)) 
  # ^ Konwertuje do ramki danych
  freq_df <- freq_df[order(-freq_df$freq), ]
  # ^ Sortuje malejaco po czestosci
  return(freq_df)
  # ^ Zwraca posortowana ramke danych
}



#' # 0. Funkcja do tworzenia chmury słów 
# 0. Funkcja do tworzenia chmury słów ----
plot_wordcloud <- function(freq_df, title, color_palette = "Dark2") {
  wordcloud(words = freq_df$word, freq = freq_df$freq, min.freq = 16,
            colors = brewer.pal(8, color_palette))
  # ^ Tworzymy chmure i wybieramy zbior slow o okreslonej minimalnej czestosci
  # ^ Ponadto, wybieramy palete kolorow
  title(title) # Nadajemy chmurze tytul
}




#' # ANALIZA TEXT MINING
# ANALIZA TEXT MINING ----


#' #### 📌 Przetwarzanie i oczyszczanie tekstu <br>*(Text Preprocessing and Text Cleaning)*
#'
#' - wczytanie tekstu z odpowiednim kodowaniem (UTF-8)
#' - normalizacja (ujednolicenie) wielkości liter (zamiana na małe litery = lowercase)
#' - normalizacja (ujednolicenie) rozbieżnych kodowań znaków (apostrofy, cudzysłowy)
#' - normalizacja (ujednolicenie) form skróconych (I'm, I've, don't) przez usunięcie lub rozwinięcie
#' - normalizacja (ujednolicenie) różnych akcentów ("café" na "cafe") przez usunięcie akcentów
#' - normalizacja (ujednolicenie) popularnych skrótów ("btw" na "by the way", "b4" na "before") przez rozwinięcie
#' - usunięcie zbędnych ciągów znaków (adresy URL, tagi HTML)
#' - usunięcie zbędnych znaków specjalnych (*, &, #, @, $)
#' - usunięcie zbędnych białych znaków (spacja, tabulacja, znak przejścia do nowej linii "enter")
#' - usunięcie cyfr i liczb
#' - usunięcie interpunkcji
#' - tokenizacja (podział tekstu na słowa = tokeny)
#' - usunięcie stopwords (słów o małej wartości semantycznej, np. "the", "and")
#' - usunięcie pustych elementów (rozważenie problemu brakujących/niekompletnych danych )
#' - stemming lub lematyzacja (sprowadzenie słów do ich rdzenia/formy podstawowej)
#'


#' #### 📌 Zliczanie częstości słów <br>*(Word Frequency Count)*
#'

#' #### 📌 Eksploracyjna analiza danych: <br>wizualizacja częstości słów (tabela, wykres, chmura słów) <br>*(Exploratory Data Analysis, EDA)*
#'

#' #### 📌 Inżynieria cech w modelu Bag of Words: <br>reprezentacja tekstu jako zbioru słów i częstości słów ( = cechy) <br>*(Feature Engineering in BoW model)*
#'



# Wczytanie i przetworzenie tekstu ----
# file_path <- "sciezka/do/pliku.txt"  <= Uzupełnij nazwę pliku i ustaw Working Directory!
#file_path <- "Trump2025.txt"
#words <- process_text(file_path)

# Wczytanie i przetworzenie tekstu ----
file_path <- file.choose()  
# ^ Otwiera okno dialogowe do wyboru pliku przez uzytkownika
words <- process_text(file_path)


custom_stopwords <- c("$")
words <- words[!words %in% custom_stopwords]
# ^ Definiujemy dodatkowe niestandardowe stopwords i usuwamy je


freq_df <- word_frequency(words)
# ^ Przypisujemy czestosc slow


plot_wordcloud(freq_df, "Chmura słów", "Dark2")
# ^ Tworzymy chmure slow

print(head(freq_df, 10))
# ^ Wyswietlamy top 10 najczestszych slow




#' # Analiza sentymentu - słowniki CSV
# Analiza sentymentu - słowniki CSV ----



#' # Wczytaj słowniki z plików csv
# Wczytujemy slowniki z plikow csv i przypisujemy je do ramek danych
afinn <- read.csv("afinn.csv", stringsAsFactors = FALSE)
bing <- read.csv("bing.csv", stringsAsFactors = FALSE)
loughran <- read.csv("loughran.csv", stringsAsFactors = FALSE)
nrc <- read.csv("nrc.csv", stringsAsFactors = FALSE)


tidy_tokeny <- as_tibble(freq_df)
# ^ Ramke danych z czestoscia slow konwertujemy do formatu tibble



#' # Analiza sentymentu przy użyciu słownika Loughran
# Analiza sentymentu przy użyciu słownika Loughran ----


tidy_tokeny %>%
  inner_join(loughran, relationship = "many-to-many")
# ^ Funkcja inner_join() laczy nasze slowa ze slownikiem Loughran
# Zachowujemy tylko slowa obecne w slowniku, zatem ich liczba spada


# Zliczanie sentymentu
sentiment_review <- tidy_tokeny %>%
  inner_join(loughran, relationship = "many-to-many")

sentiment_review %>%
  count(sentiment)
# ^ Zlicza liczbe wystapien dla kazdej kategorii sentymentu


sentiment_review %>%
  group_by(sentiment) %>%
  arrange(desc(freq)) %>%
  ungroup()
# ^ Grupuje slowa wedlug kategorii sentymentu
# i sortuje je malejaco wedlug czestosci wystepowania

# Filtrowanie analizy sentymentu
# i pozostawienie tylko słów
# o sentymencie pozytywnym lub negatywnym

sentiment_review2 <- sentiment_review %>%
  filter(sentiment %in% c("positive", "negative"))
# ^ Wybiera jedynie pozytywne lub negatywne slowa, pomijajac pozostale


word_counts <- sentiment_review2 %>%
  group_by(sentiment) %>%
  top_n(20, freq) %>%
  ungroup() %>%
  arrange(desc(freq), word) %>%
  mutate(
    word2 = factor(word, levels = rev(unique(word)))
  )
# ^ Wybiera 20 najczestszych slow dla kazdego sentymentu
# i przygotowuje czynnik (factor) do prawidlowego sortowania na wykresie

# Wizualizacja sentymentu
ggplot(word_counts[1:30,], aes(x=word2, y=freq, fill=sentiment)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free") +
  coord_flip() +
  labs(x = "Słowa", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("Liczba słów wg sentymentu (Loughran)") +
  scale_fill_manual(values = c("firebrick", "darkolivegreen4"))
# ^ Istnieje mozliwosc modyfikacji aspektow wizualnych powstalego wykresu


#' # Analiza sentymentu przy użyciu słownika NRC
# Analiza sentymentu przy użyciu słownika NRC ----


sentiment_review_nrc <- tidy_tokeny %>%
  inner_join(nrc, relationship = "many-to-many")
# ^ Funkcja inner_join() laczy nasze slowa ze slownikiem NRC
# Zachowujemy tylko slowa obecne w slowniku, zatem ich liczba spada

sentiment_review_nrc %>%
  count(sentiment)
# ^ Zlicza wystepowanie kazdej kategorii emocji


sentiment_review_nrc %>%
  group_by(sentiment) %>%
  arrange(desc(freq)) %>%
  ungroup()
# ^ Grupuje slowa przypisanych emocji
# Sortowanie malejace wedlug czestotliwosci wystepowania
# Umozliwia identyfikacje najbardziej charakterystycznych slow dla kazdej emocji


sentiment_review_nrc2 <- sentiment_review_nrc %>%
  filter(sentiment %in% c("positive", "negative"))
# ^ Wyodrebnienie slow o sentymencie wylacznie pozytywnym lub negatywnym
# Pomija emocje szczegolowe


word_counts_nrc2 <- sentiment_review_nrc2 %>%
  group_by(sentiment) %>%
  top_n(20, freq) %>%
  ungroup() %>%
  arrange(desc(freq), word) %>%
  mutate(
    word2 = factor(word, levels = rev(unique(word)))
  )
# ^ Wybiera 20 najczestszych slow dla kazdego sentymentu
# i przygotowuje czynnik (factor) do prawidlowego sortowania na wykresie

# Wizualizacja sentymentu
ggplot(word_counts_nrc2[1:30,], aes(x=word2, y=freq, fill=sentiment)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free") +
  coord_flip() +
  labs(x = "Słowa", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("Liczba słów wg sentymentu (NRC)")
# ^ Istnieje mozliwosc modyfikacji aspektow wizualnych powstalego wykresu



#' # Analiza sentymentu przy użyciu słownika Bing
# Analiza sentymentu przy użyciu słownika Bing ----


sentiment_review_bing <- tidy_tokeny %>%
  inner_join(bing)
# ^ Funkcja inner_join() laczy nasze slowa ze slownikiem Loughran
# Zachowujemy tylko slowa obecne w slowniku, zatem ich liczba spada

sentiment_review_bing %>%
  count(sentiment)
# ^ Zlicza wystepowanie kazdej kategorii emocji (tu tylko positive/negative)


sentiment_review_bing %>%
  group_by(sentiment) %>%
  arrange(desc(freq)) %>%
  ungroup()
# ^ Grupuje slowa przypisanych emocji
# Sortowanie malejace wedlug czestotliwosci wystepowania
# Umozliwia identyfikacje najbardziej charakterystycznych slow dla kazdej emocji


sentiment_review_bing2 <- sentiment_review_bing %>%
  filter(sentiment %in% c("positive", "negative"))
# ^ Wyodrebnienie slow o sentymencie wylacznie pozytywnym lub negatywnym
# Bing i tak zawiera tylko dwie

word_counts_bing2 <- sentiment_review_bing2 %>%
  group_by(sentiment) %>%
  top_n(20, freq) %>%
  ungroup() %>%
  arrange(desc(freq), word) %>%
  mutate(
    word2 = factor(word, levels = rev(unique(word)))
  )
# ^ Wybiera 20 najczestszych slow dla kazdego sentymentu
# i przygotowuje czynnik (factor) do prawidlowego sortowania na wykresie

# Wizualizacja sentymentu
ggplot(word_counts_bing2[1:30,], aes(x=word2, y=freq, fill=sentiment)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free") +
  coord_flip() +
  labs(x = "Słowa", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("Liczba słów wg sentymentu (Bing)") +
  scale_fill_manual(values = c("dodgerblue4", "goldenrod1"))
# ^ Istnieje mozliwosc modyfikacji aspektow wizualnych powstalego wykresu



#' # Analiza sentymentu przy użyciu słownika Afinn
# Analiza sentymentu przy użyciu słownika Afinn ----


sentiment_review_afinn <- tidy_tokeny %>%
  inner_join(afinn)
# ^ Funkcja inner_join() laczy nasze slowa ze slownikiem Loughran
# Zachowujemy tylko slowa obecne w slowniku, zatem ich liczba spada
# Afinn przypisuje slowom wartosci od -5 do 5

sentiment_review_afinn %>%
  count(value)
# ^ Zlicza wystepowanie kazdej kategorii emocji 


sentiment_review_afinn %>%
  group_by(value) %>%
  arrange(desc(freq)) %>%
  ungroup()
# ^ Grupuje slowa przypisanych emocji
# Sortowanie malejace wedlug czestotliwosci wystepowania
# Umozliwia identyfikacje najbardziej charakterystycznych slow dla kazdej emocji


sentiment_review_afinn3 <- sentiment_review_afinn %>%
  filter(value %in% c("3", "-3" , "4", "-4", "5", "-5"))
#^ Eliminacja slow o neutralnym lub slabym sentymencie
# Wyodrebniamy slowa o silnym ladunku emocjonalnym


word_counts_afinn3 <- sentiment_review_afinn3 %>%
  group_by(value) %>%
  top_n(20, freq) %>%
  ungroup() %>%
  arrange(desc(freq), word) %>%
  mutate(
    word2 = factor(word, levels = rev(unique(word)))
  )
# ^ Wybiera 20 najczestszych slow dla kazdego sentymentu
# i przygotowuje czynnik (factor) do prawidlowego sortowania na wykresie

# Wizualizacja sentymentu
ggplot(word_counts_afinn3[1:30,], aes(x=word2, y=freq, fill=value)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~value, scales="free") +
  coord_flip() +
  labs(x = "Słowa", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("Liczba słów wg sentymentu (AFINN)")
# ^ Istnieje mozliwosc modyfikacji aspektow wizualnych powstalego wykresu




#' # Analiza sentymentu w czasie o ustalonej długości linii
# Analiza sentymentu w czasie o ustalonej długości linii ----



full_text <- paste(words, collapse = " ")
#^ Laczy (spacjami) wszystkie slowa w jeden ciag znakow


split_text_into_chunks <- function(text, chunk_size) {
  start_positions <- seq(1, nchar(text), by = chunk_size)
  chunks <- substring(text, start_positions, start_positions + chunk_size - 1)
  return(chunks)
}
#^ Funkcja dzieli tekst na segmenty o okreslonej dlugosci chunk_size


set_length <- 50
text_chunks <- split_text_into_chunks(full_text, set_length)
#^ Dzielimy tekst na rowne fragmenty (ostatni moze byc krotszy)


# Wyświetlenie wynikowych segmentów
# print(text_chunks)
# ^ Sluzy do debugowania
# Zakomentowane, by nie zasmiecac wynikow


#' # Analiza sentymentu przy użyciu pakietu SentimentAnalysis
# Analiza sentymentu przy użyciu pakietu SentimentAnalysis ----
sentiment <- analyzeSentiment(text_chunks)
#^ Przetwarza segmenty z uzyciem czterech ponizszych slownikow



#' # Słownik GI (General Inquirer)
### Słownik GI (General Inquirer) ----
#
# Słownik ogólnego zastosowania
# zawiera listę słów pozytywnych i negatywnych
# zgodnych z psychologicznym słownikiem harwardzkim Harvard IV-4
# DictionaryGI


# Wczytaj słownik GI
# data(DictionaryGI)
# summary(DictionaryGI)


sentimentGI <- convertToDirection(sentiment$SentimentGI)
# ^ Konwersja ciągłych wartości sentymentu 
# na odpowiadające im wartości kierunkowe zgodnie ze słownikiem GI


#plot(sentimentGI)
#^ Podstawowy, zakomentowany wykres skumulowanego sentymentu kierunkowego


# Ten sam wykres w ggplot2:
df_GI <- data.frame(index = seq_along(sentimentGI), value = sentimentGI, Dictionary = "GI")
#^ Konwersja do ramki danych (struktura wymagana przez ggplot2)

df_GI <- na.omit(df_GI)
#^ Usuwa brakujace wartosci (wazne dla stabilnosci wykresow)


# Wizualizacja kierunkow sentymentu:
ggplot(df_GI, aes(x = value)) +
  geom_bar(fill = "green", alpha = 0.7) + 
  labs(title = "Skumulowany sentyment (GI)",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw()
# ^ Istnieje mozliwosc modyfikacji aspektow wizualnych powstalego wykresu



#' # Słownik HE (Henry’s Financial dictionary)
### Słownik HE (Henry’s Financial dictionary) ----
#
# zawiera listę słów pozytywnych i negatywnych
# zgodnych z finansowym słownikiem "Henry 2008"
# pierwszy, jaki powstał w wyniku analizy komunikatów prasowych 
# dotyczących zysków w branży telekomunikacyjnej i usług IT
# DictionaryHE


# Wczytaj słownik HE
# data(DictionaryHE)
# summary(DictionaryHE)


sentimentHE <- convertToDirection(sentiment$SentimentHE)
# ^ Konwersja ciągłych wartości sentymentu 
# na odpowiadające im wartości kierunkowe zgodnie ze słownikiem HE


# plot(sentimentHE)
#^ Podstawowy, zakomentowany wykres skumulowanego sentymentu kierunkowego


# Ten sam wykres w ggplot2:
df_HE <- data.frame(index = seq_along(sentimentHE), value = sentimentHE, Dictionary = "HE")
#^ Konwersja do ramki danych (struktura wymagana przez ggplot2)

df_HE <- na.omit(df_HE)
#^ Usuwa brakujace wartosci (wazne dla stabilnosci wykresow)

# Wizualizacja kierunkow sentymentu:
ggplot(df_HE, aes(x = value)) +
  geom_bar(fill = "blue", alpha = 0.7) + 
  labs(title = "Skumulowany sentyment (HE)",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw()
# ^ Istnieje mozliwosc modyfikacji aspektow wizualnych powstalego wykresu



#' # Słownik LM (Loughran-McDonald Financial dictionary)
### Słownik LM (Loughran-McDonald Financial dictionary) ----
#
# zawiera listę słów pozytywnych i negatywnych oraz związanych z niepewnością
# zgodnych z finansowym słownikiem Loughran-McDonald
# DictionaryLM


# Wczytaj słownik LM
# data(DictionaryLM)
# summary(DictionaryLM)


sentimentLM <- convertToDirection(sentiment$SentimentLM)
# ^ Konwersja ciągłych wartości sentymentu 
# na odpowiadające im wartości kierunkowe zgodnie ze słownikiem LM


# plot(sentimentLM)
#^ Podstawowy, zakomentowany wykres skumulowanego sentymentu kierunkowego


# Ten sam wykres w ggplot2:
df_LM <- data.frame(index = seq_along(sentimentLM), value = sentimentLM, Dictionary = "LM")
#^ Konwersja do ramki danych (struktura wymagana przez ggplot2)

df_LM <- na.omit(df_LM)
#^ Usuwa brakujace wartosci (wazne dla stabilnosci wykresow)

# Wizualizacja kierunkow sentymentu:
ggplot(df_LM, aes(x = value)) +
  geom_bar(fill = "orange", alpha = 0.7) + 
  labs(title = "Skumulowany sentyment (LM)",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw()
# ^ Istnieje mozliwosc modyfikacji aspektow wizualnych powstalego wykresu



#' # Słownik QDAP (Quantitative Discourse Analysis Package)
### Słownik QDAP (Quantitative Discourse Analysis Package) ----
#
# zawiera listę słów pozytywnych i negatywnych
# do analizy dyskursu


# Wczytaj słownik QDAP
qdap <- loadDictionaryQDAP()
# QDAP laduje sie inaczej od innych slownikow
# summary(qdap)


sentimentQDAP <- convertToDirection(sentiment$SentimentQDAP)
# ^ Konwersja ciągłych wartości sentymentu 
# na odpowiadające im wartości kierunkowe zgodnie ze słownikiem QDAP


# plot(sentimentQDAP)
#^ Podstawowy, zakomentowany wykres skumulowanego sentymentu kierunkowego


# Ten sam wykres w ggplot2:
df_QDAP <- data.frame(index = seq_along(sentimentQDAP), value = sentimentQDAP, Dictionary = "QDAP")
#^ Konwersja do ramki danych (struktura wymagana przez ggplot2)

df_QDAP <- na.omit(df_QDAP)
#^ Usuwa brakujace wartosci (wazne dla stabilnosci wykresow)

# Wizualizacja kierunkow sentymentu:
ggplot(df_QDAP, aes(x = value)) +
  geom_bar(fill = "red", alpha = 0.7) + 
  labs(title = "Skumulowany sentyment (QDAP)",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw()
# ^ Istnieje mozliwosc modyfikacji aspektow wizualnych powstalego wykresu



#' # Porównanie sentymentu na podstawie różnych słowników
# Porównanie sentymentu na podstawie różnych słowników ----

# Minimalistycznie
# plot(convertToDirection(sentiment$SentimentGI))
# plot(convertToDirection(sentiment$SentimentHE))
# plot(convertToDirection(sentiment$SentimentLM))
# plot(convertToDirection(sentiment$SentimentQDAP))
#^ Podstawowe, zakomentowane wykresy


# Wizualnie lepsze w ggplot2
df_all <- bind_rows(df_GI, df_HE, df_LM, df_QDAP)
#^ Funkcja laczy poszczegolne ramki w jedna ramke danych
# Nowa ramka zawiera wszystkie wyniki

# Wizualizacja skumulowanego sentymentu dla wszystkich slownikow:
ggplot(df_all, aes(x = value, fill = Dictionary)) +
  geom_bar(alpha = 0.7) + 
  labs(title = "Skumulowany sentyment według słowników",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw() +
  facet_wrap(~Dictionary) +  # Podział na cztery osobne wykresy
  scale_fill_manual(values = c("GI" = "green", 
                               "HE" = "blue", 
                               "LM" = "orange",
                               "QDAP" = "red" ))





#' # Agregowanie sentymentu z różnych słowników w czasie
# Agregowanie sentymentu z różnych słowników w czasie ----


length(sentiment[,1])
# ^ Sprawdzenie ilosci obserwacji


# Tworzymy ramke danych:
df_all <- data.frame(sentence=1:length(sentiment[,1]),
                     GI=sentiment$SentimentGI, 
                     HE=sentiment$SentimentHE, 
                     LM=sentiment$SentimentLM,
                     QDAP=sentiment$SentimentQDAP)
# Sentence to numery kolejnych segmentow tekstu
# Skladowa ramki sa wartosci sentymentow z poszczegolnych slownikow



# USUNIĘCIE BRAKUJĄCYCH WARTOŚCI
# gdyż wartości NA (puste) uniemożliwiają generowanie wykresu w ggplot
#

puste <- df_all[!complete.cases(df_all), ]
#^ Usuwamy wiersze, ktore sa niekompletne


df_all <- df_all[!is.na(df_all$QDAP), ]
#^ Usuwamy wiersze z brakujacymi wartosciami QDAP


puste2 <- df_all[!complete.cases(df_all), ]
puste2
#^ Ponownie sprawdzamy, czy usunelismy wszystkie NA
# Wowczas puste2 ma 0 wierszy




#' # Wykresy przedstawiające ewolucję sentymentu w czasie
# Wykresy przedstawiające ewolucję sentymentu w czasie ----



ggplot(df_all, aes(x=sentence, y=QDAP)) +
  geom_line(color="red", size=1) +
  geom_line(aes(x=sentence, y=GI), color="green", size=1) +
  geom_line(aes(x=sentence, y=HE), color="blue", size=1) +
  geom_line(aes(x=sentence, y=LM), color="orange", size=1) +
  labs(x = "Oś czasu zdań", y = "Sentyment") +
  theme_gdocs() + 
  ggtitle("Zmiana sentymentu w czasie")
#^ Wersja z danymi surowymi 
# Pokazuje rzeczywiste wartosci sentymentu dla kazdego segmentu



ggplot(df_all, aes(x=sentence, y=QDAP)) + 
  geom_smooth(color="red") +
  geom_smooth(aes(x=sentence, y=GI), color="green") +
  geom_smooth(aes(x=sentence, y=HE), color="blue") +
  geom_smooth(aes(x=sentence, y=LM), color="orange") +
  labs(x = "Oś czasu zdań", y = "Sentyment") +
  theme_gdocs() + 
  ggtitle("Zmiana sentymentu w czasie")
#^ Wersja wygladzona
# Pokazuje wygladzony trend uzyskany z pomoca regresji

# Kolorystyka ww wykresow domyslnie jest spojna z poprzednimi
# W razie potrzeby istnieje mozliwosc modyfikacji aspektow wizualnych





