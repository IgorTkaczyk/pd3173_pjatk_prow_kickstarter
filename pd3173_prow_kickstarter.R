# Igor Tkaczyk - pd3173
# PJATK - Studia Podyplomowe Big Data
# Projekt własny

rm(list=ls())

# Ładowanie bibliotek
library(tidyverse)
library(ggthemes)
library(plotly)
library(GGally)
library(tibble)
library(lubridate)
library(ggpubr)


# Pobrania danych
kick <- read.csv('ks-projects-201801.csv')


# Zmiana opcji wyświetlania zmiennych numerycznych tak aby nie wyświetlały się w fomie exponent
options("scipen" = 100, "digits" = 4)


# Zmienne i ich typy w zbiorze:
str(kick)


# Opis zmiennych:
#  name - nazwa projektu
#  category - kategoria projektu 
#  main_category - kategoria główna projektu
#  currency - waluta
#  deadline - data zakończenia zbiórki
#  goal - cel finansowy projektu
#  launched - data rozpoczęcia zbiórki
#  pledged - kwota zebrana (w walucie państwa, gdzie przeprowadzono zbiórkę)
#  backers - ilość sponsorów
#  country - kraj przeprowadzenia zbiórki
#  usd pledged - kwota zebrana w USD
#  usd_pledged_real - kwota zebrana w USD po korekcie
#  usd_goal_real - cel finansowy w USD
#  state - status kampanii - przyszła binarna zmienna objaśniana mówiąca o tym czy kampania zakończyła się sukcesem czy porażką


# Sprawdzenie pierwszym wierszy zbioru danych
head(kick)


# usunięcie zduplikowanych wierszy
kick <- distinct(kick)



# Konwersja typów niektórych zmiennych na kategoryczne lub tekstowe
kick$ID <- as.character(kick$ID)
kick$state <- as.factor(kick$state)
kick$category <- as.factor(kick$category)
kick$main_category <- as.factor(kick$main_category)
kick$country <- as.factor(kick$country)
kick$currency <- as.factor(kick$currency)


# W kolumnie 'launched' nie jest nam potrzebna dokładna godzina startu projektu, więc zachowamy jedynie format daty: rok-miesiąc-dzień 
# Jednocześnie zmienne 'launched' i 'deadline' zostaną przekonwertowane ze zmiennych tekstowych na zmienne daty
kick$launched <- str_sub(kick$launched , 1 , 10) %>% 
  ymd()

kick$deadline <- kick$deadline  %>% 
  ymd()


# Upewnienie sie co do typów zmiennych po zmianach:
str(kick)


# Sprawdzenie statystyk dla poszczególnych zmiennych:
summary(kick)


# Spostrzeżenia:
#---------------
# 3797 wartości NA w kolumnie 'usd.pledged'

# dla zmiennej 'country' występuje kod : 'N,O"' , który nie wiadomo jakiemu odpowiada krajowi

# jednocześnie kod 'N,O"' występuje dla 3797 obserwacji co może sugerować że pojawia się dla tych samych obserwacji gdzie brakuje wartości w kolumnie 'usd.pledged'

# dla zmiennej 'state' występują 3562 rekordy z wartością 'undefined' - taka wartość nie pozwala nam na przyporządkowanie wartości 1 lub 0 w kontekście końcowego sukcesu kampanii, dlatego te obserwacje zostaną usunięte ze zbioru

# jednocześnie po dokładniejszym przejrzeniu zbioru podjęto decyzję o usunięciu kolumny 'usd.pledged', gdyż to samo wyraża kolumna 'usd_pledged_real' z tymże dla tej pierwszej zauważalne są przypadki gdzie

# 159 unikalnych wartości dla zmiennej 'category' to za dużo w kwestii wydajnej wizualizacji danych oraz budowy modelu klasyfikującego dlatego podjęto decyzję o usunięcie zmiennej category skontentrowaniu się jedynie na głównych 15 kategoriach tematycznych

# kolumna ID nie wnosi żadnej wartościowej informacji poza numerem identyfikacyjnym danego projektu, dlatego zostanie usunięta

# dla zmiennej 'launched' minimalna wartość to 1970-01-01 -> z pewnością do sprawdzenia




# przyjrzyjmy się danym o podejrzanych wartościach
kick %>%
  filter(state == "undefined" | country == 'N,0"') %>%
  select(-c(ID,name)) %>%
  head(20)


# usunięcie ze zbioru wszystkich obserwacji gdzie: country = 'N,O"' lub state = 'undefined'
# usunięcie kolumn: ID, category, usd.pledged - nie będą przydatne zarówno na wstępnej analizy jak i budowy modeli w Azure ML Studio
kick <- kick %>%
  filter(!state == "undefined" | !country == 'N,0"') %>% # równoważne z:  kick <- kick[complete.cases(kick),]
  select(-c(usd.pledged, category, ID)) 



# Stworzenie zmiennej 'duration' mówiącej o długości kampanii w dniach, która może być wykorzystana w modelu klasyfikującym jako zmienna niezależna

kick <- kick %>% 
  mutate(duration = (as.numeric(deadline) - as.numeric(launched)))

# przyjrzenie się rekordom z podejrzaną datą w kolumnie 'launched'
# filtruję po długości kampanii większej niż 92 dni bo odpowiada to maksymalnej długości kampanii na platformie Kickstarter równej 3 miesiące

kick %>% 
  filter(duration > 92) %>%
  select(c(launched, deadline, duration))

# w zbiorze znajduje się 7 rekordów z datą '1970-01-01' w kolumnie 'launched' 
# podjęto decyzję o nadpisaniu tych wartości poprzez wykorzystanie mediany ddługości trwania projektów i obliczenie nowej wartości dla kolumny 'launched' poprzez odjęcie mediany długości kampanii od daty zakończenia zbiórki

kick$launched[kick$duration > 92] <- kick$deadline[kick$duration > 92] - median(kick$duration)
kick$duration[kick$duration > 92] <- median(kick$duration)


# na potrzeby analizy stworzenie zmiennej "year" i "month"- wzięto pod uwage datę zakończenia zbiórki

kick$year <- as.factor(year(kick$deadline))
kick$month <- as.factor(month(kick$deadline))

# stworzenie zmiennej liczącej długośc tytułu kampanii - będzie wykorzystana przy budowie klasyfikatorów
kick$title_lenght <- as.numeric(nchar(kick$name))

# Stworzenie na potrzeby części z analizą i wizualizacją danych zmiennej 'goal_percentage' mówiącej w jakim procencie zrealizowany cel finansowany projektu
kick <- kick %>% 
  mutate(goal_percentage = as.numeric(usd_pledged_real / usd_goal_real * 100))

# kick <- kick %>% 
#   mutate(goal_percentage = round(as.numeric(usd_pledged_real / usd_goal_real * 100),digits = 4))


# kick$goal_percentage <- round(kick$goal_percentage, digits = 4)
# jeszcze raz sprawdzenie typów i statystyk zmiennych po zmianach
str(kick)
summary(kick)



# sprawdzenie czy dla wszystkcih rekordów w kolumnie 'state' zawsze poprawnie przypisano status kampanii ze względu na procent zebranego celu finansowego

kick %>%
  filter(goal_percentage <= 99.9 & state == 'successful') # 2 rekordy

# nadpisanie 
kick$state[kick$goal_percentage <= 99.9 & kick$state == 'successful'] <- 'failed'


kick %>%
  filter(goal_percentage >= 100 & state == 'failed') # 6 rekordów

# nadpisanie 
kick$state[kick$goal_percentage >= 100 & kick$state == 'failed'] <- 'successful'



# suma zebranych pieniędzy w mld
sum(kick$usd_pledged_real) / 1000000000



# WIZUALIZACJA -------------------------------------------------------------------------------------


# 1 ----------------------------------------------
# Rozkład projektów ze względu na status kampanii

kick %>% group_by(state) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = reorder(state , desc(count)) , y = count , fill = state , label = count)) + 
  geom_col() + 
  ggtitle("Number of projects by Status") + 
  xlab("") +
  ylab("Projects") + 
  geom_text(size = 4, vjust = -0.5) + 
  theme_stata() +
  scale_fill_brewer(palette = "Set1")+
  theme(legend.position = 'none',
        plot.title = element_text(face = 'bold', hjust = 0.5,), 
        axis.title.y = element_text(size = 12, face = 'bold', vjust = 5),
        axis.text.x = element_text(size = 12 , face = 'bold'),
        axis.text.y = element_text(size = 10))


# ggsave('pic1.jpg')
# Obliczenie  % jaki przypada każdej grupie projektów ze względu na status kampanii
kick %>% group_by(state) %>% 
         summarise(count = n(),
                   percentage = paste(round((count / nrow(kick)) * 100 , 1) , '%' , sep = ''))



# 2 --------------------------------------------------------
# Kwota pienięzna uzbierana wg kategorii tematycznych projektóww

 money.by.cat <- kick %>%
  group_by(main_category) %>%
  summarize(suma = sum(usd_pledged_real)) %>%
  arrange(desc(suma))


money.by.cat$main_category <- factor(money.by.cat$main_category, levels = money.by.cat$main_category)

money.by.cat %>%
ggplot(aes(main_category, suma / 1000000, fill = suma)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Pledged USD by Category") + 
  xlab("Project Category") + 
  ylab("Pledged USD (millions)") + 
  geom_text(aes(label = paste0(round(suma / 1000000, 1))), vjust = -0.5) + 
  theme_stata() + 
  scale_fill_gradient(low = "aquamarine", high = "aquamarine4") +
  theme(legend.position = "null",
        plot.title = element_text(hjust = 0.5, face = 'bold'), 
        axis.title.x = element_text(size = 13, face = 'bold'), 
        axis.title.y = element_text(size = 13, face = 'bold', vjust = 5),
        axis.text.x = element_text(size = 13, angle = 90))
  

# ggsave('pic2.jpg')

# 3 -----------------------------------------------------
# Odsetek projektów wg krajów w których zostały założone

country_pie <- kick %>%
                group_by(country) %>% 
                summarise(share = n() / nrow(kick) * 100)

country_pie$country <- as.character(country_pie$country)
# wszytskie kraje które mają udział poniżej 1% zostają zakwalifikowane jako 'other'
country_pie$country[country_pie$share < 1] <- 'other'
country_pie$country <- as.factor(country_pie$country)

country_pie <- country_pie %>%
  group_by(country) %>% 
  summarise(share = sum(share))


plot_ly(country_pie, labels = ~country, values = ~share, type = 'pie', textposition = 'outside', textinfo = 'label+percent') %>%
  layout(title = list(text = 'Projects share by country', y = 1.5),
         xaxis = list(showgrid = FALSE, 
                      zeroline = FALSE, 
                      showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, 
                      zeroline = FALSE, 
                      showticklabels = FALSE))



# orca(country_share, "pic3.jpg")
# 4-----------------------
# ilośc projektów po kategoriach
projects.by.cat <- kick %>%
  group_by(main_category) %>%
  summarize(freq = n())%>%
  arrange(desc(freq))

projects.by.cat$main_category <- factor(projects.by.cat$main_category, levels = projects.by.cat$main_category)



projects.by.cat %>%
  ggplot(aes(main_category, freq, fill = freq)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Number of Projects by Category") + 
  xlab("Year") + 
  ylab("Frequency") + 
  geom_text(aes(label=paste0(freq)), vjust = -0.5) + 
  theme_stata() + 
  scale_fill_gradient(low = "aquamarine", high = "aquamarine4")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'), 
        axis.title.x = element_text(size = 12, face="bold"), 
        axis.title.y = element_text(size = 12, face="bold",  vjust = 5), 
        axis.text.x=element_text(size = 12, angle = 90), 
        legend.position = "none")
theme_stat
# ggsave('pic4.jpg')


# 5-----------------------
# ilośc projektów po latach
projects.by.year <- kick %>%
  filter(year != 2018) %>%
  group_by(year = as.factor(year)) %>%
  summarize(freq = n())

projects.by.year %>%
ggplot(aes(year, freq, fill = freq)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Number of Projects by Year") + 
  xlab("Year") + 
  ylab("Frequency") + 
  geom_text(aes(label=paste0(freq)), vjust = -0.5) + 
  theme_stata() + 
  scale_fill_gradient(low = "aquamarine", high = "aquamarine4")+
  theme(plot.title=element_text(hjust = 0.5, face = 'bold'), 
        axis.title.x = element_text(size = 12, face="bold"), 
        axis.title.y = element_text(size = 12, face="bold", vjust = 5), 
        axis.text.x=element_text(size = 12), 
        legend.position="none")

# ggsave('pic5.jpg')


# 6 ---------------------------------
# Rozkład porażek i rozkład sukcesów
# nie brane są pod uwagę wciąż trwające projekty, gdyż nie wiadomo czy zakończą się sukcesem czy porażką
succeed <- kick %>%
  filter(state == 'successful'& between(goal_percentage, 100, 200))

succeed <- kick %>%
  filter(state == 'successful')

failed <- kick %>%
  filter(state != 'live' & state != 'successful' & between(goal_percentage, 0, 100))



success <- ggplot(succeed, aes(x = goal_percentage)) +
  geom_histogram(aes(y=..count../sum(..count..) * 100), fill = 'aquamarine4', bins = 20) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  scale_x_continuous(breaks = seq(100, 200, by = 10),  limits=c(100,200)) +
  # xlim(c(100,200)) +
  ggtitle("Successes Distribution") + 
  xlab("Goal %") + 
  ylab("Projects %") + 
  theme_stata() + 
  theme(legend.position = "null",
        plot.title = element_text(hjust = 0.5), 
        axis.title = element_text(size = 13), 
        axis.text.x = element_text(size = 13))


# Histogram Porażek

fail <- ggplot(failed, aes(x=goal_percentage)) +
  geom_histogram(aes(y=..count../sum(..count..) * 100), fill = 'tomato3', bins = 20) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ggtitle("Failures Distribution") + 
  xlab("Goal %") + 
  ylab("Projects %") + 
  theme_stata() + 
  theme(legend.position = "null",
        plot.title = element_text(hjust = 0.5), 
        axis.title = element_text(size = 13), 
        axis.text.x = element_text(size = 13))


figure <- ggarrange(success, fail,
                    ncol = 1, nrow = 2)
figure

summary(kick)
# ggsave("pic6.3.jpg")

# ggexport(success, fail, filename = "fails_success_distr.jpg",
#        nrow = 2, ncol = 1)



# 7 ----------------------------------
# średni % osiągniętego celu vs średnia wielkość celu finansowego z podziałem na kategorie


# Trzeba wyrzucić outliery ponieważ mocno zawyżają wyniki
kick %>% 
      filter(goal_percentage / 100 < quantile(goal_percentage / 100 , probs = 0.99)) %>%
      group_by(main_category) %>% 
      summarise(mean_gathered = mean(goal_percentage) , mean_goal = mean(usd_goal_real) / 1000) %>%
      ggplot(aes(x = mean_gathered, y = mean_goal, label = main_category, color = main_category)) + 
      scale_y_continuous(breaks = seq(0, 120, by = 20)) +
      scale_x_continuous(breaks = seq(30, 120, by = 10)) +
      theme_stata() + 
      geom_point(size = 3) + 
      geom_text(vjust = -0.75) + 
      xlab('Average Goal % Achieved')+
      ylab('Average Goal in USD (thousands)') + 
      theme(legend.position = 'none',
            axis.title.x = element_text(size = 13, face = "bold", vjust = -3), 
            axis.title.y = element_text(size = 13, face = "bold", vjust = 5))



# ggsave("pic7.jpg")


# STATYSTYKI ---------------------------------------------------------------------


# statystyki dla zrealizowanych projektów
succeed %>% select(c(state, main_category, usd_goal_real, usd_pledged_real, backers, duration, goal_percentage)) %>% summary()

# statystyki dla niezrealizowanych projektów (zaliczam do ncih też wstrzymane i anulowane)
failed %>% select(c(state, main_category, usd_goal_real, usd_pledged_real, backers, duration, goal_percentage)) %>% summary()
# Histogram Sukcesów


# % projektów zrealizowanych gdzie uzbierano ponad 200% celu 
filter(kick, goal_percentage > 200 & state == 'successful')%>% nrow() / filter(kick, state == 'successful')%>%nrow()

# % projektów zrealizowanych gdzie uzbierano poniżej 110% celu 
filter(kick, goal_percentage < 110 & state == 'successful')%>% nrow() / filter(kick, state == 'successful')%>%nrow()


# % projektów niezrealizowanych gdzie uzbierano 5% lub mnniej celu 
filter(kick, goal_percentage <= 5 & state == 'failed') %>% nrow() / filter(kick, state == 'failed') %>% nrow()

# % projektów niezrealizowanych gdzie uzbierano 10% lub mnniej celu 
filter(kick, goal_percentage <= 10 & state == 'failed') %>% nrow() / filter(kick, state == 'failed')%>%nrow()

# % projektów niezrealizowanych gdzie uzbierano 30% lub mnniej więcej 
filter(kick, goal_percentage > 30 & state == 'failed') %>% nrow() / filter(kick, state == 'failed') %>% nrow()

# % kampanii które przekroczyły 20% celu ale zostały zrealizowane
filter(kick, goal_percentage  > 20 & state == 'failed') %>% nrow() / filter(kick, goal_percentage  > 20) %>%nrow()

# ilośc projektów powyżej uzbieranego miliona USD
filter(kick, usd_pledged_real > 1000000 & state == 'successful')%>% nrow()
mln <- filter(kick, usd_pledged_real > 1000000 & state == 'successful')
mean(mln$goal_percentage)
# projekty gdzie uzbierano powyżej 10 mln USD
above_10mln_data <- kick %>% 
  filter(usd_pledged_real > 10000000 & state == 'successful') %>%
  arrange(-c(usd_pledged_real))


#  --------------------------------------
# tabela przestawna

year_pivot <- kick %>%
  filter(state != 'live' & year != 2018) %>%
  group_by(year) %>%
  summarize(freq =  n(), 
            pledged = sum(usd_pledged_real),
            backers = sum(backers),
            avg_donation = as.numeric(pledged / backers),
            success_ratio = paste0(round(sum(state == 'successful') / n() * 100,2) ,'%')) %>%
  mutate(backers = as.integer(backers), 
         year = as.factor(year),
         pledged_mln = as.numeric(pledged / 1000000), 
         freq = as.integer(freq)) %>%
  select(c('year','freq', 'pledged_mln','backers','avg_donation','success_ratio'))

yp <- t(year_pivot)
colnames(yp) <- yp[1,]
yp <- yp[-c(1), ]  

# write.csv(yp,'year_pivot.csv')

category_pivot <- kick %>%
  filter(state != 'live' & year != 2018) %>%
  group_by(main_category) %>%
  summarize(freq =  n(), 
            pledged = sum(usd_pledged_real),
            backers = sum(backers),
            avg_donation = as.numeric(pledged / backers),
            success_ratio = paste0(round(sum(state == 'successful') / n() * 100,2) ,'%')) %>%
  mutate(backers = as.integer(backers), 
         pledged_mln = as.numeric(pledged / 1000000), 
         freq = as.integer(freq)) %>%
  select(c('main_category','freq', 'pledged_mln','backers','avg_donation','success_ratio'))


cp <- t(category_pivot)
colnames(cp) <- cp[1,]
cp <- cp[-c(1),]  

cp
# write.csv(cp,'category_pivot.csv')



# WYBRANIE ZMIENNYCH DO BUDOWY KLASYFIKATORA W AZURE ML STUDIO
# ------------------------------------------------------------

# * Konwersja zmiennej 'state' w binarną, gdzie:
# 1 - projekty zakończone sukcesem (successful)
# 0 - projekty niezrealizowane (w tym anulowane i wstrzymane)


kickstarter_final <- kick %>% 
  mutate(log_usd_goal_real = log(usd_goal_real)) %>%
  select(c(usd_goal_real, log_usd_goal_real, title_lenght, duration, main_category, year, month, country, state)) %>% 
  filter(state != 'live' & year != '2018') %>%
  mutate(state = as.factor(ifelse(state == 'successful' , 1 , 0)))


# Sprawdzenie korelacji między zmiennymi numerycznymi
kickstarter_corr <- kickstarter_final %>% 
                        select(c(usd_goal_real, log_usd_goal_real, title_lenght, duration))

round(cor(kickstarter_corr), 2)

str(kickstarter_final)

# write.csv(kickstarter_final, 'kickstarter_final.csv', row.names=FALSE)
#-------------------------------------------------------------------------------------------------------------------------------------------

# AZURE ML STUDIO ----------- AZURE ML STUDIO  -----------  AZURE ML STUDIO  ------------  AZURE ML STUDIO  --------------  AZURE ML STUDIO

