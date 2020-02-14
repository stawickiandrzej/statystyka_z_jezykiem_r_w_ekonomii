library(tidyverse)
library(ggplot2)
library(janitor)
library(gridExtra)

data <- janitor::clean_names(data)

#podział produktów na 4 grupy, ze względu na ich rodzaj
zywnosc_chem <- data %>% 
  filter(rodzaje_towarow_i_uslug %in% c("	karp świeży - za 1kg", "mleko krowie spożywcze o zawartości tłuszczu 2-2,5% - za 1l	cena", "	jaja kurze świeże (chów klatkowy lub ściółkowy) - za 1 szt", "masło świeże o zawartości tłuszczu ok. 82,5% - za 200 g", "marchew - za 1 kg", "	herbata czarna, liściasta - za 100g", "	mydło toaletowe - za 100g"))

olej <- data %>% 
  filter(rodzaje_towarow_i_uslug %in% c("olej napędowy - za 1l")) %>% 
  select(nazwa,miesiace,rodzaje_towarow_i_uslug,rok,wartosc,jednostka_miary)

olej_bezbd <- na.omit(olej)

woda <- data %>%
  filter(rodzaje_towarow_i_uslug %in% c("ciepła woda - za 1m3"))
         
kultura <- data %>% 
  filter(rodzaje_towarow_i_uslug %in% c("bilet do kina"))
  
#ceny w okresie 2006-2019
zywn_by_year <- zywnosc_chem %>%
  group_by(rok) %>%
  summarise(srednia_rok_zywnosc = mean(wartosc, na.rm=TRUE)) %>% 
  select(rok,srednia_rok_zywnosc)

olej_by_year <- olej_bezbd %>%
  group_by(rok) %>%
  summarise(srednia_rok_olej = mean(wartosc, na.rm=TRUE)) %>% 
  select(rok,srednia_rok_olej)
  
woda_by_year <- woda %>%
  group_by(rok) %>%
  summarise(srednia_rok_woda = mean(wartosc, na.rm=TRUE)) %>% 
  select(rok,srednia_rok_woda)

kultura_by_year <- kultura %>%
  group_by(rok) %>%
  summarise(srednia_kult_rok = mean(wartosc, na.rm=TRUE)) %>% 
  select(rok,srednia_kult_rok)

#ceny w przekroju województw

zywn_woj <- zywnosc_chem %>%
  group_by(nazwa) %>%
  summarise(srednia_woj_rok = mean(wartosc, na.rm=TRUE))

olej_woj <- olej_bezbd %>%
  group_by(nazwa) %>%
  summarise(srednia_woj_rok = mean(wartosc, na.rm=TRUE))

woda_woj <- woda %>%
  group_by(nazwa) %>%
  summarise(srednia_woj_rok = mean(wartosc, na.rm=TRUE))

kult_woj <- kultura %>%
  group_by(nazwa) %>%
  summarise(srednia_woj_rok = mean(wartosc, na.rm=TRUE))


#wykresy w okresie 2006-2019

require(gridExtra)

matplot(x=zywn_by_year$rok,cbind(y1=zywn_by_year$srednia_rok_zywnosc,y2=kultura_by_year$srednia_kult_rok,y3=woda_by_year$srednia_rok_woda,y4=olej_by_year$srednia_rok_olej),type="l",col = c("red","green","blue","violet"),lty=c(1,1,1,1),xlab = "Rok",ylab = "Cena",)
legend(2013,13,legend = c('Źywność','Kultura','Woda','Olej'),col = c('red','green','blue','violet'),lty = 1:1, lwd=1, xpd = T)



