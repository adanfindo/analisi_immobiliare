library(moments)
library(dplyr)
library(ggplot2)


data = read.csv("realestate_texas.csv")

#calcolo indici
index = round(apply(data[, 4:7], 2, function(x) c(massimo = max(x), minimo = min(x), media = mean(x), mediana = median(x),dev.std = sd(x))))

#distribuzione di frequenze month inventory
N=dim(data)[1]
month_inventory_cl = cut(data$months_inventory, breaks = c(3,5,7,9,11,13,15))

ni.mi = table(month_inventory_cl)
fi.mi = round(ni.mi/N,2)
Ni.mi = cumsum(ni.mi)
Fi.mi = round(Ni.mi/N,2)

distr_freq_mi = as.data.frame(cbind(ni.mi, fi.mi, Ni.mi, Fi.mi))

#divisione in classi di volume

vol_cl = cut(data$volume, breaks = c(8,15,22,29,36,43,50,57,64,71,78,84))

ni.vol = table(vol_cl)
fi.vol = round(ni.vol/N,2)
Ni.vol = cumsum(ni.vol)
Fi.vol = round(Ni.vol/N,2)

#distribuzione di frequenze
distr_freq_vol = as.data.frame(cbind(ni.vol, fi.vol, Ni.vol, Fi.vol))

#indice di gini per la distribuzione di frequenze di volume
gini.index <- function(x){
  ni = table(x)
  fi = ni/length(x)
  fi2 = fi^2
  J = length(table(x))

  gini = 1-sum(fi2)
  gini.norm = gini/((J-1)/J)

  return(gini.norm)
}

Gini_vol = gini.index(distr_freq_vol)


#grafico a barre per la distribuzione di vendite costruita
ggplot(data = distr_freq_vol) +
  geom_col(
  aes(x=rownames(distr_freq_vol),
      y=ni.vol),
  col="darkorange4",
  fill="lightblue")+
  labs(x="vendite in classi",
       y="Frequenze assolute",
       title="Distribuzione di frequenze volume di vendite")+
  theme_bw()+
  scale_y_continuous(breaks = seq(0,100,10)) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

#funzione con variabilita maggiore

CV <-function(x){
  return( sd(x)/mean(x) * 100 )
}

var_most_CV = max(names(apply(data[,4:8],2,function(x) CV(x))))
var_piu_asim = max(names(apply(data[,4:8],2,function(x) skewness(x))))

#indice gini per city
Gini.city = gini.index(data$city)

#probabilita

#di trovare Beaumont
ni.city = table(data$city)["Beaumont"]
prob_Beaumont = ni.city[1]/N *100

#di trovare luglio
ni.month = table(data$month)["7"]
prob.july = ni.month/N*100

#dicembre 2012
dec_2012 = table(data$month, data$year)["12", "2012"]
prob_dec_2012 = round(dec_2012/N * 100,2)


#aggiunta colonna prezzo medio
data$prezzo_medio = round(data$volume*1000000/data$sales)

#efficacia annunci
data$TassoConversione <- round(data$sales / data$listings *100,2)

library(RColorBrewer)
#grafico a barre tasso di conversione
data %>%
  group_by(year) %>%
  summarise(media = mean(TassoConversione)) %>%
  ggplot() +
  aes(x = year, y = media) +
  labs(x = "anni", y = "media tasso di conversione", title = "media del tasso di conversione dal 2010 al 2014") +
  geom_bar(stat = "identity", fill = "orange2" , color = "black") +
  scale_y_continuous(breaks = seq(0,16,2)) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

#grafico barre sovrapposte tasso di conversione per anno e citta
data %>%
  group_by(year,city) %>%
  summarise(media = mean(TassoConversione)) %>%
  ggplot() +
  aes(x = year, y = media, fill = city) +
  geom_bar(stat = "identity", position = "dodge", color= "black") +
  labs(x = "Anno", y = "Media Tasso di Conversione", title = "Media Tasso di Conversione per Anno") +
  scale_y_continuous(breaks = seq(0,25,2))+
  scale_fill_brewer(palette = "Set2") +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))


#grafico barre sovrapposte anno, citta e mese
data %>%
  group_by(year, month) %>%
  ggplot() +
  aes(x = month, y = sales, fill = city) +
  geom_bar(stat ="identity", position = "stack", color = "black") +
  facet_grid(~year) +
  labs(x = "mesi" , y = "vendite") +
  scale_x_continuous(breaks = seq(0,12,1)) +
  scale_y_continuous(breaks = seq(0,1200, 50)) +
  scale_fill_brewer(palette = "Set3")

#normalizzato
data %>%
  group_by(year, month,city) %>%
  summarise(tot_sales = sum(sales)) %>%
  mutate(norm_sales = tot_sales/sum(tot_sales)) %>%
  ggplot() +
  aes(x = month, y = norm_sales, fill = city) +
  geom_bar(stat ="identity", position = "stack", color = "black") +
  facet_grid(~year) +
  labs(x = "mesi" , y = "vendite") +
  scale_x_continuous(breaks = seq(0,12,1)) +
  scale_y_continuous(breaks = seq(0,1, 0.1)) +
  scale_fill_brewer(palette = "Set3") +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.1)) +
  theme(
    panel.background = element_rect(fill = "white"),  # Sfondo bianco
    panel.grid.major = element_line(color = "black", size = 0.01),  # Griglia principale
    panel.grid.minor = element_line(color = "black", size = 0.1)   # Griglia secondaria
  )


# line chart
ggplot(data, aes(x = month, y = sales, color = city)) +
  geom_line() +
  geom_point() +
  facet_wrap(~year, ncol = 1) +
  labs(x = "Mese", y = "Vendite", title = "Vendite mensili per città e per anno") +
  scale_x_continuous(breaks = seq(1,12,1)) +
  scale_y_continuous(breaks = seq(0,400,50)) +
  scale_fill_brewer(palette = "Set2") +
  theme(strip.text = element_text(color = "black", face = "bold"),
        panel.grid = element_line(color = "white")) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) +
  theme(
    panel.background = element_rect(fill = "white"),  # Sfondo bianco
    panel.grid.major = element_line(color = "black", size = 0.01),  # Griglia principale
    panel.grid.minor = element_line(color = "black", size = 0.1)   # Griglia secondaria
  )


#boxplot
ggplot(data, aes(x = city, y = median_price)) +
  geom_boxplot(fill = "orange3", color = "black") +
  labs(x = "Città", y = "Prezzo mediano delle case", title = "Distribuzione del valore totale delle vendite tra le varie città e anni") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))


ggplot(data, aes(x = city, y = sales, fill = as.factor(year))) +
  geom_boxplot() +
  labs(x = "Città", y = "Vendite", title = "Confronto delle vendite tra città e anno") +
  scale_fill_brewer(palette = "Set3") +
  guides(fill = guide_legend(title = expression("Anno"))) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) +
  theme(
    panel.background = element_rect(fill = "white"),  # Sfondo bianco
    panel.grid.major = element_line(color = "black", size = 0.1),  # Griglia principale
    panel.grid.minor = element_line(color = "black", size = 0.1)   # Griglia secondaria
  )
