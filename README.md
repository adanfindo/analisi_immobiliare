# analisi_immobiliare
analisi del mercato immobiliare del texas


Task
1) Scarica il dataset realestate_textas.csv da qui e importalo con R, questo contiene dei dati riguardanti le vendite di immobili in Texas. Le variabili del dataset sono:

city: città
year: anno di riferimento
month: mese di riferimento
sales: numero totale di vendite
volume: valore totale delle vendite in milioni di dollari
median_price: prezzo mediano di vendita in dollari
listings: numero totale di annunci attivi
months_inventory: quantità di tempo necessaria per vendere tutte le inserzioni correnti al ritmo attuale delle vendite, espresso in mesi.
2) Indica il tipo di variabili contenute nel dataset.

3) Calcola Indici di posizione, variabilità e forma per tutte le variabili per le quali ha senso farlo, per le altre crea una distribuzione di frequenza. Commenta tutto brevemente.


4) Qual è la variabile con variabilità più elevata? Come ci sei arrivato? E quale quella più asimmetrica?


5) Dividi una delle variabili quantitative in classi, scegli tu quale e come, costruisci la distribuzione di frequenze, il grafico a barre corrispondente e infine calcola l’indice di Gini.


6) Indovina l’indice di gini per la variabile city.Comment by Luca Naso: Non sarebbe meglio dire "trova" o "calcola"?


7) Qual è la probabilità che presa una riga a caso di questo dataset essa riporti la città “Beaumont”? E la probabilità che riporti il mese di Luglio? E la probabilità che riporti il mese di dicembre 2012?


8) Esiste una colonna col prezzo mediano, creane una che indica invece il prezzo medio, utilizzando le altre variabili che hai a disposizione.


9) Prova a creare un’altra colonna che dia un’idea di “efficacia” degli annunci di vendita. Riesci a fare qualche considerazione?


10) Prova a creare dei summary(), o semplicemente media e deviazione standard, di alcune variabili a tua scelta, condizionatamente alla città, agli anni e ai mesi. Puoi utilizzare il linguaggio R di base oppure essere un vero Pro con il pacchetto dplyr. Ti lascio un suggerimento in pseudocodice, oltre al cheatsheet nel materiale:


dati %>%


group_by(una o più variabili di raggruppamento) %>%


summarise(nomecolonna1=funzione1(variabile da sintetizzare),


nomecolonna2=funzione2(variabile da sintetizzare))


Sfruttando questa notazione puoi creare anche dei grafici super!


Da qui in poi utilizza ggplot2 per creare grafici fantastici!


Ma non fermarti alla semplice soluzione del quesito, prova un po’ a personalizzare i grafici utilizzando temi, colori e annotazioni, e aggiustando i vari elementi come le etichette, gli assi e la legenda.


Consiglio: Fai attenzione quando specifichi le variabili month e year tra le estetiche, potrebbe essere necessario considerarle come fattori.


1) Utilizza i boxplot per confrontare la distribuzione del prezzo mediano delle case tra le varie città. Commenta il risultato


2) Utilizza i boxplot o qualche variante per confrontare la distribuzione del valore totale delle vendite tra le varie città ma anche tra i vari anni. Qualche considerazione da fare?


3) Usa un grafico a barre sovrapposte per ogni anno, per confrontare il totale delle vendite nei vari mesi, sempre considerando le città. Prova a commentare ciò che viene fuori. Già che ci sei prova anche il grafico a barre normalizzato. Consiglio: Stai attento alla differenza tra geom_bar() e geom_col(). PRO LEVEL: cerca un modo intelligente per inserire ANCHE la variabile Year allo stesso blocco di codice, senza però creare accrocchi nel grafico.


4) Crea un line chart di una variabile a tua scelta per fare confronti commentati fra città e periodi storici. Ti avviso che probabilmente all’inizio ti verranno fuori linee storte e poco chiare, ma non demordere. Consigli: Prova inserendo una variabile per volta. Prova a usare variabili esterne al dataset, tipo vettori creati da te appositamente.


Se non riesci proprio a venirne a capo inizia lavorando su dataset ridotti, ad esempio prendendo in considerazione un solo anno o una sola città. Aiutati con il pacchetto dplyr:


dati2014 <- filter(dati, year==2014)


dati_Beaumont <- filter(dati, city==”Beaumont”

