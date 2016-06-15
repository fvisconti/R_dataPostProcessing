args<-commandArgs(TRUE)
# modifica 04/05/2016 - Francesco
# vengono usate massimo due variabili per il filtraggio, con la seguente logica:
# alla prima chiamata dello script vengono passati i valori di filtro per una sola
# variabile, per la seconda non sulla GUI non viene proposto il campo d'inserimento
# e viene passata una tripletta di zeri.
# Generato il primo grafico con filtro, viene acceso il form per la seconda variabile
# e bloccato l'inserimento per i filtri della prima.
# Il tasto reset azzera tutte le selezioni e riaccende i campi inserimento per la prima
# variabile di filtro.

## args format  (input_file.csv, n_col_input, ncol_output, col_filter1, low1, up1, col_filter2, low2, up2, only_DoE_0_1, width_px, height_px, output_file.png, group_col, group_intervals, working_dir)
## args <-c("benchmark29.csv","4","2","7","1.25","1.29", "6", "0.08843", "0.09438", "1","1000","600","parallel3.png", 6, 20, "~/R/RBFAero/Prod/v1.1/") 
## args <-c("benchmark29.csv","4","2","0","0","0", "0", "0", "0", "1","1000","600","parallel4.png", 6, 20, "~/R/RBFAero/Prod/v1.1/") 

setwd(args[length(args)]) #set wdir
args <- args[-length(args)] #rm wdir entry in args

require(MASS)
require(dplyr)

# variabile logica, 1 se solo doe
onlyDoe <- args[10] %>% as.numeric()
# variabile logica, 0 se non viene filtrato il dataset
lfilter <- args[4] %>% as.numeric()
lfilter2 <- args[7] %>% as.numeric()

# seleziono solo header in dd
dd <-args[1] %>% read.csv(sep=";") %>% as.tbl() %>% names()
# se non c'e colonna filtro, uso ID
if (lfilter == 0){
  col <- "ID"
  low <-  args[1] %>% read.csv(sep=";") %>% as.tbl() %>% select_("ID") %>% min
  up <-  args[1] %>% read.csv(sep=";") %>% as.tbl() %>% select_("ID") %>% max
} else{
  col <- dd[lfilter]
  low <- args[5] %>% as.numeric
  up <- args[6] %>% as.numeric
  if(lfilter2 != 0){
    col2 <- dd[lfilter2]
    low2 <- args[8] %>% as.numeric
    up2 <- args[9] %>% as.numeric
  }
}

### selezionare qui i dataset con i relativi filtri
if (onlyDoe){
  ds <- args[1] %>% read.csv(sep=";") %>% as.tbl() %>% filter(type=="exact") %>%
    select_("-type")
} else{
  ds <- args[1] %>% read.csv(sep=";") %>% as.tbl() %>% select_("-type")
}

# estremi assoluti del dataset
absmax <- apply(ds, 2, max)
absmin <- apply(ds, 2, min)

# filtri: il primo filtro viene effettuato senza if, perche usiamo ID come default
ds <- ds[(ds[, col] <= up) & (ds[, col] >= low), ]
# secondo filtro: controllo se la colonna != 0
if(lfilter2 != 0)
  ds <- ds[(ds[, col2] <= up2) & (ds[, col2] >= low2), ]
###

# dopo aver filtrato il dataset, attacco le righe con gli estremi assoluti!
# ds <- rbind(ds, absmin)
# ds <- rbind(ds, absmax)
# ds <- ds %>% as.tbl()

# variabili per creazione del range di colori
intervals <- as.numeric(args[length(args)])
args <- args[-length(args)] #rm group_intervals entry in args

# colonna con cui raggruppare i colori
gcol <- as.numeric(args[length(args)])

mmin <- ds %>% select_(gcol) %>% min
mmax <- ds %>% select_(gcol) %>% max
delta <- (mmax - mmin) / intervals
# calcolo l'offset come il primo intervallo, che puo essere negativo
# l'indice di intervallo di ciascun valore sara quindi x %/% delta - offset
# offst <- mmin %/% delta
# fine variabili range colori

png(args[13], width = as.numeric(args[11]), height= as.numeric(args[12]))
    
  # attacco la colonna degli intervalli in cui ricade il valore della colonna
  # su cui ho scelto di raggruppare i colori
  #for(i in 1:nrow(ds)) intervals[i] <- ds[i, gcol] %/% delta - offst + 1  
  for(i in 1:nrow(ds)) intervals[i] <- (ds[i, gcol] - mmin) %/% delta + 1  
  ds <- cbind(ds, as.numeric(intervals))
  names(ds)[length(ds)] <- "interval"

  clrs <- colors()[ds$interval * 5]

# attacco le righe con gli estremi assoluti dopo il solo filtro sul tipo,
# per fissare il fondo scala!
  ds <- rbind(ds, absmin)
  ds <- rbind(ds, absmax)
  ds %>% as.tbl()

  ds <- ds %>% select_("-ID")
  ds <- ds %>% select_("-interval")
  parcoord(ds, col = clrs, var.label=TRUE)
  #parcoord(ds, col = rainbow(nrow(ds)), var.label=TRUE)
  
dev.off()
