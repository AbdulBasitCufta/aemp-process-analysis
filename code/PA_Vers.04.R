## =========================================================
## 0) Pakete laden
## =========================================================

library(dplyr)
library(lubridate)
library(ggplot2)
library(DiagrammeR)
library(scales)     
library(tibble)     


## =========================================================
## 1) Daten laden
## =========================================================

ordner <- "C:/Users/acufta/Projektarbeit/Checkpunkte"

dateien <- list.files(
  path       = ordner,
  pattern    = "\\.csv$",
  full.names = TRUE
)

# CSV-Dateien einlesen + nur relevante Spalten behalten
checkpunkte <- dateien %>%
  lapply(
    read.csv,
    sep              = ";",
    na.strings       = c("NULL", ""),
    stringsAsFactors = FALSE
  ) %>%
  lapply(dplyr::select,
         c(1, 2, 3, 4, 5, 10, 11, 12, 24, 25, 26)) %>%
  bind_rows()

cat("Zeilen gesamt: ", nrow(checkpunkte), "\n")


## =========================================================
## 2) Typen bereinigen + Zeitspalte parsen
## =========================================================

checkpunkte <- checkpunkte %>%
  mutate(
    SiteID         = suppressWarnings(as.numeric(SiteID)),
    Obj_TrayConcID = suppressWarnings(as.numeric(Obj_TrayConcID)),
    CpRegPosID     = suppressWarnings(as.numeric(CpRegPosID))
  )

# TrackTime hat zwei mögliche Formate -> beide parsen
if ("TrackTime" %in% names(checkpunkte)) {
  checkpunkte$TrackTime <- parse_date_time(
    checkpunkte$TrackTime,
    orders = c("Y-m-d H:M:OS", "d.m.Y H:M"),
    tz     = "Europe/Zurich"
  )
}

cat("Anzahl NA in TrackTime: ", sum(is.na(checkpunkte$TrackTime)), "\n")

## =========================================================
## 3) Sortieren + Prozess-ID logische Rekonstruktion
## =========================================================

checkpunkte_sorted <- checkpunkte %>%
  arrange(CpRegPosID)

n      <- nrow(checkpunkte_sorted)
set_id <- checkpunkte_sorted$Obj_TrayConcID
cp_id  <- checkpunkte_sorted$CheckpointID

# Prozess-ID Vektor erzeugen
proc_id <- rep(NA_integer_, n)

# aktive Prozesse pro Set tracken
alle_sets   <- unique(set_id[!is.na(set_id)])
active_proc <- integer(length(alle_sets))
names(active_proc) <- as.character(alle_sets)

valid_proc   <- logical(0)   # TRUE = sauber, FALSE = invalid
next_proc_id <- 1L

## =========================================================
## 4) Schritt-für-Schritt Logik
##    Start = 1
##    Ende  = 35
##    Neues 1 vor 35 → alter Prozess invalid
## =========================================================

for (i in seq_len(n)) {
  
  sid  <- set_id[i]
  cpid <- cp_id[i]
  
  if (is.na(sid)) next  # kein Set → keine Prozess-ID
  
  key      <- as.character(sid)
  cur_proc <- active_proc[[key]]
  
  # Checkpoint unbekannt → nur Prozess-ID mitziehen, wenn aktiv
  if (is.na(cpid)) {
    if (!is.null(cur_proc) && cur_proc > 0L) proc_id[i] <- cur_proc
    next
  }
  
  # ---------- START (1) ----------
  if (cpid == 1) {
    
    if (is.null(cur_proc)) {
      active_proc[[key]] <- 0L
      cur_proc <- 0L
    }
    
    # Konfliktfall: neues 1, obwohl alter Prozess nicht beendet
    if (cur_proc > 0L) {
      if (length(valid_proc) < cur_proc) length(valid_proc) <- cur_proc
      valid_proc[cur_proc] <- FALSE
    }
    
    # neuen Prozess starten
    new_id <- next_proc_id
    next_proc_id <- next_proc_id + 1L
    
    if (length(valid_proc) < new_id) length(valid_proc) <- new_id
    valid_proc[new_id] <- TRUE
    
    active_proc[[key]] <- new_id
    proc_id[i] <- new_id
    next
  }
  
  # ---------- ENDE (35) ----------
  if (cpid == 35) {
    if (!is.null(cur_proc) && cur_proc > 0L) {
      proc_id[i]         <- cur_proc
      active_proc[[key]] <- 0L
    }
    next
  }
  
  # ---------- Zwischen-Checkpoints ----------
  if (!is.null(cur_proc) && cur_proc > 0L) proc_id[i] <- cur_proc
}

# offene Prozesse → invalid
offene <- active_proc[active_proc > 0L]
if (length(offene) > 0) {
  if (length(valid_proc) < max(offene)) length(valid_proc) <- max(offene)
  valid_proc[offene] <- FALSE
}

checkpunkte_sorted$proc_id <- proc_id


## =========================================================
## 5) Prozesse zusammenfassen (clean vs. problem)
## =========================================================

prozesse_raw <- checkpunkte_sorted %>%
  filter(!is.na(proc_id)) %>%
  arrange(proc_id, CpRegPosID) %>%
  group_by(proc_id, Obj_TrayConcID) %>%
  summarise(
    start_time = first(TrackTime),
    end_time   = last(TrackTime),
    duration_h = as.numeric(difftime(end_time, start_time, units = "hours")),
    start_cp   = first(CheckpointID),
    end_cp     = last(CheckpointID),
    n_steps    = n(),
    .groups    = "drop"
  )

valid_df <- tibble(
  proc_id    = seq_along(valid_proc),
  valid_flag = valid_proc
)

prozesse_join <- prozesse_raw %>%
  left_join(valid_df, by = "proc_id")

# Saubere Prozesse
prozesse_clean <- prozesse_join %>%
  filter(
    valid_flag == TRUE,
    start_cp   == 1,
    end_cp     == 35,
    !is.na(start_time),
    !is.na(end_time)
  )

# Problemprozesse
prozesse_problem <- prozesse_join %>%
  filter(
    is.na(valid_flag) | valid_flag == FALSE |
      start_cp != 1 | end_cp != 35 |
      is.na(start_time) | is.na(end_time)
  )

cat("Anzahl Prozesse gesamt: ", nrow(prozesse_join),  "\n")
cat("Davon saubere Prozesse:", nrow(prozesse_clean), "\n")
cat("Davon Problem-Prozesse:", nrow(prozesse_problem), "\n")

head(prozesse_clean, 10)

## =========================================================
## 6) Ergebnisse speichern
## =========================================================

saveRDS(prozesse_clean,
        "C:/Users/acufta/Projektarbeit/prozesse_clean.rds")

saveRDS(prozesse_problem,
        "C:/Users/acufta/Projektarbeit/prozesse_problem.rds")

saveRDS(checkpunkte_sorted,
        "C:/Users/acufta/Projektarbeit/checkpunkte_sorted.rds")


## =========================================================
## 7) Verteilung der Prozesslängen (Step-Gruppen)
## =========================================================

step_levels <- c(
  "01–05","06–10","11–15","16–20",
  "21–30","31–40","41–50","51–60",
  "61–70","71–80","81–90","91–100",
  "101–110","111–120","121–200","201–380"
)

prozesse_mit_gruppen <- prozesse_clean %>%
  mutate(
    step_group = case_when(
      n_steps >=   1 & n_steps <=   5 ~ "01–05",
      n_steps >=   6 & n_steps <=  10 ~ "06–10",
      n_steps >=  11 & n_steps <=  15 ~ "11–15",
      n_steps >=  16 & n_steps <=  20 ~ "16–20",
      n_steps >=  21 & n_steps <=  30 ~ "21–30",
      n_steps >=  31 & n_steps <=  40 ~ "31–40",
      n_steps >=  41 & n_steps <=  50 ~ "41–50",
      n_steps >=  51 & n_steps <=  60 ~ "51–60",
      n_steps >=  61 & n_steps <=  70 ~ "61–70",
      n_steps >=  71 & n_steps <=  80 ~ "71–80",
      n_steps >=  81 & n_steps <=  90 ~ "81–90",
      n_steps >=  91 & n_steps <= 100 ~ "91–100",
      n_steps >= 101 & n_steps <= 110 ~ "101–110",
      n_steps >= 111 & n_steps <= 120 ~ "111–120",
      n_steps >= 121 & n_steps <= 200 ~ "121–200",
      n_steps >= 201 & n_steps <= 380 ~ "201–380",
      TRUE ~ NA_character_
    )
  )

stepgruppen_uebersicht <- prozesse_mit_gruppen %>%
  filter(!is.na(step_group)) %>%
  count(step_group, name = "anz_prozesse") %>%
  mutate(step_group = factor(step_group, levels = step_levels))

print(stepgruppen_uebersicht)

ggplot(stepgruppen_uebersicht,
       aes(x = step_group, y = anz_prozesse)) +
  geom_col() +
  labs(
    title = "Anzahl Prozesse nach Anzahl Steps",
    x     = "Step-Gruppe",
    y     = "Anzahl Prozesse"
  ) +
  theme_minimal()


## =========================================================
## 8) Prozesse mit > 50 Steps herausfiltern
## =========================================================

valid_proc_ids_max50 <- prozesse_clean %>%
  filter(n_steps <= 50) %>%
  pull(proc_id)

checkpunkte_filtered <- checkpunkte_sorted %>%
  filter(proc_id %in% valid_proc_ids_max50)

# Start- und End-Checkpoint nach TrackTime
prozesse_start_end_time <- checkpunkte_filtered %>%
  group_by(proc_id) %>%
  arrange(TrackTime, CpRegPosID) %>%
  summarise(
    first_cp_time = first(CheckpointID),
    last_cp_time  = last(CheckpointID),
    n_steps_time  = n(),
    .groups       = "drop"
  )

# Häufigkeiten
prozesse_start_end_time %>%
  count(first_cp_time, name = "anzahl") %>%
  arrange(desc(anzahl))

prozesse_start_end_time %>%
  count(last_cp_time, name = "anzahl") %>%
  arrange(desc(anzahl))

# Verdächtig: Start != 1 oder Ende != 35
prozesse_verdächtig <- prozesse_start_end_time %>%
  filter(first_cp_time != 1 | last_cp_time != 35)

print(prozesse_verdächtig)


## =========================================================
## 9) Directly-Follows-Kanten aus checkpunkte_filtered
## =========================================================

min_edge_frequency <- 1000   # Mindesthäufigkeit für DFG-Kanten

# Übergänge bestimmen
edges <- checkpunkte_filtered %>%
  arrange(proc_id, CpRegPosID) %>%
  group_by(proc_id) %>%
  mutate(
    from = CheckpointID,
    to   = lead(CheckpointID)
  ) %>%
  ungroup() %>%
  filter(!is.na(from), !is.na(to)) %>%
  count(from, to, name = "n")

edges_filtered <- edges %>%
  filter(n >= min_edge_frequency)

if (nrow(edges_filtered) == 0) {
  stop("Keine Kanten erfüllen den Mindestwert (min_edge_frequency).")
}

# Penwidth aus Häufigkeit skalieren (z.B. zwischen 1 und 6)
n_min <- min(edges_filtered$n)
n_max <- max(edges_filtered$n)

edge_widths <- if (n_max > n_min) {
  # linear skalieren: kleinste Kante ≈ 1, größte ≈ 6
  1 + 3 * (edges_filtered$n - n_min) / (n_max - n_min)
} else {
  # falls alle gleich häufig sind
  rep(3, nrow(edges_filtered))
}


## =========================================================
## 10) Directly-Follows Graph (DFG) mit DiagrammeR erzeugen
## =========================================================

nodes <- sort(unique(c(edges_filtered$from, edges_filtered$to)))

dfg_graph <- create_graph()

# Knoten
for (node in nodes) {
  dfg_graph <- add_node(
    graph = dfg_graph,
    label = as.character(node)
  )
}

# Kanten (erstmal ohne Attribute)
for (i in seq_len(nrow(edges_filtered))) {
  dfg_graph <- add_edge(
    graph = dfg_graph,
    from  = as.character(edges_filtered$from[i]),
    to    = as.character(edges_filtered$to[i])
  )
}

# Labels setzen (Häufigkeiten auf den Kanten)
dfg_graph <- set_edge_attrs(
  graph     = dfg_graph,
  edge_attr = "label",
  values    = as.character(edges_filtered$n)
)

# Penwidth nach Häufigkeit setzen (dicke = viele Durchläufe)
dfg_graph <- set_edge_attrs(
  graph     = dfg_graph,
  edge_attr = "penwidth",
  values    = edge_widths
)

render_graph(dfg_graph)


## =========================================================
## 11) Top-10 häufigste vollständige Wege (Prozessvarianten)
## =========================================================

# Prozesswege nach CpRegPosID bestimmen
varianten <- checkpunkte_filtered %>%
  arrange(proc_id, CpRegPosID) %>%
  group_by(proc_id) %>%
  summarise(
    path = paste(CheckpointID, collapse = " - "),
    .groups = "drop"
  )

# Top 10 häufigste Varianten
top_varianten <- varianten %>%
  count(path, name = "anz_prozesse", sort = TRUE) %>%
  slice_head(n = 10)

print(top_varianten)

# Alle Prozess-IDs der Top-Varianten
top_proc_ids <- varianten %>%
  semi_join(top_varianten, by = "path") %>%
  pull(proc_id)

cat("Anzahl Prozesse in Top-Varianten:", length(top_proc_ids), "\n")


## =========================================================
## 12) Directly-Follows-Kanten NUR aus Top-10 Wegen
## =========================================================

edges_top <- checkpunkte_filtered %>%
  filter(proc_id %in% top_proc_ids) %>%
  arrange(proc_id, CpRegPosID) %>%
  group_by(proc_id) %>%
  mutate(
    from = CheckpointID,
    to   = lead(CheckpointID)
  ) %>%
  ungroup() %>%
  filter(!is.na(from), !is.na(to)) %>%
  count(from, to, name = "n")

# Mindesthäufigkeit (hier 1 = alles zeigen)
min_edge_freq_top <- 1

edges_top_filtered <- edges_top %>%
  filter(n >= min_edge_freq_top)

if (nrow(edges_top_filtered) == 0) {
  stop("Keine Kanten in den Top-10 Wegen nach Filter.")
}


## =========================================================
## 13) Directly-Follows Graph für Top-Varianten
## =========================================================

nodes_top <- sort(unique(c(edges_top_filtered$from, edges_top_filtered$to)))

dfg_top <- create_graph()

# Knoten hinzufügen
for (node in nodes_top) {
  dfg_top <- add_node(
    graph = dfg_top,
    label = as.character(node)
  )
}

# Kanten hinzufügen
for (i in seq_len(nrow(edges_top_filtered))) {
  dfg_top <- add_edge(
    graph = dfg_top,
    from  = as.character(edges_top_filtered$from[i]),
    to    = as.character(edges_top_filtered$to[i])
  )
}

# Häufigkeit als Label
dfg_top <- set_edge_attrs(
  graph     = dfg_top,
  edge_attr = "label",
  values    = as.character(edges_top_filtered$n)
)

# Graph anzeigen
render_graph(dfg_top)


## =========================================================
## 14) Explorative Analyse des häufigsten Prozesspfads
## =========================================================

# a) Den am häufigsten vorkommenden Pfad bestimmen
top_path <- top_varianten %>%
  slice_head(n = 1) %>%
  pull(path)

cat("Top-Pfad:", top_path, "\n")

# Prozess-IDs dieses Pfads
top_proc_ids <- varianten %>%
  filter(path == top_path) %>%
  pull(proc_id)

# Events aller Prozesse mit diesem Pfad
top_proc_events <- checkpunkte_filtered %>%
  filter(proc_id %in% top_proc_ids) %>%
  arrange(proc_id, CpRegPosID)


## b) Dauer zwischen Schritten berechnen
checkpoint_times <- top_proc_events %>%
  group_by(proc_id) %>%
  arrange(CpRegPosID) %>%
  mutate(
    next_time = lead(TrackTime),
    duration_to_next = as.numeric(
      difftime(next_time, TrackTime, units = "mins")
    )
  ) %>%
  ungroup()


## c) Performance jedes Checkpoints (Statistiken)
checkpoint_stats <- checkpoint_times %>%
  filter(!is.na(duration_to_next)) %>%
  group_by(CheckpointID) %>%
  summarise(
    n                    = n(),
    mean_duration_min   = mean(duration_to_next),
    median_duration_min = median(duration_to_next),
    sd_duration         = sd(duration_to_next),
    p25                 = quantile(duration_to_next, 0.25),
    p75                 = quantile(duration_to_next, 0.75),
    max_duration        = max(duration_to_next),
    .groups             = "drop"
  ) %>%
  arrange(desc(median_duration_min))

print(checkpoint_stats)


## d) Visualisierungen

# Boxplot pro Checkpoint
ggplot(checkpoint_times,
       aes(x = factor(CheckpointID), y = duration_to_next)) +
  geom_boxplot(outlier.alpha = 0.2) +
  labs(
    title = "Performance je Checkpoint (Top-Prozess)",
    x     = "CheckpointID",
    y     = "Dauer bis zum nächsten Schritt [min]"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

# Median pro Checkpoint
ggplot(checkpoint_stats,
       aes(x = factor(CheckpointID), y = median_duration_min)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Median-Dauer pro Checkpoint – Top-Prozess",
    x     = "CheckpointID",
    y     = "Median-Dauer [min]"
  ) +
  theme_minimal()

# Heatmap der Schritt-zu-Schritt-Dauer
transition_stats <- checkpoint_times %>%
  group_by(proc_id) %>%
  mutate(next_cp = lead(CheckpointID)) %>%
  ungroup() %>%
  filter(!is.na(next_cp)) %>%
  group_by(CheckpointID, next_cp) %>%
  summarise(
    median_duration = median(duration_to_next),
    .groups = "drop"
  )

ggplot(transition_stats,
       aes(x = factor(CheckpointID),
           y = factor(next_cp),
           fill = median_duration)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(
    title = "Median Dauer zwischen Checkpoints (Top-Prozess)",
    x     = "Von",
    y     = "Nach",
    fill  = "Minuten"
  ) +
  theme_minimal()


## =========================================================
## 14.1) Outlier-Filtern (>14 Tage entfernen)
## =========================================================

checkpoint_times_clean <- checkpoint_times %>%
  group_by(proc_id) %>%
  filter(max(duration_to_next, na.rm = TRUE) <= 4320) %>%   # 4320 = 14 Tage
  ungroup()

# Kontrollplot
ggplot(checkpoint_times_clean,
       aes(x = factor(CheckpointID), y = duration_to_next)) +
  geom_boxplot(outlier.alpha = 0.2) +
  labs(
    title = "Performance je Checkpoint (Top-Prozess, Outlier entfernt)",
    x     = "CheckpointID",
    y     = "Dauer bis zum nächsten Schritt [min]"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

# Neue Statistik
checkpoint_stats <- checkpoint_times_clean %>%
  filter(!is.na(duration_to_next)) %>%
  group_by(CheckpointID) %>%
  summarise(
    n                    = n(),
    mean_duration_min   = mean(duration_to_next),
    median_duration_min = median(duration_to_next),
    sd_duration         = sd(duration_to_next),
    p25                 = quantile(duration_to_next, 0.25),
    p75                 = quantile(duration_to_next, 0.75),
    max_duration        = max(duration_to_next),
    .groups             = "drop"
  ) %>%
  arrange(desc(median_duration_min))

print(checkpoint_stats)

## =========================================================
## 15) Bottleneck-Analyse
## =========================================================

bottleneck_data <- checkpoint_times_clean %>%
  filter(!is.na(duration_to_next))

bottleneck_stats <- bottleneck_data %>%
  group_by(CheckpointID) %>%
  summarise(
    n                   = n(),
    mean_duration_min   = mean(duration_to_next),
    median_duration_min = median(duration_to_next),
    sd_duration         = sd(duration_to_next),
    p75_duration        = quantile(duration_to_next, 0.75),
    p95_duration        = quantile(duration_to_next, 0.95),
    max_duration        = max(duration_to_next),
    .groups             = "drop"
  )

bottleneck_by_median <- bottleneck_stats %>% arrange(desc(median_duration_min))
bottleneck_by_sd     <- bottleneck_stats %>% arrange(desc(sd_duration))
bottleneck_by_p75    <- bottleneck_stats %>% arrange(desc(p75_duration))
bottleneck_by_max    <- bottleneck_stats %>% arrange(desc(max_duration))

print("=== Ranking nach Median (Haupt-Bottleneck) ===")
print(bottleneck_by_median)

print("=== Ranking nach Instabilität (Varianz) ===")
print(bottleneck_by_sd)

print("=== Ranking nach Warteschlangen (75%-Quantil) ===")
print(bottleneck_by_p75)

print("=== Ranking nach Extremfällen (max Dauer) ===")
print(bottleneck_by_max)

prozess_reihenfolge <- c(1, 2, 5, 4, 6, 9, 13, 15, 21, 22, 25, 34, 35)

bottleneck_stats_ordered <- bottleneck_stats %>%
  mutate(CheckpointID = factor(CheckpointID, levels = prozess_reihenfolge)) %>%
  arrange(CheckpointID)

print("=== Bottleneck-Statistik in Prozessreihenfolge ===")
print(bottleneck_stats_ordered)


## =========================================================
## 16) Visualisierung Bottleneck nach Median
## =========================================================

ggplot(bottleneck_by_median,
       aes(x = factor(CheckpointID, levels = CheckpointID),
           y = median_duration_min)) +
  geom_col(fill = "firebrick") +
  labs(
    title = "Bottleneck-Analyse: Median-Dauer pro Checkpoint",
    x     = "CheckpointID",
    y     = "Median-Dauer [min]"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## =========================================================
## 16.5) Häufigkeitsverteilungen für CP 2, 9, 25
## =========================================================

plot_distribution_for_cp <- function(cp) {
  data_cp <- checkpoint_times_clean %>%
    filter(CheckpointID == cp, !is.na(duration_to_next))
  
  ggplot(data_cp, aes(x = duration_to_next)) +
    geom_histogram(aes(y = ..density..),
                   bins = 60, alpha = 0.4, fill = "steelblue") +
    geom_density(color = "red", size = 1) +
    theme_minimal() +
    labs(
      title = paste("Verteilung der Dauer – Checkpoint", cp),
      x     = "Dauer [min]",
      y     = "Dichte"
    ) +
    xlim(0, quantile(data_cp$duration_to_next, 0.99))
}

plot_distribution_for_cp(2)
plot_distribution_for_cp(9)
plot_distribution_for_cp(25)


## =========================================================
## 17) Schichtlogik 
## =========================================================

assign_schicht <- function(time) {
  mins <- hour(time) * 60 + minute(time)
  mins_adj <- ifelse(mins < 405, mins + 1440, mins)
  
  fr_start <- 390   # 06:30
  fr_end   <- 909   # 15:09
  sp_start <- 861   # 14:21
  sp_end   <- 1380  # 23:00
  na_start <- 1335  # 22:15
  na_end   <- 1845  # 06:45 (+24h)
  
  case_when(
    mins_adj >= na_start & mins_adj < na_end ~ "Nacht",
    mins_adj >= fr_start & mins_adj < fr_end ~ "Früh",
    mins_adj >= sp_start & mins_adj < sp_end ~ "Spät",
    TRUE ~ "Unbekannt"
  )
}


## =========================================================
## 17.1) Schichtanalyse
## =========================================================

checkpoint_times_clean <- checkpoint_times_clean %>%
  mutate(schicht = assign_schicht(TrackTime))

schicht_stats <- checkpoint_times_clean %>%
  filter(!is.na(duration_to_next)) %>%
  group_by(schicht) %>%
  summarise(
    n                = n(),
    median_duration  = median(duration_to_next),
    mean_duration    = mean(duration_to_next),
    sd_duration      = sd(duration_to_next),
    p75              = quantile(duration_to_next, 0.75),
    max              = max(duration_to_next),
    .groups          = "drop"
  )

print("=== Schicht-Performance ===")
print(schicht_stats)

ggplot(checkpoint_times_clean %>% filter(!is.na(duration_to_next)),
       aes(x = schicht, y = duration_to_next, fill = schicht)) +
  geom_boxplot(outlier.alpha = 0.2) +
  theme_minimal() +
  labs(
    title = "Zeit zwischen zwei Checkpunkten nach Schicht",
    x     = "Schicht",
    y     = "Dauer [min]"
  ) +
  scale_y_continuous(labels = scales::comma)


## =========================================================
## 18) Wochentaganalyse
## =========================================================

checkpoint_times_clean <- checkpoint_times_clean %>%
  mutate(
    weekday_num = wday(TrackTime, week_start = 1),
    weekday     = factor(
      weekday_num,
      levels = 1:7,
      labels = c("Mo", "Di", "Mi", "Do", "Fr", "Sa", "So")
    )
  )

weekday_stats <- checkpoint_times_clean %>%
  filter(!is.na(duration_to_next)) %>%
  group_by(weekday) %>%
  summarise(
    n                = n(),
    median_duration  = median(duration_to_next),
    mean_duration    = mean(duration_to_next),
    sd_duration      = sd(duration_to_next),
    p75_duration     = quantile(duration_to_next, 0.75),
    max_duration     = max(duration_to_next),
    .groups          = "drop"
  )

print("=== Performance je Wochentag ===")
print(weekday_stats)

ggplot(checkpoint_times_clean %>% filter(!is.na(duration_to_next)),
       aes(x = weekday, y = duration_to_next, fill = weekday)) +
  geom_boxplot(outlier.alpha = 0.2) +
  theme_minimal() +
  labs(
    x = "Wochentag", y = "Dauer [min]"
  ) +
  scale_y_continuous(labels = scales::comma)

ggplot(weekday_stats,
       aes(x = weekday, y = median_duration, group = 1)) +
  geom_line(color = "firebrick", size = 1.2) +
  geom_point(size = 3, color = "firebrick") +
  theme_minimal() +
  labs(
    title = "Median-Dauer pro Wochentag",
    x     = "Wochentag",
    y     = "Median [min]"
  )


## =========================================================
## 19) Analyse der kritischen CP: 2, 9, 25
## =========================================================

kritische_cps <- c(2, 9, 25)

kritische_daten <- checkpoint_times_clean %>%
  filter(CheckpointID %in% kritische_cps,
         !is.na(duration_to_next)) %>%
  mutate(
    weekday_num = wday(TrackTime, week_start = 1),
    weekday     = factor(
      weekday_num,
      levels = 1:7,
      labels = c("Mo","Di","Mi","Do","Fr","Sa","So")
    ),
    schicht = assign_schicht(TrackTime),
    hour    = hour(TrackTime)
  )

## Verhalten über den Tag
cp_hourly <- kritische_daten %>%
  group_by(CheckpointID, hour) %>%
  summarise(
    median_duration = median(duration_to_next),
    n = n(),
    .groups = "drop"
  )

ggplot(cp_hourly,
       aes(x = hour, y = median_duration,
           color = factor(CheckpointID))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal() +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Median-Dauer je Stunde für CP 2, 9, 25",
    x     = "Stunde",
    y     = "Median [min]"
  )


## Verhalten nach Schichten
cp_schicht <- kritische_daten %>%
  group_by(CheckpointID, schicht) %>%
  summarise(
    median_duration = median(duration_to_next),
    n               = n(),
    .groups         = "drop"
  )

ggplot(cp_schicht,
       aes(x = schicht, y = median_duration,
           fill = factor(CheckpointID))) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Dauer nach Schicht für CP 2, 9, 25",
    x     = "Schicht",
    y     = "Median [min]"
  )


## Verhalten nach Wochentag
cp_weekday <- kritische_daten %>%
  group_by(CheckpointID, weekday) %>%
  summarise(
    median_duration = median(duration_to_next),
    n               = n(),
    .groups         = "drop"
  )

ggplot(cp_weekday,
       aes(x = weekday, y = median_duration,
           color = factor(CheckpointID),
           group = CheckpointID)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Dauer nach Wochentag für CP 2, 9, 25",
    x     = "Wochentag",
    y     = "Median [min]"
  )


## =========================================================
## 20) Ankünfte analysieren
## =========================================================

prozesse_ankunft <- prozesse_clean %>%
  mutate(
    start_date  = as.Date(start_time),
    start_hour  = hour(start_time),
    weekday_num = wday(start_time, week_start = 1),
    weekday     = factor(
      weekday_num,
      levels = 1:7,
      labels = c("Mo","Di","Mi","Do","Fr","Sa","So")
    ),
    schicht     = assign_schicht(start_time)
  )

# Start pro Stunde
ankunft_stunde <- prozesse_ankunft %>% count(start_hour)

ggplot(ankunft_stunde,
       aes(x = start_hour, y = n)) +
  geom_line(size = 1.2, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  theme_minimal() +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Anzahl Prozessstarts je Stunde",
    x     = "Stunde",
    y     = "Anzahl"
  )

# Start nach Schicht
ankunft_schicht <- prozesse_ankunft %>%
  count(schicht) %>%
  mutate(schicht = factor(schicht,
                          levels = c("Früh", "Spät", "Nacht")))

ggplot(ankunft_schicht,
       aes(x = schicht, y = n, fill = schicht)) +
  geom_col() +
  theme_minimal() +
  labs(
    title = "Anzahl Prozessstarts nach Schicht",
    x     = "Schicht",
    y     = "Anzahl"
  )


# Start nach Wochentag
ankunft_wochentag <- prozesse_ankunft %>% count(weekday)

ggplot(ankunft_wochentag,
       aes(x = weekday, y = n)) +
  geom_line(group = 1, size = 1.2, color = "firebrick") +
  geom_point(size = 3, color = "firebrick") +
  theme_minimal() +
  labs(
    x = "Wochentag", y = "Anzahl"
  )


## Heatmap Ankunft: Wochentag × Schicht
ankunft_heat <- prozesse_ankunft %>%
  count(weekday, schicht) %>%
  mutate(schicht = factor(schicht,
                          levels = c("Früh", "Spät", "Nacht")))

ggplot(ankunft_heat,
       aes(x = weekday, y = schicht, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(
    title = "Heatmap der Prozessstarts",
    fill  = "Anzahl"
  )


## =========================================================
## 21) Abgänge analysieren
## =========================================================

prozesse_abgang <- prozesse_clean %>%
  mutate(
    end_date      = as.Date(end_time),
    end_hour      = hour(end_time),
    weekday_num_end = wday(end_time, week_start = 1),
    weekday_end     = factor(
      weekday_num_end,
      levels = 1:7,
      labels = c("Mo","Di","Mi","Do","Fr","Sa","So")
    ),
    schicht_end     = assign_schicht(end_time)
  )

# Abgänge pro Stunde
abgang_stunde <- prozesse_abgang %>% count(end_hour)

ggplot(abgang_stunde,
       aes(x = end_hour, y = n)) +
  geom_line(size = 1.2, color = "darkgreen") +
  geom_point(size = 2, color = "darkgreen") +
  theme_minimal() +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Anzahl Prozessenden je Stunde",
    x     = "Stunde",
    y     = "Anzahl"
  )


# Abgänge pro Schicht
abgang_schicht <- prozesse_abgang %>%
  count(schicht_end) %>%
  mutate(schicht_end = factor(schicht_end,
                              levels = c("Früh", "Spät", "Nacht")))

ggplot(abgang_schicht,
       aes(x = schicht_end, y = n, fill = schicht_end)) +
  geom_col() +
  theme_minimal() +
  labs(
    title = "Anzahl Prozessenden nach Schicht",
    x     = "Schicht (Ende)",
    y     = "Anzahl"
  )


# Abgänge pro Wochentag
abgang_wochentag <- prozesse_abgang %>% count(weekday_end)

ggplot(abgang_wochentag,
       aes(x = weekday_end, y = n)) +
  geom_line(group = 1, size = 1.2, color = "purple") +
  geom_point(size = 3, color = "purple") +
  theme_minimal() +
  labs(
    title = "Anzahl Prozessenden je Wochentag",
    x     = "Wochentag (Ende)",
    y     = "Anzahl"
  )


# Heatmap Abgänge
abgang_heat <- prozesse_abgang %>% count(weekday_end, schicht_end)

ggplot(abgang_heat,
       aes(x = weekday_end, y = schicht_end, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(
    title = "Heatmap der Prozessenden",
    fill  = "Anzahl"
  )


## =========================================================
## 22) Dauerstatistiken nach Stunde / Wochentag / Schicht (Ende)
## =========================================================

abgang_stats_stunde <- prozesse_abgang %>%
  group_by(end_hour) %>%
  summarise(
    n        = n(),
    mean_h   = mean(duration_h),
    median_h = median(duration_h),
    sd_h     = sd(duration_h),
    p25_h    = quantile(duration_h, 0.25),
    p75_h    = quantile(duration_h, 0.75),
    min_h    = min(duration_h),
    max_h    = max(duration_h),
    .groups  = "drop"
  )

print(abgang_stats_stunde)



abgang_stats_wochentag <- prozesse_abgang %>%
  group_by(weekday_end) %>%
  summarise(
    n        = n(),
    mean_h   = mean(duration_h),
    median_h = median(duration_h),
    sd_h     = sd(duration_h),
    p25_h    = quantile(duration_h, 0.25),
    p75_h    = quantile(duration_h, 0.75),
    min_h    = min(duration_h),
    max_h    = max(duration_h),
    .groups  = "drop"
  )

print(abgang_stats_wochentag)


abgang_stats_schicht <- prozesse_abgang %>%
  group_by(schicht_end) %>%
  summarise(
    n        = n(),
    mean_h   = mean(duration_h),
    median_h = median(duration_h),
    sd_h     = sd(duration_h),
    p25_h    = quantile(duration_h, 0.25),
    p75_h    = quantile(duration_h, 0.75),
    min_h    = min(duration_h),
    max_h    = max(duration_h),
    .groups  = "drop"
  )

print(abgang_stats_schicht)


## =========================================================
## 23) Regressionsanalyse
## =========================================================

model_hour    <- lm(duration_h ~ start_hour, data = prozesse_ankunft)
model_schicht <- lm(duration_h ~ schicht,    data = prozesse_ankunft)
model_weekday <- lm(duration_h ~ weekday,    data = prozesse_ankunft)

model_full <- lm(duration_h ~ start_hour + schicht + weekday,
                 data = prozesse_ankunft)

summary(model_hour)
summary(model_schicht)
summary(model_weekday)
summary(model_full)


## =========================================================
## 24) Starts vs Ends vs Prozessdauer
## =========================================================

plot_stunde <- prozesse_abgang %>%
  mutate(
    start_hour = hour(start_time),
    end_hour   = hour(end_time)
  ) %>%
  group_by(start_hour) %>%
  summarise(
    starts       = n(),
    median_dauer = median(duration_h),
    .groups      = "drop"
  ) %>%
  full_join(
    prozesse_abgang %>%
      group_by(end_hour) %>%
      summarise(ends = n(), .groups = "drop"),
    by = c("start_hour" = "end_hour")
  ) %>%
  rename(hour = start_hour)

ggplot(plot_stunde,
       aes(x = hour)) +
  geom_line(aes(y = starts), color = "steelblue", size = 1.2) +
  geom_line(aes(y = ends),   color = "darkgreen", size = 1.2) +
  geom_line(aes(
    y = rescale(median_dauer,
                to = c(min(starts, na.rm = TRUE),
                       max(starts, na.rm = TRUE)))
  ),
  color = "firebrick", size = 1.2, linetype = "dashed") +
  theme_minimal() +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title    = "Starts, Ends & Prozessdauer pro Stunde",
    x        = "Stunde",
    y        = "Anzahl",
    subtitle = "Rote Linie: Median-Prozessdauer (skaliert)"
  )


## Heatmap Starts - Ends pro Wochentag
heat_week <- prozesse_ankunft %>%
  count(weekday, name = "starts") %>%
  left_join(
    prozesse_abgang %>% count(weekday_end, name = "ends"),
    by = c("weekday" = "weekday_end")
  ) %>%
  mutate(diff = starts - ends)

ggplot(heat_week,
       aes(x = weekday, y = 1, fill = diff)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(
    title = "Differenz Starts – Ends pro Wochentag",
    fill  = "Differenz"
  )

#Ankünfte analysieren
ankunft_stats_schicht <- prozesse_ankunft %>%
  group_by(schicht) %>%
  summarise(
    n        = n(),
    mean_h   = mean(duration_h, na.rm = TRUE),
    median_h = median(duration_h, na.rm = TRUE),
    sd_h     = sd(duration_h, na.rm = TRUE),
    p25_h    = quantile(duration_h, 0.25, na.rm = TRUE),
    p75_h    = quantile(duration_h, 0.75, na.rm = TRUE),
    min_h    = min(duration_h, na.rm = TRUE),
    max_h    = max(duration_h, na.rm = TRUE),
    .groups  = "drop"
  )

ankunft_stats_schicht


## Vergleich Schicht Start/Ende
schicht_compare <- ankunft_stats_schicht %>%
  rename(
    starts       = n,
    median_start = median_h
  ) %>%
  left_join(
    abgang_stats_schicht %>%
      rename(ends = n, median_end = median_h),
    by = c("schicht" = "schicht_end")
  )

print(schicht_compare)

library(writexl)

write_xlsx(schicht_compare, "schicht_compare.xlsx")


## =========================================================
## 25) Starts vs Ends vs Prozessdauer nach Wochentag
## =========================================================

# Anzahl Prozessstarts pro Wochentag
starts_weekday <- prozesse_ankunft %>% 
  count(weekday)

# Anzahl Prozessenden pro Wochentag
ends_weekday <- prozesse_abgang %>% 
  count(weekday_end) %>% 
  rename(weekday = weekday_end)

# Median-Prozessdauer pro Wochentag
dauer_weekday <- prozesse_ankunft %>%
  group_by(weekday) %>%
  summarise(
    median_dauer = median(duration_h, na.rm = TRUE),  # wichtig: na.rm = TRUE
    .groups = "drop"
  )

# Alles zusammenführen + Median-Dauer auf die Skala der Starts rescalen
plot_weekday <- starts_weekday %>%
  rename(starts = n) %>%
  left_join(ends_weekday %>% rename(ends = n), by = "weekday") %>%
  left_join(dauer_weekday, by = "weekday") %>%
  mutate(
    median_dauer_scaled = scales::rescale(
      median_dauer,
      to = range(starts, na.rm = TRUE)
    )
  )

# Plot
ggplot(plot_weekday, aes(x = weekday)) +
  geom_line(aes(y = starts, group = 1), color = "steelblue", size = 1.2) +
  geom_point(aes(y = starts), color = "steelblue", size = 2) +
  geom_line(aes(y = ends, group = 1), color = "darkgreen", size = 1.2) +
  geom_point(aes(y = ends), color = "darkgreen", size = 2) +
  geom_line(
    aes(y = median_dauer_scaled, group = 1),
    color = "firebrick",
    linetype = "dashed",
    size = 1.2
  ) +
  theme_minimal() +
  labs(
    title    = "Starts, Ends und Median-Dauer pro Wochentag",
    x        = "Wochentag",
    y        = "Anzahl",
    subtitle = "Rote Linie = Median-Dauer (skaliert)"
  )

## =========================================================
## 26) Prozessdauer-Analyse (Verteilung)
## =========================================================

prozesse_clean %>%
  summarise(
    n_prozesse = n(),
    min_h      = min(duration_h),
    p50_h      = quantile(duration_h, 0.50),
    p90_h      = quantile(duration_h, 0.90),
    p95_h      = quantile(duration_h, 0.95),
    p99_h      = quantile(duration_h, 0.99),
    max_h      = max(duration_h)
  )

# Prozesse < 120h
prozesse_clean_120 <- prozesse_clean %>%
  filter(duration_h < 120)

# Runden
prozesse_clean_plot <- prozesse_clean_120 %>%
  mutate(duration_h_round = round(duration_h)) %>%
  count(duration_h_round)

ggplot(prozesse_clean_plot,
       aes(x = duration_h_round, y = n)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  geom_smooth(method = "loess", se = FALSE,
              color = "red", size = 1) +
  theme_minimal() +
  labs(
    title = "Häufigkeitskurve der Gesamtdurchlaufzeit (<120h)",
    x     = "Durchlaufzeit [h]",
    y     = "Anzahl Prozesse"
  ) +
  scale_x_continuous(breaks = seq(0, 120, 10))

