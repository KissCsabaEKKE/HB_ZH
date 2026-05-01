set.seed(seed)

K1_A <- round(rnorm(10, mean = 71, sd = 3))
K1_B <- round(rnorm(8, mean = 66, sd = 3))

K2_A <- c(round(rnorm(8, mean = 70, sd = 4)), 94, 103, 112)
K2_B <- c(round(rnorm(7, mean = 66, sd = 4)), 88, 106, 121)

adat_teszt <- data.frame(
  csoport = c(
    rep("K1_A", length(K1_A)),
    rep("K1_B", length(K1_B)),
    rep("K2_A", length(K2_A)),
    rep("K2_B", length(K2_B))
  ),
  ertek = c(K1_A, K1_B, K2_A, K2_B)
)

bio_alvas <- round(seq(5.3, 8.8, length.out = 10) + rnorm(10, 0, 0.25), 1)
bio_energia <- round(1.2 * bio_alvas - 2 + rnorm(10, 0, 0.8))
bio_energia <- pmin(pmax(bio_energia, 1), 10)

sport_alvas <- round(seq(4.8, 9.0, length.out = 11) + rnorm(11, 0, 0.25), 1)
sport_energia <- round(1.1 * sport_alvas - 2 + rnorm(11, 0, 0.9))
sport_energia <- pmin(pmax(sport_energia, 1), 10)

adat_korrelacio <- data.frame(
  tanszek = c(
    rep("Biológia", length(bio_alvas)),
    rep("Sporttudomány", length(sport_alvas))
  ),
  alvas_ora = c(bio_alvas, sport_alvas),
  energia_pont = c(bio_energia, sport_energia)
)

n <- 2200

adat_generalas <- data.frame(
  Csoport = sample(
    c("Elsőéves", "Másodéves", "Harmadéves", "Sportoló", "Nem sportoló"),
    n,
    replace = TRUE
  ),
  Napi_aktivitas = sample(
    c("alacsony", "közepes", "magas", "nagyon magas"),
    n,
    replace = TRUE
  ),
  Alvasido = round(rnorm(n, mean = 7.1, sd = 1.2), 2),
  Kepernyoido = round(rnorm(n, mean = 5.4, sd = 1.8), 2)
)

adat_generalas$Alvasido[adat_generalas$Alvasido < 3] <- 3
adat_generalas$Kepernyoido[adat_generalas$Kepernyoido < 0] <- 0

szakok <- c(
  "Biológia",
  "Sporttudomány",
  "Pszichológia",
  "Földrajz",
  "Környezettudomány",
  "Óvodapedagógia"
)

adat_szures <- data.frame(
  Eletkor = sample(18:35, 800, replace = TRUE),
  Nem = sample(c("Férfi", "Nő"), 800, replace = TRUE),
  Szak = sample(szakok, 800, replace = TRUE),
  Pont = sample(1:5, 800, replace = TRUE)
)

adat_szures[1:180, "Eletkor"] <- sample(19:24, 180, replace = TRUE)
adat_szures[1:180, "Szak"] <- "Biológia"
adat_szures[1:180, "Nem"] <- sample(c("Férfi", "Nő"), 180, replace = TRUE)
adat_szures[1:180, "Pont"] <- sample(1:5, 180, replace = TRUE)

feladat_szoveg <- paste0(
"# ============================================================\n",
"# Humánbiológia II. – Alkalmazott biometria ZH\n",
"# Neptun: ", neptun, "\n",
"# ZH: ", ZH, "\n",
"# Seed: ", seed, "\n",
"# ============================================================\n\n",

"# 1. feladat – Két csoport összehasonlítása, eloszlás és tesztválasztás\n",
"# Használja az adat_teszt nevű data.frame-et.\n",
"# Két csoportban két-két független mérési sorozat található. Vizsgálja a normalitást, készítsen boxplotokat, majd válasszon megfelelő statisztikai próbát.\n\n",

"# 2. feladat – Korreláció és regresszió\n",
"# Használja az adat_korrelacio nevű data.frame-et. Vizsgálja az alvásidő és energiaszint kapcsolatát tanszékenként.\n\n",

"# 3. feladat – Adatmátrix és vizualizáció\n",
"# Használja az adat_generalas nevű data.frame-et. Készítsen boxplotokat az Alvasido és Kepernyoido változókra a megadott szűrések szerint.\n\n",

"# 4. feladat – Szűrés, gyakoriság és relatív gyakoriság\n",
"# Használja az adat_szures nevű data.frame-et. Szűrje ki a 19–24 éves Biológia szakos hallgatókat, majd számoljon gyakoriságokat és relatív gyakoriságokat.\n\n"
)
