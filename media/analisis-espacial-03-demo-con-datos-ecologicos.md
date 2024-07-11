Análisis espacial (3). Autocorrelación espacial. Demo con datos
ecológicos
================
José Ramón Martínez Batlle
11-07-2024

# Autocorrelación espacial

> Modificado a partir de:
> <https://github.com/biogeografia-202202/material-de-apoyo/blob/master/practicas/demos-personalizadas/practica-99-tu-manuscrito-3-resultados-fabaceae.md>

Fijar un directorio de trabajo no es recomendable, mejor trabaja por
proyecto. En cualquier caso, si no quieres o no puedes crear un
proyecto, usa la sentencia que verás abajo, cambiando `TU_DIRECTORIO`
por la ruta del directorio donde tengas almacenados tus datos y tus
scripts.

``` r
if(interactive()) {
  tryCatch(
    setwd(dirname(rstudioapi::getSourceEditorContext()$path)),
    error = function(e) {
      cat('Probablemente ya el directorio de trabajo está fijado correctamente',
          'o quizá el directorio no existe. Este fue el error devuelto:\n')
      e
    })
}
```

Cargar paquetes.

``` r
library(vegan)
library(sf)
library(tidyverse)
library(tmap)
library(kableExtra)
library(foreach)
library(leaps)
library(caret)
gh_content <- 'https://raw.githubusercontent.com/'
gh_zonal_stats <- paste0(gh_content,
                         'geofis/zonal-statistics/0b2e95aaee87bf326cf132d28f4bd15220bb4ec7/out/')
repo_analisis <- 'biogeografia-master/scripts-de-analisis-BCI/master'
repo_sem202202 <- 'biogeografia-202202/material-de-apoyo/master/practicas/'
devtools::source_url(paste0(gh_content, repo_analisis, '/biodata/funciones.R'))
devtools::source_url(paste0(gh_content, repo_sem202202, 'train.R'))
devtools::source_url(paste0(gh_content, repo_sem202202, 'funciones.R'))
```

Carga tu matriz de comunidad, que habrás generado en la práctica 2, y
elige un umbral para especies raras o rangos de registros de presencia
para seleccionar especies en una nueva matriz de comunidad.

``` r
res <- 5 #Resolución H3
mc_orig <- readRDS('demo-autocorrelacion/matriz_de_comunidad_fabaceae.RDS')
nrow(mc_orig) #Número de filas, equivale a número de hexágonos con registros de presencia
```

    ## [1] 236

``` r
ncol(mc_orig)  #Número de columnas, equivale a número de especies, riqueza
```

    ## [1] 358

``` r
data.frame(Especies = names(mc_orig)) %>% 
  kable(booktabs=T) %>%
  kable_styling(latex_options = c("HOLD_position", "scale_down")) %>%
  gsub(' NA ', '', .) #Lista de especies
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Especies
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Poitea galegoides Vent.
</td>
</tr>
<tr>
<td style="text-align:left;">
Canavalia rosea (Sw.) DC.
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna occidentalis (L.) Link
</td>
</tr>
<tr>
<td style="text-align:left;">
Flemingia strobilifera (L.) W.T.Aiton
</td>
</tr>
<tr>
<td style="text-align:left;">
Alysicarpus vaginalis (L.) DC.
</td>
</tr>
<tr>
<td style="text-align:left;">
Clitoria ternatea L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Acacia mangium Willd.
</td>
</tr>
<tr>
<td style="text-align:left;">
Cajanus cajan (L.) Huth
</td>
</tr>
<tr>
<td style="text-align:left;">
Mimosa pudica L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Albizia lebbeck (L.) Benth.
</td>
</tr>
<tr>
<td style="text-align:left;">
Centrosema plumieri (Turpin ex Pers.) Benth.
</td>
</tr>
<tr>
<td style="text-align:left;">
Mucuna pruriens (L.) DC.
</td>
</tr>
<tr>
<td style="text-align:left;">
Bauhinia purpurea L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Caesalpinia pulcherrima (L.) Sw.
</td>
</tr>
<tr>
<td style="text-align:left;">
Leucaena leucocephala (Lam.) de Wit
</td>
</tr>
<tr>
<td style="text-align:left;">
Haematoxylum campechianum L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Tamarindus indica L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Entada gigas (L.) Fawc. & Rendle
</td>
</tr>
<tr>
<td style="text-align:left;">
Bauhinia monandra Kurz
</td>
</tr>
<tr>
<td style="text-align:left;">
Delonix regia (Bojer ex Hook.) Raf.
</td>
</tr>
<tr>
<td style="text-align:left;">
Prosopis juliflora (Sw.) DC.
</td>
</tr>
<tr>
<td style="text-align:left;">
Centrosema virginianum (L.) Benth.
</td>
</tr>
<tr>
<td style="text-align:left;">
Mimosa pigra L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista nictitans (L.) Moench
</td>
</tr>
<tr>
<td style="text-align:left;">
Trifolium repens L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Crotalaria incana L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Crotalaria pallida Aiton
</td>
</tr>
<tr>
<td style="text-align:left;">
Crotalaria retusa L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Hymenaea courbaril L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Crotalaria verrucosa L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Samanea saman (Jacq.) Merr.
</td>
</tr>
<tr>
<td style="text-align:left;">
Albizia julibrissin Durazz.
</td>
</tr>
<tr>
<td style="text-align:left;">
Abrus precatorius L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmodium incanum (Sw.) DC.
</td>
</tr>
<tr>
<td style="text-align:left;">
Dalbergia ecastaphyllum (L.) Taub.
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna siamea (Lam.) H.S.Irwin & Barneby
</td>
</tr>
<tr>
<td style="text-align:left;">
Macroptilium lathyroides (L.) Urb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Pithecellobium dulce (Roxb.) Benth.
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmodium ciliare (Muhl. ex Willd.) DC.
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmodium tortuosum (Sw.) DC.
</td>
</tr>
<tr>
<td style="text-align:left;">
Crotalaria juncea L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Pithecellobium circinale (L.) Benth.
</td>
</tr>
<tr>
<td style="text-align:left;">
Phaseolus vulgaris L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Inga vera Willd.
</td>
</tr>
<tr>
<td style="text-align:left;">
Centrosema pubescens Benth.
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna spectabilis (DC.) H.S.Irwin & Barneby
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna obtusifolia (L.) H.S.Irwin & Barneby
</td>
</tr>
<tr>
<td style="text-align:left;">
Arcoa gonavensis Urb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Poitea campanilla DC.
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachellia macracantha (Humb. & Bonpl. ex Willd.) Seigler & Ebinger
</td>
</tr>
<tr>
<td style="text-align:left;">
Pictetia sulcata (P.Beauv.) Beyra & Lavin
</td>
</tr>
<tr>
<td style="text-align:left;">
Biancaea decapetala (Roth) O.Deg.
</td>
</tr>
<tr>
<td style="text-align:left;">
Calliandra haematocephala Hassk.
</td>
</tr>
<tr>
<td style="text-align:left;">
Rhodopis planisiliqua var. lowdenii (Judd) Alain
</td>
</tr>
<tr>
<td style="text-align:left;">
Guilandina sphaerosperma (Urb. & Ekman) Britton
</td>
</tr>
<tr>
<td style="text-align:left;">
Calliandra pedicellata Benth.
</td>
</tr>
<tr>
<td style="text-align:left;">
Rhynchosia reticulata (Sw.) DC.
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista lineata var. brachyloba (Griseb.) H.S.Irwin & Barneby
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna alata (L.) Roxb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Libidibia coriaria (Jacq.) Schltdl.
</td>
</tr>
<tr>
<td style="text-align:left;">
Strongylodon macrobotrys A.Gray
</td>
</tr>
<tr>
<td style="text-align:left;">
Calliandra surinamensis Benth.
</td>
</tr>
<tr>
<td style="text-align:left;">
Coronilla varia L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Calliandra houstoniana var. calothyrsus (Meisn.) Barneby
</td>
</tr>
<tr>
<td style="text-align:left;">
Inga edulis Mart.
</td>
</tr>
<tr>
<td style="text-align:left;">
Grona triflora (L.) H.Ohashi & K.Ohashi
</td>
</tr>
<tr>
<td style="text-align:left;">
Erythrina variegata L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Gliricidia sepium (Jacq.) Kunth
</td>
</tr>
<tr>
<td style="text-align:left;">
Guilandina bonduc L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Leucaena macrophylla Benth.
</td>
</tr>
<tr>
<td style="text-align:left;">
Macroptilium atropurpureum (DC.) Urb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Adenanthera pavonina L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Rhynchosia phaseoloides (Sw.) DC.
</td>
</tr>
<tr>
<td style="text-align:left;">
Parkinsonia aculeata L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna atomaria (L.) H.S.Irwin & Barneby
</td>
</tr>
<tr>
<td style="text-align:left;">
Zornia reticulata Sm.
</td>
</tr>
<tr>
<td style="text-align:left;">
Cassia fistula L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Stylosanthes hamata (L.) Taub.
</td>
</tr>
<tr>
<td style="text-align:left;">
Barnebydendron riedelii (Tul.) J.H.Kirkbr.
</td>
</tr>
<tr>
<td style="text-align:left;">
Bauhinia variegata L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Tara vesicaria (L.) Molinari, Sánchez Och. & Mayta
</td>
</tr>
<tr>
<td style="text-align:left;">
Tara Molina
</td>
</tr>
<tr>
<td style="text-align:left;">
Leucaena leucocephala subsp. leucocephala
</td>
</tr>
<tr>
<td style="text-align:left;">
Anadenanthera peregrina (L.) Speg.
</td>
</tr>
<tr>
<td style="text-align:left;">
Glycine max (L.) Merr.
</td>
</tr>
<tr>
<td style="text-align:left;">
Fabaceae
</td>
</tr>
<tr>
<td style="text-align:left;">
Parasenegalia vogeliana (Steud.) Seigler & Ebinger
</td>
</tr>
<tr>
<td style="text-align:left;">
Caesalpinia Plum. ex L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Poitea Vent.
</td>
</tr>
<tr>
<td style="text-align:left;">
Canavalia DC.
</td>
</tr>
<tr>
<td style="text-align:left;">
Rhynchosia Lour.
</td>
</tr>
<tr>
<td style="text-align:left;">
Bauhinia divaricata L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista fasciculata (Michx.) Greene
</td>
</tr>
<tr>
<td style="text-align:left;">
Jupunba glauca (Urb.) Britton & Rose
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna Mill.
</td>
</tr>
<tr>
<td style="text-align:left;">
Lablab purpureus (L.) Sweet
</td>
</tr>
<tr>
<td style="text-align:left;">
Arachis hypogaea L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Crotalaria pallida var. obovata (G.Don) Polhill
</td>
</tr>
<tr>
<td style="text-align:left;">
Erythrina poeppigiana (Walp.) O.F.Cook
</td>
</tr>
<tr>
<td style="text-align:left;">
Arachis glabrata Benth.
</td>
</tr>
<tr>
<td style="text-align:left;">
Arachis pintoi Krapov. & W.C.Greg.
</td>
</tr>
<tr>
<td style="text-align:left;">
Acacia melanoxylon R.Br.
</td>
</tr>
<tr>
<td style="text-align:left;">
Albizia niopoides (Spruce ex Benth.) Burkart
</td>
</tr>
<tr>
<td style="text-align:left;">
Galactia schomburgkii Urb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Piscidia ekmanii Rudd
</td>
</tr>
<tr>
<td style="text-align:left;">
Galactia buchii Urb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Mimosa domingensis (Bertero ex DC.) Benth.
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmanthus leptophyllus Kunth
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmodium glabrum (Mill.) DC.
</td>
</tr>
<tr>
<td style="text-align:left;">
Indigofera colutea (Burm.f.) Merr.
</td>
</tr>
<tr>
<td style="text-align:left;">
Leucaena trichodes (Jacq.) Benth.
</td>
</tr>
<tr>
<td style="text-align:left;">
Calliandra haematomma (Bertero ex DC.) Benth.
</td>
</tr>
<tr>
<td style="text-align:left;">
Caesalpinia barahonensis Urb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Coursetia caribaea (Jacq.) Lavin
</td>
</tr>
<tr>
<td style="text-align:left;">
Poitea paucifolia (DC.) Lavin
</td>
</tr>
<tr>
<td style="text-align:left;">
Denisophytum buchii (Urb.) Gagnon & G.P.Lewis
</td>
</tr>
<tr>
<td style="text-align:left;">
Parasenegalia skleroxyla (Tussac) Seigler & Ebinger
</td>
</tr>
<tr>
<td style="text-align:left;">
Caesalpinia brasiliensis L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna angustisiliqua (Lam.) H.S.Irwin & Barneby
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista lineata (Sw.) Greene
</td>
</tr>
<tr>
<td style="text-align:left;">
Phaseolus lunatus L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Cojoba arborea (L.) Britton & Rose
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmodium affine Schltdl.
</td>
</tr>
<tr>
<td style="text-align:left;">
Cassia grandis L.f.
</td>
</tr>
<tr>
<td style="text-align:left;">
Mimosa diplacantha Benth.
</td>
</tr>
<tr>
<td style="text-align:left;">
Inga vera subsp. vera
</td>
</tr>
<tr>
<td style="text-align:left;">
Tephrosia cinerea (L.) Pers.
</td>
</tr>
<tr>
<td style="text-align:left;">
Acacia scleroxyla Tussac
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmanthus Willd.
</td>
</tr>
<tr>
<td style="text-align:left;">
Acaciella glauca (L.) L.Rico
</td>
</tr>
<tr>
<td style="text-align:left;">
Mucuna urens (L.) Medik.
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmodium axillare (Sw.) DC.
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachellia barahonensis (Urb. & Ekman) Seigler & Ebinger
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachellia oviedoensis (R.G.García & M.M.Mejía) Seigler & Ebinger
</td>
</tr>
<tr>
<td style="text-align:left;">
Lysiloma sabicu Benth.
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachellia cucuyo (Barneby & Zanoni) Seigler & Ebinger
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachellia tortuosa (L.) Seigler & Ebinger
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmanthus virgatus (L.) Willd.
</td>
</tr>
<tr>
<td style="text-align:left;">
Canavalia nitida (Cav.) Piper
</td>
</tr>
<tr>
<td style="text-align:left;">
Sigmoidotropis elegans (Piper) A.Delgado
</td>
</tr>
<tr>
<td style="text-align:left;">
Galactia dubia DC.
</td>
</tr>
<tr>
<td style="text-align:left;">
Zapoteca portoricensis subsp. portoricensis
</td>
</tr>
<tr>
<td style="text-align:left;">
Sophora tomentosa var. occidentalis (L.) Isely
</td>
</tr>
<tr>
<td style="text-align:left;">
Crotalaria lotifolia L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Caesalpinia domingensis Urb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Libidibia monosperma (Tul.) Gagnon & G.P.Lewis
</td>
</tr>
<tr>
<td style="text-align:left;">
Dalea carthagenensis (Jacq.) J.F.Macbr.
</td>
</tr>
<tr>
<td style="text-align:left;">
Trifolium pratense L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Medicago lupulina L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista nictitans var. glabrata (Vogel) H.S.Irwin & Barneby
</td>
</tr>
<tr>
<td style="text-align:left;">
Lupinus mexicanus Cerv. ex Lag.
</td>
</tr>
<tr>
<td style="text-align:left;">
Genista monspessulana (L.) L.A.S.Johnson
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna ligustrina (L.) H.S.Irwin & Barneby
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista glandulosa var. pinetorum (Britton)
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna mexicana (Jacq.) H.S.Irwin & Barneby
</td>
</tr>
<tr>
<td style="text-align:left;">
Lonchocarpus neurophyllus Urb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Jupunba oppositifolia (Urb.) Britton & Rose
</td>
</tr>
<tr>
<td style="text-align:left;">
Brya buxifolia (Murray) Urb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Jupunba abbottii (Rose & Leonard) Britton & Rose
</td>
</tr>
<tr>
<td style="text-align:left;">
Lonchocarpus heptaphyllus (Poir.) DC.
</td>
</tr>
<tr>
<td style="text-align:left;">
Poitea glyciphylla (Poir.) Urb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Sigmoidotropis antillana (Urb.) A.Delgado
</td>
</tr>
<tr>
<td style="text-align:left;">
Mora ekmanii (Urb.) Britton & Rose
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna sophera (L.) Roxb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Rhodopis planisiliqua (L.) Urb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Cynometra portoricensis Krug & Urb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Dalbergia berteroi (DC.) Urb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Inga ruiziana G.Don
</td>
</tr>
<tr>
<td style="text-align:left;">
Mora abbottii Rose & Leonard
</td>
</tr>
<tr>
<td style="text-align:left;">
Lonchocarpus longipes Urb. & Ekman
</td>
</tr>
<tr>
<td style="text-align:left;">
Lathyrus articulatus L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Lupinus plattensis S.Watson
</td>
</tr>
<tr>
<td style="text-align:left;">
Trifolium dubium Sibth.
</td>
</tr>
<tr>
<td style="text-align:left;">
Cojoba samanensis R.G.García & Peguero
</td>
</tr>
<tr>
<td style="text-align:left;">
Ateleia gummifera (Bertero ex DC.) D.Dietr.
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna angustisiliqua var. angustisiliqua
</td>
</tr>
<tr>
<td style="text-align:left;">
Cojoba arborea var. arborea
</td>
</tr>
<tr>
<td style="text-align:left;">
Denisophytum pauciflorum (Griseb.) Gagnon & G.P.Lewis
</td>
</tr>
<tr>
<td style="text-align:left;">
Cojoba bahorucensis J.W.Grimes & R.García
</td>
</tr>
<tr>
<td style="text-align:left;">
Chloroleucon guantanamense (Britton) Britton & Rose
</td>
</tr>
<tr>
<td style="text-align:left;">
Neustanthus phaseoloides (Roxb.) Benth.
</td>
</tr>
<tr>
<td style="text-align:left;">
Aeschynomene americana L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Sesbania bispinosa (Jacq.) W.Wight
</td>
</tr>
<tr>
<td style="text-align:left;">
Cojoba filipes (Vent.) Barneby & J.W.Grimes
</td>
</tr>
<tr>
<td style="text-align:left;">
Pictetia obcordata DC.
</td>
</tr>
<tr>
<td style="text-align:left;">
Ormosia krugii Urb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Albizia berteroana (Balb. ex DC.) Fawc. & Rendle
</td>
</tr>
<tr>
<td style="text-align:left;">
Poitea multiflora (Sw.) Urb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Mimosa candollei R.Grether
</td>
</tr>
<tr>
<td style="text-align:left;">
Jupunba obovalis (A.Rich.) Britton & Rose
</td>
</tr>
<tr>
<td style="text-align:left;">
Inga laurina (Sw.) Willd.
</td>
</tr>
<tr>
<td style="text-align:left;">
Calliandra haematomma var. haematomma
</td>
</tr>
<tr>
<td style="text-align:left;">
Dalea carthagenensis var. carthagenensis
</td>
</tr>
<tr>
<td style="text-align:left;">
Pithecellobium Mart.
</td>
</tr>
<tr>
<td style="text-align:left;">
Inga Mill.
</td>
</tr>
<tr>
<td style="text-align:left;">
Tephrosia sinapou (Buc’hoz) A.Chev.
</td>
</tr>
<tr>
<td style="text-align:left;">
Zapoteca portoricensis (Jacq.) H.M.Hern.
</td>
</tr>
<tr>
<td style="text-align:left;">
Calliandra haematomma var. rivularis (Urb. & Ekman) Barneby
</td>
</tr>
<tr>
<td style="text-align:left;">
Pithecellobium domingense Alain
</td>
</tr>
<tr>
<td style="text-align:left;">
Andira inermis (W.Wright) DC.
</td>
</tr>
<tr>
<td style="text-align:left;">
Poitea galegoides var. galegoides
</td>
</tr>
<tr>
<td style="text-align:left;">
Melilotus albus Medik.
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna uniflora (Mill.) H.S.Irwin & Barneby
</td>
</tr>
<tr>
<td style="text-align:left;">
Mucuna Adans.
</td>
</tr>
<tr>
<td style="text-align:left;">
Peltophorum dubium var. berteroanum (Urb.) Barneby
</td>
</tr>
<tr>
<td style="text-align:left;">
Coulteria cubensis (Greenm.) Sotuyo & G.P.Lewis
</td>
</tr>
<tr>
<td style="text-align:left;">
Lonchocarpus sericeus (Poir.) Kunth ex DC.
</td>
</tr>
<tr>
<td style="text-align:left;">
Crotalaria L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna mexicana var. berteriana (DC.) H.S.Irwin & Barneby
</td>
</tr>
<tr>
<td style="text-align:left;">
Mimosa pudica var. unijuga (Duchass. & Walp.) Griseb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Dalbergia ovalis (L.) P.L.R.Moraes & L.P.Queiroz
</td>
</tr>
<tr>
<td style="text-align:left;">
Neptunia plena (L.) Benth.
</td>
</tr>
<tr>
<td style="text-align:left;">
Erythrina corallodendron L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Entada polystachya (L.) DC.
</td>
</tr>
<tr>
<td style="text-align:left;">
Rhodopis rudolphioides (Griseb.) L.P.Queiroz
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmodium Desv.
</td>
</tr>
<tr>
<td style="text-align:left;">
Poiretia punctata (Willd.) Desv.
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmodium intortum (Mill.) Urb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Aeschynomene americana var. americana
</td>
</tr>
<tr>
<td style="text-align:left;">
Crotalaria stipularia Desv.
</td>
</tr>
<tr>
<td style="text-align:left;">
Tephrosia purpurea (L.) Pers.
</td>
</tr>
<tr>
<td style="text-align:left;">
Barbieria pinnata (Pers.) Baill.
</td>
</tr>
<tr>
<td style="text-align:left;">
Calliandra Benth.
</td>
</tr>
<tr>
<td style="text-align:left;">
Cojoba zanonii (Barneby) Barneby & J.W.Grimes
</td>
</tr>
<tr>
<td style="text-align:left;">
Senegalia angustifolia (Lam.) Britton & Rose
</td>
</tr>
<tr>
<td style="text-align:left;">
Pterocarpus officinalis Jacq.
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna gundlachii (Urb.) H.S.Irwin & Barneby
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista nictitans var. diffusa (DC.) H.S.Irwin & Barneby
</td>
</tr>
<tr>
<td style="text-align:left;">
Lonchocarpus pycnophyllus Urb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Galactia P.Browne
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista Moench
</td>
</tr>
<tr>
<td style="text-align:left;">
Aeschynomene villosa Poir.
</td>
</tr>
<tr>
<td style="text-align:left;">
Acacia auriculiformis A.Cunn. ex Benth.
</td>
</tr>
<tr>
<td style="text-align:left;">
Albizia berteriana (DC.) Fawc. & Rendle
</td>
</tr>
<tr>
<td style="text-align:left;">
Indigofera subulata var. scabra (Roth) Meikle
</td>
</tr>
<tr>
<td style="text-align:left;">
Lespedeza juncea (L.f.) Pers.
</td>
</tr>
<tr>
<td style="text-align:left;">
Grona barbata (L.) H.Ohashi & K.Ohashi
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna septemtrionalis (Viv.) H.S.Irwin & Barneby
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachellia farnesiana (L.) Wight & Arn.
</td>
</tr>
<tr>
<td style="text-align:left;">
Mimosa ceratonia var. ceratonia
</td>
</tr>
<tr>
<td style="text-align:left;">
Aeschynomene pratensis Small
</td>
</tr>
<tr>
<td style="text-align:left;">
Erythrina berteroana Urb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Grona adscendens (Sw.) H.Ohashi & K.Ohashi
</td>
</tr>
<tr>
<td style="text-align:left;">
Erythrina leptopoda Urb. & Ekman
</td>
</tr>
<tr>
<td style="text-align:left;">
Mimosa L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Sophora tomentosa L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Zapoteca nervosa (Urb.) H.M.Hern.
</td>
</tr>
<tr>
<td style="text-align:left;">
Ateleia (DC.) D.Dietr.
</td>
</tr>
<tr>
<td style="text-align:left;">
Indigofera tinctoria L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista diphylla (L.) Greene
</td>
</tr>
<tr>
<td style="text-align:left;">
Zornia diphylla (L.) Pers.
</td>
</tr>
<tr>
<td style="text-align:left;">
Rhynchosia minima (L.) DC.
</td>
</tr>
<tr>
<td style="text-align:left;">
Albizia procera (Roxb.) Benth.
</td>
</tr>
<tr>
<td style="text-align:left;">
Apios Fabr.
</td>
</tr>
<tr>
<td style="text-align:left;">
Tephrosia Pers.
</td>
</tr>
<tr>
<td style="text-align:left;">
Pithecellobium unguis-cati (L.) Benth.
</td>
</tr>
<tr>
<td style="text-align:left;">
Galactia striata (Jacq.) Urb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna polyphylla var. montis-christi H.S.Irwin & Barneby
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna mexicana var. mexicana
</td>
</tr>
<tr>
<td style="text-align:left;">
Sophora albopetiolulata Leonard
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista portoricensis var. portoricensis
</td>
</tr>
<tr>
<td style="text-align:left;">
Machaerium lunatum (L.f.) Ducke
</td>
</tr>
<tr>
<td style="text-align:left;">
Grona barbata var. barbata
</td>
</tr>
<tr>
<td style="text-align:left;">
Ateleia microcarpa (Pers.) D.Dietr.
</td>
</tr>
<tr>
<td style="text-align:left;">
Calopogonium mucunoides Desv.
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna bicapsularis (L.) Roxb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Acacia confusa Merr.
</td>
</tr>
<tr>
<td style="text-align:left;">
Mucuna pruriens var. utilis (Wall. ex Wight) Baker ex Burck
</td>
</tr>
<tr>
<td style="text-align:left;">
Pachyrhizus erosus (L.) Urb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Zornia microphylla Desv.
</td>
</tr>
<tr>
<td style="text-align:left;">
Indigofera suffruticosa Mill.
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista pedicellaris var. adenosperma (Urb.) H.S.Irwin & Barneby
</td>
</tr>
<tr>
<td style="text-align:left;">
Tephrosia senna Kunth
</td>
</tr>
<tr>
<td style="text-align:left;">
Galactia longifolia (Jacq.) Benth.
</td>
</tr>
<tr>
<td style="text-align:left;">
Sesbania sericea (Willd.) Link
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista pilosa var. pilosa
</td>
</tr>
<tr>
<td style="text-align:left;">
Stylosanthes guianensis (Aubl.) Sw.
</td>
</tr>
<tr>
<td style="text-align:left;">
Indigofera subulata Vahl ex Poir.
</td>
</tr>
<tr>
<td style="text-align:left;">
Tephrosia purpurea subsp. purpurea
</td>
</tr>
<tr>
<td style="text-align:left;">
Erythrina buchii Urb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Diphysa americana (Mill.) M.Sousa
</td>
</tr>
<tr>
<td style="text-align:left;">
Enterolobium Mart.
</td>
</tr>
<tr>
<td style="text-align:left;">
Enterolobium cyclocarpum (Jacq.) Griseb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Poitea dubia (Poir.) Lavin
</td>
</tr>
<tr>
<td style="text-align:left;">
Mucuna mutisiana (Kunth) DC.
</td>
</tr>
<tr>
<td style="text-align:left;">
Lathyrus oleraceus Lam.
</td>
</tr>
<tr>
<td style="text-align:left;">
Dalbergia brownei (Jacq.) Schinz
</td>
</tr>
<tr>
<td style="text-align:left;">
Poitea galegoides var. stenophylla Ekman ex Lavin
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista pedicellaris var. pedicellaris
</td>
</tr>
<tr>
<td style="text-align:left;">
Sesbania grandiflora (L.) Pers.
</td>
</tr>
<tr>
<td style="text-align:left;">
Bauhinia tomentosa L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Entada rheedei Spreng.
</td>
</tr>
<tr>
<td style="text-align:left;">
Lysiloma ambigua Urb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Lespedeza Michx.
</td>
</tr>
<tr>
<td style="text-align:left;">
Guilandina ciliata Bergius ex Wikstr.
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachellia farnesiana var. farnesiana
</td>
</tr>
<tr>
<td style="text-align:left;">
Clitoria fairchildiana R.A.Howard
</td>
</tr>
<tr>
<td style="text-align:left;">
Andira inermis subsp. inermis
</td>
</tr>
<tr>
<td style="text-align:left;">
Vigna luteola (Jacq.) Benth.
</td>
</tr>
<tr>
<td style="text-align:left;">
Albizia carbonaria Britton
</td>
</tr>
<tr>
<td style="text-align:left;">
Eriosema crinitum (Kunth) G.Don
</td>
</tr>
<tr>
<td style="text-align:left;">
Mimosa diplotricha var. diplotricha
</td>
</tr>
<tr>
<td style="text-align:left;">
Calopogonium galactoides (Kunth) Hemsl.
</td>
</tr>
<tr>
<td style="text-align:left;">
Tephrosia sessiliflora (Poir.) Hassl.
</td>
</tr>
<tr>
<td style="text-align:left;">
Teramnus uncinatus (L.) Sw.
</td>
</tr>
<tr>
<td style="text-align:left;">
Centrosema virginianum Griseb., 1857
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmodium cubense Griseb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista glandulosa (L.) Greene
</td>
</tr>
<tr>
<td style="text-align:left;">
Indigofera fruticosa Rose
</td>
</tr>
<tr>
<td style="text-align:left;">
Bauhinia galpinii N.E.Br.
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna domingensis (Spreng.) H.S.Irwin & Barneby
</td>
</tr>
<tr>
<td style="text-align:left;">
Crotalaria spectabilis Roth
</td>
</tr>
<tr>
<td style="text-align:left;">
Crotalaria pumila Ortega
</td>
</tr>
<tr>
<td style="text-align:left;">
Galactia synandra Urb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna alexandrina Mill.
</td>
</tr>
<tr>
<td style="text-align:left;">
Aeschynomene sensitiva Sw.
</td>
</tr>
<tr>
<td style="text-align:left;">
Indigofera colutea var. colutea
</td>
</tr>
<tr>
<td style="text-align:left;">
Aeschynomene L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista pedicellaris (DC.) Britton
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachellia choriophylla (Benth.) Seigler & Ebinger
</td>
</tr>
<tr>
<td style="text-align:left;">
Alysicarpus Desv.
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmodium procumbens (Mill.) C.L.Hitchc.
</td>
</tr>
<tr>
<td style="text-align:left;">
Vigna vexillata (L.) A.Rich.
</td>
</tr>
<tr>
<td style="text-align:left;">
Clitoria falcata var. falcata
</td>
</tr>
<tr>
<td style="text-align:left;">
Vigna umbellata (Thunb.) Ohwi & H.Ohashi
</td>
</tr>
<tr>
<td style="text-align:left;">
Phaseolus lunatus Billb. ex Beurl.
</td>
</tr>
<tr>
<td style="text-align:left;">
Ancistrotropis peduncularis (Fawc. & Rendle) A.Delgado
</td>
</tr>
<tr>
<td style="text-align:left;">
Galactia parvifolia A.Rich.
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmodium scorpiurus (Sw.) Desv. ex DC.
</td>
</tr>
<tr>
<td style="text-align:left;">
Canavalia plagiosperma Piper
</td>
</tr>
<tr>
<td style="text-align:left;">
Poiretia Vent.
</td>
</tr>
<tr>
<td style="text-align:left;">
Sigmoidotropis ekmaniana (Urb.) A.Delgado
</td>
</tr>
<tr>
<td style="text-align:left;">
Zapoteca caracasana subsp. caracasana
</td>
</tr>
<tr>
<td style="text-align:left;">
Nissolia vincentina (Ker Gawl.) T.M.Moura & Fort.-Perez
</td>
</tr>
<tr>
<td style="text-align:left;">
Medicago polymorpha L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Erythrostemon glandulosus (Bertero ex DC.) Gagnon & G.P.Lewis
</td>
</tr>
<tr>
<td style="text-align:left;">
Galactia lignosa (Turpin ex Pers.) Urb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Vigna unguiculata (L.) Walp.
</td>
</tr>
<tr>
<td style="text-align:left;">
Galactia spiciformis Torr. & A.Gray
</td>
</tr>
<tr>
<td style="text-align:left;">
Teramnus labialis (L.f.) Spreng.
</td>
</tr>
<tr>
<td style="text-align:left;">
Galactia dictyophylla Urb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Crotalaria purdiana H.Senn
</td>
</tr>
<tr>
<td style="text-align:left;">
Bauhinia Plum. ex L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Indigofera lespedezioides Kunth
</td>
</tr>
<tr>
<td style="text-align:left;">
Galactia fuertesii Urb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna pendula var. indecora (Kunth) Luckow
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna tora (L.) Roxb.
</td>
</tr>
<tr>
<td style="text-align:left;">
Indigofera microcarpa Desv.
</td>
</tr>
<tr>
<td style="text-align:left;">
Cassia emarginata Berry, 1916
</td>
</tr>
<tr>
<td style="text-align:left;">
Dalbergia L.f.
</td>
</tr>
<tr>
<td style="text-align:left;">
Cassia biflora Bojer
</td>
</tr>
<tr>
<td style="text-align:left;">
Galactia filiformis (Jacq.) Benth.
</td>
</tr>
<tr>
<td style="text-align:left;">
Centrosema (DC.) Benth.
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachellia acuifera (Benth.) Seigler & Ebinger
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna mexicana var. berteroana (Balb. ex DC.) H.S.Irwin & Barneby
</td>
</tr>
<tr>
<td style="text-align:left;">
Medicago lupina L.
</td>
</tr>
<tr>
<td style="text-align:left;">
Cenostigma pellucidum (Vogel) Gagnon & G.P.Lewis
</td>
</tr>
<tr>
<td style="text-align:left;">
Ctenodon pleuronervius (DC.) D.B.O.S.Cardoso & H.C.Lima
</td>
</tr>
</tbody>
</table>

``` r
unique(word(names(mc_orig), 1, 1)) #Géneros representados
```

    ##   [1] "Poitea"         "Canavalia"      "Senna"          "Flemingia"     
    ##   [5] "Alysicarpus"    "Clitoria"       "Acacia"         "Cajanus"       
    ##   [9] "Mimosa"         "Albizia"        "Centrosema"     "Mucuna"        
    ##  [13] "Bauhinia"       "Caesalpinia"    "Leucaena"       "Haematoxylum"  
    ##  [17] "Tamarindus"     "Entada"         "Delonix"        "Prosopis"      
    ##  [21] "Chamaecrista"   "Trifolium"      "Crotalaria"     "Hymenaea"      
    ##  [25] "Samanea"        "Abrus"          "Desmodium"      "Dalbergia"     
    ##  [29] "Macroptilium"   "Pithecellobium" "Phaseolus"      "Inga"          
    ##  [33] "Arcoa"          "Vachellia"      "Pictetia"       "Biancaea"      
    ##  [37] "Calliandra"     "Rhodopis"       "Guilandina"     "Rhynchosia"    
    ##  [41] "Libidibia"      "Strongylodon"   "Coronilla"      "Grona"         
    ##  [45] "Erythrina"      "Gliricidia"     "Adenanthera"    "Parkinsonia"   
    ##  [49] "Zornia"         "Cassia"         "Stylosanthes"   "Barnebydendron"
    ##  [53] "Tara"           "Anadenanthera"  "Glycine"        "Fabaceae"      
    ##  [57] "Parasenegalia"  "Jupunba"        "Lablab"         "Arachis"       
    ##  [61] "Galactia"       "Piscidia"       "Desmanthus"     "Indigofera"    
    ##  [65] "Coursetia"      "Denisophytum"   "Cojoba"         "Tephrosia"     
    ##  [69] "Acaciella"      "Lysiloma"       "Sigmoidotropis" "Zapoteca"      
    ##  [73] "Sophora"        "Dalea"          "Medicago"       "Lupinus"       
    ##  [77] "Genista"        "Lonchocarpus"   "Brya"           "Mora"          
    ##  [81] "Cynometra"      "Lathyrus"       "Ateleia"        "Chloroleucon"  
    ##  [85] "Neustanthus"    "Aeschynomene"   "Sesbania"       "Ormosia"       
    ##  [89] "Andira"         "Melilotus"      "Peltophorum"    "Coulteria"     
    ##  [93] "Neptunia"       "Poiretia"       "Barbieria"      "Senegalia"     
    ##  [97] "Pterocarpus"    "Lespedeza"      "Apios"          "Machaerium"    
    ## [101] "Calopogonium"   "Pachyrhizus"    "Diphysa"        "Enterolobium"  
    ## [105] "Vigna"          "Eriosema"       "Teramnus"       "Ancistrotropis"
    ## [109] "Nissolia"       "Erythrostemon"  "Cenostigma"     "Ctenodon"

``` r
table(word(names(mc_orig), 1, 1)) #Número de especies por género
```

    ## 
    ##          Abrus         Acacia      Acaciella    Adenanthera   Aeschynomene 
    ##              1              5              1              1              6 
    ##        Albizia    Alysicarpus  Anadenanthera Ancistrotropis         Andira 
    ##              7              2              1              1              2 
    ##          Apios        Arachis          Arcoa        Ateleia      Barbieria 
    ##              1              3              1              3              1 
    ## Barnebydendron       Bauhinia       Biancaea           Brya    Caesalpinia 
    ##              1              7              1              1              5 
    ##        Cajanus     Calliandra   Calopogonium      Canavalia         Cassia 
    ##              1              8              2              4              4 
    ##     Cenostigma     Centrosema   Chamaecrista   Chloroleucon       Clitoria 
    ##              1              5             15              1              3 
    ##         Cojoba      Coronilla      Coulteria      Coursetia     Crotalaria 
    ##              6              1              1              1             12 
    ##       Ctenodon      Cynometra      Dalbergia          Dalea        Delonix 
    ##              1              1              5              2              1 
    ##   Denisophytum     Desmanthus      Desmodium        Diphysa         Entada 
    ##              2              3             11              1              3 
    ##   Enterolobium       Eriosema      Erythrina  Erythrostemon       Fabaceae 
    ##              2              1              6              1              1 
    ##      Flemingia       Galactia        Genista     Gliricidia        Glycine 
    ##              1             13              1              1              1 
    ##          Grona     Guilandina   Haematoxylum       Hymenaea     Indigofera 
    ##              4              3              1              1              9 
    ##           Inga        Jupunba         Lablab       Lathyrus      Lespedeza 
    ##              6              4              1              2              2 
    ##       Leucaena      Libidibia   Lonchocarpus        Lupinus       Lysiloma 
    ##              4              2              5              2              2 
    ##     Machaerium   Macroptilium       Medicago      Melilotus         Mimosa 
    ##              1              2              3              1              9 
    ##           Mora         Mucuna       Neptunia    Neustanthus       Nissolia 
    ##              2              5              1              1              1 
    ##        Ormosia    Pachyrhizus  Parasenegalia    Parkinsonia    Peltophorum 
    ##              1              1              2              1              1 
    ##      Phaseolus       Pictetia       Piscidia Pithecellobium       Poiretia 
    ##              3              2              1              5              2 
    ##         Poitea       Prosopis    Pterocarpus       Rhodopis     Rhynchosia 
    ##              9              1              1              3              4 
    ##        Samanea      Senegalia          Senna       Sesbania Sigmoidotropis 
    ##              1              1             24              3              3 
    ##        Sophora   Strongylodon   Stylosanthes     Tamarindus           Tara 
    ##              3              1              2              1              2 
    ##      Tephrosia       Teramnus      Trifolium      Vachellia          Vigna 
    ##              7              2              3              9              4 
    ##       Zapoteca         Zornia 
    ##              4              3

``` r
data.frame(`Número de hexágonos` = sort(colSums(mc_orig), decreasing = T), check.names = F) %>% 
  kable(booktabs=T) %>%
  kable_styling(latex_options = c("HOLD_position", "scale_down")) %>%
  gsub(' NA ', '', .) # Número de hexágonos en los que está presente cada especie
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Número de hexágonos
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Cajanus cajan (L.) Huth
</td>
<td style="text-align:right;">
36
</td>
</tr>
<tr>
<td style="text-align:left;">
Centrosema virginianum (L.) Benth.
</td>
<td style="text-align:right;">
32
</td>
</tr>
<tr>
<td style="text-align:left;">
Pictetia sulcata (P.Beauv.) Beyra & Lavin
</td>
<td style="text-align:right;">
28
</td>
</tr>
<tr>
<td style="text-align:left;">
Delonix regia (Bojer ex Hook.) Raf.
</td>
<td style="text-align:right;">
26
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachellia macracantha (Humb. & Bonpl. ex Willd.) Seigler & Ebinger
</td>
<td style="text-align:right;">
26
</td>
</tr>
<tr>
<td style="text-align:left;">
Leucaena leucocephala (Lam.) de Wit
</td>
<td style="text-align:right;">
25
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna occidentalis (L.) Link
</td>
<td style="text-align:right;">
22
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna atomaria (L.) H.S.Irwin & Barneby
</td>
<td style="text-align:right;">
22
</td>
</tr>
<tr>
<td style="text-align:left;">
Crotalaria retusa L.
</td>
<td style="text-align:right;">
21
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna angustisiliqua var. angustisiliqua
</td>
<td style="text-align:right;">
20
</td>
</tr>
<tr>
<td style="text-align:left;">
Mimosa pudica L.
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
Macroptilium lathyroides (L.) Urb.
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna alata (L.) Roxb.
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
Stylosanthes hamata (L.) Taub.
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
Poitea galegoides Vent.
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
Crotalaria pallida Aiton
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
Brya buxifolia (Murray) Urb.
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
Clitoria ternatea L.
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
Rhynchosia reticulata (Sw.) DC.
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
Flemingia strobilifera (L.) W.T.Aiton
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
Crotalaria incana L.
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
Poitea paucifolia (DC.) Lavin
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
Rhodopis planisiliqua (L.) Urb.
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
Prosopis juliflora (Sw.) DC.
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmodium incanum (Sw.) DC.
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
Inga vera Willd.
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
Libidibia coriaria (Jacq.) Schltdl.
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
Inga vera subsp. vera
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
Lonchocarpus sericeus (Poir.) Kunth ex DC.
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
Canavalia rosea (Sw.) DC.
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
Hymenaea courbaril L.
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
Calliandra pedicellata Benth.
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
Mimosa domingensis (Bertero ex DC.) Benth.
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmanthus virgatus (L.) Willd.
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista nictitans var. glabrata (Vogel) H.S.Irwin & Barneby
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista glandulosa var. pinetorum (Britton)
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
Calliandra haematomma var. haematomma
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
Poitea galegoides var. galegoides
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
Indigofera suffruticosa Mill.
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
Tamarindus indica L.
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
Entada gigas (L.) Fawc. & Rendle
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
Fabaceae
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
Bauhinia divaricata L.
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
Crotalaria pallida var. obovata (G.Don) Polhill
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
Parasenegalia skleroxyla (Tussac) Seigler & Ebinger
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
Ormosia krugii Urb.
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna uniflora (Mill.) H.S.Irwin & Barneby
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
Rhynchosia minima (L.) DC.
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
Acacia mangium Willd.
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
Abrus precatorius L.
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
Pithecellobium circinale (L.) Benth.
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
Centrosema pubescens Benth.
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna spectabilis (DC.) H.S.Irwin & Barneby
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
Gliricidia sepium (Jacq.) Kunth
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
Cassia grandis L.f.
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
Mucuna urens (L.) Medik.
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
Mora ekmanii (Urb.) Britton & Rose
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
Albizia berteroana (Balb. ex DC.) Fawc. & Rendle
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
Inga laurina (Sw.) Willd.
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
Andira inermis (W.Wright) DC.
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmodium tortuosum (Sw.) DC.
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
Calliandra haematomma (Bertero ex DC.) Benth.
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
Zapoteca portoricensis subsp. portoricensis
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
Aeschynomene americana L.
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
Galactia striata (Jacq.) Urb.
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
Centrosema plumieri (Turpin ex Pers.) Benth.
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
Caesalpinia pulcherrima (L.) Sw.
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
Lonchocarpus heptaphyllus (Poir.) DC.
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
Mora abbottii Rose & Leonard
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
Ateleia gummifera (Bertero ex DC.) D.Dietr.
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
Senegalia angustifolia (Lam.) Britton & Rose
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
Pithecellobium dulce (Roxb.) Benth.
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista lineata var. brachyloba (Griseb.) H.S.Irwin & Barneby
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
Guilandina bonduc L.
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmodium axillare (Sw.) DC.
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
Medicago lupulina L.
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
Jupunba oppositifolia (Urb.) Britton & Rose
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
Sesbania bispinosa (Jacq.) W.Wight
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
Mimosa pudica var. unijuga (Duchass. & Walp.) Griseb.
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
Dalbergia ovalis (L.) P.L.R.Moraes & L.P.Queiroz
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmodium Desv.
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista nictitans var. diffusa (DC.) H.S.Irwin & Barneby
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
Alysicarpus vaginalis (L.) DC.
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Albizia lebbeck (L.) Benth.
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Haematoxylum campechianum L.
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista nictitans (L.) Moench
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Crotalaria verrucosa L.
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Samanea saman (Jacq.) Merr.
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Adenanthera pavonina L.
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Cassia fistula L.
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmodium glabrum (Mill.) DC.
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Indigofera colutea (Burm.f.) Merr.
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Leucaena trichodes (Jacq.) Benth.
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Cojoba arborea (L.) Britton & Rose
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Caesalpinia domingensis Urb.
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Lonchocarpus neurophyllus Urb.
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Cojoba arborea var. arborea
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Peltophorum dubium var. berteroanum (Urb.) Barneby
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Tephrosia purpurea (L.) Pers.
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Barbieria pinnata (Pers.) Baill.
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Indigofera subulata var. scabra (Roth) Meikle
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Aeschynomene pratensis Small
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Grona adscendens (Sw.) H.Ohashi & K.Ohashi
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna obtusifolia (L.) H.S.Irwin & Barneby
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
Arcoa gonavensis Urb.
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
Rhynchosia phaseoloides (Sw.) DC.
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
Parkinsonia aculeata L.
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
Mimosa diplacantha Benth.
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
Tephrosia cinerea (L.) Pers.
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
Lysiloma sabicu Benth.
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna sophera (L.) Roxb.
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
Lonchocarpus longipes Urb. & Ekman
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
Cojoba filipes (Vent.) Barneby & J.W.Grimes
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
Neptunia plena (L.) Benth.
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
Poiretia punctata (Willd.) Desv.
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
Aeschynomene americana var. americana
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachellia farnesiana (L.) Wight & Arn.
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista diphylla (L.) Greene
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
Pithecellobium unguis-cati (L.) Benth.
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
Enterolobium cyclocarpum (Jacq.) Griseb.
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
Bauhinia monandra Kurz
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Trifolium repens L.
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Dalbergia ecastaphyllum (L.) Taub.
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Poitea campanilla DC.
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Inga edulis Mart.
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Grona triflora (L.) H.Ohashi & K.Ohashi
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Piscidia ekmanii Rudd
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Galactia buchii Urb.
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmanthus leptophyllus Kunth
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Denisophytum buchii (Urb.) Gagnon & G.P.Lewis
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Acacia scleroxyla Tussac
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna ligustrina (L.) H.S.Irwin & Barneby
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Neustanthus phaseoloides (Roxb.) Benth.
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Pictetia obcordata DC.
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Dalea carthagenensis var. carthagenensis
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Crotalaria stipularia Desv.
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Albizia berteriana (DC.) Fawc. & Rendle
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Grona barbata (L.) H.Ohashi & K.Ohashi
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna septemtrionalis (Viv.) H.S.Irwin & Barneby
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Indigofera tinctoria L.
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna mexicana var. mexicana
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Indigofera subulata Vahl ex Poir.
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Vigna luteola (Jacq.) Benth.
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Mucuna pruriens (L.) DC.
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmodium ciliare (Muhl. ex Willd.) DC.
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Biancaea decapetala (Roth) O.Deg.
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Anadenanthera peregrina (L.) Speg.
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Erythrina poeppigiana (Walp.) O.F.Cook
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Caesalpinia barahonensis Urb.
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna angustisiliqua (Lam.) H.S.Irwin & Barneby
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachellia tortuosa (L.) Seigler & Ebinger
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Galactia dubia DC.
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Dalbergia berteroi (DC.) Urb.
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Crotalaria L.
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna mexicana var. berteriana (DC.) H.S.Irwin & Barneby
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Zapoteca nervosa (Urb.) H.M.Hern.
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Machaerium lunatum (L.f.) Ducke
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Ateleia microcarpa (Pers.) D.Dietr.
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Acacia confusa Merr.
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista pedicellaris var. adenosperma (Urb.) H.S.Irwin & Barneby
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Tephrosia purpurea subsp. purpurea
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Poitea dubia (Poir.) Lavin
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Galactia synandra Urb.
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmodium procumbens (Mill.) C.L.Hitchc.
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna siamea (Lam.) H.S.Irwin & Barneby
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Rhodopis planisiliqua var. lowdenii (Judd) Alain
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Guilandina sphaerosperma (Urb. & Ekman) Britton
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Calliandra surinamensis Benth.
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Calliandra houstoniana var. calothyrsus (Meisn.) Barneby
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Erythrina variegata L.
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Macroptilium atropurpureum (DC.) Urb.
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Bauhinia variegata L.
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Parasenegalia vogeliana (Steud.) Seigler & Ebinger
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Rhynchosia Lour.
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Jupunba glauca (Urb.) Britton & Rose
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Acacia melanoxylon R.Br.
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Coursetia caribaea (Jacq.) Lavin
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachellia barahonensis (Urb. & Ekman) Seigler & Ebinger
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Canavalia nitida (Cav.) Piper
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Libidibia monosperma (Tul.) Gagnon & G.P.Lewis
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Dalea carthagenensis (Jacq.) J.F.Macbr.
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Jupunba abbottii (Rose & Leonard) Britton & Rose
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Cynometra portoricensis Krug & Urb.
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Poitea multiflora (Sw.) Urb.
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Jupunba obovalis (A.Rich.) Britton & Rose
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Melilotus albus Medik.
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Galactia P.Browne
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Aeschynomene villosa Poir.
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Sophora tomentosa L.
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Sophora albopetiolulata Leonard
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista portoricensis var. portoricensis
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Pachyrhizus erosus (L.) Urb.
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Tephrosia senna Kunth
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Galactia longifolia (Jacq.) Benth.
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Sesbania sericea (Willd.) Link
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Calopogonium galactoides (Kunth) Hemsl.
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna domingensis (Spreng.) H.S.Irwin & Barneby
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Aeschynomene sensitiva Sw.
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Galactia fuertesii Urb.
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Mimosa pigra L.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Coronilla varia L.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Zornia reticulata Sm.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Tara vesicaria (L.) Molinari, Sánchez Och. & Mayta
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna Mill.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Galactia schomburgkii Urb.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Caesalpinia brasiliensis L.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmodium affine Schltdl.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachellia oviedoensis (R.G.García & M.M.Mejía) Seigler & Ebinger
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachellia cucuyo (Barneby & Zanoni) Seigler & Ebinger
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Crotalaria lotifolia L.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna mexicana (Jacq.) H.S.Irwin & Barneby
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Denisophytum pauciflorum (Griseb.) Gagnon & G.P.Lewis
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Inga Mill.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Tephrosia sinapou (Buc’hoz) A.Chev.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Zapoteca portoricensis (Jacq.) H.M.Hern.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Calliandra haematomma var. rivularis (Urb. & Ekman) Barneby
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Entada polystachya (L.) DC.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Rhodopis rudolphioides (Griseb.) L.P.Queiroz
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmodium intortum (Mill.) Urb.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Calliandra Benth.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Cojoba zanonii (Barneby) Barneby & J.W.Grimes
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Pterocarpus officinalis Jacq.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Lonchocarpus pycnophyllus Urb.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista Moench
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Acacia auriculiformis A.Cunn. ex Benth.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Mimosa ceratonia var. ceratonia
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Erythrina berteroana Urb.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Zornia diphylla (L.) Pers.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Calopogonium mucunoides Desv.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna bicapsularis (L.) Roxb.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Zornia microphylla Desv.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Mucuna mutisiana (Kunth) DC.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Dalbergia brownei (Jacq.) Schinz
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Poitea galegoides var. stenophylla Ekman ex Lavin
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista pedicellaris var. pedicellaris
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Sesbania grandiflora (L.) Pers.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Tephrosia sessiliflora (Poir.) Hassl.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Teramnus uncinatus (L.) Sw.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Centrosema virginianum Griseb., 1857
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmodium cubense Griseb.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Crotalaria spectabilis Roth
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Crotalaria pumila Ortega
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna alexandrina Mill.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista pedicellaris (DC.) Britton
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Ancistrotropis peduncularis (Fawc. & Rendle) A.Delgado
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmodium scorpiurus (Sw.) Desv. ex DC.
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Nissolia vincentina (Ker Gawl.) T.M.Moura & Fort.-Perez
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Cenostigma pellucidum (Vogel) Gagnon & G.P.Lewis
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Bauhinia purpurea L.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Albizia julibrissin Durazz.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Crotalaria juncea L.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Phaseolus vulgaris L.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Calliandra haematocephala Hassk.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Strongylodon macrobotrys A.Gray
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Leucaena macrophylla Benth.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Barnebydendron riedelii (Tul.) J.H.Kirkbr.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Tara Molina
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Leucaena leucocephala subsp. leucocephala
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Glycine max (L.) Merr.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Caesalpinia Plum. ex L.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Poitea Vent.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Canavalia DC.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista fasciculata (Michx.) Greene
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Lablab purpureus (L.) Sweet
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Arachis hypogaea L.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Arachis glabrata Benth.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Arachis pintoi Krapov. & W.C.Greg.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Albizia niopoides (Spruce ex Benth.) Burkart
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista lineata (Sw.) Greene
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Phaseolus lunatus L.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Desmanthus Willd.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Acaciella glauca (L.) L.Rico
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Sigmoidotropis elegans (Piper) A.Delgado
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Sophora tomentosa var. occidentalis (L.) Isely
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Trifolium pratense L.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Lupinus mexicanus Cerv. ex Lag.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Genista monspessulana (L.) L.A.S.Johnson
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Poitea glyciphylla (Poir.) Urb.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Sigmoidotropis antillana (Urb.) A.Delgado
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Inga ruiziana G.Don
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Lathyrus articulatus L.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Lupinus plattensis S.Watson
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Trifolium dubium Sibth.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Cojoba samanensis R.G.García & Peguero
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Cojoba bahorucensis J.W.Grimes & R.García
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Chloroleucon guantanamense (Britton) Britton & Rose
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Mimosa candollei R.Grether
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Pithecellobium Mart.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Pithecellobium domingense Alain
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Mucuna Adans.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Coulteria cubensis (Greenm.) Sotuyo & G.P.Lewis
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Erythrina corallodendron L.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna gundlachii (Urb.) H.S.Irwin & Barneby
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Lespedeza juncea (L.f.) Pers.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Erythrina leptopoda Urb. & Ekman
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Mimosa L.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Ateleia (DC.) D.Dietr.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Albizia procera (Roxb.) Benth.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Apios Fabr.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Tephrosia Pers.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna polyphylla var. montis-christi H.S.Irwin & Barneby
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Grona barbata var. barbata
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Mucuna pruriens var. utilis (Wall. ex Wight) Baker ex Burck
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista pilosa var. pilosa
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Stylosanthes guianensis (Aubl.) Sw.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Erythrina buchii Urb.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Diphysa americana (Mill.) M.Sousa
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Enterolobium Mart.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Lathyrus oleraceus Lam.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Bauhinia tomentosa L.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Entada rheedei Spreng.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Lysiloma ambigua Urb.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Lespedeza Michx.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Guilandina ciliata Bergius ex Wikstr.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachellia farnesiana var. farnesiana
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Clitoria fairchildiana R.A.Howard
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Andira inermis subsp. inermis
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Albizia carbonaria Britton
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Eriosema crinitum (Kunth) G.Don
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Mimosa diplotricha var. diplotricha
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Chamaecrista glandulosa (L.) Greene
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Indigofera fruticosa Rose
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Bauhinia galpinii N.E.Br.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Indigofera colutea var. colutea
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Aeschynomene L.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachellia choriophylla (Benth.) Seigler & Ebinger
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Alysicarpus Desv.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Vigna vexillata (L.) A.Rich.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Clitoria falcata var. falcata
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Vigna umbellata (Thunb.) Ohwi & H.Ohashi
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Phaseolus lunatus Billb. ex Beurl.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Galactia parvifolia A.Rich.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Canavalia plagiosperma Piper
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Poiretia Vent.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Sigmoidotropis ekmaniana (Urb.) A.Delgado
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Zapoteca caracasana subsp. caracasana
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Medicago polymorpha L.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Erythrostemon glandulosus (Bertero ex DC.) Gagnon & G.P.Lewis
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Galactia lignosa (Turpin ex Pers.) Urb.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Vigna unguiculata (L.) Walp.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Galactia spiciformis Torr. & A.Gray
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Teramnus labialis (L.f.) Spreng.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Galactia dictyophylla Urb.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Crotalaria purdiana H.Senn
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Bauhinia Plum. ex L.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Indigofera lespedezioides Kunth
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna pendula var. indecora (Kunth) Luckow
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna tora (L.) Roxb.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Indigofera microcarpa Desv.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Cassia emarginata Berry, 1916
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Dalbergia L.f.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Cassia biflora Bojer
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Galactia filiformis (Jacq.) Benth.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Centrosema (DC.) Benth.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachellia acuifera (Benth.) Seigler & Ebinger
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Senna mexicana var. berteroana (Balb. ex DC.) H.S.Irwin & Barneby
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Medicago lupina L.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Ctenodon pleuronervius (DC.) D.B.O.S.Cardoso & H.C.Lima
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>

``` r
en_cuantos_hex <- 1
# Explicación: "en_cuantos_hex <- X", donde X es el número de hexágonos mínimo donde cada especie
# debe estar presente. IMPORTANTE: elige TU PROPIO umbral.
{if(length(en_cuantos_hex)==1) selector <- en_cuantos_hex:max(colSums(mc_orig)) else
  if(length(en_cuantos_hex)==2)
    selector <- min(en_cuantos_hex):max(en_cuantos_hex) else
      stop('Debes indicar uno o dos valores numéricos')}
selector
```

    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
    ## [26] 26 27 28 29 30 31 32 33 34 35 36

``` r
mc_orig_seleccionadas <- mc_orig[, colSums(mc_orig) %in% selector]

# Mínimo número de especies por hexágono
data.frame(`Número de especies por hexágono` = sort(rowSums(mc_orig), decreasing = T), check.names = F) %>% 
  kable(booktabs=T) %>%
  kable_styling(latex_options = c("HOLD_position", "scale_down")) %>%
  gsub(' NA ', '', .) # Número de hexágonos en los que está presente cada especie
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Número de especies por hexágono
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
854cd42ffffffff
</td>
<td style="text-align:right;">
49
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd427fffffff
</td>
<td style="text-align:right;">
49
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd423fffffff
</td>
<td style="text-align:right;">
37
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd44bfffffff
</td>
<td style="text-align:right;">
34
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd0d3fffffff
</td>
<td style="text-align:right;">
33
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd29bfffffff
</td>
<td style="text-align:right;">
29
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd667fffffff
</td>
<td style="text-align:right;">
29
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd5b3fffffff
</td>
<td style="text-align:right;">
28
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd6affffffff
</td>
<td style="text-align:right;">
27
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd4cbfffffff
</td>
<td style="text-align:right;">
26
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd453fffffff
</td>
<td style="text-align:right;">
25
</td>
</tr>
<tr>
<td style="text-align:left;">
854c8927fffffff
</td>
<td style="text-align:right;">
25
</td>
</tr>
<tr>
<td style="text-align:left;">
854c8997fffffff
</td>
<td style="text-align:right;">
24
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd46bfffffff
</td>
<td style="text-align:right;">
24
</td>
</tr>
<tr>
<td style="text-align:left;">
854c898ffffffff
</td>
<td style="text-align:right;">
24
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd623fffffff
</td>
<td style="text-align:right;">
24
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd093fffffff
</td>
<td style="text-align:right;">
22
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd2dbfffffff
</td>
<td style="text-align:right;">
21
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd5cffffffff
</td>
<td style="text-align:right;">
21
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd5b7fffffff
</td>
<td style="text-align:right;">
21
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd5bbfffffff
</td>
<td style="text-align:right;">
21
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd253fffffff
</td>
<td style="text-align:right;">
20
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd0dbfffffff
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd0c7fffffff
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
854c890bfffffff
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd677fffffff
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
856725a7fffffff
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
854c894bfffffff
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd47bfffffff
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd51bfffffff
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd2cbfffffff
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf347fffffff
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
854c89abfffffff
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
85672527fffffff
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
854c89bbfffffff
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd44ffffffff
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd403fffffff
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd43bfffffff
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd40bfffffff
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd08bfffffff
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
854cc673fffffff
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
85672537fffffff
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd0d7fffffff
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd467fffffff
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
854cc613fffffff
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd513fffffff
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd09bfffffff
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
854cc67bfffffff
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd63bfffffff
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd0c3fffffff
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd6a7fffffff
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd5cbfffffff
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd58ffffffff
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd0cffffffff
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf373fffffff
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd687fffffff
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd6b7fffffff
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd457fffffff
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
856725b7fffffff
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
854c882ffffffff
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd653fffffff
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
854c89a3fffffff
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd66ffffffff
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd28bfffffff
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
854c8b97fffffff
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf20ffffffff
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf353fffffff
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd4dbfffffff
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd42bfffffff
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
854c893bfffffff
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf313fffffff
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd58bfffffff
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf37bfffffff
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd5c3fffffff
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd6a3fffffff
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd477fffffff
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
854c89c3fffffff
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd65bfffffff
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
854cc64ffffffff
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd257fffffff
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
856725a3fffffff
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf31bfffffff
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd553fffffff
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf323fffffff
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd633fffffff
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd697fffffff
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf343fffffff
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
854c89cbfffffff
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd46ffffffff
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
854cc643fffffff
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf303fffffff
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
854c8913fffffff
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf243fffffff
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
854c8987fffffff
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
85672583fffffff
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf36bfffffff
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
85672597fffffff
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd437fffffff
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd40ffffffff
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
854c8933fffffff
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf24bfffffff
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf24ffffffff
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
854cc66bfffffff
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf357fffffff
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd407fffffff
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd473fffffff
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd4cffffffff
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
854cc653fffffff
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf263fffffff
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
854c890ffffffff
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf333fffffff
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
854cc657fffffff
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd21bfffffff
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd693fffffff
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd083fffffff
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
854c892ffffffff
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
854c8bb3fffffff
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
85672587fffffff
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf33bfffffff
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf35bfffffff
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
854c8937fffffff
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
854c897bfffffff
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
854c89b7fffffff
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd49bfffffff
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd463fffffff
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd4a3fffffff
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
854c89a7fffffff
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf247fffffff
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf27bfffffff
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
854cc603fffffff
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd45bfffffff
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
856725b3fffffff
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
854c8977fffffff
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd443fffffff
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd5d3fffffff
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
854cc6c7fffffff
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf267fffffff
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd2cffffffff
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf273fffffff
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd4a7fffffff
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
854c8973fffffff
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd64bfffffff
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd4affffffff
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd607fffffff
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd6b3fffffff
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
854cc6dbfffffff
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
854c8b87fffffff
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf34ffffffff
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
854c8bb7fffffff
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd41bfffffff
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd2c7fffffff
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
854c89affffffff
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd497fffffff
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd6d7fffffff
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd4d3fffffff
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
856725affffffff
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd2d7fffffff
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd417fffffff
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd447fffffff
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd413fffffff
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd243fffffff
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd6bbfffffff
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd663fffffff
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
854c8823fffffff
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
854c8b27fffffff
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
854cc65bfffffff
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf30ffffffff
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf22ffffffff
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
854cc6cffffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf26ffffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
854c8903fffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd6dbfffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd6cbfffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf337fffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
854c8b37fffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd293fffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
854c891bfffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
854c8983fffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd4c3fffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
854c89b3fffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd643fffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd2c3fffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd657fffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf32ffffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd603fffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd5dbfffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd4b7fffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
854c896ffffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd647fffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
8567258ffffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd66bfffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
856725bbfffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf22bfffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf367fffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf32bfffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd48bfffffff
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd6c7fffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cc60ffffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd583fffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854c8917fffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854c8b23fffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd4b3fffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd6c3fffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf207fffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd487fffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd6abfffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd613fffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd4abfffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf34bfffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf26bfffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cc647fffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd59bfffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854c894ffffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd2d3fffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854c89dbfffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854c8ba3fffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854c898bfffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854c8baffffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd6d3fffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf233fffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf23bfffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf3affffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cf363fffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cc60bfffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cc64bfffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd4c7fffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd4bbfffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cc6cbfffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd483fffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854c8b33fffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854c899bfffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854c8947fffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd64ffffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd5c7fffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd587fffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
854cd6cffffffff
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>

``` r
min_especies_por_hex <- 1
# Explicación: "min_especies_por_hex <- Y", donde Y es el número mínimo (inclusive) de especies
# que debe existir en cada hexágono. Por debajo de dicho valor, el hexágono es excluido.
mi_fam <- mc_orig_seleccionadas[rowSums(mc_orig_seleccionadas)>=min_especies_por_hex, ]
nrow(mi_fam)
```

    ## [1] 236

``` r
# mi_fam <- mc_orig_seleccionadas[!rowSums(mc_orig_seleccionadas)==0, ] #Elimina filas sin registros
# rowSums(mi_fam) #Riqueza por hexágonos con especies seleccionadas. Comentado por extenso
all(rowSums(mi_fam)>0) #Debe ser TRUE: todos los hexágonos tienen al menos 1 registro
```

    ## [1] TRUE

``` r
ncol(mi_fam) #Riqueza de especies
```

    ## [1] 358

``` r
# Usar nombres cortos o abreviados para las especies
nombres_largos <- colnames(mi_fam)
(colnames(mi_fam) <- make.cepnames(word(colnames(mi_fam), 1, 2)))
```

    ##   [1] "Poitgale"   "Canarose"   "Sennocci"   "Flemstro"   "Alysvagi"  
    ##   [6] "Clittern"   "Acacmang"   "Cajacaja"   "Mimopudi"   "Albilebb"  
    ##  [11] "Centplum"   "Mucuprur"   "Bauhpurp"   "Caespulc"   "Leucleuc"  
    ##  [16] "Haemcamp"   "Tamaindi"   "Entagiga"   "Bauhmona"   "Deloregi"  
    ##  [21] "Prosjuli"   "Centvirg"   "Mimopigr"   "Chamnict"   "Trifrepe"  
    ##  [26] "Crotinca"   "Crotpall"   "Crotretu"   "Hymecour"   "Crotverr"  
    ##  [31] "Samasama"   "Albijuli"   "Abruprec"   "Desminca"   "Dalbecas"  
    ##  [36] "Sennsiam"   "Macrlath"   "Pithdulc"   "Desmcili"   "Desmtort"  
    ##  [41] "Crotjunc"   "Pithcirc"   "Phasvulg"   "Ingavera"   "Centpube"  
    ##  [46] "Sennspec"   "Sennobtu"   "Arcogona"   "Poitcamp"   "Vachmacr"  
    ##  [51] "Pictsulc"   "Biandeca"   "Callhaem"   "Rhodplan"   "Guilspha"  
    ##  [56] "Callpedi"   "Rhynreti"   "Chamline"   "Sennalat"   "Libicori"  
    ##  [61] "Stromacr"   "Callsuri"   "Corovari"   "Callhous"   "Ingaedul"  
    ##  [66] "Grontrif"   "Erytvari"   "Glirsepi"   "Guilbond"   "Leucmacr"  
    ##  [71] "Macratro"   "Adenpavo"   "Rhynphas"   "Parkacul"   "Sennatom"  
    ##  [76] "Zornreti"   "Cassfist"   "Stylhama"   "Barnried"   "Bauhvari"  
    ##  [81] "Taravesi"   "TaraMoli"   "Leucleuc.1" "Anadpere"   "Glycmax"   
    ##  [86] "NA."        "Paravoge"   "CaesPlum"   "PoitVent"   "CanaDC"    
    ##  [91] "RhynLour"   "Bauhdiva"   "Chamfasc"   "Jupuglau"   "SennMill"  
    ##  [96] "Lablpurp"   "Arachypo"   "Crotpall.1" "Erytpoep"   "Aracglab"  
    ## [101] "Aracpint"   "Acacmela"   "Albiniop"   "Galascho"   "Piscekma"  
    ## [106] "Galabuch"   "Mimodomi"   "Desmlept"   "Desmglab"   "Indicolu"  
    ## [111] "Leuctric"   "Callhaem.1" "Caesbara"   "Courcari"   "Poitpauc"  
    ## [116] "Denibuch"   "Paraskle"   "Caesbras"   "Sennangu"   "Chamline.1"
    ## [121] "Phasluna"   "Cojoarbo"   "Desmaffi"   "Cassgran"   "Mimodipl"  
    ## [126] "Ingavera.1" "Tephcine"   "Acacscle"   "DesmWill"   "Acacglau"  
    ## [131] "Mucuuren"   "Desmaxil"   "Vachbara"   "Vachovie"   "Lysisabi"  
    ## [136] "Vachcucu"   "Vachtort"   "Desmvirg"   "Cananiti"   "Sigmeleg"  
    ## [141] "Galadubi"   "Zapoport"   "Sophtome"   "Crotloti"   "Caesdomi"  
    ## [146] "Libimono"   "Dalecart"   "Trifprat"   "Medilupu"   "Chamnict.1"
    ## [151] "Lupimexi"   "Genimons"   "Sennligu"   "Chamglan"   "Sennmexi"  
    ## [156] "Loncneur"   "Jupuoppo"   "Bryabuxi"   "Jupuabbo"   "Lonchept"  
    ## [161] "Poitglyc"   "Sigmanti"   "Moraekma"   "Sennsoph"   "Rhodplan.1"
    ## [166] "Cynoport"   "Dalbbert"   "Ingaruiz"   "Moraabbo"   "Lonclong"  
    ## [171] "Latharti"   "Lupiplat"   "Trifdubi"   "Cojosama"   "Atelgumm"  
    ## [176] "Sennangu.1" "Cojoarbo.1" "Denipauc"   "Cojobaho"   "Chloguan"  
    ## [181] "Neusphas"   "Aescamer"   "Sesbbisp"   "Cojofili"   "Pictobco"  
    ## [186] "Ormokrug"   "Albibert"   "Poitmult"   "Mimocand"   "Jupuobov"  
    ## [191] "Ingalaur"   "Callhaem.2" "Dalecart.1" "PithMart"   "IngaMill"  
    ## [196] "Tephsina"   "Zapoport.1" "Callhaem.3" "Pithdomi"   "Andiiner"  
    ## [201] "Poitgale.1" "Melialbu"   "Sennunif"   "MucuAdan"   "Peltdubi"  
    ## [206] "Coulcube"   "Loncseri"   "CrotL"      "Sennmexi.1" "Mimopudi.1"
    ## [211] "Dalboval"   "Neptplen"   "Erytcora"   "Entapoly"   "Rhodrudo"  
    ## [216] "DesmDesv"   "Poirpunc"   "Desminto"   "Aescamer.1" "Crotstip"  
    ## [221] "Tephpurp"   "Barbpinn"   "CallBent"   "Cojozano"   "Seneangu"  
    ## [226] "Pteroffi"   "Senngund"   "Chamnict.2" "Loncpycn"   "GalaBrow"  
    ## [231] "ChamMoen"   "Aescvill"   "Acacauri"   "Albibert.1" "Indisubu"  
    ## [236] "Lespjunc"   "Gronbarb"   "Sennsept"   "Vachfarn"   "Mimocera"  
    ## [241] "Aescprat"   "Erytbert"   "Gronadsc"   "Erytlept"   "MimoL"     
    ## [246] "Sophtome.1" "Zaponerv"   "AtelDC"     "Inditinc"   "Chamdiph"  
    ## [251] "Zorndiph"   "Rhynmini"   "Albiproc"   "ApioFabr"   "TephPers"  
    ## [256] "Pithcati"   "Galastri"   "Sennpoly"   "Sennmexi.2" "Sophalbo"  
    ## [261] "Champort"   "Machluna"   "Gronbarb.1" "Atelmicr"   "Calomucu"  
    ## [266] "Sennbica"   "Acacconf"   "Mucuprur.1" "Pacheros"   "Zornmicr"  
    ## [271] "Indisuff"   "Champedi"   "Tephsenn"   "Galalong"   "Sesbseri"  
    ## [276] "Champilo"   "Stylguia"   "Indisubu.1" "Tephpurp.1" "Erytbuch"  
    ## [281] "Diphamer"   "EnteMart"   "Entecycl"   "Poitdubi"   "Mucumuti"  
    ## [286] "Latholer"   "Dalbbrow"   "Poitgale.2" "Champedi.1" "Sesbgran"  
    ## [291] "Bauhtome"   "Entarhee"   "Lysiambi"   "LespMich"   "Guilcili"  
    ## [296] "Vachfarn.1" "Clitfair"   "Andiiner.1" "Vignlute"   "Albicarb"  
    ## [301] "Eriocrin"   "Mimodipl.1" "Calogala"   "Tephsess"   "Teraunci"  
    ## [306] "Centvirg.1" "Desmcube"   "Chamglan.1" "Indifrut"   "Bauhgalp"  
    ## [311] "Senndomi"   "Crotspec"   "Crotpumi"   "Galasyna"   "Sennalex"  
    ## [316] "Aescsens"   "Indicolu.1" "AescL"      "Champedi.2" "Vachchor"  
    ## [321] "AlysDesv"   "Desmproc"   "Vignvexi"   "Clitfalc"   "Vignumbe"  
    ## [326] "Phasluna.1" "Ancipedu"   "Galaparv"   "Desmscor"   "Canaplag"  
    ## [331] "PoirVent"   "Sigmekma"   "Zapocara"   "Nissvinc"   "Medipoly"  
    ## [336] "Erytglan"   "Galalign"   "Vignungu"   "Galaspic"   "Teralabi"  
    ## [341] "Galadict"   "Crotpurd"   "BauhPlum"   "Indilesp"   "Galafuer"  
    ## [346] "Sennpend"   "Senntora"   "Indimicr"   "Cassemar"   "Dalbf"     
    ## [351] "Cassbifl"   "Galafili"   "CentDC"     "Vachacui"   "Sennmexi.3"
    ## [356] "Medilupi"   "Cenopell"   "Ctenpleu"

``` r
(df_equivalencias <- data.frame(
  nombre_original = nombres_largos,
  abreviado = colnames(mi_fam)))
```

    ##                                                           nombre_original
    ## 1                                                 Poitea galegoides Vent.
    ## 2                                               Canavalia rosea (Sw.) DC.
    ## 3                                            Senna occidentalis (L.) Link
    ## 4                                   Flemingia strobilifera (L.) W.T.Aiton
    ## 5                                          Alysicarpus vaginalis (L.) DC.
    ## 6                                                    Clitoria ternatea L.
    ## 7                                                   Acacia mangium Willd.
    ## 8                                                 Cajanus cajan (L.) Huth
    ## 9                                                        Mimosa pudica L.
    ## 10                                            Albizia lebbeck (L.) Benth.
    ## 11                           Centrosema plumieri (Turpin ex Pers.) Benth.
    ## 12                                               Mucuna pruriens (L.) DC.
    ## 13                                                   Bauhinia purpurea L.
    ## 14                                       Caesalpinia pulcherrima (L.) Sw.
    ## 15                                    Leucaena leucocephala (Lam.) de Wit
    ## 16                                           Haematoxylum campechianum L.
    ## 17                                                   Tamarindus indica L.
    ## 18                                       Entada gigas (L.) Fawc. & Rendle
    ## 19                                                 Bauhinia monandra Kurz
    ## 20                                    Delonix regia (Bojer ex Hook.) Raf.
    ## 21                                           Prosopis juliflora (Sw.) DC.
    ## 22                                     Centrosema virginianum (L.) Benth.
    ## 23                                                        Mimosa pigra L.
    ## 24                                     Chamaecrista nictitans (L.) Moench
    ## 25                                                    Trifolium repens L.
    ## 26                                                   Crotalaria incana L.
    ## 27                                               Crotalaria pallida Aiton
    ## 28                                                   Crotalaria retusa L.
    ## 29                                                  Hymenaea courbaril L.
    ## 30                                                Crotalaria verrucosa L.
    ## 31                                            Samanea saman (Jacq.) Merr.
    ## 32                                            Albizia julibrissin Durazz.
    ## 33                                                   Abrus precatorius L.
    ## 34                                            Desmodium incanum (Sw.) DC.
    ## 35                                     Dalbergia ecastaphyllum (L.) Taub.
    ## 36                                Senna siamea (Lam.) H.S.Irwin & Barneby
    ## 37                                     Macroptilium lathyroides (L.) Urb.
    ## 38                                    Pithecellobium dulce (Roxb.) Benth.
    ## 39                                Desmodium ciliare (Muhl. ex Willd.) DC.
    ## 40                                          Desmodium tortuosum (Sw.) DC.
    ## 41                                                   Crotalaria juncea L.
    ## 42                                   Pithecellobium circinale (L.) Benth.
    ## 43                                                  Phaseolus vulgaris L.
    ## 44                                                       Inga vera Willd.
    ## 45                                            Centrosema pubescens Benth.
    ## 46                            Senna spectabilis (DC.) H.S.Irwin & Barneby
    ## 47                             Senna obtusifolia (L.) H.S.Irwin & Barneby
    ## 48                                                  Arcoa gonavensis Urb.
    ## 49                                                  Poitea campanilla DC.
    ## 50     Vachellia macracantha (Humb. & Bonpl. ex Willd.) Seigler & Ebinger
    ## 51                              Pictetia sulcata (P.Beauv.) Beyra & Lavin
    ## 52                                      Biancaea decapetala (Roth) O.Deg.
    ## 53                                       Calliandra haematocephala Hassk.
    ## 54                       Rhodopis planisiliqua var. lowdenii (Judd) Alain
    ## 55                        Guilandina sphaerosperma (Urb. & Ekman) Britton
    ## 56                                          Calliandra pedicellata Benth.
    ## 57                                        Rhynchosia reticulata (Sw.) DC.
    ## 58     Chamaecrista lineata var. brachyloba (Griseb.) H.S.Irwin & Barneby
    ## 59                                                 Senna alata (L.) Roxb.
    ## 60                                    Libidibia coriaria (Jacq.) Schltdl.
    ## 61                                        Strongylodon macrobotrys A.Gray
    ## 62                                         Calliandra surinamensis Benth.
    ## 63                                                     Coronilla varia L.
    ## 64               Calliandra houstoniana var. calothyrsus (Meisn.) Barneby
    ## 65                                                      Inga edulis Mart.
    ## 66                                Grona triflora (L.) H.Ohashi & K.Ohashi
    ## 67                                                 Erythrina variegata L.
    ## 68                                        Gliricidia sepium (Jacq.) Kunth
    ## 69                                                   Guilandina bonduc L.
    ## 70                                            Leucaena macrophylla Benth.
    ## 71                                  Macroptilium atropurpureum (DC.) Urb.
    ## 72                                                Adenanthera pavonina L.
    ## 73                                      Rhynchosia phaseoloides (Sw.) DC.
    ## 74                                                Parkinsonia aculeata L.
    ## 75                                Senna atomaria (L.) H.S.Irwin & Barneby
    ## 76                                                  Zornia reticulata Sm.
    ## 77                                                      Cassia fistula L.
    ## 78                                         Stylosanthes hamata (L.) Taub.
    ## 79                             Barnebydendron riedelii (Tul.) J.H.Kirkbr.
    ## 80                                                  Bauhinia variegata L.
    ## 81                     Tara vesicaria (L.) Molinari, Sánchez Och. & Mayta
    ## 82                                                            Tara Molina
    ## 83                              Leucaena leucocephala subsp. leucocephala
    ## 84                                     Anadenanthera peregrina (L.) Speg.
    ## 85                                                 Glycine max (L.) Merr.
    ## 86                                                               Fabaceae
    ## 87                     Parasenegalia vogeliana (Steud.) Seigler & Ebinger
    ## 88                                                Caesalpinia Plum. ex L.
    ## 89                                                           Poitea Vent.
    ## 90                                                          Canavalia DC.
    ## 91                                                       Rhynchosia Lour.
    ## 92                                                 Bauhinia divaricata L.
    ## 93                               Chamaecrista fasciculata (Michx.) Greene
    ## 94                                   Jupunba glauca (Urb.) Britton & Rose
    ## 95                                                            Senna Mill.
    ## 96                                            Lablab purpureus (L.) Sweet
    ## 97                                                    Arachis hypogaea L.
    ## 98                        Crotalaria pallida var. obovata (G.Don) Polhill
    ## 99                                 Erythrina poeppigiana (Walp.) O.F.Cook
    ## 100                                               Arachis glabrata Benth.
    ## 101                                    Arachis pintoi Krapov. & W.C.Greg.
    ## 102                                              Acacia melanoxylon R.Br.
    ## 103                          Albizia niopoides (Spruce ex Benth.) Burkart
    ## 104                                            Galactia schomburgkii Urb.
    ## 105                                                 Piscidia ekmanii Rudd
    ## 106                                                  Galactia buchii Urb.
    ## 107                            Mimosa domingensis (Bertero ex DC.) Benth.
    ## 108                                         Desmanthus leptophyllus Kunth
    ## 109                                         Desmodium glabrum (Mill.) DC.
    ## 110                                    Indigofera colutea (Burm.f.) Merr.
    ## 111                                     Leucaena trichodes (Jacq.) Benth.
    ## 112                         Calliandra haematomma (Bertero ex DC.) Benth.
    ## 113                                         Caesalpinia barahonensis Urb.
    ## 114                                      Coursetia caribaea (Jacq.) Lavin
    ## 115                                         Poitea paucifolia (DC.) Lavin
    ## 116                         Denisophytum buchii (Urb.) Gagnon & G.P.Lewis
    ## 117                   Parasenegalia skleroxyla (Tussac) Seigler & Ebinger
    ## 118                                           Caesalpinia brasiliensis L.
    ## 119                       Senna angustisiliqua (Lam.) H.S.Irwin & Barneby
    ## 120                                     Chamaecrista lineata (Sw.) Greene
    ## 121                                                  Phaseolus lunatus L.
    ## 122                                    Cojoba arborea (L.) Britton & Rose
    ## 123                                             Desmodium affine Schltdl.
    ## 124                                                   Cassia grandis L.f.
    ## 125                                             Mimosa diplacantha Benth.
    ## 126                                                 Inga vera subsp. vera
    ## 127                                          Tephrosia cinerea (L.) Pers.
    ## 128                                              Acacia scleroxyla Tussac
    ## 129                                                     Desmanthus Willd.
    ## 130                                          Acaciella glauca (L.) L.Rico
    ## 131                                              Mucuna urens (L.) Medik.
    ## 132                                          Desmodium axillare (Sw.) DC.
    ## 133               Vachellia barahonensis (Urb. & Ekman) Seigler & Ebinger
    ## 134      Vachellia oviedoensis (R.G.García & M.M.Mejía) Seigler & Ebinger
    ## 135                                                Lysiloma sabicu Benth.
    ## 136                 Vachellia cucuyo (Barneby & Zanoni) Seigler & Ebinger
    ## 137                             Vachellia tortuosa (L.) Seigler & Ebinger
    ## 138                                       Desmanthus virgatus (L.) Willd.
    ## 139                                         Canavalia nitida (Cav.) Piper
    ## 140                              Sigmoidotropis elegans (Piper) A.Delgado
    ## 141                                                    Galactia dubia DC.
    ## 142                           Zapoteca portoricensis subsp. portoricensis
    ## 143                        Sophora tomentosa var. occidentalis (L.) Isely
    ## 144                                               Crotalaria lotifolia L.
    ## 145                                          Caesalpinia domingensis Urb.
    ## 146                        Libidibia monosperma (Tul.) Gagnon & G.P.Lewis
    ## 147                               Dalea carthagenensis (Jacq.) J.F.Macbr.
    ## 148                                                 Trifolium pratense L.
    ## 149                                                  Medicago lupulina L.
    ## 150      Chamaecrista nictitans var. glabrata (Vogel) H.S.Irwin & Barneby
    ## 151                                       Lupinus mexicanus Cerv. ex Lag.
    ## 152                              Genista monspessulana (L.) L.A.S.Johnson
    ## 153                             Senna ligustrina (L.) H.S.Irwin & Barneby
    ## 154                      Chamaecrista glandulosa var. pinetorum (Britton)
    ## 155                            Senna mexicana (Jacq.) H.S.Irwin & Barneby
    ## 156                                        Lonchocarpus neurophyllus Urb.
    ## 157                           Jupunba oppositifolia (Urb.) Britton & Rose
    ## 158                                          Brya buxifolia (Murray) Urb.
    ## 159                      Jupunba abbottii (Rose & Leonard) Britton & Rose
    ## 160                                 Lonchocarpus heptaphyllus (Poir.) DC.
    ## 161                                       Poitea glyciphylla (Poir.) Urb.
    ## 162                             Sigmoidotropis antillana (Urb.) A.Delgado
    ## 163                                    Mora ekmanii (Urb.) Britton & Rose
    ## 164                                              Senna sophera (L.) Roxb.
    ## 165                                       Rhodopis planisiliqua (L.) Urb.
    ## 166                                   Cynometra portoricensis Krug & Urb.
    ## 167                                         Dalbergia berteroi (DC.) Urb.
    ## 168                                                   Inga ruiziana G.Don
    ## 169                                          Mora abbottii Rose & Leonard
    ## 170                                    Lonchocarpus longipes Urb. & Ekman
    ## 171                                               Lathyrus articulatus L.
    ## 172                                           Lupinus plattensis S.Watson
    ## 173                                               Trifolium dubium Sibth.
    ## 174                                Cojoba samanensis R.G.García & Peguero
    ## 175                           Ateleia gummifera (Bertero ex DC.) D.Dietr.
    ## 176                              Senna angustisiliqua var. angustisiliqua
    ## 177                                           Cojoba arborea var. arborea
    ## 178                 Denisophytum pauciflorum (Griseb.) Gagnon & G.P.Lewis
    ## 179                             Cojoba bahorucensis J.W.Grimes & R.García
    ## 180                   Chloroleucon guantanamense (Britton) Britton & Rose
    ## 181                               Neustanthus phaseoloides (Roxb.) Benth.
    ## 182                                             Aeschynomene americana L.
    ## 183                                    Sesbania bispinosa (Jacq.) W.Wight
    ## 184                           Cojoba filipes (Vent.) Barneby & J.W.Grimes
    ## 185                                                Pictetia obcordata DC.
    ## 186                                                   Ormosia krugii Urb.
    ## 187                      Albizia berteroana (Balb. ex DC.) Fawc. & Rendle
    ## 188                                          Poitea multiflora (Sw.) Urb.
    ## 189                                            Mimosa candollei R.Grether
    ## 190                             Jupunba obovalis (A.Rich.) Britton & Rose
    ## 191                                             Inga laurina (Sw.) Willd.
    ## 192                                 Calliandra haematomma var. haematomma
    ## 193                              Dalea carthagenensis var. carthagenensis
    ## 194                                                  Pithecellobium Mart.
    ## 195                                                            Inga Mill.
    ## 196                                   Tephrosia sinapou (Buc'hoz) A.Chev.
    ## 197                              Zapoteca portoricensis (Jacq.) H.M.Hern.
    ## 198           Calliandra haematomma var. rivularis (Urb. & Ekman) Barneby
    ## 199                                       Pithecellobium domingense Alain
    ## 200                                         Andira inermis (W.Wright) DC.
    ## 201                                     Poitea galegoides var. galegoides
    ## 202                                                Melilotus albus Medik.
    ## 203                            Senna uniflora (Mill.) H.S.Irwin & Barneby
    ## 204                                                         Mucuna Adans.
    ## 205                    Peltophorum dubium var. berteroanum (Urb.) Barneby
    ## 206                       Coulteria cubensis (Greenm.) Sotuyo & G.P.Lewis
    ## 207                            Lonchocarpus sericeus (Poir.) Kunth ex DC.
    ## 208                                                         Crotalaria L.
    ## 209              Senna mexicana var. berteriana (DC.) H.S.Irwin & Barneby
    ## 210                 Mimosa pudica var. unijuga (Duchass. & Walp.) Griseb.
    ## 211                      Dalbergia ovalis (L.) P.L.R.Moraes & L.P.Queiroz
    ## 212                                            Neptunia plena (L.) Benth.
    ## 213                                           Erythrina corallodendron L.
    ## 214                                           Entada polystachya (L.) DC.
    ## 215                          Rhodopis rudolphioides (Griseb.) L.P.Queiroz
    ## 216                                                       Desmodium Desv.
    ## 217                                      Poiretia punctata (Willd.) Desv.
    ## 218                                       Desmodium intortum (Mill.) Urb.
    ## 219                                 Aeschynomene americana var. americana
    ## 220                                           Crotalaria stipularia Desv.
    ## 221                                         Tephrosia purpurea (L.) Pers.
    ## 222                                      Barbieria pinnata (Pers.) Baill.
    ## 223                                                     Calliandra Benth.
    ## 224                         Cojoba zanonii (Barneby) Barneby & J.W.Grimes
    ## 225                          Senegalia angustifolia (Lam.) Britton & Rose
    ## 226                                         Pterocarpus officinalis Jacq.
    ## 227                           Senna gundlachii (Urb.) H.S.Irwin & Barneby
    ## 228         Chamaecrista nictitans var. diffusa (DC.) H.S.Irwin & Barneby
    ## 229                                        Lonchocarpus pycnophyllus Urb.
    ## 230                                                     Galactia P.Browne
    ## 231                                                   Chamaecrista Moench
    ## 232                                            Aeschynomene villosa Poir.
    ## 233                               Acacia auriculiformis A.Cunn. ex Benth.
    ## 234                               Albizia berteriana (DC.) Fawc. & Rendle
    ## 235                         Indigofera subulata var. scabra (Roth) Meikle
    ## 236                                         Lespedeza juncea (L.f.) Pers.
    ## 237                                Grona barbata (L.) H.Ohashi & K.Ohashi
    ## 238                      Senna septemtrionalis (Viv.) H.S.Irwin & Barneby
    ## 239                                Vachellia farnesiana (L.) Wight & Arn.
    ## 240                                       Mimosa ceratonia var. ceratonia
    ## 241                                          Aeschynomene pratensis Small
    ## 242                                             Erythrina berteroana Urb.
    ## 243                            Grona adscendens (Sw.) H.Ohashi & K.Ohashi
    ## 244                                      Erythrina leptopoda Urb. & Ekman
    ## 245                                                             Mimosa L.
    ## 246                                                  Sophora tomentosa L.
    ## 247                                     Zapoteca nervosa (Urb.) H.M.Hern.
    ## 248                                                Ateleia (DC.) D.Dietr.
    ## 249                                               Indigofera tinctoria L.
    ## 250                                     Chamaecrista diphylla (L.) Greene
    ## 251                                            Zornia diphylla (L.) Pers.
    ## 252                                            Rhynchosia minima (L.) DC.
    ## 253                                        Albizia procera (Roxb.) Benth.
    ## 254                                                           Apios Fabr.
    ## 255                                                       Tephrosia Pers.
    ## 256                                Pithecellobium unguis-cati (L.) Benth.
    ## 257                                         Galactia striata (Jacq.) Urb.
    ## 258              Senna polyphylla var. montis-christi H.S.Irwin & Barneby
    ## 259                                          Senna mexicana var. mexicana
    ## 260                                       Sophora albopetiolulata Leonard
    ## 261                         Chamaecrista portoricensis var. portoricensis
    ## 262                                       Machaerium lunatum (L.f.) Ducke
    ## 263                                            Grona barbata var. barbata
    ## 264                                   Ateleia microcarpa (Pers.) D.Dietr.
    ## 265                                         Calopogonium mucunoides Desv.
    ## 266                                         Senna bicapsularis (L.) Roxb.
    ## 267                                                  Acacia confusa Merr.
    ## 268           Mucuna pruriens var. utilis (Wall. ex Wight) Baker ex Burck
    ## 269                                          Pachyrhizus erosus (L.) Urb.
    ## 270                                              Zornia microphylla Desv.
    ## 271                                         Indigofera suffruticosa Mill.
    ## 272 Chamaecrista pedicellaris var. adenosperma (Urb.) H.S.Irwin & Barneby
    ## 273                                                 Tephrosia senna Kunth
    ## 274                                    Galactia longifolia (Jacq.) Benth.
    ## 275                                        Sesbania sericea (Willd.) Link
    ## 276                                       Chamaecrista pilosa var. pilosa
    ## 277                                   Stylosanthes guianensis (Aubl.) Sw.
    ## 278                                     Indigofera subulata Vahl ex Poir.
    ## 279                                    Tephrosia purpurea subsp. purpurea
    ## 280                                                 Erythrina buchii Urb.
    ## 281                                     Diphysa americana (Mill.) M.Sousa
    ## 282                                                    Enterolobium Mart.
    ## 283                              Enterolobium cyclocarpum (Jacq.) Griseb.
    ## 284                                            Poitea dubia (Poir.) Lavin
    ## 285                                          Mucuna mutisiana (Kunth) DC.
    ## 286                                               Lathyrus oleraceus Lam.
    ## 287                                      Dalbergia brownei (Jacq.) Schinz
    ## 288                     Poitea galegoides var. stenophylla Ekman ex Lavin
    ## 289                           Chamaecrista pedicellaris var. pedicellaris
    ## 290                                       Sesbania grandiflora (L.) Pers.
    ## 291                                                 Bauhinia tomentosa L.
    ## 292                                                Entada rheedei Spreng.
    ## 293                                                 Lysiloma ambigua Urb.
    ## 294                                                      Lespedeza Michx.
    ## 295                                 Guilandina ciliata Bergius ex Wikstr.
    ## 296                                  Vachellia farnesiana var. farnesiana
    ## 297                                     Clitoria fairchildiana R.A.Howard
    ## 298                                         Andira inermis subsp. inermis
    ## 299                                          Vigna luteola (Jacq.) Benth.
    ## 300                                            Albizia carbonaria Britton
    ## 301                                       Eriosema crinitum (Kunth) G.Don
    ## 302                                   Mimosa diplotricha var. diplotricha
    ## 303                               Calopogonium galactoides (Kunth) Hemsl.
    ## 304                                 Tephrosia sessiliflora (Poir.) Hassl.
    ## 305                                           Teramnus uncinatus (L.) Sw.
    ## 306                                  Centrosema virginianum Griseb., 1857
    ## 307                                             Desmodium cubense Griseb.
    ## 308                                   Chamaecrista glandulosa (L.) Greene
    ## 309                                             Indigofera fruticosa Rose
    ## 310                                             Bauhinia galpinii N.E.Br.
    ## 311                       Senna domingensis (Spreng.) H.S.Irwin & Barneby
    ## 312                                           Crotalaria spectabilis Roth
    ## 313                                              Crotalaria pumila Ortega
    ## 314                                                Galactia synandra Urb.
    ## 315                                               Senna alexandrina Mill.
    ## 316                                            Aeschynomene sensitiva Sw.
    ## 317                                       Indigofera colutea var. colutea
    ## 318                                                       Aeschynomene L.
    ## 319                               Chamaecrista pedicellaris (DC.) Britton
    ## 320                     Vachellia choriophylla (Benth.) Seigler & Ebinger
    ## 321                                                     Alysicarpus Desv.
    ## 322                              Desmodium procumbens (Mill.) C.L.Hitchc.
    ## 323                                          Vigna vexillata (L.) A.Rich.
    ## 324                                         Clitoria falcata var. falcata
    ## 325                              Vigna umbellata (Thunb.) Ohwi & H.Ohashi
    ## 326                                    Phaseolus lunatus Billb. ex Beurl.
    ## 327                Ancistrotropis peduncularis (Fawc. & Rendle) A.Delgado
    ## 328                                           Galactia parvifolia A.Rich.
    ## 329                               Desmodium scorpiurus (Sw.) Desv. ex DC.
    ## 330                                          Canavalia plagiosperma Piper
    ## 331                                                        Poiretia Vent.
    ## 332                             Sigmoidotropis ekmaniana (Urb.) A.Delgado
    ## 333                                 Zapoteca caracasana subsp. caracasana
    ## 334               Nissolia vincentina (Ker Gawl.) T.M.Moura & Fort.-Perez
    ## 335                                                Medicago polymorpha L.
    ## 336         Erythrostemon glandulosus (Bertero ex DC.) Gagnon & G.P.Lewis
    ## 337                               Galactia lignosa (Turpin ex Pers.) Urb.
    ## 338                                          Vigna unguiculata (L.) Walp.
    ## 339                                   Galactia spiciformis Torr. & A.Gray
    ## 340                                      Teramnus labialis (L.f.) Spreng.
    ## 341                                            Galactia dictyophylla Urb.
    ## 342                                            Crotalaria purdiana H.Senn
    ## 343                                                  Bauhinia Plum. ex L.
    ## 344                                       Indigofera lespedezioides Kunth
    ## 345                                               Galactia fuertesii Urb.
    ## 346                            Senna pendula var. indecora (Kunth) Luckow
    ## 347                                                 Senna tora (L.) Roxb.
    ## 348                                           Indigofera microcarpa Desv.
    ## 349                                         Cassia emarginata Berry, 1916
    ## 350                                                        Dalbergia L.f.
    ## 351                                                  Cassia biflora Bojer
    ## 352                                    Galactia filiformis (Jacq.) Benth.
    ## 353                                               Centrosema (DC.) Benth.
    ## 354                         Vachellia acuifera (Benth.) Seigler & Ebinger
    ## 355     Senna mexicana var. berteroana (Balb. ex DC.) H.S.Irwin & Barneby
    ## 356                                                    Medicago lupina L.
    ## 357                      Cenostigma pellucidum (Vogel) Gagnon & G.P.Lewis
    ## 358               Ctenodon pleuronervius (DC.) D.B.O.S.Cardoso & H.C.Lima
    ##      abreviado
    ## 1     Poitgale
    ## 2     Canarose
    ## 3     Sennocci
    ## 4     Flemstro
    ## 5     Alysvagi
    ## 6     Clittern
    ## 7     Acacmang
    ## 8     Cajacaja
    ## 9     Mimopudi
    ## 10    Albilebb
    ## 11    Centplum
    ## 12    Mucuprur
    ## 13    Bauhpurp
    ## 14    Caespulc
    ## 15    Leucleuc
    ## 16    Haemcamp
    ## 17    Tamaindi
    ## 18    Entagiga
    ## 19    Bauhmona
    ## 20    Deloregi
    ## 21    Prosjuli
    ## 22    Centvirg
    ## 23    Mimopigr
    ## 24    Chamnict
    ## 25    Trifrepe
    ## 26    Crotinca
    ## 27    Crotpall
    ## 28    Crotretu
    ## 29    Hymecour
    ## 30    Crotverr
    ## 31    Samasama
    ## 32    Albijuli
    ## 33    Abruprec
    ## 34    Desminca
    ## 35    Dalbecas
    ## 36    Sennsiam
    ## 37    Macrlath
    ## 38    Pithdulc
    ## 39    Desmcili
    ## 40    Desmtort
    ## 41    Crotjunc
    ## 42    Pithcirc
    ## 43    Phasvulg
    ## 44    Ingavera
    ## 45    Centpube
    ## 46    Sennspec
    ## 47    Sennobtu
    ## 48    Arcogona
    ## 49    Poitcamp
    ## 50    Vachmacr
    ## 51    Pictsulc
    ## 52    Biandeca
    ## 53    Callhaem
    ## 54    Rhodplan
    ## 55    Guilspha
    ## 56    Callpedi
    ## 57    Rhynreti
    ## 58    Chamline
    ## 59    Sennalat
    ## 60    Libicori
    ## 61    Stromacr
    ## 62    Callsuri
    ## 63    Corovari
    ## 64    Callhous
    ## 65    Ingaedul
    ## 66    Grontrif
    ## 67    Erytvari
    ## 68    Glirsepi
    ## 69    Guilbond
    ## 70    Leucmacr
    ## 71    Macratro
    ## 72    Adenpavo
    ## 73    Rhynphas
    ## 74    Parkacul
    ## 75    Sennatom
    ## 76    Zornreti
    ## 77    Cassfist
    ## 78    Stylhama
    ## 79    Barnried
    ## 80    Bauhvari
    ## 81    Taravesi
    ## 82    TaraMoli
    ## 83  Leucleuc.1
    ## 84    Anadpere
    ## 85     Glycmax
    ## 86         NA.
    ## 87    Paravoge
    ## 88    CaesPlum
    ## 89    PoitVent
    ## 90      CanaDC
    ## 91    RhynLour
    ## 92    Bauhdiva
    ## 93    Chamfasc
    ## 94    Jupuglau
    ## 95    SennMill
    ## 96    Lablpurp
    ## 97    Arachypo
    ## 98  Crotpall.1
    ## 99    Erytpoep
    ## 100   Aracglab
    ## 101   Aracpint
    ## 102   Acacmela
    ## 103   Albiniop
    ## 104   Galascho
    ## 105   Piscekma
    ## 106   Galabuch
    ## 107   Mimodomi
    ## 108   Desmlept
    ## 109   Desmglab
    ## 110   Indicolu
    ## 111   Leuctric
    ## 112 Callhaem.1
    ## 113   Caesbara
    ## 114   Courcari
    ## 115   Poitpauc
    ## 116   Denibuch
    ## 117   Paraskle
    ## 118   Caesbras
    ## 119   Sennangu
    ## 120 Chamline.1
    ## 121   Phasluna
    ## 122   Cojoarbo
    ## 123   Desmaffi
    ## 124   Cassgran
    ## 125   Mimodipl
    ## 126 Ingavera.1
    ## 127   Tephcine
    ## 128   Acacscle
    ## 129   DesmWill
    ## 130   Acacglau
    ## 131   Mucuuren
    ## 132   Desmaxil
    ## 133   Vachbara
    ## 134   Vachovie
    ## 135   Lysisabi
    ## 136   Vachcucu
    ## 137   Vachtort
    ## 138   Desmvirg
    ## 139   Cananiti
    ## 140   Sigmeleg
    ## 141   Galadubi
    ## 142   Zapoport
    ## 143   Sophtome
    ## 144   Crotloti
    ## 145   Caesdomi
    ## 146   Libimono
    ## 147   Dalecart
    ## 148   Trifprat
    ## 149   Medilupu
    ## 150 Chamnict.1
    ## 151   Lupimexi
    ## 152   Genimons
    ## 153   Sennligu
    ## 154   Chamglan
    ## 155   Sennmexi
    ## 156   Loncneur
    ## 157   Jupuoppo
    ## 158   Bryabuxi
    ## 159   Jupuabbo
    ## 160   Lonchept
    ## 161   Poitglyc
    ## 162   Sigmanti
    ## 163   Moraekma
    ## 164   Sennsoph
    ## 165 Rhodplan.1
    ## 166   Cynoport
    ## 167   Dalbbert
    ## 168   Ingaruiz
    ## 169   Moraabbo
    ## 170   Lonclong
    ## 171   Latharti
    ## 172   Lupiplat
    ## 173   Trifdubi
    ## 174   Cojosama
    ## 175   Atelgumm
    ## 176 Sennangu.1
    ## 177 Cojoarbo.1
    ## 178   Denipauc
    ## 179   Cojobaho
    ## 180   Chloguan
    ## 181   Neusphas
    ## 182   Aescamer
    ## 183   Sesbbisp
    ## 184   Cojofili
    ## 185   Pictobco
    ## 186   Ormokrug
    ## 187   Albibert
    ## 188   Poitmult
    ## 189   Mimocand
    ## 190   Jupuobov
    ## 191   Ingalaur
    ## 192 Callhaem.2
    ## 193 Dalecart.1
    ## 194   PithMart
    ## 195   IngaMill
    ## 196   Tephsina
    ## 197 Zapoport.1
    ## 198 Callhaem.3
    ## 199   Pithdomi
    ## 200   Andiiner
    ## 201 Poitgale.1
    ## 202   Melialbu
    ## 203   Sennunif
    ## 204   MucuAdan
    ## 205   Peltdubi
    ## 206   Coulcube
    ## 207   Loncseri
    ## 208      CrotL
    ## 209 Sennmexi.1
    ## 210 Mimopudi.1
    ## 211   Dalboval
    ## 212   Neptplen
    ## 213   Erytcora
    ## 214   Entapoly
    ## 215   Rhodrudo
    ## 216   DesmDesv
    ## 217   Poirpunc
    ## 218   Desminto
    ## 219 Aescamer.1
    ## 220   Crotstip
    ## 221   Tephpurp
    ## 222   Barbpinn
    ## 223   CallBent
    ## 224   Cojozano
    ## 225   Seneangu
    ## 226   Pteroffi
    ## 227   Senngund
    ## 228 Chamnict.2
    ## 229   Loncpycn
    ## 230   GalaBrow
    ## 231   ChamMoen
    ## 232   Aescvill
    ## 233   Acacauri
    ## 234 Albibert.1
    ## 235   Indisubu
    ## 236   Lespjunc
    ## 237   Gronbarb
    ## 238   Sennsept
    ## 239   Vachfarn
    ## 240   Mimocera
    ## 241   Aescprat
    ## 242   Erytbert
    ## 243   Gronadsc
    ## 244   Erytlept
    ## 245      MimoL
    ## 246 Sophtome.1
    ## 247   Zaponerv
    ## 248     AtelDC
    ## 249   Inditinc
    ## 250   Chamdiph
    ## 251   Zorndiph
    ## 252   Rhynmini
    ## 253   Albiproc
    ## 254   ApioFabr
    ## 255   TephPers
    ## 256   Pithcati
    ## 257   Galastri
    ## 258   Sennpoly
    ## 259 Sennmexi.2
    ## 260   Sophalbo
    ## 261   Champort
    ## 262   Machluna
    ## 263 Gronbarb.1
    ## 264   Atelmicr
    ## 265   Calomucu
    ## 266   Sennbica
    ## 267   Acacconf
    ## 268 Mucuprur.1
    ## 269   Pacheros
    ## 270   Zornmicr
    ## 271   Indisuff
    ## 272   Champedi
    ## 273   Tephsenn
    ## 274   Galalong
    ## 275   Sesbseri
    ## 276   Champilo
    ## 277   Stylguia
    ## 278 Indisubu.1
    ## 279 Tephpurp.1
    ## 280   Erytbuch
    ## 281   Diphamer
    ## 282   EnteMart
    ## 283   Entecycl
    ## 284   Poitdubi
    ## 285   Mucumuti
    ## 286   Latholer
    ## 287   Dalbbrow
    ## 288 Poitgale.2
    ## 289 Champedi.1
    ## 290   Sesbgran
    ## 291   Bauhtome
    ## 292   Entarhee
    ## 293   Lysiambi
    ## 294   LespMich
    ## 295   Guilcili
    ## 296 Vachfarn.1
    ## 297   Clitfair
    ## 298 Andiiner.1
    ## 299   Vignlute
    ## 300   Albicarb
    ## 301   Eriocrin
    ## 302 Mimodipl.1
    ## 303   Calogala
    ## 304   Tephsess
    ## 305   Teraunci
    ## 306 Centvirg.1
    ## 307   Desmcube
    ## 308 Chamglan.1
    ## 309   Indifrut
    ## 310   Bauhgalp
    ## 311   Senndomi
    ## 312   Crotspec
    ## 313   Crotpumi
    ## 314   Galasyna
    ## 315   Sennalex
    ## 316   Aescsens
    ## 317 Indicolu.1
    ## 318      AescL
    ## 319 Champedi.2
    ## 320   Vachchor
    ## 321   AlysDesv
    ## 322   Desmproc
    ## 323   Vignvexi
    ## 324   Clitfalc
    ## 325   Vignumbe
    ## 326 Phasluna.1
    ## 327   Ancipedu
    ## 328   Galaparv
    ## 329   Desmscor
    ## 330   Canaplag
    ## 331   PoirVent
    ## 332   Sigmekma
    ## 333   Zapocara
    ## 334   Nissvinc
    ## 335   Medipoly
    ## 336   Erytglan
    ## 337   Galalign
    ## 338   Vignungu
    ## 339   Galaspic
    ## 340   Teralabi
    ## 341   Galadict
    ## 342   Crotpurd
    ## 343   BauhPlum
    ## 344   Indilesp
    ## 345   Galafuer
    ## 346   Sennpend
    ## 347   Senntora
    ## 348   Indimicr
    ## 349   Cassemar
    ## 350      Dalbf
    ## 351   Cassbifl
    ## 352   Galafili
    ## 353     CentDC
    ## 354   Vachacui
    ## 355 Sennmexi.3
    ## 356   Medilupi
    ## 357   Cenopell
    ## 358   Ctenpleu

Transforma la matriz de comunidad. Este paso es importante, lo explico
[aquí](https://www.youtube.com/watch?v=yQ10lp0-nHc&list=PLDcT2n8UzsCRDqjqSeqHI1wsiNOqpYmsJ&index=10)

``` r
mi_fam_t <- decostand(mi_fam, 'hellinger') #Hellinger
```

Fuentes de estadística zonal. Para aprender más sobre la fuente de
estadística zonal de República Dominicana, que contiene un conjunto de
más de 100 variables resumidas por celdas H3, visita [este
repo](https://github.com/geofis/zonal-statistics). Debes visitar dicho
repo para poder citarlo apropiadamente.

``` r
#Matriz ambiental
tmpfile <- tempfile()
download.file(
  url = paste0(gh_zonal_stats, 'list_with_all_sources_all_resolution.RDS'),
  tmpfile, method = if(Sys.info()[['sysname']]=='Windows') 'curl' else 'libcurl')
tmprds <- readRDS(tmpfile)
za <- tmprds[[paste0('H3 resolution: ', res)]]
za_intermedia <- za %>%
  st_drop_geometry() %>% 
  select(-matches(c(' base'))) %>% 
  column_to_rownames('hex_id')
env <- za_intermedia[match(rownames(mi_fam), rownames(za_intermedia)), ]
all(rownames(mi_fam) == rownames(env)) #Si es TRUE, sigue adelante
```

    ## [1] TRUE

Se puede probar con un subconjunto de variables, generando una matriz
ambiental que seleccione variables según el grupo al que pertenecen, con
ayuda del prefijo.

``` r
# env_selecionada <- env %>%
#   st_drop_geometry() %>%
#   dplyr::select(matches('^ESA '))
# env_selecionada <- env %>%
#   st_drop_geometry() %>%
#   dplyr::select(matches('^G90-GEOM '))
# env_selecionada <- env %>%
#   st_drop_geometry() %>%
#   dplyr::select(matches('^CH-BIO '))
# env_selecionada <- env %>%
#   st_drop_geometry() %>%
#   dplyr::select(matches('^GHH '))
# env_selecionada <- env %>%
#   st_drop_geometry() %>%
#   dplyr::select(matches('^GSL '))
# env_selecionada <- env %>%
#   st_drop_geometry() %>%
#   dplyr::select(matches('^CGL '))
```

Ecología espacial.

1.  Los resultados a continuación, dependerán mucho de la resolución H3
    elegida. Sólo tenlo presente a la hora de extraer conclusiones.

2.  Para evitar discontinuidades y garantizar la integridad de la
    vecindad, es necesario trabajar con un objeto espacial que cubra
    todo el país, con independencia de que contenga hexágonos sin
    registros de presencia de GBIF. La continuidad en los análisis de
    ecología espacial, es fundamental para garantizar la vecindad. Un
    hexágono sin registros de presencia es un hábitat potencial de las
    especies de la comunidad, no una ausencia. En esta sección, “el
    territorio manda”, por lo que oportunamente le adosaremos una
    columna con los registros de presencia al objeto de estadística
    zonal (`za`) traído del repo correspondiente.

3.  Nota que los objetos `min_especies_por_hex` y `en_cuantos_hex`
    tienen asignados valor 1, lo cual significa que, en pocas palabras,
    el objeto `mi_fam`, que es con el que hacemos la mayor parte de los
    análisis en secciones anteriores, es exactamente igual a la matriz
    de comunidad original (si ejecutas `all(mc_orig == mi_fam)`
    recibirás `TRUE`, es decir, iguales). En otras análisis
    (agrupamiento, ordenación, diversidad), lo razonable sería filtrar
    la matriz original para quitar hexágonos con pocos registros o
    especies que están poco representadas. y así producir una matriz de
    comunidad de la cual poder extraer patrones específicos, algo
    necesario en los análisis anteriores. En este caso, nos interesa
    conservar la matriz íntegra.

4.  Nos interesa conservar los nombres largos en la matriz de comunidad
    `mi_fam`, así que los restablezco aquí:

``` r
colnames(mi_fam) <- colnames(mc_orig_seleccionadas)
# colnames(mi_fam_t) <- colnames(mc_orig_seleccionadas)
```

Cargaré algunos paquetes específicos:

``` r
library(ape)
library(spdep)
library(ade4)
library(adegraphics)
library(adespatial)
library(gridExtra)
library(grid)
library(gtable)
source('https://raw.githubusercontent.com/maestria-geotel-master/unidad-3-asignacion-1-vecindad-autocorrelacion-espacial/master/lisaclusters.R')
```

Lo primero que necesitamos es crear un objeto de vecindad. Como ya
señalé, necesitamos una superficie continua del territorio en cuestión,
además de que la transformaremos a objeto clase `sp`.

``` r
# Transformar matriz ambiental en objeto sp, clase del paquete sp, para generar vecindad.
# Retomo el objeto za de arriba, y genero objetos de clase sf y sp a partir de él
za_sf <- za %>%
  select(-matches(c(' base'))) %>% 
  column_to_rownames('hex_id') %>% st_as_sf
riq_hex <- mi_fam %>% mutate(riqueza = rowSums(.)) %>%
  rownames_to_column('hex_id') %>% select (riqueza, hex_id)
env_sf <- za_sf %>%
  rownames_to_column('hex_id') %>% 
  left_join(riq_hex, by = 'hex_id')
env_sp <- env_sf %>% as_Spatial
(vecindad <- env_sp %>% poly2nb)
```

    ## Neighbour list object:
    ## Number of regions: 335 
    ## Number of nonzero links: 1820 
    ## Percentage nonzero weights: 1.621742 
    ## Average number of links: 5.432836

``` r
islas <- which(card(vecindad)==0)
if(length(islas) > 0) {
  cat('\nHay islas, en concreto, la(s) fila(s)', islas, 'de env_sf\n')
  env_sf <- env_sf[-islas, ]
  env_sp <- env_sf %>% as_Spatial
  (vecindad <- env_sp %>% poly2nb)
  islas <- which(card(vecindad)==0)
  cat('\nIsla(s) eliminada(s)\n')
}
if(length(islas) == 0) cat('No hay isla(s). Proseguir con el script')
```

    ## No hay isla(s). Proseguir con el script

``` r
(pesos_b <- nb2listw(vecindad, style = 'B'))
```

    ## Characteristics of weights list object:
    ## Neighbour list object:
    ## Number of regions: 335 
    ## Number of nonzero links: 1820 
    ## Percentage nonzero weights: 1.621742 
    ## Average number of links: 5.432836 
    ## 
    ## Weights style: B 
    ## Weights constants summary:
    ##     n     nn   S0   S1    S2
    ## B 335 112225 1820 3640 41000

``` r
plot(env_sp)
centroides <- env_sf %>% st_centroid
env_xy <- centroides %>% st_coordinates %>% as.data.frame
plot(vecindad, coords = env_xy, add =T , col = 'red')
```

![](analisis-espacial-03-demo-con-datos-ecologicos_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

- Riqueza de especies de la familia *Fabaceae*: hostpots de riqueza.

``` r
lisamaps_mifam <- sapply(
  'riqueza',
  # grep('geom', names(mi_fam_t_sf), invert = T, value = T),
  function(x) {
    tryCatch(
    {m <- lisamap(
      objesp = env_sf[x] %>%   replace(is.na(.), 0),
      var = x,
      pesos = pesos_b,
      tituloleyenda = 'Significancia ("x-y", léase como "x" rodeado de "y")',
      leyenda = T,
      anchuratitulo = 50,
      tamanotitulo = 10,
      fuentedatos = 'GBIF',
      titulomapa = paste0('Clusters LISA de "', x, '"'))
    # dev.new();print(m$grafico)
    return(m$grafico)}, error = function(e) e)
    }, simplify = F)
lisamaps_mifam
```

    ## $riqueza

![](analisis-espacial-03-demo-con-datos-ecologicos_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Ahora exploraré la autocorrelación de las variables ambientales. Entre
estas, como verás, muchas están autocorrelacionadas, al tratarse de
variables continuas. Te interesa explorar qué variables están
autocorrelacionadas espacialmente, y qué especies también lo están, para
comprobar posteriormente si tanto especies como variables ambientales
tienen *coldspots* y *hotspots* coincidentes espacialmente, lo cual
sugeriría que existe asociación entre ellas.

``` r
env_num <- env_sf %>%
  st_drop_geometry %>% 
  select_if(is.numeric) %>% 
  replace(is.na(.), 0)
set.seed(999); suppressWarnings(
  auto_amb <- calcular_autocorrelacion(
    df_fuente = env_num[, sample(1:ncol(env_num), 3)],
    orden = 9,
    obj_vecindad = vecindad))
print(auto_amb, digits = 2, p.adj.method = 'holm')
```

    ## $`WCL bio12 Annual precipitation`
    ## Spatial correlogram for WCL bio12 Annual precipitation 
    ## method: Moran's I
    ##         estimate expectation variance standard deviate Pr(I) two sided    
    ## 1 (335)  0.76128    -0.00299  0.00113             22.8          <2e-16 ***
    ## 2 (335)  0.57670    -0.00299  0.00062             23.3          <2e-16 ***
    ## 3 (335)  0.40102    -0.00299  0.00045             19.0          <2e-16 ***
    ## 4 (335)  0.26069    -0.00299  0.00037             13.7          <2e-16 ***
    ## 5 (335)  0.16931    -0.00299  0.00032              9.6          <2e-16 ***
    ## 6 (335)  0.09765    -0.00299  0.00030              5.8           2e-08 ***
    ## 7 (335)  0.04473    -0.00299  0.00029              2.8            0.01 *  
    ## 8 (335)  0.00858    -0.00299  0.00027              0.7            0.69    
    ## 9 (335) -0.01848    -0.00299  0.00027             -0.9            0.69    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## $`WCL bio05 Max temperature of warmest month`
    ## Spatial correlogram for WCL bio05 Max temperature of warmest month 
    ## method: Moran's I
    ##         estimate expectation variance standard deviate Pr(I) two sided    
    ## 1 (335)  0.28143    -0.00299  0.00101              9.0          <2e-16 ***
    ## 2 (335)  0.13399    -0.00299  0.00056              5.8           5e-08 ***
    ## 3 (335)  0.05210    -0.00299  0.00040              2.7            0.04 *  
    ## 4 (335)  0.01814    -0.00299  0.00033              1.2            0.49    
    ## 5 (335) -0.00832    -0.00299  0.00029             -0.3            0.75    
    ## 6 (335) -0.04811    -0.00299  0.00027             -2.8            0.04 *  
    ## 7 (335) -0.06806    -0.00299  0.00026             -4.1           3e-04 ***
    ## 8 (335) -0.03278    -0.00299  0.00025             -1.9            0.23    
    ## 9 (335)  0.02526    -0.00299  0.00024              1.8            0.23    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## $`WCL bio15 Precipitation seasonality`
    ## Spatial correlogram for WCL bio15 Precipitation seasonality 
    ## method: Moran's I
    ##         estimate expectation variance standard deviate Pr(I) two sided    
    ## 1 (335)  0.82979    -0.00299  0.00113               25          <2e-16 ***
    ## 2 (335)  0.76131    -0.00299  0.00062               31          <2e-16 ***
    ## 3 (335)  0.68633    -0.00299  0.00045               32          <2e-16 ***
    ## 4 (335)  0.60634    -0.00299  0.00037               32          <2e-16 ***
    ## 5 (335)  0.53164    -0.00299  0.00032               30          <2e-16 ***
    ## 6 (335)  0.45632    -0.00299  0.00030               26          <2e-16 ***
    ## 7 (335)  0.37090    -0.00299  0.00029               22          <2e-16 ***
    ## 8 (335)  0.26496    -0.00299  0.00028               16          <2e-16 ***
    ## 9 (335)  0.16785    -0.00299  0.00027               10          <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Necesitaremos los prefijos de variables para graficarlas:
prefijos_disponibles <- c('ESA', 'CGL', 'GSL', 'GHH', 'WCL', 'CH-BIO', 'G90', 'G90-GEOM',
              'CGIAR-ELE', 'GFC-PTC YEAR 2000', 'GFC-LOSS', 'OSM-DIST', 'GP-CONSUNadj YEAR 2020')
```

I de Moran local, por medio de mapas LISA de *hotspots* y *coldspots*.
Aquí descubirás los *hotspots* de las variables ambientales y de las
especies. La coincidencia de *hotspots* es un indicador, a priori, de
que existe algún grado de asociación.

``` r
env_sf_num <- env_sf %>%
  select_if(is.numeric) %>% 
  replace(is.na(.), 0)
env_sf_num %>% tibble
```

    ## # A tibble: 335 × 138
    ##    `ESA Trees` `ESA Shrubland` `ESA Grassland` `ESA Cropland` `ESA Built-up`
    ##          <dbl>           <dbl>           <dbl>          <dbl>          <dbl>
    ##  1  12.6                18.4            25.1          7.22          0.622   
    ##  2  17.3                 6.25           20.7         51.9           2.83    
    ##  3   1.23                7.35           12.9          0.118         0.454   
    ##  4  24.2                10.5            36.3         13.9           0.738   
    ##  5   0.702               6.58            5.26         0.0529        2.55    
    ##  6   9.32               58.2            10.3         17.7           1.44    
    ##  7   0.0000434           0.560           0.249        0.00174       0.000608
    ##  8   1.29               16.8             0.657        0.0435        0.0155  
    ##  9  60.7                17.2            14.3          3.39          2.13    
    ## 10  66.4                 6.22           26.2          0.229         0.941   
    ## # ℹ 325 more rows
    ## # ℹ 133 more variables: `ESA Barren / sparse vegetation` <dbl>,
    ## #   `ESA Open water` <dbl>, `ESA Herbaceous wetland` <dbl>,
    ## #   `ESA Mangroves` <dbl>, `CGL Closed forest, evergreen needle leaf` <dbl>,
    ## #   `CGL Closed forest, evergreen broad leaf` <dbl>,
    ## #   `CGL Closed forest, deciduous broad leaf` <dbl>,
    ## #   `CGL Closed forest, mixed` <dbl>, …

``` r
lisamaps_amb <- sapply(
  grep('geom', names(env_sf_num), invert = T, value = T),
  function(x) {
    tryCatch(
      {m <- lisamap(
      objesp = env_sf_num[x],
      var = x,
      pesos = pesos_b,
      tituloleyenda = 'Significancia ("x-y", léase como "x" rodeado de "y")',
      leyenda = F,
      anchuratitulo = 50,
      tamanotitulo = 10,
      fuentedatos = '',
      titulomapa = paste0('Clusters LISA de "', x, '"'))
    return(m$grafico)}, error = function(e) e)
    }, simplify = F)
lisamaps_amb_rango <- seq_along(lisamaps_amb)
lisamaps_amb_cortes <- unique(c(
  min(lisamaps_amb_rango),
  (lisamaps_amb_rango)[which(lisamaps_amb_rango%%8==0)],
  max(lisamaps_amb_rango)))
lisamaps_amb_intervalos <- cut(lisamaps_amb_rango, lisamaps_amb_cortes, include.lowest = T)
lisamaps_amb_df <- data.frame(lisamaps_amb_rango, lisamaps_amb_intervalos)
lisamaps_amb$leyenda <- gtable_filter(ggplot_gtable(ggplot_build(lisamaps_amb[[1]] + theme(legend.position = "bottom"))), "guide-box")
suppressWarnings(invisible(sapply(levels(lisamaps_amb_df[, 'lisamaps_amb_intervalos']),
       function(x) {
         if(interactive()) dev.new()
         grid.arrange(
           do.call(
             'arrangeGrob',
             c(lisamaps_amb[
               lisamaps_amb_df[
                 lisamaps_amb_df$lisamaps_amb_intervalos==x, 'lisamaps_amb_rango', drop=T]
               ], nrow = 3)),
           lisamaps_amb$leyenda,
           heights=c(1.1, 0.1), nrow = 2)
       })))
```

![](analisis-espacial-03-demo-con-datos-ecologicos_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->![](analisis-espacial-03-demo-con-datos-ecologicos_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->![](analisis-espacial-03-demo-con-datos-ecologicos_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->![](analisis-espacial-03-demo-con-datos-ecologicos_files/figure-gfm/unnamed-chunk-12-4.png)<!-- -->![](analisis-espacial-03-demo-con-datos-ecologicos_files/figure-gfm/unnamed-chunk-12-5.png)<!-- -->![](analisis-espacial-03-demo-con-datos-ecologicos_files/figure-gfm/unnamed-chunk-12-6.png)<!-- -->![](analisis-espacial-03-demo-con-datos-ecologicos_files/figure-gfm/unnamed-chunk-12-7.png)<!-- -->![](analisis-espacial-03-demo-con-datos-ecologicos_files/figure-gfm/unnamed-chunk-12-8.png)<!-- -->![](analisis-espacial-03-demo-con-datos-ecologicos_files/figure-gfm/unnamed-chunk-12-9.png)<!-- -->![](analisis-espacial-03-demo-con-datos-ecologicos_files/figure-gfm/unnamed-chunk-12-10.png)<!-- -->![](analisis-espacial-03-demo-con-datos-ecologicos_files/figure-gfm/unnamed-chunk-12-11.png)<!-- -->![](analisis-espacial-03-demo-con-datos-ecologicos_files/figure-gfm/unnamed-chunk-12-12.png)<!-- -->![](analisis-espacial-03-demo-con-datos-ecologicos_files/figure-gfm/unnamed-chunk-12-13.png)<!-- -->![](analisis-espacial-03-demo-con-datos-ecologicos_files/figure-gfm/unnamed-chunk-12-14.png)<!-- -->![](analisis-espacial-03-demo-con-datos-ecologicos_files/figure-gfm/unnamed-chunk-12-15.png)<!-- -->![](analisis-espacial-03-demo-con-datos-ecologicos_files/figure-gfm/unnamed-chunk-12-16.png)<!-- -->![](analisis-espacial-03-demo-con-datos-ecologicos_files/figure-gfm/unnamed-chunk-12-17.png)<!-- -->![](analisis-espacial-03-demo-con-datos-ecologicos_files/figure-gfm/unnamed-chunk-12-18.png)<!-- -->

# Referencias
