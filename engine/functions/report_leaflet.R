# engine/functions/report_leaflet.R
# version du 11.08.26

# ---- Carte interactive principale (REFC + IC) ---------------------------
write_leaflet_html <- function(config, ctx) {
  
  stopifnot(!is.null(ctx$outputsdir), nzchar(ctx$outputsdir))
  
  maps_dir  <- file.path(ctx$outputsdir, "maps")
  refc_path <- file.path(maps_dir, "refc_e1.geojson")
  ic_path   <- file.path(maps_dir, "ic.geojson")
  
  if (!file.exists(refc_path)) stop("GeoJSON manquant: ", refc_path)
  if (!file.exists(ic_path))   stop("GeoJSON manquant: ", ic_path)
  
  refc_geojson <- paste(readLines(refc_path, warn = FALSE), collapse = "\n")
  ic_geojson   <- paste(readLines(ic_path,   warn = FALSE), collapse = "\n")
  
  ts       <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  out_html <- file.path(ctx$outputsdir, "carte_interactive.html")
  
  html <- paste0(
    "<!doctype html>
<html lang='fr'>
<head>
<meta charset='utf-8'>
<meta name='viewport' content='width=device-width, initial-scale=1'>
<title>Cartes interactives</title>
<link rel='stylesheet' href='https://unpkg.com/leaflet@1.9.4/dist/leaflet.css'/>
<script src='https://unpkg.com/leaflet@1.9.4/dist/leaflet.js'></script>
<style>
  body{font-family:Arial,sans-serif;margin:20px;}
  .meta{color:#444;margin-bottom:14px;}
  .grid{display:grid;grid-template-columns:1fr 1fr;gap:16px;}
  @media(max-width:1000px){.grid{grid-template-columns:1fr;}}
  .map{height:560px;border:1px solid #ddd;border-radius:10px;}
  .legend{background:white;padding:10px 12px;border:1px solid #ddd;
          border-radius:8px;line-height:18px;}
  .legend i{width:14px;height:14px;float:left;margin-right:8px;opacity:0.9;}
</style>
</head>
<body>
<h2>Cartographies de synthèse : </h2>
<div class='meta'>
</div>
<div class='grid'>
  <div><h3>Risque d'effets cumulés (REFC)</h3>
       <div id='map_refc' class='map'></div></div>
  <div><h3>Indice de confiance des données</h3>
       <div id='map_ic' class='map'></div></div>
</div>
<script>
var GEO_REFC = ", refc_geojson, ";
var GEO_IC   = ", ic_geojson, ";

function fmtVal(v,digits){
  if(v===null||v===undefined||Number.isNaN(Number(v)))return 'NA';
  return Number(v).toFixed(digits||4);
}

function colorREFC(v){
  return v>0.80 ? '#7a0177' :
         v>0.60 ? '#c51b8a' :
         v>0.40 ? '#f768a1' :
         v>0.20 ? '#fbb4b9' :
                   '#f1eef6';
}

var breaksREFC=[0.20,0.40,0.60,0.80];

var labelsREFC=[
  'Très faible',
  'Faible',
  'Moyen',
  'Fort',
  'Très fort'
];

function colorIC(v){
  return v>0.80?'#08519c':v>0.60?'#3182bd':v>0.40?'#6baed6':v>0.20?'#bdd7e7':'#eff3ff';
}
var breaksIC=[0.20,0.40,0.60,0.80];
var labelsIC=['Très faible','Faible','Moyen','Bon','Très bon'];

function makeMap(divId){
  var m=L.map(divId,{scrollWheelZoom:true});
  L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',
    {maxZoom:19,attribution:'&copy; OpenStreetMap'}).addTo(m);
  return m;
}

var mapREFC=makeMap('map_refc');
var layerREFC=L.geoJSON(GEO_REFC,{
  style:function(feat){
    var v=feat&&feat.properties?feat.properties.REFC_E1:null;
    return{weight:0.4,color:'#444',fillOpacity:0.75,fillColor:colorREFC(v)};
  },
  onEachFeature:function(feat,layer){
    var p=feat.properties||{};
    layer.bindPopup('<b>Maille :</b> '+(p.idmesh??'NA')+'<br/><b>REFC_E1 :</b> '+fmtVal(p.REFC_E1,4));
  }
}).addTo(mapREFC);
if(layerREFC.getBounds().isValid())mapREFC.fitBounds(layerREFC.getBounds(),{padding:[10,10]});

var mapIC=makeMap('map_ic');
var layerIC=L.geoJSON(GEO_IC,{
  style:function(feat){
    var v=feat&&feat.properties?feat.properties.IC:null;
    return{weight:0.4,color:'#444',fillOpacity:0.75,fillColor:colorIC(v)};
  },
  onEachFeature:function(feat,layer){
    var p=feat.properties||{};
    layer.bindPopup(
      '<b>Maille :</b> '+(p.idmesh??'NA')+'<br/>'+
      '<b>IC global :</b> '+fmtVal(p.IC,4)+'<br/>'+
      '<b>IQ_A_mean :</b> '+fmtVal(p.IQ_A_mean,4)+'<br/>'+
      '<b>IC_AP_mean :</b> '+fmtVal(p.IC_AP_mean,4)+'<br/>'+
      '<b>IQ_H_mean :</b> '+fmtVal(p.IQ_H_mean,4)+'<br/>'+
      '<b>IC_HP_mean :</b> '+fmtVal(p.IC_HP_mean,4));
  }
}).addTo(mapIC);
if(layerIC.getBounds().isValid())mapIC.fitBounds(layerIC.getBounds(),{padding:[10,10]});

function addLegend(map,breaks,labels,colorFn,title){
  var legend=L.control({position:'bottomright'});
  legend.onAdd=function(){
    var div=L.DomUtil.create('div','legend');
    div.innerHTML+='<b>'+title+'</b><br>';
    for(var i=0;i<labels.length;i++){
      var from=(i===0)?null:breaks[i-1];
      var to=(i<breaks.length)?breaks[i]:null;
      var sample=(to===null)?(breaks[breaks.length-1]+1):(from===null?0:(from+to)/2);
      div.innerHTML+='<i style=\"background:'+colorFn(sample)+'\"></i>'+labels[i]+'<br>';
    }
    return div;
  };
  legend.addTo(map);
}
addLegend(mapREFC,breaksREFC,labelsREFC,colorREFC,'REFC_E1');
addLegend(mapIC,breaksIC,labelsIC,colorIC,'IC global');
</script>
</body>
</html>"
  )
  
  writeLines(html, out_html, useBytes = TRUE)
  message("HTML Leaflet généré : ", out_html)
  invisible(out_html)
}

# ---- Carte robustesse stochastique (ecart_moy_ref) ----------------------
#
# Génère un HTML Leaflet autonome coloré sur ecart_moy_ref.
# Palette orange : écart faible = robuste (clair), écart fort = sensible (foncé).
# Seuils [0, 0.05, 0.10, 0.20] — valeurs dans [0,1] (REFC normalisés).
#
# @param geojson_path  Chemin vers ecart_moy_ref.geojson (WGS84, produit par step2c)
# @param outputsdir    Dossier de sortie (outputs/robustesse_stoch/)
# @return Chemin du fichier HTML généré (invisiblement)

write_leaflet_robustesse_html <- function(geojson_path, outputsdir) {
  
  stopifnot(
    "write_leaflet_robustesse_html: geojson_path manquant" =
      !is.null(geojson_path) && nzchar(geojson_path),
    "write_leaflet_robustesse_html: GeoJSON introuvable" =
      file.exists(geojson_path),
    "write_leaflet_robustesse_html: outputsdir manquant" =
      !is.null(outputsdir) && nzchar(outputsdir)
  )
  
  geojson_data <- paste(readLines(geojson_path, warn = FALSE), collapse = "\n")
  ts           <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  out_html     <- file.path(outputsdir, "carte_robustesse_stoch.html")
  
  html <- paste0(
    "<!doctype html>
<html lang='fr'>
<head>
<meta charset='utf-8'>
<meta name='viewport' content='width=device-width, initial-scale=1'>
<title>Carte robustesse stochastique</title>
<link rel='stylesheet' href='https://unpkg.com/leaflet@1.9.4/dist/leaflet.css'/>
<script src='https://unpkg.com/leaflet@1.9.4/dist/leaflet.js'></script>
<style>
  body{font-family:Arial,sans-serif;margin:20px;}
  .meta{color:#444;margin-bottom:14px;font-size:13px;}
  #map_rob{height:580px;border:1px solid #ddd;border-radius:10px;}
  .legend{background:white;padding:10px 12px;border:1px solid #ddd;
          border-radius:8px;line-height:20px;font-size:13px;}
  .legend i{width:14px;height:14px;float:left;margin-right:8px;opacity:0.9;}
</style>
</head>
<body>
<h2>Delta de rang produit par les incertitudes stochastiques entre le REFC par défaut et le REFC moyen des simulations</h2>
<div class='meta'>
  <div><b>Indicateur</b> : variation du rang de risque de chaque maille (rang 1
  = maille la plus à risque) entre l'analyse par défaut et la moyenne des
  simulations Monte Carlo, exprimée en % de l'effectif total de mailles.</div>
  <div>Une maille en <b>rouge</b> avance dans le classement (devient
  relativement plus à risque en simulation) ; une maille en <b>bleu</b>
  recule (devient relativement moins à risque). Plus la teinte est foncée,
  plus le changement de rang est important.</div>
  <div style='margin-top:6px;'><b>Généré le</b> : ", ts, "</div>
</div>
<div id='map_rob'></div>

<script>
var GEO_ROB = ", geojson_data, ";

function fmtVal(v,digits){
  if(v===null||v===undefined||Number.isNaN(Number(v)))return 'NA';
  return Number(v).toFixed(digits===undefined?4:digits);
}
function fmtInt(v){
  if(v===null||v===undefined||Number.isNaN(Number(v)))return 'NA';
  return Math.round(Number(v));
}

// Palette divergente : bleu = recule (delta > 0), rouge = avance (delta < 0)
// Seuils exprimés en % de l'effectif total de mailles
var breaksPct = [5, 10, 20];

function colorDeltaRang(v){
  if(v===null||v===undefined||Number.isNaN(Number(v)))return '#f5f5f5';
  var a = Math.abs(v);
  if(v > 0){
    return a>breaksPct[2] ? '#08519c' :
           a>breaksPct[1] ? '#3182bd' :
           a>breaksPct[0] ? '#6baed6' :
                             '#c6dbef';
  } else if (v < 0) {
    return a>breaksPct[2] ? '#a50f15' :
           a>breaksPct[1] ? '#de2d26' :
           a>breaksPct[0] ? '#fb6a4a' :
                             '#fcbba1';
  }
  return '#f7f7f7';
}

var map = L.map('map_rob', {scrollWheelZoom: true});
L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',
  {maxZoom:19, attribution:'&copy; OpenStreetMap'}).addTo(map);

var layer = L.geoJSON(GEO_ROB, {
  style: function(feat){
    var v = feat && feat.properties ? feat.properties.delta_rang_pct : null;
    return {weight:0.4, color:'#666', fillOpacity:0.80, fillColor: colorDeltaRang(v)};
  },
  onEachFeature: function(feat, layer){
    var p = feat.properties || {};
    layer.bindPopup(
      '<b>Maille :</b> '            + (p.idmesh   ?? 'NA')       + '<br/>' +
      '<b>Rang réf (défaut) :</b> ' + fmtInt(p.rang_ref)         + '<br/>' +
      '<b>Rang moyen simulé :</b> ' + fmtInt(p.rang_moy)         + '<br/>' +
      '<b>Delta de rang :</b> '     + fmtInt(p.delta_rang)       + '<br/>' +
      '<b>Delta de rang (%) :</b> ' + fmtVal(p.delta_rang_pct,1) + ' %<br/>' +
      '<b>Écart REFC moy/réf :</b> '+ fmtVal(p.ecart_moy_ref, 4) + '<br/>' +
      '<b>REFC réf :</b> '          + fmtVal(p.REFC_ref,      4) + '<br/>' +
      '<b>REFC simul :</b> '        + fmtVal(p.REFC_moy,      4)
    );
  }
}).addTo(map);

if(layer.getBounds().isValid()) map.fitBounds(layer.getBounds(), {padding:[10,10]});

var legend = L.control({position: 'bottomright'});
legend.onAdd = function(){
  var div = L.DomUtil.create('div', 'legend');
  div.innerHTML += '<b>Delta de rang</b><br>';
  div.innerHTML += '<i style=\"background:#08519c\"></i>Recule fort (> '+breaksPct[2]+'%)<br>';
  div.innerHTML += '<i style=\"background:#3182bd\"></i>Recule modéré<br>';
  div.innerHTML += '<i style=\"background:#6baed6\"></i>Recule faible<br>';
  div.innerHTML += '<i style=\"background:#c6dbef\"></i>Recule très faible<br>';
  div.innerHTML += '<i style=\"background:#f7f7f7\"></i>Stable (0 %)<br>';
  div.innerHTML += '<i style=\"background:#fcbba1\"></i>Avance très faible<br>';
  div.innerHTML += '<i style=\"background:#fb6a4a\"></i>Avance faible<br>';
  div.innerHTML += '<i style=\"background:#de2d26\"></i>Avance modérée<br>';
  div.innerHTML += '<i style=\"background:#a50f15\"></i>Avance forte (> '+breaksPct[2]+'%)<br>';
  return div;
};
legend.addTo(map);
</script>
</body>
</html>"
  )
  
  writeLines(html, out_html, useBytes = TRUE)
  message("HTML Leaflet robustesse stochastique (delta de rang) généré", out_html)
  invisible(out_html)
}

# ---- Carte robustesse déterministe X1_mat_AP ----------------------------
#
# Génère un HTML Leaflet autonome avec deux cartes côte à côte :
#   - ecart_precaution_ref : écart absolu REFC précaution vs médiane
#   - ecart_binaire_ref    : écart absolu REFC binaire vs médiane
#
# Palette orange identique à write_leaflet_robustesse_html.
# Produit dans outputs/robustesse_det/X1_mat_AP/
#
# @param comp_df   data.frame retourné par compare_scenarios pour X1_mat_AP
# @param grid_z    objet sf de la grille (CRS quelconque — reprojeté en WGS84)
# @param fact_dir  dossier de sortie (outputs/robustesse_det/X1_mat_AP/)
# @return Chemin du fichier HTML généré (invisiblement)

write_leaflet_robustesse_det_html <- function(comp_df, grid_z, fact_dir) {
  
  stopifnot(
    "write_leaflet_robustesse_det_html: comp_df manquant" = !is.null(comp_df),
    "write_leaflet_robustesse_det_html: grid_z manquant"  = !is.null(grid_z),
    "write_leaflet_robustesse_det_html: fact_dir manquant" =
      !is.null(fact_dir) && nzchar(fact_dir)
  )
  
  # Colonnes ecart nécessaires
  col_pre <- "ecart_precaution_ref"
  col_bin <- "ecart_binaire_ref"
  
  has_pre <- col_pre %in% names(comp_df)
  has_bin <- col_bin %in% names(comp_df)
  
  if (!has_pre && !has_bin) {
    warning("write_leaflet_robustesse_det_html: aucune colonne ecart_*_ref trouvée, HTML non généré.")
    return(invisible(NULL))
  }
  
  # Jointure et reprojection WGS84
  grid_wgs <- grid_z |>
    dplyr::left_join(
      comp_df |>
        dplyr::mutate(idmesh = as.character(.data$idmesh)) |>
        dplyr::select(idmesh,
                      dplyr::any_of(c(col_pre, col_bin))),
      by = "idmesh"
    ) |>
    sf::st_transform(4326)
  
  # Export GeoJSON
  maps_dir <- file.path(fact_dir, "maps")
  dir.create(maps_dir, recursive = TRUE, showWarnings = FALSE)
  
  geojson_path <- file.path(maps_dir, "X1_mat_AP_ecarts.geojson")
  tryCatch(
    sf::st_write(grid_wgs, dsn = geojson_path,
                 delete_dsn = TRUE, quiet = TRUE),
    error = function(e) {
      warning(sprintf("write_leaflet_robustesse_det_html: export GeoJSON échoué : %s",
                      e$message))
      return(invisible(NULL))
    }
  )
  
  geojson_data <- paste(readLines(geojson_path, warn = FALSE), collapse = "\n")
  ts           <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  out_html     <- file.path(fact_dir, "carte_robustesse_det_X1_mat_AP.html")
  
  # Blocs carte : une ou deux selon les colonnes disponibles
  map_blocks <- ""
  js_layers  <- ""
  
  if (has_pre) {
    map_blocks <- paste0(map_blocks,
                         "<div><h3>Écart REFC — précaution vs médiane</h3>",
                         "<div id='map_pre' class='map'></div></div>\n")
    js_layers <- paste0(js_layers, "
var mapPre = makeMap('map_pre');
var layerPre = makeLayer(GEO_X1, '", col_pre, "', mapPre);
if(layerPre.getBounds().isValid()) mapPre.fitBounds(layerPre.getBounds(),{padding:[10,10]});
addLegend(mapPre, 'Écart précaution/médiane');
")
  }
  
  if (has_bin) {
    map_blocks <- paste0(map_blocks,
                         "<div><h3>Écart REFC — binaire vs médiane</h3>",
                         "<div id='map_bin' class='map'></div></div>\n")
    js_layers <- paste0(js_layers, "
var mapBin = makeMap('map_bin');
var layerBin = makeLayer(GEO_X1, '", col_bin, "', mapBin);
if(layerBin.getBounds().isValid()) mapBin.fitBounds(layerBin.getBounds(),{padding:[10,10]});
addLegend(mapBin, 'Écart binaire/médiane');
")
  }
  
  html <- paste0(
    "<!doctype html>
<html lang='fr'>
<head>
<meta charset='utf-8'>
<meta name='viewport' content='width=device-width, initial-scale=1'>
<title>Robustesse déterministe — X1_mat_AP</title>
<link rel='stylesheet' href='https://unpkg.com/leaflet@1.9.4/dist/leaflet.css'/>
<script src='https://unpkg.com/leaflet@1.9.4/dist/leaflet.js'></script>
<style>
  body{font-family:Arial,sans-serif;margin:20px;}
  .meta{color:#444;margin-bottom:14px;font-size:13px;}
  .grid{display:grid;grid-template-columns:1fr 1fr;gap:16px;}
  @media(max-width:1000px){.grid{grid-template-columns:1fr;}}
  .map{height:540px;border:1px solid #ddd;border-radius:10px;}
  .legend{background:white;padding:10px 12px;border:1px solid #ddd;
          border-radius:8px;line-height:20px;font-size:13px;}
  .legend i{width:14px;height:14px;float:left;margin-right:8px;opacity:0.9;}
</style>
</head>
<body>
<h2>Robustesse déterministe — choix de la matrice A-P (X1_mat_AP)</h2>
<div class='meta'>
  <div><b>Indicateur</b> : écart absolu de REFC entre la matrice médiane
  (référence) et les matrices précaution et binaire.</div>
  <div>Un écart élevé indique une maille dont le score de risque dépend
  fortement du choix de la matrice activité-pression.</div>
  <div style='margin-top:6px;'><b>Généré le</b> : ", ts, "</div>
</div>
<div class='grid'>
", map_blocks, "
</div>
<script>
var GEO_X1 = ", geojson_data, ";

function fmtVal(v,digits){
  if(v===null||v===undefined||Number.isNaN(Number(v)))return 'NA';
  return Number(v).toFixed(digits||4);
}

function colorEcart(v){
  if(v===null||v===undefined||Number.isNaN(Number(v)))return '#f5f5f5';
  return v>0.20?'#d94801':v>0.10?'#f16913':v>0.05?'#fd8d3c':v>0.00?'#fdbe85':'#fff5eb';
}
var breaksEcart=[0.00,0.05,0.10,0.20];
var labelsEcart=['Nul','Très faible < 0.05','Faible < 0.1','Modéré < 0.2','Fort > 0.2'];

function makeMap(divId){
  var m=L.map(divId,{scrollWheelZoom:true});
  L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',
    {maxZoom:19,attribution:'&copy; OpenStreetMap'}).addTo(m);
  return m;
}

function makeLayer(geo, col, map){
  return L.geoJSON(geo,{
    style:function(feat){
      var v=feat&&feat.properties?feat.properties[col]:null;
      return{weight:0.4,color:'#666',fillOpacity:0.80,fillColor:colorEcart(v)};
    },
    onEachFeature:function(feat,layer){
      var p=feat.properties||{};
      layer.bindPopup(
        '<b>Maille :</b> '+(p.idmesh??'NA')+'<br/>'+
        '<b>Écart :</b> '+fmtVal(p[col],4)
      );
    }
  }).addTo(map);
}

function addLegend(map, title){
  var legend=L.control({position:'bottomright'});
  legend.onAdd=function(){
    var div=L.DomUtil.create('div','legend');
    div.innerHTML+='<b>'+title+'</b><br>';
    for(var i=0;i<labelsEcart.length;i++){
      var from=(i===0)?null:breaksEcart[i-1];
      var to=(i<breaksEcart.length)?breaksEcart[i]:null;
      var sample=(to===null)?(breaksEcart[breaksEcart.length-1]+0.1):
                 (from===null?0:(from+to)/2);
      div.innerHTML+='<i style=\"background:'+colorEcart(sample)+'\"></i>'+labelsEcart[i]+'<br>';
    }
    return div;
  };
  legend.addTo(map);
}

", js_layers, "
</script>
</body>
</html>")
  
  writeLines(html, out_html, useBytes = TRUE)
  message("HTML Leaflet robustesse déterministe X1_mat_AP généré : ", out_html)
  invisible(out_html)
}
