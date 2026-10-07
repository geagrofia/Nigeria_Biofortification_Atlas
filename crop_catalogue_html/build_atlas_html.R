# ==================================================================
# build_atlas_html.R
# Builds ONE standalone HTML file (no server needed) that replicates the
# Shiny app: crop / micronutrient selectors, a choropleth of variety counts
# per unit, click-to-select a unit, a filtered table, and a variety detail
# panel with image.
#
# Run from the app folder:   source("build_atlas_html.R")
# Output:                    atlas.html
#
# Needs: leaflet, sf, dplyr, htmlwidgets, htmltools, jsonlite, base64enc
# and pandoc (included with RStudio) for selfcontained = TRUE.
# ==================================================================
library(leaflet)
library(sf)
library(dplyr)
library(htmlwidgets)
library(htmltools)
library(jsonlite)
library(base64enc)

# ------------------------------------------------------------------
# 1. DATA  (same inputs as the Shiny app)
# ------------------------------------------------------------------
varieties <- read.csv("varieties.csv", stringsAsFactors = FALSE)
variety_details <- read.csv(
  "variety_details.csv",
  stringsAsFactors = FALSE,
  na.strings = c("", "NA", "N/A"),
  strip.white = TRUE
)

units <- st_read("units.gpkg", quiet = TRUE) |>
  st_make_valid() |>
  st_transform(4326) |>
  group_by(unit_id) |>
  summarise(unit_name = first(unit_name), .groups = "drop") |> # one row per unit_id
  st_make_valid()
stopifnot(!anyDuplicated(units$unit_id))

# Optional: shrink the file by simplifying boundaries (tolerance in degrees)
# units <- st_simplify(units, dTolerance = 0.001, preserveTopology = TRUE)

# ------------------------------------------------------------------
# 2. IMAGES  (embedded as base64 so the HTML is fully self-contained)
# ------------------------------------------------------------------
img_dirs <- c("images", "www")

resolve_image <- function(img) {
  if (is.null(img) || is.na(img)) {
    return(NULL)
  }
  img <- trimws(gsub("\\", "/", img, fixed = TRUE))
  if (!nzchar(img)) {
    return(NULL)
  }
  if (grepl("^https?://", img)) {
    return(img)
  }
  img <- sub("^(images|www)/", "", img, ignore.case = TRUE)

  for (d in img_dirs) {
    if (!dir.exists(d)) {
      next
    }
    files <- list.files(d, recursive = TRUE)
    hit <- files[tolower(files) == tolower(img)]
    if (!length(hit)) {
      hit <- files[tolower(basename(files)) == tolower(basename(img))]
    }
    if (length(hit)) {
      path <- file.path(d, hit[1])
      ext <- tolower(tools::file_ext(path))
      mime <- switch(
        ext,
        png = "image/png",
        jpg = ,
        jpeg = "image/jpeg",
        gif = "image/gif",
        webp = "image/webp",
        svg = "image/svg+xml",
        "application/octet-stream"
      )
      return(base64enc::dataURI(file = path, mime = mime))
    }
  }
  message("Image not found: '", img, "'")
  NULL
}

# ------------------------------------------------------------------
# 3. PAYLOAD  (everything the page needs, as one JSON string)
# ------------------------------------------------------------------
if (!"image" %in% names(variety_details)) {
  variety_details$image <- NA_character_
}

vd <- variety_details |>
  arrange(variety_id, is.na(image)) |> # prefer a row that has an image
  distinct(variety_id, .keep_all = TRUE)

details <- lapply(seq_len(nrow(vd)), function(i) {
  row <- vd[i, , drop = FALSE]
  img <- resolve_image(row$image)
  list(
    image = if (is.null(img)) NA_character_ else img,
    fields = as.list(row[,
      setdiff(names(row), c("variety_id", "image")),
      drop = FALSE
    ])
  )
})
names(details) <- as.character(vd$variety_id)

payload <- as.character(toJSON(
  list(
    varieties = varieties |>
      select(
        variety_id,
        variety_name,
        crop,
        release_year,
        micronutrient,
        unit_id
      ) |>
      distinct(),
    units = st_drop_geometry(units) |> select(unit_id, unit_name),
    details = details
  ),
  dataframe = "rows",
  auto_unbox = TRUE,
  na = "null"
))
payload <- gsub("</", "<\\\\/", payload) # "</" -> "<\/" so it can't end the <script> tag

# ------------------------------------------------------------------
# 4. PAGE LAYOUT (controls above the map, table + details below)
# ------------------------------------------------------------------
css <- tags$style(HTML(
  "
  body { font-family: system-ui, -apple-system, 'Segoe UI', sans-serif; }
  .atlas-controls { display:flex; flex-wrap:wrap; gap:16px; align-items:center; padding:8px 0; }
  .atlas-controls label { display:flex; flex-direction:column; font-size:12px; font-weight:600; }
  .atlas-controls select { font-size:14px; padding:4px; min-width:180px; }
  .atlas-bottom { display:flex; flex-wrap:wrap; gap:16px; margin-top:12px; }
  .atlas-table { flex:3 1 420px; min-width:0; }
  .atlas-table input { width:100%; box-sizing:border-box; padding:6px; margin-bottom:6px; }
  #tbl { max-height:420px; overflow:auto; border:1px solid #ddd; }
  #tbl table { border-collapse:collapse; width:100%; font-size:13px; }
  #tbl th { position:sticky; top:0; background:#f3f3f3; cursor:pointer; text-align:left; padding:6px 8px; }
  #tbl td { padding:5px 8px; border-top:1px solid #eee; }
  #tbl tbody tr { cursor:pointer; }
  #tbl tbody tr:hover { background:#f6fbf2; }
  #tbl tbody tr.sel { background:#dff0d0; }
  .atlas-details { flex:2 1 300px; background:#f7f7f7; border:1px solid #e3e3e3;
                   border-radius:4px; padding:12px; min-width:0; }
  .atlas-details img { max-width:100%; height:auto; border-radius:4px; margin-bottom:10px; }
  .atlas-details dt { font-weight:600; margin-top:6px; }
  .atlas-details dd { margin:0; overflow-wrap:anywhere; }
  .muted { color:#777; }
  .legend { background:#fff; padding:6px 8px; border-radius:4px; font:12px sans-serif;
            box-shadow:0 0 6px rgba(0,0,0,.3); line-height:1.4; }
  .legend .bar { height:10px; width:140px; margin:3px 0; }
  .legend .lbl { display:flex; justify-content:space-between; }
"
))

controls <- tags$div(
  css,
  tags$h2("Biofortified crop varieties by Agro-Ecological Zone"),
  tags$div(
    class = "atlas-controls",
    tags$label("Crop", tags$select(id = "crop")),
    tags$label("Micronutrient", tags$select(id = "micro")),
    tags$span(
      tags$b("Selected unit: "),
      tags$span(id = "sel_label", "All units")
    ),
    tags$button(id = "clear_sel", type = "button", "Clear selection")
  )
)

bottom <- tags$div(
  class = "atlas-bottom",
  tags$div(
    class = "atlas-table",
    tags$input(
      id = "search",
      type = "search",
      placeholder = "Search varieties..."
    ),
    tags$div(id = "tbl")
  ),
  tags$div(id = "details", class = "atlas-details"),
  tags$script(id = "atlas-data", type = "application/json", HTML(payload))
)

# ------------------------------------------------------------------
# 5. BEHAVIOUR (plain JavaScript, runs in the browser)
# ------------------------------------------------------------------
js <- r"--(
function(el, x) {
  var map = this;
  var D = JSON.parse(document.getElementById('atlas-data').textContent);
  var V = D.varieties, DET = D.details, U = D.units;
  var $ = function(id) { return document.getElementById(id); };
  var esc = function(s) {
    return String(s).replace(/[&<>"']/g, function(c) {
      return {'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c];
    });
  };
  var PAL = ['#ffffcc','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#005a32'];
  var ZERO = '#d9d9d9';

  var selected = null;          // selected unit_id
  var selectedVariety = null;   // selected variety_id
  var sortCol = 'variety_name', sortDir = 1;
  var layers = {}, unitName = {}, varName = {};
  U.forEach(function(u) { unitName[u.unit_id] = u.unit_name; });
  V.forEach(function(r) { varName[r.variety_id] = r.variety_name; });

  // ---- populate selectors ----
  function uniq(a) {
    return Array.from(new Set(a.filter(function(v) { return v !== null && v !== ''; }))).sort();
  }
  uniq(V.map(function(r) { return r.crop; })).forEach(function(c) { $('crop').add(new Option(c, c)); });
  $('micro').add(new Option('All', 'All'));
  uniq(V.map(function(r) { return r.micronutrient; })).forEach(function(m) { $('micro').add(new Option(m, m)); });

  // ---- data helpers ----
  function filtered() {
    var crop = $('crop').value, m = $('micro').value;
    return V.filter(function(r) { return r.crop === crop && (m === 'All' || r.micronutrient === m); });
  }
  function counts(rows) {
    var sets = {}, out = {};
    rows.forEach(function(r) { (sets[r.unit_id] = sets[r.unit_id] || new Set()).add(r.variety_id); });
    for (var k in sets) out[k] = sets[k].size;
    return out;
  }
  function colour(n, max) {
    if (!n) return ZERO;
    var i = Math.floor(n / max * PAL.length - 1e-9);
    return PAL[Math.max(0, Math.min(PAL.length - 1, i))];
  }

  // ---- legend ----
  var legend = L.control({position: 'bottomright'});
  legend.onAdd = function() { this._div = L.DomUtil.create('div', 'legend'); return this._div; };
  legend.addTo(map);
  function updateLegend(max) {
    legend._div.innerHTML =
      '<b>Varieties</b><br>' +
      '<span style="display:inline-block;width:12px;height:12px;background:' + ZERO + ';vertical-align:middle"></span> 0' +
      '<div class="bar" style="background:linear-gradient(to right,' + PAL.join(',') + ')"></div>' +
      '<div class="lbl"><span>1</span><span>' + max + '</span></div>';
  }

  // ---- map ----
  function refreshMap() {
    var c = counts(filtered()), max = 1;
    for (var k in c) max = Math.max(max, c[k]);
    U.forEach(function(u) {
      var l = layers[u.unit_id]; if (!l) return;
      var n = c[u.unit_id] || 0;
      var sel = selected !== null && String(selected) === String(u.unit_id);
      l.setStyle({fillColor: colour(n, max), fillOpacity: 0.8,
                  color: sel ? 'red' : 'white', weight: sel ? 4 : 1});
      if (sel) l.bringToFront();
      l.unbindTooltip();
      l.bindTooltip(esc(u.unit_name) + ': ' + n + ' varieties', {sticky: true});
    });
    updateLegend(max);
  }

  U.forEach(function(u) {
    var l = map.layerManager.getLayer('shape', u.unit_id);
    if (!l) return;
    layers[u.unit_id] = l;
    l.on('click', function() {
      selected = (selected !== null && String(selected) === String(u.unit_id)) ? null : u.unit_id;
      update();
    });
  });

  // ---- table ----
  function tableRows() {
    var rows = filtered();
    if (selected !== null) rows = rows.filter(function(r) { return String(r.unit_id) === String(selected); });
    var g = {};
    rows.forEach(function(r) {
      var o = g[r.variety_id] = g[r.variety_id] ||
        {variety_id: r.variety_id, variety_name: r.variety_name, crop: r.crop,
         release_year: r.release_year, micros: new Set(), units: new Set()};
      if (o.release_year == null) o.release_year = r.release_year;   // first non-blank year
      if (r.micronutrient) o.micros.add(r.micronutrient);
      o.units.add(r.unit_id);
    });
    var arr = Object.keys(g).map(function(k) {
      var o = g[k];
      return {variety_id: o.variety_id, variety_name: o.variety_name, crop: o.crop,
              release_year: o.release_year,
              micronutrients: Array.from(o.micros).sort().join(', '), n_units: o.units.size};
    });
    var q = $('search').value.toLowerCase();
    if (q) arr = arr.filter(function(r) {
      return ((r.variety_name || '') + ' ' + r.micronutrients).toLowerCase().indexOf(q) >= 0;
    });
    arr.sort(function(a, b) {
      var x = a[sortCol], y = b[sortCol];
      return (x > y ? 1 : x < y ? -1 : 0) * sortDir;
    });
    return arr;
  }

  function renderTable() {
    var cols = [['variety_name', 'Variety'], ['crop', 'Crop'], ['release_year', 'Release year'],
                ['micronutrients', 'Micronutrients'], ['n_units', 'Units']];
    var h = '<table><thead><tr>' + cols.map(function(c) {
      var arrow = sortCol === c[0] ? (sortDir > 0 ? ' ▲' : ' ▼') : '';
      return '<th data-col="' + c[0] + '">' + c[1] + arrow + '</th>';
    }).join('') + '</tr></thead><tbody>';
    var rows = tableRows();
    rows.forEach(function(r) {
      h += '<tr data-id="' + esc(r.variety_id) + '"' +
           (String(r.variety_id) === String(selectedVariety) ? ' class="sel"' : '') + '>' +
           '<td>' + esc(r.variety_name == null ? '' : r.variety_name) + '</td>' +
           '<td>' + esc(r.crop) + '</td>' +
           '<td>' + esc(r.release_year == null ? '' : r.release_year) + '</td>' +
           '<td>' + esc(r.micronutrients) + '</td><td>' + r.n_units + '</td></tr>';
    });
    if (!rows.length) h += '<tr><td colspan="5" class="muted">No varieties match.</td></tr>';
    $('tbl').innerHTML = h + '</tbody></table>';
  }

  $('tbl').addEventListener('click', function(e) {
    var th = e.target.closest('th');
    if (th) {
      var col = th.getAttribute('data-col');
      if (sortCol === col) sortDir = -sortDir; else { sortCol = col; sortDir = 1; }
      renderTable(); return;
    }
    var tr = e.target.closest('tr[data-id]');
    if (tr) { selectedVariety = tr.getAttribute('data-id'); renderTable(); renderDetails(); }
  });

  // ---- details panel ----
  function fmt(v) {
    var s = String(v);
    return /^https?:\/\//.test(s)
      ? '<a href="' + esc(s) + '" target="_blank" rel="noopener">' + esc(s) + '</a>'
      : esc(s);
  }
  function renderDetails() {
    var box = $('details');
    if (selectedVariety === null) {
      box.innerHTML = '<p class="muted">Select a variety in the table to see more information.</p>';
      return;
    }
    var d = DET[selectedVariety];
    var h = '<h4>' + esc(varName[selectedVariety] == null ? selectedVariety : varName[selectedVariety]) + '</h4>';
    if (!d) { box.innerHTML = h + '<p class="muted">No details found for this variety.</p>'; return; }
    h += d.image ? '<img src="' + esc(d.image) + '" alt="">'
                 : '<p class="muted">No image available for this variety.</p>';
    h += '<dl>';
    Object.keys(d.fields).forEach(function(k) {
      var v = d.fields[k];
      if (v !== null && v !== '') h += '<dt>' + esc(k) + '</dt><dd>' + fmt(v) + '</dd>';
    });
    box.innerHTML = h + '</dl>';
  }

  // ---- wiring ----
  function update() {
    $('sel_label').textContent = selected === null ? 'All units' : unitName[selected];
    refreshMap();
    renderTable();
  }
  $('crop').addEventListener('change', update);
  $('micro').addEventListener('change', update);
  $('search').addEventListener('input', renderTable);
  $('clear_sel').addEventListener('click', function() { selected = null; update(); });

  update();
  renderDetails();
}
)--"

# ------------------------------------------------------------------
# 6. BUILD
# ------------------------------------------------------------------
m <- leaflet(units, width = "100%", height = 520) |>
  addProviderTiles(providers$OpenStreetMap.HOT, group = "OSM (HOT)") |> # remove this line for fully offline use
  addPolygons(
    layerId = ~unit_id,
    fillColor = "#d9d9d9",
    fillOpacity = 0.8,
    color = "white",
    weight = 1
  ) |>
  prependContent(controls) |>
  appendContent(bottom) |>
  onRender(js)

saveWidget(
  m,
  "atlas.html",
  selfcontained = TRUE,
  title = "Nigerian Biofortified Crops Catalogue"
)
message(
  "Wrote ",
  normalizePath("atlas.html"),
  " (",
  round(file.size("atlas.html") / 1e6, 1),
  " MB)"
)
