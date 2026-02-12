# =============================================================
# app.R
# PRJ-00852 | تحديث تنظيمات واشتراطات البناء – مكة المكرمة
# Shapefile: C:/Work_2026/Month_2/SHAPE/PAR.shp
# =============================================================

library(shiny)
library(sf)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(DT)
library(bslib)

# ── دالة مساعدة: خلية بطاقة معلومات ──
field_cell <- function(label, val) {
  v <- ifelse(is.na(val) | trimws(as.character(val)) == "", "—", as.character(val))
  paste0(
    '<div>',
    '<div style="color:#999;font-size:10px;margin-bottom:1px;">', label, '</div>',
    '<div style="font-weight:bold;color:#222;font-size:11px;">', v, '</div>',
    '</div>'
  )
}

# ── تدرج لوني لعدد الطوابق ──
# 1-2: أصفر فاتح | 3-4: أصفر | 5-6: برتقالي | 7-9: أحمر | 10+: أحمر داكن
floor_pal <- colorBin(
  palette  = c("#ffffcc", "#fed976", "#fd8d3c", "#e31a1c", "#800026"),
  domain   = c(1, 20),
  bins     = c(1, 3, 5, 7, 10, 21),
  na.color = "#aaaaaa"
)

# =============================================================
# 1) قراءة الـ Shapefile الحقيقي
# =============================================================
sf_use_s2(FALSE)   # تجاهل مشاكل صلاحية الـ geometry
nc <- st_read("C:/Work_2026/Month_2/SHAPE/PAR.shp", quiet = TRUE)
nc <- st_make_valid(nc)
nc <- st_transform(nc, 4326)

# حقول العمل الأساسية
nc$PARCEL_ID     <- as.character(nc$pkAddressI)
nc$DISTRICT_NAME <- as.character(nc$fkDistrict)
nc$LAND_USE      <- as.character(nc$LandUse)
nc$MAX_FLOORS    <- suppressWarnings(as.numeric(nc$NoofFloor))
nc$AREA_SQM      <- round(suppressWarnings(as.numeric(nc$Shape_Area)), 1)
nc$SURVEY_STATUS <- as.character(nc$SurveyStat)
nc$LAND_TYPE     <- as.character(nc$LandType)

# مراكز المضلعات للماركرز والـ heatmap
pts       <- suppressWarnings(st_centroid(nc))
pts$value <- ifelse(is.na(nc$MAX_FLOORS), 1, nc$MAX_FLOORS)

# =============================================================
# 2) دالة بناء الـ Popup – vectorized (سريعة على أي عدد صفوف)
# =============================================================
make_popup <- function(d) {
  df <- st_drop_geometry(d)   # مرة واحدة بس

  street <- ifelse(is.na(df$StreetArab) | trimws(as.character(df$StreetArab)) == "",
                   "\u2014", as.character(df$StreetArab))
  lu     <- ifelse(is.na(df$LandUse),    "\u2014", as.character(df$LandUse))
  dist   <- ifelse(is.na(df$fkDistrict), "\u2014", as.character(df$fkDistrict))

  paste0(
    '<div dir="rtl" style="font-family:Tahoma,Arial,sans-serif;width:320px;',
    'border-radius:8px;overflow:hidden;box-shadow:0 3px 14px rgba(0,0,0,.35);">',

    # Header
    '<div style="background:linear-gradient(135deg,#1a3a5c,#2471a3);color:#fff;padding:11px 14px;">',
    '<div style="font-size:13px;font-weight:bold;">', street, '</div>',
    '<div style="font-size:11px;opacity:.85;margin-top:2px;">\u0627\u0633\u062a\u062e\u062f\u0627\u0645: ',
    lu, ' &nbsp;\u00B7&nbsp; \u062d\u064a: ', dist, '</div></div>',

    # م1: بيانات التعريف
    '<div style="padding:9px 14px;border-bottom:1px solid #eee;">',
    '<div style="color:#1a3a5c;font-weight:bold;font-size:11px;margin-bottom:5px;',
    'border-bottom:1px solid #aacbe8;padding-bottom:3px;">&#9650; \u0628\u064a\u0627\u0646\u0627\u062a \u0627\u0644\u062a\u0639\u0631\u064a\u0641</div>',
    '<div style="display:grid;grid-template-columns:1fr 1fr;gap:5px 10px;">',
    field_cell("\u0631\u0642\u0645 \u0627\u0644\u0642\u0637\u0639\u0629",    df$pkAddressI),
    field_cell("\u0631\u0642\u0645 \u0627\u0644\u0645\u0628\u0646\u0649",    df$BuildingNo),
    field_cell("\u0627\u0644\u0645\u0633\u0627\u062d\u0629 (\u0645\u00B2)",  round(as.numeric(df$Shape_Area), 1)),
    field_cell("\u0627\u0644\u0631\u0645\u0632 \u0627\u0644\u0628\u0631\u064a\u062f\u064a", df$fkZipCodeI),
    field_cell("\u0631\u0645\u0632 \u0627\u0644\u0645\u062f\u064a\u0646\u0629",  df$fkCityID),
    field_cell("\u0631\u0645\u0632 \u0627\u0644\u0625\u0645\u0627\u0631\u0629",  df$fkEmirateI),
    '</div></div>',

    # م2: بيانات البناء
    '<div style="padding:9px 14px;border-bottom:1px solid #eee;">',
    '<div style="color:#1a7a65;font-weight:bold;font-size:11px;margin-bottom:5px;',
    'border-bottom:1px solid #a8dbd0;padding-bottom:3px;">&#9650; \u0628\u064a\u0627\u0646\u0627\u062a \u0627\u0644\u0628\u0646\u0627\u0621</div>',
    '<div style="display:grid;grid-template-columns:1fr 1fr;gap:5px 10px;">',
    field_cell("\u0639\u062f\u062f \u0627\u0644\u0637\u0648\u0627\u0628\u0642",   df$NoofFloor),
    field_cell("\u0639\u062f\u062f \u0627\u0644\u0648\u062d\u062f\u0627\u062a",   df$NoofUnit),
    field_cell("\u0627\u0633\u062a\u062e\u062f\u0627\u0645 \u0627\u0644\u0623\u0631\u0636", df$LandUse),
    field_cell("\u0646\u0648\u0639 \u0627\u0644\u0623\u0631\u0636",   df$LandType),
    field_cell("\u0627\u0644\u0627\u0634\u062a\u0631\u0627\u0637\u0627\u062a",    df$Restrictio),
    field_cell("\u0627\u0634\u062a\u0631\u0627\u0637\u0627\u062a \u0625\u0636\u0627\u0641\u064a\u0629", df$Restrict_1),
    '</div></div>',

    # م3: بيانات الموقع
    '<div style="padding:9px 14px;border-bottom:1px solid #eee;">',
    '<div style="color:#7d6608;font-weight:bold;font-size:11px;margin-bottom:5px;',
    'border-bottom:1px solid #f0c940;padding-bottom:3px;">&#9650; \u0628\u064a\u0627\u0646\u0627\u062a \u0627\u0644\u0645\u0648\u0642\u0639</div>',
    '<div style="display:grid;grid-template-columns:1fr 1fr;gap:5px 10px;">',
    field_cell("\u062e\u0637 \u0627\u0644\u0637\u0648\u0644",  round(suppressWarnings(as.numeric(df$GeoXCoordi)), 5)),
    field_cell("\u062e\u0637 \u0627\u0644\u0639\u0631\u0636",  round(suppressWarnings(as.numeric(df$GeoYCoordi)), 5)),
    field_cell("X (UTM)", round(suppressWarnings(as.numeric(df$XCoordinat)), 1)),
    field_cell("Y (UTM)", round(suppressWarnings(as.numeric(df$YCoordinat)), 1)),
    '</div></div>',

    # م4: بيانات الإدارة
    '<div style="padding:9px 14px;">',
    '<div style="color:#555;font-weight:bold;font-size:11px;margin-bottom:5px;',
    'border-bottom:1px solid #ccc;padding-bottom:3px;">&#9650; \u0628\u064a\u0627\u0646\u0627\u062a \u0627\u0644\u0625\u062f\u0627\u0631\u0629</div>',
    '<div style="display:grid;grid-template-columns:1fr 1fr;gap:5px 10px;">',
    field_cell("\u062a\u0627\u0631\u064a\u062e \u0627\u0644\u0625\u0646\u0634\u0627\u0621", df$DateCreate),
    field_cell("\u0622\u062e\u0631 \u062a\u0639\u062f\u064a\u0644",   df$DateModifi),
    field_cell("\u0622\u062e\u0631 \u0645\u0633\u062a\u062e\u062f\u0645",  df$LastUser),
    field_cell("\u0645\u0635\u062f\u0631 \u0627\u0644\u0628\u064a\u0627\u0646\u0627\u062a", df$DataSource),
    field_cell("\u062d\u0627\u0644\u0629 \u0627\u0644\u0645\u0633\u062d",  df$SurveyStat),
    field_cell("\u0645\u0644\u0627\u062d\u0638\u0627\u062a",              df$Remark),
    '</div></div>',

    '</div>'
  )
}

# =============================================================
# 3) UI
# =============================================================
ui <- page_sidebar(
  title = "PR \u2014 \u062a\u062d\u062f\u064a",

  sidebar = sidebar(
    selectInput("land_use", "\u0627\u0633\u062a\u062e\u062f\u0627\u0645 \u0627\u0644\u0623\u0631\u0636",
                choices  = c("\u0627\u0644\u0643\u0644",
                             sort(unique(na.omit(as.character(nc$LandUse))))),
                selected = "\u0627\u0644\u0643\u0644"),
    selectInput("land_type", "\u0646\u0648\u0639 \u0627\u0644\u0623\u0631\u0636",
                choices  = c("\u0627\u0644\u0643\u0644",
                             sort(unique(na.omit(as.character(nc$LandType))))),
                selected = "\u0627\u0644\u0643\u0644"),
    selectInput("district", "\u0627\u0644\u062d\u064a",
                choices  = c("\u0627\u0644\u0643\u0644",
                             sort(unique(na.omit(as.character(nc$fkDistrict))))),
                selected = "\u0627\u0644\u0643\u0644"),
    selectInput("survey_status", "\u062d\u0627\u0644\u0629 \u0627\u0644\u0645\u0633\u062d",
                choices  = c("\u0627\u0644\u0643\u0644",
                             sort(unique(na.omit(as.character(nc$SurveyStat))))),
                selected = "\u0627\u0644\u0643\u0644"),
    hr(),
    checkboxInput("heat",    "\u0637\u0628\u0642\u0629 \u0627\u0644\u0643\u062b\u0627\u0641\u0629 (Heatmap)", TRUE),
    checkboxInput("cluster", "\u062a\u062c\u0645\u064a\u0639 \u0627\u0644\u0646\u0642\u0627\u0637",          TRUE)
  ),

  card(full_screen = TRUE, leafletOutput("map", height = 560)),

  layout_columns(
    card(full_screen = TRUE, DTOutput("tbl")),
    card(full_screen = TRUE, plotOutput("hist", height = 280)),
    col_widths = c(7, 5)
  )
)

# =============================================================
# 4) Server
# =============================================================
server <- function(input, output, session) {

  filtered_nc <- reactive({
    d <- nc
    if (input$land_use     != "\u0627\u0644\u0643\u0644") d <- d %>% filter(LAND_USE      == input$land_use)
    if (input$land_type    != "\u0627\u0644\u0643\u0644") d <- d %>% filter(LAND_TYPE     == input$land_type)
    if (input$district     != "\u0627\u0644\u0643\u0644") d <- d %>% filter(DISTRICT_NAME == input$district)
    if (input$survey_status!= "\u0627\u0644\u0643\u0644") d <- d %>% filter(SURVEY_STATUS == input$survey_status)
    d
  })

  filtered_pts <- reactive({
    d <- pts
    if (input$land_use     != "\u0627\u0644\u0643\u0644") d <- d %>% filter(LAND_USE      == input$land_use)
    if (input$land_type    != "\u0627\u0644\u0643\u0644") d <- d %>% filter(LAND_TYPE     == input$land_type)
    if (input$district     != "\u0627\u0644\u0643\u0644") d <- d %>% filter(DISTRICT_NAME == input$district)
    if (input$survey_status!= "\u0627\u0644\u0643\u0644") d <- d %>% filter(SURVEY_STATUS == input$survey_status)
    d
  })

  # الخريطة الأولية
  output$map <- renderLeaflet({
    bbox <- st_bbox(nc)
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addProviderTiles(providers$CartoDB.VoyagerOnlyLabels) %>%
      fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]) %>%
      addDrawToolbar(
        targetGroup      = "drawn",
        polylineOptions  = FALSE, circleOptions = FALSE,
        rectangleOptions = TRUE,
        polygonOptions   = drawPolygonOptions(showArea = TRUE),
        markerOptions    = FALSE,
        editOptions      = editToolbarOptions()
      ) %>%
      addLayersControl(
        overlayGroups = c("parcels", "points", "heat", "drawn"),
        options       = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(
        position  = "bottomright",
        pal       = floor_pal,
        values    = 1:20,
        title     = "\u0639\u062f\u062f \u0627\u0644\u0637\u0648\u0627\u0628\u0642",
        labFormat = labelFormat(suffix = " \u0637\u0627\u0628\u0642"),
        opacity   = 0.85
      )
  })

  # تحديث الطبقات
  observe({
    d_nc  <- filtered_nc()
    d_pts <- filtered_pts()

    proxy <- leafletProxy("map") %>%
      clearGroup("parcels") %>%
      clearGroup("points")  %>%
      clearGroup("heat")

    if (nrow(d_nc) > 0) {
      colors <- floor_pal(d_nc$MAX_FLOORS)

      proxy <- proxy %>%
        addPolygons(
          data        = d_nc,
          fillColor   = colors, fillOpacity = 0.45,
          color       = "#ffffff", weight = 0.8, opacity = 0.9,
          group       = "parcels",
          popup       = make_popup(d_nc),
          highlightOptions = highlightOptions(
            color = "#FFD700", weight = 3,
            fillOpacity = 0.7, bringToFront = TRUE
          )
        )
    }

    if (nrow(d_pts) > 0) {
      popups_pts <- make_popup(d_pts)

      if (isTRUE(input$cluster)) {
        proxy <- proxy %>%
          addMarkers(data = d_pts, group = "points",
                     popup = popups_pts,
                     clusterOptions = markerClusterOptions())
      } else {
        proxy <- proxy %>%
          addCircleMarkers(data = d_pts, group = "points",
                           radius = 4, color = "#1a3a5c",
                           fillOpacity = 0.8, popup = popups_pts)
      }

      if (isTRUE(input$heat)) {
        coords <- st_coordinates(d_pts)
        proxy  <- proxy %>%
          addHeatmap(lng = coords[,1], lat = coords[,2],
                     intensity = d_pts$value, group = "heat",
                     blur = 20, max = 20, minOpacity = 0.3)
      }
    }
  })

  # جدول البيانات
  output$tbl <- renderDT({
    filtered_nc() %>%
      st_drop_geometry() %>%
      select(PARCEL_ID, DISTRICT_NAME, LAND_USE, LAND_TYPE,
             MAX_FLOORS, AREA_SQM, SURVEY_STATUS) %>%
      rename(
        "\u0631\u0642\u0645 \u0627\u0644\u0642\u0637\u0639\u0629"  = PARCEL_ID,
        "\u0627\u0644\u062d\u064a"                                  = DISTRICT_NAME,
        "\u0627\u0633\u062a\u062e\u062f\u0627\u0645 \u0627\u0644\u0623\u0631\u0636" = LAND_USE,
        "\u0646\u0648\u0639 \u0627\u0644\u0623\u0631\u0636"         = LAND_TYPE,
        "\u0639\u062f\u062f \u0627\u0644\u0637\u0648\u0627\u0628\u0642" = MAX_FLOORS,
        "\u0627\u0644\u0645\u0633\u0627\u062d\u0629 (\u0645\u00B2)" = AREA_SQM,
        "\u062d\u0627\u0644\u0629 \u0627\u0644\u0645\u0633\u062d"   = SURVEY_STATUS
      ) %>%
      datatable(options = list(pageLength = 10, dom = "tip"), rownames = FALSE)
  })

  # رسم بياني: توزيع عدد الطوابق
  output$hist <- renderPlot({
    x <- filtered_nc() %>% st_drop_geometry() %>%
         filter(!is.na(MAX_FLOORS)) %>% pull(MAX_FLOORS)
    if (length(x) == 0) {
      plot.new(); text(0.5, 0.5, "\u0644\u0627 \u062a\u0648\u062c\u062f \u0628\u064a\u0627\u0646\u0627\u062a", cex = 1.2)
      return()
    }
    hist(x,
         main   = "\u062a\u0648\u0632\u064a\u0639 \u0639\u062f\u062f \u0627\u0644\u0637\u0648\u0627\u0628\u0642",
         xlab   = "\u0639\u062f\u062f \u0627\u0644\u0637\u0648\u0627\u0628\u0642",
         ylab   = "\u0639\u062f\u062f \u0627\u0644\u0642\u0637\u0639",
         col    = "#fd8d3c",
         border = "white",
         breaks = 10)
  })
}

shinyApp(ui, server)
