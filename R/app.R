# Shiny version of the explorer.   shiny::runApp("R")
# Reads web/data/wine.json (built by python/build_web.py) — same data as the static site.
if (!isTRUE(l10n_info()[["UTF-8"]])) invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))  # Turkish labels
library(shiny)
library(bslib)
library(jsonlite)
library(httr2)

root <- if (file.exists("web/data/wine.json")) "." else ".."
d <- fromJSON(file.path(root, "web", "data", "wine.json"), simplifyVector = FALSE)
w <- d$wines
desc <- d$desc
nm <- unlist(d$names)
lab <- function(x, tr) if (tr) x$tr else x$en
n <- length(w$l); m <- length(desc)
sens <- vapply(desc, function(x) x$s == 1, logical(1))
idf <- vapply(desc, function(x) x$idf, numeric(1))
M <- matrix(0, n, m)
for (i in seq_len(n)) M[i, unlist(w$at[[i]]) + 1] <- 1
V <- sweep(M, 2, ifelse(sens, idf, 0), `*`)
V <- V / (sqrt(rowSums(V^2)) + 1e-9)
L <- unlist(w$l); Y <- unlist(w$y); S <- unlist(w$s); ST <- unlist(w$st)
prov <- do.call(rbind, lapply(d$provinces, function(p) data.frame(name = p[[1]], lat = p[[2]], lon = p[[3]])))

haversine <- function(a, b) {
  r <- pi / 180
  h <- sin((b[1] - a[1]) * r / 2)^2 + cos(a[1] * r) * cos(b[1] * r) * sin((b[2] - a[2]) * r / 2)^2
  12742 * asin(sqrt(h))
}

shops <- function(lat, lon) {
  for (radius in c(3000, 10000, 30000)) {
    q <- sprintf('[out:json][timeout:20];(nwr["shop"~"^(wine|alcohol|beverages)$"](around:%d,%f,%f);nwr["craft"="winery"](around:%d,%f,%f););out center 80;',
                 radius, lat, lon, radius, lat, lon)
    js <- request("https://overpass-api.de/api/interpreter") |> req_body_form(data = q) |>
      req_timeout(30) |> req_perform() |> resp_body_json()
    rows <- lapply(js$elements, function(e) {
      la <- if (!is.null(e$lat)) e$lat else e$center$lat
      lo <- if (!is.null(e$lon)) e$lon else e$center$lon
      data.frame(km = round(haversine(c(lat, lon), c(la, lo)), 1),
                 name = if (is.null(e$tags$name)) "—" else e$tags$name,
                 type = if (is.null(e$tags$shop)) "winery" else e$tags$shop,
                 osm = sprintf("https://www.openstreetmap.org/%s/%s", e$type, e$id))
    })
    out <- do.call(rbind, rows)
    if ((!is.null(out) && nrow(out) >= 5) || radius == 30000) return(if (is.null(out)) out else head(out[order(out$km), ], 12))
  }
}

ui <- page_navbar(
  title = "Bordeaux → Türkiye",
  theme = bs_theme(version = 5, bg = "#f6f3ee", fg = "#1b1416", primary = "#7a1f33", base_font = "IBM Plex Sans, system-ui"),
  sidebar = sidebar(radioButtons("lang", NULL, setNames(c("tr", "en"), c("Türkçe", "English")), inline = TRUE),
                    selectizeInput("label", "Şarap / Wine", choices = NULL),
                    uiOutput("vintages")),
  nav_panel("Şarap / Wine", uiOutput("card")),
  nav_panel("Tat / Taste",
            selectizeInput("taste", NULL, multiple = TRUE, options = list(maxItems = 8),
                           choices = setNames(which(sens & vapply(desc, function(x) !is.null(x$c) && x$n >= 60, logical(1))),
                                              vapply(desc[sens & vapply(desc, function(x) !is.null(x$c) && x$n >= 60, logical(1))], function(x) x$en, ""))),
            uiOutput("taste_out")),
  nav_panel("Nereden? / Where?",
            selectInput("prov", "İl / Province", choices = setNames(seq_len(nrow(prov)), prov$name), selected = 34),
            actionButton("go", "Ara / Search", class = "btn-primary"),
            tableOutput("shops"),
            p(class = "text-muted", "22.00–06.00 arası perakende satış ve 18 yaş altına satış yasaktır (4250 s. Kanun md. 6). © OpenStreetMap contributors (ODbL)"))
)

server <- function(input, output, session) {
  updateSelectizeInput(session, "label", choices = setNames(seq_along(nm), nm),
                       selected = match("Château Margaux Margaux", nm), server = TRUE)
  tr <- reactive(input$lang == "tr")
  rows <- reactive({ req(input$label); r <- which(L == as.integer(input$label) - 1); r[order(-Y[r])] })
  output$vintages <- renderUI(radioButtons("wine", "Rekolte / Vintage", choices = setNames(rows(), paste(Y[rows()], "·", S[rows()]))))

  output$card <- renderUI({
    req(input$wine); i <- as.integer(input$wine)
    rule <- d$rules[[w$ru[[i]] + 1]]
    ch <- Filter(function(c) c$id %in% strsplit(rule$cheese_ids, ";")[[1]], d$cheeses)
    sims <- as.vector(V %*% V[i, ])
    cand <- order(-sims); cand <- cand[L[cand] != L[i] & ST[cand] == ST[i]]
    cand <- cand[!duplicated(L[cand])][1:5]
    eq <- lapply(w$eq[[i]], function(e) d$turkish[[e[[1]] + 1]])
    tagList(
      h3(nm[L[i] + 1], " · ", Y[i]),
      layout_columns(value_box("Puan / Score", S[i]), value_box("Model (unseen)", sprintf("%.0f%%", 100 * w$pr[[i]])),
                     value_box("$", if (is.null(w$p[[i]])) "—" else w$p[[i]])),
      p(paste(vapply(unlist(w$at[[i]]) + 1, function(a) lab(desc[[a]], tr()), ""), collapse = " · ")),
      h5(if (tr()) "Tadı en çok benzeyenler" else "Most similar"),
      tags$ul(lapply(cand, function(j) tags$li(sprintf("%s · %d · %d (%.0f%%)", nm[L[j] + 1], Y[j], S[j], 100 * sims[j])))),
      h5(if (tr()) "Yanına peynir" else "Cheese"), p(if (tr()) rule$rationale_tr else rule$rationale_en),
      p(paste(vapply(ch, function(c) if (tr()) c$name_tr else c$name_en, ""), collapse = ", ")),
      h5(if (tr()) "Türkiye'deki muadilleri" else "Turkish equivalents"),
      tags$ul(lapply(eq, function(t) tags$li(sprintf("%s — %s · %s · %s", t$producer, t$wine, gsub(";", ", ", t$grapes), t$location_tr))))
    )
  })

  output$taste_out <- renderUI({
    req(input$taste); k <- as.integer(input$taste)
    z <- d$model$intercept + sum(vapply(desc[k], function(x) x$c, numeric(1)))
    q <- numeric(m); q[k] <- idf[k]; sims <- as.vector(V %*% (q / sqrt(sum(q^2))))
    top <- order(-sims)[1:10]
    tagList(h4(sprintf("90+: %.0f%%", 100 * plogis(z))),
            tags$ol(lapply(top, function(j) tags$li(sprintf("%s · %d · %d", nm[L[j] + 1], Y[j], S[j])))))
  })

  output$shops <- renderTable({
    input$go; isolate({ p <- prov[as.integer(input$prov), ]; req(input$go > 0); shops(p$lat, p$lon) })
  })
}

shinyApp(ui, server)
