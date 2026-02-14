library(shiny)

options(encoding = "UTF-8")
tryCatch({
  l10n_info()
  for (loc in c("fr_FR.UTF-8", "C.UTF-8", "")) {
    res <- tryCatch(Sys.setlocale("LC_CTYPE", loc), error = function(e) "")
    if (!is.na(res) && nzchar(res)) break
  }
}, error = function(e) invisible(NULL))

DEFAULT_HOURLY_COST_EUR <- 60
WORK_DAYS_PER_YEAR <- 220
WORK_WEEKS_PER_YEAR <- 50

`%||%` <- function(x, y) if (is.null(x)) y else x
`%+%` <- function(a, b) paste0(a, b)

to_utf8 <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.character(x)) return(enc2utf8(x))
  if (is.list(x)) return(lapply(x, to_utf8))
  x
}

fmt_hours <- function(hours) paste0(round(hours), " h/an")
fmt_eur <- function(eur) if (eur >= 1000) paste0(round(eur / 1000), "kEUR/an") else paste0(round(eur), " EUR/an")
money_hours_hint <- function(hours_year, hourly_cost = DEFAULT_HOURLY_COST_EUR) {
  euros <- hours_year * hourly_cost
  sprintf("Ordre de grandeur : ~%s, soit ~%s.", fmt_hours(hours_year), fmt_eur(euros))
}

get_questions <- function() {
  list(
    list(id = "q1", text = "Aujourdâ€™hui, pour piloter vos opÃ©rations au quotidien, vous vous appuyez principalement sur :", choices = c(
      "Principalement Excel",
      "Excel + plusieurs outils dispersÃ©s",
      "Un ERP bien structurÃ©"
    )),
    list(id = "q2", text = "Aujourdâ€™hui, combien de versions dâ€™un mÃªme fichier circulent rÃ©ellement dans votre organisation ?", choices = c(
      "Une seule version claire",
      "2 Ã  3 versions selon les Ã©quipes",
      "HonnÃªtement, difficile Ã  dire"
    )),
    list(id = "q3", text = "Chaque semaine, combien dâ€™heures vos Ã©quipes consacrent-elles Ã  consolider, vÃ©rifier ou recouper des donnÃ©es ?", choices = c(
      "Moins dâ€™1h",
      "1-3h",
      "Plus de 3h"
    )),
    list(id = "q4", text = "Les retards dans vos projets sont gÃ©nÃ©ralement :", choices = c(
      "Visibles immÃ©diatement",
      "IdentifiÃ©s lors dâ€™un point hebdomadaire",
      "DÃ©couverts une fois quâ€™ils ont dÃ©jÃ  un impact"
    )),
    list(id = "q5", text = "Vos indicateurs clÃ©s sont aujourdâ€™hui :", choices = c(
      "Automatiquement mis Ã  jour",
      "Semi-manuellement consolidÃ©s",
      "EntiÃ¨rement mis Ã  jour Ã  la main"
    )),
    list(id = "q6", text = "Si je vous demande maintenant le statut exact dâ€™une deadline critique, vous avez besoin de :", choices = c(
      "Quelques secondes",
      "Quelques minutes",
      "Plus de 30 minutes"
    )),
    list(id = "q7", text = "Est-ce que certaines demandes disparaissent temporairement avant dâ€™Ãªtre traitÃ©es ?", choices = c(
      "Jamais",
      "Rarement",
      "Oui, cela arrive rÃ©guliÃ¨rement"
    )),
    list(id = "q8", text = "Si la personne qui tient le fichier principal sâ€™absente une semaine :", choices = c(
      "Aucun impact",
      "Lâ€™activitÃ© ralentit sensiblement",
      "Lâ€™organisation est rÃ©ellement en difficultÃ©"
    )),
    list(id = "q9", text = "Au cours des 12 derniers mois, avez-vous connu des urgences coÃ»teuses ou des pÃ©nalitÃ©s liÃ©es Ã  un manque de visibilitÃ© ?", choices = c(
      "Non",
      "Une ou deux situations isolÃ©es",
      "Oui, plusieurs situations"
    )),
    list(id = "q10", text = "Avec recul, vos process actuels vous semblent :", choices = c(
      "Robustes",
      "Corrects mais perfectibles",
      "Fragiles"
    ))
  )
}

compute_score <- function(answers) {
  points <- unlist(answers, use.names = TRUE)
  points[is.na(points)] <- 0L
  list(
    total = sum(points),
    sub_temps = sum(points[c("q2", "q3", "q6")]),
    sub_visibilite = sum(points[c("q4", "q5")]),
    sub_deadline = sum(points[c("q7")]),
    sub_finance = sum(points[c("q9")]),
    sub_dependance = sum(points[c("q8")]),
    sub_perception = sum(points[c("q10")])
  )
}

score_band <- function(score_total) {
  if (score_total <= 8) {
    list(label = "Process maÃ®trisÃ©s", color = "#2ea44f", emoji = "ðŸŸ¢")
  } else if (score_total <= 14) {
    list(label = "Sous tension", color = "#d17f00", emoji = "ðŸŸ ")
  } else {
    list(label = "Ã€ risque structurel", color = "#c5283d", emoji = "ðŸ”´")
  }
}

select_top3_leaks <- function(answers) {
  p <- unlist(answers, use.names = TRUE)
  p[is.na(p)] <- 0L
  leak_scores <- c(
    "Temps perdu en consolidation" = sum(p[c("q2", "q3", "q6")]),
    "DÃ©tection tardive des retards" = sum(p[c("q4", "q7")]),
    "DÃ©pendance critique Ã  une personne" = sum(p[c("q8")]),
    "Demandes qui se perdent" = sum(p[c("q7")]),
    "Risque financier (pÃ©nalitÃ©s / urgences)" = sum(p[c("q9")]),
    "Indicateurs non fiabilisÃ©s" = sum(p[c("q5")])
  )
  ord <- order(-leak_scores, seq_along(leak_scores), method = "radix")
  names(leak_scores)[ord][1:3]
}

safe_write_csv <- function(record, filename = "diagnostics.csv") {
  data_dir <- file.path(getwd(), "data")
  if (!dir.exists(data_dir)) {
    ok <- tryCatch(dir.create(data_dir, recursive = TRUE), error = function(e) FALSE)
    if (!isTRUE(ok) && !dir.exists(data_dir)) {
      data_dir <- file.path(tempdir(), "chatbot_linkedin_data")
      dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
      warning("Data directory in app path is not writable. Falling back to tempdir().")
    }
  }

  out_file <- file.path(data_dir, filename)
  tryCatch({
    if (!file.exists(out_file)) {
      write.csv(record, out_file, row.names = FALSE, fileEncoding = "UTF-8")
    } else {
      write.table(record, out_file, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE, fileEncoding = "UTF-8")
    }
    TRUE
  }, error = function(e) {
    fallback_dir <- file.path(tempdir(), "chatbot_linkedin_data")
    dir.create(fallback_dir, recursive = TRUE, showWarnings = FALSE)
    fallback_file <- file.path(fallback_dir, filename)
    warning(sprintf("Write failed in app data dir (%s). Fallback to tempdir().", e$message))
    tryCatch({
      if (!file.exists(fallback_file)) {
        write.csv(record, fallback_file, row.names = FALSE, fileEncoding = "UTF-8")
      } else {
        write.table(record, fallback_file, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE, fileEncoding = "UTF-8")
      }
      TRUE
    }, error = function(e2) {
      warning(sprintf("Write failed in fallback tempdir too (%s).", e2$message))
      FALSE
    })
  })
}

email_valid <- function(x) grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", x)

answer_impact_hint <- function(q_id, answer_point) {
  if (q_id == "q1") {
    if (answer_point == 0L) return("Excel est un outil puissant - surtout entre de bonnes mains.\nMais dans beaucoup d'industries, il finit par devenir le centre nÃ©vralgique... sans offrir la visibilitÃ© d'un vrai systÃ¨me de pilotage.")
    if (answer_point == 1L) return("Multiplier les outils permet souvent d'aller vite au dÃ©but.\nMais sans intÃ©gration claire, cela crÃ©e progressivement des zones floues oÃ¹ chacun travaille avec sa propre lecture de la rÃ©alitÃ©.")
  }
  if (q_id == "q2") {
    if (answer_point == 1L) return("Lorsque plusieurs versions coexistent, les dÃ©cisions prennent mÃ©caniquement plus de temps.\nOn passe de l'analyse... Ã  la vÃ©rification.")
    if (answer_point == 2L) return("Ne pas savoir quelle est la bonne version n'est pas un manque de rigueur.\nC'est souvent le signe qu'un process a grandi plus vite que sa structure.")
  }
  if (q_id == "q3") {
    if (answer_point == 1L) return("3h par semaine reprÃ©sentent environ 150h par an.\nÃ€ l'Ã©chelle d'un responsable ou d'une assistante, cela peut facilement reprÃ©senter 8 000 Ã  10 000EUR immobilisÃ©s... sans crÃ©ation de valeur directe.")
    if (answer_point == 2L) return("Au-delÃ  de 3h par semaine, on dÃ©passe souvent 200 Ã  300 heures par an.\nCe sont plusieurs semaines de travail consacrÃ©es non pas Ã  piloter, mais Ã  vÃ©rifier.")
  }
  if (q_id == "q4") {
    if (answer_point == 1L) return("Un point hebdomadaire est structurant.\nMais une semaine peut suffire Ã  transformer un lÃ©ger dÃ©calage en situation sous tension.")
    if (answer_point == 2L) return("DÃ©couvrir un retard aprÃ¨s coup n'est pas une erreur individuelle.\nC'est souvent le signe qu'il manque un systÃ¨me d'alerte en amont.")
  }
  if (q_id == "q5") {
    if (answer_point == 1L) return("Chaque manipulation manuelle est une charge invisible.\nElle mobilise de l'attention, crÃ©e un risque d'erreur... et dÃ©tourne du pilotage stratÃ©gique.")
    if (answer_point == 2L) return("30 minutes par jour de ressaisie reprÃ©sentent environ 110h par an.\nÃ€ l'Ã©chelle d'une Ã©quipe, cela peut rapidement dÃ©passer 10 000EUR par an en temps mobilisÃ©.")
  }
  if (q_id == "q6") {
    if (answer_point == 1L) return("Quelques minutes semblent anodines.\nMais multipliÃ©es par plusieurs vÃ©rifications quotidiennes, cela reprÃ©sente des dizaines d'heures par an consacrÃ©es uniquement Ã  chercher l'information.")
    if (answer_point == 2L) return("Lorsque vÃ©rifier une Ã©chÃ©ance prend 30 minutes,\ncela signifie gÃ©nÃ©ralement que l'information existe... mais n'est pas structurÃ©e pour dÃ©cider rapidement.")
  }
  if (q_id == "q7") {
    if (answer_point == 1L) return("MÃªme rarement, une demande oubliÃ©e peut crÃ©er une chaÃ®ne d'ajustements imprÃ©vus.\nEt souvent, c'est l'urgence qui rÃ©vÃ¨le la faiblesse du process.")
    if (answer_point == 2L) return("Quand les demandes se perdent, le problÃ¨me n'est pas humain.\nC'est souvent l'absence d'un systÃ¨me clair de suivi et de priorisation.")
  }
  if (q_id == "q8") {
    if (answer_point == 1L) return("Un ralentissement temporaire est courant.\nMais lorsqu'il dÃ©pend d'une seule personne, le risque organisationnel devient structurel.")
    if (answer_point == 2L) return("Lorsque la connaissance repose sur une seule tÃªte,\nla continuitÃ© opÃ©rationnelle devient fragile - mÃªme avec des Ã©quipes compÃ©tentes.")
  }
  if (q_id == "q9") {
    if (answer_point == 1L) return("Une seule urgence peut reprÃ©senter plusieurs milliers d'euros.\nMais surtout, elle mobilise l'Ã©nergie des Ã©quipes en mode rÃ©action.")
    if (answer_point == 2L) return("Lorsque les urgences deviennent rÃ©pÃ©titives,\ncela indique souvent que le pilotage est plus rÃ©actif que prÃ©dictif.")
  }
  if (q_id == "q10") {
    if (answer_point == 1L) return("C'est souvent Ã  ce stade que les fuites invisibles commencent Ã  s'installer.\nRien de dramatique... mais un potentiel d'optimisation rÃ©el.")
    if (answer_point == 2L) return("Un process fragile ne coÃ»te pas toujours cher immÃ©diatement.\nMais il finit presque toujours par coÃ»ter cher au mauvais moment.")
  }
  ""
}

impact_text <- function(band_label) {
  if (band_label == "Process maÃ®trisÃ©s") {
    return("Votre organisation paraÃ®t saine, avec des marges de progression sur la circulation d'information et la fiabilitÃ© des routines.")
  }
  if (band_label == "Sous tension") {
    return("Le diagnostic montre des points de friction qui pÃ¨sent sur la fluiditÃ© opÃ©rationnelle : temps de vÃ©rification, retards visibles trop tard et dÃ©pendances humaines.")
  }
  "MÃªme 1 heure par jour consacrÃ©e Ã  des tÃ¢ches de consolidation ou de recherche d'information reprÃ©sente plus de 200 heures par an. Ã€ 60EUR de l'heure, cela peut facilement dÃ©passer 12 000EUR immobilisÃ©s - sans amÃ©lioration directe de la performance. La bonne nouvelle : ces heures sont souvent rÃ©cupÃ©rables avec une meilleure structuration."
}

score_order_line <- function(score_total) {
  if (score_total <= 8) return("Ne serait-ce que 15 min/jour de ressaisie, c'est dÃ©jÃ  souvent plusieurs milliers d'euros par an.")
  if (score_total <= 14) return("Avec 30 min/jour de ressaisie, on parle souvent de plus de 10k EUR/an en ordre de grandeur.")
  "Avec 1h/jour perdue, l'ordre de grandeur est souvent entre 10k et 15k EUR/an, hors pÃ©nalitÃ©s et urgences."
}

send_admin_email <- function(payload) {
  host <- Sys.getenv("SMTP_HOST", "")
  port <- Sys.getenv("SMTP_PORT", "")
  user <- Sys.getenv("SMTP_USER", "")
  pass <- Sys.getenv("SMTP_PASS", "")
  from <- Sys.getenv("SMTP_FROM", "")
  to <- Sys.getenv("SMTP_TO", "")

  if (any(!nzchar(c(host, port, user, pass, from, to)))) {
    warning("SMTP non configurÃ© (SMTP_HOST/PORT/USER/PASS/FROM/TO). Email non envoyÃ©.")
    return(FALSE)
  }
  if (!requireNamespace("blastula", quietly = TRUE)) {
    warning("Package blastula indisponible. Email non envoyÃ©.")
    return(FALSE)
  }

  body_lines <- c(
    "<h3>Nouveau diagnostic chatbot</h3>",
    sprintf("<p><strong>Timestamp UTC:</strong> %s</p>", payload$timestamp_utc),
    sprintf("<p><strong>Nom:</strong> %s %s</p>", payload$prenom, payload$nom),
    sprintf("<p><strong>Entreprise:</strong> %s</p>", payload$entreprise),
    sprintf("<p><strong>Email:</strong> %s</p>", payload$email),
    sprintf("<p><strong>Score:</strong> %s (%s)</p>", payload$score_total, payload$band),
    sprintf("<p><strong>Top 3 fuites:</strong> %s</p>", paste(payload$top3, collapse = " | ")),
    sprintf("<p><strong>RÃ©ponses:</strong> %s</p>", paste(sprintf("Q%s=%s", 1:10, payload$answers), collapse = ", "))
  )

  email <- blastula::compose_email(body = blastula::HTML(paste(body_lines, collapse = "\n")))

  tryCatch({
    blastula::smtp_send(
      email = email,
      from = from,
      to = to,
      subject = sprintf("Nouveau diagnostic - %s", payload$entreprise %||% "Sans entreprise"),
      credentials = blastula::creds(
        host = host,
        port = as.integer(port),
        user = user,
        pass = pass,
        use_ssl = TRUE
      )
    )
    TRUE
  }, error = function(e) {
    warning(sprintf("Envoi email admin Ã©chouÃ©: %s", e$message))
    FALSE
  })
}

ui <- fluidPage(
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "stylesheet", type = "text/css", href = "chat.css"),
    tags$script(src = "progress.js"),
    tags$script(src = "chat.js")
  ),
  div(class = "app-wrap",
      div(class = "chat-shell",
          div(class = "title-block", h1("Faites une analyse de vos process actuels")),
          div(class = "chat-card",
              div(class = "chat-header",
                  div(class = "chat-label", "Assistant diagnostic"),
                  div(class = "header-right",
                      div(class = "progress-pct", id = "progress_pct", "0%"),
                      tags$button(id = "restart_btn", class = "restart-btn", type = "button", "Recommencer")
                  )
              ),
              div(class = "progress-wrap", div(class = "progress-track", div(class = "progress-fill", id = "progress_fill"))),
              div(id = "chat_window", class = "chat-window"),
              div(id = "chat_input", class = "chat-input")
          )
      )
  )
)

server <- function(input, output, session) {
  questions <- get_questions()
  lead_fields <- c("prenom", "nom", "entreprise", "email")

  stage <- reactiveVal("questions")
  q_index <- reactiveVal(0L)
  lead_index <- reactiveVal(0L)
  answers <- reactiveVal(setNames(as.list(rep(NA_integer_, 10)), paste0("q", 1:10)))
  lead_data <- reactiveVal(list(prenom = "", nom = "", entreprise = "", email = ""))
  pending_record <- reactiveVal(NULL)
  session_id <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-", gsub("[^A-Za-z0-9]", "", session$token))

  send_event <- function(type, ...) {
    session$sendCustomMessage("chat_event", to_utf8(c(list(type = type), list(...))))
  }

  set_progress <- function(value) {
    pct <- max(0, min(100, as.numeric(value)))
    session$sendCustomMessage("progress_update", list(pct = pct))
  }

  push_progress <- function(step, idx = NULL) {
    if (identical(step, "reset")) return(set_progress(0))
    if (identical(step, "question_answered")) return(set_progress((as.numeric(idx) / length(questions)) * 75))
    if (identical(step, "lead_validated")) return(set_progress(75 + (as.numeric(idx) / length(lead_fields)) * 15))
    if (identical(step, "analysis")) return(set_progress(90))
    if (identical(step, "result_shown")) return(set_progress(100))
  }

  choice_input <- function(options, show_optout = FALSE) {
    list(
      mode = "choices",
      payload = list(
        options = lapply(options, function(x) list(value = x$value, label = x$label)),
        optout = if (show_optout) list(value = "optout_now", label = "Pas maintenant") else NULL
      )
    )
  }

  text_input <- function(field, prompt, placeholder = "Votre rÃ©ponse") {
    list(mode = "text", payload = list(field = field, prompt = prompt, placeholder = placeholder))
  }

  submit_input <- function() {
    list(mode = "submit", payload = list(button = "Voir mon diagnostic", legal = "En soumettant ces informations, vous acceptez d'Ãªtre recontactÃ© au sujet de votre diagnostic."))
  }

  ask_question <- function(idx) {
    q <- questions[[idx]]
    opts <- lapply(q$choices, function(label) list(value = label, label = label))
    send_event("bot", text = q$text, typing = TRUE, typingMinMs = 500, typingMaxMs = 900, afterInput = choice_input(opts, show_optout = TRUE))
  }

  ask_next_lead_field <- function() {
    i <- lead_index()
    if (i > length(lead_fields)) {
      stage("lead_review")
      send_event("bot", text = "Parfait, jâ€™ai lâ€™essentiel. Vous pouvez afficher votre diagnostic personnalisÃ©.", typing = TRUE, typingMinMs = 500, typingMaxMs = 900, afterInput = submit_input())
      return(invisible(NULL))
    }

    field <- lead_fields[[i]]
    if (field == "prenom") {
      send_event("bot", text = "Quel est votre prÃ©nom ?", typing = TRUE, typingMinMs = 500, typingMaxMs = 900, afterInput = text_input("prenom", "RÃ©ponse rapide", "PrÃ©nom"))
    } else if (field == "nom") {
      send_event("bot", text = "Votre nom ?", typing = TRUE, typingMinMs = 500, typingMaxMs = 900, afterInput = text_input("nom", "RÃ©ponse rapide", "Nom"))
    } else if (field == "entreprise") {
      send_event("bot", text = "Votre entreprise ?", typing = TRUE, typingMinMs = 500, typingMaxMs = 900, afterInput = text_input("entreprise", "RÃ©ponse rapide", "Entreprise"))
    } else if (field == "email") {
      send_event("bot", text = "Votre email professionnel ?", typing = TRUE, typingMinMs = 500, typingMaxMs = 900, afterInput = text_input("email", "Pour vous envoyer le diagnostic", "vous@entreprise.com"))
    }
  }

  start_flow <- function() {
    stage("questions")
    q_index(0L)
    lead_index(0L)
    answers(setNames(as.list(rep(NA_integer_, 10)), paste0("q", 1:10)))
    lead_data(list(prenom = "", nom = "", entreprise = "", email = ""))
    pending_record(NULL)

    send_event("reset")
    push_progress("reset")
    send_event("bot", text = "Bonjour ðŸ‘‹\nEn 2 minutes, je vais vous aider Ã  mettre des mots (et des chiffres) sur ce que vous ressentez peut-Ãªtre dÃ©jÃ  dans votre organisation.", typing = FALSE, afterInput = list(mode = "none", payload = list()))
    q_index(1L)
    ask_question(1L)
  }

  finalize_result <- function() {
    a <- answers()
    l <- lead_data()
    sc <- compute_score(a)
    b <- score_band(sc$total)
    top3 <- select_top3_leaks(a)

    push_progress("analysis")
    send_event("bot", text = "Analyse en cours...", typing = FALSE, afterInput = list(mode = "none", payload = list()))
    send_event("card", typingMs = sample(1200:2000, 1), payload = list(entreprise = l$entreprise, band_label = b$label, band_emoji = b$emoji, band_color = b$color, top3 = as.list(top3), impact = impact_text(b$label), order_line = score_order_line(sc$total)), afterInput = list(mode = "end", payload = list()))

    get_a <- function(key) {
      v <- a[[key]]
      if (is.null(v) || length(v) == 0) NA_integer_ else as.integer(v)
    }

    record <- data.frame(
      timestamp_utc = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      session_id = session_id,
      q1 = get_a("q1"), q2 = get_a("q2"), q3 = get_a("q3"), q4 = get_a("q4"), q5 = get_a("q5"),
      q6 = get_a("q6"), q7 = get_a("q7"), q8 = get_a("q8"), q9 = get_a("q9"), q10 = get_a("q10"),
      q11 = NA_integer_, q12 = NA_integer_,
      score_total = sc$total, band = b$label,
      subscore_temps = sc$sub_temps, subscore_visibilite = sc$sub_visibilite,
      subscore_deadline = sc$sub_deadline, subscore_finance = sc$sub_finance,
      subscore_dependance = sc$sub_dependance,
      prenom = l$prenom, nom = l$nom, entreprise = l$entreprise, email = l$email,
      taille_entreprise = "", fonction = "", consent = TRUE,
      stringsAsFactors = FALSE
    )

    pending_record(record)
    stage("result_pending")
  }

  observeEvent(TRUE, start_flow(), once = TRUE)

  observeEvent(input$chat_control, {
    req(is.list(input$chat_control))
    if (identical(input$chat_control$type, "restart")) start_flow()
  })

  observeEvent(input$chat_input, {
    req(is.list(input$chat_input))
    msg <- input$chat_input
    msg_type <- msg$type %||% ""

    if (identical(msg_type, "choice") && identical(msg$value, "optout_now")) {
      stage("ended")
      send_event("bot", text = "Aucun souci. Revenez quand vous voulez, je serai lÃ  pour vous aider.", typing = TRUE, typingMinMs = 500, typingMaxMs = 900, afterInput = list(mode = "end", payload = list()))
      return(invisible(NULL))
    }

    if (identical(stage(), "questions") && identical(msg_type, "choice")) {
      qi <- q_index()
      if (qi < 1 || qi > length(questions)) return(invisible(NULL))
      q <- questions[[qi]]
      ans_idx <- match(msg$value, q$choices)
      if (is.na(ans_idx)) return(invisible(NULL))

      a <- answers()
      answer_point <- as.integer(ans_idx - 1L)
      a[[q$id]] <- answer_point
      answers(a)
      push_progress("question_answered", qi)

      hint <- answer_impact_hint(q$id, answer_point)
      if (nzchar(hint)) send_event("bot", text = hint, typing = TRUE, typingMinMs = 500, typingMaxMs = 900)

      if (qi < length(questions)) {
        q_index(qi + 1L)
        ask_question(qi + 1L)
      } else {
        stage("lead_intro")
        lead_index(1L)
        send_event("bot", text = "Merci.\nJâ€™ai identifiÃ© plusieurs signaux potentiels. Jâ€™ai besoin de 4 infos pour afficher votre diagnostic personnalisÃ©.", typing = TRUE, typingMinMs = 500, typingMaxMs = 900, afterInput = list(mode = "none", payload = list()))
        ask_next_lead_field()
      }
      return(invisible(NULL))
    }

    if ((identical(stage(), "lead_intro") || identical(stage(), "lead_capture")) && identical(msg_type, "text")) {
      stage("lead_capture")
      i <- lead_index()
      if (i < 1 || i > length(lead_fields)) return(invisible(NULL))

      field <- lead_fields[[i]]
      value <- trimws(msg$value %||% "")
      if (!nzchar(value)) {
        send_event("input_error", message = "RÃ©ponse requise.")
        return(invisible(NULL))
      }
      if (identical(field, "email") && !email_valid(value)) {
        send_event("input_error", message = "Email invalide.")
        return(invisible(NULL))
      }

      ld <- lead_data()
      ld[[field]] <- value
      lead_data(ld)
      push_progress("lead_validated", i)

      lead_index(i + 1L)
      ask_next_lead_field()
      return(invisible(NULL))
    }

    if (identical(stage(), "lead_review") && identical(msg_type, "lead_submit")) {
      finalize_result()
      return(invisible(NULL))
    }
  })

  observeEvent(input$open_slot, {
    showModal(modalDialog(
      title = "Proposer un creneau",
      "Merci. Cette version ne connecte aucun agenda externe. Vous pouvez ajouter ici un formulaire de prise de rendez-vous interne.",
      easyClose = TRUE,
      footer = modalButton("Fermer")
    ))
  })

  observeEvent(input$result_rendered, {
    req(identical(stage(), "result_pending"))
    rec <- pending_record()
    if (!is.null(rec)) {
      safe_write_csv(rec)
      a <- answers(); sc <- compute_score(a); b <- score_band(sc$total); top3 <- select_top3_leaks(a)
      send_admin_email(list(
        timestamp_utc = rec$timestamp_utc[[1]], prenom = rec$prenom[[1]], nom = rec$nom[[1]],
        entreprise = rec$entreprise[[1]], email = rec$email[[1]],
        score_total = sc$total, band = b$label, top3 = top3,
        answers = unname(unlist(a[paste0("q", 1:10)]))
      ))
      pending_record(NULL)
    }
    push_progress("result_shown")
    stage("result")
  })
}

shinyApp(ui, server)
