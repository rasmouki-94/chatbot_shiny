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
    list(id = "q1", text = "Aujourd’hui, pour piloter vos opérations au quotidien, vous vous appuyez principalement sur :", choices = c(
      "Principalement Excel",
      "Excel + plusieurs outils dispersés",
      "Un ERP bien structuré"
    )),
    list(id = "q2", text = "Aujourd’hui, combien de versions d’un même fichier circulent réellement dans votre organisation ?", choices = c(
      "Une seule version claire",
      "2 à 3 versions selon les équipes",
      "Honnêtement, difficile à dire"
    )),
    list(id = "q3", text = "Chaque semaine, combien d’heures vos équipes consacrent-elles à consolider, vérifier ou recouper des données ?", choices = c(
      "Moins d’1h",
      "1-3h",
      "Plus de 3h"
    )),
    list(id = "q4", text = "Les retards dans vos projets sont généralement :", choices = c(
      "Visibles immédiatement",
      "Identifiés lors d’un point hebdomadaire",
      "Découverts une fois qu’ils ont déjà un impact"
    )),
    list(id = "q5", text = "Vos indicateurs clés sont aujourd’hui :", choices = c(
      "Automatiquement mis à jour",
      "Semi-manuellement consolidés",
      "Entièrement mis à jour à la main"
    )),
    list(id = "q6", text = "Si je vous demande maintenant le statut exact d’une deadline critique, vous avez besoin de :", choices = c(
      "Quelques secondes",
      "Quelques minutes",
      "Plus de 30 minutes"
    )),
    list(id = "q7", text = "Est-ce que certaines demandes disparaissent temporairement avant d’être traitées ?", choices = c(
      "Jamais",
      "Rarement",
      "Oui, cela arrive régulièrement"
    )),
    list(id = "q8", text = "Si la personne qui tient le fichier principal s’absente une semaine :", choices = c(
      "Aucun impact",
      "L’activité ralentit sensiblement",
      "L’organisation est réellement en difficulté"
    )),
    list(id = "q9", text = "Au cours des 12 derniers mois, avez-vous connu des urgences coûteuses ou des pénalités liées à un manque de visibilité ?", choices = c(
      "Non",
      "Une ou deux situations isolées",
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
    list(label = "Process maîtrisés", color = "#2ea44f", emoji = "🟢")
  } else if (score_total <= 14) {
    list(label = "Sous tension", color = "#d17f00", emoji = "🟠")
  } else {
    list(label = "À risque structurel", color = "#c5283d", emoji = "🔴")
  }
}

select_top3_leaks <- function(answers) {
  p <- unlist(answers, use.names = TRUE)
  p[is.na(p)] <- 0L
  leak_scores <- c(
    "Temps perdu en consolidation" = sum(p[c("q2", "q3", "q6")]),
    "Détection tardive des retards" = sum(p[c("q4", "q7")]),
    "Dépendance critique à une personne" = sum(p[c("q8")]),
    "Demandes qui se perdent" = sum(p[c("q7")]),
    "Risque financier (pénalités / urgences)" = sum(p[c("q9")]),
    "Indicateurs non fiabilisés" = sum(p[c("q5")])
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
    if (answer_point == 0L) return("Excel est un outil puissant - surtout entre de bonnes mains.\nMais dans beaucoup d'industries, il finit par devenir le centre névralgique... sans offrir la visibilité d'un vrai système de pilotage.")
    if (answer_point == 1L) return("Multiplier les outils permet souvent d'aller vite au début.\nMais sans intégration claire, cela crée progressivement des zones floues où chacun travaille avec sa propre lecture de la réalité.")
  }
  if (q_id == "q2") {
    if (answer_point == 1L) return("Lorsque plusieurs versions coexistent, les décisions prennent mécaniquement plus de temps.\nOn passe de l'analyse... à la vérification.")
    if (answer_point == 2L) return("Ne pas savoir quelle est la bonne version n'est pas un manque de rigueur.\nC'est souvent le signe qu'un process a grandi plus vite que sa structure.")
  }
  if (q_id == "q3") {
    if (answer_point == 1L) return("3h par semaine représentent environ 150h par an.\nÀ l'échelle d'un responsable ou d'une assistante, cela peut facilement représenter 8 000 à 10 000EUR immobilisés... sans création de valeur directe.")
    if (answer_point == 2L) return("Au-delà de 3h par semaine, on dépasse souvent 200 à 300 heures par an.\nCe sont plusieurs semaines de travail consacrées non pas à piloter, mais à vérifier.")
  }
  if (q_id == "q4") {
    if (answer_point == 1L) return("Un point hebdomadaire est structurant.\nMais une semaine peut suffire à transformer un léger décalage en situation sous tension.")
    if (answer_point == 2L) return("Découvrir un retard après coup n'est pas une erreur individuelle.\nC'est souvent le signe qu'il manque un système d'alerte en amont.")
  }
  if (q_id == "q5") {
    if (answer_point == 1L) return("Chaque manipulation manuelle est une charge invisible.\nElle mobilise de l'attention, crée un risque d'erreur... et détourne du pilotage stratégique.")
    if (answer_point == 2L) return("30 minutes par jour de ressaisie représentent environ 110h par an.\nÀ l'échelle d'une équipe, cela peut rapidement dépasser 10 000EUR par an en temps mobilisé.")
  }
  if (q_id == "q6") {
    if (answer_point == 1L) return("Quelques minutes semblent anodines.\nMais multipliées par plusieurs vérifications quotidiennes, cela représente des dizaines d'heures par an consacrées uniquement à chercher l'information.")
    if (answer_point == 2L) return("Lorsque vérifier une échéance prend 30 minutes,\ncela signifie généralement que l'information existe... mais n'est pas structurée pour décider rapidement.")
  }
  if (q_id == "q7") {
    if (answer_point == 1L) return("Même rarement, une demande oubliée peut créer une chaîne d'ajustements imprévus.\nEt souvent, c'est l'urgence qui révèle la faiblesse du process.")
    if (answer_point == 2L) return("Quand les demandes se perdent, le problème n'est pas humain.\nC'est souvent l'absence d'un système clair de suivi et de priorisation.")
  }
  if (q_id == "q8") {
    if (answer_point == 1L) return("Un ralentissement temporaire est courant.\nMais lorsqu'il dépend d'une seule personne, le risque organisationnel devient structurel.")
    if (answer_point == 2L) return("Lorsque la connaissance repose sur une seule tête,\nla continuité opérationnelle devient fragile - même avec des équipes compétentes.")
  }
  if (q_id == "q9") {
    if (answer_point == 1L) return("Une seule urgence peut représenter plusieurs milliers d'euros.\nMais surtout, elle mobilise l'énergie des équipes en mode réaction.")
    if (answer_point == 2L) return("Lorsque les urgences deviennent répétitives,\ncela indique souvent que le pilotage est plus réactif que prédictif.")
  }
  if (q_id == "q10") {
    if (answer_point == 1L) return("C'est souvent à ce stade que les fuites invisibles commencent à s'installer.\nRien de dramatique... mais un potentiel d'optimisation réel.")
    if (answer_point == 2L) return("Un process fragile ne coûte pas toujours cher immédiatement.\nMais il finit presque toujours par coûter cher au mauvais moment.")
  }
  ""
}

impact_text <- function(band_label) {
  if (band_label == "Process maîtrisés") {
    return("Votre organisation paraît saine, avec des marges de progression sur la circulation d'information et la fiabilité des routines.")
  }
  if (band_label == "Sous tension") {
    return("Le diagnostic montre des points de friction qui pèsent sur la fluidité opérationnelle : temps de vérification, retards visibles trop tard et dépendances humaines.")
  }
  "Même 1 heure par jour consacrée à des tâches de consolidation ou de recherche d'information représente plus de 200 heures par an. À 60EUR de l'heure, cela peut facilement dépasser 12 000EUR immobilisés - sans amélioration directe de la performance. La bonne nouvelle : ces heures sont souvent récupérables avec une meilleure structuration."
}

score_order_line <- function(score_total) {
  if (score_total <= 8) return("Ne serait-ce que 15 min/jour de ressaisie, c'est déjà souvent plusieurs milliers d'euros par an.")
  if (score_total <= 14) return("Avec 30 min/jour de ressaisie, on parle souvent de plus de 10k EUR/an en ordre de grandeur.")
  "Avec 1h/jour perdue, l'ordre de grandeur est souvent entre 10k et 15k EUR/an, hors pénalités et urgences."
}

send_admin_email <- function(payload) {
  user <- Sys.getenv("SMTP_USER", "")
  pass <- Sys.getenv("SMTP_PASS", "")
  from <- Sys.getenv("SMTP_FROM", if (nzchar(user)) user else "")
  to <- Sys.getenv("SMTP_TO", Sys.getenv("ADMIN_EMAIL", if (nzchar(user)) user else ""))
  host <- Sys.getenv("SMTP_HOST", "smtp.gmail.com")
  port <- suppressWarnings(as.integer(Sys.getenv("SMTP_PORT", "465")))
  if (is.na(port)) port <- 465L

  if (any(!nzchar(c(user, pass, from, to)))) {
    warning("Email non configuré : variables requises SMTP_USER, SMTP_PASS et SMTP_TO (ou ADMIN_EMAIL).")
    return(FALSE)
  }

  python_bin <- Sys.which("python3")
  if (!nzchar(python_bin)) python_bin <- Sys.which("python")
  if (!nzchar(python_bin)) {
    warning("Python indisponible dans l'environnement. Email non envoyé.")
    return(FALSE)
  }

  detail_lines <- payload$answer_details %||% character(0)
  body_lines <- c(
    "Nouveau diagnostic chatbot",
    "",
    sprintf("Timestamp UTC: %s", payload$timestamp_utc),
    sprintf("Nom: %s %s", payload$prenom, payload$nom),
    sprintf("Entreprise: %s", payload$entreprise),
    sprintf("Email: %s", payload$email),
    sprintf("Score: %s (%s)", payload$score_total, payload$band),
    sprintf("Top 3 fuites: %s", paste(payload$top3, collapse = " | ")),
    "",
    "Détail des réponses:",
    if (length(detail_lines)) paste0("- ", detail_lines) else "- Aucun détail"
  )

  subject <- sprintf("Nouveau diagnostic - %s", payload$entreprise %||% "Sans entreprise")
  body_text <- paste(body_lines, collapse = "\n")

  py_code <- paste(
    "import os, smtplib",
    "from email.message import EmailMessage",
    "msg = EmailMessage()",
    "msg['Subject'] = os.environ['MAIL_SUBJECT']",
    "msg['From'] = os.environ['MAIL_FROM']",
    "msg['To'] = os.environ['MAIL_TO']",
    "msg.set_content(os.environ['MAIL_BODY'])",
    "with smtplib.SMTP_SSL(os.environ['SMTP_HOST'], int(os.environ['SMTP_PORT'])) as server:",
    "    server.login(os.environ['SMTP_USER'], os.environ['SMTP_PASS'])",
    "    server.send_message(msg)",
    sep = "\n"
  )

  env <- c(
    paste0("SMTP_HOST=", host),
    paste0("SMTP_PORT=", as.integer(port)),
    paste0("SMTP_USER=", user),
    paste0("SMTP_PASS=", pass),
    paste0("MAIL_FROM=", from),
    paste0("MAIL_TO=", to),
    paste0("MAIL_SUBJECT=", subject),
    paste0("MAIL_BODY=", body_text)
  )

  out <- tryCatch(
    system2(python_bin, c("-c", py_code), stdout = TRUE, stderr = TRUE, env = env),
    error = function(e) e
  )

  if (inherits(out, "error")) {
    warning(sprintf("Envoi email admin échoué (python SMTP): %s", out$message))
    return(FALSE)
  }

  status <- attr(out, "status")
  if (!is.null(status) && status != 0) {
    warning(sprintf("Envoi email admin échoué (python SMTP), code=%s, détail=%s", status, paste(out, collapse = " | ")))
    return(FALSE)
  }

  message(sprintf("Email admin envoyé via SMTP Gmail à %s", to))
  TRUE
}

validate_email_setup <- function() {
  user <- Sys.getenv("SMTP_USER", "")
  pass <- Sys.getenv("SMTP_PASS", "")
  to <- Sys.getenv("SMTP_TO", Sys.getenv("ADMIN_EMAIL", ""))
  host <- Sys.getenv("SMTP_HOST", "smtp.gmail.com")
  port <- Sys.getenv("SMTP_PORT", "465")

  checks <- c(
    sprintf("SMTP_USER: %s", if (nzchar(user)) "OK" else "MANQUANT"),
    sprintf("SMTP_PASS: %s", if (nzchar(pass)) "OK" else "MANQUANT"),
    sprintf("SMTP_TO/ADMIN_EMAIL: %s", if (nzchar(to)) "OK" else "MANQUANT"),
    sprintf("SMTP_HOST: %s", if (nzchar(host)) host else "smtp.gmail.com"),
    sprintf("SMTP_PORT: %s", if (nzchar(port)) port else "465")
  )
  paste(checks, collapse = " | ")
}

if (interactive()) {
  message("Config email SMTP: ", validate_email_setup())
}

validate_email_setup <- function() {
  user <- Sys.getenv("SMTP_USER", "")
  pass <- Sys.getenv("SMTP_PASS", "")
  to <- Sys.getenv("SMTP_TO", Sys.getenv("ADMIN_EMAIL", ""))
  host <- Sys.getenv("SMTP_HOST", "smtp.gmail.com")
  port <- Sys.getenv("SMTP_PORT", "465")

  checks <- c(
    sprintf("SMTP_USER: %s", if (nzchar(user)) "OK" else "MANQUANT"),
    sprintf("SMTP_PASS: %s", if (nzchar(pass)) "OK" else "MANQUANT"),
    sprintf("SMTP_TO/ADMIN_EMAIL: %s", if (nzchar(to)) "OK" else "MANQUANT"),
    sprintf("SMTP_HOST: %s", if (nzchar(host)) host else "smtp.gmail.com"),
    sprintf("SMTP_PORT: %s", if (nzchar(port)) port else "465")
  )
  paste(checks, collapse = " | ")
}

if (interactive()) {
  message("Config email: ", validate_email_setup())
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
          div(class = "title-block",
              h1("Faites une analyse de vos process actuels"),
              p(class = "hero-subtitle", "Transformez vos frictions opérationnelles en plan d'action clair en moins de 2 minutes.")
          ),
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
          ),
          div(
            class = "app-footer",
            HTML("Développé en 🇫🇷 par <a href='https://www.linkedin.com/in/arsmouk-data-analyst/' target='_blank' rel='noopener noreferrer'>Abd Arsmouk</a>")
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

  text_input <- function(field, prompt, placeholder = "Votre réponse") {
    list(mode = "text", payload = list(field = field, prompt = prompt, placeholder = placeholder))
  }

  submit_input <- function() {
    list(mode = "submit", payload = list(button = "Voir mon diagnostic", legal = "En soumettant ces informations, vous acceptez d'être recontacté au sujet de votre diagnostic."))
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
      send_event("bot", text = "Parfait, j’ai l’essentiel. Vous pouvez afficher votre diagnostic personnalisé.", typing = TRUE, typingMinMs = 500, typingMaxMs = 900, afterInput = submit_input())
      return(invisible(NULL))
    }

    field <- lead_fields[[i]]
    if (field == "prenom") {
      send_event("bot", text = "Quel est votre prénom ?", typing = TRUE, typingMinMs = 500, typingMaxMs = 900, afterInput = text_input("prenom", "Réponse rapide", "Prénom"))
    } else if (field == "nom") {
      send_event("bot", text = "Votre nom ?", typing = TRUE, typingMinMs = 500, typingMaxMs = 900, afterInput = text_input("nom", "Réponse rapide", "Nom"))
    } else if (field == "entreprise") {
      send_event("bot", text = "Votre entreprise ?", typing = TRUE, typingMinMs = 500, typingMaxMs = 900, afterInput = text_input("entreprise", "Réponse rapide", "Entreprise"))
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
    send_event("bot", text = "Bonjour 👋\nEn 2 minutes, je vais vous aider à mettre des mots (et des chiffres) sur ce que vous ressentez peut-être déjà dans votre organisation.", typing = FALSE, afterInput = list(mode = "none", payload = list()))
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
      send_event("bot", text = "Aucun souci. Revenez quand vous voulez, je serai là pour vous aider.", typing = TRUE, typingMinMs = 500, typingMaxMs = 900, afterInput = list(mode = "end", payload = list()))
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
        send_event("bot", text = "Merci.\nJ’ai identifié plusieurs signaux potentiels. J’ai besoin de 4 infos pour afficher votre diagnostic personnalisé.", typing = TRUE, typingMinMs = 500, typingMaxMs = 900, afterInput = list(mode = "none", payload = list()))
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
        send_event("input_error", message = "Réponse requise.")
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
        answers = unname(unlist(a[paste0("q", 1:10)])),
        answer_details = unlist(lapply(seq_along(questions), function(i) {
          key <- paste0("q", i)
          answer_idx <- as.integer(a[[key]]) + 1L
          answer_label <- if (is.na(answer_idx) || answer_idx < 1L || answer_idx > length(questions[[i]]$choices)) "Non renseigné" else questions[[i]]$choices[[answer_idx]]
          sprintf("Q%s: %s => %s", i, questions[[i]]$text, answer_label)
        }))
      ))
      pending_record(NULL)
    }
    push_progress("result_shown")
    stage("result")
  })
}

shinyApp(ui, server)
