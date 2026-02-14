(function () {
  var state = {
    queue: [],
    processing: false,
    inputMode: "none",
    inputDisabled: false,
    inputState: {},
    typingNode: null
  };

  function el(id) {
    return document.getElementById(id);
  }

  function escapeHtml(str) {
    return String(str)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/\"/g, "&quot;")
      .replace(/'/g, "&#039;");
  }

  function chatWindow() {
    return el("chat_window");
  }

  function inputRoot() {
    return el("chat_input");
  }

  function scrollToBottom() {
    var cw = chatWindow();
    if (!cw) return;
    cw.scrollTop = cw.scrollHeight;
  }

  function addBubble(role, text, asHtml) {
    var cw = chatWindow();
    if (!cw) return;
    var row = document.createElement("div");
    row.className = "msg-row " + role;

    var bubble = document.createElement("div");
    bubble.className = "bubble " + role;
    if (asHtml) {
      bubble.innerHTML = text;
    } else {
      bubble.textContent = text;
    }

    row.appendChild(bubble);
    cw.appendChild(row);
    scrollToBottom();
  }

  function addCard(payload) {
    var items = (payload.top3 || []).map(function (item) {
      return "<li>" + escapeHtml(item) + "</li>";
    }).join("");

    var html = ""
      + "<div class='result-card'>"
      + "<h3>Diagnostic - " + escapeHtml(payload.entreprise || "Votre entreprise") + "</h3>"
      + "<div class='result-badge' style='background:" + escapeHtml(payload.band_color || "#8e66c7") + "'>"
      + escapeHtml((payload.band_emoji || "") + " " + (payload.band_label || ""))
      + "</div>"
      + "<p><strong>Top 3 fuites potentielles</strong></p>"
      + "<ol class='result-list'>" + items + "</ol>"
      + "<p><strong>Impact estime</strong></p>"
      + "<p>" + escapeHtml(payload.impact || "") + "</p>"
      + "<p><strong>Ordre de grandeur</strong><br>" + escapeHtml(payload.order_line || "") + "</p>"
      + "<p>Si vous le souhaitez, je peux analyser plus precisement votre situation lors d'un echange rapide.</p>"
      + "<button class='chat-cta' data-open-slot='1'>Proposer un creneau</button>"
      + "</div>";

    addBubble("bot", html, true);
  }

  function showTyping() {
    var cw = chatWindow();
    if (!cw || state.typingNode) return;

    var row = document.createElement("div");
    row.className = "msg-row bot";
    row.innerHTML = "<div class='typing'><span></span><span></span><span></span></div>";
    state.typingNode = row;
    cw.appendChild(row);
    scrollToBottom();
  }

  function hideTyping() {
    if (state.typingNode && state.typingNode.parentNode) {
      state.typingNode.parentNode.removeChild(state.typingNode);
    }
    state.typingNode = null;
  }

  function setInputDisabled(disabled) {
    state.inputDisabled = !!disabled;
    var root = inputRoot();
    if (!root) return;
    if (disabled) {
      root.classList.add("disabled");
    } else {
      root.classList.remove("disabled");
    }

    var controls = root.querySelectorAll("button, input[type='text'], input[type='email'], input[type='checkbox']");
    controls.forEach(function (c) {
      c.disabled = !!disabled;
    });
  }

  function clearError() {
    var err = el("chat_input_error");
    if (err) err.textContent = "";
  }

  function setError(msg) {
    var err = el("chat_input_error");
    if (err) err.textContent = msg || "";
  }

  function renderInput(mode, payload) {
    state.inputMode = mode || "none";
    state.inputState = payload || {};

    var root = inputRoot();
    if (!root) return;

    var html = "";
    if (state.inputMode === "choices") {
      var options = payload.options || [];
      var buttons = options.map(function (opt, idx) {
        return "<button class='chat-choice-btn' data-choice='" + idx + "' data-value='" + escapeHtml(opt.value) + "' data-label='" + escapeHtml(opt.label) + "'>" + escapeHtml(opt.label) + "</button>";
      }).join("");
      var optout = "";
      if (payload.optout && payload.optout.value) {
        optout = "<button class='chat-optout-link' data-value='" + escapeHtml(payload.optout.value) + "' data-label='" + escapeHtml(payload.optout.label || "Pas maintenant") + "'>" + escapeHtml(payload.optout.label || "Pas maintenant") + "</button>";
      }
      html = "<div class='choice-wrap'>" + buttons + optout + "<div id='chat_input_error' class='input-error'></div></div>";
    } else if (state.inputMode === "text") {
      var t = payload || {};
      html = ""
        + "<div class='text-wrap'>"
        + "<div class='input-caption'>" + escapeHtml(t.prompt || "") + "</div>"
        + "<input id='chat_text_value' class='chat-input-field' type='text' autocomplete='off' placeholder='" + escapeHtml(t.placeholder || "Votre reponse") + "' />"
        + "<button id='chat_submit_text' class='chat-submit'>Envoyer</button>"
        + "<div id='chat_input_error' class='input-error'></div>"
        + "</div>";
    } else if (state.inputMode === "submit") {
      html = ""
        + "<div class='consent-wrap'>"
        + "<button id='chat_submit_lead' class='chat-submit'>" + escapeHtml((payload && payload.button) || "Voir mon diagnostic") + "</button>"
        + "<div class='legal-note'><em>" + escapeHtml((payload && payload.legal) || "") + "</em></div>"
        + "<div id='chat_input_error' class='input-error'></div>"
        + "</div>";
    } else if (state.inputMode === "end") {
      html = "<div class='end-wrap'><div class='input-caption'>Conversation terminee.</div></div>";
    }

    root.innerHTML = html;
    clearError();

    if (state.inputMode === "text") {
      var input = el("chat_text_value");
      if (input) {
        input.focus();
        input.addEventListener("keydown", function (e) {
          if (e.key === "Enter") {
            e.preventDefault();
            submitText();
          }
        });
      }
      var submit = el("chat_submit_text");
      if (submit) submit.addEventListener("click", submitText);
    }

    if (state.inputMode === "submit") {
      var submitLead = el("chat_submit_lead");
      if (submitLead) submitLead.addEventListener("click", submitLeadInput);
    }

    if (state.inputDisabled) {
      setInputDisabled(true);
    }
    scrollToBottom();
  }

  function submitChoice(btn) {
    if (state.inputDisabled) return;
    clearError();

    var label = btn.getAttribute("data-label") || "";
    var value = btn.getAttribute("data-value") || "";
    addBubble("user", label);
    setInputDisabled(true);

    Shiny.setInputValue("chat_input", {
      type: "choice",
      value: value,
      label: label,
      nonce: Date.now()
    }, { priority: "event" });
  }

  function submitText() {
    if (state.inputDisabled) return;
    clearError();

    var input = el("chat_text_value");
    if (!input) return;
    var value = (input.value || "").trim();
    if (!value) {
      setError("Ce champ est requis.");
      return;
    }

    addBubble("user", value);
    setInputDisabled(true);

    Shiny.setInputValue("chat_input", {
      type: "text",
      value: value,
      field: state.inputState.field || "",
      nonce: Date.now()
    }, { priority: "event" });
  }

  function submitLeadInput() {
    if (state.inputDisabled) return;
    clearError();
    addBubble("user", "Voir mon diagnostic");
    setInputDisabled(true);

    Shiny.setInputValue("chat_input", {
      type: "lead_submit",
      nonce: Date.now()
    }, { priority: "event" });
  }

  function bindGlobalHandlers() {
    document.addEventListener("click", function (e) {
      var restart = e.target.closest("#restart_btn");
      if (restart) {
        Shiny.setInputValue("chat_control", { type: "restart", nonce: Date.now() }, { priority: "event" });
        return;
      }

      var choice = e.target.closest(".chat-choice-btn");
      if (choice) {
        submitChoice(choice);
        return;
      }

      var optout = e.target.closest(".chat-optout-link");
      if (optout) {
        submitChoice(optout);
        return;
      }

      var slot = e.target.closest("[data-open-slot='1']");
      if (slot) {
        Shiny.setInputValue("open_slot", Date.now(), { priority: "event" });
      }
    });
  }

  function enqueue(event) {
    state.queue.push(event);
    processQueue();
  }

  function processQueue() {
    if (state.processing || state.queue.length === 0) return;
    state.processing = true;
    var event = state.queue.shift();

    if (event.type === "reset") {
      var cw = chatWindow();
      if (cw) cw.innerHTML = "";
      renderInput("none", {});
      hideTyping();
      setInputDisabled(false);
      state.processing = false;
      processQueue();
      return;
    }

    if (event.type === "input") {
      renderInput(event.mode, event.payload || {});
      setInputDisabled(false);
      state.processing = false;
      processQueue();
      return;
    }

    if (event.type === "input_error") {
      setError(event.message || "");
      setInputDisabled(false);
      state.processing = false;
      processQueue();
      return;
    }

    if (event.type === "bot") {
      var withTyping = event.typing !== false;
      var minMs = Number(event.typingMinMs || 500);
      var maxMs = Number(event.typingMaxMs || 900);
      var delay = minMs;
      if (maxMs > minMs) {
        delay = minMs + Math.floor(Math.random() * (maxMs - minMs + 1));
      }

      if (withTyping) {
        setInputDisabled(true);
        showTyping();
      }

      setTimeout(function () {
        hideTyping();
        addBubble("bot", event.text || "");

        if (event.afterInput) {
          renderInput(event.afterInput.mode || "none", event.afterInput.payload || {});
          setInputDisabled(false);
        }

        state.processing = false;
        processQueue();
      }, withTyping ? delay : 20);
      return;
    }

    if (event.type === "card") {
      var tMs = Number(event.typingMs || 1400);
      setInputDisabled(true);
      showTyping();
      setTimeout(function () {
        hideTyping();
        addCard(event.payload || {});
        Shiny.setInputValue("result_rendered", Date.now(), { priority: "event" });
        if (event.afterInput) {
          renderInput(event.afterInput.mode || "none", event.afterInput.payload || {});
        }
        setInputDisabled(false);
        state.processing = false;
        processQueue();
      }, tMs);
      return;
    }

    state.processing = false;
    processQueue();
  }

  function init() {
    bindGlobalHandlers();

    Shiny.addCustomMessageHandler("chat_event", function (event) {
      enqueue(event || {});
    });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
})();
