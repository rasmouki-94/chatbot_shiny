(function () {
  function clamp(v, lo, hi) {
    return Math.max(lo, Math.min(hi, Number(v || 0)));
  }

  function setProgress(pct) {
    var fill = document.getElementById("progress_fill");
    var pctNode = document.getElementById("progress_pct");
    var v = clamp(pct, 0, 100);

    if (fill) {
      fill.style.width = v + "%";
    }
    if (pctNode) {
      pctNode.textContent = Math.round(v) + "%";
    }
  }

  function register() {
    if (!window.Shiny || !window.Shiny.addCustomMessageHandler || window.__progressHandlerRegistered) {
      return;
    }
    window.__progressHandlerRegistered = true;
    window.Shiny.addCustomMessageHandler("progress_update", function (msg) {
      setProgress(msg && msg.pct);
    });
  }

  window.setProgress = setProgress;

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", function () {
      setProgress(0);
      register();
    });
  } else {
    setProgress(0);
    register();
  }

  document.addEventListener("shiny:connected", register);
})();
