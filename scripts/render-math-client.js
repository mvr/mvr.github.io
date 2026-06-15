(function () {
  function renderMathSpan(span) {
    if (!window.katex || span.querySelector(".katex")) {
      return;
    }

    window.katex.render(span.textContent || "", span, {
      displayMode: span.classList.contains("display"),
      throwOnError: false,
      macros: [],
      fleqn: false
    });
  }

  function renderMath() {
    document
      .querySelectorAll("span.math.inline, span.math.display")
      .forEach(renderMathSpan);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", renderMath);
  } else {
    renderMath();
  }
})();
