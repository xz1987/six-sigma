(function () {
  'use strict';

  var activeTour = null;

  function removeTour(notify) {
    if (!activeTour) return;
    window.removeEventListener('resize', activeTour.reposition);
    window.removeEventListener('scroll', activeTour.reposition, true);
    document.removeEventListener('keydown', activeTour.onKeydown);
    activeTour.spotlight.remove();
    activeTour.coachmark.remove();
    if (notify && window.Shiny && activeTour.completeInput) {
      window.Shiny.setInputValue(activeTour.completeInput, Date.now(), { priority: 'event' });
    }
    activeTour = null;
  }

  function visibleTarget(selector) {
    return Array.prototype.find.call(document.querySelectorAll(selector), function (node) {
      var style = window.getComputedStyle(node);
      return style.display !== 'none' && style.visibility !== 'hidden' && node.getClientRects().length;
    });
  }

  function clamp(value, minimum, maximum) {
    return Math.min(Math.max(value, minimum), maximum);
  }

  function positionTour() {
    if (!activeTour || !activeTour.target) return;
    var rect = activeTour.target.getBoundingClientRect();
    var pad = 8;
    var spotlight = activeTour.spotlight;
    spotlight.style.left = Math.max(6, rect.left - pad) + 'px';
    spotlight.style.top = Math.max(6, rect.top - pad) + 'px';
    spotlight.style.width = Math.max(24, Math.min(window.innerWidth - 12, rect.width + pad * 2)) + 'px';
    spotlight.style.height = Math.max(24, Math.min(window.innerHeight - 12, rect.height + pad * 2)) + 'px';

    var coachmark = activeTour.coachmark;
    var coachRect = coachmark.getBoundingClientRect();
    var gap = 16;
    var top = rect.bottom + gap;
    if (top + coachRect.height > window.innerHeight - 12) top = rect.top - coachRect.height - gap;
    if (top < 12) top = 12;
    var left = clamp(rect.left, 12, Math.max(12, window.innerWidth - coachRect.width - 12));
    coachmark.style.top = top + 'px';
    coachmark.style.left = left + 'px';
  }

  function renderStep(index, attempt) {
    if (!activeTour) return;
    var step = activeTour.steps[index];
    var target = visibleTarget(step.selector);
    if (!target && attempt < 12) {
      window.setTimeout(function () { renderStep(index, attempt + 1); }, 100);
      return;
    }
    if (!target) {
      removeTour(true);
      return;
    }

    activeTour.index = index;
    activeTour.target = target;
    activeTour.coachmark.innerHTML = '';

    var progress = document.createElement('div');
    progress.className = 'history-coachmark-progress';
    var count = document.createElement('span');
    count.textContent = 'Step ' + (index + 1) + ' of ' + activeTour.steps.length;
    var dots = document.createElement('span');
    dots.className = 'history-coachmark-dots';
    activeTour.steps.forEach(function (_, dotIndex) {
      var dot = document.createElement('i');
      if (dotIndex < index) dot.className = 'is-complete';
      if (dotIndex === index) dot.className = 'is-current';
      dots.appendChild(dot);
    });
    progress.appendChild(count);
    progress.appendChild(dots);

    var title = document.createElement('h3');
    title.textContent = step.title;
    var body = document.createElement('p');
    body.textContent = step.body;
    var action = document.createElement('p');
    action.className = 'history-coachmark-action';
    action.textContent = step.action;

    var buttons = document.createElement('div');
    buttons.className = 'history-coachmark-buttons';
    var skip = document.createElement('button');
    skip.type = 'button';
    skip.className = 'btn btn-link';
    skip.textContent = 'Skip tutorial';
    skip.addEventListener('click', function () { removeTour(true); });
    buttons.appendChild(skip);
    if (index > 0) {
      var back = document.createElement('button');
      back.type = 'button';
      back.className = 'btn btn-default';
      back.textContent = 'Back';
      back.addEventListener('click', function () { renderStep(index - 1, 0); });
      buttons.appendChild(back);
    }
    var next = document.createElement('button');
    next.type = 'button';
    next.className = 'btn btn-primary history-coachmark-next';
    next.textContent = index === activeTour.steps.length - 1 ? 'Finish tutorial' : 'Next';
    next.addEventListener('click', function () {
      if (index === activeTour.steps.length - 1) removeTour(true);
      else renderStep(index + 1, 0);
    });
    buttons.appendChild(next);

    activeTour.coachmark.appendChild(progress);
    activeTour.coachmark.appendChild(title);
    activeTour.coachmark.appendChild(body);
    activeTour.coachmark.appendChild(action);
    activeTour.coachmark.appendChild(buttons);

    target.scrollIntoView({ behavior: 'smooth', block: 'center', inline: 'nearest' });
    window.setTimeout(function () {
      positionTour();
      next.focus({ preventScroll: true });
    }, 360);
  }

  function startTour(payload) {
    removeTour(false);
    if (!payload || !payload.steps || !payload.steps.length) return;

    var spotlight = document.createElement('div');
    spotlight.className = 'history-coachmark-spotlight';
    spotlight.setAttribute('aria-hidden', 'true');
    var coachmark = document.createElement('section');
    coachmark.className = 'history-coachmark';
    coachmark.setAttribute('role', 'dialog');
    coachmark.setAttribute('aria-label', 'History analysis tutorial');
    document.body.appendChild(spotlight);
    document.body.appendChild(coachmark);

    activeTour = {
      steps: payload.steps,
      completeInput: payload.completeInput,
      spotlight: spotlight,
      coachmark: coachmark,
      target: null,
      index: 0,
      reposition: positionTour,
      onKeydown: function (event) {
        if (event.key === 'Escape') removeTour(true);
      }
    };
    window.addEventListener('resize', activeTour.reposition);
    window.addEventListener('scroll', activeTour.reposition, true);
    document.addEventListener('keydown', activeTour.onKeydown);
    window.setTimeout(function () { renderStep(0, 0); }, 350);
  }

  function register() {
    if (!window.Shiny || !window.Shiny.addCustomMessageHandler) {
      window.setTimeout(register, 50);
      return;
    }
    window.Shiny.addCustomMessageHandler('history-tour-start', startTour);
  }

  register();
})();
