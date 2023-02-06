;; hydra
(use-package hydra
  :defer 2
  :bind ("<f9>" . hydra-clock/body)
  :config
  (defun org-clock-in-last-with-prefix-arg ()
    (interactive)
    (setq current-prefix-arg '(4)) ; C-u
    (call-interactively 'org-clock-in-last))
  (defhydra hydra-clock (:color blue)
    "
    ^
    ^Clock^             ^Do^
    ^─────^─────────────^──^─────────
    _q_ quit            _c_ cancel
    ^^                  _d_ display
    ^^                  _e_ effort
    ^^                  _i_ in
    ^^                  _j_ jump
    ^^                  _o_ out
    ^^                  _r_ report
    ^^                  _l_ continue last
    ^^                  ^^
    "
    ("q" nil)
    ("c" org-clock-cancel :color pink)
    ("d" org-clock-display)
    ("e" org-clock-modify-effort-estimate)
    ("i" org-clock-in)
    ("j" org-clock-goto)
    ("o" org-clock-out)
    ("r" org-clock-report)
    ("l" org-clock-in-last-with-prefix-arg)))
