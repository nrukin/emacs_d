;; hydra
(use-package hydra
  :defer 2
  :bind (("<f9>" . hydra-clock/body)
	 :map go-mode-map
	 ("<f8>" . hydra-go/body))
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
    ("l" org-clock-in-last-with-prefix-arg))

  (defhydra hydra-go (:color blue)
    "
    ^
    ^Code^                  ^Folding^
    ^─────^─────────────────^───────^─────────
    _c_ code actions        _f_ fold
    _r_ rename              _s_ unfold
    _q_ quit                _F_ fold all
    _d_ buffer diagnostics  _S_ unfold all
    _D_ project diagnostics
    _h_ doc
    _H_ doc buffer
    ^^
    "
    ("c" eglot-code-actions)
    ("r" eglot-rename)
    ("d" flymake-show-buffer-diagnostics)
    ("D" flymake-show-project-diagnostics)
    ("h" eldoc)
    ("H" eldoc-doc-buffer)
    ("f" hs-hide-block :color red)
    ("s" hs-show-block :color red)
    ("F" hs-hide-all :color red)
    ("S" hs-show-all :color red)
    ("q" nil)))
