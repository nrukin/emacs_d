(use-package hydra
  :ensure t
  :defer 2
  :after (:all (go-mode python))
  :bind (("<f9>" . hydra-clock/body)
	 :map go-mode-map
	 ("<f8>" . hydra-go/body)
	 :map python-mode-map
	 ("<f8>" . hydra-py/body)
	 :map org-mode-map
	 ("<f8>" . hydra-org/body)
	 :map c++-mode-map
	 ("<f8>" . hydra-cpp/body))
  :preface
  (defun my/org-clock-in-last-with-prefix-arg ()
    (interactive)
    (setq current-prefix-arg '(4)) ; C-u
    (call-interactively 'org-clock-in-last))
  :config
  (defhydra hydra-clock (:color blue)
    "
    ^
    ^Clock^             ^Do^             ^Pomodoro^
    ^─────^─────────────^──^─────────────^────────^────────────────
    _q_ quit            _c_ cancel       _p_ pomodoro
    ^^                  _d_ display
    ^^                  _h_ hide
    ^^                  _e_ effort
    ^^                  _i_ in
    ^^                  _j_ jump
    ^^                  _o_ out
    ^^                  _r_ report
    ^^                  _l_ continue last
    ^^                  ^^
    "
    ("q" nil) ("d" org-clock-display :color red) ("h" org-clock-remove-overlays :color red) ("c" org-clock-cancel :color pink)
    ("e" org-clock-modify-effort-estimate) ("i" org-clock-in) ("j" org-clock-goto) ("o" org-clock-out) ("r" org-clock-report)
    ("p" org-pomodoro) ("l" my/org-clock-in-last-with-prefix-arg))

  (defhydra hydra-go (:color blue)
    "
    ^
    ^Code^                  ^Folding^         ^Test^
    ^─────^─────────────────^───────^─────────^─────^─────────
    _c_ code actions        _f_ fold          _t_ file
    _r_ rename              _s_ unfold        _T_ project
    _q_ quit                _F_ fold all      _b_ benchmark file
    _d_ buffer diagnostics  _S_ unfold all    _B_ benchmark project
    _D_ project diagnostics
    _h_ doc
    _H_ doc buffer
    _v_ format buffer
    _'_ definitions
    ^^
    "
    ("c" eglot-code-actions) ("r" eglot-rename) ("q" nil) ("d" flymake-show-buffer-diagnostics) ("D" flymake-show-project-diagnostics)
    ("h" eldoc) ("H" eldoc-doc-buffer) ("v" eglot-format-buffer) ("'" imenu-list-smart-toggle :color red)
    ("f" hs-hide-block :color red) ("s" hs-show-block :color red) ("F" hs-hide-all :color red) ("S" hs-show-all :color red)
    ("t" go-test-current-file) ("T" go-test-current-project) ("b" go-test-current-file-benchmarks) ("B" go-test-current-project-benchmarks) )

  (defhydra hydra-py (:color blue)
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
    _v_ format buffer
    _'_ definitions
    ^^
    "
    ("c" eglot-code-actions) ("r" eglot-rename) ("q" nil) ("d" flymake-show-buffer-diagnostics) ("D" flymake-show-project-diagnostics) ("h" eldoc)
    ("H" eldoc-doc-buffer) ("v" eglot-format-buffer) ("'" imenu-list-smart-toggle :color red) ("f" hs-hide-block :color red) ("s" hs-show-block :color red)
    ("F" hs-hide-all :color red) ("S" hs-show-all :color red))

  (defhydra hydra-cpp (:color blue)
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
    _v_ format buffer
    _'_ definitions
    ^^
    "
    ("c" eglot-code-actions) ("r" eglot-rename) ("q" nil) ("d" flymake-show-buffer-diagnostics) ("D" flymake-show-project-diagnostics) ("h" eldoc)
    ("H" eldoc-doc-buffer) ("v" eglot-format-buffer) ("'" imenu-list-smart-toggle :color red) ("f" hs-hide-block :color red) ("s" hs-show-block :color red)
    ("F" hs-hide-all :color red) ("S" hs-show-all :color red))

  (defhydra hydra-org (:color blue)
    "
    ^
    ^Edit^
    ^─────^─────────────────
    _t_ insert structure template
    _q_ quit
    ^^
    "
    ("t" org-insert-structure-template)
    ("q" nil)))
