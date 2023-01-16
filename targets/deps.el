(package-refresh-contents)

(let ((deps '(request websocket))
      (test-deps '(ert-async)))
  (dolist (dep deps)
    (package-install dep))
  (dolist (dep test-deps)
    (package-install dep)))
