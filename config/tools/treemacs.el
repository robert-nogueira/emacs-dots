;;; treemacs.el --- Configuration for Treemacs
;;; Commentary:
;; This file configures Treemacs, a file and directory visualization tool
;; in Emacs.  It allows for easy navigation of your project's file structure.
;;; Code:
(use-package treemacs
  :ensure t
  :defer nil
  :bind (("M-\\" . my/treemacs-toggle))
  :config
  (setq treemacs-hide-gitignored-files-mode t)
  (setq treemacs-width 45)
  (setq treemacs-width-is-initially-locked nil)
  (setq delete-by-moving-to-trash t)
  (setq treemacs-collapse-dirs 3)
  (setq treemacs-display-in-side-window t)
  (setq treemacs-is-never-other-window t)
  (setq treemacs-indentation 2)
  (setq treemacs-indentation-string " ")
  (setq treemacs-filewatch-mode t)
  (setq treemacs-git-mode 'deferred)
  (setq treemacs-move-files-by-mouse-dragging nil)
  (setq treemacs-move-forward-on-expand t)
  (setq treemacs-pulse-on-success t)
  (setq treemacs-file-event-delay 0)
  (setq treemacs-deferred-git-apply-delay 0)
  (setq treemacs-git-commit-diff-mode 1)

  (defun treemacs-ignore-gitignore (file _)
    (string= file "__pycache__"))
  (push #'treemacs-ignore-gitignore treemacs-ignored-file-predicates))

(remove-hook 'treemacs-mode-hook #'treemacs-project-follow-mode)

(defun my/treemacs-update-follow (&rest _args)
  (if (string= (treemacs-workspace->name (treemacs-current-workspace))
               "Default")
      (treemacs-project-follow-mode 1)
    (treemacs-project-follow-mode nil)))

(defvar my/treemacs-frame-workspaces (make-hash-table :test 'equal)
  "Hash table mapping frame IDs to workspace names.")

(defun my/treemacs-frame-id (&optional frame)
  "Return unique ID for FRAME."
  (let ((frame (or frame (selected-frame))))
    (format "%s" (sxhash frame))))

(defun my/treemacs-get-current-project-root ()
  "Return current project root, if any."
  (or
   (and (fboundp 'projectile-project-root)
        (ignore-errors (projectile-project-root)))
   (and (fboundp 'project-current)
        (when-let ((project (project-current)))
          (if (fboundp 'project-root)
              (project-root project)
            (car (project-roots project)))))
   (and (fboundp 'vc-root-dir)
        (ignore-errors (vc-root-dir)))
   default-directory))

(defun my/treemacs-add-project-to-workspace (workspace-name project-root)
  "Add PROJECT-ROOT to WORKSPACE-NAME if not already present."
  (when (and workspace-name project-root (file-directory-p project-root))
    (let* ((workspace (--first (string= (treemacs-workspace->name it) workspace-name)
                               treemacs--workspaces))
           (project-path (treemacs--canonical-path project-root)))
      (when workspace
        (unless (--any (string= (treemacs-project->path it) project-path)
                      (treemacs-workspace->projects workspace))
          (save-selected-window
            (treemacs-do-add-project-to-workspace project-path workspace-name)))))))

(defun my/treemacs-get-or-create-workspace (&optional frame)
  "Get or create unique workspace for FRAME."
  (let* ((frame (or frame (selected-frame)))
         (frame-id (my/treemacs-frame-id frame))
         (workspace-name (or (gethash frame-id my/treemacs-frame-workspaces)
                            (format "Frame-%s" frame-id)))
         (is-new-workspace nil))
    (puthash frame-id workspace-name my/treemacs-frame-workspaces)
    (unless (--first (string= (treemacs-workspace->name it) workspace-name)
                     treemacs--workspaces)
      (treemacs--find-workspace)
      (let ((new-ws (treemacs-workspace->create! :name workspace-name)))
        (add-to-list 'treemacs--workspaces new-ws)
        (setf (treemacs-current-workspace) new-ws)
        (setq is-new-workspace t)))
    (when is-new-workspace
      (let ((project-root (my/treemacs-get-current-project-root)))
        (when project-root
          (my/treemacs-add-project-to-workspace workspace-name project-root))))
    workspace-name))

(defun my/treemacs-toggle ()
  "Toggle Treemacs with frame-specific workspace."
  (interactive)
  (let* ((workspace-name (my/treemacs-get-or-create-workspace))
         (current-ws (treemacs-current-workspace))
         (project-root (my/treemacs-get-current-project-root)))
    (unless (string= (treemacs-workspace->name current-ws) workspace-name)
      (let ((target-ws (--first (string= (treemacs-workspace->name it) workspace-name)
                               treemacs--workspaces)))
        (when target-ws
          (setf (treemacs-current-workspace) target-ws))))
    (when project-root
      (my/treemacs-add-project-to-workspace workspace-name project-root))
    (treemacs)))

(defun my/treemacs-close-on-delete-frame (frame)
  "Close Treemacs and cleanup workspace when FRAME is deleted."
  (let ((frame-id (my/treemacs-frame-id frame)))
    (with-selected-frame frame
      (when (treemacs-get-local-window)
        (delete-window (treemacs-get-local-window))))
    (remhash frame-id my/treemacs-frame-workspaces)))

(defun my/treemacs-ensure-correct-workspace ()
  "Ensure correct workspace is active for current frame."
  (when (and (featurep 'treemacs)
             (treemacs-get-local-window))
    (let* ((workspace-name (my/treemacs-get-or-create-workspace))
           (current-ws (treemacs-current-workspace))
           (project-root (my/treemacs-get-current-project-root)))
      (unless (string= (treemacs-workspace->name current-ws) workspace-name)
        (let ((target-ws (--first (string= (treemacs-workspace->name it) workspace-name)
                                 treemacs--workspaces)))
          (when target-ws
            (setf (treemacs-current-workspace) target-ws)
            (treemacs--refresh-on-ui-change))))
      (when project-root
        (my/treemacs-add-project-to-workspace workspace-name project-root)))))

(defun my/treemacs-setup-new-frame (frame)
  "Setup workspace for new FRAME."
  (with-selected-frame frame
    (my/treemacs-get-or-create-workspace frame)))

(add-hook 'after-make-frame-functions #'my/treemacs-setup-new-frame)
(add-hook 'delete-frame-functions #'my/treemacs-close-on-delete-frame)
(add-hook 'buffer-list-update-hook #'my/treemacs-ensure-correct-workspace)

(with-eval-after-load 'treemacs
  (when (framep (selected-frame))
    (my/treemacs-get-or-create-workspace (selected-frame))))

(add-hook 'treemacs-mode-hook #'my/treemacs-update-follow)
(advice-add 'treemacs-switch-workspace :after #'my/treemacs-update-follow)

(use-package treemacs-nerd-icons
  :after nerd-icons
  :defer nil
  :config
  (treemacs-load-theme "nerd-icons"))

(provide 'treemacs)
;;; treemacs.el ends here
