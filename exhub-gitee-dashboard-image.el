;;; exhub-gitee-dashboard-image.el --- Gitee Enterprise dashboard: inline images -*- lexical-binding: t; -*-

;; Copyright (C) 2025 EdmondFrank

;; Author: EdmondFrank
;; Keywords: tools, gitee, dashboard

;;; Commentary:

;; Org only inlines `file:' / `attachment:' links, so the remote images in a
;; Gitee description, comment or event body -- rendered as Org links by
;; `exhub-gitee-dashboard-markdown.el' -- are decorated here: the detail
;; buffer is scanned for image links, the pictures are downloaded one at a
;; time into `exhub-gitee-dashboard-image-cache-dir', and an overlay showing
;; the image is laid over each link.
;;
;; Downloads are asynchronous, so the buffer is readable at once and images
;; appear while they arrive.  A link whose image cannot be fetched is left
;; untouched and keeps working as a normal link.

;;; Code:

(require 'cl-lib)
(require 'image)
(require 'subr-x)

(require 'exhub-gitee-dashboard-vars)

(defconst exhub-gitee-dashboard--image-link-re
  "\\[\\[\\(https?://[^]\n\t ]+\\)\\]\\(?:\\[[^]\n]*\\]\\)?\\]"
  "Regexp matching an Org link to a remote URL.
The first group captures the URL; the match covers the whole link so that the
overlay can hide it completely.  Every link matches -- the extension is
inspected separately, because Gitee serves images from URLs with a query
string and most links in a detail are not images.")

(defvar exhub-gitee-dashboard--image-queue nil
  "Image downloads waiting to start, as a list of (URL . BUFFER).")

(defvar exhub-gitee-dashboard--image-active nil
  "URL of the image download currently running, if any.")

(defvar exhub-gitee-dashboard--image-failed (make-hash-table :test #'equal)
  "URLs whose download failed during this session.")

(defvar-local exhub-gitee-dashboard--image-hidden nil
  "Non-nil when the inline images of the current buffer are hidden.")

;;;; URLs and cached files

(defun exhub-gitee-dashboard--image-url-path (url)
  "Return the path part of URL, without its query string or fragment."
  (car (split-string (or url "") "[?#]")))

(defun exhub-gitee-dashboard--image-type (url)
  "Return the image type an inline view of URL would use, or nil.
The type is inferred from the URL extension and must be supported by this
Emacs, so that an unusable image is never downloaded."
  (let* ((ext (downcase (or (file-name-extension
                             (exhub-gitee-dashboard--image-url-path url))
                            "")))
         (type (cdr (assoc ext '(("png" . png) ("jpg" . jpeg)
                                 ("jpeg" . jpeg) ("gif" . gif)
                                 ("svg" . svg) ("bmp" . bmp)
                                 ("webp" . webp) ("tif" . tiff)
                                 ("tiff" . tiff))))))
    (and type (image-type-available-p type) type)))

(defun exhub-gitee-dashboard--image-cache-file (url)
  "Return the cache file holding the image of URL."
  (expand-file-name (secure-hash 'sha1 url)
                    exhub-gitee-dashboard-image-cache-dir))

;;;; Overlays

(defun exhub-gitee-dashboard--image-overlay (beg)
  "Return the image overlay covering BEG, or nil."
  (cl-find-if (lambda (overlay) (overlay-get overlay 'exhub-gitee-image))
              (overlays-at beg)))

(defun exhub-gitee-dashboard--image-shown-p ()
  "Return non-nil when the current buffer shows at least one image."
  (cl-some (lambda (overlay) (overlay-get overlay 'exhub-gitee-image))
           (overlays-in (point-min) (point-max))))

(defun exhub-gitee-dashboard--image-display (url beg end)
  "Display the cached image of URL over the link spanning BEG to END.
Return the overlay, or nil when no usable image is cached yet."
  (let ((file (exhub-gitee-dashboard--image-cache-file url))
        (width exhub-gitee-dashboard-image-max-width))
    (when (file-readable-p file)
      (when-let* ((type (image-type-from-file-header file))
                  (type (and (image-type-available-p type) type)))
        (let ((image (apply #'create-image file type nil
                            (append '(:ascent 100)
                                    (when width (list :max-width width)))))
              (overlay (make-overlay beg end)))
          (overlay-put overlay 'display image)
          (overlay-put overlay 'exhub-gitee-image url)
          (overlay-put overlay 'keymap image-map)
          (overlay-put overlay 'help-echo (format "图片: %s" url))
          overlay)))))

(defun exhub-gitee-dashboard--image-remove ()
  "Remove the inline image overlays from the current buffer."
  (interactive)
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when (overlay-get overlay 'exhub-gitee-image)
      (delete-overlay overlay))))

(defun exhub-gitee-dashboard--image-prune ()
  "Delete the image overlays a buffer rewrite left empty.
Rewriting the buffer does not necessarily delete an overlay; it may shrink
it to a single point instead."
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when (and (overlay-get overlay 'exhub-gitee-image)
               (= (overlay-start overlay) (overlay-end overlay)))
      (delete-overlay overlay))))

;;;; Downloading

(defun exhub-gitee-dashboard--image-command (temp url)
  "Return the curl command line downloading URL into TEMP."
  (append (list "curl" "-fsSL" "--max-time"
                (number-to-string exhub-gitee-dashboard-image-timeout))
          (when exhub-gitee-dashboard-image-max-size
            (list "--max-filesize"
                  (number-to-string exhub-gitee-dashboard-image-max-size)))
          (list "-o" temp url)))

(defun exhub-gitee-dashboard--image-enqueue (url buffer)
  "Queue URL for download on behalf of BUFFER."
  (unless (or (gethash url exhub-gitee-dashboard--image-failed)
              (equal url exhub-gitee-dashboard--image-active)
              (assoc url exhub-gitee-dashboard--image-queue)
              (null (executable-find "curl")))
    (setq exhub-gitee-dashboard--image-queue
          (append exhub-gitee-dashboard--image-queue
                  (list (cons url buffer))))
    (exhub-gitee-dashboard--image-pump)))

(defun exhub-gitee-dashboard--image-pump ()
  "Start the next queued image download unless one is already running."
  (when (and (null exhub-gitee-dashboard--image-active)
             exhub-gitee-dashboard--image-queue)
    (let ((item (pop exhub-gitee-dashboard--image-queue)))
      (setq exhub-gitee-dashboard--image-active (car item))
      (exhub-gitee-dashboard--image-download (car item) (cdr item)))))

(defun exhub-gitee-dashboard--image-download (url buffer)
  "Download URL on behalf of BUFFER, then display it there."
  (let* ((file (exhub-gitee-dashboard--image-cache-file url))
         (temp (make-temp-file "exhub-gitee-image-"))
         (sentinel
          (lambda (process _event)
            (when (memq (process-status process) '(exit signal))
              (exhub-gitee-dashboard--image-finish
               url buffer temp file
               (and (zerop (process-exit-status process)) t))))))
    (condition-case nil
        (progn
          (make-directory (file-name-directory file) t)
          (make-process :name "exhub-gitee-image"
                        :noquery t
                        :buffer nil
                        :command (exhub-gitee-dashboard--image-command temp url)
                        :sentinel sentinel))
      (error
       (exhub-gitee-dashboard--image-finish url buffer temp file nil)))))

(defun exhub-gitee-dashboard--image-finish (url buffer temp file success)
  "Install TEMP as the image cache FILE of URL, then display it in BUFFER.
SUCCESS is non-nil when the download command succeeded; the file is
validated regardless.  A rejected download drops TEMP and remembers URL as
failed for the rest of the session."
  (let* ((type (and success
                    (file-readable-p temp)
                    (> (file-attribute-size (file-attributes temp)) 0)
                    (image-type-from-file-header temp)))
         (ok (and type (image-type-available-p type))))
    (if ok
        (progn
          (copy-file temp file t)
          (ignore-errors (delete-file temp))
          (remhash url exhub-gitee-dashboard--image-failed))
      (ignore-errors (delete-file temp))
      (puthash url t exhub-gitee-dashboard--image-failed))
    (setq exhub-gitee-dashboard--image-active nil)
    (when (and ok (buffer-live-p buffer))
      (with-current-buffer buffer
        (exhub-gitee-dashboard--image-refresh)))
    (exhub-gitee-dashboard--image-pump)))

;;;; Commands

(defun exhub-gitee-dashboard--image-refresh ()
  "Display the images cached for the current buffer, queueing the others."
  (exhub-gitee-dashboard--image-prune)
  (when (and exhub-gitee-dashboard-render-images
             (not exhub-gitee-dashboard--image-hidden)
             (display-graphic-p))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward exhub-gitee-dashboard--image-link-re nil t)
          (let ((url (match-string-no-properties 1))
                (beg (match-beginning 0))
                (end (match-end 0)))
            (when (and (exhub-gitee-dashboard--image-type url)
                       (null (exhub-gitee-dashboard--image-overlay beg)))
              (unless (exhub-gitee-dashboard--image-display url beg end)
                (exhub-gitee-dashboard--image-enqueue url (current-buffer))))))))))

(defun exhub-gitee-dashboard--image-inline ()
  "Display the images of the current buffer, downloading the missing ones."
  (setq-local exhub-gitee-dashboard--image-hidden nil)
  (exhub-gitee-dashboard--image-refresh))

(defun exhub-gitee-dashboard--image-toggle ()
  "Toggle the inline display of images in the current buffer."
  (interactive)
  (if (exhub-gitee-dashboard--image-shown-p)
      (progn
        (exhub-gitee-dashboard--image-remove)
        (setq-local exhub-gitee-dashboard--image-hidden t)
        (message "Gitee 图片已隐藏"))
    (exhub-gitee-dashboard--image-inline)
    (message "Gitee 图片已显示")))

;;;###autoload
(defun exhub-gitee-dashboard-clear-image-cache ()
  "Delete every image cached by the Gitee dashboard."
  (interactive)
  (let ((dir exhub-gitee-dashboard-image-cache-dir))
    (when (file-directory-p dir)
      (dolist (file (directory-files dir t "\\`[^.]"))
        (ignore-errors (delete-file file))))
    (clrhash exhub-gitee-dashboard--image-failed)
    (message "Gitee 图片缓存已清空: %s" dir)))

(provide 'exhub-gitee-dashboard-image)

;;; exhub-gitee-dashboard-image.el ends here