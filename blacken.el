;;; blacken.el --- Reformat python buffers using the "black" formatter

;; Copyright (C) 2018 Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; Homepage: https://github.com/proofit404/blacken
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.2"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Blacken uses black to format a Python buffer.  It can be called
;; explicitly on a certain buffer, but more conveniently, a minor-mode
;; 'blacken-mode' is provided that turns on automatically running
;; black on a buffer before saving.
;;
;; Installation:
;;
;; Add blacken.el to your load-path.
;;
;; To automatically format all Python buffers before saving, add the
;; function blacken-mode to python-mode-hook:
;;
;; (add-hook 'python-mode-hook 'blacken-mode)
;;
;;; Code:


(defgroup blacken nil
  "Reformat Python code with \"black\"."
  :group 'python)

(defcustom blacken-executable "black"
  "Name of the executable to run."
  :type 'string)

(defcustom blacken-line-length nil
  "Line length to enforce."
  :type 'number
  :safe 'numberp)

(defun blacken-call-bin (input-buffer output-buffer error-buffer)
  "Call process black.

Send INPUT-BUFFER content to the process stdin.  Saving the
output to OUTPUT-BUFFER.  Saving process stderr to ERROR-BUFFER.
Return black process the exit code."
  ;; using the input buffer
  (with-current-buffer input-buffer
    ;; Assign the external process to process
    (let ((process
	   ;; run a program in a subprocess and return the process object
	   (make-process :name "blacken"
			 ;; The command to be run
                         :command `(,blacken-executable ,@(blacken-call-args))
			 ;; the output buffer
                         :buffer output-buffer
			 ;; pipe the standard error to the the error buffer
                         :stderr error-buffer
			 ;; don't stop the user if this is still running
			 ;; when they try to kill emacs.
                         :noquery t
			 ;; this installs a callback which will execute whenever
			 ;; the associated process changes status
                         :sentinel (lambda (process event)))))
      ;; don't ask emacs for query on exit from the process (duplicate?)
      (set-process-query-on-exit-flag (get-buffer-process error-buffer) nil)
      ;; set the sentinel as the process associated with error buffer (also duplicate?)
      (set-process-sentinel (get-buffer-process error-buffer)
			    (lambda (process event)))
      ;; Aren't these previous 2 statements just doing what the 2 last kwargs
      ;; from the makeprocess function are doing?
      
      ;; save the current buffers status then restore it when you're done
      ;; executing BODY
      (save-restriction
	;; if the buffer is narrowed then widen it
        (widen)
	;; Send the entire buffer to the process
        (process-send-region process (point-min) (point-max)))
      ;; indicate to the process that it has received all required input
      (process-send-eof process)
      ;; pipe the output from the process into emacs
      (accept-process-output process nil nil t)
      ;; While the process hasn't yet exited, accept input from it
      (while (process-live-p process)
        (accept-process-output process nil nil t))
      ;; return the exit status of process
      (process-exit-status process))))

(defun blacken-call-args ()
  "Build black process call arguments."
  (append
   (when blacken-line-length
     (list "--line-length" (number-to-string blacken-line-length)))
   '("-")))

;;;###autoload
(defun blacken-buffer (&optional display)
  "Try to blacken the current REGION if not nil else blacken the whole buffer.

Show black output, if black exit abnormally and DISPLAY is t."
  (interactive (list t))
  (let* ((original-buffer (current-buffer))
         (original-point (point))
         (original-window-pos (window-start))
         (tmpbuf (get-buffer-create "*blacken*"))
         (errbuf (get-buffer-create "*blacken-error*")))
    ;; This buffer can be left after previous black invocation.  It
    ;; can contain error message of the previous run.
    (dolist (buf (list tmpbuf errbuf))
      (with-current-buffer buf
        (erase-buffer)))
    (condition-case err
        (if (not (zerop (blacken-call-bin original-buffer tmpbuf errbuf)))
            (error "Black failed, see %s buffer for details" (buffer-name errbuf))
          (unless (eq (compare-buffer-substrings tmpbuf nil nil original-buffer nil nil) 0)
            (with-current-buffer tmpbuf
              (copy-to-buffer original-buffer (point-min) (point-max))))
          (mapc 'kill-buffer (list tmpbuf errbuf))
          (goto-char original-point)
          (set-window-start (selected-window) original-window-pos))
      (error (message "%s" (error-message-string err))
             (when display
               (pop-to-buffer errbuf))))))


;;;###autoload
(defun blacken-region (&optional display)
  "Try to blacken the current region.

Show black output, if black exit abnormally and DISPLAY is t."
  ;; pass a list in as an argument somehow
  (interactive (list t))
  ;; let* - nest each of the succeeding lets inside the previous, thus allowing
  ;; use of an earlier variable inside the initialisation of a later one.
  (let*
      ;; Set the 'original-buffer' variable to refer to the current buffer
      ((original-buffer (current-buffer))
       ;; Set the 'original-point' variable to refer to the current point
       (original-point (point))
       ;; set the 'original-window-position' variable to refer to the start of the
       ;; current window
       (original-window-pos (window-start))
       ;; create a temporary buffer called 'blacken'
       (tmpbuf (get-buffer-create "*blacken*"))
       ;; create a temporary buffer called blacken error
       (errbuf (get-buffer-create "*blacken-error*")))
    ;; This buffer can be left after previous black invocation.  It
    ;; can contain error message of the previous run.

    ;; loop over the given list
    (dolist
	;; buf - the variable to which the car of the list of dolist is bound
	(buf
	 ;; create and return a list constructed of the subsequent elements
	 (list tmpbuf errbuf))
      ;; erase the contents of the blacken and blacken-error
      (with-current-buffer buf
        (erase-buffer)))
    ;; error handling, kind of like a try-except
    (condition-case err
        (if (not
	     ;; If the called function returns any non-zero value
	     (zerop
	      ;; call the blacken binary
	      (blacken-call-bin original-buffer tmpbuf errbuf)))
            (error "Black failed, see %s buffer for details" (buffer-name errbuf))
          (unless (eq (compare-buffer-substrings tmpbuf nil nil original-buffer nil nil) 0)
            (with-current-buffer tmpbuf
              (copy-to-buffer original-buffer (point-min) (point-max))))
          (mapc 'kill-buffer (list tmpbuf errbuf))
          (goto-char original-point)
          (set-window-start (selected-window) original-window-pos))
      (error (message "%s" (error-message-string err))
             (when display
               (pop-to-buffer errbuf))))))


;;;###autoload
(define-minor-mode blacken-mode
  "Automatically run black before saving."
  :lighter " Black"
  (if blacken-mode
      (add-hook 'before-save-hook 'blacken-buffer nil t)
    (remove-hook 'before-save-hook 'blacken-buffer t)))

(provide 'blacken)

;;; blacken.el ends here
