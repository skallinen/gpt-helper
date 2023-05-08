;;; gpt-helper.el --- Functions to for GPT-4 and GPT-3.5-TURBO (ChatGPT) -*- lexical-binding: t -*-

;; Copyright (C) 1710-2023 Sami Kallinen

;; Author: Sami sakalli Kallinen <notjustsilicon@gmail.com>
;; Version: 0.1
;; Package-Requires: ((request "0.3.2"))
;; Keywords: LLM developer-tools
;; URL: https://github.com/
;; Created: 21 Mar 2023
;; URL: https://github.com/skallinen/gpt-helper
;;; Commentary:
;;
;; This is a minimal collection of functions to help with making GPT calls in
;; emacs. You will need an account on Open AI and add your key to
;; `OPENAI_API_KEY` env variable it prints result into a GPT buffer
;; and you can choose between ChatGPT and GPT-4 models.

;;; Code:

(require 'json)
(require 'request)
(require 'cl-lib)
;;(setf lexical-binding t)

(defun getenv-or-default (name default)
  (or (getenv name) default))

(defvar open-ai-key (getenv-or-default "OPENAI_API_KEY" ""))

(defun build-api-url ()
  "https://api.openai.com/v1/chat/completions")

(defun build-headers ()
  `(("Content-Type" . "application/json")
    ("Authorization" . ,(concat "Bearer " open-ai-key))))

(defun build-payload (model system prompt region)
  (json-encode
   `((model . ,model)
     (messages . ,(if region
		      `[((role . "system") (content . ,system))
			((role . "user") (content . ,prompt))
			((role . "assistant") (content . "I can try to help, what is the text or code?"))
			((role . "user") (content . ,region))]
		    `[((role . "system") (content . ,system))
		       ((role . "user") (content . ,(concat "How do I " prompt)))])))))

(defun request-gpt-response (url headers payload callback)
  (request url
    :type "POST"
    :headers headers
    :data payload
    :sync nil
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(let* ((choices (cdr (assoc 'choices data)))
		       (first-choice (elt choices 0))
		       (message (cdr (assoc 'message first-choice)))
		       (content (cdr (assoc 'content message))))
		  (funcall callback content)
		  (message "Request has been sent to OPEN AI API, waiting for a response..."))))
    :error (cl-function
	    (lambda (&key error-thrown &allow-other-keys)
	      (message "Error calling GPT API: %S" error-thrown)))))

(defun gpt-request (model system prompt code-selection callback)
  "Sends a request to GPT API with the given MODEL, SYSTEM, PROMPT, and CODE-SELECTION."
  (let ((url (build-api-url))
        (headers (build-headers))
        (payload (build-payload model system prompt code-selection)))
    (request-gpt-response url headers payload (lambda (content) (funcall callback content prompt)))))

(defun get-prompt ()
  (read-from-minibuffer "Enter the prompt: "))

(defun get-region ()
  (if (region-active-p)
      (concat "```"
              (buffer-substring-no-properties (region-beginning)
                                              (region-end))
              "```")
    ""))

(defun display-gpt-response (content prompt)
  "Designed to display the output of a GPT-3.5/4 call in a
  user-friendly way and provide an easy way to copy the output to the
  clipboard."

  (with-current-buffer (get-buffer-create "*GPT-3.5/4 Output*")
    (setq buffer-read-only nil)
    (goto-char (point-max))
    (let ((start-pos (point)))
      (insert (concat "\n" "Prompt: "
	              prompt "\n"
	              (make-string 10 ?-) "\n\n"
		      content "\n"
		      (make-string 80 ?-) "\n\n"))
      (local-set-key "q" 'kill-buffer-and-window)
      (display-buffer (current-buffer))
      ;; Enable markdown-mode for better formatting of the GPT response content
      (markdown-mode)
      (setq buffer-read-only t)
      (when (and content (not (string-empty-p content)))
        (with-temp-buffer
          (insert content)
          (kill-ring-save (point-min) (point-max)))
        (message "GPT call ready. Result also saved in kill ring")))))


(defmacro define-gpt-call-function (name model system)
  "Create an interactive GPT request function with the given NAME, MODEL, and SYSTEM."
  `(defun ,name ()
     (interactive)
     (let ((prompt (get-prompt))
           (code-selection (get-region)))
       (gpt-request ,model ,system prompt code-selection #'display-gpt-response))))

(define-gpt-call-function gpt-smarter-programmer-region "gpt-4" "You are a programmer.")
(define-gpt-call-function gpt-faster-programmer-region "gpt-3.5-turbo" "You are a programmer.")
(define-gpt-call-function gpt-faster-assistant-region "gpt-3.5-turbo" "You are an all-around assistant!")
(define-gpt-call-function gpt-smarter-assistant-region "gpt-4" "You are an all-around assistant!")
(define-gpt-call-function emacs-how-do-i "gpt-3.5-turbo" "You are an Emacs tutor.")
(define-gpt-call-function gpt-faster-general "gpt-3.5-turbo" "You are an all-around assistant!")
(define-gpt-call-function gpt-smarter-general "gpt-4" "You are an all-around assistant!")

(defun replace-gpt-region (text)
  "Replace the currently selected region with the response TEXT."
  (when (region-active-p)
    (delete-region (region-beginning) (region-end))
    (insert text)))

(defun gpt-process-region (model role instruction)
  (let ((code-selection (buffer-substring-no-properties (region-beginning) (region-end))))
    (gpt-request model
                 (concat "You are an AI " role)
                 (concat instruction ", only return the output without explanations and do not add quotes around your response:")
                 code-selection
                 (lambda (content prompt)
                   (display-gpt-response content prompt)
                   (replace-gpt-region content)))))

(defun gpt-faster-process-region-to-eng ()
  (interactive)
  (gpt-process-region "gpt-3.5-turbo" "translator to English" "Translate the following"))

(defun gpt-faster-process-region-to-swe ()
  (interactive)
  (gpt-process-region "gpt-3.5-turbo" "translator to Swedish" "Translate the following"))

(defun gpt-faster-process-region-to-fi ()
  (interactive)
  (gpt-process-region "gpt-3.5-turbo" "translator to Finnish" "Translate the following"))

(defun gpt-faster-correct-region-to-idiomatic-english ()
  (interactive)
  (gpt-process-region "gpt-3.5-turbo" "copyrwriter and editor" "Correct the following to idiomatic English"))

(defun gpt-smarter-correct-region-to-idiomatic-english ()
  (interactive)
  (gpt-process-region "gpt-smarter-turbo" "copyrwriter and editor" "Correct the following to idiomatic English"))

(defun gpt-faster-insert-docstring ()
  (interactive)
  (gpt-process-region "gpt-3.5-turbo" "programmer" "Return the function verbatim, but insert a concise and clear docstring fort the function"))

(defun gpt-smarter-insert-docstring ()
  (interactive)
  (gpt-process-region "gpt-4" "programmer" "Return the function verbatim, but insert a concise and clear docstring for the function"))

(defun gpt-predefined-region-questions (model role instruction)
  (let ((code-selection (buffer-substring-no-properties (region-beginning) (region-end))))
    (gpt-request model
                 (concat "You are an AI " role)
                 instruction
                 code-selection
                 (lambda (content prompt)
                   (display-gpt-response content prompt)))))

(defun gpt-faster-explain-like-i-was-13 ()
  (interactive)
  (gpt-predefined-region-questions "gpt-3.5-turbo" "that is good at explaining" "Explain the following like I was 13 year old"))

(defun gpt-smarter-explain-like-i-was-13 ()
  (interactive)
  (gpt-predefined-region-questions "gpt-4" "that is good at explaining" "Explain the following like I was 13 year old"))

(defun gpt-smarter-explain-code ()
  (interactive)
  (gpt-predefined-region-questions "gpt-4" "programmer that can verbalizbize well" "Explain the following code"))

(defun gpt-smarter-improve-code ()
  (interactive)
  (gpt-predefined-region-questions "gpt-4" "programmer that can verbalizbize well" "Improve the follwoing code"))

(defun gpt-faster-translate-to-eng-in-buffer ()
  (interactive)
  (gpt-predefined-region-questions "gpt-3.5-turbo" "English translator" "Translate the following to english"))

(defun gpt-smarter-translate-to-eng-in-buffer ()
  (interactive)
  (gpt-predefined-region-questions "gpt-4" "English translator" "Translate the following to english"))

(global-set-key (kbd "C-c C-g C-g") 'gpt-smarter-general)
(global-set-key (kbd "C-c C-g C-S-g") 'gpt-faster-general)
(global-set-key (kbd "C-c C-g C-p") 'gpt-smarter-programmer-region)
(global-set-key (kbd "C-c C-g C-S-p") 'gpt-faster-programmer-region)
(global-set-key (kbd "C-c C-g C-a") 'gpt-smarter-assistant-region)
(global-set-key (kbd "C-c C-g C-S-a") 'gpt-faster-assistant-region)
(global-set-key (kbd "C-c C-g C-e") 'gpt-faster-process-region-to-eng)
(global-set-key (kbd "C-c C-g C-s") 'gpt-faster-process-region-to-swe)
(global-set-key (kbd "C-c C-g C-f") 'gpt-faster-process-region-to-fi)
(global-set-key (kbd "C-c C-g C-t") 'gpt-faster-translate-to-eng-in-buffer)
(global-set-key (kbd "C-c C-g C-S-t") 'gpt-smarter-translate-to-eng-in-buffer)
(global-set-key (kbd "C-c C-g C-x") 'gpt-faster-explain-like-i-was-13)
(global-set-key (kbd "C-c C-g C-S-x") 'gpt-smarter-explain-like-i-was-13)
(global-set-key (kbd "C-c C-g C-n") 'gpt-faster-correct-region-to-idiomatic-english)
(global-set-key (kbd "C-c C-g C-S-n") 'gpt-smarter-correct-region-to-idiomatic-english)
(global-set-key (kbd "C-c C-g C-S-c") 'gpt-smarter-explain-code)
(global-set-key (kbd "C-c C-g C-S-b") 'gpt-smarter-improve-code)
(global-set-key (kbd "C-c C-g C-d") 'gpt-faster-insert-docstring)
(global-set-key (kbd "C-c C-g C-S-d") 'gpt-smarter-insert-docstring)

(provide 'gpt-helper)

;;; gpt-helper.el ends here
