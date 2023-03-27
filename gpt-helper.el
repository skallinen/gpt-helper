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
;; This is a minal collection of functions to help doing GPT calls in
;; emacs. You will need an account on Open AI and add your key to
;; `OPENAI_API_KEY` env variable it prints result into a GPT buffer
;; and you can choose between ChatGPT and GPT-4 models.

;;; Code:

(require 'json)
(require 'request)

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
		  (funcall callback content))))
    :error (cl-function
	    (lambda (&key error-thrown &allow-other-keys)
	      (message "Error calling GPT API: %S" error-thrown)))))

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
		  (funcall callback content))))
    :error (cl-function
	    (lambda (&key error-thrown &allow-other-keys)
	      (message "Error calling GPT API: %S" error-thrown)))))

(defun get-prompt ()
  (read-from-minibuffer "Enter the prompt: "))

(defun get-region ()
  (if (region-active-p)
      (concat "```"
              (buffer-substring-no-properties (region-beginning)
                                              (region-end))
              "```")
    ""))

(defun display-gpt-response (content)
  "Designed to display the output of a GPT-3.5/4 call in a
  user-friendly way and provide an easy way to copy the output to the
  clipboard."

  (with-current-buffer (get-buffer-create "*GPT-3.5/4 Output*")
    (setq buffer-read-only nil)
    (goto-char (point-max))
    (insert (concat "\n" content "\n"
		    (make-string 80 ?-) "\n\n"))
    (local-set-key "q" 'kill-buffer-and-window)
    (setq buffer-read-only t)
    (display-buffer (current-buffer))
    (markdown-mode)
    (when (and content (not (string-empty-p content)))
      (with-temp-buffer
        (insert content)
        (kill-ring-save (point-min) (point-max)))
      (message "GPT call refady. Result also saved in kill ring"))))

(defmacro define-gpt-call-function (name model system)
  `(defun ,name ()
     (interactive)
     (let ((prompt (get-prompt))
           (code-selection (get-region)))
       (gpt-request ,model ,system prompt code-selection #'display-gpt-response))))

(define-gpt-call-function gpt-4-programmer-region "gpt-4" "You are a programmer.")
(define-gpt-call-function chat-gpt-programmer-region "gpt-3.5-turbo" "You are a programmer.")
(define-gpt-call-function chat-gpt-assistant-region "gpt-3.5-turbo" "You are an all-around assistant!")
(define-gpt-call-function gpt-4-assistant-region "gpt-4" "You are an all-around assistant!")
(define-gpt-call-function emacs-how-do-i "gpt-3.5-turbo" "You are an Emacs tutor.")
(define-gpt-call-function gpt-3-general "gpt-3.5-turbo" "You are an all-around assistant!")
(define-gpt-call-function gpt-4-general "gpt-4" "You are an all-around assistant!")

(provide 'gpt-helper)

;;; gpt-helper.el ends here
