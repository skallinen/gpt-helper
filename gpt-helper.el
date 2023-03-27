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

(defun ;selection-prompt (model system prompt code-selection callback)
    (let ((url "https://api.openai.com/v1/chat/completions")
          (headers `(("Content-Type" . "application/json")
                     ("Authorization" . ,(concat "Bearer " open-ai-key))))
          (payload (json-encode
                    `((model . ,model;"gpt-4";
			     )
                      (messages . [((role . "system") (content . ,system))
                                   ((role . "user") (content . ,prompt))
                                   ((role . "assistant") (content . "I can try to help, what is the text or code?"))
                                   ((role . "user") (content . ,code-selection))])))))
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
    
    (message "GPT API call triggered"))

(defun general-prompt (model system prompt callback)
  (let ((url "https://api.openai.com/v1/chat/completions")
        (headers `(("Content-Type" . "application/json")
                   ("Authorization" . ,(concat "Bearer " open-ai-key))))
        (payload (json-encode
                  `((model . ,model	;"gpt-4";
			   )
		    (messages . [((role . "system") (content . ,system))
                                 ((role . "user") (content . ,(concat "How do I " prompt)))])))))
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
                (message "Error calling GPT API: %S" error-thrown))))
    )
  
  (message "GPT API call triggered"))



(defun get-prompt ()
  (read-from-minibuffer "Enter the prompt: "))



(defun get-code-selection ()
  (if (region-active-p)
      (concat "```"
              (buffer-substring-no-properties (region-beginning)
                                              (region-end))
              "```")
    ""))

(defun display-gpt-response (content)
  (with-current-buffer (get-buffer-create "*GPT-3.5/4 Output*")
    (setq buffer-read-only nil)
    (goto-char (point-max))
    (insert (concat "\n" content "\n"
		    (make-string 80 ?-) "\n\n"))
    (local-set-key "q" 'kill-buffer-and-window)
    (setq buffer-read-only t)
    (display-buffer (current-buffer)))
  (when (and content (not (string-empty-p content)))
    (kill-new content))
  (message "GPT call ready. Result also saved in kill ring"))

(defun gpt-4-programmer-region ()
  (interactive)
  (let ((prompt (get-prompt))
	(code-selection (get-code-selection)))
    (selection-prompt "gpt-4"
		      "You are a programmer."
		      prompt
		      code-selection
		      #'display-gpt-response)))

(defun chat-gpt-programmer-region ()
  (interactive)
  (let ((prompt (get-prompt))
	(code-selection (get-code-selection)))
    (selection-prompt "gpt-3.5-turbo"
		      "You are a programmer."
		      prompt
		      code-selection
		      #'display-gpt-response)))

(defun chat-gpt-assistant-region ()
  (interactive)
  (let ((prompt (get-prompt))
	(code-selection (get-code-selection)))
    (selection-prompt "gpt-3.5-turbo"
		      "You are an all around assistant!"
		      prompt
		      code-selection
		      #'display-gpt-response)))

(defun gpt-4-assistant-region ()
  (interactive)
  (let ((prompt (get-prompt))
	(code-selection (get-code-selection)))
    (selection-prompt "gpt-3.5-turbo"
                      "You are an all around assistant!"
		      prompt
		      code-selection
		      #'display-gpt-response)))

(defun emacs-how-do-i ()
  (interactive)
  (let ((prompt (get-prompt)))
    (general-prompt "gpt-3.5-turbo"
		    "You are an emacs tutor."
		    prompt
		    #'display-gpt-response)))


(defun gpt-3-assistant ()
  (interactive)
  (let ((prompt (get-prompt)))
    (general-prompt "gpt-3.5-turbo"
		    "You are an all around assistant!"
		    prompt
		    #'display-gpt-response)))


(defun gpt-4-assistant ()
  (interactive)
  (let ((prompt (get-prompt)))
    (general-prompt "gpt-4"
		    "You are an all around assistant!"
		    prompt
		    #'display-gpt-response)))

(provide 'gpt-helper)

;;; gpt-helper.el ends here
g
