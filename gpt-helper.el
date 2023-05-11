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
  "Get the value of environment variable NAME or return DEFAULT if it is not set."
  (or (getenv name) default))

(defvar open-ai-key (getenv-or-default "OPENAI_API_KEY" "")
  "API key for the OpenAI API. If not set, use an empty string as a default.")

(defun build-api-url ()
  "Return the OpenAI API URL."
  "https://api.openai.com/v1/chat/completions")

(defun build-headers ()
  "Build the headers needed for the request with OpenAI API key."
  `(("Content-Type" . "application/json")
    ("Authorization" . ,(concat "Bearer " open-ai-key))))

(defun build-payload (model system prompt region)
  "Build the payload for OpenAI API request with MODEL, SYSTEM, PROMPT, and REGION."
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
  "Send a request to the GPT API with the given URL, HEADERS, PAYLOAD, and CALLBACK function. The CALLBACK function should accept a single argument: the content of the response. If an error occurs during the API call, an error message is printed."
  ;; Notify the user
  (message "Attempting to call OPEN AI API, please wait...")

  ;; Make an HTTP request to the GPT API
  (request url
    :type "POST"
    :headers headers
    :data payload
    :sync nil
    :parser 'json-read

    :success (cl-function
              (lambda (&key data &allow-other-keys)
                "Process a successful GPT API call and execute CALLBACK with the response content."
                (let* ((choices (assoc-default 'choices data))
                       (first-choice (elt choices 0))
                       (message (assoc-default 'message first-choice))
                       (content (assoc-default 'content message)))
                  (when content
                    (funcall callback content)))))

    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
                "Handle an error in the GPT API call by printing an error message."
              (message "Error calling GPT API: %S" error-thrown)))))

(defun display-gpt-response (content prompt)
  "Display the output of a GPT call in a user-friendly way and provide an easy way to copy the output to the clipboard."
  (with-current-buffer (get-buffer-create "*GPT Output*")
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
          (kill-ring-save (point-min) (point-max)))))))

(defun show-gpt-completion-message ()
  "Show a completion message in the minibuffer."
  (message "GPT call ready. Results displayed in *GPT Output* buffer."))

(defun remove-backticks (str)
  "Remove all occurrences of ``` from STR."
  (let ((result (replace-regexp-in-string "```" "" str)))
    result))

(defun remove-wrapping-double-quotes (str)
  "Remove wrapping double quotes from STR if present."
  (if (and (> (length str) 1)
           (eq (aref str 0) ?\")
           (eq (aref str (- (length str) 1)) ?\"))
      (substring str 1 (- (length str) 1))
    str))

(defun replace-gpt-region (text)
  "Replace the currently selected region with the response TEXT."
  (when (region-active-p)
    (delete-region (region-beginning) (region-end))
    (insert (remove-backticks text))))

(defun gpt-request (system &optional prompt region replace-region model)
  "Send a request to GPT API with MODEL, SYSTEM, PROMPT, and optional REGION."
  (interactive
   (list (read-string "Enter the role: ")
         nil
         nil
         nil
         nil))

  (when (not replace-region)
    (setq replace-region (if (y-or-n-p "Do you want to replace the region? ")
			       :replace
			     :dont-replace
			     )))

  (unless region
    (setq region (when (region-active-p)
		   (concat "```"
			   (buffer-substring-no-properties (region-beginning)
							   (region-end))
			   "```"))))
  
  (unless model
    (setq model (completing-read "Select the model 2 (gpt-3.5-turbo or gpt-4): " '("gpt-3.5-turbo" "gpt-4"))))
  (unless prompt
    (setq prompt (read-string "Enter the prompt: ")))
  (let ((url (build-api-url))
        (headers (build-headers))
        (payload (build-payload model system prompt region)))
    (request-gpt-response url headers payload
                          (lambda (content)
                            (if (equal replace-region :replace)
				(progn
                                 (remove-wrapping-double-quotes (replace-gpt-region content))
				 (message "Region replaced with GPT result")
				 )
                              (progn
				(display-gpt-response content prompt)
				(show-gpt-completion-message)))))))



(defun gpt-programmer-default ()
  (interactive)
  (gpt-request "programmer" nil nil :dont-replace "gpt-4"))

(global-set-key (kbd "C-c C-g C-p") 'gpt-programmer-default)

(defun gpt-programmer ()
  (interactive)
  (gpt-request "programmer" nil nil nil nil))

(global-set-key (kbd "C-c C-g C-S-p") 'gpt-programmer)



(defun gpt-assistant-default ()
  (interactive)
  (gpt-request "assistant" nil nil :dont-replace nil))

(global-set-key (kbd "C-c C-g C-a") 'gpt-assistant-defalt)

(defun gpt-assistant ()
  (interactive)
  (gpt-request "assistant" nil nil nil nil))

(global-set-key (kbd "C-c C-g C-S-a") 'gpt-assistant)



(defun gpt-to-idiomatic-english ()
  (interactive)
  (gpt-request "copy editor and translator" "Correct and or translate the following to idiomatic English, plsease respons without context. do not wrap in quotes" nil :replace "gpt-3.5-turbo"))

(global-set-key (kbd "C-c C-g C-e") 'gpt-to-idiomatic-english)

(defun gpt-to-idiomatic-swedish ()
  (interactive)
  (gpt-request "korrekturläsare och översättare" "Korrigera och/eller översätt följande till bra idiomatisk svenska, vänligen svara utan förklaringar. Använd inte citattecken" nil :replace "gpt-3.5-turbo"))

(global-set-key (kbd "C-c C-g C-s") 'gpt-to-idiomatic-swedish)

(defun gpt-to-idiomatic-finnish ()
  (interactive)
  (gpt-request "kielihuotaja ja kääntäjä" "Korjaa ja/tai käännä seuraava hyväksi suomeksi, ole hyvä ja vastaa ilman selityksiä. Älä käytä lainausmerkkejä" nil :replace "gpt-3.5-turbo"))

(global-set-key (kbd "C-c C-g C-f") 'gpt-to-idiomatic-finnish)


(defun gpt-translate-to-english ()
  (interactive)
  (gpt-request "copy editor and translator" "Correct and or translate the following to idiomatic English, plsease respons without context. do not wrap in quotes" nil nil "gpt-3.5-turbo"))

(global-set-key (kbd "C-c C-g C-t") 'gpt-to-idiomatic-english)

;; todo
;; 'gpt-smarter-explain-like-i-was-13
;; 'gpt-smarter-explain-code
;; 'gpt-smarter-improve-code
;; 'gpt-smarter-insert-docstring

(add-hook 'markdown-mode-hook 'visual-line-mode)

(provide 'gpt-helper)

;;; gpt-helper.el ends here













