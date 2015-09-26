;;; codic.el --- Search Codic (codic.jp) naming dictionaries -*- lexical-binding: t; -*-

;; Copyright (C) 2015 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-codic
;; Version: 0.03
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Codic(http://codic.jp/) is naming dictionary service for Japanese programmers
;; and system engineers. This service is useful for naming class names, method names,
;; variables name database column etc. This package provides commands which search
;; from Codic dictionaries by Japanese and English.
;;
;; This package Emacs port of [Codic.vim](https://github.com/koron/codic-vim)

;;; Code:

(eval-when-compile
  (defvar url-http-end-of-headers))

(require 'cl-lib)
(require 'url)
(require 'json)

(defgroup codic nil
  "`codic' for Emacs."
  :group 'applications)

(defcustom codic-api-token nil
  "API token"
  :type '(choice (const nil) string)
  :group 'codic)

(defcustom codic-limit 10
  "Limit number of candidates."
  :type 'integer
  :group 'codic)

(defvar codic--dictionary-path
  (file-name-as-directory
   (concat (if load-file-name
               (file-name-directory load-file-name)
             default-directory) "dict")))

(defvar codic--dictionary-cache nil)
(defvar codic--history nil)

(defsubst codic--entry-dictionary-path (type)
  (concat codic--dictionary-path
          (symbol-name type)
          "-entry.csv"))

(defsubst codic--translation-dictionary-path (type)
  (concat codic--dictionary-path
          (symbol-name type)
          "-translation.csv"))

(defun codic--load-csv (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (cl-loop with entries = nil
             while (not (eobp))
             for start = (1+ (point))
             do
             (progn
               (while (re-search-forward "\015$" (line-end-position) t)
                 (forward-line 1))
               (let* ((line (buffer-substring-no-properties
                             start (1- (line-end-position))))
                      (cols (split-string line "\",\"")))
                 (push cols entries)
                 (forward-line 1)))

             finally
             return (nreverse entries))))

(defun codic--to-hash (entries)
  (let ((hash (make-hash-table :test 'equal)))
    (cl-loop for entry in entries
             for id = (car entry)
             do
             (let ((val (gethash id hash)))
               (puthash id (cons (cdr entry) val) hash)))
    hash))

(defun codic--map-function (type)
  (cl-case type
    (english (lambda (x) (list :word (nth 3 x) :desc (nth 4 x))))
    (naming (lambda (x) (list :word (nth 2 x) :desc (nth 3 x))))))

(defun codic--load-dictionary (type)
  (or (assoc-default type codic--dictionary-cache)
      (let* ((mapfn (codic--map-function type))
             (entry-dict (codic--entry-dictionary-path type))
             (trans-dict (codic--translation-dictionary-path type))
             (entries (codic--load-csv entry-dict))
             (data (codic--to-hash (codic--load-csv trans-dict)))
             (dict (make-hash-table :test 'equal)))
        (cl-loop for entry in entries
                 for id = (cl-first entry)
                 for label = (cl-second entry)
                 for values = (gethash id data)
                 for mapped-values = nil
                 when values
                 do
                 (progn
                   (cl-loop for value in values
                            for word-desc = (funcall mapfn value)
                            when word-desc
                            do
                            (push word-desc mapped-values))
                   (puthash label (list :id id :label label :values mapped-values)
                            dict)))
        (add-to-list 'codic--dictionary-cache (cons type dict))
        dict)))

(defun codic--sort-by-score (a b)
  (let ((delta (- (plist-get a :score) (plist-get b :score))))
    (or (< delta 0)
        (and (zerop delta)
             (<= (length (plist-get a :key)) (length (plist-get b :key)))))))

(defun codic--find (dictionary keyword limit)
  (cl-loop for key being the hash-keys of dictionary
           for val = (gethash key dictionary)
           when (string-match keyword key)
           collect (list :score (match-beginning 0) :key key :item val) into items
           finally
           return (let ((sorted (sort items 'codic--sort-by-score)))
                    (cl-loop repeat limit
                             for item in sorted
                             collect (plist-get item :item)))))

(defsubst codic--dictionary-type (keyword)
  (if (string-match "\\`[a-zA-Z_]+\\'" keyword)
      'english
    'naming))

(defun codic--search (keyword limit)
  (let* ((type (codic--dictionary-type keyword))
         (dict (codic--load-dictionary type))
         (items (codic--find dict keyword limit)))
    (when (zerop (length items))
      (error "No Result: '%s'" keyword))
    items))

(defun codic--view (items)
  (with-current-buffer (get-buffer-create "*Codic Result*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (cl-loop for item in items
             for label = (plist-get item :label)
             do
             (progn
               (insert (format "[%s]\n" label))
               (cl-loop for value in (plist-get item :values)
                        for word = (plist-get value :word)
                        for desc = (plist-get value :desc)
                        do
                        (insert (format " * %s %s\n" word desc)))
               (insert "\n")))
    (goto-char (point-min))
    (setq buffer-read-only t)
    (pop-to-buffer (current-buffer))))

(defun codic--render-response (&rest _unused)
  (let* ((json (json-read-from-string
                (buffer-substring-no-properties url-http-end-of-headers (point-max))))
         (res (aref json 0)))
    (with-current-buffer (get-buffer-create "*Codic Result*")
      (setq buffer-read-only nil)
      (erase-buffer)
      (let ((orig (assoc-default 'text res))
            (orig-translated (assoc-default 'translated_text res))
            (words (assoc-default 'words res)))
        (unless (= (length words) 1)
          (insert (format "[%s]\n" orig))
          (insert (format " * %s\n" orig-translated))
          (insert "\n"))
        (cl-loop for word across words
                 for text = (assoc-default 'text word)
                 for candidates = (assoc-default 'candidates word)
                 do
                 (progn
                   (insert (format "[%s]\n" text))
                   (cl-loop for candidate across candidates
                            for w = (assoc-default 'text candidate)
                            do
                            (insert (format " * %s\n" w)))
                   (insert "\n"))))
      (delete-trailing-whitespace)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(defun codic--request (keyword)
  (let ((url-request-method "GET")
        (url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer " codic-api-token))))
        (url (format "https://api.codic.jp/v1/engine/translate.json?text=%s"
                     (url-encode-url keyword))))
    (url-retrieve url #'codic--render-response nil t)))

(defun codic--read-keyword ()
  (let ((default (when (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end)))))
    (read-string "Keyword: " default 'codic--history)))

;;;###autoload
(defun codic (keyword &optional limit)
  "Search `keyword' from Codic dictionary."
  (interactive
   (list (codic--read-keyword)))
  (when (or (not (stringp keyword)) (string= keyword ""))
    (error "Error: input is empty or invalid"))
  (let ((items (codic--search keyword (or limit codic-limit))))
    (codic--view items)))

;;;###autoload
(defun codic-translate (keyword)
  "Search `keyword' from Codic translate Web API"
  (interactive
   (list (codic--read-keyword)))
  (when (or (not (stringp keyword)) (string= keyword ""))
    (error "Error: input is empty or invalid"))
  (unless codic-api-token
    (error "Error: 'codic-api-token' is not set"))
  (codic--request keyword))

(provide 'codic)

;;; codic.el ends here
