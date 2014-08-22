;;; codic.el --- Search Codic (codic.jp) naming dictionaries

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-codic
;; Version: 0.02
;; Package-Requires: ((cl-lib "0.5"))

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

(require 'cl-lib)

(defgroup codic nil
  "`codic' for Emacs."
  :group 'applications)

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

(defvar codic--history nil)

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

;;;###autoload
(defun codic (keyword &optional limit)
  "Search `keyword' by Codic dictionary."
  (interactive
   (list (read-string "Keyword: " nil 'codic--history)))
  (when (or (not (stringp keyword)) (string= keyword ""))
    (error "Error: input is empty or invalid"))
  (let ((items (codic--search keyword (or limit codic-limit))))
    (codic--view items)))

(provide 'codic)

;;; codic.el ends here
