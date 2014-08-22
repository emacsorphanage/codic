;;; test-codic.el --- Unit test for codic.el

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>

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

;;; Code:

(require 'ert)
(require 'codic)

(ert-deftest dictionary-type ()
  "Choose dictionary type from `keyword'"
  (let ((got (codic--dictionary-type "りんご")))
    (should (eq got 'naming)))
  (let ((got (codic--dictionary-type "apple")))
    (should (eq got 'english)))
  (let ((got (codic--dictionary-type "appleりんご")))
    (should (eq got 'naming))))

(ert-deftest load-csv-english-entries ()
  "Load English entries CSV"
  (let* ((dict (codic--entry-dictionary-path 'english))
         (got (codic--load-csv dict)))
    (let ((entry (car got)))
      (should (= (length entry) 7))
      (should (string= (cl-first entry) "48978"))
      (should (string= (cl-second entry) "-based")))))

(ert-deftest load-csv-naming-entries ()
  "Load Naming entries CSV"
  (let* ((dict (codic--entry-dictionary-path 'naming))
         (got (codic--load-csv dict)))
    (let ((entry (car got)))
      (should entry)
      (should (= (length entry) 2))
      (should (string= (cl-first entry) "40719"))
      (should (string= (cl-second entry) "減少")))))

(ert-deftest english-translation-hash ()
  "Construct English translation hash"
  (let* ((dict (codic--translation-dictionary-path 'english))
         (data (codic--load-csv dict))
         (dict (codic--to-hash data)))
    (let ((entry (gethash "3070" dict)))
      (should (= (length entry) 3))
      (let ((labels (mapcar 'car entry)))
        (should (equal labels '("393" "392" "391")))))

    (let ((entry (gethash "52381" dict)))
      (should (= (length entry) 1)))))

(ert-deftest naming-translation-hash ()
  "Construct Naming translation hash"
  (let* ((dict (codic--translation-dictionary-path 'naming))
         (data (codic--load-csv dict))
         (dict (codic--to-hash data)))
    (let ((entry (gethash "40719" dict)))
      (should entry)
      (should (= (length entry) 1))
      (should (string= (caar entry) "59608")))

    (let ((entry (car (gethash "41255" dict))))
      (should entry)
      (should (string-match-p "en\\.wikipedia\\.org/wiki/Order_by" (nth 3 entry))))))

(ert-deftest load-english-dictionary ()
  "Load English dictionary"
  (let ((dictionary (codic--load-dictionary 'english)))
    (let ((entry (gethash "editor" dictionary)))
      (should entry)
      (should (string= (plist-get entry :id) "40927"))
      (should (string= (plist-get entry :label) "editor") )
      (should (= (length (plist-get entry :values)) 2))
      (should (assoc-default 'english codic--dictionary-cache)))))

(ert-deftest load-naming-dictionary ()
  "Load Naming dictionary"
  (let ((dictionary (codic--load-dictionary 'naming)))
    (let ((entry (gethash "エディター" dictionary)))
      (should entry)
      (let ((val (plist-get entry :values)))
        (should (string= (plist-get (car val) :word) "editor"))
        (should (assoc-default 'naming codic--dictionary-cache))))))

(ert-deftest sort-by-score ()
  "Sort by score."
  (let* ((data '((:score 10 :key "apple" :name a)
                 (:score 10 :key "applejuice" :name b)
                 (:score 20 :key "orange" :name c)
                 (:score 3 :key "melon" :name d)))
         (got (sort data 'codic--sort-by-score))
         (names (mapcar (lambda (x) (plist-get x :name)) got)))
    (should (equal names '(d a b c)))))

;;; test-codic.el ends here
