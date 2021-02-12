;;; scalpel.el --- Tools for interactive face operations  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  D. Williams

;; Author: D. Williams <d.williams@posteo.net>
;; Keywords: faces, maint
;; Version: 0.4.0
;; Homepage: https://github.com/integral-dw/scalpel
;; Package-Requires: ((emacs "25.1"))


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Manipulate faces interactively in a given buffer.  The point of
;; this package is to manipulate text properties of a buffer to debug
;; cosmetic modes.  Currently there are no plans of putting it on
;; MELPA.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defvar-local scalpel-anonymous-face nil
  "The most recent anonymous face created in buffer.

Use ‘scalpel-create-face’ to create or edit this face
interactively.")


;;; Declarations
(declare-function string-empty-p "subr-x" (string))


;;; Property List Operations
(defun scalpel-plist-remq (plist prop)
  "Return a copy of PLIST with the property PROP removed.

PLIST must be a property list, which is a list of the form
\(PROP1 VALUE1 PROP2 VALUE2...).

The comparison with PROP is done using ‘eq’."
  (cl-loop for (property value) on plist by #'cddr
           unless (eq property prop) collect property and collect value))


;;; Interactive Boilerplate

(defun scalpel--generate-sample ()
  "Generate sample text using ‘scalpel-anonymous-face’.

The resulting string will be \"[sample] \", with the word
\"sample\" being formatted using the anonymous face, and
everything else being formatted using the face
‘minibuffer-prompt’."
  (let ((sample "[sample] "))
    (put-text-property 0 9 'face
                       'minibuffer-prompt
                       sample)
    (put-text-property 1 7 'face
                       scalpel-anonymous-face
                       sample)
    sample))

(defmacro scalpel--with-faceless-prompt (&rest body)
  "Execute BODY, allowing prompts to be formatted using nonstandard faces.

Within BODY, the current face property in
‘minibuffer-prompt-properties’ (see ‘minibuffer-prompt’) is
ignored."
  `(let ((minibuffer-prompt-properties minibuffer-prompt-properties))
     (setq minibuffer-prompt-properties
           (scalpel-plist-remq minibuffer-prompt-properties 'face))
     ,@body))

(defun scalpel--format-prompt (prompt &rest objects)
  "Format PROMPT with a preview of the current anonymous face.

PROMPT and OBJECTS correspond to the first and optional
argument(s) passed to ‘format’, respectively.

Use the resulting prompt string within
‘scalpel--with-faceless-prompt’ for the formatting to work
correctly See also ‘scalpel-anonymous-face’."
  (let ((prompt (apply #'format prompt objects)))
    (put-text-property 0 (length prompt) 'face 'minibuffer-prompt prompt)
    (concat (scalpel--generate-sample)
            prompt)))

(defun scalpel--read-minibuffer (prompt)
  "Read a string from the minibuffer using PROMPT.
If no input is provided, return nil."
  (let (value)
    (setq value (read-from-minibuffer prompt))
    (unless (string-empty-p value)
      value)))

(defun scalpel--read-face-property ()
  "Read a face property as a string from the minibuffer.
If no input is provided, return nil instead."
  (let (face-property
        (attributes
         (mapcar #'car (face-all-attributes 'default))))
    (setq face-property
          (scalpel--with-faceless-prompt
           (completing-read
            (scalpel--format-prompt "Face Attribute: ")
            attributes nil t)))
    (unless (string-empty-p face-property)
      face-property)))


;;; Interactive Commands

(defun scalpel-apply-face (start end &optional new-face)
  "Apply an existing face to the current region.

First two arguments START and END are positions specifying the
region.

Optional third argument NEW-FACE, if non-nil, is the face to
apply to the region.  If no face is provided, prompt for a face
name."
  (interactive "r")
  (let ((new-face (or new-face
                      (read-face-name "Insert face: " 'default))))
    (put-text-property start end 'face new-face)))

(defun scalpel-clear-anon-face ()
  "Clear ‘scalpel-anonymous-face’."
  (interactive)
  (setq scalpel-anonymous-face nil))

(defun scalpel-edit-anon-face ()
  "Manipulate the anonymous face ‘scalpel-anonymous-face’.

Continuously prompts for face properties to set them to.  Stop if
no property is selected.  Similarly, if no value is selected,
propmt for deleting the entry.  To set the value of a given
property to nil, explicitly enter nil.  To remove all properties
from the anonymous face, use ‘scalpel-clear-anon-face’."
  (interactive)
  (let ((face-property nil)
        (value nil))
     (while (setq face-property
                  (scalpel--read-face-property))
       (scalpel--with-faceless-prompt
        (setq value
              (scalpel--read-minibuffer
               (scalpel--format-prompt
                "Set value of ‘%s’ to: "
                face-property))))
       (setq face-property (read face-property))
       (cond (value
              (setq value (read value)
                    scalpel-anonymous-face
                    (plist-put scalpel-anonymous-face face-property value)))
             ((y-or-n-p (format "Remove property ‘%S’? " face-property))
              (setq scalpel-anonymous-face
                    (scalpel-plist-remq scalpel-anonymous-face
                                        face-property))))
    (message "Face set to ‘%S’" scalpel-anonymous-face))))

(defun scalpel-apply-anon-face (start end)
  "Apply ‘scalpel-anonymous-face’ to region.

First two arguments START and END are positions specifying the
region."
  (interactive "r")
  (scalpel-apply-face start end scalpel-anonymous-face))

(defun scalpel-add-text-propery (start end)
  "Add a text property in region.

First two arguments START and END are positions specifying the
region."
  (interactive "r")
  (when-let ((prop (scalpel--read-minibuffer "Text Property: "))
             (val (scalpel--read-minibuffer "Value: "))
             (proplist (list (read prop) (read val))))
    (add-text-properties start end proplist)
    (message "Property list set: %S" proplist)))

(provide 'scalpel)
;;; scalpel.el ends here
