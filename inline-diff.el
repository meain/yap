;;; inline-diff.el --- Diff in-place -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 TEC
;;
;; Author: TEC <contact@tecosaur.net>
;; Maintainer: TEC <contact@tecosaur.net>
;; Created: June 26, 2024
;; Modified: June 26, 2024
;; Version: 0.0.1
;; Keywords: convenience tools unix
;; Homepage: https://code.tecosaur.net/tec/inline-diff
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Diff in-place
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib))

;;;; Customisation

(defgroup inline-diff nil
  "Diff in-place."
  :link '(url-link :tag "Homepage" "https://code.tecosaur.net/tec/inline-diff")
  :link '(emacs-library-link :tag "Library Source" "inline-diff.el")
  :group 'convenience
  :prefix "inline-diff-")

(defcustom inline-diff-move-after-acting t
  "Whether to move to the next diff after acting on the current one."
  :type 'boolean)

(defcustom inline-diff-loop-around-scope t
  "Whether to loop around the buffer when reaching the end."
  :type 'boolean)

(defcustom inline-diff-really-insert nil
  "Whether to actually insert additions from a diff.
If nil, the additions will be displayed via empty overlays."
  :type 'boolean)

(defcustom inline-diff-change-joiner "→"
  "String to use to indicate joined changes."
  :type 'string)

(defcustom inline-diff-do-refinement t
  "Whether to refine diffs with finer changes."
  :type 'boolean)

(defcustom inline-diff-refine-similarity 0.7
  "The similarity threshold for refining diffs, between 0.0 and 1.0.

This is the proportion of characters that must remain in common between
the old and new versions in order for refinement highlighting to be shown."
  :type 'number)

;;;; Faces

(defface inline-diff-added
  '((t :inherit diff-added))
  "Face for added text in inline-diff.")

(defface inline-diff-removed
  '((t :inherit diff-removed))
  "Face for removed text in inline-diff.")

(defface inline-diff-refine-added
  '((t :inherit diff-refine-added))
  "Face for added text in inline-diff.")

(defface inline-diff-refine-removed
  '((t :inherit diff-refine-removed))
  "Face for removed text in inline-diff.")

;;;; Keymaps

(defvar-keymap inline-diff-overlay-map
  :doc "Keymap attached to misspelled words."
  "<down-mouse-3>" `(menu-item "" (keymap) :filter ,#'inline-diff-act)
  "M-n" #'inline-diff-next
  "M-p" #'inline-diff-previous
  "M-a" #'inline-diff-apply
  "M-k" #'inline-diff-reject
  "RET" #'inline-diff-act
  "<ret>" #'inline-diff-act)

(fset 'inline-diff-overlay-map inline-diff-overlay-map)

(defvar-keymap inline-diff-repeat-map
  :doc "Repeat keymap for navigation commands."
  :repeat (:exit (inline-diff-correct))
  "M-n" #'inline-diff-next
  "M-p" #'inline-diff-previous
  "M-a" #'inline-diff-apply
  "M-k" #'inline-diff-reject
  "n" #'inline-diff-next
  "p" #'inline-diff-previous
  "a" #'inline-diff-apply
  "x" #'inline-diff-reject
  "RET" #'inline-diff-act
  "<ret>" #'inline-diff-act)

;;;; Overlay properties

(put 'inline-diff-parent-overlay 'evaporate t)
(put 'inline-diff-overlay 'evaporate t)
(put 'inline-diff-refinement-overlay 'evaporate t)

(put 'inline-diff-overlay 'help-echo #'inline-diff--help-echo)
(put 'inline-diff-overlay 'keymap 'inline-diff-overlay-map)

;;;; Myers diff implementation

(cl-defun inline-diff--myers-diff (a b &key (test #'eql) (key #'identity))
  "Compute the shortest edit script that transforms sequence A into sequence B.

This uses the simple quadratic space form of Myers' algorithm to compute the
shortest edit script. Items to compare are extracted using the function KEY, and
equality checked using the function TEST."
  (let ((va (if (vectorp a) a (vconcat a)))
        (vb (if (vectorp b) b (vconcat b))))
    (inline-diff--merge-adjoining-indels
     (inline-diff--compactify
      (inline-diff--backtrack
       (inline-diff--myers-shortest-edit va vb :test test :key key) va vb)))))

(cl-defun inline-diff--myers-shortest-edit (a b &key (test #'eql) (key #'identity))
  "Compute the shortest edit that transforms sequence A into sequence B.

This uses the simple quadratic space form of Myers' algorithm to compute the
shortest edit script. Items to compare are extracted using the function KEY, and
equality checked using the function TEST.

Returns a reverse trace of state vectors that can be interpreted by
`myers-diff-backtrack'."
  (let* ((len-a (length a))
         (len-b (length b))
         (max-depth (+ len-a len-b))
         (len-v (1+ (* 2 max-depth)))
         (v (make-vector len-v 0))
         trace)
    (catch 'done
      (dotimes (depth (1+ max-depth))
        (push (copy-sequence v) trace)
        (dotimes (half-offset (1+ depth))
          (let* ((k (- (* half-offset 2) depth))
                 (vk-1 (aref v (mod (1- k) len-v)))
                 (vk+1 (aref v (mod (1+ k) len-v)))
                 (x (if (or (= k (- depth))
                            (and (/= k depth) (< vk-1 vk+1)))
                        vk+1 (1+ vk-1)))
                 (y (- x k)))
            ;; Extend the match along the diagonal
            (while (and (< x len-a) (< y len-b)
                        (funcall test
                                 (funcall key (elt a x))
                                 (funcall key (elt b y))))
              (setq x (1+ x) y (1+ y)))
            ;; Update the furthest reach for the current diagonal
            (aset v (mod k len-v) x)
            ;; If the end of both sequences is reached, backtrack to generate the edit script
            (when (and (>= x len-a) (>= y len-b))
              (throw 'done trace))))))))

(defun inline-diff--backtrack (reverse-trace a b)
  "Backtrack through REVERSE-TRACE to generate the edit script for A and B."
  (let* ((x (length a))
         (y (length b))
         (max-depth (+ (length a) (length b)))
         (len-v (1+ (* 2 max-depth)))
         (depth (1- (length reverse-trace)))
         edit-script)
    (while reverse-trace
      (let* ((v (car reverse-trace))
             (k (- x y))
             (vk-1 (aref v (mod (1- k) len-v)))
             (vk+1 (aref v (mod (1+ k) len-v)))
             (prev-k (if (or (= k (- depth))
                             (and (/= k depth) (< vk-1 vk+1)))
                         (1+ k) (1- k)))
             (prev-x (aref v (mod prev-k len-v)))
             (prev-y (- prev-x prev-k)))
        ;; Process diagonal matches
        (while (and (> x prev-x) (> y prev-y))
          (push (list 'match (elt a (1- x)) (elt b (1- y))) edit-script)
          (setq x (1- x) y (1- y)))
        ;; Process deletions and insertions
        (when (> depth 0)
          (cond
         ((eq x prev-x)
          (push (list 'insert (elt b (1- y))) edit-script))
         ((eq y prev-y)
          (push (list 'delete (elt a (1- x))) edit-script))))
        (setq x prev-x y prev-y))
      (setq reverse-trace (cdr reverse-trace)
            depth (1- depth)))
    edit-script))

(defun inline-diff--compactify (script)
  "Combine adjacent insertions and deletions in SCRIPT."
  (let (combined-script)
    (dolist (op script)
      (if (eq (car op) (caar combined-script))
          (push (cadr op) (cdar combined-script))
        (when (car combined-script)
          (setcdr (car combined-script)
                  (nreverse (cdar combined-script))))
        (push (cons (car op) (list (cadr op)))
              combined-script)))
    (when (car combined-script)
      (setcdr (car combined-script) (nreverse (cdar combined-script))))
    (nreverse combined-script)))

(defun inline-diff--merge-adjoining-indels (diff)
  "Merge whitespace-adjoined (delete insert) pairs in DIFF.
Specifically, (delete insert (match \" \") delete insert) will be transformed to (delete insert)."
  (let ((subdiff diff))
    (while subdiff
      (let ((head subdiff))
        (if (and (eq (caar head) 'delete)
                 (eq (caar (setq head (cdr head))) 'insert)
                 (eq (caar (setq head (cdr head))) 'match)
                 (and (= (length (cdar head)) 1)
                      (or (string= (cdadar head) " ")
                          (string-match-p (cdadar head) "\\`[ \t\n\r]+\\'")))
                 (eq (caar (setq head (cdr head))) 'delete)
                 (eq (caar (cdr head)) 'insert))
            (progn
              (setcdr (car subdiff)            ; Del 1
                      (append (cdar subdiff)   ; Del 1
                              (cdaddr subdiff) ; Match
                              (cdar head)))    ; Del 2
              (setcdr (cadr subdiff)           ; Ins 1
                      (append (cdadr subdiff)  ; Ins 1
                              (cdaddr subdiff) ; Match
                              (cdadr head)))   ; Ins 2
              (setcdr (cdr subdiff) (cddr head)))
          (setq subdiff (cdr subdiff)))))
    diff))

;;;; Internal functions

(defun inline-diff--word-positions (start stop)
  "Return a list of words with their positions in the region from START to STOP."
  (save-excursion
    (goto-char (or start (point-min)))
    (let ((pos (point))
          (stop (or stop (point-max)))
          entries)
      (while (and (<= (point) stop) (not (eobp)))
        (let ((syntax (syntax-class (syntax-after (point)))))
          (skip-syntax-forward (string (syntax-class-to-char syntax)))
          (if (memq syntax '(0 12)) ; Whitespace or comment-end/newline
              (push (cons pos " ") entries)
            (push (cons pos (buffer-substring-no-properties pos (point)))
                  entries))
          (setq pos (point))))
      (nreverse entries))))

(defun inline-diff--string-words (str &optional syntax-table)
  "Return a list of words in STR.
Use SYNTAX-TABLE as the current `syntax-table' if provided."
  (with-temp-buffer
    (insert str)
    (with-syntax-table (or syntax-table text-mode-syntax-table)
      (inline-diff--word-positions (point-min) (point-max)))))

(defun inline-diff--char-positions (start &optional stop)
  "Return a list of characters with their positions in the region START to STOP.
START can also be a string, in which case the positions are indices starting
with 1 or the value of STOP."
  (let (entries)
    (if (stringp start)
        (dotimes (n (length start))
          (push (cons (+ n (or stop 1)) (aref start n)) entries))
      (save-excursion
        (goto-char start)
        (while (and (<= (point) stop) (not (eobp)))
          (push (cons (point) (char-after (point))) entries)
          (forward-char 1))))
    (nreverse entries)))

(defun inline-diff--parent-overlay (start stop)
  "Create a parent overlay that spans from START to STOP."
  (let ((ov (make-overlay start stop)))
    (overlay-put ov 'category 'inline-diff-parent-overlay)
    (overlay-put ov 'children nil)
    ov))

(defun inline-diff--overlay-index (ov)
  "Return the index of the overlay OV."
  (let* ((siblings (overlay-get (overlay-get ov 'parent) 'children))
         (nprior 0))
    (dolist (s siblings)
      (when (< (overlay-start s) (overlay-start ov))
        (cl-incf nprior)))
    (cons (1+ nprior) (length siblings))))

(defun inline-diff--help-echo (_window object _pos)
  "Generate an info string for the overlay OBJECT."
  (with-temp-buffer
    (insert (propertize "Inline change" 'face 'bold))
    (when (eq (overlay-get object 'category) 'inline-diff-overlay)
      (let ((idx (inline-diff--overlay-index object)))
        (insert " "
                (propertize (format "[%d/%d]" (car idx) (cdr idx))
                            'face 'font-lock-builtin-face))))
    (insert ":")
    (let ((first t))
      (dolist (pair '((inline-diff-apply . "apply")
                      (inline-diff-reject . "reject")
                      (inline-diff-next . "next")
                      (inline-diff-previous . "previous")))
        (when-let (key (car (where-is-internal (car pair) inline-diff-overlay-map)))
          (insert (if first (progn (setq first nil) " ") ", ")
                  (propertize (key-description key) 'face 'help-key-binding)
                  " " (cdr pair)))))
    (buffer-string)))

(defun inline-diff--change-overlay (kind start stop &optional parent display face)
  "Create an overlay from START to STOP for a change of KIND.

Optionally:
- PARENT can be provided to record it as the \"parent\" overlay
- DISPLAY can be provided to display a string in place of the change
- FACE can be provided to apply a face to the overlay"
  (let ((ov (make-overlay start stop)))
    (overlay-put ov 'category 'inline-diff-overlay)
    (overlay-put ov 'kind kind)
    (when display
      (cond
       ((/= start stop)
        (overlay-put ov 'display display))
       ((and (< (point-min) start)
             (eql (char-before start) ?\s)
             (not (inline-diff--overlay-at (1- start))))
        (move-overlay ov (1- start) start)
        (overlay-put ov 'display display)
        (overlay-put ov 'before-string " "))
       ((and (> (point-max) stop)
             (eql (char-after stop) ?\s)
             (not (inline-diff--overlay-at (1+ stop))))
        (move-overlay ov stop (1+ stop))
        (overlay-put ov 'display display)
        (overlay-put ov 'after-string " "))
       (t ; start=stop, and can't do space-display trick
        (overlay-put ov 'before-string
                     (if face
                         (propertize display 'face face)
                       display)))))
    (when face
      (overlay-put ov 'face face))
    (when parent
      (overlay-put ov 'parent parent)
      (push ov (overlay-get parent 'children)))
    ov))

(defun inline-diff--remove-overlay (ov)
  "Remove the parent or child overlay OV."
  (pcase (overlay-get ov 'category)
    ('inline-diff-overlay
     (let ((linked (overlay-get ov 'inline-diff-linked))
           (parent (overlay-get ov 'parent)))
       (delete-overlay ov)
       (dolist (link linked)
         (when (eq (overlay-get link 'category) 'inline-diff-refinement-overlay)
           (delete-overlay link)))
       (when parent
         (unless (delete ov (overlay-get parent 'children))
           (delete-overlay parent)))))
    ('inline-diff-parent-overlay
     (dolist (child (overlay-get ov 'children))
       (delete-overlay child))
     (delete-overlay ov))))

(defun inline-diff--overlays-within (start stop &optional skip-linked)
  "Return a list of difference overlays within the region from START to STOP.
When SKIP-LINKED is non-nil, linked overlays are skipped over."
  (let (overlays skippable-links)
    (dolist (ov (overlays-in start stop))
      (when (eq (overlay-get ov 'category) 'inline-diff-overlay)
        (if (not skip-linked)
            (push ov overlays)
          (unless (memq ov skippable-links)
            (push ov overlays)
            (when-let ((linked (inline-diff--get-linked ov)))
          (setq skippable-links (nconc skippable-links linked)))))))
    (nreverse overlays)))

(defun inline-diff--overlay-at (&optional pt)
  "Return the inline-diff overlay at PT, if any."
  (let (ov)
    (dolist (o (overlays-at pt))
      (when (and (not ov)
                 (eq (overlay-get o 'category) 'inline-diff-overlay))
        (setq ov o)))
    ov))

(defun inline-diff--set-insert-positions (diff &optional max-pos)
  "Process a DIFF list and set the insert positions.
DIFF should be of the form returned by `inline-diff--myers-diff',
reversed.

MAX-POS is the maximum position in the buffer, and is used to set the
position of an insert which is the very first entry of DIFF."
  (let ((pt max-pos)
        (subdiff diff))
    (while subdiff
      (let ((op (car subdiff)))
        (if (eq (car op) 'insert)
            (let ((next-op (cadr subdiff)))
              (setcar (cadr op)
                      (if (eq (car next-op) 'delete)
                          (apply #'+
                                 (caadr next-op)
                                 (mapcar #'length
                                         (mapcar #'cdr
                                                 (cdr next-op))))
                        pt)))
          (setq pt (caadr op))))
      (setq subdiff (cdr subdiff))))
  diff)

(defun inline-diff--insert-words (words)
  "Insert the list of WORDS into the buffer, at point."
  (let ((ws-before
         (or (bolp)
             (eql (syntax-class (syntax-after (1- (point)))) 0)))
        past-start)
    (dolist (word-entry words)
      (if past-start
          (unless (eq (cadr word-entry) 1) ; Punctuation
            (insert " "))
        (setq past-start t))
      (insert (caddr word-entry)))
    (when (and ws-before (not (eolp)))
      (insert " "))))

(defun inline-diff--restructure-ws-straddling (diff)
  "Replace instances of (op ws op) with (ws op) in DIFF."
  (let ((subdiff diff))
    (while subdiff
      (when (and (not (eq (caar subdiff) 'match))        ; Non-match op
                 (equal (cdar (last (car subdiff))) " ") ; Ending in ws
                 (eq (caadr subdiff) 'match)             ; Next is a match
                 (= (length (cdadr subdiff)) 1)          ; With a single element
                 (equal (cdar (cdadr subdiff)) " ")      ; That is ws
                 (eq (caaddr subdiff) (caar subdiff)))   ; Then same op
        (let ((op-kind (caar subdiff))
              (op-data
               (nconc
                (cdaddr subdiff)
                (cdadr subdiff)
                (cdar subdiff)))
              sub-op-data ws)
          ;; Pull the last element of `op-data' out as `ws', destructively.
          (setq sub-op-data op-data)
          (while sub-op-data
            (if (cddr sub-op-data)
                (setq sub-op-data (cdr sub-op-data))
              (setq ws (cdr sub-op-data))
              (setcdr sub-op-data nil)
              (setq sub-op-data nil)))
          ;; Restructure (op ws op) into (ws op)
          (setcar (car subdiff) 'match)
          (setcdr (car subdiff) ws)
          (setcar (cadr subdiff) op-kind)
          (setcdr (cadr subdiff) op-data)
          (setcdr (cdr subdiff) (cdddr subdiff))
          (setq subdiff (cdr subdiff))))
      (setq subdiff (cdr subdiff)))
    diff))

(defun inline-diff--link-inplace-changes (diff-ovs)
  "Find associated operations in DIFF-OVS and link them."
  (let (last-op-ov)
    (dolist (op-ov diff-ovs)
      (when (and (eq (caar last-op-ov) 'delete)
                 (eq (caar op-ov) 'insert))
        (push (cdr op-ov) (overlay-get (cdr last-op-ov) 'inline-diff-linked))
        (push (cdr last-op-ov) (overlay-get (cdr op-ov) 'inline-diff-linked))
        (overlay-put (cdr last-op-ov) 'after-string
                     (propertize inline-diff-change-joiner
                                 'face '(shadow inline-diff-removed))))
      (setq last-op-ov op-ov))))

(defun inline-diff--get-linked (ov &optional so-far)
  "Get the linked overlays from OV, recursively.
SO-FAR is a list of overlays already seen."
  (let ((first (not so-far)))
    (dolist (link (overlay-get ov 'inline-diff-linked))
      (unless (or (not (overlay-buffer link)) (memq link so-far))
        (push link so-far)
        (inline-diff--get-linked link so-far)))
    (if first
        (nreverse so-far)
      so-far)))

(defun inline-diff--place (start stop diff)
  "Place overlays and content to show DIFF in the region from START to STOP."
  (let ((parent-ov (inline-diff--parent-overlay start stop))
        diff-ovs)
    (save-excursion
      (dolist (op diff)
        (push
         (cons op
               (pcase (car op)
                 ('insert
                  (goto-char (caadr op))
                  (when inline-diff-really-insert
                    (dolist (word-entry (cdr op))
                      (insert (cdr word-entry))))
                  (inline-diff--change-overlay
                   'insert (caadr op) (point)
                   parent-ov
                   (and (not inline-diff-really-insert)
                        (mapconcat #'cdr (cdr op) nil))
                   'inline-diff-added))
                 ('delete
                  (let ((start (caadr op))
                        (op-last (car (last op))))
                    (inline-diff--change-overlay
                     'delete start
                     (+ (car op-last)
                        (if (stringp (cdr op-last))
                            (length (cdr op-last))
                          1))
                     parent-ov nil 'inline-diff-removed)))))
         diff-ovs)))
    (overlay-put parent-ov 'child-count
                 (length (overlay-get parent-ov 'children)))
    (inline-diff--link-inplace-changes diff-ovs)
    diff-ovs))

(defun inline-diff--refine-chars (diff-ovs)
  "Refine DIFF-OVS with character-grained highlighting."
  (let (last-op-ov)
    (dolist (op-ov diff-ovs)
      (when (and (eq (caar last-op-ov) 'delete)
                 (eq (caar op-ov) 'insert))
        (let* ((old-chars (vconcat (inline-diff--char-positions
                                    (mapconcat #'cdr (cdar last-op-ov) nil))))
               (new-chars (vconcat (inline-diff--char-positions
                                    (mapconcat #'cdr (cdar op-ov) nil))))
               (max-length (max (length old-chars) (length new-chars)))
               (char-diff (inline-diff--myers-diff old-chars new-chars :test #'eql :key #'cdr))
               deletions insertions)
          (dolist (op char-diff)
            (pcase (car op)
              ('delete
               (setq deletions (nconc deletions (cdr op))))
              ('insert
               (setq insertions (nconc insertions (cdr op))))))
          (when (and deletions
                     (<= (length deletions) (* (- 1 inline-diff-refine-similarity) (length old-chars)))
                     (<= (* inline-diff-refine-similarity max-length) (length old-chars)))
            (inline-diff--show-refinement
             (cdr last-op-ov)
             (inline-diff--seq-ranges deletions #'car)
             'inline-diff-refine-removed))
          (when (and insertions
                     (<= (length insertions) (* (- 1 inline-diff-refine-similarity) (length new-chars)))
                     (<= (* inline-diff-refine-similarity max-length) (length new-chars)))
            (inline-diff--show-refinement
             (cdr op-ov)
             (inline-diff--seq-ranges insertions #'car)
             'inline-diff-refine-added))))
      (setq last-op-ov op-ov))))

(defun inline-diff--seq-ranges (seq &optional key)
  "Return a list of cons cells representing consecutive ranges in SEQ.
SEQ is a sequence of elements.
KEY is a function that extracts a number from each element in SEQ."
  (let (ranges start end)
    (dolist (elem seq)
      (let ((num (if key (funcall key elem) elem)))
        (if (or (null start) (= num (1+ end)))
            (setq end num)
          (push (cons start end) ranges)
          (setq start num
                end num))
        (unless start
          (setq start num
                end num))))
    (push (cons start end) ranges)
    (nreverse ranges)))

(defun inline-diff--show-refinement (ov ranges face)
  "Highlighted RANGES in OV with FACE.
This takes into account whether or not OV has a display string."
  (let ((content (or (overlay-get ov 'display)
                     (overlay-get ov 'before-string)))
        (offset (overlay-start ov)))
    (if content
        (dolist (range ranges)
          (add-face-text-property (1- (car range)) (cdr range) face nil content))
      (dolist (range ranges)
        (let ((hl (make-overlay (+ (car range) offset -1)
                                (+ (cdr range) offset))))
          (overlay-put hl 'category 'inline-diff-refinement-overlay)
          (overlay-put hl 'face face)
          (push hl (overlay-get ov 'inline-diff-linked)))))))

;;;; Public functions

(defun inline-diff-chars (start stop alt &optional alt-stop alt-buffer)
  "Diff the chars in the region START to STOP with alternative content.
ALT can be the start of a range going to ALT-STOP, a string, or a list of
strings. When ALT and ALT-STOP are positions, ALT-BUFFER can be provided to
specify the buffer used."
  (let ((chars (inline-diff--char-positions start stop))
        (alt-chars
         (cond
          ((stringp alt)
           (inline-diff--char-positions alt))
          ((and (listp alt) (consp (car alt)) (numberp (cdar alt)))
           alt)
          ((and (numberp alt) (numberp alt-stop))
           (if alt-buffer
               (with-current-buffer alt-buffer
                 (inline-diff--char-positions alt alt-stop))
             (inline-diff--char-positions alt alt-stop)))
          (t (error "Invalid ALT argument")))))
    (inline-diff--place
     start stop
     (inline-diff--set-insert-positions
      (nreverse
       (inline-diff--myers-diff
        chars alt-chars :test #'eql :key #'cdr))
      stop))))

(defun inline-diff-words (start stop alt &optional alt-stop alt-buffer)
  "Diff the words in the region START to STOP with alternative content.
ALT can be the start of a range going to ALT-STOP, a string, or a list of
strings. When ALT and ALT-STOP are positions, ALT-BUFFER can be provided to
specify the buffer used."
  (let* ((words (inline-diff--word-positions start stop))
         (alt-words
          (cond
           ((stringp alt)
            (inline-diff--string-words alt))
           ((and (listp alt) (consp (car alt)) (stringp (cdar alt)))
            alt)
           ((bufferp alt-buffer)
            (with-current-buffer alt-buffer
              (inline-diff--word-positions alt alt-stop)))
           ((and (numberp alt) (numberp alt-stop))
            (inline-diff--word-positions alt alt-stop))
           (t (error "Invalid ALT argument"))))
         (diff-ovs
          (inline-diff--place
           (or start (point-min))
           (or stop (point-max))
           (inline-diff--restructure-ws-straddling
            (inline-diff--set-insert-positions
             (nreverse
              (inline-diff--myers-diff
               words alt-words :test #'equal :key #'cdr))
             stop)))))
    (when inline-diff-do-refinement
      (inline-diff--refine-chars diff-ovs))
    diff-ovs))

;;;; Commands

(defun inline-diff-apply (ov &optional can-move)
  "Apply the inline-diff overlay OV (if applicable).
When called interactively OV is the relevant overlay at point.
May move `point' when CAN-MOVE is non-nil."
  (interactive
   (list (inline-diff--overlay-at (point)) t))
  (if buffer-read-only
      (message "Buffer is read-only: %S" (current-buffer))
    (when (and ov
               (overlay-buffer ov) ; Not deleted
               (eq (overlay-get ov 'category) 'inline-diff-overlay))
      (let (iov)
        ;; We need to handle linked inserts before deletions,
        ;; otherwise accepting the deletion may incidentally erase
        ;; the insertion overlay.
        (when (eq (overlay-get ov 'kind) 'delete)
          (dolist (lov (inline-diff--get-linked ov))
            (when (and (not iov)
                       (eq (overlay-get lov 'category) 'inline-diff-overlay)
                       (eq (overlay-get lov 'kind) 'insert)
                       (memq ov (inline-diff--get-linked lov)))
              (setq iov lov))))
        (if iov
            (inline-diff-apply iov can-move)
          (pcase (overlay-get ov 'kind)
            ('insert
             (let ((content (or (overlay-get ov 'display)
                                (overlay-get ov 'before-string))))
               (when content
                 (goto-char (overlay-start ov))
                 (delete-region (overlay-start ov) (overlay-end ov))
                 (when (and (overlay-get ov 'display) (overlay-get ov 'before-string))
                   (insert (overlay-get ov 'before-string)))
                 (insert (substring-no-properties content))
                 (when (and (overlay-get ov 'display) (overlay-get ov 'after-string))
                   (insert (overlay-get ov 'after-string))))))
            ('delete
             (delete-region (overlay-start ov) (overlay-end ov))))
          (inline-diff--remove-overlay ov)
          (mapc #'inline-diff-apply (inline-diff--get-linked ov))
          (when (and can-move inline-diff-move-after-acting)
            (inline-diff-next 1)))))))

(defun inline-diff-reject (ov &optional can-move)
  "Reject the inline-diff overlay OV (if applicable).
When called interactively OV is the relevant overlay at point.
May move `point' when CAN-MOVE is non-nil."
  (interactive
   (list (inline-diff--overlay-at (point))))
  (if buffer-read-only
      (message "Buffer is read-only: %S" (current-buffer))
    (when (and ov (eq (overlay-get ov 'category) 'inline-diff-overlay))
      (pcase (overlay-get ov 'kind)
        ('insert
         (let ((start (overlay-start ov))
               (borrowed-content
                (and (overlay-get ov 'display)
                     (or (overlay-get ov 'before-string)
                         (overlay-get ov 'after-string)))))
           (delete-region (overlay-start ov) (overlay-end ov))
           (when borrowed-content
             (save-excursion
               (goto-char start)
               (insert borrowed-content)))))
        ('delete)) ; Removing the overlay is enough
      (inline-diff--remove-overlay ov)
      (mapc #'inline-diff-reject (inline-diff--get-linked ov))
      (when inline-diff-move-after-acting
        (inline-diff-next 1)))))

(defun inline-diff-next (n)
  "Go to the Nth next inline-diff overlay."
  (interactive "p")
  (unless (= n 0)
    (let ((ov-set 0) ovs)
      (when inline-diff-loop-around-scope
        (dolist (ov (overlays-at (point)))
          (when (and (not ovs)
                     (eq (overlay-get ov 'category) 'inline-diff-overlay)
                     (overlay-get ov 'parent))
            (let ((parent (overlay-get ov 'parent)))
              (setq ovs (inline-diff--overlays-within
                         (overlay-start parent) (overlay-end parent) t))))))
      (unless ovs
        (setq ovs (inline-diff--overlays-within (point-min) (point-max) t)))
      (when ovs
        (let ((ov-slice ovs)
              (o 0))
          (while ov-slice
            (if (< (point) (overlay-start (car ov-slice)))
                (setq ov-set o
                      ov-slice nil)
              (setq ov-slice (cdr ov-slice)
                    o (1+ o)))))
        (goto-char (overlay-start (nth (mod (+ n ov-set -1) (length ovs)) ovs)))))))

(defun inline-diff-previous (n)
  "Go to the Nth previous inline-diff overlay."
  (interactive "p")
  (inline-diff-next (- n)))

(defun inline-diff-act ()
  "Act on the inline-diff overlay at point."
  (interactive)
  (pcase (read-answer
          "Act on overlay (a)pply, (r)eject, (n)ext, (p)revious: "
          '(("apply" ?a "Apply the change at point")
            ("reject" ?r "Reject the change at point")
            ("reject" ?k "Alias for reject")
            ("reject" ?x "Alias for reject")
            ("next" ?n "Next change")
            ("previous" ?p "Previous change")))
    (?a (inline-diff-apply))
    ((or ?r ?k ?x) (inline-diff-reject))
    (?n (inline-diff-next 1))
    (?p (inline-diff-previous 1))))

(provide 'inline-diff)
;;; inline-diff.el ends here
