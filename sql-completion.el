;;; -*- lexical-binding: t -*-
;;; sql-completion.el --- Completion at point for use with DB2 and sql-mode.

;;; Commentary:

;;;  This completion at point function is for use with sql-mode and a DB2
;;;  database.  The first part of the completion system is manual.  A function
;;;  needs to be called to look at the current query and determine the
;;;  schemas and tables that are part of the query.  Second a function is
;;;  called to query DB2 and get table listings for the schemas identified and
;;;  column listings for the tables identified.  After this is done the
;;;  completion at point function will have the data it needs to offer
;;;  completion candidates.  The completion function will work with company
;;;  to offer additional details for completions.  For instance each candidate
;;;  has where it was found: database.schema.table or database.schema.

;;; Code:

(require 's)
(require 'cl-lib)

;;------------------------------------------------------------------------------
;;  Testing setup (begin)
;;------------------------------------------------------------------------------

;;  Add this completion at point function to the list of functions.
;;  Obviously this is not the real way to do it.
(add-hook 'completion-at-point-functions 'sql-completion-at-point)

(defun test ()
  ""
  (interactive)
  (sql-get-schema-or-table "SUSPEND"))

(defun test2 ()
  ""
  (interactive)
  (maphash #'(lambda (key value)
               (message "Key: %s\tValue: %s" key value))
           db2-completions))

(setq db2-completions (make-hash-table :test 'equal))
db2-completions

;;------------------------------------------------------------------------------
;;  Testing setup (end)
;;------------------------------------------------------------------------------

(defvar db2-completions (make-hash-table :test 'equal)
  "Hashtable to hold possible completions for
DB2 objects.")

(defun sql-completion-at-point ()
  (interactive)
  "Provide completion candidates for the thing at point.
The current query is inspected to find schemas and 
tables that have not been resolved against the database
yet.  Database objects that have been resolved will not
trigger a query to the database again."

  ;;  TODO:  Isolate only the current query.
  
  ;;  TODO:  Inspect current query and look for things 
  ;;  that may need to be looked up in the database.
  ;;  This could be slow if there are a number of queries
  ;;  that need to be run.

  ;;  Steps:
  ;;  1.  Find SELECT queries to run.
  ;;  2.  Find FOR queries to run.
  ;;  3.  Find where queries to run.
  ;;  4.  Call function to query for a list of targets.

  ;;  Need to look back to see what schema/table list
  ;;  is before the period.
  (let ((key)
        (dot-prior nil)
        (candidates))
    
    ;;  If the char before is a period move backward
    ;;  to get the word before the period.
    (if (eq 46 (char-before))
    ;; (if (looking-back "\\s-+.*\..*")
        (progn
          (save-excursion
            (search-backward "." nil t)
            (setq dot-prior t)
            (setq key (upcase (thing-at-point 'symbol)))))
      (setq key (upcase (thing-at-point 'symbol))))

    (let ((query-begin)
          (query-end (point)))
      (save-excursion
        (save-restriction
          (backward-paragraph)
          (setq query-begin (point))

          (narrow-to-region query-begin query-end)

          (goto-char (point-max))
                     
          (cond ((search-backward-no-move "where")
                 (if dot-prior
               (setq candidates (gethash key db2-completions)) ;;  completing column name with table as the key.
             (setq candidates (sql-get-table-and-column-candidates))))  ;;  General completion
          ((search-backward-no-move "from")      
           (if dot-prior
               (setq candidates (gethash key db2-completions)) ;;  completing table name with schema as the key.
             (setq candidates (sql-get-schema-and-table-candidates))))  ;;  General completion
          ((search-backward-no-move "select")
           (if dot-prior
               (setq candidates (gethash key db2-completions)) ;;  completing column name with table as the key.
             (setq candidates (sql-get-table-and-column-candidates))))))))  ;;  General completion

    ;;  Might make sense to sort the candidates based on schema or schema.table
    ;;  where the candidate was found so that they appear together in the completion
    ;;  list.  Need to pass :sorted t to company.
    (message "Candidates: %s" candidates)
    
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (list (car bounds)
              (cdr bounds)
              candidates
              :exclusive 'yes
              :company-docsig #'identity
              :annotation-function #'sql-completion--annotation
              :company-doc-buffer #'sql-completion--metadata)))))

(defun sql-completion--annotation (cand)
  "Return the :note text property for the 
candidate provided."
  (format "[%s]" (get-text-property 0 :note cand)))

(defun sql-completion--metadata (cand)
  "Function to put the candidate :meta text
property into the doc buffer used by company."
  (company-doc-buffer (get-text-property 0 :meta cand)))

(defun search-backward-no-move (target)
  "Search backward but don't move point."
  (save-restriction
    (save-excursion
      (search-backward target nil t))))

(defun sql-get-from-clause ()
  "Get a list containing the schema.table
pairs in the FROM clause of a SELECT statement."
  (interactive)
  (save-excursion
    (save-restriction

      ;;  TODO : Narrow the buffer to be only the current
      ;;  paragraph/query.
      
      (let ((for-begin)
            (for-end))
        (goto-char (point-min))

        ;;  Find start of FROM clause
        (search-forward "from ")
        (setq f-begin (point))

        ;;  Find start of WHERE clause or use
        ;;  the end of the buffer.
        (if (search-forward "where")
            (progn
              (backward-word)
              (setq f-end (point)))
          (setq f-end (point-max)))

        ;;  Substring the buffer to pull only
        ;;  the schema.table pairs.       
        (split-string (upcase (buffer-substring-no-properties f-begin f-end)) "\\s-*,\\s-*")))))

;; (defun sql-get-schemas-and-tables ()
;;   "Get the schema.table pairs part of an
;; SELECT statement and if they are missing from
;; the db2-completion hashmap then add them."
;;   (interactive)
;;   (let ((schema-table-list (sql-get-from-clause)))
;;     (cl-loop for item in schema-table-list do
;;              (let* ((parts (split-string item "\\."))
;;                     (schema (s-trim (first parts)))
;;                     (pair (s-trim item)))
;;                (when (not (gethash schema db2-completions))
;;                  (puthash schema nil db2-completions))

;;                (when (not (gethash pair db2-completions))
;;                  (puthash pair nil db2-completions))))))

(defun sql-get-from-completions-from-db ()
  "Get completion candidates for the schemas/tables found
in the FROM clause of the current query."
  (interactive)
  (let* ((schema-table-list (sql-get-from-clause)))
    (cl-loop for item in schema-table-list do
             (let* ((p (split-string item "\\."))
                    (schema (s-trim (first p)))
                    (table (s-trim (second p))))
               (if (table)
                   (sql-get-schema-or-table table)
                 (sql-get-schema-or-table schema))))))

(defun sql-get-table-and-column-candidates ()
  "Get a list of candidates that consists of only
table and column names."
  (let ((cands ()))
    (maphash #'(lambda (key value)
                (when (s-contains-p "." key)
                    (progn
                      (push (second (split-string key "\\.")) cands)
                      (dolist (item value)
                        (push item cands)))))
             db2-completions)
    cands))

(defun sql-get-schema-and-table-candidates ()
  "Get a list of candidates that consists of only
schema and table names."
    (message "Getting schema and table candidates.")
  (let ((cands ()))
    (maphash #'(lambda (key value)
                 (if (s-contains-p "." key)
                     (push (second split-string key "\\.") cands)
                   (push key cands)))
             db2-completions)
    cands))

(defun sql-get-schema-or-table (target)
  "Get schema, table, or columns matching the 
target string passed.

The input string is a schema name, table name,
or column name \(partial or full\) to search 
the database for."
  (save-excursion
    (save-restriction
      (let* ((query (format "SELECT TABSCHEMA || '|' || TABNAME || '|' || COLNAME || '|' || COLNO || '|' || COALESCE(REMARKS, 'NONE') FROM SYSCAT.COLUMNS WHERE TABNAME LIKE '%s%%' OR TABSCHEMA LIKE '%s%%' OR COLNAME LIKE '%s%%' ORDER BY TABSCHEMA, TABNAME, COLNO WITH UR;" target target target))
             (output-buffer "*SQL: OUTPUT*")
             (objects ())
             (c-beg)
             (c-end))
        (sql-redirect (get-buffer sql-buffer)
                      query
                      output-buffer
                      nil)
        
        (with-current-buffer output-buffer
          (widen)
          (if (not (buffer-contains-substring "^\\s-*0 record(s)"))
              (progn
                (goto-char (point-min))
                
                (search-forward "---------" nil t)

                (next-line)
                (beginning-of-line)

                (setq c-beg (point))

                (forward-paragraph)

                (setq c-end (1- (point)))

                (narrow-to-region c-beg c-end)
                (goto-char (point-min))

                ;;  Loop over narrowed region and process each result.
                ;;  Add each result to the hashmap of completions if
                ;;  it doesn't exist yet.
                (cl-loop until (eobp) do
                         (progn
                           (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                                  (parts (split-string line "|" split-string-default-separators))
                                  (schema (s-trim (nth 0 parts)))
                                  (table (s-trim (nth 1 parts)))
                                  (column (s-trim (nth 2 parts)))
                                  (type (s-trim (nth 3 parts)))
                                  (remarks (s-trim (nth 4 parts)))
                                  (schema-table (concat schema "." table)))

                             ;;  Conditions:
                             ;;  1.  Schema hash doesn't exist:
                             ;;      Insert table into schema value list
                             ;;      Insert column into schema.table value list
                             ;;  2.  Schema hash exsist but table is not in value list:
                             ;;      Insert table into schema value list
                             ;;  3.  Schema-table hash exists but doesn't contain the column in the value list:
                             ;;      Insert column into schema.table value list
                             (when (not (gethash schema db2-completions))
                                 (progn
                                   (puthash schema
                                            (list (sql-propertize-annotation table schema-table sql-database))
                                            db2-completions)
                                   (puthash schema-table
                                            (list (sql-propertize-metadata column schema-table sql-database schema table type remarks))
                                            db2-completions)))
                             
                             (when (not (-contains-p (gethash schema db2-completions) table))
                               (let ((tables (gethash schema db2-completions)))
                                 (push (sql-propertize-annotation table schema-table sql-database) tables)
                                 (puthash schema
                                          tables
                                          db2-completions)))
                             
                             (when (not (-contains-p (gethash schema-table db2-completions) column))
                               (let ((cols (gethash schema-table db2-completions)))
                                 (push (sql-propertize-metadata column schema-table sql-database schema table type remarks) cols)
                                 (puthash schema-table cols db2-completions))))
                           (forward-line 1))))))))))

(defun sql-propertize-annotation (candidate schema-table database)
  "Return a propertized text containing the 
candidate notes."
  (propertize candidate :note (concat database "." schema-table)))

(defun sql-propertize-metadata (candidate schema-table database schema table type remarks)
  "Return a propertized text containing the 
candidate meta data."
  (propertize candidate
              :note (concat database "." schema-table)
              :meta (concat "Database:  " database "\n\n"
                            "Schema:    " schema   "\n\n"
                            "Object:    " table    "\n\n"
                            "Type:      " type     "\n\n"
                            "Remarks:   " remarks)))

(defun buffer-contains-substring (string)
  "Used to tell if a given string is in the
current buffer.  Found at:
http://stackoverflow.com/questions/3034237/check-if-current-emacs-buffer-contains-a-string"
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward string nil t))))
