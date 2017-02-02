;;; -*- lexical-binding: t -*-

(require 's)

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

  ;;  Add schema and schema.table keys to the hashmap.
  ;; (sql-get-schemas-and-tables)

  ;;  Go through hashmap and fill in missing values for
  ;;  nil keys.
  ;; (sql-get-missing-completions)

  ;;  Need to look back to see what schema/table list
  ;;  is before the period.
  (let ((key)
        (dot-prior nil)
        (candidates))

    ;;  If the char before is a period move backward
    ;;  to get the word before the period.
    (if (eq 46 (char-before))
        (progn
          (save-excursion
            (backward-word)
            (setq dot-prior t)
            (setq key (upcase (thing-at-point 'symbol)))))
      (setq key (upcase (thing-at-point 'symbol))))

    (cond ((search-backward-no-move "where")
           (if dot-prior
               (setq candidates (gethash key db2-completions)) ;;  completing column name with table as the key.
             (message "Trying to complete either table or column.")))
          ((search-backward-no-move "from")      
           (if dot-prior
               (setq candidates (gethash key db2-completions)) ;;  completing table name with schema as the key.
             (message "Trying to complete either schema or table.")))
          ((search-backward-no-move "select")
           (if dot-prior
               (setq candidates (gethash key db2-completions)) ;;  completing column name with table as the key.
             (message "Trying to complete either table or column."))))

    (message "Candidates: %s" candidates)
    
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (list (car bounds)
              (cdr bounds)
              candidates
              :exclusive 'no
              :company-docsig #'identity
              :company-doc-buffer #'(lambda (cand)
                                      (company-doc-buffer (format "'%s' was found using minsql-completion-at-point" cand))))))))

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

(defun sql-get-schemas-and-tables ()
  "Get the schema.table pairs part of an
SELECT statement and if they are missing from
the db2-completion hashmap then add them."
  (interactive)
  (let ((schema-table-list (sql-get-from-clause)))
    (cl-loop for item in schema-table-list do
             (let* ((parts (split-string item "\\."))
                    (schema (s-trim (first parts)))
                    (pair (s-trim item)))
               (when (not (gethash schema db2-completions))
                 (puthash schema nil db2-completions))

               (when (not (gethash pair db2-completions))
                 (puthash pair nil db2-completions))))))

(defun sql-get-missing-completions ()
  "Get missing completions for values in 
the hashtable."
  (interactive)
  (maphash #'sql-get-missing-completions-2 db2-completions))

(defun sql-get-missing-completions-2 (key value)
  "Get completions for the schema.table or table
passed if the value for the key is nil."
  (when (eq value nil)
    (if  (s-contains-p "." key)
        (sql-get-columns-in-table key)
      (sql-get-tables-in-schema key))))

(defun sql-get-columns-in-table (schema-table database-name)
  "Get column listing for the table passed."
  (save-excursion
    (save-restriction
      (let* ((parts (split-string schema-table "\\."))
             (schema (first parts))
             (table (second parts))
             (query (format "SELECT COLUMN_NAME FROM SYSIBM.COLUMNS WHERE TABLE_NAME = '%s' AND TABLE_SCHEMA = '%s' WITH UR;" table schema))
             (sqli-buffer (get-buffer (format "*SQL: %s*" database-name)))
             (output-buffer "*SQL: OUTPUT*")
             (columns ())
             (c-beg)
             (c-end))
        (sql-redirect sqli-buffer
                      query
                      output-buffer
                      nil)

        ;;  Goto output buffer to pull column names.
        (switch-to-buffer output-buffer)

        (goto-char (point-min))

        (search-forward "---------" nil t)

        (beginning-of-line)
        (next-line)

        (setq c-beg (point))

        (forward-paragraph)

        (setq c-end (1- (point)))

        (narrow-to-region c-beg c-end)
        (goto-char (point-min))

        (while (search-forward-regexp "^\\(.*\\)\\s-*$" nil t)
          (push (s-trim (match-string 1)) columns))

        ;;  Replace the old hash that had a nil value with
        ;;  the list of column names.
        (puthash schema-table (nreverse columns) db2-completions)))))

(defun sql-get-tables-in-schema (schema database-name)
  "Get table names for the schema passed."
  (save-excursion
    (save-restriction
      (let* ((query (format "SELECT DISTINCT(TABLE_NAME) FROM SYSIBM.COLUMNS WHERE TABLE_SCHEMA = '%s' WITH UR;" schema))
             (sqli-buffer (get-buffer (format "*SQL: %s*" database-name)))
             (output-buffer "*SQL: OUTPUT*")
             (tables ())
             (c-beg)
             (c-end))
        (message "Starting tables-in-schema-process: %s" query)
        (sql-redirect sqli-buffer
                      query
                      output-buffer
                      nil)
        
        (message "Done calling DB")

        ;;  Goto output buffer to pull column names.
        (switch-to-buffer output-buffer)

        (goto-char (point-min))

        (search-forward "---------" nil t)

        (beginning-of-line)
        (next-line)

        (setq c-beg (point))

        (forward-paragraph)

        (setq c-end (1- (point)))

        (narrow-to-region c-beg c-end)
        (goto-char (point-min))

        (while (search-forward-regexp "^\\(.*\\)\\s-*$" nil t)
          (push (s-trim (match-string 1)) tables))

        ;;  Replace the old hash that had a nil value with
        ;;  the list of column names.
        (puthash schema (nreverse tables) db2-completions)))))
