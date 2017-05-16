;;; sql-completion.el --- Completion at point for use with DB2 and sql-mode.  -*- lexical-binding: t -*-

;;; Commentary:
;;
;;  This completion at point function is for use with sql-mode and a DB2
;;  database.  When triggered this completion at point function will:
;;
;;  1. look at the current query to find database objects that may need
;;     to be queried in the database.
;;  2. If the databse objects identified in step one are not in either
;;     `sql-completions' or `sql-query-targets' it will perform a query
;;     to find if the object is part of the current database.  The sql-mode
;;     buffer local variables will be used for determining which databse
;;     and proc buffer should be used.
;;  3. Update `sql-completions' and `sql-query-targets' as needed.
;;  4. Look at the current query to see if point is after a period.  If
;;     it is candidates will be narrowed to those matching the token
;;     before the period (schema or table).
;;
;;     If there is not a period before point candidates will include
;;     schemas and tables or tables and columns depending on where in
;;     the current query point is.
;;
;;  The `sql-query-targets' is a list kept internally and is intended to
;;  hold a list of targets that have been queried.  Targets are added to
;;  this list regardless of if the target was found in the database or not.
;;  Keeping a list of objects that have been queried is done to decrease
;;  repeated queries for a target that cannot be resolved.
;;
;;  The `sql-completions' hash table contains the resolved database objects.
;;  The table keys are either schemas or schema/table pairs with a period
;;  between them.  The values for both types of keys are the children
;;  (tables or columns) within the key in the database.
;;
;;  Text properties are added to the completion candidates.  If
;;  company-mode is used each candidate will be annotated with the source
;;  for the completion, database.schema[.table].  The second property
;;  holds more specific information associated with a candidate.
;;
;;  In order to use this function it needs to be added to the
;;  `completion-at-point-functions' list as below.
;;
;;  (add-hook 'completion-at-point-functions 'sql-completion-at-point)
;;
;;  Setting `completion-ignore-case' to t should also be done.  The
;;  completion candidates are stored internally using all caps.
;;
;;  (setq completion-ignore-case t)
;;
;;  The `sql-completion-min-target-size' property can be used to restrict
;;  the number of queries sent to the databse.  Any potential databse
;;  object that has a length less than this variable will be skipped and
;;  will not trigger a query to the database.

;;; Todo:
;;  - It would be nice to have the candidates grouped by their location in
;;    the database, ie. sorting of the candidates when presented to the user.

;;; Code:

(require 's)
(require 'cl-lib)

(defvar sql-completions (make-hash-table :test 'equal)
  "Hashtable to hold resolved completions.
DB2 objects.")

(defvar sql-completion-min-target-size 4
  "The minimum size of a database object to queryi for.
If the potential database object has a name
less than this length it will not be sent to the
database to be looked up.")

(defvar sql-completion-debugging nil
  "Enables debugging messages.")

(defvar sql-find-query-tokens nil
  "Database specific function for identifying query tokens.

The method should inspect the current query and identify any tokens that
need to be sent to the database to be resolved.")

(defvar sql-get-database-objects nil
  "Database specific function for resolving query tokens.

This function should take a string as its only paramter.  The string is
a target that should be queried for using the current database connection.")

(defun sql-completion-at-point ()
  "Provide completion candidates for the thing at point.

The current query is inspected to find schemas and
tables that have not been resolved against the database
yet.  Database objects that have been resolved will not
trigger a query to the database again."
  (interactive)
  
  (when sql-completion-debugging
    (message "Starting completion function."))
  
  (save-excursion
    (save-restriction
      (sql-narrow-to-statement)
      
      ;;  Inspect current query and look for things that
      ;;  may need to be looked up in the database.  This
      ;;  could be slow if there are a number of queries
      ;;  that need to be run.  To help mitigate slowness
      ;;  only look-up objects that haven't been looked up
      ;;  already and that are longer than four characters.
      (cl-loop for target in (funcall sql-find-query-tokens)
               for trg = (s-upcase target) do
               (if (and (> (length trg) sql-completion-min-target-size)
                        (not (char-or-string-p (gethash trg sql-completions)))
                        (not (gethash trg sql-completions)))
                   (funcall sql-get-database-objects trg)
                 (when sql-completion-debugging
                   (message "Skipping candidate lookup: %s" trg))))

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
                (setq key (s-upcase (thing-at-point 'symbol)))))
          (setq key (s-upcase (thing-at-point 'symbol))))

        (goto-char (point-max))
        
        (cond ((sql-search-backward-no-move "where")
               (if dot-prior
                   (setq candidates (gethash key sql-completions)) ;;  completing column name with table as the key.
                 (setq candidates (sql-get-table-and-column-candidates))))  ;;  General completion
              ((sql-search-backward-no-move "from")
               (if dot-prior
                   (setq candidates (gethash key sql-completions)) ;;  completing table name with schema as the key.
                 (setq candidates (sql-get-schema-and-table-candidates))))  ;;  General completion
              ((sql-search-backward-no-move "select")
               (if dot-prior
                   (setq candidates (gethash key sql-completions)) ;;  completing column name with table as the key.
                 (setq candidates (sql-get-table-and-column-candidates)))))  ;;  General completion

        ;;  Candidates have been established, print them for
        ;;  debudding and then finish completion by returning
        ;;  the values that capf expects.
        (when sql-completion-debugging
          (message "Candidates: %s" candidates))
        
        (let ((bounds (bounds-of-thing-at-point 'symbol)))
          (when bounds
            (list (car bounds)
                  (cdr bounds)
                  candidates
                  :exclusive 'no
                  :company-docsig #'identity
                  :annotation-function #'sql-completion--annotation
                  :company-doc-buffer #'sql-completion--metadata)))))))

(defun sql-completion--annotation (cand)
  "Return the :note text property for the candidate provided.

CAND is the candidate to return the annotation for."
  (if (get-text-property 0 :note cand)
      (format "[%s]" (get-text-property 0 :note cand))
    ""))

(defun sql-completion--metadata (cand)
  "Return the :meta text property for the candidate provided.

CAND is the candidate to return the metadata for."
  (company-doc-buffer (get-text-property 0 :meta cand)))

(defun sql-propertize-annotation (candidate database &optional schema-table)
  "Return a propertized text containing the candidate notes.

CANDIDATE is the candidate to add the text property to.
SCHEMA-TABLE is the schema.table that the candidate was found.
DATABASE is the database that the candidate was found."
  (if schema-table
      (propertize candidate :note (concat database "." schema-table))
    (propertize candidate :note database)))

(defun sql-propertize-metadata (candidate schema-table database schema table type remarks)
  "Return a propertized text containing the candidate meta data.

CANDIDATE is the candidate to add the text property to.
SCHEMA-TABLE is the schema and table that the candidate was found.
DATABASE is the database that the candidate was found.
SCHEMA is the schema that the candidate was found.
TABLE is the table that the candidate was found.
TYPE is the type of database object that the candidate is.
REMARKS is the remarks that were entered in the database for the candidate."
  (propertize candidate
              :note (concat database "." schema-table)
              :meta (concat "Database:  " database "\n\n"
                            "Schema:    " schema   "\n\n"
                            "Object:    " table    "\n\n"
                            "Type:      " type     "\n\n"
                            "Remarks:   " remarks)))

(defun sql-get-table-and-column-candidates ()
  "Get a list of candidates that consists of only table and column names."

  (when sql-completion-debugging
    (message "Getting list of table or column candidates."))

  (cl-loop for key being the hash-keys of sql-completions
           using (hash-values value)
           with cands
           when (and (not (char-or-string-p value))
                     (s-contains-p "." key))
           do
           (let ((table (second (split-string key "\\."))))
             (propertize table :note (get-text-property 0 :note key))
             
             (push table cands)
             
             (cl-loop for item in value do
                      (push item cands)))
           finally return cands))
  
(defun sql-get-schema-and-table-candidates ()
  "Get a list of candidates that consists of only schema and table names."

  (when sql-completion-debugging
    (message "Getting list of schema or table candidates."))

  (cl-loop for key being the hash-keys of sql-completions
           using (hash-values value)
           with cands do
           (cond ((and (not (char-or-string-p value))
                       (s-contains-p "." key))
                  (push (second (split-string key "\\.")) cands))
                 ((not (s-blank? value))
                  (push key cands)))
           finally return cands))

(defun sql-narrow-to-statement ()
  "Narrow the current buffer to the current SQL statement.

Narrow the buffer to the current SQL statement and leave
point where it is."
  (interactive)
  (let ((point-curr (point))
        (query-begin)
        (query-end))
    (backward-paragraph)
    (setq query-begin (point))

    (forward-paragraph)
    (setq query-end (point))

    (narrow-to-region query-begin query-end)

    ;;  Put point back where it was before the query was
    ;;  isolated.
    (goto-char point-curr)))

(defun sql-buffer-contains-substring (string)
  "Used to tell if a given string is in the current buffer.

STRING is the string to find in the buffer."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward-regexp string nil t))))

(defun sql-search-backward-no-move (target)
  "Search backward but don't move point for the target provided.

TARGET is the string that is to be looked for."
  (save-restriction
    (save-excursion
      (search-backward target nil t))))

(defun sql-db2-get-database-objects (target)
  "Get schema, table, or columns matching the target string passed.

TARGET is the name or partial name of a database object to query the database for."
  (interactive)
  (save-excursion
    (save-restriction
      (let* ((query (format "SELECT TABSCHEMA || '|' || TABNAME || '|' || COLNAME || '|' || COLNO || '|' || COALESCE(REMARKS, 'NONE') FROM SYSCAT.COLUMNS WHERE TABNAME LIKE '%s%%' OR TABSCHEMA LIKE '%s%%' ORDER BY TABSCHEMA, TABNAME, COLNO WITH UR;" target target))
             (output-buffer "*SQL: OUTPUT*")
             (objects ())
             (c-beg)
             (c-end))

        (when sql-completion-debugging
          (message "Sending query: %s" query))

        (sql-redirect (get-buffer sql-buffer)
                      query
                      output-buffer
                      nil)

        (with-current-buffer output-buffer
          (widen)
          (toggle-truncate-lines 1)

          (if (sql-buffer-contains-substring "^\\s-*0 record(s)")
              (puthash target "" sql-completions)
            (sql-db2-narrow-to-result)
            
            (goto-char (point-min))

            ;;  Loop over narrowed region and process each result.
            ;;  Add each result to the hashmap of completions if
            ;;  it doesn't exist yet.
            (cl-loop for line = (buffer-substring-no-properties (line-beginning-position) (line-end-position))
                     for parts = (split-string line "|" split-string-default-separators)
                     for schema = (s-trim (nth 0 parts))
                     for table = (s-trim (nth 1 parts))
                     for column = (s-trim (nth 2 parts))
                     for type = (s-trim (nth 3 parts))
                     for remarks = (s-trim (nth 4 parts))
                     for schema-table = (concat schema "." table)
                     until (eobp) do

                     (when sql-completion-debugging
                       (message "Processing line: %s" (s-trim line)))

                     ;;  Conditions:
                     ;;  1.  Schema hash doesn't exist:
                     ;;      Insert table into schema value list
                     ;;      Insert column into schema.table value list
                     ;;  2.  Schema hash exsist but table is not in value list:
                     ;;      Insert table into schema value list
                     ;;  3.  Schema-table hash exists but doesn't contain the column in the value list:
                     ;;      Insert column into schema.table value list
                     ;;  4.  Table hash doesn't exist:
                     ;;      Insert dummy hash for table.
                     (when (not (gethash schema sql-completions))
                       (puthash schema
                                (list (sql-propertize-annotation table sql-database schema-table))
                                sql-completions)
                       (puthash (sql-propertize-annotation schema sql-database schema-table)
                                (list (sql-propertize-metadata column schema-table sql-database schema table type remarks))
                                sql-completions))

                     (when (not (-contains-p (gethash schema sql-completions) table))
                       (let ((tables (gethash schema sql-completions)))
                         (push (sql-propertize-annotation table sql-database schema-table) tables)
                         (puthash schema
                                  tables
                                  sql-completions)))

                     (when (not (-contains-p (gethash schema-table sql-completions) column))
                       (let ((cols (gethash schema-table sql-completions)))
                         (push (sql-propertize-metadata column schema-table sql-database schema table type remarks) cols)
                         (puthash (sql-propertize-annotation schema-table sql-database) cols sql-completions)))

                     (when (not (gethash table sql-completions))
                       (puthash table "" sql-completions))

                     (when (not (gethash column sql-completions))
                       (puthash column "" sql-completions))
                     (forward-line 1))))))))

(defun sql-db2-find-query-tokens ()
  "Inspect the current query for tokens that should be resolved in the database.

Tokens include schema names, table names, and column names.  A list of these tokens is
returned."
  (when sql-completion-debugging
    (message "Starting sql-db2-find-query-tokens."))
    
  (save-excursion
    (let ((m1)
          (m2)
          (m3)
          (tokens ()))
      (goto-char (point-min))
      (when (search-forward-regexp "select\\([^\\0]*from\\)\\|select\\([^\\0]*\\)" nil t)
        (let* ((m (or (match-string 1) (match-string 2)))
               (mt (s-trim m)))
          (cond ((match-string 1)
                 (setq m1 (replace-regexp-in-string "\n" "" (substring mt 0 (- (length mt) 4))))
                 (backward-word))
                (t
                 (setq m1 mt)))
          (cl-loop for item in (split-string m1 "[,\. ]") 
                   when (not (-contains-p tokens (s-trim item)))
                   do
                   (push (s-trim item) tokens))))
      (when (and m1 (search-forward-regexp "from\\([^\\0]*where\\)\\|from\\([^\\0]*\\)" nil t))
        (let* ((m (or (match-string 1) (match-string 2)))
              (mt (s-trim m)))
          (cond ((match-string 1)
                 (setq m2 (replace-regexp-in-string "\n" "" (substring mt 0 (- (length mt) 5))))
                 (backward-word))
                (t
                 (setq m2 mt)))
          (cl-loop for item in (split-string m2 "[,\. ]")
                   when (not (-contains-p tokens (s-trim item)))
                   do
                   (push (s-trim item) tokens))))
      (when (search-forward-regexp "where\\([^\\0]*\\)" nil t)
       (let ((mt (s-trim (match-string 1))))
          (setq m3 (replace-regexp-in-string "\n" "" mt))
          (cl-loop for item in (split-string m3 "[,\. ]")
                   when (not (-contains-p tokens item))
                   do
                   (push item tokens))))

      (when sql-completion-debugging
        (message "Found potential objects to lookup: %s" tokens))
      
      tokens)))

;; (defun sql-db2-find-query-tokens ()
;;   "Inspect the current query for tokens that should be resolved in the database.

;; Tokens include schema names, table names, and column names.  A list of these tokens is
;; returned."
;;   (when sql-completion-debugging
;;     (message "Starting sql-db2-find-query-tokens."))
    
;;   (save-excursion
;;     (let ((tokens ()))
;;       (goto-char (point-min))
;;       (when (search-forward-regexp "select\\([^\\0]*from\\)\\|select\\([^\\0]*\\)" nil t)
;;         (let* ((m (or (match-string 1) (match-string 2)))
;;                (mt (s-trim m)))
;;           (cond ((match-string 1)
;;                  (backward-word)           
;;                  (setq tokens (append (split-string (substring mt 0 (- (length mt) 4))) tokens)))
;;                 (t
;;                  (setq tokens (append (split-string mt) tokens))))))
;;       (when (search-forward-regexp "from\\([^\\0]*where\\)\\|from\\([^\\0]*\\)" nil t)
;;         (let* ((m (or (match-string 1) (match-string 2)))
;;               (mt (s-trim m)))
;;           (cond ((match-string 1)
;;                  (setq tokens (append (split-string (substring mt 0 (- (length mt) 5))) tokens))
;;                  (backward-word))
;;                 (t
;;                  (setq tokens (append (split-string mt) tokens))))))
;;       (when (search-forward-regexp "where\\([^\\0]*\\)" nil t)
;;        (let ((mt (s-trim (match-string 1))))
;;          (setq tokens (append (split-string mt) tokens))))

;;       (cl-loop for i in tokens
;;                collect (split-string i "[,\. ]"))
      
;;       ;; (when sql-completion-debugging
;;       ;;   (message "Found potential objects to lookup: %s" tokens))
;;       ;; tokens
;;       )))

(defun sql-db2-narrow-to-result ()
  "Narrow the current buffer to a DB2 result, not including headers.

Narrow the current buffer to a DB2 result.  Headers are not included in
the narrowing.  The line that contains the number of results is also
not included in the narrowing."
  (interactive)
  (goto-char (point-min))

  (search-forward "---------" nil t)

  (next-line)
  (beginning-of-line)

  (setq c-beg (point))

  (forward-paragraph)

  (setq c-end (1- (point)))

  (narrow-to-region c-beg c-end))

(provide 'sql-completion)
;;; sql-completion.el ends here
