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
;;  - Currently two lists are kept one to hold the queries that have already
;;    been sent to the database to be resolved and another (hashtable) to
;;    hold the resolved database objects.  It would be nice to have one list.
;;  - It would be nice to have the candidates grouped by their location in
;;    the database.
;;  - The annotations for schemas are not working correctly.  They are showing
;;    nil instead of the remarks that were found.

;;; Code:

(require 's)
(require 'cl-lib)

(defvar sql-completions (make-hash-table :test 'equal)
  "Hashtable to hold possible completions for
DB2 objects.")

(defvar sql-query-targets ()
  "List of targets that have been searched for
this list is maintained so that repeated queries
for the same database object is avoided.")

(defun sql-completion-at-point ()
  (interactive)
  "Provide completion candidates for the thing at point.
The current query is inspected to find schemas and 
tables that have not been resolved against the database
yet.  Database objects that have been resolved will not
trigger a query to the database again."

  (let ((query-begin)
        (query-end (point)))
    (save-excursion
      (save-restriction
        (backward-paragraph)
        (setq query-begin (point))

        (narrow-to-region query-begin query-end)

        ;;  Inspect current query and look for things that
        ;;  may need to be looked up in the database.  This
        ;;  could be slow if there are a number of queries
        ;;  that need to be run.  To help mitigate slowness
        ;;  only look-up objects that haven't been looked up
        ;;  already.
        (let ((targets (sql-find-query-tokens)))
          (cl-loop for target in targets do
                   (when (not (and (-contains-p target sql-query-targets)
                                   (gethash target sql-completions)))
                     (sql-get-database-objects (s-upcase target))
                     (push (s-upcase target) sql-query-targets))))

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
                   (setq candidates (sql-get-table-and-column-candidates)))))))))  ;;  General completion

  ;;  Candidates have been established, print them for
  ;;  debudding and then finish completion by returning
  ;;  the values that capf expects.
  (message "Candidates: %s" candidates)
    
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (list (car bounds)
            (cdr bounds)
            candidates
            :exclusive 'yes
            :company-docsig #'identity
            :annotation-function #'sql-completion--annotation
            :company-doc-buffer #'sql-completion--metadata))))

(defun sql-completion--annotation (cand)
  "Return the :note text property for the 
candidate provided."
  (format "[%s]" (get-text-property 0 :note cand)))

(defun sql-completion--metadata (cand)
  "Function to put the candidate :meta text
property into the doc buffer used by company."
  (company-doc-buffer (get-text-property 0 :meta cand)))

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

(defun sql-get-table-and-column-candidates ()
  "Get a list of candidates that consists of only
table and column names."
  (when sql-completion-debugging
    (message "Getting list of table or column candidates."))
    
  (let ((cands ()))
    (maphash #'(lambda (key value)
                 (when (s-contains-p "." key)
                   (progn
                     (push (second (split-string key "\\.")) cands)
                     (cl-loop for item in value do
                              (push item cands)))))
             sql-completions)
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
             sql-completions)
    cands))

(defun sql-get-database-objects (target)
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
          (if (not (sql-buffer-contains-substring "^\\s-*0 record(s)"))
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
                             (when (not (gethash schema sql-completions))
                                 (progn
                                   (puthash schema
                                            (list (sql-propertize-annotation table schema-table sql-database))
                                            sql-completions)
                                   (puthash schema-table
                                            (list (sql-propertize-metadata column schema-table sql-database schema table type remarks))
                                            sql-completions)))
                             
                             (when (not (-contains-p (gethash schema sql-completions) table))
                               (let ((tables (gethash schema sql-completions)))
                                 (push (sql-propertize-annotation table schema-table sql-database) tables)
                                 (puthash schema
                                          tables
                                          sql-completions)))
                             
                             (when (not (-contains-p (gethash schema-table sql-completions) column))
                               (let ((cols (gethash schema-table sql-completions)))
                                 (push (sql-propertize-metadata column schema-table sql-database schema table type remarks) cols)
                                 (puthash schema-table cols sql-completions))))
                           (forward-line 1))))))))))

(defun sql-find-query-tokens ()
  "Look through the current query for tokens
that should be queried for in the database.
Tokens include schema names, table names, and
column names.  A list of these tokens is
returned."
  (save-excursion
    (let ((m1)
          (m2)
          (m3)
          (tokens ()))
      (when (search-forward-regexp "select\\([^\\0]*from\\)\\|select\\([^\\0]*\\)" nil t)
        (let* ((m (or (match-string 1) (match-string 2)))
               (mt (s-trim m)))
          (cond ((match-string 1)
                 (setq m1 (replace-regexp-in-string "\n" "" (substring mt 0 (- (length mt) 4))))
                 (backward-word))
                (t
                 (setq m1 mt)))
          (message "Match one: %s" m1)
          (cl-loop for item in (split-string m1 "[,\. ]") do
                   (when (not (-contains-p tokens item))
                     (push item tokens)))))
      (when (and m1 (search-forward-regexp "from\\([^\\0]*where\\)\\|from\\([^\\0]*\\)" nil t))
        (let* ((m (or (match-string 1) (match-string 2)))
              (mt (s-trim m)))
          (cond ((match-string 1)
                 (setq m2 (replace-regexp-in-string "\n" "" (substring mt 0 (- (length mt) 5))))
                 (backward-word))
                (t
                 (setq m2 mt)))
          (message "Match two: %s" m2)
          (cl-loop for item in (split-string m2 "[,\. ]") do
                   (when (not (-contains-p tokens item))
                     (push item tokens)))))
      (when (search-forward-regexp "where\\([^\\0]*\\)" nil t)
       (let ((mt (s-trim (match-string 1))))
          (setq m3 (replace-regexp-in-string "\n" "" mt))
          (message "Match three: %s" m3)
          (cl-loop for item in (split-string m3 "[,\. ]") do
                   (when (not (-contains-p tokens item))
                     (push item tokens)))))
      tokens)))

(defun sql-buffer-contains-substring (string)
  "Used to tell if a given string is in the
current buffer.  Found at:
http://stackoverflow.com/questions/3034237/check-if-current-emacs-buffer-contains-a-string"
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward string nil t))))

(defun sql-search-backward-no-move (target)
  "Search backward but don't move point."
  (save-restriction
    (save-excursion
      (search-backward target nil t))))

(provide 'sql-completion)
;;; sql-completion.el ends here
