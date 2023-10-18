(defun borwe/turn-string-to-os (val)
  "Turns the string to path to windows format if on windows, else returns it normally"
  (if (eq system-type 'windows-nt)
	  (let ((expanded-val
			 (replace-regexp-in-string
			  "/" "\\\\" (expand-file-name val))))
		(concat (getenv "USERPROFILE") (substring expanded-val 1) )
		)
	val
  ))

(setq borwe-file_seperator (if (eq system-type 'windows-nt)
						   "\\"
						   "/")) 
(setq borwe-lsp-dirs ".emacs.d/LSPs/")
(setq borwe-typecript-lsp-exec `(,(borwe/turn-string-to-os
								  (concat borwe-lsp-dirs "typescript-server/node_modules/.bin/typescript-language-server"))
								 "--stdio"))

(defun borwe/create-dir (dir)
  "Create directory from value DIR"
  (unless (file-directory-p dir)
	(mkdir dir)))

(defun borwe/install-typescript-lsp ()
  "Install Typescript LSP"
  (interactive)
  (let ((typescript-server-dir
		 (concat borwe-lsp-dirs "typescript-server" )))
	(message "Starting installing typescript lsp server")
	(borwe/create-dir borwe-lsp-dirs)
	(borwe/create-dir typescript-server-dir)
	(call-process "npm" nil nil nil "install" "--prefix"
				  (format "%s" typescript-server-dir)
				  "typescript-language-server" "typescript")
	(message "Done installing typescript lsp server")
	 ))
