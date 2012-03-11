Web Site Framework
==================

Let there be site:

    (defvar site (make-instance 'website :system '#:online-asdf-system))

Optional HTTP port's default value is 8666.

    (with-server site

Routing
-------

Simple route:

      (set-route :examples
                 :link "/examples"
                 :action (respond-examples))

Route with parameters:

      (let ((prefix "/example/"))
        (set-route :example
                   :args (example)
                   :link (join prefix (keyword-name (id example)))
                   :clause-with-scope t
                   :scope ((uri (script-name*)))
                   :clause (begins-with? uri prefix)
                   :action (awhen (example (name-keyword (trim-left prefix uri)))
                             (respond-example it))))

For public scripts and theme folders:

      (set-route :static
                 :clause-with-scope t
                 :scope ((uri (script-name*)))
                 :clause (iter (for path in '("/js/" "/css/" "/image/"))
                               (when (begins-with? uri path)
                                 (leave t)))
                 :action (handle-static-file (docroot/ uri)))

For public folder with URI masquerade:

      (set-route :public
                 :clause-with-scope t
                 :scope ((uri (script-name*)))
                 :clause (scan (join-by "|"
                                        "^/robots\\.txt$"
                                        "^/kinda-root-folder/")
                               uri)
                 :action (handle-static-file (docroot/ "public" uri)))

Bootstrap default AJAX route:

      (set-route-ajax)

Handle "/ajax/test" => { status: "win", data: { key: "value" }}

      (set-route :asynchronous-test
                 :follow :ajax
                 :clause (eql ajax-action :test)
                 :action (list :key "value"))

Handle "/ajax/error" => { status: "fail", data: "FU5" }

      (set-route :error-test
                 :follow :ajax
                 :clause (eql ajax-action :error)
                 :action (error (join "FU" (+ 2 3))))

Response
--------

    (defun respond-example (example)
      (let* ((title (join "Example " (keyword-name (id example))))
             (content (page-content example)))
        (make-instance 'html-response
                       :appendix (path-content "view/google-analytics.html")
                       :script   (list "jquery")
                       :style    (list "common")
                       :title    title
                       :meta    '(("description" "Examples are for learning.")
                                  ("author"      "May B. Yew"))
                       :content  (with-view-data ('title title
                                                  'text content)
                                   (path-view "view/example-page.html")))))

@TODO
-----

* Write about parsers, pookies, etc.
* Add a test site system.
