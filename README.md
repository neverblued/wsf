Web Site Framework
==================

Создать сайт:

```cl
(defvar site (make-instance 'website :system '#:your-system))
```

Замените YOUR-SYSTEM на имя системы ASDF, из которой движок узнает путь к папке проекта.
Необязательный параметр PORT для установки HTTP-порта по умолчанию равен 8666.

Маршрутизация
-------------

```cl
(with-server site
```

Простой маршрут:

```cl
  (set-route :examples
             :link "/examples"
             :action (respond-examples))
```

Маршрут с параметрами:

```cl
  (let ((prefix "/example/"))
    (set-route :example
               :args (example)
               :link (join prefix (keyword-name (id example)))
               :clause-with-scope t
               :scope ((uri (script-name*)))
               :clause (begins-with? uri prefix)
               :action (awhen (example (name-keyword (trim-left prefix uri)))
                         (respond-example it))))
```

Маршрут для клиентских скриптов и темы оформления:

```cl
  (set-route :static
             :clause-with-scope t
             :scope ((uri (script-name*)))
             :clause (iter (for path in '("/js/" "/css/" "/image/"))
                           (when (begins-with? uri path)
                             (leave t)))
             :action (handle-static-file (docroot/ uri)))
```

Публичная папка с маскировкой пути:

```cl
  (set-route :public
             :clause-with-scope t
             :scope ((uri (script-name*)))
             :clause (scan (join-by "|"
                                    "^/robots\\.txt$"
                                    "^/kinda-root-folder/")
                           uri)
             :action (handle-static-file (docroot/ "public" uri)))
```

Подключить стандартный маршрут для AJAX:

```cl
  (set-route-ajax)
```

"/ajax/test" => { status: "win", data: { key: "value" }}

```cl
  (set-route :asynchronous-test
             :follow :ajax
             :clause (eql ajax-action :test)
             :action (list :key "value"))
```

"/ajax/error" => { status: "fail", data: "FU5" }

```cl
  (set-route :error-test
             :follow :ajax
             :clause (eql ajax-action :error)
             :action (error (join "FU" (+ 2 3))))
```

Ответы
------

```cl
(defun respond-example (example)
  (let* ((title (join "Example " (keyword-name (id example))))
         (content (page-content example)))
    (make-instance 'html-response
                   :appendix (text-docroot/ "view/google-analytics.html")
                   :script   (list "jquery")
                   :style    (list "common")
                   :title    title
                   :meta    '(("description" "Examples are for learning.")
                              ("author"      "May B. Yew"))
                   :content  (with-view-data ('title title 'content content)
                               (view-docroot/ "view/example-page.html")))))
```

@ДОДЕ
-----

* Написать про парсеры, пуки и т.д.
* Сделать тестовый сайт.
