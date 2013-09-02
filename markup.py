def head(title):
    return """<!doctype html>
        <style>
            body {{
                background-color: #EEEEEE;
                color: #666666;
                font: large monospace;
            }}

            a {{
                color: #666666;
                display: block;
                padding: 5px;
                text-decoration: none;
            }}

            a.directory {{
                font-weight: bold;
            }}

            h1 a {{
                color: #000000;
                display: inline;
            }}

            .upload-form {{
                border-top: 3px solid #000;
                margin-top: 20px;
            }}

        </style>
        <title>{title} | Truss</title>
    """.format(
        title=title
    )


def breadcrumbs(crumbs):
    markup = ''

    for name, uri in crumbs:
        markup += '<a href="{uri}">{name}</a>/'.format(
            uri=uri,
            name=name)

    return '<h1>{}</h1>'.format(markup)


def file(name, uri, klass):
    return '<a class="{klass}" href="{uri}">{icon}{name}</a>'.format(
        name=name,
        uri=uri,
        klass=klass,
        icon = '&#8627 ' if klass == 'directory' else '')


def upload_form():
    return """
        <div class="upload-form">
            <h3>Upload files.</h3>
            <form method="post" enctype="multipart/form-data" action="">
                <input type="file" name="uploaded-files" multiple="multiple">
                <input type="submit" value="upload">
            </form>
        </div>
    """
