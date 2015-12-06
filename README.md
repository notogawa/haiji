# haiji

A typed template engine, subset of jinja2

[![Build Status](https://travis-ci.org/notogawa/haiji.svg?branch=master)](https://travis-ci.org/notogawa/haiji)
[![Coverage Status](https://img.shields.io/coveralls/notogawa/haiji.svg)](https://coveralls.io/r/notogawa/haiji?branch=master)

Haiji is a template engine which is subset of jinja2.
This is designed to free from the unintended rendering result
by strictly typed variable interpolation.

## Typed Template

~~~~
{{ foo }}
~~~~

For example, this jinja2 template requires "foo".
A dictionary which provides a variable "foo" is required to render it.
If a variable "foo" does not exist in a given dictionary,
jinja2 evaluates it to an empty string,
whereas haiji treats this case as compile error.

## Support Features

### Variables

You can use a dot to access attributes of a variable.

~~~~
{{ foo.bar }}
~~~~

### Control Structures

#### If

~~~~
{% if foo %}true{% else %}false{% endif %}
~~~~

#### For

~~~~
{% for bar in foo %}
  loop: {{ bar }}
{% endfor %}
~~~~

#### Include

~~~~
{% include "parts.html" %}
~~~~

#### Extends

Base template
~~~~
<html>
<head>
{% block head %}
  <title>{{ foo }}{% block title %}{% endblock %}</title>
{% endblock %}
</head>
<body>
  {{ bar }}
  {% block missing %}child missing this block{% endblock %}
</body>
</html>
~~~~

Child template
~~~~
{% extends "parent.tmpl" %}
{% block title %}{{ super() }}{{ baz }}{{ super() }}{% endblock %}
{% block head %}
 {{ super() }}
 <style type="text/css"></style>
{% endblock %}
~~~~

#### Escaping

Raw block
~~~~
{% raw %}
  <ul>
  {% for item in seq %}
    <li>{{ item }}</li>
  {% endfor %}
  </ul>
{% endraw %}
~~~~

### Comments

~~~~
{# a comment #}
~~~~
