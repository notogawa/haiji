# haiji

A typed template engine, subset of jinja2

![CI](https://github.com/notogawa/haiji/workflows/CI/badge.svg)

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

### Filters

* abs
* length

~~~~
{{ foo|length|abs }}
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

#### Assignments

~~~~
{% for y in ys %}
{% set prev_loop = loop %}
{% for x in xs %}
{{ prev_loop.index0 }} {{ loop.index0 }}
{% endfor %}
{% endfor %}
~~~~

Don't support

* immediate value assignment
* multiple targets
* namespace objects
* block assignments

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

### Expressions

#### Literals

Integer Literals

~~~~
{{ 123 }}
~~~~

Boolean Literals

~~~~
{{ true }}
{{ false }}
~~~~

#### Arithmetics

~~~~
{{ foo + bar}}
{{ foo - bar}}
{{ foo * bar}}
{{ foo // bar}}
{{ foo % bar}}
{{ foo ** bar}}
~~~~

#### Comparisons

~~~~
{{ foo == bar }}
{{ foo != bar }}
{{ foo > bar }}
{{ foo >= bar }}
{{ foo < bar }}
{{ foo <= bar }}
~~~~

#### Logic

~~~~
{{ foo and bar }}
{{ foo or bar }}
~~~~

### Functions

* range

~~~~
{% for i in range(n) %}{{ i }}{% endfor %}
~~~~

## Testing

The tests can be run with `stack`:

~~~~
$ stack test
~~~~

In order to run the tests, you need a `python2` binary on your `PATH`, and the
`jinja2` python library installed.
