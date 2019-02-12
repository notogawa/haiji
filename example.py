#!/usr/bin/python
from jinja2 import Environment, PackageLoader

if __name__ == '__main__':
    env = Environment(loader=PackageLoader('example', '.'))
    template = env.get_template('example.tmpl')
    print(template.render(
        a_variable = "Hello,World!",
        navigation = [ { 'href': 'content/a.html', 'caption': 'A'},
                       { 'href': 'content/b.html', 'caption': 'B'} ]
    ))
