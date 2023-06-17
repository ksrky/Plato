# Writing documentation

## MkDocs

### Setting up environment

```
$ pip install -r requirements.txt
```

or

```
$ pipenv install
```

### Creating new documentation

```
$ mkdocs new <name>
```

### Building Web server

```
$ mkdocs serve
```

### Deploying docs

```
$ mkdocs build -f en/mkdocs.yml
$ cd site
$ mkdocs gh-deploy --config-file en/mkdocs.yml --remote-branch master
```

### More info

For full documentation visit [mkdocs.org](https://www.mkdocs.org).
