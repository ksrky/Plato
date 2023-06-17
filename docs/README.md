# Writing documentation

## Setting up environment

```
$ pipenv install
```

## Creating new documentation

```
$ mkdocs new <name>
```

## Bulding Web server

```
$ mkdocs build
```

## Deploying docs

```
$ mkdocs build -f en/mkdocs.yml
$ cd site
$ mkdocs gh-deploy --config-file en/mkdocs.yml --remote-branch master
```
