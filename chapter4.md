# Build yourself a website

https://haskell-at-work.com/episodes/2018-04-09-your-first-web-application-with-spock.html


`stack new notekeeper`

add spock into the `package.yaml`, so you package.yaml looks like

```yaml
dependencies:
- base >= 4.7 && < 5
- Spock
```

```stack install ghcid```

`ghcid -T :main`
