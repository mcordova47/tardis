# Elmish Time Travel

```purs
main :: Effect Unit
main = defaultMain
  { def: withTimeTravel { init, update, view }
  , elementId: "app"
  }
```

![tardis](https://github.com/user-attachments/assets/295e9eaf-0ee8-4eea-8a37-8a278f604270)
