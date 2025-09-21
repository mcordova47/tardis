# Elmish Time Travel

```purs
main :: Effect Unit
main = defaultMain
  { def: withTimeTravel { init, update, view }
  , elementId: "app"
  }
```
