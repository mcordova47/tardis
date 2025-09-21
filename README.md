# Elmish Time Travel

```purs
main :: Effect Unit
main = defaultMain
  { def: withTimeTravel { init, update, view }
  , elementId: "app"
  }
```

![tardis](https://github.com/user-attachments/assets/e6b3f46c-8d30-4331-938c-e1f67cb96949)
