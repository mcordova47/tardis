# Elmish Time Travel

```purs
main :: Effect Unit
main = defaultMain
  { def: withTimeTravel { init, update, view }
  , elementId: "app"
  }
```

![tardis](https://github.com/user-attachments/assets/53aa4eed-ff35-45d2-85d1-76f187b46733)
