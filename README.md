# Elmish Time Travel

```purs
main :: Effect Unit
main = defaultMain
  { def: withTimeTravel { init, update, view }
  , elementId: "app"
  }
```

![tardis2](https://github.com/user-attachments/assets/edd68077-110e-4d9c-8433-a4207b505644)
