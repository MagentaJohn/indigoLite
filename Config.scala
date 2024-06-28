package game

import indigo.*

// DO NOT EDIT: Generated by Indigo.
object Config:

  val config: GameConfig =
    GameConfig(
      viewport = GameViewport(550, 400),
      frameRateLimit = Option(FPS.`60`),
      clearColor = RGBA.fromHexString("#000000"),
      magnification = 1,
      transparentBackground = false,
      resizePolicy = ResizePolicy.Resize,
      advanced = AdvancedGameConfig(
        renderingTechnology = RenderingTechnology.WebGL2WithFallback,
        antiAliasing = false,
        premultipliedAlpha = true,
        batchSize = 256,
        autoLoadStandardShaders = true,
        disableContextMenu = true
      )
    )