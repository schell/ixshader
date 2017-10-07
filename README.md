# ixshader
`ixshader` is a shallow embedding of the OpenGL Shading Language in Haskell. It
aims to look as close to actual glsl shader code as possible, while providing
better compile-time safety. Currently writing shader code in `ixshader`'s
`IxShader` monad will catch variable assignment mismatches, multiplication
mismatches and some other common errors. It also builds a description of your
shader at the type level to use downstream during buffering and uniform updates.
Lastly, it abstracts over shader code written for opengl and webgl.

Since this is a work in progress the entire language is not yet supported,
though you can easy use the `nxt` and `acc` functions to push your own glsl into
the pipeline, typechecking only what you want.

The resulting source is pretty and human readable, even debuggable without
sourcemapping.

## example

```haskell
myvertex
  :: forall ctx. HasContext ctx
  => IxShader ctx '[] '[ In      Xvec2 "position"
                       , In      Xvec4 "color"
                       , Uniform Xmat4 "projection"
                       , Uniform Xmat4 "modelview"
                       , Out     Xvec4 "fcolor"
                       , Out     Xvec4 "gl_Position"
                       , Main
                       ] ()
myvertex = do
  pos    <- in_
  color  <- in_
  proj   <- uniform_
  modl   <- uniform_
  fcolor <- out_
  glPos  <- gl_Position
  main_ $ do
    fcolor .= color
    glPos  .= proj .* modl .* (pos .: 0.0 .: 1.0)

myfragment
  :: forall (ctx :: GLContext). IsGLContext ctx
  => IxShader ctx '[] '[ In  Xvec4 "fcolor"
                       , Out Xvec4 (GLFragName ctx)
                       , Main
                       ] ()
myfragment = do
  fcolor <- in_
  glFrag <- gl_FragColor
  main_ $ glFrag .= fcolor


main = do
  putStrLn "First OpenGL:"
  putSrcLn $ vertex @'OpenGLContext
  putStrLn "\nThen WebGL:"
  putSrcLn $ vertex @'WebGLContext
{-
First OpenGL:
in vec2 position;
in vec4 color;
uniform mat4 projection;
uniform mat4 modelview;
out vec4 fcolor;
void main ()
{ fcolor = color;
  gl_Position = projection * modelview * vec4 (position.x, position.y, 0.0, 1.0);
}

Then WebGL:
attribute vec2 position;
attribute vec4 color;
uniform mat4 projection;
uniform mat4 modelview;
varying vec4 fcolor;
void main ()
{ fcolor = color;
  gl_Position = projection * modelview * vec4 (position.x, position.y, 0.0, 1.0);
}
-}
```
