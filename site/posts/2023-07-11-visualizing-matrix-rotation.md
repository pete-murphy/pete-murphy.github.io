---
author: Me
date: Jul 11, 2023
tags: [typescript, react, framer-motion, algorithm]
---

# Visualizing matrix rotation

I made a little React app for visualizing how an in-place matrix rotation algorithm works.

You can click a square to select it, press <kbd>Enter</kbd> once you've got four selected to swap them in order of selection, and <kbd>Esc</kbd> to clear the selection. A green background means that square has been correctly rotated.

<video controls autoplay>
  <source src="../images/2023-07-12-matrix.mp4" type="video/mp4">
</video>

Here it is in a CodeSandbox:

<iframe style="border: 1px solid rgba(0, 0, 0, 0.1);border-radius:2px;" width="100%" height="450" src="https://codesandbox.io/p/sandbox/solitary-feather-9zp9gc?embed=1" allowfullscreen></iframe>
