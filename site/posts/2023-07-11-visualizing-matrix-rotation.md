---
author: Me
date: Jul 11, 2023
tags: [typescript, react, framer-motion, algorithm]
---

# In-place matrix rotation (a visual aid)

I made a little React app for visualizing how an in-place matrix rotation algorithm works, because the existing textual (or even image/video-based) explanations I came across felt lacking.

You can click a square to select it, press <kbd>Enter</kbd> once you've got four selected to swap them in order of selection, and <kbd>Esc</kbd> to clear the selection. A green background means that square has been correctly rotated.

<video controls autoplay>
  <source src="../images/2023-07-12-matrix.mp4" type="video/mp4">
</video>

Here it is in a CodeSandbox:

<iframe style="border: 1px solid rgba(0, 0, 0, 0.1);border-radius:2px;" width="100%" height="450" src="https://codesandbox.io/p/sandbox/solitary-feather-9zp9gc?embed=1" allowfullscreen></iframe>

At my job we were briefly using array rotation as an interview prompt where the in-place algorithm was a "bonus" exercise, so this visual aid was good to have on hand to help with an intuition if a candidate was stuck.
