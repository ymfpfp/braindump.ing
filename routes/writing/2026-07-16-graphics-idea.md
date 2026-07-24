---
date: 2026-07-16
title: Something on GPUs I would like to write, or pixels2tensors
layout: writing
---

There is this project that I would like to actualize: I'd like to write a book on everything GPUs, from rendering pixels to encoding tensors, inspired by a mix of something like [Making Software](https://www.makingsoftware.com/) and [Crafting Interpreters](https://craftinginterpreters.com/).

The thesis is to present a story about GPUs to curious people by doing hands-on experiments, like writing a font renderer from scratch. The story is that two of the most relevant things in computing (certainly right now) — graphics and machine learning[^1] — are directly related because they're both tied to massive parallelism - and that parallelism happens to be in the form of GPUs. And maybe doing these hands-on experiments will really clarify: how did we get to where we are today?

A lot of the reason why I want to do this is because the Feynman Technique - teach something to really understand it - really appeals to me.

And the truth is that I feel like I myself have not fully internalized a lot of this knowledge. Also sometimes I need a reference of mine to read back! And I figure since I have a bunch of personal notes on this stuff I should just consolidate it into one.

Also it's on my bucket list to write a piece of nonfiction.

A brief table of contents might look like:

- Introduction/Acknowledgements/Notes
- Part I: Pixels
- Part II: GPU
- Part III: Silicon
- Part IV: Tensors
- Bonuses
- Appendices

Below follows a quick outline of what each part could potentially look like.

## Part I: Pixels

This part would walk through the high-level abstractions of actually getting pixels onto the screen, i.e. with a high-level API like WebGL. The question I had when I decided to learn about this stuff was the question 10-year-old me had, which is how video games were created (well, more specifically, how could I create them).

Have you see [Minecraft](https://en.wikipedia.org/wiki/Minecraft)? You could learn how that works here.

Have you see [Figma](https://figma.com), and wonder how they are able to render SVGs and text? You could learn how that works here.

Have you see [Pretext](https://github.com/chenglou/pretext) recently? You could learn how that works here.

A table of contents for this part might look like:

1. _Hello, World! Rendering A Cube_: Do the classic rendering a square in clip space, then render a cube in clip space that looks like a square, and then leave on a cliffhanger for transformations: "Why does it look exactly the same?"
2. _How Shaders Work_: The "Hello, world!" introduces two ideas that might be a little bit jarring: VAOs and shaders. This part talks about the second and tries to instill some sort of wonder with regards to this, and also tries to lay out the idea that rendering is a very _optimized pipeline_.
3. _Making A Cube Move_: Now we look at transformations - translations, rotations, scaling - and try to figure out how to shove all of that into one singular transformation matrix. This touches on the model to world pipeline in the typical model-world-projection model.
4. _How Cameras Work_: This would touch on the world to projection in. We'd try to derive both the orthographic and perspective projection matrix, as seen [here](https://www.songho.ca/opengl/gl_projectionmatrix.html). By the end we'd be able to move around in the world with the controls you might typically expect, proper pitch/yaw/roll.
5. _Voxels: Making Many Cubes_: Now that we have the concept of a world, we would try to render many of these cubes. We'd touch upon using indexed vertices (that is, EBOs) and backface culling (that is, counterclockwise vertices).
6. _How Textures Work_: We'd talk about laying textures on cubes, how mipmapping works.
7. _How Lighting Works_: Now that we have a bunch of voxels with textures, we'd like to apply some lighting to them. We would look at lighting source, reflection/refraction, and utilize the Phong lighting model, i.e. ambient + diffuse + specular lighting.
8. _Landscaping With Noise_: Now that we can have many cubes, we can lay them out. We would segue from setting up a chunking system to discussing Perlin noise and other noise formats.
9. _Lighting Optimizations: Flood Fill/Ambient Occlusion_: At this point in time, we can transition into more traditional lighting methods for voxel engines, which is to apply a flood-fill algorithm (e.g. breadth-first search) to each voxel side. Then we'd talk about ambient occlusion, a method for better shadows, first as a general concept, then in the concept of voxel engines, where the fact that voxels are usually.
10. _Voxel Optimizations_: We'd look at how to optimize the chunking via different methods, e.g., using workers (threads), frustum culling, etc.
11. _Rendering The Sky: Cubemaps_: Figure out how to render the sky as a cubemap, then figure out day/night cycles and the sun as a light source.
12. _Transparency_: Figure out transparent objects (glass, water). We'll implement blending and order-dependent transparency, and maybe look at order-independent transparency.
13. _Rendering Text, Part I: How Fonts & Bezier Curves Work_: When I started messing around with this, I was entirely nerdsniped by it - I would keep thinking, "This is so cool!" - so the goal of this chapter is to instill this same wonder. We'll write a font parser ([TTF file format](https://en.wikipedia.org/wiki/TrueType)), which will involve learning about Bezier curves as well.
14. _Rendering Text, Part II: Rendering On The GPU_: We'll figure out how to render bezier curves online to render text.
15. _Rendering Text, Part III: Rendering Offline, Or How SDFs Work_: To render text offline, we'll make use of [signed distance functions](https://iquilezles.org/articles/distfunctions/) to encode the distance between pixels and curves into a fixed, packed font atlas.
16. _Rendering UI, Part I: UI Components From Shapes_:
17. _Rendering UI, Part I: Layout Algorithms_: Now that we have the ability to render text and graphics, we want to be able to render
18. _Rendering UI, Part II: Text Measurements_: We'll learn a bit about modern day text rendering libraries, like
19. _Optimizations & Testing_
20. _Bonus: Storing And Loading Maps_: i.e., let's load up some [Anvil maps](https://minecraft.wiki/w/Anvil_file_format) and see how they perform!
21. _Bonus: Multiplayer_

So by the end of this part you can kind of begin to see, "Hey! This is how a video game works!" And if you're anything like me that would bring out a bit of the childhood wonder in you :)

## Part II: GPU

At this point, we can go even lower-level. So we know that the large majority of the code we write for placing pixels on the screen ends up getting passed into shaders, which we know conceptually are just $f(x, y) = \text{color}$, where $\text{color}$ is usually $(r, g, b, a)$.

One of the big-picture goals here is to walk away with an idea of the advancements we've made in terms of GPUs over the last twenty years. I want the

The thing I'm curious about is that

1. _Writing A Raytracer, Part I_
2. _Writing A Raytracer, Part II: Parallelization With SIMD_
3. _It's Just Not Fast Enough! GPUs To The Rescue_: I actually don't know if this is true. CPUs are really bloody fast nowadays. That being said, I really like the title, and I really like it as a segue,
4. _A Gander At CUDA_
5. _Writing A Raytracer, Part III: Rewriting In CUDA_
6. _Writing A Raytracer, Part IV: Other Optimizations_
7. _What It Takes To Render The Moana Scene_: I actually don't know yet but I would like to find out. Being able to render a Disney scene has to feel really good, same way I imagine kicking off a training run.
8. _Writing A Renderer, Part I_
9. _Writing A Renderer, Part II: Directly Interfacing_

## Part III: Silicon

At this point, we can see why we need GPUs. And we have an idea of how they work.

But also to me, when it first

## Part IV: Tensors

This is the last core part

1. _Woah, This Machine Does Learning! 101_: Learn
2. _Writing A Tensor Framework_
3. _Tensors In Hardware_
4. _Taping Out & Testing_

## Bonuses

Just a bunch of random, related topics that I couldn't stuff into the actual parts:

1. _A Gander At Vulkan & Metal_: I think learning WebGPU roughly encompasses the necessary abstractions needed to understand these lower-level APIs. At the same time, I personally would really like to mess around with these APIs. Maybe write a 3D object renderer or something.
2. _Messing Around With Guassian Splats_
3. _Building The Simulator_

## Appendices

The appendices would contain the bare minimum info to read the actual stuff. I'm a firm believer in ["You are not dumb, you just lack the prerequisites"](https://lelouch.dev/blog/you-are-probably-not-dumb/).

1. _Just Enough Computer Architecture_: I think understanding how CPUs work is probably a good pretext to seeing how computer architecture works
2. _Just Enough Color Science_: While writing out the table of contents for [_Part II_](#part-ii-gpu) I realized that I assume that people know that computers render RGB color triplets typically. This is not necessarily true; thus, a appendix on color science and other fascinating info!
3. _Just Enough Trigonometry_
4. _Just Enough Calculus_
5. _Just Enough Linear Algebra_

= chapters total

So yeah. I'm going to try and carve out a little bit of space and time in my life to work on this :) It really fulfills sort of an ethos I [have](). Ideally each part would take me roughly three months at the max... so timeline is at the max end of 2027. I am realizing all of this is quite ambitious.

If you are interested in funding this sort of thing so I can spend more time on it - please email me at jc at this website.
