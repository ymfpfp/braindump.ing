---
date: 2026-07-24
title: Something on GPUs I would like to write, or "From Pixels To Tensors"
layout: writing
---

There is this project that I would like to actualize: I'd like to write a series of volumes on everything GPUs, from rendering pixels to encoding tensors, inspired by a mix of something like [Making Software](https://www.makingsoftware.com/) and [Crafting Interpreters](https://craftinginterpreters.com/). It'd be called _From Pixels To Tensors_.

The thesis is to present a story about GPUs to curious people by doing hands-on experiments, like writing a font renderer from scratch. The story is that two of the most relevant things in computing (certainly right now) — graphics and machine learning — are directly related because they're both tied to massive parallelism — and that parallelism happens to be in the form of GPUs. And maybe doing these hands-on experiments will really clarify: how did we get to where we are today? What is the evolution here?

Some reasons why I want to do this off the top of my head:

- The Feynman Technique — teach something to really understand it — really appeals to me. Also, I want to spread the joy that I've had talking about this stuff in comfortable environments and explore something I'm curious about in more depth.
- Graphics textbooks can be kind of dense and unrewarding and FOR WHAT. Graphics is supposed to be about ART; it's visually appealing and is a subfield that can be so intuitively appreciated, even by children. Wouldn't it be nice for this to be **the** graphics textbook that people reach for? It covers most of what you might find in a typical graphics programming course, if not more[^1]. On the other side of the spectrum is a bunch of resources you have to cobble together. (This is not to dunk on the greats like _OpenGL Superbible_, _Real-Time Rendering_, _Physically Based Rendering_, etc., just pointing out that these can feel like references or impenetrable sometimes.)
- Also, open source. Open access can be annoying.
- Coming back here after I finished writing this — this project feels very [_The Art Of Computer Programming_](https://www-cs-faculty.stanford.edu/~knuth/taocp.html)-esque.

[^1]: Based off the table of contents below, a bit of computer architecture, a bit of compilers, a bit of distributed systems, a bit of hardware...

And the truth is that I feel like I myself have not fully internalized a lot of this knowledge. Also sometimes I need a reference of mine to read back! And I figure since I have a bunch of personal notes on this stuff I should just consolidate it into one.

Also it's on my bucket list to write a longform piece of nonfiction.

One argument against this is whether or not it's worth it to put all the effort in; something I'm thinking about. Arguments in favor of: GPUs are probably going to be around for a bit.

A brief table of contents might look like:

- Introduction/Acknowledgements/Notes
- Part I: Pixels
- Part II: GPU
- Part III: Silicon
- Part IV: Tensors
- Bonuses
- Appendices

Language wise, a mix of: TypeScript and CUDA; I kind of want to introduce Rust, but I don't think that will actually add anything. The ideal case would be literate programming but I don't think that'll work since there are so many specifics.

Below follows a quick outline of what each part could potentially look like. As you get further down the list you might notice that the list gets a bit more sparse — I'm not sure what these parts will really look like until I write the thing.

## Part I: Pixels

This part would walk through the high-level abstractions of actually getting pixels onto the screen, i.e. with a high-level API like WebGL. The question I had when I decided to learn about this stuff was the question 10-year-old me had, which is how video games were created (well, more specifically, how could I create them).

Have you seen [Minecraft](https://en.wikipedia.org/wiki/Minecraft)? You could learn how that works here.

Have you seen [Figma](https://figma.com), and wonder how they are able to render SVGs and text? You could learn how that works here.

Have you seen [Pretext](https://github.com/chenglou/pretext) recently? You could learn how that works here.

A table of contents for this part might look like:

1. _Hello, World! Rendering A Cube_: Do the classic rendering a square in clip space, then render a cube in clip space that looks like a square, and then leave on a cliffhanger for transformations: "Why does it look exactly the same?"
2. _How Shaders Work_: The "Hello, world!" introduces two ideas that might be a little bit jarring: VAOs and shaders. This part talks about the second and tries to instill some sort of wonder with regards to this, and also tries to lay out the idea that rendering is a very _optimized pipeline_.
3. _Making A Cube Move_: Now we look at transformations — translations, rotations, scaling — and try to figure out how to shove all of that into one singular transformation matrix. This touches on the model to world pipeline in the typical model-world-projection model.
4. _How Cameras Work_: This would touch on the world to projection matrix. We'd try to derive both the orthographic and perspective projection matrix, as seen [here](https://www.songho.ca/opengl/gl_projectionmatrix.html). By the end we'd be able to move around in the world with the controls you might typically expect, proper pitch/yaw/roll.
5. _Voxels: Making Many Cubes_: Now that we have the concept of a world, we would try to render many of these cubes. We'd touch upon using indexed vertices (that is, EBOs) and backface culling (that is, counterclockwise vertices).
6. _How Textures Work_: We'd talk about laying textures on cubes, how filtering, mipmapping, texture units, etc. works. Other things that could probably be touched upon here: antialiasing, framebuffers?
7. _How Lighting Works_: Now that we have a bunch of voxels with textures, we'd like to apply some lighting to them. We would look at lighting source, reflection/refraction, and utilize the Phong lighting model, i.e. ambient + diffuse + specular lighting. I think touching upon other lighting models at this point could be useful here too.
8. _Landscaping With Noise_: Now that we can have many cubes, we can lay them out. We would segue from setting up a chunking system to discussing Perlin noise and other noise formats.
9. _Lighting Optimizations: Flood Fill/Ambient Occlusion_: At this point in time, we can transition into more traditional lighting methods for voxel engines, which is to apply a flood-fill algorithm (e.g. breadth-first search) to each voxel side. Then we'd talk about ambient occlusion, a method for better shadows, first as a general concept, then in the concept of voxel engines, where the fact that voxels are at fixed-degree angles to each other simplifies things.
10. _Voxel Optimizations_: We'd look at how to optimize the chunking via different methods, e.g., using workers (threads), frustum culling, etc.
11. _Rendering The Sky: Cubemaps_: Figure out how to render the sky as a cubemap, then figure out day/night cycles and the sun as a light source.
12. _Transparency_: Figure out transparent objects (glass, water). We'll implement blending and order-dependent transparency, and maybe look at order-independent transparency.
13. _Rendering Text, Part I: How Fonts & Bezier Curves Work_: When I started messing around with this, I was entirely nerdsniped by it — I would keep thinking, "This is so cool!" — so the goal of this chapter is to instill this same wonder. We'll write a font parser ([TTF file format](https://en.wikipedia.org/wiki/TrueType)), which will involve learning about Bezier curves as well. We'll also touch upon the typical modern text rendering pipeline here, which will be relevant in the next couple of chapters.
14. _Rendering Text, Part II: Rendering On The GPU_: We'll figure out how to render bezier curves online to render text.
15. _Rendering Text, Part III: Rendering Offline, Or How SDFs Work_: To render text offline, we'll make use of [signed distance functions](https://iquilezles.org/articles/distfunctions/) to encode the distance between pixels and curves into a fixed, packed font atlas.
16. _Rendering UI, Part I: UI Components From Shapes_: Now that we have the ability to render text and graphics, we want to be able to render a proper UI. Let's make use of SDFs to render shapes, and construct UI primitives (e.g., buttons, text input, etc.).
17. _Rendering UI, Part II: Layout Algorithms_: If you've ever used something like CSS flex/grid or some UI framework like [SwiftUI](https://developer.apple.com/swiftui/), you might wonder what's actually happening behind the scenes; how does auto layout work when we don't provide fixed dimensions? We'll figure that out by implementing a subset of the [flex algorithm](https://www.w3.org/TR/css-flexbox-1/#layout-algorithm).
18. _Rendering UI, Part III: Text Measurements_: An extension to layout algorithms — we talk about laying out text given constraints — this is what libraries like [Pretext](https://github.com/chenglou/pretext) do — and write a simple function to do so. By the end of this we'll be able to render all the UI screens we could possibly want for our voxel engine.
19. _Optimizations & Testing_
20. _Bonus: Storing And Loading Maps_: i.e., let's load up some [Anvil maps](https://minecraft.wiki/w/Anvil_file_format) and see how they perform!
21. _Bonus: Multiplayer_
22. At this point, I want to introduce WebGPU, so we can go down to a lower-level API. Part of this is to understand why we want to jump down to that lower-level and provide some transition before the next part. I'm just not sure how to work this in though...

So by the end of this part you can kind of begin to see, "Hey! This is how a video game works!" And if you're anything like me that would bring out a bit of the childhood wonder in you :)

## Part II: GPU

At this point, we can go even lower-level. We know that there's some sort of rendering pipeline that was doing a lot of the heavy lifting for us. What if we could write that rendering pipeline from scratch, or at least parts of it, to see how it works? We can write our own rasterizer to try and do this, and maybe even try to get our voxel engine to work.

Another one of the big-picture goals in this part is to walk away with an idea of the advancements we've made in terms of GPUs over the last twenty years. I remember [reading](https://fabiensanglard.net/cuda/index.html) about the evolution of NVIDIA GPUs and finding it fascinating that NVIDIA moved away from having a fixed-function pipeline towards a more general purpose design that was optimized for general parallelism:

> Up to 2006, NVidia's GPU design was correlated to the logical stages in the rendering API. The GeForce 7900 GTX, powered by a G71 die is made of three sections dedicated to vertex processing (8 units), fragment generation (24 units), and fragment merging (16 units)...
>
> This correlation forced designers to guess the location of bottlenecks in order to properly balance each layers [sic]. With the emergence of yet another stage in DirectX 10, the geometry shader, Nvidia engineers found themselves faced with the difficult task of balancing a die without knowing how much a stage was going to be adopted. It was time for a change.
>
> Nvidia solved the problem of escalating complexity with its "unified" Tesla architecture, released in 2006.
>
> In the G80 die, there is no more distinction between layers. The Stream Multiprocessor (SM) replaces all previous units thanks to its ability to run vertex, fragment and geometry "kernel" without distinction. The load balancing happens automatically by swapping the "kernel" run by each SM depending on the need of the pipeline.

Conveying this to the reader would be pretty awesome; **after all, part of the thesis is that graphics and machine learning are two sides of the same coin**. One of the things I really struggle with as someone who hasn't been in the field of computing for >= 10,000 hours is that iteration is really, really relevant, and it's really hard to get anything right on the first try!

So something like starting from implementing a rasterizer on the CPU and looking at these fixed functions we're implementing and realizing that getting our voxel engine to be working pretty smoothly is kind of difficult &rarr; and then taking a look at GPU history and seeing a solution to our problem &rarr; rewriting our code to use the GPU instead seems to be a pretty good line of thought here.

1. _Rasterizing A Cube_: Recall the rendering pipeline, determine that the easiest way to start is to figure out how to rasterize lines, then triangles, and then finally a cube. Begin to draw analogs to Part I: we'll leave off at the cliffhanger of being able to see a square.
2. _Cameras In Our Rasterizer_
3. _Rasterizing Many Cubes_: Try to render many cubes. Observe that it is quite slow/hard.
4. _It's Just Not Fast Enough! GPUs To The Rescue_: Not sure if it's actually not fast enough given how good[^2] CPUs are nowadays, but use that as the segue into talking about GPU history and how modern GPUs work (SIMT, warps, "streaming multiprocessor", kernels, memory Hierarchy).
5. _A Gander At CUDA_
6. _Rewriting Our Rasterizer_: Also cover landscaping in Part I here.
7. _Shaders From Scratch_: No DSL, just understanding what exactly is happening with fragment shaders behind the scenes.
8. _Textures From Scratch_
9. _Lighting In Our Rasterizer_
10. _Raytracing: Another Rendering Method_: What's the segue here? I guess observe that despite being the most common method, there are several drawbacks to rasterizing in terms of render quality that physically-based rendering doesn't have, and also observe that modern GPU architecture also lines up with this evolution (if you will). Also write in CUDA.
11. _Writing A Raytracer: Optimizations_: Will definitely be split up into multiple parts, just need to decide how. Acceleration structures/sampling/denoising/etc.
12. _What It Takes To Render The Moana Scene_: Ideally here is where we emphasize _massive_ parallelism. While trying to see what it takes to render the [_Moana_ Island scene](https://disneyanimation.com/resources/moana-island-scene/), we'll realize that to do things fast we still need a pretty big amount of memory, processors, etc. I've never done this before so I just think this will be so cool and a great buildup.

[^2]: And how they're kind of converging towards GPU architecture... would it actually be helpful to slot in SIMD somewhere?

To do all this, I see most people doing parts of this in the cloud with an attached GPU, especially for the last part. If people have an NVIDIA GPU that of course makes things easier, AMD has their transpilers for CUDA; and people with M-Series MacBooks like me finally miss out on something!

From here on out, the remaining chapters aren't quite laid out as the rest and are more so just the briefest outline. Let me know if I'm missing anything.

## Part III: Silicon

At this point, we can see why we need GPUs. And we have an idea of how they work. We've also gone pretty low-level, and now when we see magic colors on our screen we know how they got there!

But also to me, it's kind of hard to internalize how something how something works without actually building it out. So what I'm thinking is a good idea to do here is similar to what [Nand2Tetris](https://www.nand2tetris.org/) does — have a simulator for building this out and getting to see what really happens, all the way down to the silicon.

To see what exactly we're doing, here is a simplified diagram of NVIDIA's H100 GPU, generated from their [whitepaper](https://resources.nvidia.com/en-us-hopper-architecture/nvidia-h100-tensor-c) (pape 19):

<div class="demo">
  <style>
    #post article .demo .gpu {
      display: flex;
      flex-direction: column;
      gap: 0.4rem;
    }

    #post article .demo .gpu .bar {
      font-weight: 600;
    }

    /* The scheduler is the one orange band, echoing NVIDIA's own diagrams. */
    #post article .demo .gpu .sched {
      background-color: #f6a623;
      border-color: #d98c00;
      color: #3a2600;
    }

    /* The die core: an HBM3 memory controller flanks each row of the compute
       cluster, so the labels line up with the GPC banks and the L2 band. */
    #post article .demo .gpu .cluster {
      display: flex;
      flex-direction: column;
      gap: 0.4rem;
    }

    #post article .demo .gpu .cluster .row {
      gap: 0.4rem;
      align-items: stretch;
    }

    #post article .demo .gpu .hbm {
      flex: 0 0 3.6rem;
      display: flex;
      align-items: center;
      justify-content: center;
      background-color: var(--border);
      border: 1px solid var(--darker-border);
      color: #666;
      font-size: 0.7rem;
      letter-spacing: 0.05em;
      padding: 0.4rem 0.3rem;
      text-align: center;
    }

    /* L2 is the wide blue band the two GPC banks share. */
    #post article .demo .gpu .l2 {
      flex: 1;
      font-weight: 600;
    }

    /* Each GPC is a green compute unit textured with a lattice of SMs, drawn with
       gradients so the density needs no extra markup. */
    #post article .demo .gpu .gpc {
      min-height: 5.5rem;
      justify-content: flex-start;
      gap: 0.4rem;
      background-color: color-mix(in srgb, var(--theme) 32%, var(--foreground));
      background-position: center 1.7rem;
    }

    #post article .demo .gpu .gpc > .title {
      align-self: flex-start;
      color: #2f4a12;
      font-size: 0.75rem;
    }

    /* NVLink lanes: a strip of small green units along the bottom edge. */
    #post article .demo .gpu .links {
      display: grid;
      grid-template-columns: repeat(18, 1fr);
      gap: 0.25rem;
    }

    #post article .demo .gpu .links i {
      background-color: color-mix(in srgb, var(--theme) 55%, var(--foreground));
      border: 1px solid color-mix(in srgb, var(--theme) 70%, var(--foreground));
      height: 1.1rem;
    }

    @media screen and (max-width: 576px) {
      #post article .demo .gpu .row { flex-wrap: wrap; }
      #post article .demo .gpu .gpc { min-width: 6rem; }
      #post article .demo .gpu .links { grid-template-columns: repeat(9, 1fr); }
    }

  </style>
  <div class="gpu">
    <div class="layer hardware bar">PCI Express 5.0 Host Interface</div>
    <div class="layer sched bar">GigaThread Engine with MIG Control</div>
    <div class="cluster">
      <div class="row">
        <span class="hbm">HBM3</span>
        <div class="box gpc"><span class="title">Graphics Processing Cluster</span></div>
        <div class="box gpc"><span class="title">Graphics Processing Cluster</span></div>
        <div class="box gpc"><span class="title">Graphics Processing Cluster</span></div>
        <div class="box gpc"><span class="title">Graphics Processing Cluster</span></div>
        <span class="hbm">HBM3</span>
      </div>
      <div class="row">
        <span class="hbm">HBM3</span>
        <div class="layer l2">L2 Cache</div>
        <span class="hbm">HBM3</span>
      </div>
      <div class="row">
        <span class="hbm">HBM3</span>
        <div class="box gpc"><span class="title">Graphics Processing Cluster</span></div>
        <div class="box gpc"><span class="title">Graphics Processing Cluster</span></div>
        <div class="box gpc"><span class="title">Graphics Processing Cluster</span></div>
        <div class="box gpc"><span class="title">Graphics Processing Cluster</span></div>
        <span class="hbm">HBM3</span>
      </div>
    </div>
    <div class="layer hardware bar">High-Speed Hub</div>
    <p class="note">HBM3 = High Bandwidth Memory; High Speed Hub consists of NVIDIA's proprietary NVLink connectors.</p>
  </div>
</div>

We will be skipping all the external connectors (GigaThread Engine, HBM3, High Speed Hub, PCIe), leaving us to implement caching — straightforward — and an equivalent of the _Graphics Processing Cluster_.

In this case each cluster is made up of individual generalized _Streaming Multiprocessors_, which perform the general SIMT. To clarify, NVIDIA GPUs tend to still have specialized hardware within each cluster, but rather than having fixed function parts these are just abstractions on top of their Streaming Multiprocessors[^3].

[^3]: E.g., H100 has Texture Processing Cluster.

As seen on page 21 of the same whitepaper:

<div class="demo">
  <style>
    #post article .demo .sm {
      display: flex;
      flex-direction: column;
      gap: 0.4rem;
    }

    #post article .demo .sm > .title {
      font-size: 1.1rem;
      font-weight: 700;
    }

    /* The four processing blocks (SM sub-partitions) sit in a 2x2 grid. */
    #post article .demo .sm .blocks {
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 0.5rem;
    }

    #post article .demo .sm .block {
      display: flex;
      flex-direction: column;
      gap: 0.3rem;
      background-color: var(--foreground);
      border: 1px solid var(--darker-border);
      padding: 0.4rem;
    }

    #post article .demo .sm .block .layer {
      font-size: 0.8rem;
      padding: 0.35rem;
    }

    /* Coloured control bands inside each block. */
    #post article .demo .sm .sched {
      background-color: #f6a623;
      border-color: #d98c00;
      color: #3a2600;
      font-weight: 600;
    }

    #post article .demo .sm .dispatch {
      background-color: #c2611c;
      border-color: #9d4d13;
      color: #fff;
    }

    #post article .demo .sm .regfile {
      background-color: #2f6c7c;
      border-color: #234f5b;
      color: #fff;
      font-weight: 600;
    }

    /* Datapath: four lane columns (INT32 / FP32 / FP32 / FP64) beside the
       tensor core. Each lane is striped with a gradient to imply its ~16 stacked
       cores, so the density needs no extra markup. */
    #post article .demo .sm .cores {
      display: flex;
      gap: 0.3rem;
    }

    #post article .demo .sm .lanes {
      display: flex;
      flex: 2.1;
      gap: 0.2rem;
    }

    #post article .demo .sm .lane {
      --cell: 1.1rem;
      flex: 1;
      min-height: 8rem;
      display: flex;
      flex-direction: column;
      border: 1px solid rgba(0, 0, 0, 0.12);
      color: #1f3a10;
      font-size: 0.6rem;
      font-weight: 700;
      /* Cell lines start at the top and repeat every --cell, so the first line
         lands right under the label and the label reads as the first cell. */
      background-image: repeating-linear-gradient(
        to bottom,
        transparent 0 calc(var(--cell) - 1px),
        rgba(255, 255, 255, 0.55) calc(var(--cell) - 1px) var(--cell)
      );
    }

    #post article .demo .sm .lane > span {
      display: flex;
      align-items: center;
      justify-content: center;
      height: var(--cell);
    }

    #post article .demo .sm .lane.int {
      background-color: color-mix(in srgb, var(--theme) 40%, var(--foreground));
    }

    #post article .demo .sm .lane.fp {
      background-color: color-mix(in srgb, var(--theme) 24%, var(--foreground));
    }

    #post article .demo .sm .lane.fp64 {
      flex: 1.3;
      background-color: color-mix(in srgb, var(--theme) 58%, var(--foreground));
      color: #12280a;
    }

    #post article .demo .sm .tensor {
      flex: 1.15;
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      text-align: center;
      background-color: color-mix(in srgb, var(--theme) 70%, var(--foreground));
      border: 1px solid color-mix(in srgb, var(--theme) 80%, black);
      color: #fff;
      font-size: 0.8rem;
      font-weight: 700;
      line-height: 1.2;
    }

    #post article .demo .sm .tensor .gen {
      font-size: 0.7rem;
      font-weight: 400;
    }

    /* Load/store units and the special-function unit run along the bottom. */
    #post article .demo .sm .ldst {
      display: flex;
      gap: 0.2rem;
    }

    #post article .demo .sm .ldst span {
      flex: 1;
      background-color: #8f4444;
      border: 1px solid #6f3434;
      color: #fff;
      font-size: 0.6rem;
      font-weight: 700;
      padding: 0.3rem 0.1rem;
      text-align: center;
    }

    #post article .demo .sm .ldst .sfu {
      flex: 1.3;
      background-color: #7a3a3a;
    }

    /* Shared, SM-wide units below the blocks. */
    #post article .demo .sm .tma {
      background-color: #7ab63f;
      border-color: #5e9130;
      color: #12280a;
      font-weight: 700;
    }

    #post article .demo .sm .tex {
      display: grid;
      grid-template-columns: repeat(4, 1fr);
      gap: 0.4rem;
    }

    #post article .demo .sm .tex .box {
      background-color: #2f56b0;
      border-color: #22408a;
      color: #fff;
      font-weight: 600;
    }

    @media screen and (max-width: 576px) {
      #post article .demo .sm .blocks { grid-template-columns: 1fr; }
      #post article .demo .sm .tex { grid-template-columns: repeat(2, 1fr); }
    }

  </style>
  <div class="sm">
    <div class="layer cache">L1 Instruction Cache</div>
    <div class="blocks">
      <div class="block">
        <div class="layer cache">L0 Instruction Cache</div>
        <div class="layer sched">Warp Scheduler (32 thread/clk)</div>
        <div class="layer dispatch">Dispatch Unit (32 thread/clk)</div>
        <div class="layer regfile">Register File (16,384 x 32-bit)</div>
        <div class="cores">
          <div class="lanes">
            <div class="lane int"><span>INT32</span></div>
            <div class="lane fp"><span>FP32</span></div>
            <div class="lane fp"><span>FP32</span></div>
            <div class="lane fp64"><span>FP64</span></div>
          </div>
          <div class="tensor"><span>TENSOR CORE</span><span class="gen">4th Generation</span></div>
        </div>
        <div class="ldst">
          <span>LD/ST</span><span>LD/ST</span><span>LD/ST</span><span>LD/ST</span><span>LD/ST</span><span>LD/ST</span><span>LD/ST</span><span>LD/ST</span><span class="sfu">SFU</span>
        </div>
      </div>
      <div class="block">
        <div class="layer cache">L0 Instruction Cache</div>
        <div class="layer sched">Warp Scheduler (32 thread/clk)</div>
        <div class="layer dispatch">Dispatch Unit (32 thread/clk)</div>
        <div class="layer regfile">Register File (16,384 x 32-bit)</div>
        <div class="cores">
          <div class="lanes">
            <div class="lane int"><span>INT32</span></div>
            <div class="lane fp"><span>FP32</span></div>
            <div class="lane fp"><span>FP32</span></div>
            <div class="lane fp64"><span>FP64</span></div>
          </div>
          <div class="tensor"><span>TENSOR CORE</span><span class="gen">4th Generation</span></div>
        </div>
        <div class="ldst">
          <span>LD/ST</span><span>LD/ST</span><span>LD/ST</span><span>LD/ST</span><span>LD/ST</span><span>LD/ST</span><span>LD/ST</span><span>LD/ST</span><span class="sfu">SFU</span>
        </div>
      </div>
      <div class="block">
        <div class="layer cache">L0 Instruction Cache</div>
        <div class="layer sched">Warp Scheduler (32 thread/clk)</div>
        <div class="layer dispatch">Dispatch Unit (32 thread/clk)</div>
        <div class="layer regfile">Register File (16,384 x 32-bit)</div>
        <div class="cores">
          <div class="lanes">
            <div class="lane int"><span>INT32</span></div>
            <div class="lane fp"><span>FP32</span></div>
            <div class="lane fp"><span>FP32</span></div>
            <div class="lane fp64"><span>FP64</span></div>
          </div>
          <div class="tensor"><span>TENSOR CORE</span><span class="gen">4th Generation</span></div>
        </div>
        <div class="ldst">
          <span>LD/ST</span><span>LD/ST</span><span>LD/ST</span><span>LD/ST</span><span>LD/ST</span><span>LD/ST</span><span>LD/ST</span><span>LD/ST</span><span class="sfu">SFU</span>
        </div>
      </div>
      <div class="block">
        <div class="layer cache">L0 Instruction Cache</div>
        <div class="layer sched">Warp Scheduler (32 thread/clk)</div>
        <div class="layer dispatch">Dispatch Unit (32 thread/clk)</div>
        <div class="layer regfile">Register File (16,384 x 32-bit)</div>
        <div class="cores">
          <div class="lanes">
            <div class="lane int"><span>INT32</span></div>
            <div class="lane fp"><span>FP32</span></div>
            <div class="lane fp"><span>FP32</span></div>
            <div class="lane fp64"><span>FP64</span></div>
          </div>
          <div class="tensor"><span>TENSOR CORE</span><span class="gen">4th Generation</span></div>
        </div>
        <div class="ldst">
          <span>LD/ST</span><span>LD/ST</span><span>LD/ST</span><span>LD/ST</span><span>LD/ST</span><span>LD/ST</span><span>LD/ST</span><span>LD/ST</span><span class="sfu">SFU</span>
        </div>
      </div>
    </div>
    <div class="layer tma">Tensor Memory Accelerator</div>
    <div class="layer cache">256 KB L1 Data Cache / Shared Memory</div>
    <div class="tex">
      <div class="box">Tex</div>
      <div class="box">Tex</div>
      <div class="box">Tex</div>
      <div class="box">Tex</div>
    </div>
    <p class="note">There is no term for the individual execution units here. <code>Tex</code> refers to the Texture Processing Cluster (TPC) I mentioned.</p>
  </div>
</div>

For our GPU, we'll most likely handle FP16 versus 32/64 bits, meaning that in the next part we might need to quantize the model, and will have three layers of instruction caching. Things we do on our toy GPU will be constrained by the fact that we have no memory past the highest memory cache. My thinking is that all the memory our kernels will need will fit into this highest memory cache, and our kernels' working set will also fit it in. The scope for implementing memory buses/interconnect is out of scope here, but we'll discuss how modern GPU communication works toward the end.

This will also reduce the problem set, e.g. we won't have to handle cache eviction at the highest-level cache.

1. _Manufacturing Circuits_: Discuss how circuits are manufactured.
2. _Logic Gates_: Introduction to our simulator, try to build out some basic logic gates.
3. _Arithmetic Circuits_
4. _Clocks & Registers_: State.
5. _Constructing A Processor_: At this point we have a simple fetch-decode-execute processor. This is basically what you derive in something like [Nand2Tetris](https://www.nand2tetris.org/). This probably involves discussing some sort of ISA for our GPU that is simple and extensible in future chapters.
6. _Control Flow_
7. _SIMT/SIMD_: Discuss parallelism.
8. _Extending The Memory Hierarchy_: Build out the higher level caches.
9. _Building The Streaming Multiprocessor_: Warps, etc.
10. _Compiling A Kernel_: Compile a kernel down to our ISA by writing a quick compiler to try and convert some custom language we'll design down.
11. _A Hello, World! Kernel_: Moment of truth: let's test tiny kernel.
12. _Interconnect Protocols In Modern GPUs_: Purely reading chapter, good time to also discuss HPC.

The end goal of this is to understand what NVIDIA/AMD/etc. are doing in this day and age.

## Part IV: Tensors

This is the last core part. GPUs nowadays have tensor units directly in the hardware for matrix computations, as you can see in the diagram above: this is where we tie in that part and really fulfill the namesake of this whole series. The goal is to see our machine learning model

1. _Woah, This Machine Does Learning! 101_: Learn how multilayer perceptrons work. Classic MNIST from scratch is what I'm thinking. Get a set of hyperparameters and parameters for inference in a later chapter.
2. _Writing A Tensor Framework_: Write a tiny tensor framework with the kernel
3. _Inference_: Discuss quantization,
4. _Tensors In Hardware_: Discuss tensor units, try to write
5. _Taping Out & Testing_: Most definitely multiple parts, I'm just not sure what this looks like yet[^4].

[^4]: I've never done this, I would really like to do this.

## Bonuses

Just a bunch of random, related topics that I couldn't stuff into the actual parts. I'm not sure if this will be a part of the book or just here on my personal website.

1. _A Gander At Vulkan & Metal_: I think learning WebGPU roughly encompasses the necessary abstractions needed to understand these lower-level APIs. At the same time, I personally would really like to mess around with these APIs. Maybe write a 3D object renderer or something.
2. _Messing Around With Gaussian Splats_
3. _Building The Simulator_: To be honest might be a job for LLM given the scope of everything else.

## Appendices

The appendices would contain the bare minimum info to read the actual stuff. I'm a firm believer in ["You are not dumb, you just lack the prerequisites"](https://lelouch.dev/blog/you-are-probably-not-dumb/).

1. _Just Enough Color Science_: While writing out the table of contents for [_Part II_](#part-ii-gpu) I realized that I assume that people know that computers render RGB color triplets typically. This is not necessarily true; thus, an appendix on color science and other fascinating info!
2. _Just Enough Trigonometry_
3. _Just Enough Calculus_: Understand as mathematics of change. Limits, differentiation, chain rule, partial derivatives.
4. _Just Enough Linear Algebra_: Gradients.

&approx; around 70 chapters total.

So yeah. I'm going to try and carve out space and time in my life to work on this :) It really fulfills sort of an ethos I have: to explore what I'm interested in while making along the way. I am realizing all of this is quite ambitious.

If you are interested in funding this sort of thing so I can spend more time on it — please email me at jc at this website.

---

Diagrams rendered with LLM assistance.
