---
date: 2026-09-04
title: Solving the Jane Street ASIC reverse-engineering puzzle
layout: writing
---

Jane Street recently published a [puzzle](https://blog.janestreet.com/can-you-reverse-engineer-an-asic/) on reverse engineering an ASIC. Given a layout file:

![GDS visualization](https://cdn.braindump.ing/writing/asic-puzzle/gds.png)

We need to figure out a) what this ASIC does, and b) try to coax a value out of it that doesn't read like gibberish.

Let's start with what we're given! First off, if we download the [repo](https://github.com/janestreet/asic-puzzle-2026), we can see that we're given the GDS file for this as shown above (which we can view with [KLayout](https://www.klayout.de/) or the [TinyTapeout GDS viewer](https://gds-viewer.tinytapeout.com/) as mentioned in the puzzle description); an image of the layout that is supposed to "hint at its functionality"; and an example waveform so we can see the relationship between the inputs and outputs.

![Layout image](https://cdn.braindump.ing/writing/asic-puzzle/layout.png)

From the layout image, I got kind of hopeful because you can see that all the inputs we can toggle are single-bit. While we don't know that the functionality of each of these inputs necessarily match up to the respective label, let's just assume they're true, and take a gander at what this chip might do!

* Whatever we are doing, there's some sort of sequential logic going on, since there's a clock! A sequential circuit can be represented by a [finite state machine](https://en.wikipedia.org/wiki/Finite-state_machine).
* We only have one bit of serious input, `input`, which indicates we are **serially communicating** something.
* Therefore, we can derive this as some of state machine where we have to input the right set of input bits — $\text{input} = \{0, 1\}[]$, to set `success` to high. You can call this whatever you want, e.g. it's a password checker or whatever. 

Other than this info, there isn't much more use we're going to get out of this picture, I think. But now our guess is that if this is a state machine, we can figure out how to get it to a more useful representation, namely:

$$f(S, I) = S'$$

where $S$ is the set of all state and $I$ is the set of changing inputs. This is actually overly verbose because $I$ is technically part of $S$, but I find this to be clarifying.

Once we're there we can figure out how to brute force it, or maybe we'll be able to notice patterns on our own. 

Next up is the waveform (to open it, the puzzle mentioned [Surfer](https://surfer-project.org/), which is what I used to inspect the puzzle): 

![Waveform visualization](https://cdn.braindump.ing/writing/asic-puzzle/surfer.png)

I'm not an expert at this so right now I'm not really deriving any useful info from this; maybe this could come in handy later though, since in a puzzle there is a purpose to everything! There are three Easter eggs (or ish) in the waveform file though — .vcd files are just text:

* The date is funny, "Sat Dec 31 23:59:60 2016". At first I thought it was just peculiar because "2016", but apparently this is a leap date.
* Version: "Leave no stone unturned! But for this file, consider looking at it in a waveform viewer instead."
* The output, which at each step/cycle is one byte, produces the ASCII sequence "TRY AGAIN" twice.

## Extracting the netlist

If we open the GDS file in KLayout or TinyTapeout — I used the latter for this part — we can see that the cell instances have their types intact:

![Cell instances](https://cdn.braindump.ing/writing/asic-puzzle/cells.png)

This is really really good, because the [SkyWater PDK](https://github.com/google/skywater-pdk) is open source and we can use it to derive a netlist from the GDS file. I wasn't really sure how to do this so I asked ChatGPT/Claude. It turns out there there's this thing called LVS, which is short for Layout Vs. Schematic.

Essentially, it's a script you write to confirm that the layout matches a given schematic — a [SPICE](https://ibiblio.org/kuphaldt/socratic/model/mod_spice_semi.pdf?curius=3118) netlist. Specifically, it converts a layout back to SPICE, then tries to make sure it's functionally equivalent to a SPICE netlist you pass in. This means that we can remove the latter step — we don't have a SPICE netlist on hand — and just use the script to derive the SPICE netlist.

KLayout's documentation has a wonderful introduction to LVS [here](https://www.klayout.de/doc-qt5/manual/lvs_intro.html?curius=3118) that has an example of a LVS script. I won't go into too much more in detail here.

Writing this LVS script by hand is tedious, so it's either write/prompt a program to do it, or find a program that already exists. Fortunately, [Efabless](https://www.tomshardware.com/tech-industry/semiconductors/efabless-shuts-down-fate-of-tiny-tapeout-chip-production-projects-unclear) has a LVS script that we can install inside KLayout by going to Tools > Manage Packages > searching ["Efabless\_sky130 1.0.7"](https://github.com/efabless/sky130_klayout_pdk/) and installing it.

We can pull that script into our puzzle directory and then remove the parts that do the comparison. Alternatively, you can also find it [here](https://github.com/efabless/sky130_klayout_pdk/blob/main/tech/sky130/lvs/sky130.lvs) in their repo.

Removing all the parts that we don't need:

```diff
--- original.lvs	
+++ sky130.lvs	
@@ -438,20 +438,6 @@
 
 # Instantiate a reader using the new delegate
 reader = RBA::NetlistSpiceReader::new(SubcircuitModelsReader::new)

-#=== GET NETLIST ===
-if $schematic
-  schematic($schematic, reader)
-  logger.info("Netlist file: #{$schematic}")
-else
-  begin  
-    schematic("#{source.cell_name}.cdl", reader)
-    logger.info("Netlist file: #{source.cell_name}.cdl")
-  rescue 
-    puts "No schematic loaded , please add your netlist file"
-    logger.error("No schematic loaded, please add your netlist file")
-  end 
-end
 
 class BResistor < RBA::DeviceClassResistorWithBulk
   def initialize
@@ -2637,9 +2623,6 @@
 
 logger.info("Starting SKY130 LVS comparison section")
 
-#=== FLATTEN CELLS ===
-align

 #=== NETLIST EXTRACTION ===
 if SIMPLIFY
   netlist.simplify
@@ -2666,31 +2649,11 @@
   netlist.purge_nets
 end
 
-#=== SCHEMATIC OPTIONS ===
-if SCH_SIMPLE
-  schematic.simplify
-end

 #=== IGNORE EXTREME VALUES ===
 max_res(1e9)
 min_caps(1e-16)
 
-#=== COMPARE ===
-compare

 #=== TIME ESTIMATE ===
 exec_end_time = Time.now
 run_time = exec_end_time - exec_start_time
 logger.info("LVS Total Run time %f seconds" % [run_time])

-#=== DECISION ===
-if ! compare
-  logger.info("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
-  logger.error("ERROR : Netlists don't match")
-  logger.info("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
-  exit(1)
-else
-  logger.info("==========================================")
-  logger.info("INFO : Congratulations! Netlists match.")
-  logger.info("==========================================")
-end
```

Then run that with:

```
what's the klayout thing for this TODO
```

This gives us a SPICE netlist, wooh!

```
* Extracted by KLayout with SKY130 LVS runset on : 30/08/2026 16:36

.SUBCKT puzzle I O[0] O[1] O[2] O[3] O[4] O[5] O[6] O[7] VGND VPWR clk enable
+ rst_n success
X$1 VPWR VGND VPWR \$148 \$16 \$1 \$31 VGND sky130_fd_sc_hd__o21a_2
X$8 VPWR VPWR \$89 \$23 \$1 VGND VGND sky130_fd_sc_hd__and2_2
X$9 VGND \$1412 \$3588 \$3918 VPWR VPWR VGND sky130_fd_sc_hd__dfxtp_2
X$10 VGND \$1363 \$3588 \$3994 VPWR VPWR VGND sky130_fd_sc_hd__dfxtp_2
X$11 VGND \$1362 \$3066 \$4002 VPWR VPWR VGND sky130_fd_sc_hd__dfxtp_2
X$12 VGND rst_n \$2651 \$3066 \$3069 VPWR VPWR VGND sky130_fd_sc_hd__dfstp_2
	
* ...
```

## Transpiling to Verilog

Ah, a new piece of info to work with! Too bad I don't actually have any idea of what 
these cells do logically; it's pretty difficult to parse in its current state. Our next steps therefore are to convert this to Verilog and then to simplify down the cells to their fundamental logic gates.

For converting to Verilog, I had an agent write a SPICE to Verilog parser, which was relatively quick and self-contained. The `spice2verilog` my agent wrote in action produces something like this from the SPICE netlist:

```verilog
module puzzle (
    I,
    O,
    VGND,
    VPWR,
    clk,
    enable,
    rst_n,
    success
);
    input I;
    output [7:0] O;
    inout VGND;
    inout VPWR;
    input clk;
    input enable;
    input rst_n;
    output success;
    wire _148;
    wire _16;
    wire _1;
	// ...
	sky130_fd_sc_hd__o21a_2 X_1 (.VPB(VPWR), .VGND(VGND), .VPWR(VPWR), .X(_148), .B1(_16), .A2(_1), .A1(_31), .sky130_gnd(VGND));
sky130_fd_sc_hd__and2_2 X_8 (.VPB(VPWR), .VPWR(VPWR), .A(_89), .B(_23), .X(_1), .VGND(VGND), .sky130_gnd(VGND));
sky130_fd_sc_hd__dfxtp_2 X_9 (.VGND(VGND), .Q(_1412), .CLK(_3588), .D(_3918), .VPWR(VPWR), .VPB(VPWR), .sky130_gnd(VGND));
sky130_fd_sc_hd__dfxtp_2 X_10 (.VGND(VGND), .Q(_1363), .CLK(_3588), .D(_3994), .VPWR(VPWR), .VPB(VPWR), .sky130_gnd(VGND));
sky130_fd_sc_hd__dfxtp_2 X_11 (.VGND(VGND), .Q(_1362), .CLK(_3066), .D(_4002), .VPWR(VPWR), .VPB(VPWR), .sky130_gnd(VGND));
sky130_fd_sc_hd__dfstp_2 X_12 (.VGND(VGND), .SET_B(rst_n), .Q(_2651), .CLK(_3066), .D(_3069), .VPWR(VPWR), .VPB(VPWR), .sky130_gnd(VGND));
	// ...
endmodule
```

## Verifying what we've got

At this point in time, I thought it was a good time to verify that the two steps we've taken so far, GDS &rarr; SPICE &rarr; Verilog, actually produce output that we might expect. To do this, we would have to compare our generated Verilog netlist to a golden Verilog netlist.

Of course, we don't have one of those. And this is where the warmup puzzle comes in handy, because it does have a golden Verilog netlist, [`warmup/01_netlist.v`](https://github.com/janestreet/asic-puzzle-2026/blob/master/warmup/01_netlist.v)!

We can do this comparison with a tool like [Yosys](https://yosyshq.readthedocs.io/projects/yosys/en/latest/introduction.html)[^1].

[^1] I only knew about this because I was messing around with Yosys to do the exact opposite of what this puzzle is about. See the [notes](#notes) at the end of this post.

Yosys is "an open source framework for RTL synthesis". RTL stands for Register Transfer Level. Basically Yosys lets you do manipulations on HDLs that describe hardware as a list of instructions that manipulate and move data between registers, eventually synthesizing such an abstraction down to actual flip flops. 

In this case, Verilog is such an HDL, so Yosys can parse it into an IR and then manipulate it. Along the way, it can simplify the IR and then compare it to another simplified IR. In Yosys this looks like:

```
read_verilog -sv 01_netlist.v
read_verilog -sv produced_netlist.v

# Process into Yosys' internal representation.
proc

memory
# Flatten so cells are directly nested.
flatten
# Optimize, this is made of the smaller Yosys optimize commands.
opt

equiv_make adder_demo produced equiv
equiv_simple equiv
equiv_status -assert
```

Of course, this won't actually work:

```

```

This is because we need the functional Verilog for the cells! Fortunately, the [PDK]() has the functional Verilog for each cell that our 

```
blank
```

After having an agent compile all of this into a single script and running that:

```
PASS: 01_netlist.v and produced_netlist.v are functionally equivalent.
```

Nice! Of course, in retrospect, I could have waited until fully decompiling down to logic gates to do this verification. Hindsight is 20/20, though, and we still need the logic gates to  

After doing some research (chatting with LLM + poking through the Yosys [reference](https://yosyshq.readthedocs.io/projects/yosys/en/latest/cmd-cmd.html?curius=3118)), I came up with, roughly, the following Yosys script:

```sh
script cells_fn.ys
# `puzzle` is the top-level design we're looking at.
hierarchy -top puzzle
# Process into Yosys' internal RTL representation.
proc
# Optimize, this is made of the smaller Yosys optimize commands.
opt

# Flatten `puzzle` so cells are directly nested.
flatten
# Generic techmapping, convert to actual logical circuits. 
techmap
# Optimize again.
opt

# Simplify logic gates.
abc -g simple
# Optimize again.
opt_clean

# Replace async flip-flops with synchronous ones to match clock.
async2sync
# Simplify flip-flops down to one single kind, _DFF_P_.
dffunmap

# Select all the flip-flops and their connections.
select -list t:$_DFF_P_ %ci*
# Create a submodule out of what's been selected.
submod -name logic
select logic
flatten logic
write_verilog -noattr logic.v
```

After running this and outputting the resulting representation to Verilog, we finally have the nuts and bolts of our logic in `logic.v`:

```

```

This is really, really good! Well, it's good in that we can now 

Here I got a bit lost and decided that maybe the right next step to translate this into a more understandable representation of what a state machine is. Specifically, I wanted to reason about this as a literal state transition function: I wanted to have a function

Ok, so at this point it's pretty easy to tell that this is not for a human to tease out. 

Once again, before we go any further we've hit the stage

This seems like we could try to pass this into a solver. My main worry would be the number of iterations this would take — in theory we could go on ad infinitum!, but the hope is that the constraints here limit the choices we have and thus reduce the search space.

I asked ChatGPT what was the best general-purpose solver I could get up and running with and it gave me Microsoft's Z3.

Z3, from what I can understand, is a collection of solvers that can perform SMT. To understand SMT, you have to understand SAT, which is short for satisfiability. SAT allows you to solve for boolean equations like::

By relying on the fact that boolean equations are commutative and thus we can group parts into AND statements, OR statements, and individual unary statements (NOT, or a variable by itself) to simplify the equation, determine.

Two prominent algorithms for this are DPLL and CDCL. Z3 uses the latter, and builds. I haven't read it yet, but I believe the main authors of Z3 have written a draft on the [internals](https://z3prover.github.io/papers/z3internals.html) that I'd like to read sometime.

To use Z3, I found [this](https://ar-ms.me/thoughts/a-gentle-introduction-to-z3/?curius=3118) to be useful. Anyways, how do we convert our environment to translate cleanly into Z3? We can 

Running this gives us our answer:

```
[solve] single-solve N=2 ...
	unsat at N=2
[solve] single-solve N=4 ...
	unsat at N=4
[solve] single-solve N=8 ...
	unsat at N=8
[solve] single-solve N=16 ...
	unsat at N=16
[solve] single-solve N=32 ...
	unsat at N=32
[solve] single-solve N=64 ...
	unsat at N=64
[solve] single-solve N=128 ...
[solve] SAT (enable_fixed=-1): success first at cycle 128
I sequence: 00000001010100001000000100000010101010000000000001010000100010000001000000100000101000010000000010000001000000100100010100000000
en sequence: 11111111111111111111111011111111111111111111111111111110011111011111111111111111111111111111101111111111101111111111111111111110
O (hex) at success: 0x28 | chars: (
```

Now that we have a sequence, we can simulate this until `success` is flipped low again. In this case, we can simply take advantage of our transition function to simulate this, really cleanly in fact:

```python
import dataclasses
import logic

I_BITS = "00000001010100001000000000000101010100000000000010100000010000010000001000001010000100000001000000100000100100010100000000"
TAIL_BITS = "0" * 64


def fmt(o):
    if 32 <= o <= 126:
        return f"0x{o:02x} ({chr(o)})"
    return f"0x{o:02x} (none)"


bits = [int(c) for c in I_BITS] + [int(c) for c in TAIL_BITS]
st = logic.initial_state()
chars = []
seen_ascii = False

print(f"feeding up to {len(bits)} bits (prefix {len(I_BITS)} + tail), enable=1, rst_n=1\n")

step = 0
for bit in bits:
    step += 1
    st = logic.next_state(
        dataclasses.replace(st, I=bit, enable=1, rst_n=1),
        debug_signals=False,
    )
    _, sig = logic.next_state(st, debug_signals=True)
    o = sig["O"]
    ascii_ok = 32 <= o <= 126

    if seen_ascii and not ascii_ok:
        print(f"step {step:3d}: I={bit}  O={fmt(o)}  ASCII=' '  success={getattr(st, 'success')}  <- stopped: no longer ASCII")
        break
    if ascii_ok:
        seen_ascii = True

    ch = chr(o) if ascii_ok else ""
    chars.append(ch)
    ascii_ch = ch if ch else " "
    print(f"step {step:3d}: I={bit}  O={fmt(o)}  ASCII='{ascii_ch}'  success={getattr(st, 'success')}")

msg = "".join(chars)
print("\nfull message displayed:", repr(msg))
```

Running this:

```
step 122: I=0  O=0x28 (()  ASCII='('  success=1
step 123: I=0  O=0x2a (*)  ASCII='*'  success=1
step 124: I=0  O=0x20 ( )  ASCII=' '  success=1
step 125: I=0  O=0x54 (T)  ASCII='T'  success=1
step 126: I=0  O=0x57 (W)  ASCII='W'  success=1
step 127: I=0  O=0x4f (O)  ASCII='O'  success=1
step 128: I=0  O=0x20 ( )  ASCII=' '  success=1
step 129: I=0  O=0x53 (S)  ASCII='S'  success=1
step 130: I=0  O=0x54 (T)  ASCII='T'  success=1
step 131: I=0  O=0x41 (A)  ASCII='A'  success=1
step 132: I=0  O=0x52 (R)  ASCII='R'  success=1
step 133: I=0  O=0x53 (S)  ASCII='S'  success=1
step 134: I=0  O=0x20 ( )  ASCII=' '  success=1
step 135: I=0  O=0x2a (*)  ASCII='*'  success=1
step 136: I=0  O=0x29 ())  ASCII=')'  success=1
step 137: I=0  O=0x00 (none)  ASCII=' '  success=1  <- stopped: no longer ASCII

full message displayed: '(* TWO STARS *)'
```

That doesn't look like gibberish to me, yay :)

## Notes

Takeaways from this:

* Having never done this before, I took a couple of extra steps that I probably didn't need to take. I tried to detail that in this post, at the expense of maybe being a bit confusing :)
* LLMs are really good for discardable tasks like this where code quality doesn't matter (i.e. I don't read it, I just need to verify the output is what I would expect). What would take me a day or two is maybe about half an hour, and the LLMs can also write temp verification scripts to check its output. I did this puzzle with a shitty free model.
	* My assumption is that you can probably also just give this puzzle directly to an agent and it'll be able to solve it. It's much more fun to have an idea of what's going on though.
* Like I mentioned, I took some time before working on the puzzle to read about how all of this stuff worked, since I didn't actually understand anything that was going on. Specifically I read about how FPGAs work (I figured they were related), how transistors work, how ASICs work, etc.
	* I also got nerd-sniped a bit before actually focusing on this puzzle, I admit... I wanted to understand what it would be like to go down the opposite lane so I started looking into that. (Still trying to replicate [OpenLane](https://openroad.org/) via individual steps. I even started writing a tiny Yosys 💀)
* I want to learn more about how Z3 works internally.
