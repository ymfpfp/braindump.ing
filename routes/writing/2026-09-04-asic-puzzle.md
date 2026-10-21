---
date: 2026-09-04
title: Solving the Jane Street ASIC reverse-engineering puzzle
layout: writing
---

TL;DR[^1]: I treated the ASIC as a black-box state machine. I extracted its transistor-level layout into a SPICE netlist using KLayout/LVS, converted that into Verilog, simplified it with Yosys into 92 flip-flops plus combinational logic, and transpiled that into a Python state-transition function. I then encoded the transition function symbolically in Z3 and asked it to find an input sequence that makes success go high. It found a 122-bit sequence which eventually causes the chip to output `(* TWO STARS *)`.

[^1]: Written by AI, rest of post is written by me.

Jane Street recently published a [puzzle](https://blog.janestreet.com/can-you-reverse-engineer-an-asic/) on reverse engineering an ASIC. Given a layout file:

![GDS visualization](https://cdn.braindump.ing/writing/asic-puzzle/gds.png)

We need to figure out a) what this ASIC does, and b) try to coax a value out of it that doesn't read like gibberish.

Let's start with what we're given! First off, if we download the [repo](https://github.com/janestreet/asic-puzzle-2026), we can see that we're given the GDS file for this as shown above (which we can view with [KLayout](https://www.klayout.de/) or the [TinyTapeout GDS viewer](https://gds-viewer.tinytapeout.com/) as mentioned in the puzzle description); an image of the layout that is supposed to "hint at its functionality"; and an example waveform so we can see the relationship between the inputs and outputs.

![Layout image](https://cdn.braindump.ing/writing/asic-puzzle/layout.png)

From the layout image, I got kind of hopeful because you can see that all the inputs we can toggle are single-bit. While we don't know that the functionality of each of these inputs necessarily match up to the respective label, let's just assume they're true, and take a gander at what this chip might do!

- Whatever we are doing, there's some sort of sequential logic going on, since there's a clock! A sequential circuit can be represented by a [finite state machine](https://en.wikipedia.org/wiki/Finite-state_machine).
- We only have one bit of serious input, `input`, which indicates we are **serially communicating** something.
- Therefore, we can derive this as some of state machine where we have to input the right set of input bits — $\text{input} = \{0, 1\}[]$, to set `success` to high. You can call this whatever you want, e.g. it's a password checker or whatever.

Other than this info, there isn't much more use we're going to get out of this picture, I think. But now our guess is that if this is a state machine, we can figure out how to get it to a more useful representation, namely:

$$f(S) = \{S', O\}$$

where $S$ is the set of all state and $O$ is the extra output.

Once we're there we can figure out how to brute force it, or maybe we'll be able to notice patterns on our own. **So that'll be our goal now — to simplify down to this state transition function.**

Next up is the waveform (to open it, the puzzle mentioned [Surfer](https://surfer-project.org/), which is what I used to inspect the puzzle):

![Waveform visualization](https://cdn.braindump.ing/writing/asic-puzzle/surfer.png)

I'm not an expert at this so right now I'm not really deriving any useful info from this; maybe this could come in handy later though, since in a puzzle there is a purpose to everything! There are three Easter eggs (or ish) in the waveform file though — .vcd files are just text:

- The date is funny, "Sat Dec 31 23:59:60 2016". At first I thought it was just peculiar because "2016", but apparently this is a leap second.
- Version: "Leave no stone unturned! But for this file, consider looking at it in a waveform viewer instead."
- The output, which at each step/cycle is one byte, produces the ASCII sequence "TRY AGAIN" twice.

## Extracting the netlist

If we open the GDS file in KLayout or TinyTapeout — I used the latter for this part — we can see that the cell instances have their types intact:

![Cell instances](https://cdn.braindump.ing/writing/asic-puzzle/cells.png)

This is really really good, because the [SkyWater PDK](https://github.com/google/skywater-pdk) is open source and we can use it to derive a netlist from the GDS file. I wasn't really sure how to do this so I asked an LLM. It turns out there's this thing called LVS, which is short for Layout Vs. Schematic.

Essentially, it's a script you write to confirm that the layout matches a given schematic — a [SPICE](https://ibiblio.org/kuphaldt/socratic/model/mod_spice_semi.pdf?curius=3118) netlist. Specifically, it converts a layout back to SPICE, then tries to make sure it's functionally equivalent to a SPICE netlist you pass in. This means that we can remove the latter step — we don't have a SPICE netlist on hand — and just use the script to derive the SPICE netlist.

KLayout's documentation has a wonderful introduction to LVS [here](https://www.klayout.de/doc-qt5/manual/lvs_intro.html?curius=3118) that has an example of a LVS script. I won't go into too much more in detail here.

Writing this LVS script by hand is tedious, so it's either write/prompt a program to do it, or find a program that already exists. Fortunately, [Efabless](https://www.tomshardware.com/tech-industry/semiconductors/efabless-shuts-down-fate-of-tiny-tapeout-chip-production-projects-unclear) has a LVS script that we can install inside KLayout by going to Tools > Manage Packages > searching [`Efabless_sky130 1.0.7`](https://github.com/efabless/sky130_klayout_pdk/) and installing it.

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
klayout -b -r sky130.lvs -rd input=puzzle.gds -rd target_netlist=extracted.cir
```

This gives us a SPICE netlist at `extracted.cir`, wooh!

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

Ok, a new piece of info to work with! Too bad I don't actually have any idea of what these cells do logically; it's pretty difficult to parse in its current state. Our next steps therefore are to convert this to Verilog and then to simplify down the cells to their fundamental logic gates.

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

We can do this comparison with a tool like [Yosys](https://yosyshq.readthedocs.io/projects/yosys/en/latest/introduction.html)[^2]. Yosys is "an open source framework for RTL synthesis". RTL stands for Register Transfer Level. Basically Yosys lets you do manipulations on HDLs that describe hardware as a list of instructions that manipulate and move data between registers, eventually synthesizing such an abstraction down to actual flip flops and logic circuits. Such HDLs are said to operate at the Register Transfer Level.

[^2]: I only knew about this because I was messing around with Yosys to do the exact opposite of what this puzzle is about. See the [notes](#notes) at the end of this post.

In this case, Verilog is such an HDL, so Yosys can parse it into an IR and then manipulate it. Along the way, it can simplify the IR and then compare it to another simplified IR.
To do this in Yosys, we simply send in a list of commands and a lot of IR manipulation magic happens on the backend. We can also send in a script like so:

```sh
read_verilog 01_netlist.v
read_verilog produced_netlist.v

# Process into Yosys' internal representation.
proc
# Simplify memory.
memory
# Flatten so cells are directly nested.
flatten
# Optimize, this is made of the smaller Yosys optimize commands.
opt

check -assert -mapped
equiv_make adder_demo produced equiv
equiv_simple equiv
equiv_status -assert
```

Feel free to refer to the [Yosys command reference](https://yosyshq.readthedocs.io/projects/yosys/en/latest/cmd-cmd.html?curius=3118) for more details.

I got two errors while trying to run this. The bigger error is that both netlists make use of SkyWater cells, and Yosys has no idea what these functionally look like; in other words, we haven't mapped them yet.

![Black box visualization of `sky130_fd_sc_hd__and2_2`](https://cdn.braindump.ing/writing/asic-puzzle/box.png)

By default, Yosys is fine with treating these as a black box and ignoring them; `check -assert -mapped` asserts that that doesn't happen.

So we have to import them. Fortunately, the [PDK](https://github.com/google/skywater-pdk/tree/7198cf647113f56041e02abf3eb623692820c5e1/libraries/sky130_fd_sc_hd) has the functional Verilog for each cell that we need. For example, the warmup puzzle has a Verilog module called `adder_demo`, and in there there's `sky130_fd_sc_hd__and2_2`[^3], which we can find in [`cells/and2/sky130_fd_sc_hd__and2.functional.v`](https://github.com/google/skywater-pdk-libs-sky130_fd_sc_hd/blob/ac7fb61f06e6470b94e8afdf7c25268f62fbd7b1/cells/and2/sky130_fd_sc_hd__and2.functional.v):

[^3]: The `_2` at the end represents drive (how big the transistors are).

```verilog
`ifndef SKY130_FD_SC_HD__AND2_FUNCTIONAL_V
`define SKY130_FD_SC_HD__AND2_FUNCTIONAL_V

/**
 * and2: 2-input AND.
 *
 * Verilog simulation functional model.
 */

`timescale 1ns / 1ps
`default_nettype none

`celldefine
module sky130_fd_sc_hd__and2 (
    X,
    A,
    B
);

    // Module ports
    output X;
    input  A;
    input  B;

    // Local signals
    wire and0_out_X;

    //  Name  Output      Other arguments
    and and0 (and0_out_X, A, B           );
    buf buf0 (X         , and0_out_X     );

endmodule
`endcelldefine

`default_nettype wire
`endif  // SKY130_FD_SC_HD__AND2_FUNCTIONAL_V
```

All we need to do is grab all of these `*.functional.v` modules. Since I have them on my laptop I can just write (or, have an agent) write a shell script to just grab all the ones I need:

```sh
cat > "$WORKDIR/cut_defs.v" <<'EOF'
module sky130_fd_sc_hd__dfrtp (Q, CLK, D, RESET_B);
  input Q, D, CLK, RESET_B;
  wire unused = Q;
endmodule

module sky130_fd_sc_hd__mux2 (X, A0, A1, S);
  input A0, A1, S;
  output X;
  assign X = S ? A1 : A0;
endmodule
EOF

for base in a21bo a21boi a21o a31o and2 and3 and4bb clkbuf decap nand2 nor2 \
            o21bai or2 tapvpwrvgnd xnor2 xor2; do
  f="$SKY130_CELLS/$base/sky130_fd_sc_hd__${base}.functional.v"
  if [ ! -f "$f" ]; then
    echo "ERROR: missing official functional model: $f" >&2
    exit 1
  fi
  cp "$f" "$WORKDIR/${base}_fn.v"
done
```

In this case, the agent simply read the two Verilog modules, and pulled out all the SkyWater cells they used, LOL. It also turns out there were some modules that make use of a fancy Verilog keyword called [`primitive`](https://chipverify.com/verilog/verilog-udp), which in essence appear to let you describe a module as a custom truth table. Yosys' support for these is limited, and the PDK also doesn't include functional models for them for this reason, so here we've chosen to define them manually.

The other issue I ran into was that the Verilog netlist we generated has power ports/rails that the golden netlist doesn't have. We don't need those anyway, since we're curious about the logic of our chip. Let's just do some preprocessing:

```sh
preprocess() {
  # Strip power/body tie-off pins (not ports on the official models), normalize
  # drive-strength suffixes (dfrtp_2 -> dfrtp), and remove the empty connection
  # slots left behind by the stripped pins (e.g. "(, .A(x), ... ,)" -> "( .A(x), ...)").
  perl -pe '
    s/\s*\.(VPWR|VGND|VPB|VNB|sky130_gnd)\s*\([^)]*\)//g;
    s/sky130_fd_sc_hd__([a-z0-9]+)_[0-9]+/sky130_fd_sc_hd__$1/g;
    # Remove the empty connection slots left by stripped pins and any runs of
    # adjacent commas (looping until stable).
    1 while s/\(\s*,+/(/g;   # leading empty slot: "(, , .a(..)" -> "("
    1 while s/,\s*\)/)/g;    # trailing empty slot: ".a(..), ... ,)" -> ")"
    1 while s/,\s*,+/,/g;    # inner runs: "...,, ..." -> "... ,"
  ' "$1"
}
preprocess 01_netlist.v > "$WORKDIR/pp_01.v"
preprocess produced_netlist.v > "$WORKDIR/pp_net.v"
```

After having an agent combine all of this into a quick script to handle all of these issues and output a Yosys script, I ran that script with bated breath:

```sh
echo "using official skywater-pdk combinational .functional.v models from:"
echo "  $SKY130_FD"
echo "running formal equivalence (flop-cut, exhaustive over all register states)..."
(
  cd "$WORKDIR"
  yosys equiv.ys
) > "$WORKDIR/equiv.log" 2>&1

if grep -q "equivalence successfully proven" "$WORKDIR/equiv.log"; then
  echo "PASS: 01_netlist.v and netlist.v are functionally equivalent."
  exit 0
else
  echo "FAIL: yosys did not prove equivalence. See $WORKDIR/equiv.log" >&2
  exit 1
fi

# PASS: 01_netlist.v and produced_netlist.v are functionally equivalent.
```

Nice! Of course, in retrospect, I realized that I didn't have to do all this. I mean, if the cells are labeled correctly, they have to have the same functionality, right? But I think I was sort of confused as to what exactly a cell library was and doing this extra work helped clarify a lot.

I also could have waited until fully decompiling down to logic gates to do this verification. Hindsight is 20/20, though, and we still need the functional Verilog representation of the cells to convert the whole thing down to logic gates and flip flops for state, so that's ok. In fact, before we move on to converting the whole thing down to logic gates, I'm just going to shove a bunch of `read_verilog` calls into a `cells_fn.ys`, derived from our original puzzle netlist `produced_netlist.v`:

```sh
read_verilog /Users/jc/Programming/clones/skywater-pdk/libraries/sky130_fd_sc_hd/latest/cells/a2111oi/sky130_fd_sc_hd__a2111oi.functional.v
read_verilog /Users/jc/Programming/clones/skywater-pdk/libraries/sky130_fd_sc_hd/latest/cells/a211o/sky130_fd_sc_hd__a211o.functional.v
read_verilog /Users/jc/Programming/clones/skywater-pdk/libraries/sky130_fd_sc_hd/latest/cells/a211oi/sky130_fd_sc_hd__a211oi.functional.v
read_verilog /Users/jc/Programming/clones/skywater-pdk/libraries/sky130_fd_sc_hd/latest/cells/a21bo/sky130_fd_sc_hd__a21bo.functional.v
read_verilog /Users/jc/Programming/clones/skywater-pdk/libraries/sky130_fd_sc_hd/latest/cells/a21boi/sky130_fd_sc_hd__a21boi.functional.v

# ... 55 more lines

read_verilog cells_fn.hand.v
```

(Generated from a script)

I have no idea what any of these cells do but I don't need to :) Observe that we also need to hand roll some definitions too inside `cells_fn.hand.v`:

```verilog
// D flip-flop, positive-edge clock, async active-low reset.
module sky130_fd_sc_hd__dfrtp (Q, CLK, D, RESET_B);
  input CLK, D, RESET_B;
  output Q;
  reg Q;
  always @(posedge CLK or negedge RESET_B)
    if (!RESET_B) Q <= 1'b0;
    else          Q <= D;
endmodule

// D flip-flop, positive-edge clock, async active-low set.
module sky130_fd_sc_hd__dfstp (Q, CLK, D, SET_B);
  input CLK, D, SET_B;
  output Q;
  reg Q;
  always @(posedge CLK or negedge SET_B)
    if (!SET_B) Q <= 1'b1;
    else        Q <= D;
endmodule

// D flip-flop, positive-edge clock, no async control.
module sky130_fd_sc_hd__dfxtp (Q, CLK, D);
  input CLK, D;
  output Q;
  reg Q;
  always @(posedge CLK) Q <= D;
endmodule

// 2:1 multiplexer, X = S ? A1 : A0.
module sky130_fd_sc_hd__mux2 (X, A0, A1, S);
  input A0, A1, S;
  output X;
  assign X = S ? A1 : A0;
endmodule
```

## Simplify, simplify!

After doing some research (chatting with LLM + poking through the Yosys [reference](https://yosyshq.readthedocs.io/projects/yosys/en/latest/cmd-cmd.html?curius=3118)), I came up with, roughly, the following Yosys script to simplify down to logic gates plus flip flops.

```sh
script cells_fn.ys
# `puzzle` is the top-level design we're looking at.
hierarchy -top puzzle
proc
opt

flatten
# Generic techmapping, convert to actual logical circuits.
techmap
# Optimize again.
opt

# Simplify logic gates.
abc -g simple
# Optimize again.
opt_clean

# Replace async flip-flops with synchronous ones to match clock, if there are any.
async2sync
# Simplify flip-flops down to one single kind.
dffunmap

stat
# 74. Printing statistics.
#
# === puzzle ===
#
#         +----------Local Count, excluding submodules.
#         |
#      2525 wires
#      2532 wire bits
#      1480 public wires
#      1487 public wire bits
#         8 ports
#        15 port bits
#      1933 cells
#       412   $_AND_
#        92   $_DFF_P_
#       220   $_MUX_
#       103   $_NOT_
#       331   $_OR_
#        68   $_XOR_
#       707   $scopeinfo

# Select all the flip-flops and their connections.
select -list t:$_DFF_P_ %ci*
# Create a submodule out of what's been selected.
submod -name logic
select logic
flatten logic
write_verilog -noattr logic.v
```

[`techmap`](https://yosyshq.readthedocs.io/projects/yosys/en/latest/cmd/index_passes_techmap.html#cmd-techmap) and [`abc`](https://yosyshq.readthedocs.io/projects/yosys/en/latest/using_yosys/synthesis/abc.html) are doing most of the heavy lifting here. The former does technology mapping, which does the actual, "Hey, how do we turn these cells into, like, actual gates and wires between the gates?" ABC is a "public-domain system for logic synthesis... of binary logic circuits" as stated in this [paper](https://link.springer.com/content/pdf/10.1007/978-3-642-14295-6_5.pdf?curius=3118), but all you and I really need to know is that it's a dependency Yosys relies on to provide simplification of logic circuits by making use of Boolean algebra.

(I talk a bit about this Boolean simplification stuff later when we get to ["Solvers come to the rescue"](#solvers-come-to-the-rescue) so keep reading!)

I also want to point out `stat` as a really useful command here for seeing what's going on. Here, we can see that after a lot of simplifying, the resulting puzzle only makes use of positive-edge triggered D flip flops, shortened to `DFF_P`. In fact, it tells us there are 92 of them, which means there's 92 bits of sequential state that are being tracked in between cycles, **which means that in addition to our input bits, these are the other bits of state in $S$ in our state transition function**. We now know:

$$S = \{ \text{rst_n, enable, input, X[} 92 \text{]} \}$$

Given this info, and how each bit in $X$ actually changes over each cycle, we can derive our state transition function :)

After running this and outputting the resulting representation to Verilog, we finally have the nuts and bolts of our logic in `logic.v`. This turns out to be roughly 6000 lines of statements that look like a mix of wires that get assigned Boolean expressions, plus a set of positive edge events. Like here, `O0` through `O7` represent the output wires:

```verilog
module logic (I, clk, VGND, VPWR, enable, rst_n, success, O7, O6, O5, O4, O3, O2, O1, O0);
  // ...
  assign O0 = _0196_ & _0207_;
  assign _0208_ = _0081_ & _0153_;
  assign _0209_ = _0154_ | _0159_;
  assign _0210_ = _0081_ & \X_10.Q ;
  assign _0211_ = _0081_ & _0147_;
  assign _0212_ = _0148_ ? _0184_ : \X_539.Q ;
  assign _0213_ = _0209_ & _0212_;
  assign _0214_ = \X_408.Q  ^ _0213_;
  assign _0215_ = _0165_ & _0214_;
  assign _0216_ = ~_0215_;
  assign _0217_ = VGND | _0165_;
  assign _0218_ = _0205_ & _0217_;
  assign O7 = _0216_ & _0218_;
  // ...
  always @(posedge clk)
    _0942_ <= _0947_;
  always @(posedge clk)
    _0943_ <= _0946_;
  always @(posedge clk)
    _0944_ <= _0945_;
  always @(posedge clk)
    \X_11.Q  <= \X_11.D ;
  always @(posedge clk)
    \X_10.Q  <= \X_10.D ;
  // ... 87 more `always` blocks.
endmodule
```

There are indeed 92 more `always` blocks so we're in a really, really good spot! We have gone from an unrevealing layout file to an actual set of changing inputs.

Except there's 6000 lines of this to trace through 🫪 Let's try to construct that state transition function...

## Getting a state transition function

Now I want to reason about this as a literal state transition function: I want to have a `initial_state` function and a `next_state` function to reason about. This is just another parser to write. Having an agent do that, the parser produces:

```python
from dataclasses import dataclass, replace

@dataclass(frozen=True)
class State:
    # Inputs
    I: int = 0
    enable: int = 0
    rst_n: int = 1

    success: int = 0    # \success | storage _0914_ | reset 0

    # Registered (clocked) state bits, with their reset value
    X_408_Q: int = 1    # \X_408.Q | storage _0857_ | reset 1
    X_395_Q: int = 0    # \X_395.Q | storage _0858_ | reset 0
    X_263_Q: int = 0    # \X_263.Q | storage _0859_ | reset 0
    # ... 89 more lines


def initial_state():
    """State after rst_n has been held low: all resettable bits at reset value."""
    return State()


def next_state(state, debug_signals=True, ops=None):
    _0852_ = (_0368_ & _0401_)
    _0855_ = (_0853_ | _0854_)
    X_173_X = (_0852_ | _0855_)
    # With this, we can start pulling out patterns more obviously, like the fact that
    # rst_n does exactly what it says it does!
    _1044_ = ops.mux(rst_n, X_173_X, 1)
    # ...

    # Next-state values
    X_11_Q = X_11_D
    X_10_Q = X_10_D
    # ...

    new_state = replace(state,
        X_408_Q=_1044_,
        X_539_Q=X_15_Y,
        X_178_Q=_0949_,
        # ...,
        success=_0980_,
    )

    if not debug_signals:
        return new_state
    signals = {
        'I': I,
        'enable': enable,
        'rst_n': rst_n,
        'X_408_Q': X_408_Q,
        'X_395_Q': X_395_Q,
        'X_263_Q': X_263_Q,
        # ...
        'O': (O7 << 7) | (O6 << 6) | (O5 << 5) | (O4 << 4) | (O3 << 3) | (O2 << 2) | (O1 << 1) | O0,
    }
    return new_state, signals
```

The transpiler handles the ordering of assigning values to wires — i.e., above, you can see that wire `X_408_Q` depends on `_1044_`, which depends on `X_173_X` and `rst_n`, which depends on... well, you get the idea. This makes sense! A circuit, bar any sequential magic, is just a graph!

The transpiler handles `always` blocks by dropping whatever is referenced inside the event and merging it with the rest of this assignment, thus fully getting rid of any flip flops, the "[n]ext state values" if you will.

<img alt="Drop clk" src="https://cdn.braindump.ing/writing/asic-puzzle/drop.png" width="80%">

For `next_state`, there's an option to pass in `ops`, this is used to replace the two way multiplexer that seems to be a Yosys primitive:

```python
_CONCRETE_OPS = (
    type('_ConcreteOps', (), {
        'mux': staticmethod(lambda c, t, f: (t if c else f))
    })()
)
```

We can now simulate this chip!

## The waveform is useful, actually

Once again, before we go any further we've hit the stage where we need to verify the two steps we just did - simplifying the netlist &rarr; transpiling it to a Python program. You know what's awesome, though? Every piece of a puzzle usually is part of the puzzle[^4], and the waveform file has been laying around so far, not being put to any good use. But now we can make use of it by plugging it into our simulator.

[^4]: Unless it's a red herring, I guess.

All we have to do is parse it first. It's in VCD (Value Change Dump) form and it's literally just a dump of how the values change over time. First it defines variables:

```
$var reg 1 ! clk $end
$var reg 1 " rst_n $end
$var reg 1 # enable $end
$var reg 1 $ I $end
$var wire 8 % O [7:0] $end
$var wire 1 & success $end
```

`!` maps to `clk`, `"` maps to `rst_n`, etc., and then we assign values to these shortform variables in the actual list of timestamps. For example, in the file:

```
#1255000
b1010100 %
1!
```

At timestamp 1255000, we set `%`, which maps to `O`, to binary 1010100, which is 84 (ASCII "T"), and `!`, which maps to `clk`, to high.

Then we have a bunch of timestamps that look like this. So all we need to do is parse this, and then for each cycle/step, we can update our logic simulator and see if it produces the same state that the waveform dumps. With `run_validation("example_inputs.vcd")`:

```
PASS: all 22 comparisons matched
```

This is great, but at this point it's pretty easy to tell that this is not for a human to tease out, even in this much nicer form...

## Solvers come to the rescue

But this seems like something we could try to pass this into a constraint solver to tease out! Which I'd heard of vaguely before. My main worry would be the number of iterations this would take — in theory we could go on ad infinitum!, but the hope is that the many constraints here limit the choices we have and thus reduce the search space.

I asked an LLM what the best general-purpose solver I could get up and running with was and it gave me Microsoft's [Z3](https://github.com/z3prover/z3).

Z3, from what I can understand, is a collection of solvers that can perform SMT. To understand SMT, you have to understand SAT, which is short for satisfiability. SAT allows you to solve for boolean equations like:

$$(\neg a \vee b) \wedge (a \vee \neg b \vee c) \wedge (\neg c)$$

_How do we get this to produce true as an answer?_ By relying on the fact that boolean equations are commutative, we can group parts into AND statements (as denoted by the parentheses), OR statements, and individual unary statements (NOT, or a variable by itself) to simplify the equation. From there, we can determine constraints (i.e., $c$ above must be false for $\neg c$ to evaluate to true), and then mess around with setting variables to true/false until we either reach SATisfied or UNSATisfied (impossible to satisfy, either literally or within time/memory constraints). The last part is the bread and butter of SAT solvers.

SMT, which is short for Satisfiability Modulo Theories, builds on top of this by figuring how to apply different kinds of solvers (different kinds of theories?) to solve problems that are similar to SAT problems, but for more general atoms. For example:

$$x + 2y - z = 7$$

$$3x - y + 2z = 13$$

$$2x + y + z = 10$$

**SAT is useful because our state transition function just gave us a whole lot of Boolean logic to wade through. SMT is useful because we're trying to use all that Boolean logic to simulate and solve what is basically a systems of equations!** Specifically, the systems of equations takes place over _time_, as we'll see.

To understand how to use Z3, I found [_A Dumb Introduction to z3_](https://ar-ms.me/thoughts/a-gentle-introduction-to-z3/?curius=3118) to be useful. Even though it makes use of the Rust bindings, it's not too different from the Python bindings; for example, the above systems of equations solved with Z3:

```python
import z3

x, y, z = z3.Reals('x y z')

s = z3.Solver()
s.add(x + 2 * y - z == 7)
s.add(3 * x - y + 2 * z == 13)
s.add(2 * x + y + z == 10)

print(s.check())
# sat
model = s.model()
print(model)
# [x = 5, z = -2/3, y = 2/3]
print(model.eval(x * 2, model_completion=True))
# 10
```

So, how do we convert our environment to translate cleanly into Z3? We need to a) figure out how to represent our data in Z3, and b) how to structure this as a search problem so that Z3 can do its thing.

For a), we can represent the input bits as a [`BitVec`](https://microsoft.github.io/z3guide/docs/theories/Bitvectors/), which is basically a Z3 datatype that represents a fixed-length vector of bits, i.e. `z3.BitVec(symbol: str, length: int)`.

What about b)? Well, what we're trying to find is like, a sequence of bits, right? So it's kind of like a tree, I can choose either bit 0 and bit 1 each time and that'll change the state in some way, hopefully it'll toggle `success` to high.

We have to stop at some point, so we can try traversing the tree up until a certain number of bits and increase the depth exponentially (maybe like a power of two) each time, and see how that goes.

@components/asic-puzzle/tree.svg

In the tree above, the outputs of `st2` rely on the outputs of `st1`, so that's an extra constraint that Z3 will be aware of. Also, remember that Z3 will represent this as a bunch of unsolved equations and _won't actually compute anything_ until it makes sure the constraints don't have contradictions. So maybe it'll realize that, I dunno, `I1=1` actually doesn't work with the constraints and just cut off that branch entirely.

So if we can model this tree, and then pass it off to Z3, we can cross our fingers and hope that it's able to solve it. Hopefully the code is more clarifying:

```python
from dataclasses import replace
import dataclasses
import z3

import logic

OPS = type("_ZOps", (), {
    "mux": staticmethod(lambda c, t, f: z3.If(
        (c if not isinstance(c, int) else z3.BitVecVal(c, 1)) == z3.BitVecVal(1, 1),
        (t if not isinstance(t, int) else z3.BitVecVal(t, 1)),
        (f if not isinstance(f, int) else z3.BitVecVal(f, 1)),
    ))
})()


def symbolic_state(prefix):
    vals = {f.name: z3.BitVec(f"{prefix}_{f.name}", 1) for f in FIELDS}
    return replace(logic.initial_state(), **vals)


def extract(model, sym):
    return model.eval(sym, model_completion=True)


def run(N):
    s = z3.Solver()

    # Replace hard-coded values with symbolic variables.
    st = symbolic_state("st0")
    I_symbols = []
    states = [st]

    # Loop through the cycles.
    for i in range(1, N + 1):
        # Create a new input bit for this cycle.
        I_i = z3.BitVec(f"I{i}", 1)
        I_symbols.append(I_i)

        curr = replace(st, I=I_i, enable=z3.BitVecVal(1, 1), rst_n=z3.BitVecVal(1, 1))
        # Derive the next set of "nodes", if you will.
        next = logic.next_state(curr, debug_signals=False, ops=OPS)
        st_next = symbolic_state(f"st{i}")

        # Add the constraints. They get propagated...
        for f in FIELDS:
            s.add(getattr(st_next, f.name) == getattr(next, f.name))

        st = st_next
        states.append(st)

    # The constraint we care about is that one of these states has `success` as high.
    s.add(z3.Or([getattr(states[i], "success") == 1 for i in range(1, N + 1)]))

    result = s.check()
    if result != z3.sat:
        return result, None, None, None

    model = s.model()
    # Traverse through the solved model and stop at the earliest cycle where
    # `success` is toggled to high.
    for i in range(1, N + 1):
        if extract(model, getattr(states[i], "success")).as_long() == 1:
            return result, model, i, I_symbols
    return result, model, N, I_symbols


N = 2
MAX = 128
while N <= MAX:
    print(f"[solve] single-solve N={N} ...", flush=True)
    result, model, hit, I_symbols = run(N)
    if result == z3.sat:
        print(f"[solve] SAT: success first at cycle {hit}", flush=True)
        iv = "".join(str(extract(model, I_j).as_long()) for I_j in I_symbols[:hit])
        print("I sequence:", iv)
        o = eval_O(hit, model)
        print("O (hex) at success:", hex(o) if o is not None else None,
              "| chars:", printable(o))
    elif result == z3.unsat:
        print(f"unsat at N={N}", flush=True)
        N *= 2
    else:
        print(f"unknown/timed-out at N={N}", flush=True)
        break

print("[solve] success not reached up to N =", args.max)
```

Observe that Z3 doesn't see this as a tree the way that we've intuited it. Instead, we've sort of unrolled all the equations out over time. The best analogy I can make is that this is very similar to feedforward in a neural network, if you will.

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
[solve] SAT: success first at cycle 122
I sequence: 00000001010100001000000000000101010100000000000010100000010000010000001000001010000100000001000000100000100100010100000000
O (hex) at success: 0x28 | chars: (
```

Yes! Now that we have a sequence, we can simulate this until `success` is flipped low again. In this case, we can simply take advantage of our transition function to simulate this, really cleanly in fact:

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

- Having never done this before, I took a couple of extra steps that I probably didn't need to take. I tried to detail that in this post, at the expense of maybe being a bit confusing :)
- I didn't talk much about the four parsers that were used to solve this puzzle because I didn't write them (and I don't think you want to read slop code), but as you can imagine they're really primitive, just lexer &rarr; parser produces an AST/IR &rarr; traverse through AST and produce final format. For parsing the waveform file, it's even simpler, just a direct pass to get a dictionary of timestamps and the map of variables changed during that timestamp.
- LLMs are really good for discardable tasks like this where code quality doesn't matter (i.e. I don't read it, I just need to verify the output is what I would expect). What would take me a day or two is maybe about half an hour, and the LLMs can also write temp verification scripts to check its output. I did this puzzle with a shitty free model.
  - My assumption is that you can probably also just give this puzzle directly to an agent and it'll be able to solve it. It's much more fun to have an idea of what's going on though.
- Like I mentioned, I took some time before working on the puzzle to read about how all of this stuff worked, since I didn't actually understand anything that was going on. Specifically I read about how FPGAs work (I figured they were related), how transistors work, how ASICs work, etc. Context is useful for understanding how to solve problems.
  - I also got nerd-sniped a bit before actually focusing on this puzzle, I admit... I wanted to understand what it would be like to go down the opposite lane so I started looking into that. (Still trying to replicate [OpenLane](https://openroad.org/) via individual steps. I even started writing a tiny Yosys 💀)
- I want to learn more about how Z3 works internally.
