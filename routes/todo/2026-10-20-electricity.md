---
date: 2026-10-20
title: Some epiphanies on this electricity thing
layout: writing
---

Ok, so I've really been learning more about electronics over the last two or so weeks and I'm quickly realizing that this topic is very much like an iceberg. The more you dig the more there is to learn. I think how electriity works is a very overcomplicated topic so I'm going to talk about that here.

First off, I think the easiest thing to understand is that **current** is the movement of something. What is moving is electrons, or more pedantically, **charge**. It's not really relevant for us (at least, not at the level we're discussing), to think of electrons as more than these really tiny particles that each have a _negative charge_.

What's charge though? It turns out we can't explain that super well, we have no idea why it exists. But we do know that certain particles — protons — have positive charge, some have no particular charge — neutrons — and some have negative charge. The way I like to think about this is meta — how good something has to be relative to something else. In the same way that there can't be happiness without sadness, there can't be positivity without negativity. It's all relative!

The level at which it's relative is, to the best of our knowledge, at the level of these protons and electrons. Like, how much negative charge is in an electron, and how much positive charge is in a proton? We measure charge in _coulombs_, and we say that electrons have a charge of $q_e = -1.602 \times 10^{-19} \text{C}$, and we say protons have the opposite, $q_p = +1.602 \times 10^{-19} \text{C}$.

Like the fact that we make up many other things in life, we've assigned some bullshit numbers to an imaginary unit. But that's ok, because all we need is for the numbers to be relative _to each other_. And in this case, that's true, one electron cancels out one proton in terms of charge.

(And yes, charge is typically indicated by the letter $Q$.)

We also have some idea of the properties of charge! Supposedly, negatively charged particles want to move to positively charged particles really badly. This is essential to electricity because this is how a circuit works. We have a bunch of these electrons, these negatively charged particles, moving through some wire, to get to a positive terminal. At any given cross-section of the wire, we can imagine that a certain amount of charge is moving through it because these electrons are moving:

![TODO: A circuit, with some cross section of wire highlighted, with]()

That's what current is, it's a rate. Kind of like how a car has a velocity. Specifically, **current** is $I = \frac{\Delta Q}{\Delta t}$. Reading this out, all it's saying is, the current at this cross section is just the movement in charge over a period of time. Try to imagine that... a bunch of charge moving through this cross section, over a second for example... then imagine the cross section being the whole wire.

(Yeah, current is denoted with an $I$, I guess because $C$ has already been taken. Also, all of these units are)

Kind of like how a car's speed tends to be measured in miles per hour, rather than like, feet per second or something, we've also chosen a default measurement unit. We've decided that it should be coulombs per second, and we've even given a name to this unit! Every $\frac{1 C}{1 s}$ is called an **ampere**.

(Amperes also follow the [SI]())

It's important to remember that in literature, when we talk about current casually, we are usually talking about the magnitude of current, that is, $\lvert I \rvert$. If you're observant, you might have noticed that an ampere is positive. Why would we do that, when we know it's negatively charged particles that will drift toward a positive terminal? Shouldn't it be negative, since negative charge is traveling? Just realize that it's casual and when someone tells you, "This circuit draws 20mA", they mean that across the circuit, from the negative to positive terminal, the current is traveling -20mA.

While we're on this topic, a lot of circuits will also show you an arrow pointing from the positive terminal to the negative terminal:

![TODO: A circuit, that demonstrates this]()

This is stupid, but the way to visualize this is to imagine this is to imagine it as the space left behind. When you're on the highway, cars tend to move in front of each other, trying to fill in the empty spaces; this produces an empty space that slowly ripples backward. As you see the space going backward, you might imagine, a wave going backwards in a way. Just remember that that's what the arrow is denoting. Unfortunately we're just stuck with some bad notation sometimes.

Next up is voltage. Talking about voltage is really confusing without understanding electric potential energy. I suppose potential energy is quite literally what it sounds like — the potential to make something happen. In this case, **electric potential energy** is basically the energy that's represented by negative charge wanting to move towards positive charge. Just imagine all that negative charge on the negative terminal of a battery, wanting to go to the positive terminal, but just not able to because there is simply no direct way to do so. Voltage represents the amount of electric potential energy in each unit of charge, and is, once again, relative.

![TODO: A cartoon of a battery, that demonstrates this]()

I think the epiphany to understanding voltage is that there is _no current without voltage_. If you take two equally charged sources — let's say they both have the same amount of protons and electrons, for whatever reason — and expose them to one another, nothing will happen, since nothing needs to happen! The property of charge that we talked about earlier

But with voltage, the potential energy is there.

Remember talking about potential energy in middle school? You might recall that we were taught that energy was measured in **joules**. Well,

This concept of ... that we have the whole notion of **ground**, which you'll typically see shortened to GND in circuits.

**Kirchoff's Voltage Law** states

Voltage and current are related to each other

- What is a voltage drop?

$P = VI$

Now let's toy around with the relationship between these three things: voltage, current, and power.

One crazy epiphany to have here is that **voltage is used to communicate, not current**. Let's think about how to send one bit, from one end to another. The intuitive

Transistors are essentially voltage-controlled switches! I will write more about this in another post where I take a look at all the different kinds of transistors.

This is the standard we have come up with, in most circuits and concepts (e.g., differential signaling), because drawing current means . We can communicate with current, it's just not efficient.

A lot of articles try to explain this using water, which to some extent is an useful analogy, but always breaks down. So it's better to just understand it this way.

---

Electronic components can be roughly placed into three categories, and usually are a mix:

- Resistors
- Capacitors
- Inductors

Let's go through these because this

The epiphany you should have is that resistors provide resistance, which is a big part of

So it's essentially an **insulator**. On the other hand, you have

Say you have a circuit:

![TODO: A circuit, with a LED and a resistor]()

How does one come to build this circuit? First, we say, we want to light a LED. Ok. We take that LED. Usually it comes with a datasheet that describes the physics of it, but let's say it doesn't: we can pull up an oscilloscope.

We pull up

In fact, a common way of using resistors is as a _divider_. For example, one common thing you'll see in ICs, or integrated circuits (black boxes with input pins and output pins; imagine each output pin having a function that) have a "sensing" pin:

How exactly

Realizing the reason

Capacitors are used in a lot of different ways. Two notable ways I want to talk about are **decoupling capacitors** and **AC-coupling capacitors**, since they're pretty common.

I think it's really cool that, yes, intuitively capacitors are a bank for holding onto charge, but you can then turn around and use that to do something else. There's a dogma for electronics somewhere in there...

So far, when we talk about charge, we've been talking about electric charge. Inductors introduce a different kind of charge, one that you might be familiar with if you've ever played around with magnets:

Together, you get electromagnetic force.

Future heading: there's more to resistance, and it's called impedance!

So far, we've been talking about

---

Anyways, that's that! I hope to deeply internalize these concepts at the PCB level - I think it's definitely easy to believe that one understands these concepts until one looks at a PCB schematic and is beyond confused. And beyond that, when I was talking about the iceberg earlier:

I hope to really deeply understand EMF and how it affects routing
