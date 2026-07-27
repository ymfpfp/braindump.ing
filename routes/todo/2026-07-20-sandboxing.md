---
date: 2026-07-20
title: On all the sandboxing options
layout: writing
---

tl;dr: Being written, incomplete, not very well structured, yapfest. Goals: look at three different levels of running sandboxed code - containers, Type I, Type I hosted, Type II, assuming you basically know nothing. Details relevant to x86-64

## Containers

Containers are basically just isolated Linux containers

Docker is a little more than just containers. I think describing it as a runtime might be the best way to approach it. There are a lot of moving parts and it's easy to over-complicate it, but it's basically just stacking different tools on top of one another. From top to bottom, it looks mostly like:

1. Docker CLI/Docker Desktop: Some user interface. Since Docker makes use of specific Linux kernel features, on non-Linux OSes Docker Desktop will also run and manage a lightweight VM (e.g., WSL2 on Windows). Docker Desktop nowadays also comes with Kubernetes
2. Docker Engine/dockerd(aemon): This is the
3. containerd: At this point we're running in the Linux VM if we aren't on Linux. This is the daemon that does the actual grunt work of managing the containers and images. Pull down an image, containerd constructs the overlayfs snapshot.
4. runc(ontainer): Fork a child process, creating the actual container by making use of the aforementioned Linux features. Since we're trying to lock
   - cgroups and namespaces etc.

Kubernetes is just a scaling technology built on top of this.

Docker is a lot of shit, actually. Did I mention that already? Some other features Docker provides you with, briefly:

- BuildKit
  - Docker uses [overlayfs]().
  - Docker will typically

I'm not going to go into containers into too much detail, but here's a nice bird's eye view in case you would like to do more research into the individual components:

<div class="demo">
  <div class="stack">
    <p class="note">↗ kubernetes expands on this</p>
    <div class="tier">
      <code>docker cli / desktop</code>
      <div class="row">
        <div class="box">operate a VM if not on Linux</div>
      </div>
    </div>
    <div class="tier">
      <code>docker engine / dockerd</code>
      <div class="row">
        <div class="box">networking</div>
        <div class="box">storage</div>
        <div class="box">
          <p class="title">extra tools</p>
          <div class="row">
            <div class="box"><code>buildkit</code></div>
            <div class="box"><code>compose</code></div>
          </div>
        </div>
      </div>
    </div>
    <div class="tier">
      <code>containerd</code>
      <div class="row">
        <div class="box">images: pull, cache, save, extract and overlay with <code>overlayfs</code></div>
        <div class="box">manage containers</div>
      </div>
    </div>
    <div class="tier">
      <code>runc</code>
      <div class="model">
        <div class="box">fork a process</div>
        <div class="row">
          <div class="box">
            <p class="title">security / "hardening"</p>
            <div class="box"><code>capabilities</code></div>
            <div class="box"><code>seccomp</code></div>
          </div>
          <div class="box">
            <p class="title">isolation</p>
            <div class="box"><code>cgroups</code> (cpu, ram, memory, etc.)</div>
            <div class="box"><code>namespaces</code> (pid/uid/gid, mounts, uts, etc.)</div>
          </div>
        </div>
      </div>
    </div>
  </div>
  <p class="note">Each layer delegates downward: the CLI drives the daemon, which drives <code>containerd</code>, which drives <code>runc</code> — which in the end just forks an isolated, hardened Linux process.</p>
</div>

## VMs

tl;dr: Read the first paragraph. This is basically me getting nerdsniped int

VMs are deceptively simple at their core. Point to memory region, fetch/decode/execute, emulate some devices - whether by poking through to actual devices on the host or simulating via allocating more memory regions. The majority of emulation makes use of virtualized memory + virtualized processor + virtualized devices, in spirit of Von Neumann/Harvard architectures.

Thus the differences lie in how exactly this emulation is performed and what optimizations are done; terminology typically separates

Pedantically, the difference between a hyper

### Images

Some common formats:

## Firecracker & co.

[Firecracker](https://github.com/firecracker-microvm/firecracker) is "an open source virtualization technology that is purpose-built for creating and managing secure, multi-tenant container and function-based services... Firecracker runs workloads in lightweight virtual machines, called microVMs, ... that uses [KVM] to create and run microVMs".

Firecracker is used by AWS to provide [Lambda](https://aws.amazon.com/lambda/), their serverless functions, and [Fargate](https://aws.amazon.com/fargate/), their service for deploying container-based services serverless-ly.

Firecracker is essentially a really stripped down fork of Google's [`crosvm`](https://github.com/google/crosvm), which I need to do more research on.

In simpler terms, it provides you with:

1. A VMM (Virtual Machine Monitor) exposed over a HTTP service on a Unix socket that lets you create and manage virtual machines and associated resources. The virtual machines are created by taking advantage of a Linux kernel module called KVM, short for Linux Kernel Virtual Machine.
2. A set of programs that allow you to run a full Linux kernel supplied by the user on these virtual machines. Behind the scenes these programs interface with KVM to set up what is necessary to run Linux (e.g., setting up virtual CPUs, virtual memory, etc.).
3. Extra utilities, e.g. a set of virtio devices

Why you might want to use Firecracker boils down to: it's faster than a normal VM in terms of spinning up and down, and provides. This is why you'll see it labeled as a . In theory I doubt that either of those are super big issues given modern hardware and software, but it is objectively attractive.

### KVM

The source code for KVM on x86 architectures can be found at [`linux/arch/x86/kvm`](https://github.com/torvalds/linux/tree/b95f03f04d475aa6719d15a636ddf32222d55657/arch/x86/kvm).

KVM runs instructions on the CPU directly, without the host Linux kernel getting in the way. You write userspace code to set up the resources, e.g. memory: you can `mmap` memory and then copy a set of instructions into that memory. The virtual machine you create can then execute these instructions directly on the CPU via hardware extensions. In addition, the kernel module allows you to emulate

- When an interrupt happens, if it's relevant to the VM, it'll inject a virtual interrupt into the guest.
- The VM will then exit for operations that require emulation. When this occurs, KVM will pass the details to your userspace process, which handles the operation and then passes control back to KVM.

Compare this to the Linux container model. Containers are basically isolated Linux processes. A comparison of the two different models looks something like:

<div class="demo">
  <div class="diagram">
    <div class="model">
      <p class="title">Linux container model</p>
      <p class="note">Isolation is a kernel feature.</p>
      <div class="row">
        <div class="box">Container<code>process</code></div>
        <div class="box">Container<code>process</code></div>
        <div class="box">Container<code>process</code></div>
      </div>
      <p class="boundary"><code>namespaces</code> + <code>cgroups</code> + <code>seccomp</code> / <code>LSM</code></p>
      <div class="layer">
        Shared host kernel
        <span class="note">One syscall surface for everyone, around 350 syscalls of attack surface.</span>
      </div>
      <div class="layer hardware">Hardware</div>
      <p class="note">A container is just an isolated Linux process: cheap to start, but it shares the very kernel it is being isolated by.</p>
    </div>
    <div class="model">
      <p class="title">KVM virtualization model</p>
      <p class="note">Isolation is a hardware feature.</p>
      <div class="row">
        <div class="box guest">
          microVM
          <span class="inner"><code>process</code></span>
          <span class="inner">Guest kernel</span>
        </div>
        <div class="box guest">
          microVM
          <span class="inner"><code>process</code></span>
          <span class="inner">Guest kernel</span>
        </div>
      </div>
      <div class="layer"><code>firecracker</code> VMM, one per microVM</div>
      <div class="layer">
        Host kernel, driven through <code>ioctl</code> on <code class="accent">/dev/kvm</code>
      </div>
      <div class="layer hardware">
        Hardware
        <span class="note">VT-x / AMD-V, EPT, VMCS</span>
      </div>
      <p class="note">Guest instructions run on the CPU directly; the guest only exits to the VMM for what needs emulating.</p>
    </div>
  </div>
</div>

This is vaguely based off a similar [diagram](https://cdn.braindump.ing/writing/kvm.png) in AWS' [paper](https://www.usenix.org/system/files/nsdi20-paper-agache.pdf) on Firecracker[^0].

[^0]: The end of this paper is cool, they talk a bit about Firecracker usage in Lambda and performance.

Interacting with KVM is done by making `ioctl` syscalls to the file descriptor at `/dev/kvm`, given that the module is enabled (use `lsmod | grep kvm` for quick check). You might know `ioctl` as the grab-bag syscall on POSIX systems; its primitive type signature is basically a syscall in the form `ioctl(fd, int op, void* arg)`, per the [man](https://man7.org/linux/man-pages/man2/ioctl.2.html) page:

```c
#include <sys/ioctl.h>

int ioctl(int fd, unsigned long op, ...);  /* glibc, BSD */
int ioctl(int fd, int op, ...);            /* musl, other UNIX */

// The ioctl() system call manipulates the underlying device
// parameters of special files. In particular, many operating
// characteristics of character special files (e.g., terminals) may
// be controlled with ioctl() operations. The argument fd must be an
// open file descriptor.
//
// The second argument is a device-dependent operation code. The
// third argument is an untyped pointer to memory. It's
// traditionally char *argp (from the days before void * was valid
// C), and will be so named for this discussion.
```

Extra data (e.g., a file descriptor index, is typically written to the )

KVM's `op` value is `0xAE`, as defined [here](https://github.com/torvalds/linux/blob/b95f03f04d475aa6719d15a636ddf32222d55657/include/uapi/linux/kvm.h#L682) in the Linux kernel. So interfacing with KVM might look like:

1. Open a file handle to `/dev/kvm`.
2. `ioctl` to the handle with arg `0x01` to create a VM handle (as file descriptor) - KVM will create a VM behind the scenes.
3. `ioctl` to this VM handle with arg `0x41` to create a vCPU handle (also as file descriptor).
4. `ioctl` to this vCPU handle to g
5. Copy instructions over to mapped memory
6. Jump to code start by setting appropriate registers to `ioctl` (i.e.,), and execute with `ioctl` to the vCPU handle with arg `0x80`, which will cause the CPU to begin executing instructions until a VM exit happens.

It may be useful to look at KVM's `ioctl` [reference](https://www.kernel.org/doc/html/latest/virt/kvm/api.html).

Naturally you'll also have to handle all the quirks of the actual CPU. For example, if we want to run 64-bit code in _long mode_, you'll have to set up paging.

That's really all there is to it on the surface. Firecracker uses this to create VMs; now instead of running instructions directly, we want to run a whole Linux kernel.

## Digression: before the bootloader

Some prerequisite understanding is what exactly your device does when you turn it on. Feel free to skip down to booting Linux/what Firecracker does for you.

---

(dropdown)

Some intuition on how physical drives work: at this point in time, most nonvolatile memory storage is stereotypically either magnetic, a la HDDs (Hard Disk Drives), or solid-state, a la SSDs (Solid State Drives).

Hard drives are made of magnetic platters that spin at a fixed speed (typically, either 5400 or 7200 RPM for consumer hard drives).

Since there are two methods, there are also two addressing schemes that you'll see:

- The old fashioned Cylinder-Head-Sector
- LBA (Logical Block Address) addressing. Instead of using hard-disk based physical geometry, treat the disk as a flat array of blocks, which is more analogous to SSDs then

todo(jc): Maybe helps to have a diagram here.

There's a wonderful article

---

Typically it starts with the power supply triggering a `PWR_OK` line to the motherboard... this powers up everything including the volatile parts (e.g., RAM is volatile so it starts out with random values).

The CPU starts out in _reset mode_. It automatically starts executing from a fixed reset vector, a hardcoded address where it expects to find the first instruction. The fixed reset vector on modern day CPUs is typically equivalent or similar to `0xFFFFFFF0`, which is near the top of the 4GiB memory space (that is, the 32-bit address space). To be pedantic, when the CPU starts out in reset mode, it is in something called _real mode_ for 16-bit compatibility (from 8086 days). In real mode, the address space is 16 bits, but can extend to 20 bits[^1],

[^1]: The difference here is having access to X more addresses.

Anyways, this address maps to firmware that lives in a flash ROM chip on the motherboard. It's a physical chip, but the CPU memory maps it to the top of the 32-bit address space.

(But how is this firmware mapped? It's thanks to the address bus, it hits the address bus and the address bus' chipset will automatically route to the

Firmware nowadays fall into one of two categories:

- BIOS (Basic Input/Output System). This the original, came out in 1981. Runs in real mode, is limited. Uses a partition scheme called Master Boot Record (MBR) to find the _bootloader_. The bootloader is what is actually responsible for performing the operations needed to set up and load the kernel into memory.
- UEFI (Unified Extensible Firmware Interface). Modern replacement developed by Intel in the late 90s/early 00s. This specifically operates in the 64-bit address space; the firmware is typically in PE/COFF format.

Nowadays these are typically written in some higher language and compiled down to the appropriate architecture. Flashing utilizes SPI (serial peripheral interface), using:

- In-system: Chipset comes with SPI controller.
- External programmer: Physical hardware controller that clips on or solders on to flash chp
- Nowadays motherboards come with dual BIOS (two SPI controllers!) or BIOS flashback (flash from thumb drive via USB port on motherboard) if you somehow manage to mess it up.

Most motherboards only ship with UEFI in this modern day and age, but include CSM (Compatibility Support Module) to emulate legacy BIOS behavior. (Although they're also beginning to drop this).

If you're running a hypervisor like [QEMU](), you can typically expect these to be emulated. For example, QEMU comes by default with SeaBIOS, but you can pass custom firmware with `-bios`, and there's also OVMF (Open Virtual Machine Framework) which is open source UEFI firmware.

On the surface level, firmware will typically do the following things in common:

- POST, or Power On Self Test. Check up on critical connections.
- Sets up all hardware components in a specific order, and construct a map of resources that will be passed
- Scan all PCI/PCIe (Peripheral Component Interconnect / Express, interface standards for connecting hardware components to a computer's motherboard), looking for all add-on cards. Add-on cards typically have their own firmware called Option ROMs, small pieces of code the firmware will execute to set them up.
- Choose an initial _bootloader_ to hand off to. On BIOS, this is done by searching each sector on the available disks for a magic number indicating a valid bootloader. For UEFI, this is done by searching each
- Hand it off to the bootloader!

### BIOS

Each storage device that contains a working bootloader has to have a MBR at the beginning of LBA 0. LBA refers to _Logical Block Address_: instead of using the physical geometry of a typical disk - cylinder/des

(dropdown) Nowadays we have hard disks

The MBR is 512 bytes and starts with the actual MBR, the bootstrap code that the BIOS loads and reads directly. This is about 90% of the bytes.

The next 64 bytes are the partition table, which contains four 16-byte partition table entries (PTE). Each PTE contains:

- Boot flag (1 byte): Indicates this partition is active/bootable.
- Starting CHS address (3 bytes): Old addressing for start of this partition.
- Partition type (1 byte): Identifies the file system, e.g., `0x83` for Linux.
- Ending CHS address (3 bytes)
- Starting LBA address (4 bytes)
- Partition size in sectors (4 bytes)

As you can see, for backwards compatibility there is a CHS range (start + end) and a LBA range (start + size).

The last 2 bytes are the magic number `0x55aa`, the magic number that validates this as a bootable sector. BIOS will scan this first to determine whether or not to skip this disk. BIOS does not let you construct a priority list; the list of storage devices it scans is in a fixed order.

When BIOS finds a valid MBR locates this magic number, it'll bootstrap the MBR code, which in turn scans the four PTEs, finds the partition with the boot flag set, and loads the bootloader from that partition. It passes off control to the bootloader; this is typically referred to as _chain loading_. The partition that has the boot flag is known as the PBR (Partition Boot Record) or VBR (Volume Boot Record).

As you can see there are lot of limitations. For one, there are only four entries max. To deal wth this, you can designate one of your four primary partitions as an extended partition (i.e., partition type `0x05` for CHS addressing and `0x0f` for LBA addressing).

todo(jc): Diagram

### UEFI

Instead of usinw usin

Once firmware has located a bootloader.

Some typical bootloaders are [GRUB]() (GRand Unified Bootloader) for Linux and [`bootmgr`]() for Windows. Naturally, Apple has their own firmware; their boot chain looks a bit like: boot ROM

## Digression: the bootloader

Booting Linux in general requires at minimum three things:

1. We need the actual kernel image we'll be running.
2. We need `initramfs` (Initial RAM Filesystem), a filesystem that has a binary that we can run as `init` for PID 1.
3. We need a kernel command line (usually abbreviated to cmdline), a string of parameters. These are also typically referred to as the boot args.
4. We need to load `boot_params` somewhere in memory.
5. In terms of registers

Unfortunately there is sort of a chicken-and-egg problem: most kernels nowadays are modular and need access to drivers to be able to access disks; but disks are

What a bootloader might do:

### Kernel images

Typically the bootloader knows where to find kernel file on storage file, e.g.:

- `vmlinuz` on Linux

### initramfs

This is typically a CPIO archive, per this [kernel documentation](https://www.kernel.org/doc/Documentation/early-userspace/buffer-format.txt).

### Example bootloader

todo: I think writing a quick bootloader would be educational.

## Minimal working Firecracker

Given that booting Linux requires these things, Firecracker has us pass in a couple of options:

1. The kernel image.
2. Boot args
3. A rootfs.
4. Machine configuration, i.e., how much

All of this is done by communicating over the API that you expose on a Unix socket, e.g.:

```shell
API_SOCKET="/tmp/firecracker.socket"

# Remove API unix socket
sudo rm -f $API_SOCKET

# Run firecracker
sudo ./firecracker --api-sock "${API_SOCKET}" --enable-pci
```

Once you pass in these parameters Firecracker will do most of the hard work.

## virtio

`virtio` for some reason was confusing to

## v8 isolates

## Extras

Please see
