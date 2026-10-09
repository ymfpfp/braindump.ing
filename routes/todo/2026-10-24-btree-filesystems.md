---
date: 2026-10-24
title: A Tour Of Modern Filesystems, Part II: B-tree magic 
---

Carrying up from the previous post

## XFS

In the mid 1990s, [Silicon Graphics](https://en.wikipedia.org/wiki/Silicon_Graphics) wanted to design a highly scalable filesystem for high-performance computing. The features that XFS introduced deeply influenced ext4.

So far, we've been storing inode data in a table, which really is just an array we can index into. This is fine, but observe that this is bad for some situations.

XFS doesn't actually stand for anything, it's the X Filesystem.

XFS is made up of individual _Allocation Groups_. These are similar to block groups in ext, but the difference is that these are much more self-contained: they can be thought of as their own filesystem. This means that you can perform operations on them in parallel, which, as you can imagine, scales really well!

todo(jc): This is a dropdown

---

### B-trees and variations

First off, I assume you understand what a [tree](https://en.wikipedia.org/wiki/Tree_(abstract_data_type) is; a B-tree is just a _self-balancing_ tree where every node. Because it rebalances on every operation, it can maintain a worse-case $O(n)$ time complexity over

Here's what a B-tree might look like:

todo(jc): Depiction.

XFS doesn't use B-trees; rather, it uses a variation of B-trees, called **B+ trees**. In B+ trees, data is only stored in the leaf nodes

---

Sometimes actualizing what the data structures being used actually looks like in practice is useful, so let's take a look at the source code for the b+ trees XFS uses in [`fs/xfs/libxfs/xfs_btree.h`]().

First off, there are two kinds of B+ trees in

---

One of the strongest uses of B+ trees here is to

## Btrfs

Btrfs is short for B-tree FS, and is supposed to be a replacement filesystem for Ext:

> Despite the fact that Ext4 adds a number of compelling features to the filesystem, T'so [one of the main developers of Ext] doesn't see it as a major step forward. He dismisses it as a rehash of outdated "1970s technology" and describes it as a short-term solution. He believes that the way forward is Oracle's open source Btrfs filesystem...
>
> — [Panelists ponder the kernel at Linux Collaboration Summit (2009)](https://arstechnica.com/information-technology/2009/04/linux-collaboration-summit-the-kernel-panel/)

It comes with a slew of features that Ext doesn't have, notably:

- Checksumming
- Snapshots
- The concept of having a logical to physical addressing scheme is strongly worked in

As you might expect, Btrfs is called what it's called because, well, it makes use of B-trees. In fact, every piece of metadata is encoded as a B-tree. Let's take a look at what that looks:

### What the B-trees look like

Before we get started, I want to take a look at what the B-trees here look like. For that, let's look inside the source code, at [`include/uapi/linux/btrfs_tree.h`](https://github.com/torvalds/linux/blob/master/include/uapi/linux/btrfs_tree.h) within the Linux kernel source.

First off, each node is **fixed size**. The size is determined by the superblock.

Each node has a header, which looks like [this](https://github.com/torvalds/linux/blob/2709dd5ae32f0828f386327c76bba9f39f63a1c6/include/uapi/linux/btrfs_tree.h#L488):

```c
/*
 * Every tree block (leaf or node) starts with this header.
 */
struct btrfs_header {
	/* These first four must match the super block */
	__u8 csum[BTRFS_CSUM_SIZE];
	/* FS specific uuid */
	__u8 fsid[BTRFS_FSID_SIZE];
	/* Which block this node is supposed to live in */
	__le64 bytenr;
	__le64 flags;

	/* Allowed to be different from the super from here on down */
	__u8 chunk_tree_uuid[BTRFS_UUID_SIZE];
	__le64 generation;
	__le64 owner;
	__le32 nritems;
	__u8 level;
} __attribute__ ((__packed__));
```

The notable field here is `nritems`, which is a integer value describing how many items are in this node. Since the node is fixed size, this is useful for tracking The shape of the items depend on whether the node is an internal node or a leaf node.

At this point, our tree looks something like this:

todo(jc): Depiction of a tree, all headers.

For internal nodes, each item looks like [this](https://github.com/torvalds/linux/blob/4352b8aee98005853aa63f57d6377282de17a33f/include/uapi/linux/btrfs_tree.h#L573):

```c
/*
 * All non-leaf blocks are nodes, they hold only keys and pointers to other
 * blocks.
 */
struct btrfs_key_ptr {
	struct btrfs_disk_key key;
	__le64 blockptr;
	__le64 generation;
} __attribute__ ((__packed__));

struct btrfs_node {
	struct btrfs_header header;
	struct btrfs_key_ptr ptrs[];
} __attribute__ ((__packed__));
```

As you can see, each internal node is made up of an array of `btrfs_key_ptr`. Each of these `btrfs_key_ptr` is roughly in the shape of (key, pointer to block).

This makes sense because most of our metadata will point to actual data, or even other metadata. The question is what the key, represented by `btrfs_disk_key`, looks like:

```c
/*
 * The key defines the order in the tree, and so it also defines (optimal)
 * block layout.
 *
 * objectid corresponds to the inode number.
 *
 * type tells us things about the object, and is a kind of stream selector.
 * so for a given inode, keys with type of 1 might refer to the inode data,
 * type of 2 may point to file data in the btree and type == 3 may point to
 * extents.
 *
 * offset is the starting byte offset for this key in the stream.
 *
 * btrfs_disk_key is in disk byte order.  struct btrfs_key is always
 * in cpu native order.  Otherwise they are identical and their sizes
 * should be the same (ie both packed)
 */
struct btrfs_disk_key {
	__le64 objectid;
	__u8 type;
	__le64 offset;
} __attribute__ ((__packed__));
```

This is a very general-purpose key. First off, although `objectid` in the comments specifically refers to it as an inode number, it is a general-purpose numerical ID that has meaning based what `type` is. `type` is essentially an enum

So at this point we have something like

So eventually you get to the leaf nodes, which look like [this]():

```c
/*
 * A leaf is full of items. offset and size tell us where to find the item in
 * the leaf (relative to the start of the data area)
 */
struct btrfs_item {
	struct btrfs_disk_key key;
	__le32 offset;
	__le32 size;
} __attribute__ ((__packed__));

/*
 * Leaves have an item area and a data area:
 * [item0, item1....itemN] [free space] [dataN...data1, data0]
 *
 * The data is separate from the items to get the keys closer together during
 * searches.
 */
struct btrfs_leaf {
	struct btrfs_header header;
	struct btrfs_item items[];
} __attribute__ ((__packed__));
```

Leaf nodes are special in the sense that they

Overrall, you can understand the structure of a b-tree in Btrfs as looking something like this:

todo(jc): Depict this.

You might notice this feels more akin to a B+ tree than a

Ok — so this is a new data structure we're using! But how?

First, as you might have come to expect by now, there's a superblock. Once again you can find this definition inside [`include/uapi/linux/btrfs_tree.h`](https://github.com/torvalds/linux/blob/4352b8aee98005853aa63f57d6377282de17a33f/include/uapi/linux/btrfs_tree.h#L671):

```c
/*
 * The super block basically lists the main trees of the FS.
 */
struct btrfs_super_block {
	/* The first 4 fields must match struct btrfs_header */
	__u8 csum[BTRFS_CSUM_SIZE];
	/* FS specific UUID, visible to user */
	__u8 fsid[BTRFS_FSID_SIZE];
	/* This block number */
	__le64 bytenr;
	__le64 flags;

	/* Allowed to be different from the btrfs_header from here own down */
	__le64 magic;
	__le64 generation;
	__le64 root;
	__le64 chunk_root;
	__le64 log_root;

	/*
	 * This member has never been utilized since the very beginning, thus
	 * it's always 0 regardless of kernel version.  We always use
	 * generation + 1 to read log tree root.  So here we mark it deprecated.
	 */
	__le64 __unused_log_root_transid;
	__le64 total_bytes;
	__le64 bytes_used;
	__le64 root_dir_objectid;
	__le64 num_devices;
	__le32 sectorsize;
	__le32 nodesize;
	__le32 __unused_leafsize;
	__le32 stripesize;
	__le32 sys_chunk_array_size;
	__le64 chunk_root_generation;
	__le64 compat_flags;
	__le64 compat_ro_flags;
	__le64 incompat_flags;
	__le16 csum_type;
	__u8 root_level;
	__u8 chunk_root_level;
	__u8 log_root_level;
	struct btrfs_dev_item dev_item;

	char label[BTRFS_LABEL_SIZE];

	__le64 cache_generation;
	__le64 uuid_tree_generation;

	/* The UUID written into btree blocks */
	__u8 metadata_uuid[BTRFS_FSID_SIZE];

	__u64 nr_global_roots;

	/* Future expansion */
	__le64 reserved[27];
	__u8 sys_chunk_array[BTRFS_SYSTEM_CHUNK_ARRAY_SIZE];
	struct btrfs_root_backup super_roots[BTRFS_NUM_BACKUP_ROOTS];

	/* Padded to 4096 bytes */
	__u8 padding[565];
} __attribute__ ((__packed__));
```

Unlike Ext or XFS, we don't replicate our superblock within each "group" (whether that be block groups, or Allocation Groups). Instead, there are up to three of them, depending on the size of your disk at fixed addresses: 64KiB, 64MiB, and 256GiB.

But back to what's actually inside the superblock. As the helpful comment indicates, the superblock has pointers to the main B-trees the filesystem needs to bootstrap itself:

- `root`, the root tree
- `chunk_root`, the chunk tree
- `log_root`, the log tree

The root tree enapsulates a load of stuff, as we'll see, so we won't start there. We'll start with the chunk tree instead.

### Chunk tree

Earlier, I mentioned

**How do we bootstrap to the chunk tree?** If every address encoded in Btrfs is a logical address, how do we get to the chunk tree, which tells us the actual physical address? Fortunately, it's encoded in the superblock in `sys_chunk_array`, which to refresh, looks like:

```c
    // ...
	__le32 sys_chunk_array_size;
    // ...
	__u8 sys_chunk_array[BTRFS_SYSTEM_CHUNK_ARRAY_SIZE];
    // ...
```

[`btrfs_read_sys_array`](https://github.com/torvalds/linux/blob/66498c75b4f8017f62d720d9b59675bdf3abce91/fs/btrfs/volumes.c#L7940) in `fs/btrfs/volumes.c` tells us that `sys_chunk_array` looks roughly:

```c
int btrfs_read_sys_array(struct btrfs_fs_info *fs_info)
{
    // ...
	while (cur_offset < array_size) {
		struct btrfs_chunk *chunk;
		struct btrfs_disk_key *disk_key = (struct btrfs_disk_key *)array_ptr;
		u32 len = sizeof(*disk_key);

		/*
		 * The sys_chunk_array has been already verified at super block
		 * read time.  Only do ASSERT()s for basic checks.
		 */
		ASSERT(cur_offset + len <= array_size);

		btrfs_disk_key_to_cpu(&key, disk_key);

		array_ptr += len;
		sb_array_offset += len;
		cur_offset += len;

		ASSERT(key.type == BTRFS_CHUNK_ITEM_KEY);

		chunk = (struct btrfs_chunk *)sb_array_offset;
    // ...
}
```

### Root tree

Now that we're all bootstrapped and ready to convert all t

### Subvolumes

The root tree has

The default subvolume

Now let's look how the design of Btrfs

### Snapshots with CoW

Using subvolumes means that we can now do snapshots really easily.

### Device tree

Another sort of first-class concept that

In my opinion, Btrfs feels very incomplete. It seems like it tries to straddle the line

## ZFS

ZFS is rethinking all of these abstractions on a much bigger scale: quite literally, it stands for Zettabyte File System[^99].

[^99]: $10^{21}$ bits.

ZFS introduces a completely radical concept: a completely new storage model! Well, new in the sense that it asks, "How can we reinvent the way data goes from user program to device driver so that it's even more scalable than it currently is?"

A visual diagram of this difference:

todo(jc): Diagram

As you can see, rather than going through the traditional syscall &rarr; file system &rarr; volume manager

Since it bypasses parts of the traditional Linux filesystem stack, the source driver and associated kernel subsystem are open sourced in a different repository

### SPA

We'll start with the SPA, short for Storage Pool Allocator. This abstracts away all attached devices, in favor of exposing logical addresses.

The paper

How does one store data in the SPA?

### vdevs

vdevs stands for

### ARC

One of the cool things about ZFS is that it really does reinvent everything. It even bypasses the Linux page cache by introducing its own page cache! This

Let's take a look at the psuedocode the paper presents us with

What does ZFS add on top of this:

### ZPL

Now we can finally make DMU objects seem like

### Other features

ZFS has so many layers to it — it's roughly around 300k lines of code at time of writing this — so trying to discuss all the features. Here are some interesting ones, taken from the [wonderful talk]() given by:

- RAID levels 1, 5, 6 suffe

## LFS

So far, we've been thinking about storing

What if we kept storing

## Extra: FAT

Separate

## Extra: APFS

APFS was released around 2017 as the successor to Apple's

Now, for a fun detour, let's look at a FUSE implementation for this!

## So, what filesystem?

I'm not in a position to give you advice on that, but the answer is probably "It depends". The lesson to take away from all of this is that you can't do everything and you should find a reasonable compromise.

I think it's crazy how far you can take this idea that everything is a file. Object storage, which AWS pioneered, is basically a custom filesystem on a huge scale. Each node in AWS'

For a list of the papers, talks, etc. previewed while researching this, see [here]().

I didn't mean for this to end up being this long, but [files are fraught with peril](https://danluu.com/deconstruct-files/?curius=3118&trail=6ead794a)! And so a lot of the work ends up trying to figure out ways to navigate that in conjunction with speed.

---

Diagrams rendered with LLM assistance.
