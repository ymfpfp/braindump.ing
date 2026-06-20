---
date: 2026-06-19
title: Test
---

broadly, divide into primitives and frameworks that use these primitives to build actual security

* enciphering/deciphering vs encoding/decoding
* history
	* ceasar
	* viginere
	* monoalphabetic/polyalphabetic
* one time pad/xor
* pem (privacy-enhanced mail) is format for storing and sharing keys. literally just `BEGIN <type>` followed by base64 encoded key and `END <type>`
* pgp/gpg
* password hashing algorithms
	* bcrypt
	* scrypt
	* argon2
* constant time comparison
* hash functions - $f(x): y$ where y is typically a shorter value
	* preimage resistance: given hash value h, is computationally infeasible to find an input x such that `hash_function(x) = h`.
	* second-preimage resistance: given an input x1, it's hard to find another input x2 such that `hash_function(x1) = hash_function(x2)`
	* collision resistance: it is computationally infeasible to find two inputs that map to the same output. this is different from second-preimage resistance, where you have a provided x1
	* types of hash functions:
		* crc32, other crc functions are not designed with security in mind, just useful for checksumming
		* sha-1, sha-2, md5  
	* most cryptographic hash functions use merkle-damgard construction
* macs: message authentication codes. used for authentication - if two peers have symmetric key you can prove whether or not a message that has been hashed has been intercepted, thus proving authentication
	* hmac = hash-based message authentication, hash with secret key and use that
* why should tokens have at least 112 bits of entropy?
* laws/gen principles
	* kerckhoff's law: your cipher algorithm is only as safe as the keys are!  
* symmetric encryption: $f^{-1}(\text{key}, f(\text{key}, \text{plaintext})) = \text{plaintext}$
	* functions for symmetric encryption generally fall into two categories, block ciphers, which encrypt fixed size blocks, and
	* block ciphers share:
		* padding, mode of operation for encrypting messages longer than the block of bits the cipher supports
		* electronic codebook (ecb) mode of operation is naive: divide into blocks, pad last block if needed, and encrypt each block. the result? if you coincidentally have two same blocks, the output is the same, and this can be obvious depending on the kind of data you're sending (e.g. an image, which can have many similar repeating pixels, i.e. "ecb penguin")
		* therefore we have cipher block chaining (cbc). take an random initialization vector (**iv**) to randomize encryption, xor with the first block of plaintext, and for every block afterwards, use the output of the previous block as the iv. this means that the function produces $(\text{plaintext}, \text{iv})$
	* des/3des
	* lucifer
	* aes = advanced encryption standard.
		* aes-128, aes-192, aes-256, i.e. 128-bit key because $2^{128}$ sized key is hard to brute force
		* aes is pretty widely implemented in hardware nowadays
		* plaintext and ciphertext are both fixed 128 bits (16 bytes)
		* shift 16 bytes to 4x4 matrix -> round function (multiple rounds, each round using a different round key derived from the passed-in key, i.e. **key schedule**, allowing for slightest change to give different ciphertext, i.e. **diffusion**, ) made of sub functions (SubBytes, ShiftRows, MixColumns, $\text{AddRoundKey}(\text{RoundKey})$). the rounds are fixed
	* we also need to be able to prove that a message actually came from the person we were expecting it to come from, so we can attach a mac of some sort. aead function = authenticated encryption with associated data. means that instead of producing $(\text{plaintext}, \text{iv})$ we now produce $(\text{plaintext}, \text{iv}, \text{authentication tag})$
		* aes-cbc-hmac 
		* aes-gcm = aes with galois/counter mode
	* chacha20-poly1205 = designed to be fast when used in software
* diffie-hellman is protocol for asymmetric key exchange. basically it defines how to derive a shared key between two peers given properties of a mathematical group
	* need to know some discrete math
		* a group is a set of elements + a binary operation. this automatically reminded me of `Semigroup` in haskell
		* groups should have closure as a property: $\text{binop<T>}(x: T, y: T): T$. basically needs to output value into 
		* associativity, e.g. swapping order of binary op produces same output
		* identity, e.g. any  `x <binary_op> base element = x`
		* inverse, e.g. every value $x$ has an inverse element $x^{-1}$ that produces 1
		* groups can have subgroups. a cyclic subgroup is a subgroup that repeats when the binary is repeatedly performed
		* a generator is an element (generators = set of elements) that when repeated with the binary op, can produce a subgroup. if the generator generates a set of the entire space, then it's called a generator of the group. the size of the subgroup is called the order of the subgroup.
	* for key exchange:
		* usually each peer has a public/private key pair, and some shared public state between the 2. the public/private key pair is generated by inverse of
		* they both present their public key
		* by combining the other peer's public key with their private key -> create shared key
* ffdh = finite field diffie-hellman
	* ffdh uses the group of positive integers up to a prime number, a number that can only be divided by 1 and itself. for its binary op it uses modular multiplication
	* some extra properties of this group, ffdh:
		* commutative, which makes ffdh part of galois group
	*  $6 = 1 \bmod 5$
		* 1 is the inverse of 6
		* mod 5 is the modulus
	* here the generator 4:
		* $4 \bmod 5 = 4$
		* $4 * 4 \bmod 5 = 1$
		* $4 * 4 * 4 \bmod 5 = 4$
		* $4 * 4 * 4 * 4 \bmod 5 = 1$ = order 2
		* $2 \bmod 5 = 2$
		* $2 * 2 \bmod 5 = 4$
		* $2 * 2 * 2 \bmod 5 = 3$
		* $2 * 2 * 2 * 2 \bmod 5 = 1$
		* $2 * 2 * 2  * 2 * 2 \bmod 5 = 2$ = order 4
		* the special case is prime numbers. e.g.:
			* 
	* how does this work for key exchange?
		* all peers agree on a large prime $p$ and a generator $g$
			* 2048 bits for security
		* each peer generates a random number $x$ = their private key
		* each peer generates a public key from $X = g^x \text{ mod } p$
		* discrete logarithm problem means it's pretty computationally hard to figure out $x$ from the public key. discrete log is basically given $b = a^x$, and you want to find $x$, you have to calculate $log_a(b)$.
		* then to compute a shared key, we take the other peer's public key (say $Y$), combine it with our private key, to get $Y^x = (g^y)^x \text{ mod } p$ . and on the other end $X^y = (g^x)^y \text{ mod } p$, thus producing a shared key
* ecdh = elliptic curve diffie-hellman
	* elliptic curves are just curves. curves have a specific equation, for most valid elliptic curves they can be simplified down to the short weierstrass equation $y^2 = x^3 + ax + b$ where $4a^3 + 27b^2 \neq 0$
	* ecdh uses the group of points in an elliptic points with addition as the binary operation. basically, to perform addition on two points $P$ and $Q$:
		* draw a line between $P$ and $Q$. it should hit the curve at another point.
		* draw a vertical line through that point, it hits the curve at $P + Q$.
		* how to add point to itself? draw line tangent to that point
		* what happens if line does not hit curve at any other point? use point at infinity, which in literature on elliptic curves is usually written with big letter $O$
	* identity element becomes $O + O = O$ (like for multiplicative, 1 times 1 = 1). for any point $P$ on curve, $P + O = P$
	* at this point, we can take a point $G$ and add it to itself $x$ times to produce a point $P$, e.g. $P = [x]G$, which ties in with discrete log (diffie-hellman: use operation where reversing is hard to do) 
	* ecdh adds a finite field over an elliptic curve 
	* how does this work for key exchange? (compare to ffdh above to see similarities)
		* all peers agree on an elliptic curve equation, a finite field, and a generator $G$. the generator is referred to as the *base point* if you will, and is a generator because when 
		* each peer generates a random number $x$ = their private key
		* each peer generates a public key from $X = [x]G$. 
		* as you can see, this basically boils down to discrete log, since to determine what $x$ is we'd need to compute $x = log_G(X)$. 
		* then to compute a shared key, we take the other peer's public key (say $Y$), combine it with our private key, to get $Y^x = [x][y]$
	* nowadays, most curves come from a couple standards focusing on 2 curves, either P-256 (secp256r1) or Curve25519. 
		* how to encrypt
		* combination of Curve25519 with ecdh = X25519. 
			* curve25519 offers approx. 128 bits of security, there's also curve448 for 224 bits of security
			* it operates over the finite field of $p = 2255 - 19$.
* why does ffdh need a much bigger key size than ecdh? TODO
* asymmetric encryption: $g(\text{private key}, f(\text{plaintext}, \text{public key})) = \text{plaintext}$
	* typically used to generate shared symmetric keys
	* asymmetric encryption requires more math ops than symmetric (which is mostly bit manipulation)
	* hybrid encryption has two parts:
		* data encapsulation mechanism (dem): generate symmetric key, use that to generate ciphertext
		* key encapsulation mechanism (kem): use receiver's public key to encrypt symmetric key used to encrypt data
		* send both parts, data + key
	* rivest-shamir-adleman (rsa)
		* rsa intuition:
			* pick a prime number $p$. the multiplicative group of prime numbers is the set of numbers up to p - 1.
			* to encrypt a message, choose a number $e$. our encryption function will be $f(m, e, p) = m^e \bmod p$. e.g. $f(2, 3, 11) = 2^3 \bmod 11 = 8$.
			* we need a reverse exponent to decrypt this. i.e., this is true: $m^{ed} = m \bmod p$ and we need to solve for $d$. e.g. for the above we get $d = 7$. when you take the ciphertext and pass it through this, you get the plaintext again: $8^7 \bmod 11 = 2$. that's your private key, if you will.
			* this works because euler's theorem states that $m^{\phi(n)} = 1 \bmod p$, where $m$ and $p$ are co-prime (that is, they share no common factors). $\phi$ is euler's totient function which is all numbers up to $n$ that are co-prime to $n$. if $n$ is prime ofc, this means that $\phi(n) = |\{1, 2, ..., n - 1\}|$, or in group theory, $m^\text{order} = 1 \bmod p$. we want to solve for $m$, and we know what $\text{order}$ and $p$ are. we can multiply both sides by m to get $m^{\text{order} + 1} = m \bmod p$. take a moment to think about why this intuitively makes sense. ok. that means that $m^{k \cdot order + 1} = m \bmod p$ is also true for any number $k$. exponents are $\bmod p - 1$. take a moment to think about why this intuitively makes sense. ok. we want $ed = k \cdot order + 1$. this can be rewritten to $ed = 1 \bmod \text{order}$, e.g. $3d = 1 \bmod 10$ 
			* the problem with this is that we can compute the private exponent (our private key) pretty easily (e.g. with extended euclid)... but if we replace $p$ with a number $N = pq$ where p and q are prime numbers than the order becomes $\phi = (p - 1)(q - 1)$ and relies on figuring out the factorization of $N$, which is a hard problems
		* textbook rsa is:
			* generate 2 large prime numbers $p$ and $q$. $N = pq$
			* choose a large public exponent $e$, either random or fixed.
			* your public key is $(N, e)$
			* derive $d = e^{-1} \bmod (p - 1)(q - 1)$, which is your private key
			* for encryption, compute $f(m, e, N) = m^e \bmod N = C$. for decryption, compute $f(C, d, N) = C^d \bmod N$.
		* so going back to rsa; the above is textbook rsa. it's problematic. notably, if you have a small message, say 2 bits, it's pretty easy for an attacker to brute-force. pkcs (public key cryptography standard)#1 v1.5 introduced padding, but an attack called million-message attack was discovered
			* million-message attack: choose 
		* pkcs#1 v2.0 fixes this by introducing a new padding scheme called optimal asymmetric encryption padding (oeap)
* digital signatures: are like real life signatures, except cryptographically verifiable
	* consists of three parts:
		* key pair generation algorithm
		* signing algorithm: $f(\text{message}, \text{private key}) : \text{signature}$
		* verification algorithm: $f(\text{message}, \text{public key}, \text{signature}) : \text{bool}$
	* signatures are good for origin and integrity
	* like macs, but asymmetric (can verify without private key)
* zero knowledge proofs (zkp) = proofs of knowledge that don't divulge knowledge to user
* randomness
	* entropy source, high/low entropy
	* combining randomness sources
	* fast = prngs. deterministic, but indistinguishable from random
		* forward secrecy: intercepting one state should not allow you to derive past state
		* backward secrecy: intercepting one state should not allow you to derive future state. for prngs, some have mechanisms to "heal" themselves by re-seeding
	* verifiable random funciton
		* generate keypair and publish public key + public seed
		* to generate random numbers, f(secret key, x) -> (random value y, hash )
		* then to verify random number, 
* secrets
	* key derivation function (kdf) = deriving several secrets from one secret
	* keeping secrets safe
		* secret splitting
	* hmac-based key derivation function (hkdf)
		* hkdf-extract - removes biases from a secret input, producing a uniformly random secret
			* f(salt, key) i.e. a mac -> psuedorandom key (PRK)
		* hkdf-expand - produces an arbitrary length and uniformly random output.
			* f()
* secure transport protocols - basically specs for constructing actual useful processes out of cryptographic primitives.
	* [[#ssh]]
	* [[#ssl/tls]]
	* [[#wireguard]]/ipsec
	* wifi protected access (wpa)
	* noise protocol

ideally the way you learn cryptography is to pick it up along the way, and then when you actually get to reading you'll be able to understand it pretty well

## ssl/tls

both are on top of transport layer, wrapping application data

ssl (secure sockets layer) was predecessor to tls

tls (transport layer security). tls depends on 2 pieces of info:
* some config
* for client, some info abt server

for tls 1.3, current version: https://tls13.xargs.org/. notably:
* server sends CertificateVerify message, answering the question, how to prevent mitm? e.g. when connecting to google.com how do you know it's actually google.com. answer is with **public key infrastructure** (pki)
	* browsers trust set of root public keys called **certificate authorities** (ca). hundreds of CAs run by various companies/orgs, they can also sign intermediate CAs
		* don't want to deal with pki? use your own CA
		* where are these root CAs coming from? they're manually loaded into your trust store and you've just got to trust them
	* certificates are basically $(\text{identity information}, \text{public key}, \text{ca signature})$. the certificate request basically says, "yes, this public key belongs to this identity". then usually a parent CA signs it, producing a certificate.
		* how do you prove something hasn't been tampered with? 
		* how do you prove that the owner of this data is really who they say they are?
	* certificate requests are given to parent CAs to sign. they're made of
		* your public key
		* identity info
		* extra data (e.g., extensions)
		* signature with your private key over the above data
		* in other words, this is $(\text{identity information}, \text{public key}, \text{signature})$. the only difference from a certificate is who is signing the data and verifying that the public key does belong to this identity from a reputable source
	*  websites that want to use https must have way to obtain certification from these CAs by presenting a certificate. 
		* nowadays you can generate certificates for free with CAs like Let's Encrypt
		* essentially you prove you own a domain by hosting something on the domain given to you by CA (certificate), then CA provides signature over website's public key
		* signature is long-term signing public key
	* you give csr to a ca, which then verifies that the csr is valid given the public key, data, and signature. in rsa verification, decrypt signature with public key and match to hash of data. if it is able to verify 
	* certificates are usually formatted with x.509, uses description lang called abstract syntax notation 1 (asn.1, basically a dsl) -> distinguished encoding rules (der, basically a binary encoding format) -> typically pem:
		* certificate to be signed
		* signature algorithm
		* signature value
	* to prove to browser that it is what it claims to be, server sends certificate chain as part of tls handshake. basically it's certificate, and all the intermediate CAs up to a root CA. the chain building process looks something like:
		* look at certificate in x.509 format, there is usually an issuer
		* most certificates have an "authority info access" field, url of issuer certificate
		* use issuer's public key in their certificate to prove that this certificate hasn't been tampered with. how? use public key to decipher signature, which gives us hash. compare hash to hash of data; if they're not equal something is wrong. the certificate contains what algorithm to use for hashing and what algorithm to use for asymmetric encryption
		* ok the above isn't fully accurate - just consider it to be a general purpose verification algorithm
	* this is really high trust, esp for root CAs! as thus:
		* cert revocation - lets CAs revoke cert and warn browser with ocsp (online certificate status protocol)
		* cert monitoring - forces CAs to publicly log every cert signed 
	* this is the 3rd step. after this step, the server then uses its long-term signing public key to sign all handshake messages, which allows the client to check that all handshake messages were indeed sent by the correct authenticated server
* mtls = mutually authenticated tls
* finished message that the server and client both send to the other that is a hash of all the handshake messages

useful example: k8s has a node authorizer that authorizes nodes to send api requests if they can present a signed certificate that says that they're part of the `system:nodes` group (in the identity info part, Common Name = `system:node:name` + Organization = `system:nodes`). what does this look like if you're bootstrapping a cluster? basically generate a keypair -> root CA. when running kube-apiserver, pass in the path to the root CA, and when a node establishes a tcp connection to the server, during tls handshake will pass its certificate. use root CA's public key and certificate with verification algorithm to verify that this certificate was in fact signed by root CA. if so, authenticate it, this node belongs to the group it says it belongs to. to get the certificate in the first place you got to: generate it, then sign it with the private key match to the root CA's public key. the private key from generating node CA is used to prove that you own the identity during tls handshake. 

once a node is authenticated, they can perform authorized requests over tls.

as you can see from this example, certificates are useful for proving that someone is who they say they are (i.e., saying that this data is in fact associated with them), and tls is used as protocol to communicate securely.

## wireguard 

ok let me write down how wireguard works

* tiny, 4k lines vs ipsec (massive) and openvpn (userspace, extremely slow as a result)
* i'm pleasantly surprised at how simple it is. basically just like creating another device interface with wg type, setting up with respective wg client (i.e., state behind the scenes, like peer and crypto-routing table; yes this can be configured beforehand), and it Just Works. extremely elegant, e.g.
	* entirely kernel-space by default, although there are now user-space implementations as well (e.g. mac wireguard is entirely user-space) 
	* use fixed allocation of memory at the start
	* "silence is a virtue!" if an invalid packet is received drop it and ignore it
	* all cryptographic choices are fixed! "no cypher agility" i.e.
		* uses noise protocol for cryptographic sequence. need to do more research but uses IKpsk2 (which describes what set of cryptographic primitives to use and how to use them together)
		* blake2s for hashing and mac ($f(input, key?): digest$) 
		* for aead wg uses chacha20poly1305
		* for keypair gen wg uses curve25519
		* optional pre-shared key with client that augments handshake for post-quantum reasons
* walking it thru step by step, say i'm on client sending userspace packet:
	* plaintext packet reaches wg interface (that i set up). i.e. routing table directs to wg device, which is a `net_device` and has `netdev_ops` that map to wireguard kernel functions
	* behind the scenes, wg kernel module keeps a crypto-routing table; that is, it keeps a map of peers and their associated public keys. 
	* looking at the plain text packet's ip addr, wg matches it up with its public key in the crypto-routing table (which is the static public key of the peer). if no peer is found, return icmp message with no route to host; otherwise
	* wg has some fixed state that each session shares:
		* a static keypair. this is asymmetric, thus keypair
	* each session has state, namely:
		* ephemeral keypairs, used specifically for this session. wg recycles ephemeral keypairs for security every 3 minutes, by re-performing handshakes (see below)
		* a symmetric encryption key
		* a nonce counter (since wg uses chacha, which makes use of a nonce counter)
		* a timestamp, to prevent replay attacks. that is, every time a handshake is performed, a interceptor could replay the handshake packet, but because there's a encrypted timestamp, this won't work
		* a sender index/receiver index (reversed on the other peer) that is used for quick lookup into the crypto-routing table
	* sequence in noise ik generates a couple of different
	* wg uses this session data to encrypt the entire ip packet that was sent to the interface (tunneling!) and then wg prepends the packet with a custom wg header, and then finally wraps it in a udp datagram and then an ip packet. the ip of the outer packet is either pre-configured with a config file, or it is learned from the most recent outer ip of the most recently authenticated received packet. if the endpoint can't be found, return icmp message
		* this is why roaming just works with wireguard
	* wg hands packet back to kernel networking, where routing table will send thru physical device
	* ip packet is sent thru to other peer. the wg kernel module also has a socket open on a configured port listening. when it receives the udp datagram from the kernel. strip the udp header -> check the wg header, which is not encrypted, to:
		* try and authenticate **TODO**
		* try and decrypt with the session shared key
		* if the packet authenticates, we use the outer source ip to update the list of available endpoints for the peer
		* try and extract an ip packet. if it's not a ip packet, it gets dropped. otherwise, we compare the source ip to the crypto-routing table and make sure it's in there; if not, it gets dropped
	* insert into receive queue of wg interface and call it a day :)
* of course, before a session is started a handshake is performed to set up session state 
* 1 rtt for initiation, extra packet if trying to secure against ddos attacks. 1rtt looks like:
	* handshake initiation; notably:
		* sender index
		* static public key
		* timestamp, encrypted
		* mac1/mac2
		* both peers do the following on their side to set up session state:
			* $C := \text{HASH}(\text{CONSTRUCTION})$, where `CONSTRUCTION` is a fixed string "Noise_IKpsk2_25519_ChaChaPoly_BLAKE2s" -> chaining value for hashing 
			* $H := \text{HASH}(C || \text{IDENTIFIER})$, where `IDENTIFIER` is a fixed string "WireGuard v1 zx2c4 Jason@zx2c4.com”
			* $H := \text{HASH(H || )}$
			* $(E^\text{priv}, E^\text{pub}) := \text{DH-GENERATE}()$  
		* 4 keypairs as a result of this:
			* initiator has (static private, static public)
			* initiator has (ephemeral private, ephemeral public)
			* receiver has (static private, static public)
			* receiver has (ephemeral private, ephemeral public)
	* if peer expects to be under load/wants to prevent ddos, sends back a cookie, and handshaker is expected to send back handshake initiation. this is what the mac fields are used for
		* mac1 is calculated as `HASH(responder_public_key || handshake_message)`. this ensures that the initiator is aware of the responder and is actually intending to send a packet to the responder, which helps with stealth
		* mac2 is empty the very first time an initiation message is sent
		* the responder receives the initiation message, replies with a special cookie message. notably it has a `cookie` field, which is 16 bytes and is `XEAD(key=HASH(responder_public_key), additional_data=handshake_message, MAC(responder_secret, initiator_ip_addr))`. 
		* initiator will then set mac2 as `MAC(key: cookie, handshake_message)`. when the responder receives a new initiation message, if mac2 is not this then it will reject it. this prevents attackers from spoofing ip and getting away with it
	* handshake response from peer; notably:
		* a receiver index
		* for securing each session packet, wg needs to derive a shared ephemeral symmetric key. wg does this through a series of steps with the chaining key per noise protocol and then produces $(T_{\text{send}}, T_{\text{recv}})$. you end up with keys to be used with chacha20-poly1305, which is the symmetric encryption protocol for the successive session packets
	* then begin session, where each transport message looks like:
* for security, handshakes are re-performed every 3 minutes (the default). what does this look like? well, to the peers that are sending packets, wg appears to be stateless. maintaining session states, perfect forward secrecy, handshakes, etc. behind the scenes wg maintains an extremely simple timer state machine that makes sure that the following is happening consistently:
	* key rotation - re-perform handshake, default is every 2 minutes. also handle

tailscale is basically private wireguard networks, without the pain and hassle. basically

* they have a control layer where they manage public key distribution for you, so that you can configure devices, set up auth 
* then, the data layer is peer-to-peer w/ wireguard tunnels, making life easy and awesome. the data layer is called the tailnet.

plus some extra features:

* magicdns lets u refer to devices by name
	* tailscale runs local dns resolver on each device at 100.100.100.100 (their quad 100 addr)
* derp = designated encrypted relay for packets

## ssh

ssh and wg are fundamentally really fucking similar, now that i'm reading abt both

scp/sftp runs over ssh. you can do stuff like local port forwarding, remote port forwarding. the default use is to ssh into a terminal, sshd just sits at the master side instead of a terminal emulator and writes input/output from the tunnel to the pty which forwards it to the shell.

server authentication just ensures no mitm etc. server sends its public host key and client matches to `known_hosts` file, if it doesn't match it warns the user and 

ssh is a application-layer protocol built on top of tcp. ssh is basically 3 way tcp handshake -> first message is ssh version exchange, to make sure they match -> perform dh to get shared key (server authentication happens here) -> custom kdf to generate private symmetric keys -> user auth -> channel multiplexing

1. ssh first creates a tcp session
2. ssh negotiates a secure ssh connection between the 2 machines. uses public key encryption
3. client creates a channel on the connection. a channel has 2 things: a data stream and control message layer. before sending data, the client will send requests on the control layer, e.g. "start a shell and use this channel as stdin/out/err".
4. client can create further channels

opening channels, making requests on them, and sending data are all done using ssh messages, which look something like this:

```
byte    SSH_MSG_CHANNEL_REQUEST = 98
uint32  recipient channel
string  "exec"
boolean want reply
string  command
```

what does the handshake protocol look like?
